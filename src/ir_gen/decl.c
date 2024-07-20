#include "ir_gen/decl.h"

#include "ir_gen/c_type.h"
#include "ir_gen/context.h"
#include "ir_gen/expr.h"
#include "ir_gen/initializer.h"
#include "parse.h"

static void direct_declarator_to_cdecl(
    IrGenContext *ctx, CType *ident_type, ASTDirectDeclarator *declarator,
    CDecl *cdecl);

IrGlobal *ir_global_for_decl(
    IrGenContext *ctx, ASTDeclSpecifier *decl_specifier_list,
    ASTDeclarator *declarator, ASTInitializer *initializer,
    CType **result_c_type)
{
  assert(declarator != NULL);

  CType *decl_spec_type =
      decl_specifier_list_to_c_type(ctx, decl_specifier_list);
  CDecl cdecl;
  decl_to_cdecl(ctx, decl_spec_type, declarator, &cdecl);
  infer_array_size_from_initializer(ctx, initializer, cdecl.type);

  CType *ctype = cdecl.type;
  if (ctype->t == FUNCTION_TYPE) {
    // Struct returns are handled in the frontend, by adding a pointer
    // parameter at the start, and allocating a local in the caller.
    bool struct_ret = ctype->u.function.return_type->t == STRUCT_TYPE;

    u32 arity = ctype->u.function.arity;
    if (struct_ret) arity++;

    IrType *arg_ir_types =
        pool_alloc(&ctx->builder->module->pool, sizeof(*arg_ir_types) * arity);

    u32 i = 0;
    u32 j = 0;
    if (struct_ret) {
      arg_ir_types[0] = (IrType){.t = IR_POINTER};
      i++;
    }
    for (; i < arity; i++, j++) {
      CType *arg_c_type = cdecl.type->u.function.arg_type_array[j];
      arg_ir_types[i] = c_type_to_ir_type(arg_c_type);
    }

    IrGlobal *global = NULL;
    Array(IrGlobal *) *globals = &ctx->builder->module->globals;
    assert(cdecl.name != NULL);
    for (u32 i = 0; i < globals->size; i++) {
      IrGlobal *curr_global = *ARRAY_REF(globals, IrGlobal *, i);
      if (streq(curr_global->name, cdecl.name)) {
        // @TODO: Check C type matches
        global = curr_global;
        break;
      }
    }

    if (global == NULL) {
      IrType return_type =
          struct_ret ? (IrType){.t = IR_VOID}
                     : c_type_to_ir_type(ctype->u.function.return_type);

      global = ir_module_add_function(
          ctx->builder->module, cdecl.name, return_type, arity,
          ctype->u.function.variable_arity, arg_ir_types);
    }

    assert(global->type.t == IR_FUNCTION);
    *result_c_type = ctype;

    return global;
  } else {
    IrGlobal *global = NULL;
    Array(IrGlobal *) *globals = &ctx->builder->module->globals;
    for (u32 i = 0; i < globals->size; i++) {
      IrGlobal *curr_global = *ARRAY_REF(globals, IrGlobal *, i);
      if (streq(curr_global->name, cdecl.name)) {
        // @TODO: Check C type matches
        global = curr_global;
        break;
      }
    }

    if (global == NULL) {
      global = ir_module_add_var(
          ctx->builder->module, cdecl.name, c_type_to_ir_type(ctype));
    }

    *result_c_type = cdecl.type;

    return global;
  }
}

void decl_to_cdecl(
    IrGenContext *ctx, CType *ident_type, ASTDeclarator *declarator,
    CDecl *cdecl)
{
  if (declarator == NULL) {
    *cdecl = (CDecl){NULL, ident_type};
    return;
  }

  switch (declarator->t) {
  case POINTER_DECLARATOR: {
    decl_to_cdecl(
        ctx, pointer_type(&ctx->type_env, ident_type),
        declarator->u.pointer_declarator.pointee, cdecl);
    break;
  }
  case DIRECT_DECLARATOR:
    direct_declarator_to_cdecl(
        ctx, ident_type, declarator->u.direct_declarator, cdecl);
    break;
  }
}

static void direct_declarator_to_cdecl(
    IrGenContext *ctx, CType *ident_type, ASTDirectDeclarator *declarator,
    CDecl *cdecl)
{
  switch (declarator->t) {
  case DECLARATOR:
    decl_to_cdecl(ctx, ident_type, declarator->u.declarator, cdecl);
    break;
  case IDENTIFIER_DECLARATOR:
    *cdecl = (CDecl){declarator->u.name, ident_type};
    break;
  case ARRAY_DECLARATOR: {
    ASTDirectDeclarator *elem_declarator =
        declarator->u.array_declarator.element_declarator;
    direct_declarator_to_cdecl(ctx, ident_type, elem_declarator, cdecl);

    CType *array = array_type(ctx->builder, &ctx->type_env, cdecl->type);
    cdecl->type = array;
    ASTExpr *array_length_expr = declarator->u.array_declarator.array_length;
    if (array_length_expr != NULL) {
      IrConst *length_const = eval_constant_expr(ctx, array_length_expr);
      assert(length_const->type.t == IR_INT);
      u64 length = length_const->u.integer;

      set_array_type_length(array, length);
    }

    break;
  }
  case FUNCTION_DECLARATOR: {
    ASTParameterDecl *first_param =
        declarator->u.function_declarator.parameters;
    ASTParameterDecl *params = first_param;

    u32 arity = 0;
    while (params != NULL) {
      switch (params->t) {
      case PARAMETER_DECL: arity++; break;
      case ELLIPSIS_DECL: assert(params->next == NULL); break;
      }
      params = params->next;
    }

    params = first_param;

    CType **arg_c_types =
        pool_alloc(&ctx->builder->module->pool, sizeof(*arg_c_types) * arity);

    bool variable_arity = false;
    for (u32 i = 0; params != NULL; i++) {
      switch (params->t) {
      case PARAMETER_DECL: {
        CType *param_ident_type =
            decl_specifier_list_to_c_type(ctx, params->decl_specifier_list);
        CDecl param_cdecl;
        decl_to_cdecl(ctx, param_ident_type, params->declarator, &param_cdecl);

        if (param_cdecl.type->t == VOID_TYPE) {
          assert(i == 0);
          assert(param_cdecl.name == NULL);
        }

        // As per 6.7.5.3.7, parameters of array type are adjusted to
        // pointers to the element type.
        param_cdecl.type = decay_to_pointer(&ctx->type_env, param_cdecl.type);

        arg_c_types[i] = param_cdecl.type;
        break;
      }
      case ELLIPSIS_DECL:
        variable_arity = true;
        // Can't have more params after an ellipsis.
        assert(params->next == NULL);
        break;
      }

      params = params->next;
    }

    // This is a nullary function declaration, using void,
    // e.g. int foo(void);
    if (arity == 1 && arg_c_types[0]->t == VOID_TYPE) {
      assert(!variable_arity);
      arg_c_types = NULL;
      arity = 0;
    }

    CType *ctype = pool_alloc(&ctx->builder->module->pool, sizeof *ctype);
    ctype->t = FUNCTION_TYPE;
    ctype->u.function.arity = arity;
    ctype->u.function.variable_arity = variable_arity;
    ctype->u.function.arg_type_array = arg_c_types;
    ctype->u.function.return_type = ident_type;

    ASTDirectDeclarator *function_declarator =
        declarator->u.function_declarator.declarator;
    direct_declarator_to_cdecl(ctx, ctype, function_declarator, cdecl);

    break;
  }
  }
}

CType *decl_specifier_list_to_c_type(
    IrGenContext *ctx, ASTDeclSpecifier *decl_specifier_list)
{
  TypeEnv *type_env = &ctx->type_env;

  // @TODO: Actually handle type qualifiers rather than ignoring them.
  while (decl_specifier_list != NULL
         && decl_specifier_list->t == TYPE_QUALIFIER) {
    decl_specifier_list = decl_specifier_list->next;
  }

  assert(decl_specifier_list != NULL);
  assert(decl_specifier_list->t == TYPE_SPECIFIER);

  ASTTypeSpecifier *type_spec = decl_specifier_list->u.type_specifier;

  switch (type_spec->t) {
  case NAMED_TYPE_SPECIFIER: {
    return named_type_specifier_to_ctype(type_env, decl_specifier_list);
  }
  case STRUCT_TYPE_SPECIFIER:
  case UNION_TYPE_SPECIFIER: {
    ASTFieldDecl *field_list =
        type_spec->u.struct_or_union_specifier.field_list;
    char *name = type_spec->u.struct_or_union_specifier.name;
    ASTAttribute *attribute = type_spec->u.struct_or_union_specifier.attribute;
    bool is_packed = attribute != NULL && streq(attribute->name, "packed");

    CType *existing_type = NULL;
    if (name != NULL) {
      // @TODO: Really we just want to search in the current scope; it's
      // perfectly valid to shadow a struct or union type from an
      // enclosing scope.
      existing_type = search(&type_env->struct_types, name);
    }

    if (field_list == NULL) {
      if (name == NULL) {
        assert(!"Error, no name or fields for struct or union type");
      } else if (existing_type == NULL) {
        // Incomplete type
        return struct_type(type_env, name);
      } else {
        return existing_type;
      }
    }
    CType *type;
    if (existing_type != NULL) {
      assert(existing_type->t == STRUCT_TYPE);
      if (!existing_type->u.strukt.incomplete)
        assert(!"Error, redefinition of struct or union type");

      type = existing_type;
    } else {
      type = struct_type(type_env, name);
    }
    Array(CDecl) *fields = &type->u.strukt.fields;

    while (field_list != NULL) {
      CType *decl_spec_type =
          decl_specifier_list_to_c_type(ctx, field_list->decl_specifier_list);
      ASTFieldDeclarator *field_declarator = field_list->field_declarator_list;
      while (field_declarator != NULL) {
        assert(field_declarator->t == NORMAL_FIELD_DECLARATOR);
        ASTDeclarator *declarator = field_declarator->u.declarator;

        CDecl *cdecl = ARRAY_APPEND(fields, CDecl);
        decl_to_cdecl(ctx, decl_spec_type, declarator, cdecl);

        field_declarator = field_declarator->next;
      }

      field_list = field_list->next;
    }

    IrType *ir_struct =
        ir_module_add_struct(ctx->builder->module, name, fields->size);
    u32 current_offset = 0;
    u32 max_field_size = 0;
    u32 max_field_align = 0;
    for (u32 i = 0; i < fields->size; i++) {
      CDecl *field = ARRAY_REF(fields, CDecl, i);
      IrType field_type = c_type_to_ir_type(field->type);

      ir_struct->u.strukt.fields[i].type = field_type;

      u32 field_size = size_of_ir_type(field_type);
      u32 field_align = align_of_ir_type(field_type);
      max_field_size = max(max_field_size, field_size);
      max_field_align = max(max_field_align, field_align);

      if (type_spec->t == STRUCT_TYPE_SPECIFIER) {
        if (!is_packed) current_offset = align_to(current_offset, field_align);
        ir_struct->u.strukt.fields[i].offset = current_offset;

        current_offset += field_size;
      } else {
        ir_struct->u.strukt.fields[i].offset = 0;
      }
    }
    ir_struct->u.strukt.total_size = align_to(
        type_spec->t == STRUCT_TYPE_SPECIFIER ? current_offset : max_field_size,
        is_packed ? 1 : max_field_align);
    ir_struct->u.strukt.alignment = is_packed ? 1 : max_field_align;

    type->u.strukt.ir_type = ir_struct;
    type->u.strukt.incomplete = false;

    return type;
  }
  case ENUM_TYPE_SPECIFIER: {
    char *tag = type_spec->u.enum_specifier.name;
    ASTEnumerator *enumerator_list =
        type_spec->u.enum_specifier.enumerator_list;

    CType *ctype = &type_env->int_type;

    CType *existing_type = NULL;
    if (tag != NULL) {
      existing_type = search(&type_env->enum_types, tag);
    }

    if (enumerator_list == NULL) {
      if (tag == NULL) {
        assert(!"Error, no name or enumerators for enum type");
      } else if (existing_type == NULL) {
        // Incomplete type.
        // @TODO: This should be illegal to use, but for now we just
        // call it int
        return ctype;
      } else {
        return existing_type;
      }
    }
    // @TODO: Incomplete enum types.
    assert(existing_type == NULL);

    if (tag != NULL) {
      TypeEnvEntry *new_type_alias =
          pool_alloc(&type_env->pool, sizeof *new_type_alias);
      *ARRAY_APPEND(&type_env->enum_types, TypeEnvEntry *) = new_type_alias;
      new_type_alias->name = tag;
      new_type_alias->type = *ctype;
    }

    u64 curr_enum_value = 0;
    while (enumerator_list != NULL) {
      char *name = enumerator_list->name;
      ASTExpr *expr = enumerator_list->value;

      if (expr != NULL) {
        IrConst *value = eval_constant_expr(ctx, expr);
        assert(value->type.t == IR_INT);
        curr_enum_value = value->u.integer;
      }

      Binding *binding = ARRAY_APPEND(&ctx->scope->bindings, Binding);
      binding->name = name;
      binding->constant = true;
      binding->term.ctype = ctype;
      binding->term.value =
          value_const(c_type_to_ir_type(ctype), curr_enum_value++);

      enumerator_list = enumerator_list->next;
    }

    return ctype;
  }
  default: UNIMPLEMENTED;
  }
}

void cdecl_to_binding(IrBuilder *builder, CDecl *cdecl, Binding *binding)
{
  IrType ir_type = c_type_to_ir_type(cdecl->type);

  binding->name = cdecl->name;
  binding->constant = false;
  binding->term.ctype = cdecl->type;
  binding->term.value = build_local(builder, ir_type);
}