#include "ir_gen/initializer.h"

#include "ir_gen/c_type.h"
#include "ir_gen/context.h"
#include "ir_gen/convert.h"
#include "ir_gen/expr.h"
#include "parse.h"

static void ir_gen_c_init(
    IrBuilder *builder, TypeEnv *type_env, IrValue base_ptr,
    CInitializer *c_init, u32 current_offset);
static bool is_full_initializer(CInitializer *c_init);
static void const_gen_c_init_array(
    IrBuilder *builder, CInitializer *c_init, IrConst *konst, u32 *const_index);

void make_c_initializer(
    IrGenContext *ctx, Pool *pool, CType *type, ASTInitializer *init,
    bool const_context, CInitializer *c_init)
{
  c_init->type = type;

  if (type->t == ARRAY_TYPE && init->t == EXPR_INITIALIZER
      && init->u.expr->t == STRING_LITERAL_EXPR) {
    c_init->t = C_INIT_COMPOUND;

    String str = init->u.expr->u.string_literal;
    CInitializer *init_elems =
        pool_alloc(pool, (str.len + 1) * sizeof *init_elems);
    for (u32 i = 0; i < str.len + 1; i++) {
      CType *char_type = &ctx->type_env.char_type;
      init_elems[i] = (CInitializer){
          .t = C_INIT_LEAF,
          .type = char_type,
          .u.leaf_value =
              value_const(c_type_to_ir_type(char_type), str.chars[i]),
      };
    }

    c_init->u.sub_elems = init_elems;
  } else if (init->t == BRACE_INITIALIZER) {
    assert(type->t == STRUCT_TYPE || type->t == ARRAY_TYPE);

    u32 num_fields = c_type_num_fields(type);

    CInitializer *init_elems =
        pool_alloc(pool, num_fields * sizeof *init_elems);
    memset(init_elems, 0, num_fields * sizeof *init_elems);
    c_init->u.sub_elems = init_elems;

    ASTInitializerElement *elems = init->u.initializer_element_list;
    u32 curr_elem_index = 0;
    while (elems != NULL) {
      CInitializer *containing_init = c_init;
      CInitializer *curr_elem = c_init;

      ASTDesignator *designator_list = elems->designator_list;
      while (designator_list != NULL) {
        CType *field_type;
        switch (designator_list->t) {
        case FIELD_DESIGNATOR: {
          assert(curr_elem->type->t == STRUCT_TYPE);
          Array(CDecl) *fields = &curr_elem->type->u.strukt.fields;

          CDecl *selected_field = NULL;
          u32 field_number;
          for (u32 i = 0; i < fields->size; i++) {
            CDecl *field = ARRAY_REF(fields, CDecl, i);
            if (streq(field->name, designator_list->u.field_name)) {
              selected_field = field;
              field_number = i;
              break;
            }
          }
          assert(selected_field != NULL);

          field_type = selected_field->type;
          curr_elem_index = field_number;

          break;
        }
        case INDEX_DESIGNATOR: {
          assert(curr_elem->type->t == ARRAY_TYPE);
          IrConst *index =
              eval_constant_expr(ctx, designator_list->u.index_expr);
          assert(index->type.t == IR_INT);

          field_type = curr_elem->type->u.array.elem_type;
          curr_elem_index = index->u.integer;

          break;
        }
        }

        curr_elem = containing_init->u.sub_elems + curr_elem_index;
        curr_elem->type = field_type;

        if ((curr_elem->type->t == STRUCT_TYPE
             || curr_elem->type->t == ARRAY_TYPE)
            && curr_elem->u.sub_elems == NULL) {
          u32 inner_num_fields = c_type_num_fields(field_type);
          CInitializer *sub_elems =
              pool_alloc(pool, inner_num_fields * sizeof *sub_elems);
          memset(sub_elems, 0, inner_num_fields * sizeof *sub_elems);
          curr_elem->u.sub_elems = sub_elems;
        }

        if (designator_list->next != NULL) {
          containing_init = curr_elem;
        }

        designator_list = designator_list->next;
      }

      curr_elem = containing_init->u.sub_elems + curr_elem_index;

      CType *curr_elem_type;
      switch (containing_init->type->t) {
      case ARRAY_TYPE:
        curr_elem_type = containing_init->type->u.array.elem_type;
        break;
      case STRUCT_TYPE:
        curr_elem_type =
            ARRAY_REF(
                &containing_init->type->u.strukt.fields, CDecl, curr_elem_index)
                ->type;
        break;
      default: UNREACHABLE;
      }

      make_c_initializer(
          ctx, pool, curr_elem_type, elems->initializer, const_context,
          curr_elem);

      curr_elem_index++;
      elems = elems->next;
    }
  } else {
    assert(init->t == EXPR_INITIALIZER);

    c_init->t = C_INIT_LEAF;

    ASTExpr *expr = init->u.expr;
    IrValue value;

    if (const_context) {
      IrConst *konst = eval_constant_expr(ctx, expr);

      // @TODO: This would be much nicer if IrValue contained IrConst
      // instead of just a u64.
      switch (type->t) {
      case INTEGER_TYPE:
        assert(konst->type.t == IR_INT);
        assert(type->t == INTEGER_TYPE);
        value = value_const(konst->type, konst->u.integer);
        break;
      case POINTER_TYPE: {
        assert(konst->type.t == IR_POINTER);
        value = value_global(konst->u.global_pointer);

        break;
      }
      default: UNIMPLEMENTED;
      }
    } else {
      Term term = ir_gen_expr(ctx, expr, RVALUE_CONTEXT);
      value = convert_type(ctx->builder, term, type).value;
    }

    c_init->u.leaf_value = value;
  }
}

void ir_gen_initializer(IrGenContext *ctx, Term to_init, ASTInitializer *init)
{
  Pool c_init_pool;
  pool_init(&c_init_pool, sizeof(CInitializer) * 5);

  CInitializer c_init;
  ZERO_STRUCT(&c_init);
  make_c_initializer(ctx, &c_init_pool, to_init.ctype, init, false, &c_init);

  if (!is_full_initializer(&c_init)) {
    IrValue *memset_args =
        pool_alloc(&ctx->builder->module->pool, 3 * sizeof *memset_args);
    memset_args[0] = to_init.value;
    memset_args[1] = value_const(c_type_to_ir_type(&ctx->type_env.int_type), 0);
    memset_args[2] = value_const(
        c_type_to_ir_type(ctx->type_env.size_type), c_type_size(to_init.ctype));

    // @TODO: Open-code this for small sizes
    build_call(
        ctx->builder, builtin_memset(ctx->builder), (IrType){.t = IR_POINTER},
        3, memset_args);
  }

  // @TODO: Sort initializer element list by offset because something
  // something cache something something.

  IrValue base_ptr = build_type_instr(
      ctx->builder, OP_CAST, to_init.value,
      c_type_to_ir_type(ctx->type_env.int_ptr_type));
  ir_gen_c_init(ctx->builder, &ctx->type_env, base_ptr, &c_init, 0);

  pool_free(&c_init_pool);
}

void infer_array_size_from_initializer(
    IrGenContext *ctx, ASTInitializer *init, CType *type)
{
  if (type->t != ARRAY_TYPE || !type->u.array.incomplete || init == NULL)
    return;

  u32 size;

  if (init->t == BRACE_INITIALIZER) {
    i32 current_index = -1;
    i32 max_index = -1;
    ASTInitializerElement *init_elem = init->u.initializer_element_list;
    while (init_elem != NULL) {
      ASTDesignator *designator = init_elem->designator_list;
      if (designator != NULL) {
        assert(designator->t == INDEX_DESIGNATOR);
        IrConst *index_value =
            eval_constant_expr(ctx, designator->u.index_expr);
        assert(index_value->type.t == IR_INT);

        current_index = index_value->u.integer;
      } else {
        current_index++;
      }

      if (current_index > max_index) max_index = current_index;

      init_elem = init_elem->next;
    }

    size = max_index + 1;
  } else {
    assert(init->u.expr->t == STRING_LITERAL_EXPR);
    size = init->u.expr->u.string_literal.len + 1;
  }

  set_array_type_length(type, size);
}

static void ir_gen_c_init(
    IrBuilder *builder, TypeEnv *type_env, IrValue base_ptr,
    CInitializer *c_init, u32 current_offset)
{
  CType *type = c_init->type;
  if (type == NULL) return;

  switch (type->t) {
  case ARRAY_TYPE: {
    assert(!type->u.array.incomplete);
    // Array values must be initialized by compound initializers.
    assert(c_init->t == C_INIT_COMPOUND);

    u32 elem_size = c_type_size(type->u.array.elem_type);

    for (u32 i = 0; i < type->u.array.size; i++) {
      ir_gen_c_init(
          builder, type_env, base_ptr, c_init->u.sub_elems + i, current_offset);
      current_offset += elem_size;
    }
    break;
  }
  case STRUCT_TYPE:
    switch (c_init->t) {
    case C_INIT_COMPOUND: {
      IrStructField *fields = type->u.strukt.ir_type->u.strukt.fields;
      for (u32 i = 0; i < type->u.strukt.fields.size; i++) {
        u32 field_offset = current_offset + fields[i].offset;
        ir_gen_c_init(
            builder, type_env, base_ptr, c_init->u.sub_elems + i, field_offset);
      }
      break;
    }
    // Struct values can be initialized with expressions.
    case C_INIT_LEAF: {
      IrValue *memcpy_args =
          pool_alloc(&builder->module->pool, 3 * sizeof *memcpy_args);
      memcpy_args[0] = build_binary_instr(
          builder, OP_ADD, base_ptr,
          value_const(
              c_type_to_ir_type(type_env->int_ptr_type), current_offset));
      memcpy_args[1] = c_init->u.leaf_value;
      memcpy_args[2] = value_const(
          c_type_to_ir_type(type_env->size_type), c_type_size(type));

      // @TODO: Open-code this for small sizes
      build_call(
          builder, builtin_memcpy(builder), (IrType){.t = IR_POINTER}, 3,
          memcpy_args);
      break;
    }
    }
    break;
  default: {
    assert(c_init->t == C_INIT_LEAF);

    IrType int_ptr_type = c_type_to_ir_type(type_env->int_ptr_type);
    IrValue field_ptr = build_binary_instr(
        builder, OP_ADD, base_ptr, value_const(int_ptr_type, current_offset));
    build_store(builder, field_ptr, c_init->u.leaf_value);
    break;
  }
  }
}

static bool is_full_initializer(CInitializer *c_init)
{
  CType *type = c_init->type;
  if (type == NULL) return false;

  if (c_init->t == C_INIT_LEAF) return true;

  u32 num_elems;
  switch (type->t) {
  case ARRAY_TYPE:
    assert(!type->u.array.incomplete);
    num_elems = type->u.array.size;
    break;
  case STRUCT_TYPE: num_elems = type->u.strukt.fields.size; break;
  default: UNREACHABLE;
  }

  for (u32 i = 0; i < num_elems; i++) {
    if (!is_full_initializer(c_init->u.sub_elems + i)) {
      return false;
    }
  }
  return true;
}

IrConst *const_gen_c_init(IrBuilder *builder, CInitializer *c_init)
{
  CType *type = c_init->type;
  assert(type != NULL);
  switch (type->t) {
  case STRUCT_TYPE: {
    IrConst *c = add_struct_const(builder, *type->u.strukt.ir_type);
    Array(CDecl) *fields = &type->u.strukt.fields;
    for (u32 i = 0; i < fields->size; i++) {
      CType *field_type = ARRAY_REF(fields, CDecl, i)->type;
      CInitializer *sub_init = c_init->u.sub_elems + i;

      if (sub_init->type == NULL) {
        c->u.struct_fields[i] = *zero_initializer(builder, field_type);
      } else {
        c->u.struct_fields[i] = *const_gen_c_init(builder, sub_init);
      }
    }

    return c;
  }
  case ARRAY_TYPE: {
    IrConst *c = add_array_const(builder, *type->u.array.ir_type);
    u32 i = 0;
    const_gen_c_init_array(builder, c_init, c, &i);

    return c;
  }
  case INTEGER_TYPE: {
    IrValue value = c_init->u.leaf_value;
    assert(value.type.t == IR_INT);
    return add_int_const(builder, c_type_to_ir_type(type), value.u.constant);
  }
  case POINTER_TYPE: {
    IrValue value = c_init->u.leaf_value;
    assert(value.t == IR_VALUE_GLOBAL);
    return add_global_const(builder, value.u.global);
  }
  default: UNIMPLEMENTED;
  }
}

static void const_gen_c_init_array(
    IrBuilder *builder, CInitializer *c_init, IrConst *konst, u32 *const_index)
{
  CType *type = c_init->type;
  assert(type->t == ARRAY_TYPE);

  CType *elem_type = type->u.array.elem_type;
  u32 array_size = type->u.array.size;

  if (elem_type->t == ARRAY_TYPE) {
    for (u32 i = 0; i < array_size; i++) {
      const_gen_c_init_array(
          builder, c_init->u.sub_elems + i, konst, const_index);
    }
  } else {
    for (u32 i = 0; i < array_size; i++) {
      CInitializer *sub_init = c_init->u.sub_elems + i;

      if (sub_init->type == NULL) {
        konst->u.array_elems[*const_index + i] =
            *zero_initializer(builder, elem_type);
      } else {
        konst->u.array_elems[*const_index + i] =
            *const_gen_c_init(builder, sub_init);
      }
    }

    *const_index += type->u.array.size;
  }
}

IrConst *zero_initializer(IrBuilder *builder, CType *ctype)
{
  switch (ctype->t) {
  case INTEGER_TYPE: return add_int_const(builder, c_type_to_ir_type(ctype), 0);
  case POINTER_TYPE: return add_global_const(builder, NULL);
  case ARRAY_TYPE: {
    assert(!ctype->u.array.incomplete);
    // @TODO: This allocates unnecessarily by calling zero_initializer
    // recursively and then copying the result into array_elems.
    IrConst *konst = add_array_const(builder, c_type_to_ir_type(ctype));
    for (u32 i = 0; i < ctype->u.array.size; i++) {
      konst->u.array_elems[i] =
          *zero_initializer(builder, ctype->u.array.elem_type);
    }
    return konst;
  }
  case STRUCT_TYPE: {
    assert(!ctype->u.strukt.incomplete);
    // @TODO: This allocates unnecessarily by calling zero_initializer
    // recursively and then copying the result into array_elems.
    IrConst *konst = add_struct_const(builder, c_type_to_ir_type(ctype));
    for (u32 i = 0; i < ctype->u.strukt.fields.size; i++) {
      CType *field_type = ARRAY_REF(&ctype->u.strukt.fields, CDecl, i)->type;
      konst->u.struct_fields[i] = *zero_initializer(builder, field_type);
    }
    return konst;
  }
  default: UNIMPLEMENTED;
  }
}