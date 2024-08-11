#include "ir_gen/expr.h"

#include <stdlib.h>

#include "ir.h"
#include "ir_gen/context.h"
#include "ir_gen/convert.h"
#include "ir_gen/decl.h"
#include "ir_gen/initializer.h"
#include "syntax/parse.h"

static Term ir_gen_assign_expr(IrGenContext *ctx, ASTExpr *expr, IrOp ir_op);
static Term ir_gen_struct_field(
    IrBuilder *builder, Term struct_term, char *field_name,
    ExprContext context);
static Term ir_gen_deref(
    IrBuilder *builder, TypeEnv *type_env, Term pointer, ExprContext context);
static Term ir_gen_cmp_expr(IrGenContext *ctx, ASTExpr *expr, IrCmp cmp);
static Term ir_gen_cmp(IrGenContext *ctx, Term left, Term right, IrCmp cmp);
static Term ir_gen_binary_expr(IrGenContext *ctx, ASTExpr *expr, IrOp ir_op);
static Term ir_gen_binary_operator(
    IrGenContext *ctx, Term left, Term right, IrOp ir_op);
static Term ir_gen_add(IrGenContext *ctx, Term left, Term right);
static Term ir_gen_sub(IrGenContext *ctx, Term left, Term right);
static Term ir_gen_inc_dec(IrGenContext *ctx, ASTExpr *expr);

static CType *type_name_to_c_type(IrGenContext *ctx, ASTTypeName *type_name);

Term ir_gen_expr(IrGenContext *ctx, ASTExpr *expr, ExprContext context)
{
  IrBuilder *builder = ctx->builder;

  ASTExprType t = expr->t;
  if (context == LVALUE_CONTEXT) {
    switch (t) {
    case IDENTIFIER_EXPR:
    case STRUCT_DOT_FIELD_EXPR:
    case STRUCT_ARROW_FIELD_EXPR:
    case INDEX_EXPR:
    case DEREF_EXPR: break;
    default: UNREACHABLE;
    }
  }

  if (context == CONST_CONTEXT) {
    switch (t) {
    case ASSIGN_EXPR:
    case ADD_ASSIGN_EXPR:
    case MINUS_ASSIGN_EXPR:
    case PRE_INCREMENT_EXPR:
    case POST_INCREMENT_EXPR:
    case PRE_DECREMENT_EXPR:
    case POST_DECREMENT_EXPR:
    case BIT_XOR_ASSIGN_EXPR:
    case BIT_AND_ASSIGN_EXPR:
    case BIT_OR_ASSIGN_EXPR:
    case RIGHT_SHIFT_ASSIGN_EXPR:
    case MULTIPLY_ASSIGN_EXPR:
    case DIVIDE_ASSIGN_EXPR:
    case FUNCTION_CALL_EXPR:
    case COMMA_EXPR: UNREACHABLE;
    default: break;
    }
  }

  switch (t) {
  case IDENTIFIER_EXPR: {
    Binding *binding = binding_for_name(ctx->scope, expr->u.identifier);

    if (binding == NULL) {
      fprintf(stderr, "Unknown identifier '%s'\n", expr->u.identifier);
      exit(1);
    }
    assert(binding->term.value.type.t == IR_POINTER || binding->constant);

    IrValue value;

    // Functions, arrays, and structs implicitly have their address taken.
    if (context == LVALUE_CONTEXT || binding->term.ctype->t == FUNCTION_TYPE
        || binding->term.ctype->t == ARRAY_TYPE
        || binding->term.ctype->t == STRUCT_TYPE) {
      assert(!binding->constant);
      value = binding->term.value;
    } else {
      if (binding->constant) {
        value = binding->term.value;
      } else {
        assert(context != CONST_CONTEXT);
        value = build_load(
            builder, binding->term.value,
            c_type_to_ir_type(binding->term.ctype));
      }
    }

    return (Term){.ctype = binding->term.ctype, .value = value};
  }
  case STRUCT_ARROW_FIELD_EXPR: {
    ASTExpr *struct_expr = expr->u.struct_field.struct_expr;
    Term struct_term = ir_gen_expr(ctx, struct_expr, RVALUE_CONTEXT);
    assert(struct_term.ctype->t == POINTER_TYPE);
    assert(struct_term.ctype->u.pointee_type->t == STRUCT_TYPE);

    return ir_gen_struct_field(
        builder, struct_term, expr->u.struct_field.field_name, context);
  }
  case STRUCT_DOT_FIELD_EXPR: {
    ASTExpr *struct_expr = expr->u.struct_field.struct_expr;
    Term struct_term = ir_gen_expr(ctx, struct_expr, RVALUE_CONTEXT);
    assert(struct_term.ctype->t == STRUCT_TYPE);

    return ir_gen_struct_field(
        builder, struct_term, expr->u.struct_field.field_name, context);
  }
  case ADDRESS_OF_EXPR: {
    ASTExpr *inner_expr = expr->u.unary_arg;
    Term ptr = ir_gen_expr(ctx, inner_expr, LVALUE_CONTEXT);
    ptr.ctype = pointer_type(&ctx->type_env, ptr.ctype);

    return ptr;
  }
  case DEREF_EXPR: {
    ASTExpr *inner_expr = expr->u.unary_arg;
    Term pointer = ir_gen_expr(ctx, inner_expr, RVALUE_CONTEXT);
    return ir_gen_deref(builder, &ctx->type_env, pointer, context);
  }
  case INDEX_EXPR: {
    Term pointer = ir_gen_add(
        ctx, ir_gen_expr(ctx, expr->u.binary_op.arg1, RVALUE_CONTEXT),
        ir_gen_expr(ctx, expr->u.binary_op.arg2, RVALUE_CONTEXT));
    assert(pointer.ctype->t == POINTER_TYPE);
    return ir_gen_deref(builder, &ctx->type_env, pointer, context);
  }
  case INT_LITERAL_EXPR: {
    CType *result_type =
        type_of_int_literal(&ctx->type_env, expr->u.int_literal);

    IrValue value = value_const_int(
        c_type_to_ir_type(result_type), expr->u.int_literal.value);

    return (Term){.ctype = result_type, .value = value};
  }
  case FLOAT_LITERAL_EXPR: {
    CType *result_type =
        type_of_float_literal(&ctx->type_env, expr->u.float_literal);
    IrValue value = value_const_float(
        c_type_to_ir_type(result_type), expr->u.float_literal.value);
    return (Term){.ctype = result_type, .value = value};
  }
  case STRING_LITERAL_EXPR: {
    char fmt[] = "__string_literal_%x";

    // - 2 adjusts down for the "%x" which isn't present in the output
    // sizeof(u32) * 2 is the max length of globals.size in hex
    // + 1 for the null terminator
    u32 name_max_length = sizeof fmt - 2 + sizeof(u32) * 2 + 1;
    char *name = pool_alloc(&builder->module->pool, name_max_length);
    snprintf(name, name_max_length, fmt, builder->module->globals.size);

    String string = expr->u.string_literal;
    u32 length = string.len + 1;
    CType *result_type =
        array_type(builder, &ctx->type_env, &ctx->type_env.char_type);
    set_array_type_length(result_type, length);
    IrType ir_type = c_type_to_ir_type(result_type);
    IrGlobal *global = ir_module_add_var(builder->module, name, ir_type);
    global->linkage = IR_LOCAL_LINKAGE;

    IrConst *konst = add_array_const(builder, ir_type);
    IrType ir_char_type = c_type_to_ir_type(&ctx->type_env.char_type);
    for (u32 i = 0; i < length; i++) {
      konst->u.array_elems[i] = (IrConst){
          .type = ir_char_type,
          .u.integer = string.chars[i],
      };
    }

    konst->type = ir_type;
    global->initializer = konst;

    return (Term){.ctype = result_type, .value = value_global(global)};
  }
  case ADD_EXPR: return ir_gen_binary_expr(ctx, expr, OP_ADD);
  case MINUS_EXPR: return ir_gen_binary_expr(ctx, expr, OP_SUB);
  case BIT_XOR_EXPR: return ir_gen_binary_expr(ctx, expr, OP_BIT_XOR);
  case BIT_AND_EXPR: return ir_gen_binary_expr(ctx, expr, OP_BIT_AND);
  case BIT_OR_EXPR: return ir_gen_binary_expr(ctx, expr, OP_BIT_OR);
  case BIT_NOT_EXPR: {
    // @TODO: Determine type correctly.
    CType *result_type = &ctx->type_env.int_type;
    Term term = ir_gen_expr(ctx, expr->u.unary_arg, RVALUE_CONTEXT);

    return (Term){
        .value = build_unary_instr(builder, OP_BIT_NOT, term.value),
        .ctype = result_type,
    };
  }
  case LOGICAL_NOT_EXPR: {
    CType *result_type = &ctx->type_env.int_type;
    Term inner = ir_gen_expr(ctx, expr->u.unary_arg, RVALUE_CONTEXT);
    Term zero = {
        .ctype = result_type,
        .value = value_const_int(c_type_to_ir_type(result_type), 0),
    };
    return ir_gen_cmp(ctx, inner, zero, CMP_EQ);
  }
  case UNARY_MINUS_EXPR: {
    Term term = ir_gen_expr(ctx, expr->u.unary_arg, RVALUE_CONTEXT);

    return (Term){
        .value = build_unary_instr(builder, OP_NEG, term.value),
        .ctype = term.ctype,
    };
  }
  case LEFT_SHIFT_EXPR: return ir_gen_binary_expr(ctx, expr, OP_SHL);
  // @TODO: Emit arithmetic shifts for signed LHS.
  case RIGHT_SHIFT_EXPR: return ir_gen_binary_expr(ctx, expr, OP_SHR);
  case MULTIPLY_EXPR: return ir_gen_binary_expr(ctx, expr, OP_MUL);
  case DIVIDE_EXPR: return ir_gen_binary_expr(ctx, expr, OP_DIV);
  case MODULO_EXPR: return ir_gen_binary_expr(ctx, expr, OP_MOD);
  case EQUAL_EXPR: return ir_gen_cmp_expr(ctx, expr, CMP_EQ);
  case NOT_EQUAL_EXPR: return ir_gen_cmp_expr(ctx, expr, CMP_NEQ);
  case GREATER_THAN_EXPR: return ir_gen_cmp_expr(ctx, expr, CMP_SGT);
  case GREATER_THAN_OR_EQUAL_EXPR: return ir_gen_cmp_expr(ctx, expr, CMP_SGTE);
  case LESS_THAN_EXPR: return ir_gen_cmp_expr(ctx, expr, CMP_SLT);
  case LESS_THAN_OR_EQUAL_EXPR: return ir_gen_cmp_expr(ctx, expr, CMP_SLTE);

  case ASSIGN_EXPR: return ir_gen_assign_expr(ctx, expr, OP_INVALID);
  case ADD_ASSIGN_EXPR: return ir_gen_assign_expr(ctx, expr, OP_ADD);
  case MINUS_ASSIGN_EXPR: return ir_gen_assign_expr(ctx, expr, OP_SUB);
  case PRE_INCREMENT_EXPR:
  case POST_INCREMENT_EXPR:
  case PRE_DECREMENT_EXPR:
  case POST_DECREMENT_EXPR: return ir_gen_inc_dec(ctx, expr);
  case BIT_XOR_ASSIGN_EXPR: return ir_gen_assign_expr(ctx, expr, OP_BIT_XOR);
  case BIT_AND_ASSIGN_EXPR: return ir_gen_assign_expr(ctx, expr, OP_BIT_AND);
  case BIT_OR_ASSIGN_EXPR: return ir_gen_assign_expr(ctx, expr, OP_BIT_OR);
  case RIGHT_SHIFT_ASSIGN_EXPR: return ir_gen_assign_expr(ctx, expr, OP_SHR);
  case LEFT_SHIFT_ASSIGN_EXPR: return ir_gen_assign_expr(ctx, expr, OP_SHL);
  case MULTIPLY_ASSIGN_EXPR: return ir_gen_assign_expr(ctx, expr, OP_MUL);
  case DIVIDE_ASSIGN_EXPR: return ir_gen_assign_expr(ctx, expr, OP_DIV);
  case FUNCTION_CALL_EXPR: {
    ASTExpr *callee_expr = expr->u.function_call.callee;

    u32 call_arity = 0;
    ASTArgument *arg = expr->u.function_call.arg_list;
    while (arg != NULL) {
      call_arity++;
      arg = arg->next;
    }

    if (callee_expr->t == IDENTIFIER_EXPR) {
      char *name = callee_expr->u.identifier;
      if (streq(name, "__builtin_va_start")) {
        assert(call_arity == 1);

        Term va_list_ptr = ir_gen_expr(
            ctx, expr->u.function_call.arg_list->expr, RVALUE_CONTEXT);
        assert(
            va_list_ptr.ctype->t == ARRAY_TYPE
            || va_list_ptr.ctype->t == POINTER_TYPE);
        assert(va_list_ptr.ctype->u.array.elem_type->t == STRUCT_TYPE);

        // @TODO: Search through the type env and asert that the elem
        // type is the same as the type bound to "va_list".

        return (Term){
            .ctype = &ctx->type_env.void_type,
            .value = build_builtin_va_start(builder, va_list_ptr.value),
        };
      } else if (streq(name, "__builtin_va_end")) {
        // va_end is a NOP for System V x64, so just return a dummy
        // value, and give it void type to ensure it's not used.
        return (Term){
            .ctype = &ctx->type_env.void_type,
            .value = value_const_int((IrType){.t = IR_VOID}, 0),
        };
      }
    }

    Term callee = ir_gen_expr(ctx, callee_expr, RVALUE_CONTEXT);
    // @TODO: We should never have objects of bare function type in the
    // first place - when ir_gen'ing an identifier expr referring to a
    // global function it should have type "pointer to F", where F is the
    // type of the function in question.
    if (callee.ctype->t != FUNCTION_TYPE) {
      assert(callee.ctype->t == POINTER_TYPE);
      CType *pointee_type = callee.ctype->u.pointee_type;
      assert(pointee_type->t == FUNCTION_TYPE);
      callee.ctype = pointee_type;
    }

    u32 callee_arity = callee.ctype->u.function.arity;

    // Struct returns are handled in the frontend, by adding a pointer
    // parameter at the start, and allocating a local in the caller.
    bool struct_ret = callee.ctype->u.function.return_type->t == STRUCT_TYPE;
    if (struct_ret) call_arity++;

    CType *return_type = callee.ctype->u.function.return_type;
    IrValue *arg_array =
        pool_alloc(&builder->module->pool, call_arity * sizeof(*arg_array));

    // If we have a struct return, then the IR parameters and the C
    // parameters are off by one. So we track "out_index" and "i"
    // separately here.
    u32 out_index = 0;
    IrValue local_for_ret_value;
    if (struct_ret) {
      local_for_ret_value =
          build_local(builder, c_type_to_ir_type(return_type));
      arg_array[0] = local_for_ret_value;
      out_index++;
    }

    arg = expr->u.function_call.arg_list;
    for (u32 i = 0; arg != NULL; i++, out_index++, arg = arg->next) {
      Term arg_term = ir_gen_expr(ctx, arg->expr, RVALUE_CONTEXT);

      if (i < callee_arity) {
        CType *arg_type = callee.ctype->u.function.arg_type_array[i];
        arg_term = convert_type(builder, arg_term, arg_type);
      }

      // @TODO: For structs we have a type mismatch in the IR here. We
      // always handle structs as pointers, so we pass a pointer even
      // though the type of the argument is $SomeStruct. This all works
      // because asm_gen expects it, but it's really messy, and should
      // be cleaned up.
      arg_array[out_index] = arg_term.value;
    }

    IrType return_ir_type =
        struct_ret ? (IrType){.t = IR_VOID} : c_type_to_ir_type(return_type);

    IrValue value = build_call(
        builder, callee.value, return_ir_type, call_arity, arg_array);
    if (struct_ret) value = local_for_ret_value;

    return (Term){
        .ctype = return_type,
        .value = value,
    };
  }
  case COMMA_EXPR:
    ir_gen_expr(ctx, expr->u.binary_op.arg1, RVALUE_CONTEXT);
    return ir_gen_expr(ctx, expr->u.binary_op.arg2, RVALUE_CONTEXT);
  case SIZEOF_TYPE_EXPR: {
    ASTDeclSpecifier *decl_specifier_list = expr->u.type->decl_specifier_list;
    ASTDeclarator *declarator = expr->u.type->declarator;

    CDecl cdecl;
    CType *decl_spec_type =
        decl_specifier_list_to_c_type(ctx, decl_specifier_list);
    decl_to_cdecl(ctx, decl_spec_type, declarator, &cdecl);

    CType *result_type = ctx->type_env.size_type;

    IrValue value = value_const_int(
        c_type_to_ir_type(result_type), c_type_size(cdecl.type));

    return (Term){.ctype = result_type, .value = value};
  }
  case LOGICAL_OR_EXPR:
  case LOGICAL_AND_EXPR: {
    bool is_or = t == LOGICAL_OR_EXPR;

    IrBlock *rhs_block = add_block(builder, is_or ? "or.rhs" : "and.rhs");
    IrBlock *after_block = add_block(builder, is_or ? "or.after" : "and.after");

    ASTExpr *lhs_expr = expr->u.binary_op.arg1;
    ASTExpr *rhs_expr = expr->u.binary_op.arg2;

    Term lhs = ir_gen_expr(ctx, lhs_expr, RVALUE_CONTEXT);
    assert(lhs.ctype->t == INTEGER_TYPE);
    if (is_or) {
      build_cond(builder, lhs.value, after_block, rhs_block);
    } else {
      build_cond(builder, lhs.value, rhs_block, after_block);
    }
    // ir_gen'ing the LHS expr may have changed the current block.
    IrBlock *lhs_resultant_block = builder->current_block;

    builder->current_block = rhs_block;
    Term rhs = ir_gen_expr(ctx, rhs_expr, RVALUE_CONTEXT);
    assert(rhs.ctype->t == INTEGER_TYPE);
    IrValue rhs_as_bool = build_cmp(
        builder, CMP_NEQ, rhs.value,
        value_const_int(c_type_to_ir_type(rhs.ctype), 0));
    build_jump(builder, after_block);

    // ir_gen'ing the RHS expr may have changed the current block.
    IrBlock *rhs_resultant_block = builder->current_block;

    builder->current_block = after_block;
    IrValue phi =
        build_phi(builder, c_type_to_ir_type(&ctx->type_env.int_type), 2);
    phi_set_param(
        phi, 0, lhs_resultant_block,
        value_const_int(
            c_type_to_ir_type(&ctx->type_env.int_type), is_or ? 1 : 0));
    phi_set_param(phi, 1, rhs_resultant_block, rhs_as_bool);

    return (Term){.ctype = &ctx->type_env.int_type, .value = phi};
  }
  case CONDITIONAL_EXPR: {
    IrBlock *then_block = add_block(builder, "ternary.then");
    IrBlock *else_block = add_block(builder, "ternary.else");
    IrBlock *after_block = add_block(builder, "ternary.after");

    ASTExpr *condition_expr = expr->u.ternary_op.arg1;
    Term condition_term = ir_gen_expr(ctx, condition_expr, RVALUE_CONTEXT);
    assert(condition_term.ctype->t == INTEGER_TYPE);
    build_cond(builder, condition_term.value, then_block, else_block);

    ASTExpr *then_expr = expr->u.ternary_op.arg2;
    builder->current_block = then_block;
    Term then_term = ir_gen_expr(ctx, then_expr, RVALUE_CONTEXT);
    then_term.ctype = decay_to_pointer(&ctx->type_env, then_term.ctype);
    // ir_gen'ing the "then" expr may have changed the current block.
    IrBlock *then_resultant_block = builder->current_block;

    ASTExpr *else_expr = expr->u.ternary_op.arg3;
    builder->current_block = else_block;
    Term else_term = ir_gen_expr(ctx, else_expr, RVALUE_CONTEXT);
    else_term.ctype = decay_to_pointer(&ctx->type_env, else_term.ctype);
    // ir_gen'ing the "else" expr may have changed the current block.
    IrBlock *else_resultant_block = builder->current_block;

    // @TODO: The rest of the conversions specified in C99 6.5.15.
    CType *result_type = then_term.ctype;
    if (is_arithmetic_type(then_term.ctype)
        && is_arithmetic_type(else_term.ctype)) {
      do_arithmetic_conversions_with_blocks(
          builder, &then_term, then_resultant_block, &else_term,
          else_resultant_block);
      result_type = then_term.ctype;
    } else if (
        then_term.ctype->t == POINTER_TYPE && else_term.ctype->t == POINTER_TYPE
        && (then_term.ctype->u.pointee_type->t == VOID_TYPE
            || else_term.ctype->u.pointee_type->t == VOID_TYPE)) {
      // IR pointers are untyped, so this is a no-op conversion.
      result_type = pointer_type(&ctx->type_env, &ctx->type_env.void_type);
    } else {
      assert(c_type_eq(then_term.ctype, else_term.ctype));
    }

    // We have to build the branches after doing conversions, since if any
    // conversions occur they may add instructions.
    builder->current_block = then_resultant_block;
    build_jump(builder, after_block);
    builder->current_block = else_resultant_block;
    build_jump(builder, after_block);

    builder->current_block = after_block;
    IrValue phi = build_phi(builder, then_term.value.type, 2);
    phi_set_param(phi, 0, then_resultant_block, then_term.value);
    phi_set_param(phi, 1, else_resultant_block, else_term.value);
    return (Term){.ctype = result_type, .value = phi};
  }
  case COMPOUND_EXPR: {
    CType *type = type_name_to_c_type(ctx, expr->u.compound.type_name);
    ASTInitializer initializer = {
        .t = BRACE_INITIALIZER,
        .u.initializer_element_list = expr->u.compound.initializer_element_list,
    };

    infer_array_size_from_initializer(ctx, &initializer, type);

    IrValue local = build_local(builder, c_type_to_ir_type(type));
    Term compound_value = {.value = local, .ctype = type};

    ir_gen_initializer(ctx, compound_value, &initializer);

    return compound_value;
  }
  case SIZEOF_EXPR_EXPR: {
    ASTExpr *sizeof_expr = expr->u.unary_arg;

    IrFunction *prev_function = builder->current_function;
    IrBlock *prev_block = builder->current_block;

    // @TODO: Maybe we should clear the function out after using it.
    builder->current_function = ctx->scratch_function;
    builder->current_block =
        *ARRAY_LAST(&builder->current_function->blocks, IrBlock *);

    Term term = ir_gen_expr(ctx, sizeof_expr, RVALUE_CONTEXT);

    builder->current_function = prev_function;
    builder->current_block = prev_block;

    u64 size = c_type_size(term.ctype);

    CType *result_type = ctx->type_env.size_type;
    IrValue value = value_const_int(c_type_to_ir_type(result_type), size);
    return (Term){.ctype = result_type, .value = value};
  }
  case CAST_EXPR: {
    CType *cast_type = type_name_to_c_type(ctx, expr->u.cast.cast_type);
    Term castee = ir_gen_expr(ctx, expr->u.cast.arg, RVALUE_CONTEXT);
    return convert_type(builder, castee, cast_type);
  }
  case BUILTIN_VA_ARG_EXPR: {
    Term va_list_term =
        ir_gen_expr(ctx, expr->u.builtin_va_arg.va_list_expr, RVALUE_CONTEXT);
    CType *arg_type =
        type_name_to_c_type(ctx, expr->u.builtin_va_arg.type_name);

    assert(
        va_list_term.ctype->t == ARRAY_TYPE
        || va_list_term.ctype->t == POINTER_TYPE);
    assert(va_list_term.ctype->u.array.elem_type->t == STRUCT_TYPE);

    // @TODO: Search through the type env and asert that the elem type is
    // the same as the type bound to "va_list".

    assert(arg_type->t == INTEGER_TYPE || arg_type->t == POINTER_TYPE);

    IrGlobal *global_builtin_va_arg_int =
        find_global_by_name(builder->module, "__builtin_va_arg_uint64");
    assert(global_builtin_va_arg_int != NULL);

    // @PORT: We want "uint64_t" here.
    IrType unsigned_long_type =
        c_type_to_ir_type(&ctx->type_env.unsigned_long_type);

    IrValue *args = malloc(sizeof *args * 1);
    args[0] = va_list_term.value;
    Term builtin_result = (Term){
        .ctype = &ctx->type_env.unsigned_long_type,
        .value = build_call(
            builder, value_global(global_builtin_va_arg_int),
            unsigned_long_type, 1, args),
    };
    return convert_type(builder, builtin_result, arg_type);
  }
  default: printf("%d\n", t); UNIMPLEMENTED;
  }
}

static Term ir_gen_assign_expr(IrGenContext *ctx, ASTExpr *expr, IrOp ir_op)
{
  Term left = ir_gen_expr(ctx, expr->u.binary_op.arg1, LVALUE_CONTEXT);
  Term right = ir_gen_expr(ctx, expr->u.binary_op.arg2, RVALUE_CONTEXT);
  return ir_gen_assign_op(ctx, left, right, ir_op, NULL);
}

Term ir_gen_assign_op(
    IrGenContext *ctx, Term left, Term right, IrOp ir_op,
    Term *pre_assign_value)
{
  Term result = right;

  if (left.ctype->t == STRUCT_TYPE || left.ctype->t == ARRAY_TYPE) {
    assert(c_type_eq(left.ctype, right.ctype));

    IrValue *memcpy_args =
        pool_alloc(&ctx->builder->module->pool, 3 * sizeof *memcpy_args);
    memcpy_args[0] = left.value;
    memcpy_args[1] = right.value;
    memcpy_args[2] = value_const_int(
        c_type_to_ir_type(ctx->type_env.int_ptr_type),
        size_of_ir_type(*left.ctype->u.strukt.ir_type));

    // @TODO: Open-code this for small sizes.
    build_call(
        ctx->builder, builtin_memcpy(ctx->builder), (IrType){.t = IR_POINTER},
        3, memcpy_args);
  } else {
    if (ir_op != OP_INVALID) {
      Term load = (Term){
          .ctype = left.ctype,
          .value = build_load(
              ctx->builder, left.value, c_type_to_ir_type(left.ctype)),
      };
      if (pre_assign_value != NULL) *pre_assign_value = load;

      result = ir_gen_binary_operator(ctx, load, right, ir_op);
    }

    result = convert_type(ctx->builder, result, left.ctype);
    build_store(ctx->builder, left.value, result.value);
  }

  return result;
}

static Term ir_gen_struct_field(
    IrBuilder *builder, Term struct_term, char *field_name, ExprContext context)
{
  assert(struct_term.value.type.t == IR_POINTER);

  CType *ctype = struct_term.ctype;
  if (struct_term.ctype->t == POINTER_TYPE) {
    ctype = ctype->u.pointee_type;
  }

  assert(ctype->t == STRUCT_TYPE);
  Array(CDecl) *fields = &ctype->u.strukt.fields;
  CDecl *selected_field = NULL;
  u32 field_number;
  for (u32 i = 0; i < fields->size; i++) {
    CDecl *field = ARRAY_REF(fields, CDecl, i);
    if (streq(field->name, field_name)) {
      selected_field = field;
      field_number = i;
      break;
    }
  }
  assert(selected_field != NULL);

  IrValue value = build_field(
      builder, struct_term.value, *ctype->u.strukt.ir_type, field_number);
  IrType *struct_ir_type = ctype->u.strukt.ir_type;
  assert(struct_ir_type->t == IR_STRUCT);
  IrType field_type = struct_ir_type->u.strukt.fields[field_number].type;

  if (context == RVALUE_CONTEXT && selected_field->type->t != STRUCT_TYPE
      && selected_field->type->t != ARRAY_TYPE) {
    value = build_load(builder, value, field_type);
  }

  return (Term){.ctype = selected_field->type, .value = value};
}

static Term ir_gen_deref(
    IrBuilder *builder, TypeEnv *type_env, Term pointer, ExprContext context)
{
  CType *pointer_type = decay_to_pointer(type_env, pointer.ctype);
  assert(pointer_type->t == POINTER_TYPE);
  CType *pointee_type = pointer_type->u.pointee_type;

  IrValue value;
  // Structs and arrays implicitly have their address taken.
  if (context == LVALUE_CONTEXT || pointee_type->t == STRUCT_TYPE
      || pointee_type->t == ARRAY_TYPE) {
    value = pointer.value;
  } else {
    assert(context == RVALUE_CONTEXT);

    value = build_load(builder, pointer.value, c_type_to_ir_type(pointee_type));
  }

  return (Term){.ctype = pointee_type, .value = value};
}

static Term ir_gen_cmp_expr(IrGenContext *ctx, ASTExpr *expr, IrCmp cmp)
{
  return ir_gen_cmp(
      ctx, ir_gen_expr(ctx, expr->u.binary_op.arg1, RVALUE_CONTEXT),
      ir_gen_expr(ctx, expr->u.binary_op.arg2, RVALUE_CONTEXT), cmp);
}

static Term ir_gen_cmp(IrGenContext *ctx, Term left, Term right, IrCmp cmp)
{
  left.ctype = decay_to_pointer(&ctx->type_env, left.ctype);
  right.ctype = decay_to_pointer(&ctx->type_env, right.ctype);

  bool left_is_ptr = left.ctype->t == POINTER_TYPE;
  bool right_is_ptr = right.ctype->t == POINTER_TYPE;

  if (left_is_ptr || right_is_ptr) {
    CType *int_type = &ctx->type_env.int_type;
    if (!left_is_ptr || !right_is_ptr) {
      Term *ptr_term, *other_term;
      if (left_is_ptr) {
        ptr_term = &left;
        other_term = &right;
      } else {
        ptr_term = &right;
        other_term = &left;
      }

      // "ptr <cmp> !ptr" is only valid if "!ptr" is zero, as a constant
      // zero integer expression is a null pointer constant.
      assert(other_term->ctype->t == INTEGER_TYPE);
      assert(other_term->value.t == IR_VALUE_CONST_INT);
      assert(other_term->value.u.const_int == 0);

      // Constant fold tautological comparisons between a global and NULL.
      if (ptr_term->value.t == IR_VALUE_GLOBAL) {
        return (Term){
            .ctype = int_type,
            .value =
                value_const_int(c_type_to_ir_type(int_type), cmp == CMP_NEQ),
        };
      }

      *other_term = convert_type(ctx->builder, *other_term, ptr_term->ctype);
    } else if (
        left.value.t == IR_VALUE_GLOBAL && right.value.t == IR_VALUE_GLOBAL) {
      // Constant fold tautological comparisons between global.
      return (Term){
          .ctype = int_type,
          .value = value_const_int(c_type_to_ir_type(int_type), cmp == CMP_NEQ),
      };
    }
  } else {
    do_arithmetic_conversions(ctx->builder, &left, &right);

    assert(c_type_eq(left.ctype, right.ctype));
    assert(left.ctype->t == INTEGER_TYPE);

    // @NOTE: We always pass the signed comparison ops to this function.
    // Not because we specifically want a signed comparison. Just because
    // all of the IrCmp members have explicit signedness. The caller
    // expects ir_gen_cmp to adjust as necessary based on the signedness of
    // the arguments after conversion.
    if (!left.ctype->u.integer.is_signed) {
      switch (cmp) {
      case CMP_SGT: cmp = CMP_UGT; break;
      case CMP_SGTE: cmp = CMP_UGTE; break;
      case CMP_SLT: cmp = CMP_ULT; break;
      case CMP_SLTE: cmp = CMP_ULTE; break;
      default: break;
      }
    }
  }

  CType *result_type = &ctx->type_env.int_type;
  IrValue value = build_cmp(ctx->builder, cmp, left.value, right.value);
  return (Term){.ctype = result_type, .value = value};
}

static Term ir_gen_binary_expr(IrGenContext *ctx, ASTExpr *expr, IrOp ir_op)
{
  return ir_gen_binary_operator(
      ctx, ir_gen_expr(ctx, expr->u.binary_op.arg1, RVALUE_CONTEXT),
      ir_gen_expr(ctx, expr->u.binary_op.arg2, RVALUE_CONTEXT), ir_op);
}

static Term ir_gen_binary_operator(
    IrGenContext *ctx, Term left, Term right, IrOp ir_op)
{
  if (ir_op == OP_ADD) return ir_gen_add(ctx, left, right);
  if (ir_op == OP_SUB) return ir_gen_sub(ctx, left, right);

  left.ctype = decay_to_pointer(&ctx->type_env, left.ctype);
  right.ctype = decay_to_pointer(&ctx->type_env, right.ctype);

  do_arithmetic_conversions(ctx->builder, &left, &right);

  CType *result_type = left.ctype;
  IrValue value =
      build_binary_instr(ctx->builder, ir_op, left.value, right.value);
  return (Term){.ctype = result_type, .value = value};
}

static Term ir_gen_add(IrGenContext *ctx, Term left, Term right)
{
  left.ctype = decay_to_pointer(&ctx->type_env, left.ctype);
  right.ctype = decay_to_pointer(&ctx->type_env, right.ctype);

  bool left_is_pointer = left.ctype->t == POINTER_TYPE;
  bool right_is_pointer = right.ctype->t == POINTER_TYPE;

  if (is_arithmetic_type(left.ctype) && is_arithmetic_type(right.ctype)) {
    do_arithmetic_conversions(ctx->builder, &left, &right);

    // @TODO: Determine type correctly
    CType *type = left.ctype;
    IrValue value;
    if (type->t == INTEGER_TYPE) {
      value = build_binary_instr(ctx->builder, OP_ADD, left.value, right.value);
    } else {
      value =
          build_binary_instr(ctx->builder, OP_ADDF, left.value, right.value);
    }

    return (Term){
        .ctype = type,
        .value = value,
    };
  } else if (left_is_pointer ^ right_is_pointer) {
    Term pointer = left_is_pointer ? left : right;
    Term other = left_is_pointer ? right : left;
    assert(other.ctype->t == INTEGER_TYPE);

    CType *result_type = pointer.ctype;
    CType *pointee_type = result_type->u.pointee_type;

    // @TODO: Extend OP_FIELD to non-constant field numbers?
    if (other.value.t == IR_VALUE_CONST_INT) {
      u64 offset = other.value.u.const_int;
      if (pointee_type->t == ARRAY_TYPE) {
        // @NOTE: We have to use the IR type size in case the inner
        // elem is itself an array of arrays.
        offset *= pointee_type->u.array.ir_type->u.array.size;
      }

      IrType array = c_type_to_ir_type(
          array_type(ctx->builder, &ctx->type_env, pointee_type));
      return (Term){
          .ctype = result_type,
          .value = build_field(ctx->builder, pointer.value, array, offset),
      };
    }

    // @TODO: Determine type correctly
    IrType pointer_int_type = c_type_to_ir_type(ctx->type_env.int_ptr_type);

    IrValue zext =
        build_type_instr(ctx->builder, OP_ZEXT, other.value, pointer_int_type);
    IrValue ptr_to_int = build_type_instr(
        ctx->builder, OP_CAST, pointer.value, pointer_int_type);
    IrValue addend = build_binary_instr(
        ctx->builder, OP_MUL, zext,
        value_const_int(pointer_int_type, c_type_size(pointee_type)));

    IrValue sum = build_binary_instr(ctx->builder, OP_ADD, ptr_to_int, addend);
    IrValue int_to_ptr = build_type_instr(
        ctx->builder, OP_CAST, sum, c_type_to_ir_type(result_type));

    return (Term){
        .ctype = result_type,
        .value = int_to_ptr,
    };
  } else {
    UNIMPLEMENTED;
  }
}

static Term ir_gen_sub(IrGenContext *ctx, Term left, Term right)
{
  left.ctype = decay_to_pointer(&ctx->type_env, left.ctype);
  right.ctype = decay_to_pointer(&ctx->type_env, right.ctype);

  bool left_is_pointer = left.ctype->t == POINTER_TYPE;
  bool right_is_pointer = right.ctype->t == POINTER_TYPE;

  if (left.ctype->t == INTEGER_TYPE && right.ctype->t == INTEGER_TYPE) {
    do_arithmetic_conversions(ctx->builder, &left, &right);

    IrValue value =
        build_binary_instr(ctx->builder, OP_SUB, left.value, right.value);

    // @TODO: Determine type correctly
    return (Term){
        .ctype = left.ctype,
        .value = value,
    };
  } else if (left_is_pointer && right_is_pointer) {
    CType *pointee_type = left.ctype->u.pointee_type;

    // @TODO: Determine type correctly
    IrType pointer_int_type = c_type_to_ir_type(ctx->type_env.int_ptr_type);

    // @TODO: This should be ptrdiff_t
    CType *result_c_type = &ctx->type_env.int_type;

    IrValue left_int =
        build_type_instr(ctx->builder, OP_CAST, left.value, pointer_int_type);
    IrValue right_int =
        build_type_instr(ctx->builder, OP_CAST, right.value, pointer_int_type);
    IrValue diff =
        build_binary_instr(ctx->builder, OP_SUB, left_int, right_int);
    IrValue cast = build_type_instr(
        ctx->builder, OP_CAST, diff, c_type_to_ir_type(result_c_type));
    IrValue scaled = build_binary_instr(
        ctx->builder, OP_DIV, cast,
        value_const_int(cast.type, c_type_size(pointee_type)));

    return (Term){
        .ctype = result_c_type,
        .value = scaled,
    };
  } else if (left_is_pointer && (right.ctype->t == INTEGER_TYPE)) {
    // @TODO: This block is almost identical to the corresponding block in
    // ir_gen_add, except for OP_SUB instead of OP_ADD. Factor out?
    assert(right.ctype->t == INTEGER_TYPE);

    CType *result_type = left.ctype;
    CType *pointee_type = result_type->u.pointee_type;

    // @TODO: Determine type correctly
    IrType pointer_int_type = c_type_to_ir_type(ctx->type_env.int_ptr_type);

    IrValue zext =
        build_type_instr(ctx->builder, OP_ZEXT, right.value, pointer_int_type);
    IrValue ptr_to_int =
        build_type_instr(ctx->builder, OP_CAST, left.value, pointer_int_type);
    IrValue subtrahend = build_binary_instr(
        ctx->builder, OP_MUL, zext,
        value_const_int(pointer_int_type, c_type_size(pointee_type)));

    IrValue sum =
        build_binary_instr(ctx->builder, OP_SUB, ptr_to_int, subtrahend);
    IrValue int_to_ptr = build_type_instr(
        ctx->builder, OP_CAST, sum, c_type_to_ir_type(result_type));

    return (Term){
        .ctype = result_type,
        .value = int_to_ptr,
    };
  } else {
    UNIMPLEMENTED;
  }
}

static Term ir_gen_inc_dec(IrGenContext *ctx, ASTExpr *expr)
{
  IrOp op;
  switch (expr->t) {
  case PRE_INCREMENT_EXPR:
  case POST_INCREMENT_EXPR: op = OP_ADD; break;
  case PRE_DECREMENT_EXPR:
  case POST_DECREMENT_EXPR: op = OP_SUB; break;
  default: UNREACHABLE;
  }
  bool is_pre = expr->t == PRE_INCREMENT_EXPR || expr->t == PRE_DECREMENT_EXPR;

  Term ptr = ir_gen_expr(ctx, expr->u.unary_arg, LVALUE_CONTEXT);
  // @TODO: Correct type
  CType *one_type = &ctx->type_env.int_type;
  Term one = (Term){
      .value = value_const_int(c_type_to_ir_type(one_type), 1),
      .ctype = one_type,
  };
  Term pre_assign_value;
  Term incremented = ir_gen_assign_op(ctx, ptr, one, op, &pre_assign_value);

  if (is_pre) {
    return incremented;
  } else {
    return pre_assign_value;
  }
}

IrConst *eval_constant_expr(IrGenContext *ctx, ASTExpr *expr)
{
  u32 num_blocks = 0, num_instrs = 0;
  if (ctx->builder->current_function != NULL) {
    num_blocks = ctx->builder->current_function->blocks.size;
  }
  if (ctx->builder->current_block != NULL) {
    num_instrs = ctx->builder->current_block->instrs.size;
  }

  Term term = ir_gen_expr(ctx, expr, CONST_CONTEXT);

  // Quick sanity check - this is a constant expression, so we shouldn't have
  // added any instructions or blocks.
  if (ctx->builder->current_function != NULL) {
    assert(ctx->builder->current_function->blocks.size == num_blocks);
  }
  if (ctx->builder->current_block != NULL) {
    assert(ctx->builder->current_block->instrs.size == num_instrs);
  }

  switch (term.value.t) {
  case IR_VALUE_CONST_INT:
    return add_int_const(
        ctx->builder, c_type_to_ir_type(term.ctype), term.value.u.const_int);
  case IR_VALUE_CONST_FLOAT:
    return add_float_const(
        ctx->builder, c_type_to_ir_type(term.ctype), term.value.u.const_float);
  case IR_VALUE_GLOBAL:
    return add_global_const(ctx->builder, term.value.u.global);
  case IR_VALUE_ARG:
  case IR_VALUE_INSTR: UNREACHABLE;
  }

  UNREACHABLE;
}

static CType *type_name_to_c_type(IrGenContext *ctx, ASTTypeName *type_name)
{
  CDecl cdecl;
  CType *decl_spec_type =
      decl_specifier_list_to_c_type(ctx, type_name->decl_specifier_list);
  decl_to_cdecl(ctx, decl_spec_type, type_name->declarator, &cdecl);
  assert(cdecl.name == NULL);
  return cdecl.type;
}