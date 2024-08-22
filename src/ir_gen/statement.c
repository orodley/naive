#include "ir_gen/statement.h"

#include "ir.h"
#include "ir_gen/context.h"
#include "ir_gen/convert.h"
#include "ir_gen/decl.h"
#include "ir_gen/expr.h"
#include "ir_gen/initializer.h"
#include "macros.h"

static void add_decl_to_scope(IrGenContext *ctx, ASTDecl *decl);

void ir_gen_statement(IrGenContext *ctx, ASTStatement *statement)
{
  IrBuilder *builder = ctx->builder;

  switch (statement->t) {
  case COMPOUND_STATEMENT: {
    Scope block_scope;
    push_scope(ctx, &block_scope);

    ASTBlockItem *block_item_list = statement->u.block_item_list;
    while (block_item_list != NULL) {
      switch (block_item_list->t) {
      case BLOCK_ITEM_DECL: {
        add_decl_to_scope(ctx, block_item_list->u.decl);
        break;
      }
      case BLOCK_ITEM_STATEMENT:
        ir_gen_statement(ctx, block_item_list->u.statement);
        break;
      }

      block_item_list = block_item_list->next;
    }

    pop_scope(ctx);

    break;
  }
  case EXPR_STATEMENT: {
    ir_gen_expr(ctx, statement->u.expr, RVALUE_CONTEXT);
    break;
  }
  case RETURN_STATEMENT: {
    if (statement->u.expr == NULL) {
      build_nullary_instr(builder, OP_RET_VOID, (IrType){.t = IR_VOID});
    } else {
      Term term = ir_gen_expr(ctx, statement->u.expr, RVALUE_CONTEXT);
      if (term.ctype->t == STRUCT_TYPE) {
        // If we return a struct, the first arg is a pointer to space
        // the caller allocated for the struct.
        Term caller_ptr = (Term){
            .ctype = term.ctype,
            .value = value_arg(0, (IrType){.t = IR_POINTER}),
        };
        ir_gen_assign_op(ctx, caller_ptr, term, OP_INVALID, NULL);
        build_nullary_instr(builder, OP_RET_VOID, (IrType){.t = IR_VOID});
      } else {
        CType *return_type = ctx->current_function_type->u.function.return_type;
        Term converted = convert_type(builder, term, return_type);
        build_unary_instr(builder, OP_RET, converted.value);
      }
    }
    break;
  }
  case IF_STATEMENT: {
    ASTStatement *then_statement = statement->u.if_statement.then_statement;
    ASTStatement *else_statement = statement->u.if_statement.else_statement;

    ASTExpr *condition_expr = statement->u.if_statement.condition;
    Term condition_term = ir_gen_expr(ctx, condition_expr, RVALUE_CONTEXT);
    switch (condition_term.ctype->t) {
    case INTEGER_TYPE: break;
    case POINTER_TYPE: {
      CType *int_ptr_type = ctx->type_env.int_ptr_type;
      condition_term.ctype = int_ptr_type;
      condition_term.value = build_type_instr(
          builder, OP_CAST, condition_term.value,
          c_type_to_ir_type(int_ptr_type));
      break;
    }
    default: UNIMPLEMENTED("if condition of type %d", condition_term.ctype->t);
    }
    if (condition_term.ctype->t != INTEGER_TYPE) {
      emit_fatal_error_no_loc("if condition must be of integer type");
    }

    IrBlock *before_block = builder->current_block;

    IrBlock *then_block = add_block(builder, "if.then");
    builder->current_block = then_block;
    ir_gen_statement(ctx, then_statement);
    IrBlock *then_resultant_block = builder->current_block;

    IrBlock *else_block = NULL;
    IrBlock *else_resultant_block = NULL;
    if (else_statement != NULL) {
      else_block = add_block(builder, "if.else");
      builder->current_block = else_block;
      ir_gen_statement(ctx, else_statement);
      else_resultant_block = builder->current_block;
    }

    IrBlock *after_block = add_block(builder, "if.after");

    builder->current_block = before_block;
    if (else_statement == NULL) {
      build_cond(builder, condition_term.value, then_block, after_block);
    } else {
      build_cond(builder, condition_term.value, then_block, else_block);
    }

    builder->current_block = then_resultant_block;
    build_jump(builder, after_block);
    if (else_statement != NULL) {
      builder->current_block = else_resultant_block;
      build_jump(builder, after_block);
    }

    builder->current_block = after_block;
    break;
  }
  case SWITCH_STATEMENT: {
    Array(SwitchCase) prev_case_labels = ctx->case_labels;
    ctx->case_labels = EMPTY_ARRAY;

    IrBlock *switch_entry = builder->current_block;
    u32 before_body = builder->current_function->blocks.size;
    IrBlock *after = add_block(builder, "switch.after");
    IrBlock *prev_break_target = ctx->break_target;
    ctx->break_target = after;

    builder->current_block = add_block(builder, "switch.body");
    ir_gen_statement(ctx, statement->u.expr_and_statement.statement);
    build_jump(builder, after);

    builder->current_block = switch_entry;
    Term switch_value =
        ir_gen_expr(ctx, statement->u.expr_and_statement.expr, RVALUE_CONTEXT);
    if (switch_value.ctype->t != INTEGER_TYPE) {
      emit_fatal_error_no_loc("switch value must be of integer type");
    }

    i32 default_index = -1;

    for (u32 i = 0; i < ctx->case_labels.size; i++) {
      SwitchCase *label = ARRAY_REF(&ctx->case_labels, SwitchCase, i);
      if (label->is_default) {
        default_index = i;
      } else {
        IrBlock *next = pool_alloc(&builder->module->pool, sizeof *next);
        block_init(next, "switch.cmp", builder->current_function->blocks.size);

        IrValue cmp = build_cmp(
            builder, CMP_EQ, switch_value.value,
            value_const_int(switch_value.value.type, label->value->u.integer));
        build_cond(builder, cmp, label->block, next);
        builder->current_block = next;

        // @TODO: Shift them all down at once rather than one-by-one.
        *ARRAY_INSERT(
            &builder->current_function->blocks, IrBlock *, before_body) = next;
        before_body++;
      }
    }

    if (default_index == -1) {
      build_jump(builder, after);
    } else {
      SwitchCase *default_case =
          ARRAY_REF(&ctx->case_labels, SwitchCase, default_index);
      build_jump(builder, default_case->block);
    }
    builder->current_block = after;

    ctx->break_target = prev_break_target;
    array_free(&ctx->case_labels);
    ctx->case_labels = prev_case_labels;
    break;
  }
  case CASE_STATEMENT: {
    // @TODO: Ensure we're inside a switch statement.
    IrBlock *case_block = add_block(builder, "switch.case");
    build_jump(builder, case_block);
    builder->current_block = case_block;

    ir_gen_statement(ctx, statement->u.expr_and_statement.statement);

    SwitchCase *switch_case = ARRAY_APPEND(&ctx->case_labels, SwitchCase);
    switch_case->is_default = false;
    switch_case->value =
        eval_constant_expr(ctx, statement->u.expr_and_statement.expr);
    switch_case->block = case_block;
    break;
  }
  case LABELED_STATEMENT: {
    char *label_name = statement->u.labeled_statement.label_name;
    IrBlock *label_block = add_block(builder, label_name);
    build_jump(builder, label_block);
    builder->current_block = label_block;
    if (streq(label_name, "default")) {
      SwitchCase *default_case = ARRAY_APPEND(&ctx->case_labels, SwitchCase);
      default_case->is_default = true;
      default_case->block = label_block;
    } else {
      GotoLabel *label = ARRAY_APPEND(&ctx->goto_labels, GotoLabel);
      label->name = label_name;
      label->block = label_block;
    }

    ir_gen_statement(ctx, statement->u.labeled_statement.statement);

    break;
  }
  case WHILE_STATEMENT: {
    IrBlock *pre_header = add_block(builder, "while.ph");
    // @NOTE: We allocate this now, but only add it to the function later.
    // This is because we need it to exist as break_target while ir_gen'ing
    // the body, but we want it to be after the body, so the blocks are
    // laid out better.
    IrBlock *after = pool_alloc(&builder->module->pool, sizeof *after);

    ASTExpr *condition_expr = statement->u.expr_and_statement.expr;
    ASTStatement *body_statement = statement->u.expr_and_statement.statement;

    build_jump(builder, pre_header);
    builder->current_block = pre_header;
    Term condition_term = ir_gen_expr(ctx, condition_expr, RVALUE_CONTEXT);
    if (condition_term.ctype->t != INTEGER_TYPE) {
      emit_fatal_error_no_loc("while condition must be of integer type");
    }

    IrBlock *body = add_block(builder, "while.body");
    build_cond(builder, condition_term.value, body, after);

    IrBlock *prev_break_target = ctx->break_target;
    IrBlock *prev_continue_target = ctx->continue_target;
    ctx->break_target = after;
    ctx->continue_target = pre_header;
    builder->current_block = body;

    ir_gen_statement(ctx, body_statement);

    build_jump(builder, pre_header);
    ctx->break_target = prev_break_target;
    ctx->continue_target = prev_continue_target;

    *ARRAY_APPEND(&builder->current_function->blocks, IrBlock *) = after;
    block_init(
        after, "while.after", builder->current_function->blocks.size - 1);
    builder->current_block = after;

    break;
  }
  case DO_WHILE_STATEMENT: {
    IrBlock *pre_header = add_block(builder, "do_while.ph");
    IrBlock *body = add_block(builder, "do_while.body");
    IrBlock *after = add_block(builder, "do_while.after");

    ASTExpr *condition_expr = statement->u.expr_and_statement.expr;
    ASTStatement *body_statement = statement->u.expr_and_statement.statement;

    build_jump(builder, body);
    builder->current_block = pre_header;
    Term condition_term = ir_gen_expr(ctx, condition_expr, RVALUE_CONTEXT);

    if (condition_term.ctype->t != INTEGER_TYPE) {
      emit_fatal_error_no_loc("do-while condition must be of integer type");
    }
    build_cond(builder, condition_term.value, body, after);

    IrBlock *prev_break_target = ctx->break_target;
    IrBlock *prev_continue_target = ctx->continue_target;
    ctx->break_target = after;
    ctx->continue_target = pre_header;
    builder->current_block = body;

    ir_gen_statement(ctx, body_statement);

    build_jump(builder, pre_header);
    ctx->break_target = prev_break_target;
    ctx->continue_target = prev_continue_target;

    builder->current_block = after;

    break;
  }
  case FOR_STATEMENT: {
    IrBlock *pre_header = add_block(builder, "for.ph");
    IrBlock *body = add_block(builder, "for.body");
    // @NOTE: We allocate these now, but only add it to the function later.
    // This is because we need them to exist as break_target and
    // continue_target while ir_gen'ing the body, but we want them to be
    // after the body so the blocks are laid out better.
    IrBlock *update = pool_alloc(&builder->module->pool, sizeof *update);
    IrBlock *after = pool_alloc(&builder->module->pool, sizeof *after);

    Scope init_scope;
    push_scope(ctx, &init_scope);

    ASTForStatement *f = &statement->u.for_statement;
    switch (f->init_type) {
    case FOR_INIT_DECL: add_decl_to_scope(ctx, f->init.decl); break;
    case FOR_INIT_EXPR:
      if (f->init.expr != NULL) ir_gen_expr(ctx, f->init.expr, RVALUE_CONTEXT);
      break;
    }

    build_jump(builder, pre_header);
    builder->current_block = pre_header;
    Term condition_term;
    if (f->condition != NULL) {
      condition_term = ir_gen_expr(ctx, f->condition, RVALUE_CONTEXT);
    } else {
      condition_term = (Term){
          .value =
              value_const_int(c_type_to_ir_type(&ctx->type_env.int_type), 1),
          .ctype = &ctx->type_env.int_type,
      };
    }

    if (condition_term.ctype->t != INTEGER_TYPE) {
      emit_fatal_error_no_loc("for condition must be of integer type");
    }
    build_cond(builder, condition_term.value, body, after);

    builder->current_block = body;
    IrBlock *prev_break_target = ctx->break_target;
    IrBlock *prev_continue_target = ctx->continue_target;
    ctx->break_target = after;
    ctx->continue_target = update;

    ir_gen_statement(ctx, f->body);
    build_jump(builder, update);
    builder->current_block = update;

    *ARRAY_APPEND(&builder->current_function->blocks, IrBlock *) = update;
    block_init(
        update, "for.update", builder->current_function->blocks.size - 1);

    ctx->break_target = prev_break_target;
    ctx->continue_target = prev_continue_target;

    if (f->update_expr != NULL)
      ir_gen_expr(ctx, f->update_expr, RVALUE_CONTEXT);

    build_jump(builder, pre_header);

    pop_scope(ctx);
    builder->current_block = after;

    *ARRAY_APPEND(&builder->current_function->blocks, IrBlock *) = after;
    block_init(after, "for.after", builder->current_function->blocks.size - 1);

    break;
  }
  case GOTO_STATEMENT: {
    IrInstr *branch_instr = build_jump(builder, NULL);
    GotoFixup *fixup = ARRAY_APPEND(&ctx->goto_fixups, GotoFixup);
    fixup->label_name = statement->u.goto_label;
    fixup->instr = branch_instr;

    builder->current_block = add_block(builder, "goto.after");
    break;
  }
  case BREAK_STATEMENT:
    if (ctx->break_target == NULL) {
      emit_fatal_error_no_loc("break statement must be within loop or switch");
    }
    build_jump(builder, ctx->break_target);
    break;
  case CONTINUE_STATEMENT:
    if (ctx->continue_target == NULL) {
      emit_fatal_error_no_loc("continue statement must be within loop");
    }
    build_jump(builder, ctx->continue_target);
    break;
  case EMPTY_STATEMENT: break;
  }
}

static void add_decl_to_scope(IrGenContext *ctx, ASTDecl *decl)
{
  ASTInitDeclarator *init_declarator = decl->init_declarators;
  CType *decl_spec_type =
      decl_specifier_list_to_c_type(ctx, decl->decl_specifier_list);

  while (init_declarator != NULL) {
    CDecl cdecl;
    decl_to_cdecl(ctx, decl_spec_type, init_declarator->declarator, &cdecl);
    infer_array_size_from_initializer(
        ctx, init_declarator->initializer, cdecl.type);

    Binding *binding = ARRAY_APPEND(&ctx->scope->bindings, Binding);
    cdecl_to_binding(ctx->builder, &cdecl, binding);

    ASTInitializer *initializer = init_declarator->initializer;
    if (initializer != NULL) {
      // @TODO: This case isn't really necessary, as it should work
      // through ir_gen_initializer. However, ir_gen_initializer
      // currently unconditionally memsets to zero before assigning to
      // fields, which just feels gross to do for every local scalar
      // value. Once we've fixed this, we should remove this case.
      if (initializer->t == EXPR_INITIALIZER
          && !(
              initializer->u.expr->t == STRING_LITERAL_EXPR
              && cdecl.type->t == ARRAY_TYPE)) {
        Term init_term = ir_gen_expr(ctx, initializer->u.expr, RVALUE_CONTEXT);
        ir_gen_assign_op(ctx, binding->term, init_term, OP_INVALID, NULL);
      } else {
        ir_gen_initializer(ctx, binding->term, initializer);
      }
    }

    init_declarator = init_declarator->next;
  }
}
