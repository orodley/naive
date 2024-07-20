#ifndef NAIVE_IR_GEN_EXPR_H_
#define NAIVE_IR_GEN_EXPR_H_

#include "ir_gen/context.h"

typedef enum ExprContext
{
  LVALUE_CONTEXT,
  RVALUE_CONTEXT,
  CONST_CONTEXT,
} ExprContext;

Term ir_gen_expr(IrGenContext *ctx, ASTExpr *expr, ExprContext context);
Term ir_gen_assign_op(
    IrGenContext *ctx, Term left, Term right, IrOp ir_op,
    Term *pre_assign_value);
IrConst *eval_constant_expr(IrGenContext *ctx, ASTExpr *expr);

#endif