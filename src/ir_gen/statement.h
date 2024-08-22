#ifndef NAIVE_IR_GEN_STATEMENT_H_
#define NAIVE_IR_GEN_STATEMENT_H_

#include "ir_gen/context.h"
#include "syntax/ast.h"

void ir_gen_statement(IrGenContext *ctx, ASTStatement *statement);

#endif