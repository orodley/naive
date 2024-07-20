#ifndef NAIVE_IR_GEN_STATEMENT_H_
#define NAIVE_IR_GEN_STATEMENT_H_

#include "ir.h"
#include "ir_gen/context.h"
#include "parse.h"

void ir_gen_statement(IrGenContext *ctx, ASTStatement *statement);

#endif