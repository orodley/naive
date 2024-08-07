#ifndef NAIVE_IR_GEN_IR_GEN_H
#define NAIVE_IR_GEN_IR_GEN_H

#include "ir.h"
#include "syntax/parse.h"

IrModule ir_gen(ASTToplevel *toplevel);

#endif
