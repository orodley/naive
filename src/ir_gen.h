#ifndef NAIVE_IR_GEN_H
#define NAIVE_IR_GEN_H

#include "ir.h"
#include "parse.h"

void ir_gen_toplevel(IrBuilder *builder, ASTToplevel *toplevel);

#endif
