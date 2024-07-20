#ifndef NAIVE_IR_GEN_CONVERT_H_
#define NAIVE_IR_GEN_CONVERT_H_

#include "ir_gen/c_type.h"
#include "ir_gen/context.h"

Term convert_type(IrBuilder *builder, Term term, CType *target_type);
void do_arithmetic_conversions(IrBuilder *builder, Term *left, Term *right);
void do_arithmetic_conversions_with_blocks(
    IrBuilder *builder, Term *left, IrBlock *left_block, Term *right,
    IrBlock *right_block);

#endif