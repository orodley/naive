#ifndef NAIVE_IR_GEN_INITIALIZER_H_
#define NAIVE_IR_GEN_INITIALIZER_H_

#include "ir_gen/c_type.h"
#include "ir_gen/context.h"
#include "parse.h"

// @TODO: We should special-case zero-initializers, so that we don't need huge
// amounts of memory to store large zeroed arrays.
typedef struct CInitializer
{
  CType *type;

  enum
  {
    C_INIT_COMPOUND,
    C_INIT_LEAF,
  } t;

  union
  {
    IrValue leaf_value;
    struct CInitializer *sub_elems;
  } u;
} CInitializer;

void make_c_initializer(
    IrGenContext *ctx, Pool *pool, CType *type, ASTInitializer *init,
    bool const_context, CInitializer *c_init);
void ir_gen_initializer(IrGenContext *ctx, Term to_init, ASTInitializer *init);
void infer_array_size_from_initializer(
    IrGenContext *ctx, ASTInitializer *init, CType *type);
IrConst *const_gen_c_init(IrBuilder *builder, CInitializer *c_init);
IrConst *zero_initializer(IrBuilder *builder, CType *ctype);

#endif