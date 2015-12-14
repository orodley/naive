#ifndef NAIVE_IR_H_
#define NAIVE_IR_H_

#include "array.h"
#include "misc.h"

typedef struct TransUnit
{
	Array functions;
} TransUnit;

typedef struct Block
{
	const char *name;

	u32 arity;
	Array instrs;
} Block;

typedef struct Function
{
	const char *name;

	Block entry_block;
	Block ret_block;
} Function;

typedef struct Builder
{
	Function *function;
	Block *current_block;
} Builder;


typedef struct IrType
{
	u32 bit_width;
} IrType;

typedef enum Op
{
	OP_CONST,
	OP_BRANCH,
} Op;

typedef struct Instr
{
	IrType type;
	Op op;
	u32 id;

	union
	{
		i64 constant;
		struct
		{
			Block *target_block;
			struct Instr *argument;
		} branch;
	} val;
} Instr;

void trans_unit_init(TransUnit *tu);
Function *trans_unit_add_function(TransUnit *tu, const char *name, u32 arity);
void dump_trans_unit(TransUnit *tu);

void builder_init(Builder *builder);
Instr *build_branch(Builder *builder, Block *block, Instr *value);

Instr *build_const(Builder *builder, i64 value);

#endif
