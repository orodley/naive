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
	struct Arg *args;
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
	OP_BRANCH,
} Op;

typedef struct Value
{
	enum
	{
		VALUE_CONST,
		VALUE_ARG,
		VALUE_INSTR,
	} kind;

	union
	{
		i64 constant;
		struct Instr *instr;
		struct Arg *arg;
	} val;
} Value;

typedef struct Instr
{
	u32 id;
	IrType type;
	Op op;

	union
	{
		i64 constant;
		struct
		{
			Block *target_block;
			struct Value argument;
		} branch;
	} val;
} Instr;

typedef struct Arg
{
	u32 index;
	IrType type;
} Arg;

void trans_unit_init(TransUnit *tu);
Function *trans_unit_add_function(TransUnit *tu, const char *name,
		IrType *return_type, u32 arity, IrType *arg_types);
void dump_trans_unit(TransUnit *tu);

void builder_init(Builder *builder);
Instr *build_branch(Builder *builder, Block *block, Value value);

Value value_const(i64 value);

#endif
