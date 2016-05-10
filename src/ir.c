#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "asm.h"
#include "asm_gen.h"
#include "ir.h"
#include "misc.h"

void trans_unit_init(TransUnit *tu)
{
	ARRAY_INIT(&tu->functions, IrFunction, 10);
}

static inline void block_init(Block *block, char *name,
		u32 arity, IrType *arg_types)
{
	block->name = name;
	block->arity = arity;
	block->args = malloc(sizeof(*block->args) * arity);
	for (u32 i = 0; i < arity; i++) {
		Arg *arg = &block->args[i];
		arg->index = i;
		arg->type = arg_types[i];
	}

	ARRAY_INIT(&block->instrs, IrInstr, 10);
}

IrFunction *trans_unit_add_function(TransUnit *tu, char *name,
		IrType return_type, u32 arity, IrType *arg_types)
{
	IrFunction *new_func = ARRAY_APPEND(&tu->functions, IrFunction);

	new_func->name = name;
	block_init(&new_func->entry_block, "entry", arity, arg_types);
	block_init(&new_func->ret_block, "ret", 1, &return_type);

	return new_func;
}

extern inline IrType ir_function_return_type(IrFunction *f);

bool ir_type_eq(IrType a, IrType b)
{
	return a.bit_width == b.bit_width;
}

static inline void dump_type(IrType type)
{
	printf("i%d", type.bit_width);
}

static void dump_value(Value value)
{
	switch (value.kind) {
	case VALUE_CONST: 
		printf("%" PRId64, value.val.constant);
		break;
	case VALUE_ARG:
		printf("#%d", value.val.arg->index);
		break;
	case VALUE_INSTR:
		printf("#%d", value.val.instr->id);
	}
}

static void dump_instr(IrInstr *instr)
{
	switch (instr->op) {
	case OP_BRANCH:
		printf("branch(%s, ", instr->val.branch.target_block->name);
		dump_value(instr->val.branch.argument);
		break;
	case OP_BIT_XOR:
		fputs("bit_xor(", stdout);
		dump_value(instr->val.binary_op.arg1);
		putchar(',');
		dump_value(instr->val.binary_op.arg2);
		break;
	}

	puts(")");
}

void dump_trans_unit(TransUnit *tu)
{
	Array(IrFunction) *functions = &tu->functions;
	for (u32 i = 0; i < functions->size; i++) {
		IrFunction *f = ARRAY_REF(functions, IrFunction, i);

		dump_type(ir_function_return_type(f));
		printf(" %s(", f->name);
		Arg *args = f->entry_block.args;
		for (u32 i = 0; i < f->entry_block.arity; i++) {
			IrType arg_type = args[i].type;
			dump_type(arg_type);

			if (i != f->entry_block.arity - 1)
				fputs(", ", stdout);
		}

		puts(")\n{");

		Block *block = &f->entry_block;
		for (;;) {
			printf("%s(%d):\n", block->name, block->arity);

			Array(IrInstr) *instrs = &block->instrs;
			for (u32 i = 0; i < instrs->size; i++) {
				IrInstr *instr = ARRAY_REF(instrs, IrInstr, i);
				putchar('\t');
				dump_instr(instr);
			}

			if (instrs->size == 0)
				break;

			IrInstr *last_instr = ARRAY_REF(instrs, IrInstr, instrs->size - 1);
			assert(last_instr->op == OP_BRANCH);
			block = last_instr->val.branch.target_block;
		}

		puts("}");

		if (i != functions->size - 1)
			putchar('\n');
	}
}

void builder_init(Builder *builder)
{
	builder->function = NULL;
}

static inline IrInstr *append_instr(Block *block)
{
	IrInstr *instr = ARRAY_APPEND(&block->instrs, IrInstr);
	instr->id = block->instrs.size + block->arity - 1;
	return instr;
}

IrInstr *build_branch(Builder *builder, Block *block, Value value)
{
	IrInstr *instr = append_instr(builder->current_block);
	instr->op = OP_BRANCH;
	instr->val.branch.target_block = block;
	instr->val.branch.argument = value;

	return instr;
}

static u64 constant_fold_op(IrOp op, u64 arg1, u64 arg2)
{
	switch (op) {
	case OP_BIT_XOR:
		return arg1 ^ arg2;
		break;
	case OP_BRANCH:
		UNREACHABLE;
	}
}

static Value value_instr(IrInstr *instr)
{
	return (Value) {
		.kind = VALUE_INSTR,
		.type = instr->type,
		.val.instr = instr,
	};
}

Value build_binary_instr(Builder *builder, IrOp op, Value arg1, Value arg2)
{
	assert(ir_type_eq(arg1.type, arg2.type));

	IrType type = arg1.type;

	if (arg1.kind == VALUE_CONST && arg2.kind == VALUE_CONST) {
		return value_const(type, constant_fold_op(op, arg1.val.constant, arg2.val.constant));
	}

	IrInstr *instr = append_instr(builder->current_block);
	instr->op = op;
	instr->type = type;
	instr->val.binary_op.arg1 = arg1;
	instr->val.binary_op.arg2 = arg2;

	return value_instr(instr);
}

Value value_const(IrType type, u64 constant)
{
	Value value = {
		.kind = VALUE_CONST,
		.type = type,
		.val = { .constant = constant }
	};

	return value;
}

void generate_asm_module(TransUnit *trans_unit, AsmModule *asm_module)
{
	for (u32 i = 0; i < trans_unit->functions.size; i++) {
		IrFunction *func = ARRAY_REF(&trans_unit->functions, IrFunction, i);
		asm_gen_function(asm_module, func);
	}
}
