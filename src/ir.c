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

static inline void block_init(IrBlock *block, char *name,
		u32 arity, IrType *arg_types)
{
	block->name = name;
	block->arity = arity;
	block->args = malloc(sizeof(*block->args) * arity);
	for (u32 i = 0; i < arity; i++) {
		Arg *arg = &block->args[i];
		arg->index = i;
		arg->type = arg_types[i];
		arg->virtual_register = -1;
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
	if (a.kind != b.kind)
		return false;

	switch (a.kind) {
	case IR_INT:
		return a.val.bit_width == b.val.bit_width;
	case IR_POINTER:
		return true;
	}
}

void dump_ir_type(IrType type)
{
	switch (type.kind) {
	case IR_INT:
		printf("i%d", type.val.bit_width);
		break;
	case IR_POINTER:
		putchar('*');
		break;
	}
}

static void dump_value(Value value)
{
	switch (value.kind) {
	case VALUE_CONST: 
		printf("%" PRId64, value.val.constant);
		break;
	case VALUE_ARG:
		printf("@%d", value.val.arg->index);
		break;
	case VALUE_INSTR:
		printf("#%d", value.val.instr->id);
		break;
	}
}

static void dump_instr(IrInstr *instr)
{
	switch (instr->op) {
	case OP_LOCAL:
		fputs("local(", stdout);
		dump_ir_type(instr->val.type);
		break;
	case OP_LOAD:
		fputs("load(", stdout);
		dump_ir_type(instr->val.load.type);
		fputs(", ", stdout);
		dump_value(instr->val.load.pointer);
		break;
	case OP_STORE:
		fputs("store(", stdout);
		dump_value(instr->val.store.pointer);
		fputs(", ", stdout);
		dump_value(instr->val.store.value);
		fputs(", ", stdout);
		dump_ir_type(instr->val.store.type);
		break;
	case OP_BRANCH:
		printf("branch(%s, ", instr->val.branch.target_block->name);
		dump_value(instr->val.branch.argument);
		break;
	case OP_BIT_XOR:
		fputs("bit_xor(", stdout);
		dump_value(instr->val.binary_op.arg1);
		fputs(", ", stdout);
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

		dump_ir_type(ir_function_return_type(f));
		printf(" %s(", f->name);
		Arg *args = f->entry_block.args;
		for (u32 i = 0; i < f->entry_block.arity; i++) {
			IrType arg_type = args[i].type;
			dump_ir_type(arg_type);

			if (i != f->entry_block.arity - 1)
				fputs(", ", stdout);
		}

		puts(")\n{");

		IrBlock *block = &f->entry_block;
		for (;;) {
			printf("%s(%d):\n", block->name, block->arity);

			Array(IrInstr) *instrs = &block->instrs;
			for (u32 i = 0; i < instrs->size; i++) {
				IrInstr *instr = ARRAY_REF(instrs, IrInstr, i);
				putchar('\t');
				if (instr->op != OP_STORE && instr->op != OP_BRANCH)
					printf("#%u = ", i);
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

static inline IrInstr *append_instr(IrBlock *block)
{
	IrInstr *instr = ARRAY_APPEND(&block->instrs, IrInstr);
	instr->id = block->instrs.size - 1;
	instr->virtual_register = -1;
	return instr;
}

// @TODO: Currently this is limited to blocks of arity 1.
IrInstr *build_branch(Builder *builder, IrBlock *block, Value value)
{
	assert(ir_type_eq(block->args[0].type, value.type));

	IrInstr *instr = append_instr(builder->current_block);
	instr->op = OP_BRANCH;
	instr->val.branch.target_block = block;
	instr->val.branch.argument = value;

	return instr;
}

static u64 constant_fold_op(IrOp op, u64 arg1, u64 arg2)
{
	switch (op) {
	case OP_LOCAL:
	case OP_LOAD:
	case OP_STORE:
	case OP_BRANCH:
		UNREACHABLE;
	case OP_BIT_XOR:
		return arg1 ^ arg2;
		break;
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

Value build_local(Builder *builder, IrType type)
{
	IrInstr *instr = append_instr(builder->current_block);
	instr->type = (IrType) { .kind = IR_POINTER };
	instr->op = OP_LOCAL;
	instr->val.type = type;

	return value_instr(instr);
}

Value build_load(Builder *builder, Value pointer, IrType type)
{
	IrInstr *instr = append_instr(builder->current_block);
	instr->type = type;
	instr->op = OP_LOAD;
	instr->val.load.pointer = pointer;
	instr->val.load.type = type;

	return value_instr(instr);
}

Value build_store(Builder *builder, Value pointer, Value value, IrType type)
{
	IrInstr *instr = append_instr(builder->current_block);
	instr->op = OP_STORE;
	instr->val.store.pointer = pointer;
	instr->val.store.type = type;
	instr->val.store.value = value;

	return value_instr(instr);
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
		.val.constant = constant,
	};

	return value;
}

Value value_arg(Arg *arg)
{
	Value value = {
		.kind = VALUE_ARG,
		.type = arg->type,
		.val.arg = arg,
	};

	return value;
}
