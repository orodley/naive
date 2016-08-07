#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "asm.h"
#include "asm_gen.h"
#include "ir.h"
#include "misc.h"

void trans_unit_init(TransUnit *trans_unit)
{
	ARRAY_INIT(&trans_unit->globals, IrGlobal, 10);
	pool_init(&trans_unit->pool, 512);
}

void trans_unit_free(TransUnit *trans_unit)
{
	array_free(&trans_unit->globals);
	pool_free(&trans_unit->pool);
}

// @TODO: Maybe we should let the caller initialize arg_types rather than
// making them allocate an array?
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

IrGlobal *trans_unit_add_function(TransUnit *trans_unit, char *name,
		IrType return_type, u32 arity, IrType *arg_types)
{
	IrGlobal *new_global = ARRAY_APPEND(&trans_unit->globals, IrGlobal);
	ZERO_STRUCT(new_global);

	new_global->name = name;
	new_global->ir_type.kind = IR_FUNCTION;
	new_global->kind = IR_GLOBAL_FUNCTION;
	new_global->id = trans_unit->globals.size - 1;
	block_init(&new_global->val.function.entry_block, "entry", arity, arg_types);
	block_init(&new_global->val.function.ret_block, "ret", 1, &return_type);

	return new_global;
}

extern inline IrType ir_function_return_type(IrFunction *f);

bool ir_type_eq(IrType a, IrType b)
{
	if (a.kind != b.kind)
		return false;

	switch (a.kind) {
	case IR_INT:
		return a.val.bit_width == b.val.bit_width;
	case IR_POINTER: case IR_FUNCTION:
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
	case IR_FUNCTION:
		fputs("fn", stdout);
		break;
	}
}

static void dump_value(TransUnit *trans_unit, Value value)
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
	case VALUE_GLOBAL:
		printf("$%s", ARRAY_REF(&trans_unit->globals, IrGlobal, value.val.global_id)->name);
		break;
	}
}

static void dump_instr(TransUnit *trans_unit, IrInstr *instr)
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
		dump_value(trans_unit, instr->val.load.pointer);
		break;
	case OP_STORE:
		fputs("store(", stdout);
		dump_value(trans_unit, instr->val.store.pointer);
		fputs(", ", stdout);
		dump_value(trans_unit, instr->val.store.value);
		fputs(", ", stdout);
		dump_ir_type(instr->val.store.type);
		break;
	case OP_BRANCH:
		printf("branch(%s, ", instr->val.branch.target_block->name);
		dump_value(trans_unit, instr->val.branch.argument);
		break;
	case OP_CALL:
		fputs("call(", stdout);
		dump_value(trans_unit, instr->val.call.callee);
		for (u32 i = 0; i < instr->val.call.arity; i++) {
			fputs(", ", stdout);
			dump_value(trans_unit, instr->val.call.arg_array[i]);
		}
		break;
	case OP_BIT_XOR:
		fputs("bit_xor(", stdout);
		dump_value(trans_unit, instr->val.binary_op.arg1);
		fputs(", ", stdout);
		dump_value(trans_unit, instr->val.binary_op.arg2);
		break;
	case OP_IMUL:
		fputs("imul(", stdout);
		dump_value(trans_unit, instr->val.binary_op.arg1);
		fputs(", ", stdout);
		dump_value(trans_unit, instr->val.binary_op.arg2);
		break;
	}

	puts(")");
}

void dump_trans_unit(TransUnit *trans_unit)
{
	Array(IrGlobal) *globals = &trans_unit->globals;
	for (u32 i = 0; i < globals->size; i++) {
		IrGlobal *global = ARRAY_REF(globals, IrGlobal, i);

		switch (global->kind) {
		case IR_GLOBAL_FUNCTION: {
			IrFunction *f = &global->val.function;

			dump_ir_type(ir_function_return_type(f));
			printf(" %s(", global->name);
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
					dump_instr(trans_unit, instr);
				}

				if (instrs->size == 0)
					break;

				IrInstr *last_instr = ARRAY_REF(instrs, IrInstr, instrs->size - 1);
				assert(last_instr->op == OP_BRANCH);
				block = last_instr->val.branch.target_block;
			}

			puts("}");
			break;
		}
		case IR_GLOBAL_SCALAR:
			UNIMPLEMENTED;
		}

		if (i != globals->size - 1)
			putchar('\n');
	}
}

void builder_init(Builder *builder, TransUnit *trans_unit)
{
	builder->current_function = NULL;
	builder->trans_unit = trans_unit;
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
	case OP_LOCAL: case OP_LOAD: case OP_STORE: case OP_BRANCH: case OP_CALL:
		UNREACHABLE;
	case OP_BIT_XOR:
		return arg1 ^ arg2;
		break;
	case OP_IMUL:
		return arg1 * arg2;
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

Value build_call(Builder *builder, Value callee, IrType return_type, u32 arity,
		Value *arg_array)
{
	IrInstr *instr = append_instr(builder->current_block);
	instr->op = OP_CALL;
	instr->type = return_type;
	instr->val.call.return_type = return_type;
	instr->val.call.callee = callee;
	instr->val.call.arity = arity;
	instr->val.call.arg_array = arg_array;

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

Value value_global(IrGlobal *global)
{
	Value value = {
		.kind = VALUE_GLOBAL,
		.type = global->ir_type,
		.val.global_id = global->id,
	};

	return value;
}
