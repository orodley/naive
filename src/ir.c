#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#include "asm.h"
#include "asm_gen.h"
#include "ir.h"
#include "misc.h"
#include "util.h"

static void block_init(IrBlock *block, char *name)
{
	block->name = name;
	ARRAY_INIT(&block->instrs, IrInstr *, 10);
}

static void block_free(IrBlock *block)
{
	array_free(&block->instrs);
}

void trans_unit_init(TransUnit *trans_unit)
{
	ARRAY_INIT(&trans_unit->globals, IrGlobal *, 10);
	pool_init(&trans_unit->pool, 512);
}

void trans_unit_free(TransUnit *trans_unit)
{
	for (u32 i = 0; i < trans_unit->globals.size; i++) {
		IrGlobal *global = *ARRAY_REF(&trans_unit->globals, IrGlobal *, i);
		if (global->kind == IR_GLOBAL_FUNCTION) {
			IrFunction *func = &global->val.function;
			for (u32 j = 0; j < func->blocks.size; j++) {
				IrBlock *block = *ARRAY_REF(&func->blocks, IrBlock *, j);
				block_free(block);
			}
			array_free(&func->blocks);
			free(func->arg_types);
		}
	}

	array_free(&trans_unit->globals);
	pool_free(&trans_unit->pool);
}

IrBlock *add_block_to_function(
		TransUnit *trans_unit, IrFunction *function, char *name)
{
	IrBlock *block = pool_alloc(&trans_unit->pool, sizeof *block);
	*ARRAY_APPEND(&function->blocks, IrBlock *) = block;
	block_init(block, name);

	return block;
}

IrGlobal *trans_unit_add_function(TransUnit *trans_unit, char *name,
		IrType return_type, u32 arity, IrType *arg_types)
{
	IrGlobal *new_global = pool_alloc(&trans_unit->pool, sizeof *new_global);
	*ARRAY_APPEND(&trans_unit->globals, IrGlobal *) = new_global;
	ZERO_STRUCT(new_global);

	new_global->name = name;
	new_global->ir_type.kind = IR_FUNCTION;
	new_global->kind = IR_GLOBAL_FUNCTION;

	IrFunction *new_function = &new_global->val.function;
	new_function->return_type = return_type;
	new_function->arity = arity;
	new_function->arg_types = arg_types;
	new_function->label = NULL;

	ARRAY_INIT(&new_function->blocks, IrBlock *, 5);
	add_block_to_function(trans_unit, new_function, "entry");

	return new_global;
}

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

static void dump_value(IrValue value)
{
	switch (value.kind) {
	case VALUE_CONST: 
		printf("%" PRId64, value.val.constant);
		break;
	case VALUE_ARG:
		printf("@%d", value.val.arg_index);
		break;
	case VALUE_INSTR:
		printf("#%d", value.val.instr->id);
		break;
	case VALUE_GLOBAL:
		printf("$%s", value.val.global->name);
		break;
	}
}

#define X(x) #x
static char *ir_op_names[] = {
	IR_OPS
};
#undef X

static void dump_instr(IrInstr *instr)
{
	char *op_name = ir_op_names[instr->op];
	for (u32 i = 3; op_name[i] != '\0'; i++)
		putchar(tolower(op_name[i]));
	putchar('(');

	switch (instr->op) {
	case OP_LOCAL:
		dump_ir_type(instr->val.type);
		break;
	case OP_LOAD:
		dump_ir_type(instr->val.load.type);
		fputs(", ", stdout);
		dump_value(instr->val.load.pointer);
		break;
	case OP_STORE:
		dump_value(instr->val.store.pointer);
		fputs(", ", stdout);
		dump_value(instr->val.store.value);
		fputs(", ", stdout);
		dump_ir_type(instr->val.store.type);
		break;
	case OP_BRANCH:
		fputs(instr->val.target_block->name, stdout);
		break;
	case OP_COND:
		dump_value(instr->val.cond.condition);
		fputs(", ", stdout);
		printf("%s, %s", instr->val.cond.then_block->name, instr->val.cond.else_block->name);
		break;
	case OP_RET:
		dump_value(instr->val.arg);
		break;
	case OP_CALL:
		dump_value(instr->val.call.callee);
		for (u32 i = 0; i < instr->val.call.arity; i++) {
			fputs(", ", stdout);
			dump_value(instr->val.call.arg_array[i]);
		}
		break;
	case OP_BIT_XOR: case OP_MUL: case OP_EQ: case OP_ADD:
		dump_value(instr->val.binary_op.arg1);
		fputs(", ", stdout);
		dump_value(instr->val.binary_op.arg2);
		break;
	}

	puts(")");
}

void dump_trans_unit(TransUnit *trans_unit)
{
	Array(IrGlobal *) *globals = &trans_unit->globals;
	for (u32 i = 0; i < globals->size; i++) {
		IrGlobal *global = *ARRAY_REF(globals, IrGlobal *, i);

		switch (global->kind) {
		case IR_GLOBAL_FUNCTION: {
			IrFunction *f = &global->val.function;

			dump_ir_type(f->return_type);
			printf(" %s(", global->name);
			for (u32 i = 0; i < f->arity; i++) {
				IrType arg_type = f->arg_types[i];
				dump_ir_type(arg_type);

				if (i != f->arity - 1)
					fputs(", ", stdout);
			}
			puts(")");

			if (!global->defined) {
				putchar('\n');
				continue;
			}

			puts("{");

			for (u32 i = 0; i < f->blocks.size; i++) {
				IrBlock *block = *ARRAY_REF(&f->blocks, IrBlock *, i);
				printf("%s:\n", block->name);

				Array(IrInstr *) *instrs = &block->instrs;
				for (u32 i = 0; i < instrs->size; i++) {
					IrInstr *instr = *ARRAY_REF(instrs, IrInstr *, i);
					putchar('\t');
					if (instr->op != OP_STORE && instr->op != OP_BRANCH
							&& instr->op != OP_COND && instr->op != OP_RET) {
						printf("#%u = ", i);
					}
					dump_instr(instr);
				}
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

void builder_init(IrBuilder *builder, TransUnit *trans_unit)
{
	builder->current_function = NULL;
	builder->trans_unit = trans_unit;
}

static inline IrInstr *append_instr(IrBuilder *builder)
{
	IrBlock *block = builder->current_block;

	IrInstr *instr = pool_alloc(&builder->trans_unit->pool, sizeof *instr);
	instr->id = block->instrs.size;
	instr->virtual_register = -1;
	*ARRAY_APPEND(&block->instrs, IrInstr *) = instr;

	return instr;
}

IrInstr *build_branch(IrBuilder *builder, IrBlock *block)
{
	IrInstr *instr = append_instr(builder);
	instr->op = OP_BRANCH;
	instr->val.target_block = block;

	return instr;
}

IrInstr *build_cond(IrBuilder *builder,
		IrValue condition, IrBlock *then_block, IrBlock *else_block)
{
	IrInstr *instr = append_instr(builder);
	instr->op = OP_COND;
	instr->val.cond.condition = condition;
	instr->val.cond.then_block = then_block;
	instr->val.cond.else_block = else_block;

	return instr;
}

static u64 constant_fold_op(IrOp op, u64 arg1, u64 arg2)
{
	switch (op) {
	case OP_LOCAL: case OP_LOAD: case OP_STORE:
	case OP_RET: case OP_BRANCH: case OP_COND: case OP_CALL:
		UNREACHABLE;
	case OP_BIT_XOR: return arg1 ^ arg2;
	case OP_MUL: return arg1 * arg2;
	case OP_EQ: return arg1 == arg2;
	case OP_ADD: return arg1 + arg2;
	}
}

static IrValue value_instr(IrInstr *instr)
{
	return (IrValue) {
		.kind = VALUE_INSTR,
		.type = instr->type,
		.val.instr = instr,
	};
}

IrValue build_local(IrBuilder *builder, IrType type)
{
	IrInstr *instr = append_instr(builder);
	instr->type = (IrType) { .kind = IR_POINTER };
	instr->op = OP_LOCAL;
	instr->val.type = type;

	return value_instr(instr);
}

IrValue build_load(IrBuilder *builder, IrValue pointer, IrType type)
{
	IrInstr *instr = append_instr(builder);
	instr->type = type;
	instr->op = OP_LOAD;
	instr->val.load.pointer = pointer;
	instr->val.load.type = type;

	return value_instr(instr);
}

IrValue build_store(IrBuilder *builder, IrValue pointer, IrValue value, IrType type)
{
	IrInstr *instr = append_instr(builder);
	instr->op = OP_STORE;
	instr->val.store.pointer = pointer;
	instr->val.store.type = type;
	instr->val.store.value = value;

	return value_instr(instr);
}

IrValue build_unary_instr(IrBuilder *builder, IrOp op, IrValue arg)
{
	// @TODO: Constant folding for unary ops once we have unary operations that
	// can be constant folded.

	IrInstr *instr = append_instr(builder);
	instr->op = op;
	instr->type = arg.type;
	instr->val.arg = arg;

	return value_instr(instr);
}

IrValue build_binary_instr(IrBuilder *builder, IrOp op, IrValue arg1, IrValue arg2)
{
	assert(ir_type_eq(arg1.type, arg2.type));

	IrType type = arg1.type;

	if (arg1.kind == VALUE_CONST && arg2.kind == VALUE_CONST) {
		return value_const(type, constant_fold_op(op, arg1.val.constant, arg2.val.constant));
	}

	IrInstr *instr = append_instr(builder);
	instr->op = op;
	instr->type = type;
	instr->val.binary_op.arg1 = arg1;
	instr->val.binary_op.arg2 = arg2;

	return value_instr(instr);
}

IrValue build_call(IrBuilder *builder, IrValue callee, IrType return_type, u32 arity,
		IrValue *arg_array)
{
	IrInstr *instr = append_instr(builder);
	instr->op = OP_CALL;
	instr->type = return_type;
	instr->val.call.return_type = return_type;
	instr->val.call.callee = callee;
	instr->val.call.arity = arity;
	instr->val.call.arg_array = arg_array;

	return value_instr(instr);
}

IrValue value_const(IrType type, u64 constant)
{
	IrValue value = {
		.kind = VALUE_CONST,
		.type = type,
		.val.constant = constant,
	};

	return value;
}

IrValue value_arg(u32 arg_index, IrType type)
{
	IrValue value = {
		.kind = VALUE_ARG,
		.type = type,
		.val.arg_index = arg_index,
	};

	return value;
}

IrValue value_global(IrGlobal *global)
{
	IrValue value = {
		.kind = VALUE_GLOBAL,
		.type = global->ir_type,
		.val.global = global,
	};

	return value;
}

AsmLabel *global_label(IrGlobal *global)
{
	switch (global->kind) {
	case IR_GLOBAL_FUNCTION:
		return global->val.function.label;
	case IR_GLOBAL_SCALAR:
		UNIMPLEMENTED;
	}
}
