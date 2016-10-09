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
	ARRAY_INIT(&trans_unit->types, IrGlobal *, 5);
	pool_init(&trans_unit->pool, 512);
}

void trans_unit_free(TransUnit *trans_unit)
{
	for (u32 i = 0; i < trans_unit->globals.size; i++) {
		IrGlobal *global = *ARRAY_REF(&trans_unit->globals, IrGlobal *, i);
		if (global->type.kind == IR_FUNCTION) {
			if (global->initializer != NULL) {
				IrFunction *func = &global->initializer->val.function;
				for (u32 j = 0; j < func->blocks.size; j++) {
					IrBlock *block = *ARRAY_REF(&func->blocks, IrBlock *, j);
					block_free(block);
				}
				array_free(&func->blocks);
				free(func->arg_types);
			}
		}
	}

	array_free(&trans_unit->globals);
	array_free(&trans_unit->types);
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

// @TODO: Split this into two: first add the function with no initializer, then
// add an initializer for the function.
IrGlobal *trans_unit_add_function(TransUnit *trans_unit, char *name,
		IrType return_type, u32 arity, IrType *arg_types)
{
	IrGlobal *new_global = pool_alloc(&trans_unit->pool, sizeof *new_global);
	*ARRAY_APPEND(&trans_unit->globals, IrGlobal *) = new_global;
	ZERO_STRUCT(new_global);

	new_global->name = name;

	IrType *return_type_ptr = pool_alloc(&trans_unit->pool, sizeof *return_type_ptr);
	*return_type_ptr = return_type;
	IrType *arg_types_ptr = pool_alloc(&trans_unit->pool, arity * sizeof *arg_types_ptr);
	memcpy(arg_types_ptr, arg_types, arity * sizeof *arg_types_ptr);

	IrType function_type = {
		.kind = IR_FUNCTION,
		.val.function.arity = arity,
		.val.function.return_type = return_type_ptr,
		.val.function.arg_types = arg_types_ptr,
	};
	new_global->type = function_type;

	return new_global;
}

IrInit *add_init_to_function(TransUnit *trans_unit, IrGlobal *global)
{
	IrInit *initializer = pool_alloc(&trans_unit->pool, sizeof *initializer);
	initializer->type = global->type;
	IrFunction *function = &initializer->val.function;
	function->return_type = *global->type.val.function.return_type;
	function->arity = global->type.val.function.arity;
	function->arg_types = global->type.val.function.arg_types;
	function->label = NULL;

	global->initializer = initializer;

	ARRAY_INIT(&function->blocks, IrBlock *, 5);
	add_block_to_function(trans_unit, function, "entry");

	return initializer;
}

IrGlobal *trans_unit_add_var(TransUnit *trans_unit, char *name, IrType type)
{
	IrGlobal *new_global = pool_alloc(&trans_unit->pool, sizeof *new_global);
	*ARRAY_APPEND(&trans_unit->globals, IrGlobal *) = new_global;
	ZERO_STRUCT(new_global);

	new_global->name = name;
	new_global->type = type;
	new_global->initializer = NULL;

	return new_global;
}

IrType *trans_unit_add_struct(TransUnit *trans_unit, char *name, u32 num_fields)
{
	IrType *new_type = pool_alloc(&trans_unit->pool, sizeof *new_type);
	*ARRAY_APPEND(&trans_unit->types, IrType *) = new_type;

	new_type->kind = IR_STRUCT;
	new_type->val.strukt.name = name;
	new_type->val.strukt.num_fields = num_fields;
	IrStructField *fields = pool_alloc(&trans_unit->pool, num_fields * sizeof *fields);
	new_type->val.strukt.fields = fields;
	new_type->val.strukt.total_size = 0;

	return new_type;
}

bool ir_type_eq(IrType *a, IrType *b)
{
	if (a->kind != b->kind)
		return false;

	switch (a->kind) {
	case IR_INT:
		return a->val.bit_width == b->val.bit_width;
	case IR_POINTER: case IR_FUNCTION:
		return true;
	case IR_STRUCT:
		return streq(a->val.strukt.name, b->val.strukt.name);
	case IR_ARRAY:
		return ir_type_eq(a->val.array.elem_type, b->val.array.elem_type)
			&& a->val.array.size == b->val.array.size;
	}
}

u32 size_of_ir_type(IrType type)
{
	switch (type.kind) {
	case IR_INT:
		return type.val.bit_width / 8;
	case IR_POINTER: case IR_FUNCTION:
		return 8;
	case IR_STRUCT:
		return type.val.strukt.total_size;
	case IR_ARRAY:
		return type.val.array.size * size_of_ir_type(*type.val.array.elem_type);
	}
}

u32 align_of_ir_type(IrType type)
{
	switch (type.kind) {
	case IR_STRUCT:
		return type.val.strukt.alignment;
	case IR_ARRAY:
		return align_of_ir_type(*type.val.array.elem_type);
	default:
		return size_of_ir_type(type);
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
		putchar('(');
		u32 arity = type.val.function.arity;
		for (u32 i = 0; i < arity; i++) {
			IrType arg_type = type.val.function.arg_types[i];
			dump_ir_type(arg_type);

			if (i != arity - 1)
				fputs(", ", stdout);
		}
		fputs(") -> ", stdout);
		dump_ir_type(*type.val.function.return_type);

		break;
	case IR_STRUCT:
		printf("$%s", type.val.strukt.name);
		break;
	case IR_ARRAY:
		printf("[%lu x ", type.val.array.size);
		dump_ir_type(*type.val.array.elem_type);
		putchar(']');
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
	case OP_FIELD:
		dump_value(instr->val.field.struct_ptr);
		fputs(", ", stdout);
		dump_ir_type(instr->val.field.struct_type);
		printf(", %d", instr->val.field.field_number);
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
	case OP_CAST: case OP_ZEXT: case OP_SEXT:
		dump_value(instr->val.arg);
		fputs(", ", stdout);
		dump_ir_type(instr->type);
		break;
	case OP_BRANCH:
		fputs(instr->val.target_block->name, stdout);
		break;
	case OP_COND:
		dump_value(instr->val.cond.condition);
		fputs(", ", stdout);
		printf("%s, %s", instr->val.cond.then_block->name, instr->val.cond.else_block->name);
		break;
	case OP_RET: case OP_BIT_NOT: case OP_LOG_NOT:
		dump_value(instr->val.arg);
		break;
	case OP_CALL:
		dump_value(instr->val.call.callee);
		for (u32 i = 0; i < instr->val.call.arity; i++) {
			fputs(", ", stdout);
			dump_value(instr->val.call.arg_array[i]);
		}
		break;
	case OP_BIT_XOR: case OP_BIT_AND: case OP_BIT_OR: case OP_MUL: case OP_DIV:
	case OP_EQ: case OP_ADD: case OP_SUB: case OP_NEQ: case OP_GT: case OP_GTE:
	case OP_LT: case OP_LTE:
		dump_value(instr->val.binary_op.arg1);
		fputs(", ", stdout);
		dump_value(instr->val.binary_op.arg2);
		break;
	}

	puts(")");
}

static void dump_init(IrInit *init)
{
	switch (init->type.kind) {
	case IR_INT:
		printf("%lu", init->val.integer);
		break;
	case IR_POINTER:
		printf("$%s", init->val.global_pointer->name);
		break;
	case IR_ARRAY: {
		putchar('[');
		u32 len = init->type.val.array.size;
		for (u32 i = 0; i < len; i++) {
			dump_init(init->val.array_elems + i);

			if (i != len - 1)
				fputs(", ", stdout);
		}
		putchar(']');
		break;
	}
	case IR_STRUCT: {
		putchar('{');
		u32 len = init->type.val.strukt.num_fields;
		for (u32 i = 0; i < len; i++) {
			dump_init(init->val.struct_fields + i);

			if (i != len - 1)
				fputs(", ", stdout);
		}
		putchar('}');
		break;
	}
	case IR_FUNCTION: {
		IrFunction *f = &init->val.function;

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

		putchar('}');
		break;
	}
	}
}

void dump_trans_unit(TransUnit *trans_unit)
{
	for (u32 i = 0; i < trans_unit->types.size; i++) {
		IrType *type = *ARRAY_REF(&trans_unit->types, IrType *, i);
		assert(type->kind == IR_STRUCT);

		printf("struct $%s\n{\n", type->val.strukt.name);
		for (u32 i = 0; i < type->val.strukt.num_fields; i++) {
			putchar('\t');
			dump_ir_type(type->val.strukt.fields[i].type);
			putchar('\n');
		}
		puts("}");
	}
	putchar('\n');

	for (u32 i = 0; i < trans_unit->globals.size; i++) {
		IrGlobal *global = *ARRAY_REF(&trans_unit->globals, IrGlobal *, i);
		printf("%s ", global->name);
		dump_ir_type(global->type);

		if (global->initializer != NULL) {
			fputs(" = ", stdout);
			dump_init(global->initializer);
		}

		putchar('\n');
		if (i != trans_unit->globals.size - 1)
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
	instr->vreg_number = -1;
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
	case OP_LOCAL: case OP_FIELD: case OP_LOAD: case OP_STORE: case OP_CAST:
	case OP_RET: case OP_BRANCH: case OP_COND: case OP_CALL: case OP_ZEXT:
	case OP_SEXT: case OP_BIT_NOT: case OP_LOG_NOT:
		UNREACHABLE;
	case OP_BIT_XOR: return arg1 ^ arg2;
	case OP_BIT_AND: return arg1 & arg2;
	case OP_BIT_OR: return arg1 | arg2;
	case OP_MUL: return arg1 * arg2;
	case OP_DIV: return arg1 / arg2;
	case OP_EQ: return arg1 == arg2;
	case OP_NEQ: return arg1 != arg2;
	case OP_GT: return arg1 > arg2;
	case OP_GTE: return arg1 >= arg2;
	case OP_LT: return arg1 < arg2;
	case OP_LTE: return arg1 <= arg2;
	case OP_ADD: return arg1 + arg2;
	case OP_SUB: return arg1 - arg2;
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

IrValue build_field(IrBuilder *builder, IrValue struct_ptr, IrType struct_type,
		u32 field_number)
{
	IrInstr *instr = append_instr(builder);
	instr->type = (IrType) { .kind = IR_POINTER };
	instr->op = OP_FIELD;
	instr->val.field.struct_ptr = struct_ptr;
	instr->val.field.struct_type = struct_type;
	instr->val.field.field_number = field_number;

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
	assert(ir_type_eq(&arg1.type, &arg2.type));

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

IrValue build_type_instr(IrBuilder *builder, IrOp op, IrValue value, IrType result_type)
{
	if (value.kind == VALUE_CONST) {
		return value_const(result_type, value.val.constant);
	}

	IrInstr *instr = append_instr(builder);
	instr->op = op;
	instr->type = result_type;
	instr->val.arg = value;

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
		.type = (IrType) { .kind = IR_POINTER },
		.val.global = global,
	};

	return value;
}

AsmLabel *global_label(IrGlobal *global)
{
	if (global->type.kind == IR_FUNCTION) {
		return global->initializer->val.function.label;
	} else {
		UNIMPLEMENTED;
	}
}

IrInit *add_int_init(IrBuilder *builder, IrType int_type, u64 value)
{
	IrInit *init = pool_alloc(&builder->trans_unit->pool, sizeof *init);
	init->type = int_type;
	init->val.integer = value;

	return init;
}

IrInit *add_array_init(IrBuilder *builder, IrType type)
{
	IrInit *init = pool_alloc(&builder->trans_unit->pool, sizeof *init);
	init->type = type;
	init->val.array_elems = pool_alloc(&builder->trans_unit->pool,
			type.val.array.size * sizeof *init->val.array_elems);

	return init;
}
