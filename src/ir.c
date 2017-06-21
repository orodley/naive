#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#include "asm.h"
#include "asm_gen.h"
#include "ir.h"
#include "misc.h"
#include "util.h"

static void block_init(IrBlock *block, char *name, u32 id)
{
	block->name = name;
	block->id = id;
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
		if (global->type.t == IR_FUNCTION) {
			if (global->initializer != NULL) {
				IrFunction *func = &global->initializer->u.function;
				for (u32 j = 0; j < func->blocks.size; j++) {
					IrBlock *block = *ARRAY_REF(&func->blocks, IrBlock *, j);
					block_free(block);
				}
				array_free(&func->blocks);
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
	block_init(block, name, function->blocks.size - 1);

	return block;
}

IrGlobal *trans_unit_add_function(TransUnit *trans_unit, char *name,
		IrType return_type, u32 arity, bool variable_arity, IrType *arg_types)
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
		.t = IR_FUNCTION,
		.u.function.arity = arity,
		.u.function.variable_arity = variable_arity,
		.u.function.return_type = return_type_ptr,
		.u.function.arg_types = arg_types_ptr,
	};
	new_global->type = function_type;

	return new_global;
}

IrConst *add_init_to_function(TransUnit *trans_unit, IrGlobal *global)
{
	IrConst *initializer = pool_alloc(&trans_unit->pool, sizeof *initializer);
	initializer->type = global->type;
	IrFunction *function = &initializer->u.function;

	global->initializer = initializer;

	ARRAY_INIT(&function->blocks, IrBlock *, 5);
	function->curr_instr_id = 0;
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

	if (name == NULL) {
		char fmt[] = "__anon_struct_%x";

		// - 2 adjusts down for the "%x" which isn't present in the output
		// sizeof(u32) * 2 is the max length of globals.size in hex
		// + 1 for the null terminator
		u32 name_max_length = sizeof fmt - 2 + sizeof(u32) * 2 + 1;
		name = pool_alloc(&trans_unit->pool, name_max_length);
		snprintf(name, name_max_length, fmt, trans_unit->types.size);
	}

	new_type->t = IR_STRUCT;
	new_type->u.strukt.name = name;
	new_type->u.strukt.num_fields = num_fields;
	IrStructField *fields = pool_alloc(&trans_unit->pool, num_fields * sizeof *fields);
	new_type->u.strukt.fields = fields;
	new_type->u.strukt.total_size = 0;

	return new_type;
}

bool ir_type_eq(IrType *a, IrType *b)
{
	if (a->t != b->t)
		return false;

	switch (a->t) {
	case IR_INT:
		return a->u.bit_width == b->u.bit_width;
	case IR_VOID: case IR_POINTER: case IR_FUNCTION:
		return true;
	case IR_STRUCT:
		return streq(a->u.strukt.name, b->u.strukt.name);
	case IR_ARRAY:
		return ir_type_eq(a->u.array.elem_type, b->u.array.elem_type)
			&& a->u.array.size == b->u.array.size;
	}
}

u32 size_of_ir_type(IrType type)
{
	switch (type.t) {
	case IR_INT:
		return type.u.bit_width / 8;
	case IR_POINTER: case IR_FUNCTION:
		return 8;
	case IR_STRUCT:
		return type.u.strukt.total_size;
	case IR_ARRAY:
		assert(type.u.array.size != 0);
		return type.u.array.size * size_of_ir_type(*type.u.array.elem_type);
	case IR_VOID:
		UNREACHABLE;
	}
}

u32 align_of_ir_type(IrType type)
{
	switch (type.t) {
	case IR_STRUCT:
		return type.u.strukt.alignment;
	case IR_ARRAY:
		return align_of_ir_type(*type.u.array.elem_type);
	default:
		return size_of_ir_type(type);
	}
}

void dump_ir_type(IrType type)
{
	switch (type.t) {
	case IR_VOID:
		fputs("void", stdout);
		break;
	case IR_INT:
		printf("i%d", type.u.bit_width);
		break;
	case IR_POINTER:
		putchar('*');
		break;
	case IR_FUNCTION:
		putchar('(');
		u32 arity = type.u.function.arity;
		for (u32 i = 0; i < arity; i++) {
			IrType arg_type = type.u.function.arg_types[i];
			dump_ir_type(arg_type);

			if (i != arity - 1)
				fputs(", ", stdout);
		}
		if (type.u.function.variable_arity)
			fputs(", ...", stdout);
		fputs(") -> ", stdout);
		dump_ir_type(*type.u.function.return_type);

		break;
	case IR_STRUCT:
		printf("$%s", type.u.strukt.name);
		break;
	case IR_ARRAY:
		printf("[%lu x ", type.u.array.size);
		dump_ir_type(*type.u.array.elem_type);
		putchar(']');
		break;
	}
}

static void dump_value(IrValue value)
{
	switch (value.t) {
	case VALUE_CONST: 
		printf("%" PRId64, value.u.constant);
		break;
	case VALUE_ARG:
		printf("@%d", value.u.arg_index);
		break;
	case VALUE_INSTR:
		printf("#%d", value.u.instr->id);
		break;
	case VALUE_GLOBAL:
		printf("$%s", value.u.global->name);
		break;
	}
}

static void dump_block_name(IrBlock *block)
{
	printf("%u.%s", block->id, block->name);
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
	case OP_INVALID: UNREACHABLE;
	case OP_LOCAL:
		dump_ir_type(instr->u.local.type);
		break;
	case OP_FIELD:
		dump_value(instr->u.field.struct_ptr);
		fputs(", ", stdout);
		dump_ir_type(instr->u.field.struct_type);
		printf(", %d", instr->u.field.field_number);
		break;
	case OP_LOAD:
		dump_ir_type(instr->u.load.type);
		fputs(", ", stdout);
		dump_value(instr->u.load.pointer);
		break;
	case OP_CAST: case OP_ZEXT: case OP_SEXT: case OP_TRUNC:
		dump_value(instr->u.arg);
		fputs(", ", stdout);
		dump_ir_type(instr->type);
		break;
	case OP_BRANCH:
		dump_block_name(instr->u.target_block);
		break;
	case OP_COND:
		dump_value(instr->u.cond.condition);
		fputs(", ", stdout);
		dump_block_name(instr->u.cond.then_block);
		fputs(", ", stdout);
		dump_block_name(instr->u.cond.else_block);
		break;
	case OP_PHI:
		for (u32 i = 0; i < instr->u.phi.arity; i++) {
			IrPhiParam *param = instr->u.phi.params + i;
			putchar('(');
			dump_block_name(param->block);
			fputs(", ", stdout);
			dump_value(param->value);
			putchar(')');
			if (i != instr->u.phi.arity - 1)
				fputs(", ", stdout);
		}
	case OP_RET_VOID:
		break;
	case OP_RET: case OP_BIT_NOT: case OP_BUILTIN_VA_START:
	case OP_NEG:
		dump_value(instr->u.arg);
		break;
	case OP_CALL:
		dump_value(instr->u.call.callee);
		for (u32 i = 0; i < instr->u.call.arity; i++) {
			fputs(", ", stdout);
			dump_value(instr->u.call.arg_array[i]);
		}
		break;
	case OP_BIT_XOR: case OP_BIT_AND: case OP_BIT_OR: case OP_SHL: case OP_SHR:
	case OP_MUL: case OP_DIV: case OP_MOD: case OP_EQ: case OP_ADD:
	case OP_SUB: case OP_NEQ: case OP_GT: case OP_GTE: case OP_LT: case OP_LTE:
	case OP_STORE: case OP_BUILTIN_VA_ARG:
		dump_value(instr->u.binary_op.arg1);
		fputs(", ", stdout);
		dump_value(instr->u.binary_op.arg2);
		break;
	}

	puts(")");
}

static void dump_const(IrConst *konst)
{
	switch (konst->type.t) {
	case IR_INT:
		printf("%lu", konst->u.integer);
		break;
	case IR_POINTER:
		printf("$%s", konst->u.global_pointer->name);
		break;
	case IR_ARRAY: {
		putchar('[');
		u32 len = konst->type.u.array.size;
		for (u32 i = 0; i < len; i++) {
			dump_const(konst->u.array_elems + i);

			if (i != len - 1)
				fputs(", ", stdout);
		}
		putchar(']');
		break;
	}
	case IR_STRUCT: {
		putchar('{');
		u32 len = konst->type.u.strukt.num_fields;
		for (u32 i = 0; i < len; i++) {
			dump_const(konst->u.struct_fields + i);

			if (i != len - 1)
				fputs(", ", stdout);
		}
		putchar('}');
		break;
	}
	case IR_FUNCTION: {
		IrFunction *f = &konst->u.function;

		puts("{");

		for (u32 i = 0; i < f->blocks.size; i++) {
			IrBlock *block = *ARRAY_REF(&f->blocks, IrBlock *, i);
			dump_block_name(block);
			fputs(":\n", stdout);

			Array(IrInstr *) *instrs = &block->instrs;
			for (u32 i = 0; i < instrs->size; i++) {
				IrInstr *instr = *ARRAY_REF(instrs, IrInstr *, i);
				putchar('\t');
				if (instr->type.t != IR_VOID) {
					printf("#%u = ", instr->id);
				}
				dump_instr(instr);
			}
		}

		putchar('}');
		break;
	}
	case IR_VOID:
		UNREACHABLE;
	}
}

void dump_trans_unit(TransUnit *trans_unit)
{
	for (u32 i = 0; i < trans_unit->types.size; i++) {
		IrType *type = *ARRAY_REF(&trans_unit->types, IrType *, i);
		assert(type->t == IR_STRUCT);

		printf("struct $%s\n{\n", type->u.strukt.name);
		for (u32 i = 0; i < type->u.strukt.num_fields; i++) {
			printf("\t[%u] ", type->u.strukt.fields[i].offset);
			dump_ir_type(type->u.strukt.fields[i].type);
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
			dump_const(global->initializer);
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
	instr->id = builder->current_function->curr_instr_id++;
	instr->vreg_number = -1;
	*ARRAY_APPEND(&block->instrs, IrInstr *) = instr;

	return instr;
}

IrInstr *build_branch(IrBuilder *builder, IrBlock *block)
{
	IrInstr *instr = append_instr(builder);
	instr->op = OP_BRANCH;
	instr->type = (IrType) { .t = IR_VOID };
	instr->u.target_block = block;

	return instr;
}

IrInstr *build_cond(IrBuilder *builder,
		IrValue condition, IrBlock *then_block, IrBlock *else_block)
{
	IrInstr *instr = append_instr(builder);
	instr->op = OP_COND;
	instr->type = (IrType) { .t = IR_VOID };
	instr->u.cond.condition = condition;
	instr->u.cond.then_block = then_block;
	instr->u.cond.else_block = else_block;

	return instr;
}

static bool constant_foldable(IrOp op)
{
	switch (op) {
	case OP_LOCAL: case OP_FIELD: case OP_LOAD: case OP_STORE: case OP_CAST:
	case OP_RET: case OP_BRANCH: case OP_COND: case OP_CALL: case OP_ZEXT:
	case OP_SEXT: case OP_RET_VOID:
		return false;
	default:
		return true;
	}
}

static u64 constant_fold_unary_op(IrOp op, u64 arg)
{
	switch (op) {
	case OP_BIT_XOR: case OP_BIT_AND: case OP_BIT_OR: case OP_MUL: case OP_DIV:
	case OP_EQ: case OP_NEQ: case OP_GT: case OP_GTE: case OP_LT: case OP_LTE:
	case OP_ADD: case OP_SUB:
		UNREACHABLE;
	case OP_BIT_NOT:
		return ~arg;
	case OP_NEG:
		return -arg;
	default:
		assert(constant_foldable(op));
		UNIMPLEMENTED;
	}
}

static u64 constant_fold_binary_op(IrOp op, u64 arg1, u64 arg2)
{
	switch (op) {
	case OP_BIT_NOT:
		UNREACHABLE;
	case OP_BIT_XOR: return arg1 ^ arg2;
	case OP_BIT_AND: return arg1 & arg2;
	case OP_BIT_OR: return arg1 | arg2;
	case OP_SHL: return arg1 << arg2;
	case OP_SHR: return arg1 >> arg2;
	case OP_MUL: return arg1 * arg2;
	case OP_DIV: return arg1 / arg2;
	case OP_MOD: return arg1 % arg2;
	case OP_EQ: return arg1 == arg2;
	case OP_NEQ: return arg1 != arg2;
	case OP_GT: return arg1 > arg2;
	case OP_GTE: return arg1 >= arg2;
	case OP_LT: return arg1 < arg2;
	case OP_LTE: return arg1 <= arg2;
	case OP_ADD: return arg1 + arg2;
	case OP_SUB: return arg1 - arg2;
	default:
		assert(constant_foldable(op));
		UNIMPLEMENTED;
	}
}

static IrValue value_instr(IrInstr *instr)
{
	return (IrValue) {
		.t = VALUE_INSTR,
		.type = instr->type,
		.u.instr = instr,
	};
}

IrValue build_local(IrBuilder *builder, IrType type)
{
	assert(size_of_ir_type(type) != 0);

	IrInstr *instr = append_instr(builder);
	instr->op = OP_LOCAL;
	instr->type = (IrType) { .t = IR_POINTER };
	instr->u.local.type = type;
	instr->u.local.stack_offset = 0;

	return value_instr(instr);
}

IrValue build_field(IrBuilder *builder, IrValue struct_ptr, IrType struct_type,
		u32 field_number)
{
	IrInstr *instr = append_instr(builder);
	instr->op = OP_FIELD;
	instr->type = (IrType) { .t = IR_POINTER };
	instr->u.field.struct_ptr = struct_ptr;
	instr->u.field.struct_type = struct_type;
	instr->u.field.field_number = field_number;

	return value_instr(instr);
}

IrValue build_load(IrBuilder *builder, IrValue pointer, IrType type)
{
	IrInstr *instr = append_instr(builder);
	instr->op = OP_LOAD;
	instr->type = type;
	instr->u.load.pointer = pointer;
	instr->u.load.type = type;

	return value_instr(instr);
}

IrValue build_store(IrBuilder *builder, IrValue pointer, IrValue value)
{
	IrInstr *instr = append_instr(builder);
	instr->op = OP_STORE;
	instr->type = (IrType) { .t = IR_VOID };
	instr->u.binary_op.arg1 = pointer;
	instr->u.binary_op.arg2 = value;

	return value_instr(instr);
}

IrValue build_nullary_instr(IrBuilder *builder, IrOp op, IrType type)
{
	IrInstr *instr = append_instr(builder);
	instr->op = op;
	instr->type = type;

	return value_instr(instr);
}

IrValue build_unary_instr(IrBuilder *builder, IrOp op, IrValue arg)
{
	IrType type = arg.type;

	if (arg.t == VALUE_CONST && constant_foldable(op)) {
		return value_const(type, constant_fold_unary_op(op, arg.u.constant));
	}

	IrInstr *instr = append_instr(builder);
	instr->op = op;
	if (op == OP_RET) {
		instr->type = (IrType) { .t = IR_VOID };
	} else {
		instr->type = arg.type;
	}
	instr->u.arg = arg;

	return value_instr(instr);
}

IrValue build_binary_instr(IrBuilder *builder, IrOp op, IrValue arg1, IrValue arg2)
{
	assert(ir_type_eq(&arg1.type, &arg2.type));

	IrType type = arg1.type;
	switch (op) {
	case OP_EQ: case OP_NEQ: case OP_GT: case OP_GTE: case OP_LT: case OP_LTE:
		// @TODO: This shouldn't be hardcoded here. We should define some fixed
		// type for IR comparisons and the caller should explicitly cast to
		// whatever type they want (int in C).
		type = (IrType) { .t = IR_INT, .u.bit_width = 32 };
		break;
	default:
		break;
	}

	if (arg1.t == VALUE_CONST && arg2.t == VALUE_CONST && constant_foldable(op)) {
		return value_const(type,
				constant_fold_binary_op(op, arg1.u.constant, arg2.u.constant));
	}

	IrInstr *instr = append_instr(builder);
	instr->op = op;
	instr->type = type;
	instr->u.binary_op.arg1 = arg1;
	instr->u.binary_op.arg2 = arg2;

	return value_instr(instr);
}

IrValue build_call(IrBuilder *builder, IrValue callee, IrType return_type, u32 arity,
		IrValue *arg_array)
{
	IrInstr *instr = append_instr(builder);
	instr->op = OP_CALL;
	instr->type = return_type;
	instr->u.call.return_type = return_type;
	instr->u.call.callee = callee;
	instr->u.call.arity = arity;
	instr->u.call.arg_array = arg_array;

	return value_instr(instr);
}

IrValue build_type_instr(IrBuilder *builder, IrOp op, IrValue value, IrType result_type)
{
	if (ir_type_eq(&value.type, &result_type))
		return value;

	if (value.t == VALUE_CONST) {
		return value_const(result_type, value.u.constant);
	}

	IrInstr *instr = append_instr(builder);
	instr->op = op;
	instr->type = result_type;
	instr->u.arg = value;

	return value_instr(instr);
}

IrValue build_phi(IrBuilder *builder, IrType type, u32 arity)
{
	IrInstr *instr = append_instr(builder);
	instr->op = OP_PHI;
	instr->type = type;
	instr->u.phi.arity = arity;
	instr->u.phi.params = pool_alloc(&builder->trans_unit->pool,
			arity * sizeof *instr->u.phi.params);

	return value_instr(instr);
}

void phi_set_param(IrValue phi, u32 index, IrBlock *source_block, IrValue value)
{
	assert(ir_type_eq(&value.type, &phi.type));
	assert(phi.t == VALUE_INSTR);
	IrPhiParam *param = phi.u.instr->u.phi.params + index;
	param->block = source_block;
	param->value = value;
}

IrValue value_const(IrType type, u64 constant)
{
	IrValue value = {
		.t = VALUE_CONST,
		.type = type,
		.u.constant = constant,
	};

	return value;
}

IrValue value_arg(u32 arg_index, IrType type)
{
	IrValue value = {
		.t = VALUE_ARG,
		.type = type,
		.u.arg_index = arg_index,
	};

	return value;
}

IrValue value_global(IrGlobal *global)
{
	IrValue value = {
		.t = VALUE_GLOBAL,
		.type = (IrType) { .t = IR_POINTER },
		.u.global = global,
	};

	return value;
}

IrConst *add_int_const(IrBuilder *builder, IrType int_type, u64 value)
{
	IrConst *konst = pool_alloc(&builder->trans_unit->pool, sizeof *konst);
	konst->type = int_type;
	konst->u.integer = value;

	return konst;
}

IrConst *add_global_const(IrBuilder *builder, IrGlobal *global)
{
	IrConst *konst = pool_alloc(&builder->trans_unit->pool, sizeof *konst);
	konst->type = (IrType) { .t = IR_POINTER };
	konst->u.global_pointer = global;

	return konst;
}

IrConst *add_array_const(IrBuilder *builder, IrType type)
{
	IrConst *konst = pool_alloc(&builder->trans_unit->pool, sizeof *konst);
	konst->type = type;
	konst->u.array_elems = pool_alloc(&builder->trans_unit->pool,
			type.u.array.size * sizeof *konst->u.array_elems);

	return konst;
}

IrConst *add_struct_const(IrBuilder *builder, IrType type)
{
	IrConst *konst = pool_alloc(&builder->trans_unit->pool, sizeof *konst);
	konst->type = type;
	konst->u.struct_fields = pool_alloc(&builder->trans_unit->pool,
			type.u.strukt.num_fields * sizeof *konst->u.struct_fields);

	return konst;
}

// @TODO: Make this not linear in the number of functions in the TU.
static IrValue builtin_function(IrBuilder *builder, char *name, u32 arity,
		IrType return_type, IrType *arg_types)
{
	for (u32 i = 0; i < builder->trans_unit->globals.size; i++) {
		IrGlobal *global =
			*ARRAY_REF(&builder->trans_unit->globals, IrGlobal *, i);
		if (streq(global->name, name))
			return value_global(global);
	}

	return value_global(trans_unit_add_function(
				builder->trans_unit, name, return_type, arity, false, arg_types));
}

IrValue builtin_memcpy(IrBuilder *builder)
{
	IrType pointer = (IrType) { .t = IR_POINTER };
	IrType arg_types[] = {
		pointer,
		pointer,
		// @TODO: Don't hardcode the size of a pointer!
		(IrType) { .t = IR_INT, .u.bit_width = 64 }
	};
	return builtin_function(builder, "memcpy", 3, pointer, arg_types);
}

IrValue builtin_memset(IrBuilder *builder)
{
	IrType arg_types[] = {
		(IrType) { .t = IR_POINTER },
		// @TODO: Don't hardcode the size of an int!
		(IrType) { .t = IR_INT, .u.bit_width = 32 },
		// @TODO: Don't hardcode size_t!
		(IrType) { .t = IR_INT, .u.bit_width = 64 },
	};

	return builtin_function(
			builder, "memset", 3, (IrType) { .t = IR_POINTER }, arg_types);
}

IrValue build_builtin_va_start(IrBuilder *builder, IrValue va_list_ptr)
{
	IrInstr *instr = append_instr(builder);
	instr->op = OP_BUILTIN_VA_START;
	instr->type = (IrType) { .t = IR_VOID };
	instr->u.arg = va_list_ptr;

	return value_instr(instr);
}

IrValue build_builtin_va_arg(IrBuilder *builder, IrValue va_list_ptr,
		IrValue object_size)
{
	IrInstr *instr = append_instr(builder);
	instr->op = OP_BUILTIN_VA_ARG;
	instr->type = (IrType) { .t = IR_POINTER };
	instr->u.binary_op.arg1 = va_list_ptr;
	instr->u.binary_op.arg2 = object_size;

	return value_instr(instr);
}
