#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "ir.h"
#include "misc.h"

void trans_unit_init(TransUnit *tu)
{
	ARRAY_INIT(&tu->functions, Function, 10);
}

static inline void block_init(Block *block, const char *name,
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

	ARRAY_INIT(&block->instrs, Instr, 10);
}

Function *trans_unit_add_function(TransUnit *tu, const char *name,
		IrType return_type, u32 arity, IrType *arg_types)
{
	Function *new_func = ARRAY_APPEND(&tu->functions, Function);

	new_func->name = name;
	block_init(&new_func->entry_block, "entry", arity, arg_types);
	block_init(&new_func->ret_block, "ret", 1, &return_type);

	return new_func;
}

static inline IrType function_return_type(Function *f)
{
	return f->ret_block.args[0].type;
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

static void dump_instr(Instr *instr)
{
	switch (instr->op) {
	case OP_BRANCH:
		printf("branch(%s, ", instr->val.branch.target_block->name);
		dump_value(instr->val.branch.argument);
		puts(")");
		break;
	}
}

void dump_trans_unit(TransUnit *tu)
{
	Array(Function) *functions = &tu->functions;
	for (u32 i = 0; i < functions->size; i++) {
		Function *f = ARRAY_REF(functions, Function, i);

		dump_type(function_return_type(f));
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

			Array(Instr) *instrs = &block->instrs;
			for (u32 i = 0; i < instrs->size; i++) {
				Instr *instr = ARRAY_REF(instrs, Instr, i);
				putchar('\t');
				dump_instr(instr);
			}

			if (instrs->size == 0)
				break;

			Instr *last_instr = ARRAY_REF(instrs, Instr, instrs->size - 1);
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

static inline Instr *append_instr(Block *block)
{
	Instr *instr = ARRAY_APPEND(&block->instrs, Instr);
	instr->id = block->instrs.size + block->arity - 1;
	return instr;
}

Instr *build_branch(Builder *builder, Block *block, Value value)
{
	Instr *i = append_instr(builder->current_block);
	i->op = OP_BRANCH;
	i->val.branch.target_block = block;
	i->val.branch.argument = value;

	return i;
}

Value value_const(i64 constant)
{
	Value value = { .kind = VALUE_CONST, .val = { .constant = constant } };

	return value;
}
