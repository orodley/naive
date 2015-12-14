#include <assert.h>
#include <stdio.h>

#include "ir.h"
#include "misc.h"

void trans_unit_init(TransUnit *tu)
{
	array_init(&tu->functions, sizeof(Function), 10);
}

static inline void block_init(Block *block, const char *name, u32 arity)
{
	block->name = name;
	block->arity = arity;
	array_init(&block->instrs, sizeof(Instr), 10);
}

Function *trans_unit_add_function(TransUnit *tu, const char *name, u32 arity)
{
	Function *new_func = array_append(&tu->functions);

	new_func->name = name;
	block_init(&new_func->entry_block, "entry", arity);
	block_init(&new_func->ret_block, "ret", 1);

	return new_func;
}

static void dump_instr(Instr *instr)
{
	switch (instr->op) {
	case OP_CONST:
		printf("#%d = const(%ld)\n", instr->id, instr->val.constant);
		break;
	case OP_BRANCH:
		printf("branch(%s, #%d)\n",
				instr->val.branch.target_block->name,
				instr->val.branch.argument->id);
		break;
	}
}

void dump_trans_unit(TransUnit *tu)
{
	Array *functions = &tu->functions;
	for (u32 i = 0; i < functions->size; i++) {
		Function *f = array_ref(functions, i);

		printf("%s(%d)\n", f->name, f->entry_block.arity);
		puts("{");

		Block *block = &f->entry_block;
		for (;;) {
			printf("%s(%d):\n", block->name, block->arity);

			Array *instrs = &block->instrs;
			for (u32 i = 0; i < instrs->size; i++) {
				Instr *instr = array_ref(instrs, i);
				putchar('\t');
				dump_instr(instr);
			}

			if (instrs->size == 0)
				break;

			Instr *last_instr = array_ref(instrs, instrs->size - 1);
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
	Instr *instr = array_append(&block->instrs);
	instr->id = block->instrs.size - 1;
	return instr;
}

Instr *build_branch(Builder *builder, Block *block, Instr *instr)
{
	Instr *i = append_instr(builder->current_block);
	i->op = OP_BRANCH;
	i->val.branch.target_block = block;
	i->val.branch.argument = instr;

	return i;
}

Instr *build_const(Builder *builder, i64 value)
{
	Instr *i = append_instr(builder->current_block);
	i->type.bit_width = 32;
	i->op = OP_CONST;
	i->val.constant = value;

	return i;
}
