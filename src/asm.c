#include <stdio.h>

#include "array.h"
#include "asm.h"

static void dump_asm_function(AsmFunction *function);
static void dump_asm_instr(AsmInstr *instr);
static void dump_asm_args(AsmArg *args, u32 num_args);

void init_asm_module(AsmModule *asm_module)
{
	ARRAY_INIT(&asm_module->functions, AsmFunction, 10);
}

void init_asm_func(AsmFunction *asm_func, const char *name)
{
	ARRAY_INIT(&asm_func->instrs, AsmInstr, 25);
	asm_func->name = name;
}

void emit_instr0(AsmFunction *asm_func, AsmOp op)
{
	AsmInstr *instr = ARRAY_APPEND(&asm_func->instrs, AsmInstr);
	instr->op = op;
}

void emit_instr2(AsmFunction *asm_func, AsmOp op, AsmArg arg1, AsmArg arg2)
{
	AsmInstr *instr = ARRAY_APPEND(&asm_func->instrs, AsmInstr);
	instr->op = op;
	instr->args[0] = arg1;
	instr->args[1] = arg2;
}

AsmArg asm_reg(Register reg)
{
	AsmArg asm_arg = {
		.type = REGISTER,
		.val = {
			.reg = reg,
		},
	};

	return asm_arg;
}

AsmArg asm_const32(i32 constant)
{
	AsmArg asm_arg = {
		.type = CONST32,
		.val = {
			.const32 = constant,
		},
	};

	return asm_arg;
}

void dump_asm_module(AsmModule *asm_module)
{
	for (u32 i = 0; i < asm_module->functions.size; i++) {
		AsmFunction *function =
			ARRAY_REF(&asm_module->functions, AsmFunction, i);
		dump_asm_function(function);
	}
}

static void dump_asm_function(AsmFunction *function)
{
	printf("%s:\n", function->name);
	for (u32 i = 0; i < function->instrs.size; i++) {
		AsmInstr *instr =
			ARRAY_REF(&function->instrs, AsmInstr, i);
		dump_asm_instr(instr);
	}
}

static void dump_asm_instr(AsmInstr *instr)
{
	putchar('\t');

	switch (instr->op) {
	case MOV:
		fputs("mov ", stdout);
		dump_asm_args(instr->args, 2);
		break;
	case RET:
		fputs("ret", stdout);
		break;
	}

	putchar('\n');
}

static void dump_asm_args(AsmArg *args, u32 num_args)
{
	for (u32 i = 0; i < num_args; i++) {
		if (i != 0)
			fputs(", ", stdout);

		AsmArg *arg = &args[i];
		switch (arg->type) {
		case REGISTER:
			switch (arg->val.reg) {
			case EAX: fputs("eax", stdout); break;
			}
			break;
		case CONST32:
			printf("%" PRId32, arg->val.const32);
			break;
		case CONST64:
			printf("%" PRId64, arg->val.const64);
			break;
		}
	}
}
