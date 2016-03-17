#include <stdio.h>

#include "array.h"
#include "asm.h"

static void dump_asm_line(AsmLine *line);
static void dump_asm_args(AsmArg *args, u32 num_args);

void init_asm_module(AsmModule *asm_module)
{
	ARRAY_INIT(&asm_module->lines, AsmLine, 10);
}

void emit_label(AsmModule *asm_module, const char *name)
{
	AsmLine *line = ARRAY_APPEND(&asm_module->lines, AsmLine);
	line->type = LABEL;
	line->val.label_name = name;
}

void emit_instr0(AsmModule *asm_module, AsmOp op)
{
	AsmLine *line = ARRAY_APPEND(&asm_module->lines, AsmLine);
	line->type = INSTR;
	line->val.instr.op = op;
}

void emit_instr2(AsmModule *asm_module, AsmOp op, AsmArg arg1, AsmArg arg2)
{
	AsmLine *line = ARRAY_APPEND(&asm_module->lines, AsmLine);
	line->type = INSTR;
	line->val.instr.op = op;
	line->val.instr.args[0] = arg1;
	line->val.instr.args[1] = arg2;
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
	for (u32 i = 0; i < asm_module->lines.size; i++) {
		AsmLine *line = ARRAY_REF(&asm_module->lines, AsmLine, i);
		dump_asm_line(line);
	}
}

static void dump_asm_line(AsmLine *line)
{
	if (line->type == LABEL) {
		printf("%s:\n", line->val.label_name);
	} else {
		putchar('\t');

		switch (line->val.instr.op) {
			case MOV:
				fputs("mov ", stdout);
				dump_asm_args(line->val.instr.args, 2);
				break;
			case RET:
				fputs("ret", stdout);
				break;
		}

		putchar('\n');
	}
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
			printf("%" PRIu32, arg->val.const32);
			break;
		case CONST64:
			printf("%" PRIu64, arg->val.const64);
			break;
		}
	}
}
