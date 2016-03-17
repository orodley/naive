#ifndef ASM_H_
#define ASM_H_

#include "array.h"

typedef struct AsmModule
{
	Array(AsmLine) lines;
} AsmModule;

typedef enum Register
{
	EAX,
} Register;

typedef struct AsmArg
{
	enum
	{
		REGISTER,
		CONST32,
		CONST64,
	} type;

	union
	{
		Register reg;
		u32 const32;
		u64 const64;
	} val;
} AsmArg;

typedef enum AsmOp
{
	MOV, RET,
} AsmOp;

typedef struct AsmLine
{
	enum
	{
		LABEL,
		INSTR,
	} type;

	union
	{
		struct
		{
			AsmOp op;
			AsmArg args[2];
		} instr;

		const char *label_name;
	} val;
} AsmLine;

void init_asm_module(AsmModule *asm_module);

void emit_label(AsmModule *asm_module, const char *name);
void emit_instr0(AsmModule *asm_module, AsmOp op);
void emit_instr2(AsmModule *asm_module, AsmOp op, AsmArg arg1, AsmArg arg2);

AsmArg asm_reg(Register reg);
AsmArg asm_const32(i32 constant);

void dump_asm_module(AsmModule *asm_module);

#endif
