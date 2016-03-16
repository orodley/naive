#ifndef ASM_H_
#define ASM_H_

#include "array.h"

typedef struct AsmModule
{
	Array(AsmFunction) functions;
} AsmModule;

typedef struct AsmFunction
{
	Array(AsmInstr) instrs;
	const char *name;
} AsmFunction;

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
		// @TODO: u64?
		i32 const32;
		i64 const64;
	} val;
} AsmArg;

typedef enum AsmOp
{
	MOV, RET,
} AsmOp;

typedef struct AsmInstr
{
	AsmOp op;

	AsmArg args[2];
} AsmInstr;

void init_asm_module(AsmModule *asm_module);

void init_asm_func(AsmFunction *asm_func, const char *name);

void emit_instr0(AsmFunction *asm_func, AsmOp op);
void emit_instr2(AsmFunction *asm_func, AsmOp op, AsmArg arg1, AsmArg arg2);

AsmArg asm_reg(Register reg);
AsmArg asm_const32(i32 constant);

void dump_asm_module(AsmModule *asm_module);

#endif
