#ifndef NAIVE_ASM_H_
#define NAIVE_ASM_H_

#include <stdio.h>

#include "array.h"

typedef struct AsmModule
{
	Array(AsmLine) lines;
	u32 next_virtual_register;
} AsmModule;

typedef enum PhysicalRegister
{
	EAX,
} PhysicalRegister;

typedef struct Register
{
	enum
	{
		PHYSICAL_REGISTER,
		VIRTUAL_REGISTER,
	} type;

	union
	{
		u32 register_number;
		PhysicalRegister physical_register;
	} val;
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

		char *label_name;
	} val;
} AsmLine;

void init_asm_module(AsmModule *asm_module);

void emit_label(AsmModule *asm_module, char *name);
void emit_instr0(AsmModule *asm_module, AsmOp op);
AsmArg emit_instr2(AsmModule *asm_module, AsmOp op, AsmArg arg1, AsmArg arg2);

AsmArg asm_physical_register(PhysicalRegister reg);
AsmArg asm_const32(i32 constant);

void dump_asm_module(AsmModule *asm_module);

u64 assemble(AsmModule *asm_module, FILE *output_file, u64 base_virtual_address);

#endif
