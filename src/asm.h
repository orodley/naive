#ifndef NAIVE_ASM_H_
#define NAIVE_ASM_H_

#include <stdio.h>

#include "array.h"

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
	bool is_deref;

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

typedef struct AsmInstr
{
	AsmOp op;
	AsmArg args[2];
} AsmInstr;

typedef struct AsmBlock
{
	Array(AsmInstr) instrs;
} AsmBlock;

typedef struct AsmModule
{
	Array(AsmBlock) blocks;
} AsmModule;

AsmArg asm_physical_register(PhysicalRegister reg);
AsmArg asm_virtual_register(u32 n);
AsmArg asm_const32(i32 constant);
AsmArg asm_deref(AsmArg asm_arg);

void dump_asm_module(AsmModule *asm_module);

u64 assemble(AsmModule *asm_module, FILE *output_file, u64 base_virtual_address);

#endif
