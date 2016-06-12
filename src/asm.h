#ifndef NAIVE_ASM_H_
#define NAIVE_ASM_H_

#include <stdio.h>

#include "array.h"

typedef enum PhysicalRegister
{
	EAX,
	RSP,
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
		OFFSET_REGISTER,
		CONST32,
		CONST64,
	} type;

	union
	{
		Register reg;
		struct
		{
			Register reg;
			u64 offset;
		} offset_register;
		u32 const32;
		u64 const64;
	} val;
} AsmArg;

typedef enum AsmOp
{
	MOV,
	RET,
	XOR,
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

void init_asm_block(AsmBlock *block);

AsmArg asm_virtual_register(u32 n);
AsmArg asm_physical_register(PhysicalRegister reg);
AsmArg asm_offset_register(PhysicalRegister reg, u64 offset);
AsmArg asm_const32(i32 constant);
AsmArg asm_deref(AsmArg asm_arg);

void dump_asm_module(AsmModule *asm_module);

u64 assemble(AsmModule *asm_module, FILE *output_file, u64 base_virtual_address);

#endif
