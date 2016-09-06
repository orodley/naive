#ifndef NAIVE_ASM_H_
#define NAIVE_ASM_H_

#include <stdio.h>

#include "array.h"
#include "pool.h"

// @TODO: We need to unify registers that overlap like rax, eax, al, ah.
#define PHYSICAL_REGISTERS \
	X(INVALID_REGISTER), \
	X(RAX), \
	X(RBX), \
	X(RCX), \
	X(RDX), \
	X(RDI), \
	X(RSI), \
	X(RBP), \
	X(RSP), \
	X(R8), \
	X(R9), \
	X(R10), \
	X(R11), \
	X(R12), \
	X(R13), \
	X(R14), \
	X(R15),

#define X(x) x
typedef enum PhysicalRegister
{
	PHYSICAL_REGISTERS
} PhysicalRegister;
#undef X

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

typedef struct AsmLabel
{
	char *name;
	u32 file_location;
} AsmLabel;

typedef struct AsmArg
{
	// @TODO: This is kinda messy considering that OFFSET_REGISTER doesn't make
	// sense if this isn't set.
	bool is_deref;

	enum
	{
		ASM_ARG_REGISTER,
		ASM_ARG_OFFSET_REGISTER,
		ASM_ARG_LABEL,
		ASM_ARG_GLOBAL,

		// @TODO: Do we want to encode the size of the immediate here rather
		// than just having one type for all constants? Seems unnecessary.
		ASM_ARG_CONST8,
		ASM_ARG_CONST16,
		ASM_ARG_CONST32,
		ASM_ARG_CONST64,
	} type;

	union
	{
		Register reg;
		struct
		{
			Register reg;
			u64 offset;
		} offset_register;
		u64 constant;
		AsmLabel *label;
		struct AsmGlobal *global;
	} val;
} AsmArg;

#define ASM_OPS \
	X(NOP), \
	X(MOV), \
	X(RET), \
	X(CALL), \
	X(XOR), \
	X(ADD), \
	X(SUB), \
	X(PUSH), \
	X(POP), \
	X(IMUL), \
	X(CMP), \
	X(JMP), \
	X(JE)

#define X(x) x
typedef enum AsmOp
{
	ASM_OPS
} AsmOp;
#undef X

typedef struct AsmInstr
{
	AsmOp op;
	u32 num_args;
	AsmArg args[2];
	AsmLabel *label;
} AsmInstr;

typedef struct AsmFunction
{
	char *name;

	Array(AsmInstr) instrs;
	Array(AsmLabel *) labels;
} AsmFunction;

typedef struct AsmGlobal
{
	enum
	{
		ASM_GLOBAL_FUNCTION,
	} type;

	char *name;
	bool defined;
	i32 offset;
	struct AsmSymbol *symbol;

	union
	{
		AsmFunction function;
	} val;
} AsmGlobal;

typedef struct Fixup
{
	enum
	{
		FIXUP_LABEL,
		FIXUP_GLOBAL,
	} type;

	u32 file_location;
	u32 size_bytes;

	union
	{
		AsmLabel *label;
		AsmGlobal *global;
	} val;
} Fixup;

typedef struct AsmModule
{
	Array(AsmGlobal *) globals;

	Array(Fixup) fixups;

	Pool pool;
} AsmModule;

typedef struct AsmSymbol
{
	char *name;
	u32 defined;
	u32 symtab_index;
	u32 string_table_offset_for_name;
	u32 offset;
	u32 size;
} AsmSymbol;

void init_asm_function(AsmFunction *func, char *name);
void init_asm_module(AsmModule *asm_module);

void free_asm_module(AsmModule *asm_module);

AsmArg asm_virtual_register(u32 n);
AsmArg asm_physical_register(PhysicalRegister reg);
AsmArg asm_offset_register(PhysicalRegister reg, u64 offset);
AsmArg asm_const32(i32 constant);
AsmArg asm_deref(AsmArg asm_arg);
AsmArg asm_global(AsmGlobal *global);
AsmArg asm_label(AsmLabel *label);

void dump_asm_function(AsmFunction *asm_function);
void dump_asm_module(AsmModule *asm_module);

void assemble(AsmModule *asm_module, FILE *output_file, Array(AsmSymbol) *symbols);

#endif
