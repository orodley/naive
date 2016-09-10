#ifndef NAIVE_ASM_H_
#define NAIVE_ASM_H_

#include <stdio.h>

#include "array.h"
#include "pool.h"

#define REG_CLASSES \
	X(INVALID_REG_CLASS, "INVALID", "INVALID", "INVALID", "INVALID"), \
	X(REG_CLASS_A,   "AL",   "AX",   "EAX",  "RAX"), \
	X(REG_CLASS_B,   "BL",   "BX",   "EBX",  "RBX"), \
	X(REG_CLASS_C,   "CL",   "CX",   "ECX",  "RCX"), \
	X(REG_CLASS_D,   "DL",   "DX",   "EDX",  "RDX"), \
	X(REG_CLASS_DI,  "DIL",  "DI",   "EDI",  "RDI"), \
	X(REG_CLASS_SI,  "SIL",  "SI",   "ESI",  "RSI"), \
	X(REG_CLASS_BP,  "BPL",  "BP",   "EBP",  "RBP"), \
	X(REG_CLASS_SP,  "SPL",  "SP",   "ESP",  "RSP"), \
	X(REG_CLASS_R8,  "R8B",  "R8W",  "R8D",  "R8"), \
	X(REG_CLASS_R9,  "R9B",  "R9W",  "R9D",  "R9"), \
	X(REG_CLASS_R10, "R10B", "R10W", "R10D", "R10"), \
	X(REG_CLASS_R11, "R11B", "R11W", "R11D", "R11"), \
	X(REG_CLASS_R12, "R12B", "R12W", "R12D", "R12"), \
	X(REG_CLASS_R13, "R13B", "R13W", "R13D", "R13"), \
	X(REG_CLASS_R14, "R14B", "R14W", "R14D", "R14"), \
	X(REG_CLASS_R15, "R15B", "R15W", "R15D", "R15"), \
	
#define X(x, b, w, d, o) x
typedef enum RegClass
{
	REG_CLASSES
} RegClass;
#undef X

typedef struct Register
{
	u8 width;

	enum
	{
		PHYS_REG,
		V_REG,
	} type;

	union
	{
		u32 vreg_number;
		RegClass class;
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
	AsmArg args[3];
	AsmLabel *label;
} AsmInstr;

typedef struct AsmFunction
{
	char *name;

	Array(AsmInstr) prologue;
	Array(AsmInstr) body;
	Array(AsmInstr) epilogue;

	Array(AsmLabel *) labels;
	AsmLabel *ret_label;
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

AsmArg asm_vreg(u32 vreg_number, u8 width);
AsmArg asm_phys_reg(RegClass reg, u8 width);
AsmArg asm_offset_reg(RegClass reg, u8 width, u64 offset);
AsmArg asm_const32(i32 constant);
AsmArg asm_deref(AsmArg asm_arg);
AsmArg asm_global(AsmGlobal *global);
AsmArg asm_label(AsmLabel *label);

inline bool asm_arg_is_const(AsmArg asm_arg)
{
	return (asm_arg.type == ASM_ARG_CONST8) || (asm_arg.type == ASM_ARG_CONST16) ||
		(asm_arg.type == ASM_ARG_CONST32) || (asm_arg.type == ASM_ARG_CONST64);
}

void dump_asm_function(AsmFunction *asm_function);
void dump_asm_module(AsmModule *asm_module);

void assemble(AsmModule *asm_module, FILE *output_file, Array(AsmSymbol) *symbols);

#endif
