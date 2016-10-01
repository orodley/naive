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
	X(REG_CLASS_IP, "INVALID", "INVALID", "INVALID", "RIP"), \
	
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

typedef struct AsmConst
{
	enum
	{
		ASM_CONST_IMMEDIATE,
		ASM_CONST_GLOBAL,
	} type;

	union
	{
		u64 immediate;
		struct AsmGlobal *global;
	} val;
} AsmConst;

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
		ASM_ARG_CONST,
	} type;

	union
	{
		Register reg;
		struct
		{
			Register reg;
			AsmConst offset;
		} offset_register;
		AsmConst constant;
		AsmLabel *label;
	} val;
} AsmArg;

#define ASM_OPS \
	X(NOP), \
	X(MOV), \
	X(RET), \
	X(CALL), \
	X(XOR), \
	X(AND), \
	X(OR), \
	X(NOT), \
	X(ADD), \
	X(SUB), \
	X(PUSH), \
	X(POP), \
	X(IMUL), \
	X(IDIV), \
	X(CDQ), \
	X(CMP), \
	X(SETE), \
	X(SETNE), \
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
	u8 num_args;
	AsmArg args[3];

	u8 num_deps;
	u32 vreg_deps[2];

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
		ASM_GLOBAL_VAR,
	} type;

	char *name;
	bool defined;
	i32 offset;
	struct AsmSymbol *symbol;

	union
	{
		AsmFunction function;
		u32 var_size_bytes;
	} val;
} AsmGlobal;

typedef enum FixupType
{
	FIXUP_RELATIVE,
	FIXUP_ABSOLUTE,
} FixupType;

typedef struct Fixup
{
	FixupType type;

	u32 file_location;
	u32 next_instr_file_location;
	u32 size_bytes;

	enum
	{
		FIXUP_LABEL,
		FIXUP_GLOBAL,
	} source;

	union
	{
		AsmLabel *label;
		AsmGlobal *global;
	} val;
} Fixup;

typedef struct AsmModule
{
	char *input_file_name;

	Pool pool;

	Array(AsmGlobal *) globals;
	Array(Fixup) fixups;
} AsmModule;

typedef struct AsmSymbol
{
	char *name;
	enum
	{
		TEXT_SECTION,
		BSS_SECTION,
	} section;
	u32 defined;
	u32 symtab_index;
	u32 string_table_offset_for_name;
	u32 offset;
	u32 size;
} AsmSymbol;

void init_asm_function(AsmFunction *func, char *name);
void init_asm_module(AsmModule *asm_module, char *input_file_name);

void free_asm_module(AsmModule *asm_module);

AsmArg asm_vreg(u32 vreg_number, u8 width);
AsmArg asm_phys_reg(RegClass reg, u8 width);
AsmArg asm_offset_reg(RegClass reg, u8 width, AsmConst offset);
AsmArg asm_const(u64 constant);
AsmArg asm_deref(AsmArg asm_arg);
AsmArg asm_global(AsmGlobal *global);
AsmArg asm_label(AsmLabel *label);

void dump_asm_function(AsmFunction *asm_function);
void dump_asm_module(AsmModule *asm_module);

void assemble(AsmModule *asm_module, FILE *output_file, Array(AsmSymbol) *symbols);

#endif
