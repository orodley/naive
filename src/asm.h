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
	} t;

	union
	{
		u32 vreg_number;
		RegClass class;
	} u;
} Register;

typedef struct AsmLabel
{
	char *name;
	u32 offset;
} AsmLabel;

typedef struct AsmConst
{
	enum
	{
		ASM_CONST_IMMEDIATE,
		ASM_CONST_GLOBAL,
	} t;

	union
	{
		u64 immediate;
		struct AsmGlobal *global;
	} u;
} AsmConst;

typedef struct AsmValue
{
	// @TODO: This is kinda messy considering that OFFSET_REGISTER doesn't make
	// sense if this isn't set.
	bool is_deref;

	enum
	{
		ASM_VALUE_REGISTER,
		ASM_VALUE_OFFSET_REGISTER,
		ASM_VALUE_LABEL,
		ASM_VALUE_CONST,
	} t;

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
	} u;
} AsmValue;

#define ASM_OPS \
	X(NOP), \
	X(MOV), \
	X(MOVSX), \
	X(MOVZX), \
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
	X(SETG), \
	X(SETGE), \
	X(SETL), \
	X(SETLE), \
	X(TEST), \
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
	AsmValue args[3];

	u8 num_deps;
	u32 vreg_deps[2];

	AsmLabel *label;
} AsmInstr;

typedef struct ArgClass
{
	enum
	{
		ARG_CLASS_REG,
		ARG_CLASS_MEM,
	} t;

	union
	{
		struct
		{
			u32 vreg;
			RegClass reg;
		} reg;
		struct
		{
			u32 offset;
			u32 size;
			bool remains_in_memory;
		} mem;
	} u;
} ArgClass;

typedef struct CallSeq
{
	u32 stack_space;
	ArgClass *arg_classes;
} CallSeq;

typedef struct AsmFunction
{
	// @TODO: This seems unnecessary, since we have a name field on AsmGlobal
	// anyway. Remove.
	char *name;

	CallSeq call_seq;

	Array(AsmInstr) prologue;
	Array(AsmInstr) body;
	Array(AsmInstr) epilogue;

	Array(AsmLabel *) labels;
	AsmLabel *ret_label;
} AsmFunction;

typedef enum AsmLinkage
{
	ASM_GLOBAL_LINKAGE,
	ASM_LOCAL_LINKAGE,
} AsmLinkage;

typedef struct AsmGlobal
{
	enum
	{
		ASM_GLOBAL_FUNCTION,
		ASM_GLOBAL_VAR,
	} t;

	char *name;
	AsmLinkage linkage;
	// @TODO: Remove this too? Less easy than on IrGlobal, as the value isn't
	// a pointer.
	bool defined;
	i32 offset;
	struct AsmSymbol *symbol;

	union
	{
		AsmFunction function;
		struct
		{
			u32 size_bytes;
			u8 *value;
		} var;
	} u;
} AsmGlobal;

typedef enum FixupType
{
	FIXUP_RELATIVE,
	FIXUP_ABSOLUTE,
} FixupType;

typedef struct Fixup
{
	enum
	{
		FIXUP_LABEL,
		FIXUP_GLOBAL,
	} t;

	FixupType type;

	u32 offset;
	u32 next_instr_offset;
	u32 size_bytes;

	union
	{
		AsmLabel *label;
		AsmGlobal *global;
	} u;
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
		DATA_SECTION,
	} section;
	u32 defined;
	AsmLinkage linkage;
	u32 symtab_index;
	u32 string_table_offset_for_name;
	u32 offset;
	u32 size;
} AsmSymbol;

typedef struct Binary
{
	Array(u8) text;
	Array(u8) data;
	Array(AsmSymbol *) symbols;
	u32 bss_size;
} Binary;

void init_asm_function(AsmFunction *func, char *name);
void init_asm_module(AsmModule *asm_module, char *input_file_name);

void free_asm_module(AsmModule *asm_module);

AsmValue asm_vreg(u32 vreg_number, u8 width);
AsmValue asm_phys_reg(RegClass reg, u8 width);
AsmValue asm_offset_reg(RegClass reg, u8 width, AsmConst offset);
AsmValue asm_const(u64 constant);
AsmValue asm_deref(AsmValue asm_arg);
AsmValue asm_global(AsmGlobal *global);
AsmValue asm_label(AsmLabel *label);

void dump_asm_function(AsmFunction *asm_function);
void dump_asm_module(AsmModule *asm_module);

void init_binary(Binary *binary);
void free_binary(Binary *binary);

void assemble(AsmModule *asm_module, Binary *binary);

#endif
