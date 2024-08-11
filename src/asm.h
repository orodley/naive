#ifndef NAIVE_ASM_H_
#define NAIVE_ASM_H_

#include <stdio.h>

#include "array.h"
#include "pool.h"

#define REG_CLASSES                                                            \
  X(INVALID_REG_CLASS, "INVALID", "INVALID", "INVALID", "INVALID", "INVALID"), \
      X(REG_CLASS_A, "AL", "AX", "EAX", "RAX", "INVALID"),                     \
      X(REG_CLASS_B, "BL", "BX", "EBX", "RBX", "INVALID"),                     \
      X(REG_CLASS_C, "CL", "CX", "ECX", "RCX", "INVALID"),                     \
      X(REG_CLASS_D, "DL", "DX", "EDX", "RDX", "INVALID"),                     \
      X(REG_CLASS_DI, "DIL", "DI", "EDI", "RDI", "INVALID"),                   \
      X(REG_CLASS_SI, "SIL", "SI", "ESI", "RSI", "INVALID"),                   \
      X(REG_CLASS_BP, "BPL", "BP", "EBP", "RBP", "INVALID"),                   \
      X(REG_CLASS_SP, "SPL", "SP", "ESP", "RSP", "INVALID"),                   \
      X(REG_CLASS_R8, "R8B", "R8W", "R8D", "R8", "INVALID"),                   \
      X(REG_CLASS_R9, "R9B", "R9W", "R9D", "R9", "INVALID"),                   \
      X(REG_CLASS_R10, "R10B", "R10W", "R10D", "R10", "INVALID"),              \
      X(REG_CLASS_R11, "R11B", "R11W", "R11D", "R11", "INVALID"),              \
      X(REG_CLASS_R12, "R12B", "R12W", "R12D", "R12", "INVALID"),              \
      X(REG_CLASS_R13, "R13B", "R13W", "R13D", "R13", "INVALID"),              \
      X(REG_CLASS_R14, "R14B", "R14W", "R14D", "R14", "INVALID"),              \
      X(REG_CLASS_R15, "R15B", "R15W", "R15D", "R15", "INVALID"),              \
      X(REG_CLASS_IP, "INVALID", "INVALID", "INVALID", "RIP", "INVALID"),      \
      X(REG_CLASS_XMM0, "INVALID", "INVALID", "INVALID", "INVALID", "XMM0"),   \
      X(REG_CLASS_XMM1, "INVALID", "INVALID", "INVALID", "INVALID", "XMM1"),   \
      X(REG_CLASS_XMM2, "INVALID", "INVALID", "INVALID", "INVALID", "XMM2"),   \
      X(REG_CLASS_XMM3, "INVALID", "INVALID", "INVALID", "INVALID", "XMM3"),   \
      X(REG_CLASS_XMM4, "INVALID", "INVALID", "INVALID", "INVALID", "XMM4"),   \
      X(REG_CLASS_XMM5, "INVALID", "INVALID", "INVALID", "INVALID", "XMM5"),   \
      X(REG_CLASS_XMM6, "INVALID", "INVALID", "INVALID", "INVALID", "XMM6"),   \
      X(REG_CLASS_XMM7, "INVALID", "INVALID", "INVALID", "INVALID", "XMM7")

#define X(x, b, w, d, o, do) x
typedef enum RegClass
{
  REG_CLASSES
} RegClass;
#undef X

typedef enum RegType
{
  REG_TYPE_INTEGER,
  REG_TYPE_FLOAT,
} RegType;

typedef struct Register
{
  // The physical width of the register.
  u8 width;

  // The width of the value stored within. Will be equal to width for all
  // integer values, but for floats the physical width will be 128 (XMM
  // register), while this is 32, 64, or 80.
  u8 value_width;

  enum
  {
    PHYS_REG,
    V_REG,
  } t;

  union
  {
    RegClass class;
    u32 vreg_number;
  } u;
} Register;

typedef struct AsmConst
{
  enum
  {
    ASM_CONST_IMMEDIATE,
    ASM_CONST_FIXED_IMMEDIATE,
    ASM_CONST_SYMBOL,
  } t;

  union
  {
    u64 immediate;
    struct
    {
      u64 value;
      u32 width;
    } fixed_immediate;
    struct AsmSymbol *symbol;
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
  } u;
} AsmValue;

// @NOTE: Be sure to update compute_live_ranges and allocate_registers if more
// Jcc instructions are added.
#define ASM_OPS                                                               \
  X(NOP), X(MOV), X(MOVSX), X(MOVZX), X(MOVSS), X(MOVSD), X(RET), X(CALL),    \
      X(XOR), X(AND), X(OR), X(NOT), X(NEG), X(SHL), X(SHR), X(ADD), X(SUB),  \
      X(PUSH), X(POP), X(IMUL), X(IDIV), X(CDQ), X(CQO), X(CVTSI2SS),         \
      X(CVTSI2SD), X(CVTSS2SI), X(CVTSD2SI), X(CMP), X(SETE), X(SETNE),       \
      X(SETG), X(SETGE), X(SETL), X(SETLE), X(SETA), X(SETAE), X(SETB),       \
      X(SETBE), X(TEST), X(JMP), X(JE), X(JNE), X(JG), X(JGE), X(JL), X(JLE), \
      X(JA), X(JAE), X(JB), X(JBE), X(ADDSS), X(ADDSD), X(ADC), X(SBB),       \
      X(SYSCALL),

#define X(x) x
typedef enum AsmOp
{
  ASM_OPS
} AsmOp;
#undef X

typedef struct AsmInstr
{
  AsmOp op;
  u8 arity;
  AsmValue args[3];

  u8 num_deps;
  u32 vreg_deps[2];

  struct AsmSymbol *label;
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

// @TODO: Should this be in asm_gen.h instead?
typedef struct CallSeq
{
  u32 stack_space;
  ArgClass *arg_classes;
} CallSeq;

typedef enum AsmLinkage
{
  ASM_GLOBAL_LINKAGE,
  ASM_LOCAL_LINKAGE,
} AsmLinkage;

typedef enum AsmSymbolSection
{
  // @TODO: Maybe this should indicate "undefined"?
  UNKNOWN_SECTION,
  TEXT_SECTION,
  DATA_SECTION,
  BSS_SECTION,
} AsmSymbolSection;

typedef struct AsmSymbol
{
  char *name;
  AsmSymbolSection section;
  bool defined;
  AsmLinkage linkage;
  // @TODO: Do we need this? It's always just equal to the index in the
  // symbol array plus one, maybe we can compute it in elf.c on Symbol
  // instead?
  u32 symtab_index;
  // @NOTE: This does double duty. During asm_gen it stores the offset from
  // the start of the function in number of instructions, to be used for
  // asm-level analysis. After assembly it stores the offset in number of
  // bytes, for object file emission.
  u32 offset;
  u32 size;

  // Can be NULL if there's no corresponding IR global, e.g. for automatically
  // generated temporaries.
  struct IrGlobal *ir_global;

  // Used for asm_gen
  struct Pred *pred;
} AsmSymbol;

typedef enum FixupType
{
  FIXUP_RELATIVE,
  FIXUP_ABSOLUTE,
} FixupType;

typedef struct Fixup
{
  FixupType type;

  AsmSymbolSection section;

  u32 offset;
  u32 next_instr_offset;
  u32 size_bytes;

  AsmSymbol *symbol;
} Fixup;

typedef struct AsmModule
{
  char *input_file_name;
  Pool pool;

  struct
  {
    Array(AsmInstr) instrs;
    Array(u8) bytes;
  } text;

  Array(u8) data;
  u32 bss_size;

  Array(AsmSymbol *) symbols;
  Array(Fixup) fixups;
} AsmModule;

void init_asm_module(AsmModule *asm_module, char *input_file_name);

void free_asm_module(AsmModule *asm_module);

AsmConst asm_const_imm(u64 value);
AsmConst asm_const_fixed_imm(u64 value, u32 width);
AsmConst asm_const_symbol(AsmSymbol *symbol);

AsmValue asm_vreg(u32 vreg_number, u8 width);
AsmValue asm_float_vreg(u32 vreg_number, u8 width);
AsmValue asm_phys_reg(RegClass reg, u8 width);
AsmValue asm_offset_reg(RegClass reg, u8 width, AsmConst offset);
AsmValue asm_imm(u64 value);
AsmValue asm_fixed_imm(u64 value, u32 width);
AsmValue asm_symbol(AsmSymbol *symbol);
AsmValue asm_deref(AsmValue asm_value);

Register *value_reg(AsmValue *arg);

bool reg_class_is_gpr(RegClass reg_class);

bool is_sign_extending_op(AsmOp op);

void dump_asm_instr(AsmInstr *instr);
void dump_asm_instr_with_line_number(AsmInstr *instr, i32 line_no);
void dump_asm_module(AsmModule *asm_module);
void dump_phys_reg(RegClass class, u8 width);

void assemble(AsmModule *asm_module);

#endif
