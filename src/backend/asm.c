#include "backend/asm.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "array.h"
#include "backend/asm_gen.h"
#include "file.h"
#include "macros.h"
#include "maths.h"

void init_asm_module(AsmModule *asm_module, String input_file_name)
{
  asm_module->input_file_name = input_file_name;
  pool_init(&asm_module->pool, 1024);

  ARRAY_INIT(&asm_module->text.instrs, AsmInstr, 100);
  asm_module->text.bytes = EMPTY_ARRAY;
  ARRAY_INIT(&asm_module->data, u8, 100);
  asm_module->bss_size = 0;

  ARRAY_INIT(&asm_module->symbols, AsmSymbol *, 60);
  ARRAY_INIT(&asm_module->fixups, Fixup, 10);
}

void free_asm_module(AsmModule *asm_module)
{
  pool_free(&asm_module->pool);

  array_free(&asm_module->text.instrs);
  array_free(&asm_module->text.bytes);
  array_free(&asm_module->symbols);
  array_free(&asm_module->data);

  array_free(&asm_module->fixups);
}

AsmConst asm_const_imm(u64 value)
{
  return (AsmConst){
      .t = ASM_CONST_IMMEDIATE,
      .u.immediate = value,
  };
}

AsmConst asm_const_fixed_imm(u64 value, u32 width)
{
  return (AsmConst){
      .t = ASM_CONST_FIXED_IMMEDIATE,
      .u.fixed_immediate.value = value,
      .u.fixed_immediate.width = width,
  };
}

AsmConst asm_const_symbol(AsmSymbol *symbol)
{
  return (AsmConst){
      .t = ASM_CONST_SYMBOL,
      .u.symbol = symbol,
  };
}

AsmValue asm_vreg(u32 vreg_number, u8 width)
{
  PRECONDITION(width != 0);
  return (AsmValue){
      .is_deref = false,
      .t = ASM_VALUE_REGISTER,
      .u.reg.width = width,
      .u.reg.value_width = width,
      .u.reg.t = V_REG,
      .u.reg.u.vreg_number = vreg_number,
  };
}

AsmValue asm_float_vreg(u32 vreg_number, u8 width)
{
  PRECONDITION(width != 0);
  return (AsmValue){
      .is_deref = false,
      .t = ASM_VALUE_REGISTER,
      .u.reg.width = 128,
      .u.reg.value_width = width,
      .u.reg.t = V_REG,
      .u.reg.u.vreg_number = vreg_number,
  };
}

AsmValue asm_phys_reg(RegClass reg, u8 width)
{
  PRECONDITION(width != 0);
  return (AsmValue){
      .is_deref = false,
      .t = ASM_VALUE_REGISTER,
      .u.reg.width = reg_class_is_gpr(reg) ? width : 128,
      .u.reg.value_width = width,
      .u.reg.t = PHYS_REG,
      .u.reg.u.class = reg,
  };
}

AsmValue asm_deref(AsmValue asm_value)
{
  asm_value.is_deref = true;
  return asm_value;
}

Register *value_reg(AsmValue *arg)
{
  if (arg->t == ASM_VALUE_REGISTER) return &arg->u.reg;
  if (arg->t == ASM_VALUE_OFFSET_REGISTER) return &arg->u.offset_register.reg;
  return NULL;
}

AsmValue asm_offset_reg(RegClass reg, u8 width, AsmConst offset)
{
  if (offset.t == ASM_CONST_IMMEDIATE && offset.u.immediate == 0) {
    return asm_phys_reg(reg, width);
  } else {
    return (AsmValue){
        .is_deref = false,
        .t = ASM_VALUE_OFFSET_REGISTER,
        .u.offset_register.reg.width = width,
        .u.offset_register.reg.t = PHYS_REG,
        .u.offset_register.reg.u.class = reg,
        .u.offset_register.offset = offset,
    };
  }
}

AsmValue asm_imm(u64 value)
{
  return (AsmValue){
      .is_deref = false,
      .t = ASM_VALUE_CONST,
      .u.constant = asm_const_imm(value),
  };
}

AsmValue asm_fixed_imm(u64 value, u32 width)
{
  return (AsmValue){
      .is_deref = false,
      .t = ASM_VALUE_CONST,
      .u.constant = asm_const_fixed_imm(value, width),
  };
}

AsmValue asm_symbol(AsmSymbol *symbol)
{
  return (AsmValue){
      .is_deref = false,
      .t = ASM_VALUE_CONST,
      .u.constant =
          (AsmConst){
              .t = ASM_CONST_SYMBOL,
              .u.symbol = symbol,
          },
  };
}

bool reg_class_is_gpr(RegClass reg_class)
{
  return reg_class >= REG_CLASS_A && reg_class < REG_CLASS_IP;
}

// Certain instructions sign-extend their immediates. We need to know which
// ones so that we can correctly determine the width we need for the immediate.
bool is_sign_extending_op(AsmOp op)
{
  return op == ADD || op == AND || op == ADC ||
         // The manual is contradictory for CMP: on the one hand the description
         // for, say, "CMP r/m16, imm8" doesn't say it sign-extends the
         // immediate. On the other hand the main description for CMP says it
         // always sign-extends immediates to the length of the first operand.
         op == CMP ||
         // Ditto.
         op == IMUL ||
         // The rest are documented as expected.
         op == MOV || op == OR || op == SBB || op == SUB || op == TEST;

  // Should PUSH be included too? PUSH is weird. It only sign-extends if the
  // operand size is greater than the size of the immediate. The operand size
  // is determined by values in segment descriptors... the take-away for me
  // is don't push immediates.
}

static bool is_sign_extending_instr(AsmInstr *instr)
{
  return is_sign_extending_op(instr->op);
}

// We need to deal with negative numbers here. For example, -4 should fit into
// an 8-bit immediate, even though as a 64-bit int (which is how we store
// constants) this is 0xFFFFFFFFFFFFFFFC, which looks like it doesn't fit into
// 8 bits. Really the issue here isn't negative vs. positive numbers though -
// numbers are just patterns of bits, and all we care about is that we get the
// correct pattern of bits out the end. To deal with this, all we need is:
// * A canonical pattern of bits that we expect to end up with.
// * The bit width we're trying to fit into.
// * The bit width we'll be implicitly extended to, e.g.: for ADD r/m64, imm8
//   the immediate will be extended to 64 bits.
// * Whether the immediate is sign-extended. This is an important point because
//   without it we try to put 255 into an 8-bit immediate, and if it's
//   sign-extended to a larger width we end up with a different value since the
//   MSB is 1.
//
// Then to check if it fits we can do something like this:
// a = canonical input value
// b = truncate a to ext_width
// c = truncate a to imm_width
// d = sign- or zero-extend c to ext_width
// a fits iff d == b
static bool is_const_and_fits(
    AsmValue asm_value, u32 ext_width, u32 imm_width, bool sext)
{
  PRECONDITION(ext_width >= imm_width);

  if (asm_value.t != ASM_VALUE_CONST) {
    return false;
  }

  AsmConst constant = asm_value.u.constant;
  switch (constant.t) {
  case ASM_CONST_IMMEDIATE: {
    // Handle this case specially, as various bitwise calculations are
    // harder if we don't have any extra bits to work with.
    if (imm_width == 64) return true;

    u64 imm = constant.u.immediate;
    u64 canonical_value;

    if (ext_width == 64) {
      canonical_value = imm;
    } else {
      // If our canonical value doesn't fit into ext_width, then this
      // constant doesn't fit. "fitting" here means either the bits we
      // will truncate are all zero, or they are all one and the MSB is
      // one, so sign-extension would produce the same value.
      u64 ext_trunc_bits = imm >> ext_width;
      if (ext_trunc_bits != 0
          && !(
              (ext_trunc_bits == (1ULL << (64 - ext_width)) - 1)
              && ((imm >> (ext_width - 1)) & 1) == 1)) {
        return false;
      }

      canonical_value = imm & ((1ULL << ext_width) - 1);
    }

    u64 truncated = imm & ((1ULL << imm_width) - 1);
    u64 extended = truncated;
    if (sext && ((truncated & (1ULL << (imm_width - 1))) != 0)) {
      extended |= ((1ULL << (ext_width - imm_width)) - 1) << imm_width;
    }
    return canonical_value == extended;
  }
  case ASM_CONST_FIXED_IMMEDIATE:
    return imm_width == constant.u.fixed_immediate.width;
  case ASM_CONST_SYMBOL: return imm_width == 64;
  }

  UNREACHABLE;
}

#define X(x) #x
static char *asm_op_names[] = {ASM_OPS};
#undef X

#define X(x, b, d, w, o, do) \
  {                          \
    b, d, w, o, do           \
  }
static char *physical_register_names[][5] = {REG_CLASSES};
#undef X

void dump_phys_reg(RegClass class, u8 width)
{
  char *reg_name = physical_register_names[class][lowest_set_bit(width) - 3];
  for (u32 i = 0; reg_name[i] != '\0'; i++) putchar(tolower(reg_name[i]));
}

static void dump_register(Register reg)
{
  switch (reg.t) {
  case PHYS_REG: dump_phys_reg(reg.u.class, reg.width); break;
  case V_REG: printf("#%u(%u)", reg.u.vreg_number, reg.width); break;
  }
}

static void dump_immediate(u64 x)
{
  if ((i64)x < 0)
    printf("%" PRId64, (i64)x);
  else
    printf("%" PRIu64, (i64)x);
}

static void dump_asm_const(AsmConst constant)
{
  switch (constant.t) {
  case ASM_CONST_IMMEDIATE: dump_immediate(constant.u.immediate); break;
  case ASM_CONST_FIXED_IMMEDIATE: {
    char *size_name;
    switch (constant.u.fixed_immediate.width) {
    case 8: size_name = "byte"; break;
    case 16: size_name = "word"; break;
    case 32: size_name = "dword"; break;
    case 64: size_name = "qword"; break;
    default: UNREACHABLE;
    }
    printf("%s ", size_name);

    dump_immediate(constant.u.immediate);
    break;
  }
  case ASM_CONST_SYMBOL: printf("%s", constant.u.symbol->name); break;
  }
}

static void dump_symbol(AsmSymbol *symbol)
{
  char *name = symbol->name;
  if (symbol->linkage == ASM_GLOBAL_LINKAGE) printf("global %s\n", name);
  printf("%s:\n", name);
}

void dump_asm_instr_with_line_number(AsmInstr *instr, i32 line_no)
{
  if (instr->label != NULL) dump_symbol(instr->label);
  if (line_no != -1) printf("%u", line_no);

  putchar('\t');
  char *op_name = asm_op_names[instr->op];
  for (u32 i = 0; op_name[i] != '\0'; i++) putchar(tolower(op_name[i]));

  putchar(' ');

  for (u32 i = 0; i < instr->arity; i++) {
    if (i != 0) fputs(", ", stdout);

    AsmValue *arg = instr->args + i;
    if (arg->is_deref) putchar('[');
    switch (arg->t) {
    case ASM_VALUE_REGISTER: dump_register(arg->u.reg); break;
    case ASM_VALUE_OFFSET_REGISTER:
      dump_register(arg->u.offset_register.reg);
      fputs(" + ", stdout);
      dump_asm_const(arg->u.offset_register.offset);
      break;
    case ASM_VALUE_CONST: dump_asm_const(arg->u.constant); break;
    }
    if (arg->is_deref) putchar(']');
  }
  putchar('\n');
}

void dump_asm_instr(AsmInstr *instr)
{
  dump_asm_instr_with_line_number(instr, -1);
}

void dump_asm_module(AsmModule *asm_module)
{
  Array(AsmSymbol *) *symbols = &asm_module->symbols;
  for (u32 i = 0; i < symbols->size; i++) {
    AsmSymbol *symbol = *ARRAY_REF(symbols, AsmSymbol *, i);

    if (!symbol->defined) printf("extern %s\n", symbol->name);
  }
  putchar('\n');

  puts("section .text");
  Array(AsmInstr) *instrs = &asm_module->text.instrs;
  for (u32 i = 0; i < instrs->size; i++) {
    AsmInstr *instr = ARRAY_REF(instrs, AsmInstr, i);
    dump_asm_instr(instr);
  }
  putchar('\n');

  puts("section .data");
  Array(u8) *data = &asm_module->data;
  u32 next_symbol = 0;
  for (u32 i = 0; i < data->size; i++) {
    while (next_symbol < symbols->size
           && (*ARRAY_REF(symbols, AsmSymbol *, next_symbol))->section
                  != DATA_SECTION) {
      next_symbol++;
    }

    AsmSymbol *symbol = *ARRAY_REF(symbols, AsmSymbol *, next_symbol);
    if (next_symbol < symbols->size && symbol->section == DATA_SECTION
        && symbol->offset == i) {
      dump_symbol(symbol);
      next_symbol++;
    }

    // @TODO: Use d[wdq] as well when possible.
    printf("\tdb 0x%x\n", (unsigned int)*ARRAY_REF(data, u8, i));
  }

  puts("section .bss");
  u32 pos = 0;
  for (u32 i = 0; i < symbols->size; i++) {
    AsmSymbol *symbol = *ARRAY_REF(symbols, AsmSymbol *, i);
    if (symbol->section != BSS_SECTION) continue;

    ASSERT(symbol->offset == pos);

    dump_symbol(symbol);
    printf("\tresb 0x%x\n", symbol->size);

    pos += symbol->size;
  }
}

static void write_u8(Array(u8) *output, u8 x) { *ARRAY_APPEND(output, u8) = x; }

static void write_int_at(Array(u8) *output, u32 offset, u64 x, u32 size)
{
  ARRAY_ENSURE_ROOM(output, u8, offset + size);
  for (u32 n = 0; n < size; n++) {
    u8 byte = (x >> (n * 8)) & 0xFF;
    *ARRAY_REF(output, u8, offset + n) = byte;
  }
  if (output->size < offset + size) output->size = offset + size;
}

static void write_int(Array(u8) *output, u64 x, u32 size)
{
  write_int_at(output, output->size, x, size);
}

static RegClass get_reg_class(AsmValue *asm_value)
{
  Register reg;
  PRECONDITION(
      asm_value->t == ASM_VALUE_REGISTER
      || asm_value->t == ASM_VALUE_OFFSET_REGISTER);
  switch (asm_value->t) {
  case ASM_VALUE_REGISTER: reg = asm_value->u.reg; break;
  case ASM_VALUE_OFFSET_REGISTER: reg = asm_value->u.offset_register.reg; break;
  default: UNREACHABLE;
  }

  ASSERT(reg.t == PHYS_REG);
  return reg.u.class;
}

static u32 encoded_register_number(RegClass reg)
{
  switch (reg) {
  case INVALID_REG_CLASS: UNREACHABLE;
  case REG_CLASS_A: return 0;
  case REG_CLASS_C: return 1;
  case REG_CLASS_D: return 2;
  case REG_CLASS_B: return 3;
  case REG_CLASS_SP: return 4;
  case REG_CLASS_BP: return 5;
  case REG_CLASS_SI: return 6;
  case REG_CLASS_DI: return 7;
  case REG_CLASS_R8: return 8;
  case REG_CLASS_R9: return 9;
  case REG_CLASS_R10: return 10;
  case REG_CLASS_R11: return 11;
  case REG_CLASS_R12: return 12;
  case REG_CLASS_R13: return 13;
  case REG_CLASS_R14: return 14;
  case REG_CLASS_R15: return 15;
  case REG_CLASS_IP: UNREACHABLE;
  case REG_CLASS_XMM0: return 0;
  case REG_CLASS_XMM1: return 1;
  case REG_CLASS_XMM2: return 2;
  case REG_CLASS_XMM3: return 3;
  case REG_CLASS_XMM4: return 4;
  case REG_CLASS_XMM5: return 5;
  case REG_CLASS_XMM6: return 6;
  case REG_CLASS_XMM7: return 7;
  }

  UNREACHABLE;
}

#define MAX_OPCODE_SIZE 3

typedef struct EncodedInstr
{
  u8 rex_prefix;
  bool has_oso;
  u8 opcode_size;
  u8 opcode[MAX_OPCODE_SIZE];
  u8 opcode_extension;
  bool has_modrm;
  u8 mod;
  u8 reg;
  u8 rm;
  bool has_sib;
  u8 scale;
  u8 index;
  u8 base;
  i8 displacement_size;
  u64 displacement;
  Fixup *disp_fixup;
  i8 immediate_size;
  u64 immediate;
  Fixup *imm_fixup;
} EncodedInstr;

static void add_mod_rm_arg(
    AsmModule *asm_module, EncodedInstr *encoded_instr, AsmValue *asm_value,
    FixupType fixup_type)
{
  encoded_instr->has_modrm = true;

  if (asm_value->t == ASM_VALUE_REGISTER) {
    RegClass class = get_reg_class(asm_value);

    if (asm_value->is_deref) {
      switch (class) {
      default: {
        encoded_instr->mod = 0;
        encoded_instr->rm = encoded_register_number(class);
        return;
      }
      case REG_CLASS_SP:
      case REG_CLASS_R12: {
        // Mod = 0, R/M = 4 means SIB addressing
        encoded_instr->mod = 0;
        encoded_instr->rm = 4;
        // no index/scale
        encoded_instr->has_sib = true;
        encoded_instr->scale = 0;
        encoded_instr->index = 4;
        encoded_instr->base = encoded_register_number(class);

        return;
      }
      case REG_CLASS_BP:
      case REG_CLASS_R13: {
        // Mod = 1 means 8-bit displacement
        encoded_instr->mod = 1;
        encoded_instr->rm = encoded_register_number(class);
        encoded_instr->displacement_size = 1;
        encoded_instr->displacement = 0;

        return;
      }
      }
    } else {
      encoded_instr->mod = 3;
      encoded_instr->rm = encoded_register_number(class);
      return;
    }
  } else if (asm_value->t == ASM_VALUE_OFFSET_REGISTER) {
    ASSERT(asm_value->is_deref);

    AsmConst asm_const = asm_value->u.offset_register.offset;
    RegClass reg = get_reg_class(asm_value);

    u64 offset;
    switch (asm_const.t) {
    case ASM_CONST_SYMBOL:
      encoded_instr->mod = 2;

      // Dummy value, gets patched later.
      offset = 0;

      if (reg == REG_CLASS_IP) fixup_type = FIXUP_RELATIVE;

      Fixup *fixup = pool_alloc(&asm_module->pool, sizeof *fixup);
      *ARRAY_APPEND(&asm_module->fixups, Fixup *) = fixup;
      encoded_instr->disp_fixup = fixup;
      fixup->type = fixup_type;
      fixup->section = TEXT_SECTION;
      fixup->size_bytes = 4;
      fixup->symbol = asm_const.u.symbol;
      break;

    case ASM_CONST_IMMEDIATE:
      offset = asm_const.u.immediate;
      encoded_instr->displacement = offset;
      if ((i8)offset == (i64)offset) {
        encoded_instr->mod = 1;
        encoded_instr->displacement_size = 1;
      } else if ((offset & 0xFFFFFFFF) == offset) {
        encoded_instr->mod = 2;
        encoded_instr->displacement_size = 4;
      } else {
        ASSERT_FAIL("Offset too large!");
      }
      break;
    default: UNREACHABLE;
    }

    switch (reg) {
    case REG_CLASS_A:
    case REG_CLASS_C:
    case REG_CLASS_D:
    case REG_CLASS_B:
    case REG_CLASS_BP:
    case REG_CLASS_SI:
    case REG_CLASS_DI:
    case REG_CLASS_R8:
    case REG_CLASS_R9:
    case REG_CLASS_R10:
    case REG_CLASS_R11:
    case REG_CLASS_R13:
    case REG_CLASS_R14:
    case REG_CLASS_R15: {
      encoded_instr->rm = encoded_register_number(reg);
      break;
    }
    case REG_CLASS_R12:
    case REG_CLASS_SP:
      // Same as above: SIB addressing
      encoded_instr->rm = encoded_register_number(reg);
      encoded_instr->has_sib = true;
      encoded_instr->scale = 0;
      encoded_instr->index = 4;
      encoded_instr->base = 4;

      if (encoded_instr->mod == 1)
        encoded_instr->displacement_size = 1;
      else
        encoded_instr->displacement_size = 4;
      encoded_instr->displacement = offset;

      break;
    case REG_CLASS_IP:
      encoded_instr->mod = 0;
      encoded_instr->rm = 5;
      encoded_instr->displacement_size = 4;
      encoded_instr->displacement = 0;
      break;
    default: UNIMPLEMENTED("mod/rm arg for register class %u", reg);
    }
  } else {
    UNIMPLEMENTED("mod/rm arg for asm value type %u", asm_value->t);
  }
}

typedef enum ArgOrder
{
  INVALID,
  RM,
  MR
} ArgOrder;

static void write_bytes(Array(u8) *output, u32 size, u8 *bytes)
{
  ARRAY_APPEND_ELEMS(output, u8, size, bytes);
}

typedef enum RexPrefix
{
  REX_B = 1 << 0,
  REX_X = 1 << 1,
  REX_R = 1 << 2,
  REX_W = 1 << 3,
  REX_HIGH = 0x40,
} RexPrefix;

// Called by the generated function "assemble_instr".
static void encode_instr(
    Array(u8) *output, AsmModule *asm_module, AsmInstr *instr,
    ArgOrder arg_order, bool use_rex_w, bool use_oso, u32 opcode_size,
    u8 opcode[], bool reg_and_rm, i32 opcode_extension, i32 immediate_size,
    bool reg_in_opcode, FixupType fixup_type)
{
  PRECONDITION(instr != NULL);

  EncodedInstr encoded_instr;
  ZERO_STRUCT(&encoded_instr);
  encoded_instr.displacement_size = -1;
  encoded_instr.immediate_size = -1;

  if (use_rex_w) encoded_instr.rex_prefix |= REX_W;

  encoded_instr.has_oso = use_oso;

  encoded_instr.opcode_size = opcode_size;
  memcpy(encoded_instr.opcode, opcode, opcode_size);
  if (reg_in_opcode) {
    encoded_instr.opcode_extension =
        encoded_register_number(get_reg_class(instr->args));
  }

  if (reg_and_rm) {
    AsmValue *register_operand;
    AsmValue *memory_operand;
    if (arg_order == RM) {
      register_operand = instr->args;
      memory_operand = instr->args + 1;
    } else if (arg_order == MR) {
      memory_operand = instr->args;
      register_operand = instr->args + 1;
    } else if (instr->arity == 1) {
      memory_operand = instr->args;
      register_operand = NULL;
    } else {
      UNREACHABLE;
    }

    if (register_operand == NULL) {
      encoded_instr.reg = 0;
    } else {
      encoded_instr.reg =
          encoded_register_number(get_reg_class(register_operand));
    }
    add_mod_rm_arg(asm_module, &encoded_instr, memory_operand, fixup_type);
  } else if (opcode_extension != -1) {
    encoded_instr.reg = opcode_extension;
    add_mod_rm_arg(asm_module, &encoded_instr, instr->args, fixup_type);
  } else {
    // @NOTE: I'm not sure this is true in general, but it seems like there
    // are three cases:
    //   * The ModR/M byte contains a register and an r/m operand
    //   * The ModR/M byte contains an opcode extension and an r/m operand
    //   * The ModR/M byte is not present
    // This branch represents the third case. Since we already wrote the
    // opcode above there's nothing left to do.
  }

  // Check if we need an empty REX prefix. Registers SPL, BPL, SIL, and DIL are
  // encoded using the same values as AH, CH, DH, BH (respectively). They are
  // distinguished by the presence of a REX prefix.
  for (u32 i = 0; i < instr->arity; i++) {
    AsmValue *arg = instr->args + i;
    if (arg->t != ASM_VALUE_REGISTER) {
      continue;
    }

    Register reg = arg->u.reg;
    ASSERT(reg.t == PHYS_REG);

    if (reg.width == 8
        && (reg.u.class == REG_CLASS_SP || reg.u.class == REG_CLASS_BP
            || reg.u.class == REG_CLASS_SI || reg.u.class == REG_CLASS_DI)) {
      encoded_instr.rex_prefix |= REX_HIGH;
      break;
    }
  }

  // @TODO: This seems kinda redundant considering we already encode the
  // immediate size in AsmValue.
  if (immediate_size != -1) {
    encoded_instr.immediate_size = immediate_size;
    AsmValue *immediate_arg = NULL;
    for (u32 i = 0; i < instr->arity; i++) {
      if (instr->args[i].t == ASM_VALUE_CONST) {
        // Check that we only have one immediate.
        ASSERT(immediate_arg == NULL);
        immediate_arg = instr->args + i;
      }
    }
    ASSERT(immediate_arg != NULL);

    if (immediate_arg->t == ASM_VALUE_CONST) {
      AsmConst constant = immediate_arg->u.constant;
      switch (constant.t) {
      case ASM_CONST_SYMBOL: {
        Fixup *fixup = pool_alloc(&asm_module->pool, sizeof *fixup);
        *ARRAY_APPEND(&asm_module->fixups, Fixup *) = fixup;
        encoded_instr.imm_fixup = fixup;
        fixup->type = fixup_type;
        fixup->section = TEXT_SECTION;
        fixup->size_bytes = 4;
        fixup->symbol = constant.u.symbol;

        // Dummy value, gets patched later.
        encoded_instr.immediate = 0;
        break;
      }
      case ASM_CONST_IMMEDIATE:
        encoded_instr.immediate = constant.u.immediate;
        break;
      case ASM_CONST_FIXED_IMMEDIATE:
        encoded_instr.immediate = constant.u.fixed_immediate.value;
      }

    } else {
      UNREACHABLE;
    }
  }

  if (encoded_instr.has_modrm) {
    if (encoded_instr.reg >= 8) encoded_instr.rex_prefix |= REX_R;
    if (encoded_instr.rm >= 8) encoded_instr.rex_prefix |= REX_B;
  }
  if (encoded_instr.has_sib) {
    if (encoded_instr.index >= 8) encoded_instr.rex_prefix |= REX_X;
    if (encoded_instr.base >= 8) {
      // Make sure we didn't already use REX.B for the RM field.
      ASSERT((encoded_instr.rex_prefix & REX_B) == 0);
      encoded_instr.rex_prefix |= REX_B;
    }
  }
  if (encoded_instr.opcode_extension >= 8) {
    // Make sure we didn't already use REX.B for the RM field or SIB.
    ASSERT((encoded_instr.rex_prefix & REX_B) == 0);
    encoded_instr.rex_prefix |= REX_B;
  }

  // Done with figuring out the encoding, now lets write it.

  opcode_size = encoded_instr.opcode_size;
  opcode = encoded_instr.opcode;
  opcode[0] |= encoded_instr.opcode_extension & 7;
  if (opcode[0] == 0xF2 || opcode[0] == 0xF3 || opcode[0] == 0x66) {
    // This is a "mandatory prefix", used with some SIMD instructions. It's
    // listed as part of the opcode, but it must precede any REX prefix, if
    // present.
    write_u8(output, opcode[0]);
    opcode++;
    opcode_size--;
  }
  if (encoded_instr.has_oso) {
    write_u8(output, 0x66);
  }
  if (encoded_instr.rex_prefix != 0) {
    encoded_instr.rex_prefix |= REX_HIGH;
    write_u8(output, encoded_instr.rex_prefix);
  }
  write_bytes(output, opcode_size, opcode);
  if (encoded_instr.has_modrm) {
    u8 mod = encoded_instr.mod;
    u8 reg = encoded_instr.reg;
    u8 rm = encoded_instr.rm;
    u8 mod_rm_byte = ((mod & 3) << 6) | ((reg & 7) << 3) | (rm & 7);

    write_u8(output, mod_rm_byte);
  }
  if (encoded_instr.has_sib) {
    u8 scale = encoded_instr.scale;
    u8 index = encoded_instr.index;
    u8 base = encoded_instr.base;
    u8 sib_byte = ((scale & 3) << 6) | ((index & 7) << 3) | (base & 7);
    write_u8(output, sib_byte);
  }
  if (encoded_instr.disp_fixup != NULL)
    encoded_instr.disp_fixup->offset = output->size;
  if (encoded_instr.displacement_size != -1)
    write_int(
        output, encoded_instr.displacement, encoded_instr.displacement_size);
  if (encoded_instr.imm_fixup != NULL)
    encoded_instr.imm_fixup->offset = output->size;
  if (encoded_instr.immediate_size != -1)
    write_int(output, encoded_instr.immediate, encoded_instr.immediate_size);

  if (encoded_instr.disp_fixup != NULL)
    encoded_instr.disp_fixup->next_instr_offset = output->size;
  if (encoded_instr.imm_fixup != NULL)
    encoded_instr.imm_fixup->next_instr_offset = output->size;
}

// This is generated from "x64.enc", and defines the function "assemble_instr".
#include "backend/x64.inc"

void assemble(AsmModule *asm_module)
{
  // Reserve space for 3 bytes per instruction (pulling numbers out of thin
  // air here).
  ARRAY_INIT(&asm_module->text.bytes, u8, asm_module->text.instrs.size * 3);

  Array(AsmInstr) *instrs = &asm_module->text.instrs;
  for (u32 i = 0; i < instrs->size; i++) {
    AsmInstr *instr = ARRAY_REF(instrs, AsmInstr, i);

    u32 instr_start = asm_module->text.bytes.size;
    assemble_instr(&asm_module->text.bytes, asm_module, instr);

    AsmSymbol *symbol = instr->label;
    if (symbol != NULL) {
      ASSERT(symbol->section == TEXT_SECTION);
      ASSERT(symbol->defined);

      symbol->offset = instr_start;
    }
  }

  array_free(&asm_module->text.instrs);
  asm_module->text.instrs = EMPTY_ARRAY;

  Array(AsmSymbol *) *symbols = &asm_module->symbols;
  for (u32 i = 0; i < symbols->size; i++) {
    AsmSymbol *symbol = *ARRAY_REF(symbols, AsmSymbol *, i);
    // Add one to account for 0 = undef symbol index
    symbol->symtab_index = i + 1;
  }

  // @TODO: Emit relocations here instead?
  // @TODO: Remove the fixups we process here, so that write_elf_object_file
  // doesn't have to ignore them.
  for (u32 i = 0; i < asm_module->fixups.size; i++) {
    Fixup *fixup = *ARRAY_REF(&asm_module->fixups, Fixup *, i);
    AsmSymbol *symbol = fixup->symbol;
    if (fixup->type == FIXUP_ABSOLUTE || !symbol->defined
        || fixup->section != TEXT_SECTION || symbol->section != TEXT_SECTION) {
      continue;
    }

    // Relative accesses are relative to the start of the next instruction.
    i32 value = (i32)symbol->offset - (i32)fixup->next_instr_offset;
    write_int_at(
        &asm_module->text.bytes, fixup->offset, (u64)value, fixup->size_bytes);
  }
}
