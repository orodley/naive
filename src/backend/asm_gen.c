#include "backend/asm_gen.h"

#include <stdlib.h>

#include "array.h"
#include "assertions.h"
#include "backend/asm.h"
#include "backend/reg_alloc.h"
#include "flags.h"
#include "ir.h"
#include "macros.h"
#include "maths.h"

#define REGISTER_SAVE_AREA_SIZE                    \
  (STATIC_ARRAY_LENGTH(int_argument_registers) * 8 \
   + STATIC_ARRAY_LENGTH(float_argument_registers) * 16)

static void write_const(AsmModule *asm_module, IrConst *konst, Array(u8) *out);
static void asm_gen_call(AsmBuilder *builder, IrInstr *instr);

void init_asm_builder(AsmBuilder *builder, String input_file_name)
{
  init_asm_module(&builder->asm_module, input_file_name);

  builder->local_stack_usage = 0;
  builder->curr_sp_diff = 0;
  builder->virtual_registers = EMPTY_ARRAY;
  builder->global_temp_floats = 0;
}

void free_asm_builder(AsmBuilder *builder)
{
  free_asm_module(&builder->asm_module);

  if (ARRAY_IS_VALID(&builder->virtual_registers))
    array_free(&builder->virtual_registers);
}

AsmSymbol *add_asm_symbol(AsmBuilder *builder)
{
  Array(AsmSymbol *) *symbols = &builder->asm_module.symbols;
  AsmSymbol *asm_symbol =
      pool_alloc(&builder->asm_module.pool, sizeof *asm_symbol);
  ZERO_STRUCT(asm_symbol);
  *ARRAY_APPEND(symbols, AsmSymbol *) = asm_symbol;

  return asm_symbol;
}

AsmSymbol *add_label(AsmBuilder *builder, String name)
{
  AsmSymbol *symbol = add_asm_symbol(builder);
  *symbol = (AsmSymbol){
      .name = name,
      .section = TEXT_SECTION,
      .defined = true,
      .linkage = ASM_LOCAL_LINKAGE,
      .pred = NULL,
  };

  return symbol;
}

AsmInstr *emit_instr0(AsmBuilder *builder, AsmOp op)
{
  AsmInstr *instr = ARRAY_APPEND(builder->current_block, AsmInstr);
  instr->op = op;
  instr->arity = 0;
  instr->num_deps = 0;
  instr->label = NULL;

  return instr;
}

AsmInstr *emit_instr1(AsmBuilder *builder, AsmOp op, AsmValue arg1)
{
  AsmInstr *instr = ARRAY_APPEND(builder->current_block, AsmInstr);
  instr->op = op;
  instr->arity = 1;
  instr->args[0] = arg1;
  instr->num_deps = 0;
  instr->label = NULL;

  return instr;
}

AsmInstr *emit_instr2(
    AsmBuilder *builder, AsmOp op, AsmValue arg1, AsmValue arg2)
{
  AsmInstr *instr = ARRAY_APPEND(builder->current_block, AsmInstr);
  instr->op = op;
  instr->arity = 2;
  instr->args[0] = arg1;
  instr->args[1] = arg2;
  instr->num_deps = 0;
  instr->label = NULL;

  return instr;
}

AsmInstr *emit_instr3(
    AsmBuilder *builder, AsmOp op, AsmValue arg1, AsmValue arg2, AsmValue arg3)
{
  AsmInstr *instr = ARRAY_APPEND(builder->current_block, AsmInstr);
  instr->op = op;
  instr->arity = 3;
  instr->args[0] = arg1;
  instr->args[1] = arg2;
  instr->args[2] = arg3;
  instr->num_deps = 0;
  instr->label = NULL;

  return instr;
}

static bool asm_value_is_float(AsmBuilder *builder, AsmValue v)
{
  if (v.is_deref || v.t != ASM_VALUE_REGISTER) return false;

  Register r = v.u.reg;
  switch (r.t) {
  case PHYS_REG: return r.width == 128;
  case V_REG: {
    VReg *vreg = ARRAY_REF(&builder->virtual_registers, VReg, r.u.vreg_number);
    return vreg->type == REG_TYPE_FLOAT;
  }
  }
}

AsmInstr *emit_mov(AsmBuilder *builder, AsmValue dest, AsmValue src)
{
  PRECONDITION(
      dest.t != ASM_VALUE_CONST, "Bad MOV! Can't move into a constant");
  if (dest.t == ASM_VALUE_REGISTER && src.t == ASM_VALUE_REGISTER
      && !dest.is_deref && !src.is_deref) {
    PRECONDITION(
        dest.u.reg.width == src.u.reg.width,
        "Bad MOV! Registers are different sizes.");
  }

  bool dest_is_float = asm_value_is_float(builder, dest);
  bool src_is_float = asm_value_is_float(builder, src);
  if (dest_is_float || src_is_float) {
    u8 value_width;
    if (dest_is_float)
      value_width = dest.u.reg.value_width;
    else
      value_width = src.u.reg.value_width;

    if (value_width == 32) return emit_instr2(builder, MOVSS, dest, src);
    if (value_width == 64) return emit_instr2(builder, MOVSD, dest, src);
    UNIMPLEMENTED("MOV for %d-bit floats", value_width);
  }

  return emit_instr2(builder, MOV, dest, src);
}

static void add_dep(AsmInstr *instr, AsmValue dep)
{
  PRECONDITION(instr->num_deps < STATIC_ARRAY_LENGTH(instr->vreg_deps));
  PRECONDITION(dep.t == ASM_VALUE_REGISTER && dep.u.reg.t == V_REG);

  instr->vreg_deps[instr->num_deps++] = dep.u.reg.u.vreg_number;
}

static u32 new_vreg(AsmBuilder *builder)
{
  VReg *vreg = ARRAY_APPEND(&builder->virtual_registers, VReg);
  vreg->t = UNASSIGNED;
  vreg->type = REG_TYPE_INTEGER;
  vreg->live_range_start = vreg->live_range_end = -1;
  vreg->pre_alloced = false;

  return builder->virtual_registers.size - 1;
}

static u32 new_float_vreg(AsmBuilder *builder)
{
  VReg *vreg = ARRAY_APPEND(&builder->virtual_registers, VReg);
  vreg->t = UNASSIGNED;
  vreg->type = REG_TYPE_FLOAT;
  vreg->live_range_start = vreg->live_range_end = -1;
  vreg->pre_alloced = false;

  return builder->virtual_registers.size - 1;
}

// @TODO: We could use the return value to rewrite some stuff of the form:
// AsmValue vreg = ...;
// assign_vreg(instr, vreg);
//
// to:
// AsmValue vreg = assign_vreg(instr, ...);
//
// But I'm not sure if this is more readable. Maybe if the "..." part always
// has a lot in common that could be factored out too.
static AsmValue assign_vreg(IrInstr *instr, AsmValue vreg)
{
  PRECONDITION(vreg.t == ASM_VALUE_REGISTER && vreg.u.reg.t == V_REG);

  instr->vreg_number = vreg.u.reg.u.vreg_number;
  return vreg;
}

static AsmValue pre_alloced_vreg(AsmBuilder *builder, RegClass class, u8 width)
{
  RegType reg_type =
      reg_class_is_gpr(class) ? REG_TYPE_INTEGER : REG_TYPE_FLOAT;
  u32 vreg_number = reg_type == REG_TYPE_INTEGER ? new_vreg(builder)
                                                 : new_float_vreg(builder);

  VReg *vreg = ARRAY_LAST(&builder->virtual_registers, VReg);
  vreg->t = IN_REG;
  vreg->pre_alloced = true;
  vreg->u.assigned_register = class;

  if (vreg->type == REG_TYPE_INTEGER) return asm_vreg(vreg_number, width);
  return asm_float_vreg(vreg_number, width);
}

static AsmValue asm_vreg_of_type(u32 vreg_number, IrType type)
{
  PRECONDITION(type.t == IR_INT || type.t == IR_FLOAT || type.t == IR_POINTER);

  switch (type.t) {
  case IR_FLOAT: return asm_float_vreg(vreg_number, type.u.float_bits);
  case IR_INT:
  case IR_POINTER: return asm_vreg(vreg_number, size_of_ir_type(type) * 8);
  default: UNREACHABLE;
  }
}

static AsmValue new_asm_vreg_of_type(AsmBuilder *builder, IrType type)
{
  u32 vreg_number;
  if (type.t == IR_FLOAT) {
    vreg_number = new_float_vreg(builder);
  } else {
    vreg_number = new_vreg(builder);
  }
  return asm_vreg_of_type(vreg_number, type);
}

AsmValue return_reg_for_type(AsmBuilder *builder, IrType type)
{
  PRECONDITION(type.t == IR_INT || type.t == IR_FLOAT || type.t == IR_POINTER);

  return pre_alloced_vreg(
      builder, type.t == IR_FLOAT ? REG_CLASS_XMM0 : REG_CLASS_A,
      size_of_ir_type(type) * 8);
}

static AsmValue asm_const_float(AsmBuilder *builder, u8 bit_width, double value)
{
  String name = string_printf("__f%d", builder->global_temp_floats);
  builder->global_temp_floats++;

  AsmSymbol *symbol = add_asm_symbol(builder);
  symbol->name = name;
  symbol->defined = true;
  symbol->linkage = ASM_LOCAL_LINKAGE;
  symbol->section = DATA_SECTION;
  symbol->ir_global = NULL;
  symbol->size = bit_width / 8;

  IrConst konst = {
      .type = (IrType){.t = IR_FLOAT, .u.float_bits = bit_width},
      .u.floatt = value,
  };

  Array(u8) *data = &builder->asm_module.data;
  // @TODO: Alignment
  symbol->offset = data->size;
  write_const(&builder->asm_module, &konst, data);

  AsmValue rip_relative_addr =
      asm_offset_reg(REG_CLASS_IP, 64, asm_const_symbol(symbol));
  AsmValue vreg = asm_float_vreg(new_float_vreg(builder), bit_width);
  emit_mov(builder, vreg, asm_deref(rip_relative_addr));
  return vreg;
}

static AsmValue asm_gen_relational_instr(AsmBuilder *builder, IrInstr *instr);

static AsmValue asm_value(AsmBuilder *builder, IrValue value)
{
  switch (value.t) {
  case IR_VALUE_CONST_FLOAT:
    return asm_const_float(
        builder, value.type.u.float_bits, value.u.const_float);
  case IR_VALUE_CONST_INT: return asm_imm(value.u.const_int);
  case IR_VALUE_INSTR: {
    IrInstr *instr = value.u.instr;

    switch (instr->op) {
    // Most of the time OP_LOCAL and OP_FIELD are used only for OP_STORE
    // and OP_LOAD. In those cases we can usually fold the offset into the
    // MOV. If we use it for something else, we hit the cases below as a
    // fallback.
    // @TODO: The code below for OP_LOCAL and OP_FIELD always adds
    // instructions, even if it's called multiple times. We could check if
    // we've already asm_gen'ed these instructions and use the same vregs
    // we used last time, but it's not clear that rematerializing the value
    // is actually more expensive than introducing potentially very large
    // live ranges and hurting register allocation. It's a moot point for
    // now anyway, since our ISel generally won't produce multiple uses of
    // OP_FIELD instrs.
    case OP_LOCAL: {
      AsmValue vreg = asm_vreg(new_vreg(builder), 64);
      emit_mov(builder, vreg, asm_phys_reg(REG_CLASS_SP, 64));
      emit_instr2(
          builder, ADD, vreg,
          asm_imm(instr->u.local.stack_offset + builder->curr_sp_diff));
      return vreg;
    }
    // @TODO: Call asm_gen_pointer_instr and use LEA.
    case OP_FIELD: {
      IrValue ptr = instr->u.field.ptr;
      PRECONDITION(ptr.type.t == IR_POINTER);
      IrType type = instr->u.field.type;
      u32 field_num = instr->u.field.field_number;

      AsmValue temp_vreg = asm_vreg(new_vreg(builder), 64);
      assign_vreg(instr, temp_vreg);

      u32 offset;
      switch (type.t) {
      case IR_STRUCT: offset = type.u.strukt.fields[field_num].offset; break;
      case IR_ARRAY:
        offset = size_of_ir_type(*type.u.array.elem_type) * field_num;
        break;
      default: UNREACHABLE;
      }
      emit_mov(builder, temp_vreg, asm_value(builder, ptr));
      emit_instr2(builder, ADD, temp_vreg, asm_imm(offset));

      return temp_vreg;
    }
    // @NOTE: Handled specially for a similar reason to OP_FIELD and
    // OP_LOCAL above. These instructions are mostly used as the argument
    // to OP_COND, so we let OP_COND match them itself in those cases.
    case OP_CMP:
    case OP_CMPF: return asm_gen_relational_instr(builder, instr);
    case OP_CAST: {
      AsmValue cast_value = asm_value(builder, instr->u.arg);
      if (cast_value.t == ASM_VALUE_REGISTER)
        cast_value.u.reg.width = size_of_ir_type(instr->type) * 8;

      return cast_value;
    }
    default: {
      PRECONDITION(
          instr->vreg_number != -1,
          "VReg should be assigned for instr before passing to asm_value");
      return asm_vreg_of_type(instr->vreg_number, instr->type);
    }
    }
  }
  case IR_VALUE_ARG: {
    u32 index = value.u.arg_index;
    ArgClass *arg_class =
        builder->current_function->call_seq.arg_classes + index;

    switch (arg_class->t) {
    case ARG_CLASS_REG:
      return asm_vreg_of_type(arg_class->u.reg.vreg, value.type);
    case ARG_CLASS_MEM: {
      // Arguments passed on the stack sit above the previous frame
      // pointer and return address (hence 16 bytes).
      // See the System V x86-64 ABI spec, figure 3.3
      AsmValue bp_offset = asm_imm(arg_class->u.mem.offset + 16);

      u32 vreg = new_vreg(builder);
      AsmValue vreg64 = asm_vreg(vreg, 64);
      if (arg_class->u.mem.remains_in_memory) {
        emit_mov(builder, vreg64, asm_phys_reg(REG_CLASS_BP, 64));
        emit_instr2(builder, ADD, vreg64, bp_offset);
        return vreg64;
      } else {
        emit_mov(
            builder, vreg64,
            asm_deref(asm_offset_reg(REG_CLASS_BP, 64, bp_offset.u.constant)));
        return asm_vreg(vreg, size_of_ir_type(value.type) * 8);
      }
    }
    }

    UNREACHABLE;
  }
  case IR_VALUE_GLOBAL: {
    AsmSymbol *symbol = value.u.global->asm_symbol;
    PRECONDITION(
        symbol != NULL,
        "Symbol should be assigned for global '%s' before passing to asm_value",
        value.u.global->name);
    return asm_symbol(symbol);
  }
  }

  UNREACHABLE;
}

static AsmValue maybe_move_const_to_reg(
    AsmBuilder *builder, AsmValue asm_value, u32 other_operand_width, bool sext)
{
  if (asm_value.t == ASM_VALUE_CONST
      && asm_value.u.constant.t == ASM_CONST_IMMEDIATE) {
    u64 c = asm_value.u.constant.u.immediate;

    if (!sext) {
      if ((u32)c == c) return asm_value;
    } else if ((i32)c == (i64)c) {
      return asm_value;
    }

    // Generally there's no imm64, so we have to spill the immediate
    // to a register here.
    AsmValue const_spill = asm_vreg(new_vreg(builder), 64);

    emit_mov(builder, const_spill, asm_value);

    const_spill.u.reg.width = other_operand_width;
    return const_spill;
  }

  return asm_value;
}

static IrCmp maybe_flip_conditional(IrCmp cmp, IrValue *arg1, IrValue *arg2)
{
  if (arg1->t != IR_VALUE_CONST_INT) return cmp;

  // The only form of comparison between a register and an immediate has the
  // immediate on the RHS. So we need to swap the LHS and RHS if the
  // immediate is on the LHS.
  IrCmp flipped;
  switch (cmp) {
  // Antisymmetric relations.
  case CMP_SGT: flipped = CMP_SLT; break;
  case CMP_SLT: flipped = CMP_SGT; break;
  case CMP_SGTE: flipped = CMP_SLTE; break;
  case CMP_SLTE: flipped = CMP_SGTE; break;
  case CMP_UGT: flipped = CMP_ULT; break;
  case CMP_ULT: flipped = CMP_UGT; break;
  case CMP_UGTE: flipped = CMP_ULTE; break;
  case CMP_ULTE: flipped = CMP_UGTE; break;
  // Symmetric relations.
  case CMP_EQ: flipped = CMP_EQ; break;
  case CMP_NEQ: flipped = CMP_NEQ; break;
  }

  IrValue temp = *arg1;
  *arg1 = *arg2;
  *arg2 = temp;

  return flipped;
}

// @TODO: This should probably be done on the IR level instead.
static bool get_inner_cmp(AsmBuilder *builder, IrInstr *instr, IrInstr **out)
{
  if (instr->op != OP_CMP
      || (instr->u.cmp.cmp != CMP_EQ && instr->u.cmp.cmp != CMP_NEQ)) {
    *out = instr;
    return false;
  }

  IrValue arg1 = instr->u.binary_op.arg1;
  IrValue arg2 = instr->u.binary_op.arg2;

  u64 c;
  IrValue non_const_arg;
  if (arg1.t == IR_VALUE_CONST_INT) {
    ASSERT(
        arg2.t != IR_VALUE_CONST_INT,
        "If both arguments are const, we should have constant folded this "
        "already");
    c = arg1.u.const_int;
    non_const_arg = arg2;
  } else if (arg2.t == IR_VALUE_CONST_INT) {
    ASSERT(
        arg1.t != IR_VALUE_CONST_INT,
        "If both arguments are const, we should have constant folded this "
        "already");
    c = arg2.u.const_int;
    non_const_arg = arg1;
  } else {
    *out = instr;
    return false;
  }

  if (non_const_arg.t != IR_VALUE_INSTR || (c != 0 && c != 1)) {
    *out = instr;
    return false;
  }

  IrInstr *inner = non_const_arg.u.instr;

  if (inner->op != OP_CMP) {
    *out = instr;
    return false;
  }

  IrCmp cmp = instr->u.cmp.cmp;
  if (cmp == CMP_EQ && c == 0) return !get_inner_cmp(builder, inner, out);
  if (cmp == CMP_EQ && c == 1) return get_inner_cmp(builder, inner, out);
  if (cmp == CMP_NEQ && c == 0) return get_inner_cmp(builder, inner, out);
  if (cmp == CMP_NEQ && c == 1) return !get_inner_cmp(builder, inner, out);

  UNREACHABLE;
}

static IrCmp invert_cmp(IrCmp cmp)
{
  switch (cmp) {
  case CMP_EQ: return CMP_NEQ;
  case CMP_NEQ: return CMP_EQ;
  case CMP_SGT: return CMP_SLTE;
  case CMP_SGTE: return CMP_SLT;
  case CMP_SLT: return CMP_SGTE;
  case CMP_SLTE: return CMP_SGT;
  case CMP_UGT: return CMP_ULTE;
  case CMP_UGTE: return CMP_ULT;
  case CMP_ULT: return CMP_UGTE;
  case CMP_ULTE: return CMP_UGT;
  }

  UNREACHABLE;
}

static AsmValue asm_gen_relational_instr(AsmBuilder *builder, IrInstr *instr)
{
  PRECONDITION(instr->op == OP_CMP || instr->op == OP_CMPF);

  bool invert = get_inner_cmp(builder, instr, &instr);

  IrValue arg1 = instr->u.cmp.arg1;
  IrValue arg2 = instr->u.cmp.arg2;
  IrCmp cmp = maybe_flip_conditional(instr->u.cmp.cmp, &arg1, &arg2);

  if (invert) cmp = invert_cmp(cmp);

  AsmOp op;
  switch (cmp) {
  case CMP_EQ: op = SETE; break;
  case CMP_NEQ: op = SETNE; break;
  case CMP_SGT: op = SETG; break;
  case CMP_SGTE: op = SETGE; break;
  case CMP_SLT: op = SETL; break;
  case CMP_SLTE: op = SETLE; break;
  case CMP_UGT: op = SETA; break;
  case CMP_UGTE: op = SETAE; break;
  case CMP_ULT: op = SETB; break;
  case CMP_ULTE: op = SETBE; break;
  }

  AsmValue asm_arg1 = asm_value(builder, arg1);
  AsmValue asm_arg2 = asm_value(builder, arg2);

  u32 vreg = new_vreg(builder);

  ASSERT(asm_arg1.t == ASM_VALUE_REGISTER);

  emit_instr2(builder, XOR, asm_vreg(vreg, 32), asm_vreg(vreg, 32));
  AsmOp cmp_op;
  switch (instr->op) {
  case OP_CMP: cmp_op = CMP; break;
  case OP_CMPF:
    cmp_op = instr->u.cmp.arg1.type.u.float_bits == 32 ? UCOMISS : UCOMISD;
    break;
  default: UNREACHABLE;
  }
  emit_instr2(
      builder, cmp_op, asm_arg1,
      maybe_move_const_to_reg(builder, asm_arg2, asm_arg1.u.reg.width, true));
  emit_instr1(builder, op, asm_vreg(vreg, 8));

  return asm_vreg(vreg, 32);
}

static void asm_gen_binary_instr(AsmBuilder *builder, IrInstr *instr, AsmOp op)
{
  PRECONDITION(instr->type.t == IR_INT);

  AsmValue lhs = asm_value(builder, instr->u.binary_op.arg1);
  AsmValue rhs = asm_value(builder, instr->u.binary_op.arg2);

  AsmValue target = asm_vreg(new_vreg(builder), instr->type.u.bit_width);
  assign_vreg(instr, target);

  emit_mov(builder, target, lhs);
  emit_instr2(
      builder, op, target,
      maybe_move_const_to_reg(
          builder, rhs, instr->type.u.bit_width, is_sign_extending_op(op)));
}

static void asm_gen_binary_float_instr(
    AsmBuilder *builder, IrInstr *instr, AsmOp float_op, AsmOp double_op)
{
  PRECONDITION(instr->type.t == IR_FLOAT);

  AsmValue lhs = asm_value(builder, instr->u.binary_op.arg1);
  AsmValue rhs = asm_value(builder, instr->u.binary_op.arg2);

  AsmValue target =
      asm_float_vreg(new_float_vreg(builder), instr->type.u.float_bits);
  assign_vreg(instr, target);
  emit_mov(builder, target, lhs);

  AsmOp op;
  if (instr->type.u.float_bits == IR_FLOAT_32) {
    op = float_op;
  } else if (instr->type.u.float_bits == IR_FLOAT_64) {
    op = double_op;
  } else {
    UNIMPLEMENTED(
        "Binary instruction for %d-bit floats", instr->type.u.float_bits);
  }

  emit_instr2(builder, op, target, rhs);
}

// @TODO: This is a very naive translation that produces incorrect results in
// some situations.
//
// We unconditionally copy into the phi vreg for all successors, so if the phi
// vreg is live across the other outgoing branch, we'll have clobbered it:
//
// For example:
// bb0:
//     #0 = ...
//     jump(bb1)
// bb1:
//     #1 = phi((bb0, #0), (bb1, #2))
//     #2 = ...
//     cond(#2, bb1, bb2)
// bb2:
//     #3 = add(#1, 1)
//
// For this function we'll insert a copy from #2 into the vreg of #1 at the end
// of bb1. Then if the edge to bb2 is taken instead, #3 will end up using the
// value of #2 instead of #1 as desired.
//
// The other issue is the swap problem -- if a block has multiple phi nodes,
// they're supposed to all execute concurrently. So if we have two phi nodes
// that swap values, we'll use sequential copies which don't implement this
// correctly.
//
// We get away with it for now simply because we don't do SSA formation for
// local variables, so we only emit phi nodes for conditional expressions, which
// don't have these problems. Once we start doing so, we'll have to fix this.
//
// We can fix both of these problems with a better approach, something like the
// "Unified Approach to Out-of-SSA Translation" given on p.490 of "Engineering a
// Compiler", 3rd edition.
static void handle_phi_nodes(
    AsmBuilder *builder, IrBlock *src_block, IrBlock *dest_block)
{
  Array(IrInstr *) *dest_instrs = &dest_block->instrs;
  for (u32 ir_instr_index = 0; ir_instr_index < dest_instrs->size;
       ir_instr_index++) {
    IrInstr *instr = *ARRAY_REF(dest_instrs, IrInstr *, ir_instr_index);
    if (instr->op != OP_PHI) break;

    if (instr->vreg_number == -1) {
      instr->vreg_number = instr->type.t == IR_FLOAT ? new_float_vreg(builder)
                                                     : new_vreg(builder);
    }

    bool encountered_src_block = false;
    for (u32 phi_param_index = 0; phi_param_index < instr->u.phi.arity;
         phi_param_index++) {
      IrPhiParam *param = instr->u.phi.params + phi_param_index;
      if (param->block == src_block) {
        u32 size = size_of_ir_type(instr->type) * 8;
        AsmValue target = instr->type.t == IR_FLOAT
                              ? asm_float_vreg(instr->vreg_number, size)
                              : asm_vreg(instr->vreg_number, size);
        emit_mov(builder, target, asm_value(builder, param->value));

        encountered_src_block = true;
        break;
      }
    }
    ASSERT(
        encountered_src_block,
        "Phi node in block %s has no parameter for incoming block %s",
        dest_block->name, src_block->name);
  }
}

static RegClass int_argument_registers[] = {
    REG_CLASS_DI, REG_CLASS_SI, REG_CLASS_D,
    REG_CLASS_C,  REG_CLASS_R8, REG_CLASS_R9,
};

static RegClass float_argument_registers[] = {
    REG_CLASS_XMM0, REG_CLASS_XMM1, REG_CLASS_XMM2, REG_CLASS_XMM3,
    REG_CLASS_XMM4, REG_CLASS_XMM5, REG_CLASS_XMM6, REG_CLASS_XMM7,
};

// @TODO: This doesn't conform to the ABI, but it's definitely okay as long as
// we only call/are called by stuff we compile, and it's probably okay as long
// as structs passed/returned by value aren't involved.
// The specific issue is that we always classify structs as memory arguments
// and pass them by the stack, but the calling convention is more complicated
// than that.
static CallSeq classify_arguments(u32 arity, IrType *arg_types)
{
  ArgClass *arg_classes = malloc(arity * sizeof *arg_classes);

  u32 curr_offset = 0;
  u32 curr_int_reg_index = 0;
  u32 curr_float_reg_index = 0;

  for (u32 i = 0; i < arity; i++) {
    ArgClass *arg_class = arg_classes + i;
    IrType *arg_type = arg_types + i;
    switch (arg_type->t) {
    case IR_INT:
    case IR_POINTER:
      if (curr_int_reg_index < STATIC_ARRAY_LENGTH(int_argument_registers)) {
        arg_class->t = ARG_CLASS_REG;
        arg_class->u.reg.reg = int_argument_registers[curr_int_reg_index++];
      } else {
        arg_class->t = ARG_CLASS_MEM;
        arg_class->u.mem.offset = curr_offset;
        arg_class->u.mem.size = 8;
        arg_class->u.mem.remains_in_memory = false;
        curr_offset += 8;
      }
      break;
    case IR_FLOAT:
      if (curr_float_reg_index
          < STATIC_ARRAY_LENGTH(float_argument_registers)) {
        arg_class->t = ARG_CLASS_REG;
        arg_class->u.reg.reg = float_argument_registers[curr_float_reg_index++];
      } else {
        arg_class->t = ARG_CLASS_MEM;
        arg_class->u.mem.offset = curr_offset;
        arg_class->u.mem.size = arg_type->u.float_bits / 8;
        arg_class->u.mem.remains_in_memory = false;
      }
      break;
    case IR_STRUCT: {
      arg_class->t = ARG_CLASS_MEM;
      u32 size = size_of_ir_type(*arg_type);
      arg_class->u.mem.offset = curr_offset;
      arg_class->u.mem.size = size;
      arg_class->u.mem.remains_in_memory = true;
      curr_offset += size;
      break;
    }
    case IR_ARRAY:
    case IR_FUNCTION:
    case IR_VOID: UNREACHABLE;
    }
  }

  return (CallSeq){
      .stack_space = curr_offset,
      .arg_classes = arg_classes,
  };
}

static AsmValue asm_gen_pointer_instr(AsmBuilder *builder, IrValue pointer)
{
  switch (pointer.t) {
  case IR_VALUE_INSTR: {
    IrInstr *instr = pointer.u.instr;
    switch (instr->op) {
    case OP_LOCAL: {
      u32 offset = instr->u.local.stack_offset + builder->curr_sp_diff;
      return asm_offset_reg(REG_CLASS_SP, 64, asm_const_imm(offset));
    }
    case OP_FIELD: {
      AsmValue inner = asm_gen_pointer_instr(builder, instr->u.field.ptr);
      IrType type = instr->u.field.type;
      u32 field_num = instr->u.field.field_number;

      u32 offset;
      switch (type.t) {
      case IR_STRUCT: offset = type.u.strukt.fields[field_num].offset; break;
      case IR_ARRAY:
        offset = size_of_ir_type(*type.u.array.elem_type) * field_num;
        break;
      default: UNREACHABLE;
      }

      if (offset == 0) return inner;

      switch (inner.t) {
      case ASM_VALUE_REGISTER:
        return (AsmValue){
            .t = ASM_VALUE_OFFSET_REGISTER,
            .u.offset_register.reg = inner.u.reg,
            .u.offset_register.offset = asm_const_imm(offset),
        };
      case ASM_VALUE_OFFSET_REGISTER: {
        AsmConst reg_offset = inner.u.offset_register.offset;
        switch (reg_offset.t) {
        case ASM_CONST_IMMEDIATE:
          return (AsmValue){
              .t = ASM_VALUE_OFFSET_REGISTER,
              .u.offset_register.reg = inner.u.offset_register.reg,
              .u.offset_register.offset =
                  asm_const_imm(reg_offset.u.immediate + offset),
          };
        case ASM_CONST_SYMBOL: {
          // @TODO: In this case we should still be able to use an
          // offset register, but we'd need to use a .rela relocation
          // with an additional offset, which we currently don't
          // support. Until then we just use an ADD. Since this
          // returns a vreg we can at least fold any additional
          // offsets into an offset_register.
          AsmValue temp_vreg = asm_vreg(new_vreg(builder), 64);
          assign_vreg(instr, temp_vreg);

          emit_mov(builder, temp_vreg, asm_symbol(reg_offset.u.symbol));
          return (AsmValue){
              .t = ASM_VALUE_OFFSET_REGISTER,
              .u.offset_register.reg = temp_vreg.u.reg,
              .u.offset_register.offset = asm_const_imm(offset),
          };
        }
        case ASM_CONST_FIXED_IMMEDIATE: UNREACHABLE;
        }
        break;
      }
      default: break;
      }

      // @TODO: This code doesn't actually work, but it's never hit in
      // any test cases or in self-hosting. Figure out if it's possible,
      // and if so make it work.
      UNREACHABLE;
      // Fall back to computing the offset using ADD in cases where we
      // can't use an offset register.
      AsmValue temp_vreg = asm_vreg(new_vreg(builder), 64);
      assign_vreg(instr, temp_vreg);

      emit_mov(builder, temp_vreg, inner);
      emit_instr2(builder, ADD, temp_vreg, asm_imm(offset));

      return asm_value(builder, pointer);
    }
    // @TODO: Handle OP_ADD and OP_SUB. This is a bit trickier because I
    // think we have to reach through casts?
    default: return asm_value(builder, pointer);
    }
  }
  case IR_VALUE_GLOBAL: {
    return asm_offset_reg(
        REG_CLASS_IP, 64, asm_const_symbol(pointer.u.global->asm_symbol));
  }
  default: return asm_value(builder, pointer);
  }
}

static bool asm_gen_cond_of_cmp(AsmBuilder *builder, IrInstr *cond)
{
  PRECONDITION(cond->op == OP_COND);
  IrValue condition = cond->u.cond.condition;
  if (condition.t != IR_VALUE_INSTR) return false;

  IrInstr *cond_instr = condition.u.instr;
  bool invert = get_inner_cmp(builder, cond_instr, &cond_instr);
  if (cond_instr->op != OP_CMP && cond_instr->op != OP_CMPF) {
    return false;
  }

  IrValue arg1 = cond_instr->u.cmp.arg1;
  IrValue arg2 = cond_instr->u.cmp.arg2;
  IrCmp cmp = maybe_flip_conditional(cond_instr->u.cmp.cmp, &arg1, &arg2);

  if (invert) cmp = invert_cmp(cmp);

  AsmOp jcc;
  switch (cmp) {
  case CMP_EQ: jcc = JE; break;
  case CMP_NEQ: jcc = JNE; break;
  case CMP_SGT: jcc = JG; break;
  case CMP_SGTE: jcc = JGE; break;
  case CMP_SLT: jcc = JL; break;
  case CMP_SLTE: jcc = JLE; break;
  case CMP_UGT: jcc = JA; break;
  case CMP_UGTE: jcc = JAE; break;
  case CMP_ULT: jcc = JB; break;
  case CMP_ULTE: jcc = JBE; break;
  }

  AsmValue arg2_value = maybe_move_const_to_reg(
      builder, asm_value(builder, arg2), size_of_ir_type(arg1.type) * 8, true);

  AsmOp op;
  switch (cond_instr->op) {
  case OP_CMP: op = CMP; break;
  case OP_CMPF:
    op = cond_instr->u.cmp.arg1.type.u.float_bits == 32 ? UCOMISS : UCOMISD;
    break;
  default: UNREACHABLE;
  }
  emit_instr2(builder, op, asm_value(builder, arg1), arg2_value);
  emit_instr1(builder, jcc, asm_symbol(cond->u.cond.then_block->label));
  // The "else" case is handled by the caller.

  return true;
}

// @TODO: Can ir_global be passed as a function instead, or pushed into the
// builder as the current function?
static void asm_gen_instr(
    AsmBuilder *builder, IrGlobal *ir_global, IrBlock *curr_block,
    IrInstr *instr)
{
  PRECONDITION(ir_global->type.t == IR_FUNCTION);

  switch (instr->op) {
  case OP_INVALID: UNREACHABLE;
  case OP_LOCAL: {
    // @TODO: Alignment of stack slots. This could probably use similar
    // logic to that of struct layout.
    instr->u.local.stack_offset = builder->local_stack_usage;
    builder->local_stack_usage += size_of_ir_type(instr->u.local.type);

    break;
  }
  case OP_FIELD:
    // Don't do anything. OP_STORE and OP_LOAD handle args of type OP_FIELD
    // directly, and others uses are handled by asm_value.
    break;
  case OP_RET_VOID:
    emit_instr1(builder, JMP, asm_symbol(builder->ret_label));

    break;
  case OP_RET: {
    IrValue arg = instr->u.arg;
    ASSERT(
        ir_type_eq(ir_global->type.u.function.return_type, &arg.type),
        "IR function return type does not match value passed to ret "
        "instruction");

    AsmValue return_reg =
        return_reg_for_type(builder, *ir_global->type.u.function.return_type);
    emit_mov(builder, return_reg, asm_value(builder, arg));
    emit_instr1(builder, JMP, asm_symbol(builder->ret_label));

    break;
  }
  case OP_JUMP:
    handle_phi_nodes(builder, curr_block, instr->u.target_block);
    emit_instr1(builder, JMP, asm_symbol(instr->u.target_block->label));
    break;
  case OP_COND: {
    IrValue condition = instr->u.cond.condition;
    if (condition.t == IR_VALUE_CONST_INT) {
      IrBlock *block = condition.u.const_int ? instr->u.cond.then_block
                                             : instr->u.cond.else_block;
      IrBlock *other_block = condition.u.const_int ? instr->u.cond.else_block
                                                   : instr->u.cond.then_block;

      handle_phi_nodes(builder, curr_block, block);
      emit_instr1(builder, JMP, asm_symbol(block->label));

      // In this case we are deliberately emitting dead code (after the
      // unconditional jump just above). This is because handle_phi_nodes
      // assigns a virtual register for any unassigned phi nodes at the
      // start of the target block. If all entries into a block are
      // constant-folded away like this we still want to assign a vreg to
      // the phi node so it can be compiled correctly.
      //
      // If we don't want to emit this dead code then the dead block
      // should be removed entirely by IR-level DCE. If not, we should
      // still compile the resulting IR correctly even if it's suboptimal.
      handle_phi_nodes(builder, curr_block, other_block);
    } else {
      handle_phi_nodes(builder, curr_block, instr->u.cond.then_block);

      // @TODO: Special case isel for OP_NOT as well.
      if (!asm_gen_cond_of_cmp(builder, instr)) {
        emit_instr2(builder, CMP, asm_value(builder, condition), asm_imm(0));
        emit_instr1(builder, JNE, asm_symbol(instr->u.cond.then_block->label));
      }

      handle_phi_nodes(builder, curr_block, instr->u.cond.else_block);
      emit_instr1(builder, JMP, asm_symbol(instr->u.cond.else_block->label));
    }
    break;
  }
  case OP_PHI: {
    // Phi nodes are handled by asm_gen for the incoming branches, and
    // require no codegen in their containing block. All we need to do is
    // make sure we have a vreg assigned, as we might not have visited the
    // incoming blocks yet.
    if (instr->vreg_number == -1) {
      instr->vreg_number = instr->type.t == IR_FLOAT ? new_float_vreg(builder)
                                                     : new_vreg(builder);
    }

    break;
  }
  case OP_STORE: {
    IrValue ir_pointer = instr->u.binary_op.arg1;
    IrValue ir_value = instr->u.binary_op.arg2;
    IrType type = ir_value.type;
    u32 bit_width = size_of_ir_type(type) * 8;
    if (type.t == IR_FLOAT) bit_width = 128;

    AsmValue value = asm_value(builder, ir_value);
    switch (value.t) {
    case ASM_VALUE_REGISTER: value.u.reg.width = bit_width; break;
    case ASM_VALUE_CONST: {
      // @TODO: Use maybe_move_const_to_reg instead?
      if (size_of_ir_type(type) == 8) {
        AsmValue temp_vreg = asm_vreg(new_vreg(builder), 64);
        emit_mov(builder, temp_vreg, value);
        value = temp_vreg;

        break;
      }

      AsmConst konst = value.u.constant;
      switch (konst.t) {
      case ASM_CONST_SYMBOL: break;
      case ASM_CONST_FIXED_IMMEDIATE:
        ASSERT(konst.u.fixed_immediate.width == bit_width);
        break;
      case ASM_CONST_IMMEDIATE:
        value.u.constant = asm_const_fixed_imm(konst.u.immediate, bit_width);
        break;
      }
    } break;
    case ASM_VALUE_OFFSET_REGISTER: UNREACHABLE;
    }

    AsmValue pointer = asm_gen_pointer_instr(builder, ir_pointer);
    if (ir_pointer.t == IR_VALUE_GLOBAL) {
      AsmValue rip_relative_addr = asm_offset_reg(
          REG_CLASS_IP, 64, asm_const_symbol(ir_pointer.u.global->asm_symbol));
      pointer = rip_relative_addr;
    }

    if (type.t == IR_FLOAT) {
      switch (type.u.float_bits) {
      case IR_FLOAT_32:
        emit_instr2(builder, MOVSS, asm_deref(pointer), value);
        break;
      case IR_FLOAT_64:
        emit_instr2(builder, MOVSD, asm_deref(pointer), value);
        break;
      case IR_FLOAT_80: UNIMPLEMENTED("OP_STORE for 80-bit floats");
      }
    } else {
      emit_mov(builder, asm_deref(pointer), value);
    }

    break;
  }
  case OP_LOAD: {
    IrValue ir_pointer = instr->u.load.pointer;
    IrType type = instr->u.load.type;

    AsmValue target = new_asm_vreg_of_type(builder, type);
    assign_vreg(instr, target);

    AsmValue pointer = asm_gen_pointer_instr(builder, ir_pointer);

    if (ir_pointer.t == IR_VALUE_GLOBAL) {
      AsmValue rip_relative_addr = asm_offset_reg(
          REG_CLASS_IP, 64, asm_const_symbol(ir_pointer.u.global->asm_symbol));
      emit_mov(builder, target, asm_deref(rip_relative_addr));
    } else {
      emit_mov(builder, target, asm_deref(pointer));
    }

    break;
  }
  case OP_CAST: break;  // cast is a type-theoretic operation only
  case OP_ZEXT: {
    PRECONDITION(instr->type.t == IR_INT && instr->u.arg.type.t == IR_INT);

    if (instr->type.u.bit_width == 64 && instr->u.arg.type.u.bit_width == 32) {
      AsmValue vreg = asm_vreg(new_vreg(builder), 32);
      assign_vreg(instr, vreg);

      // Implicit zero-extending of MOV 32 -> 32
      emit_mov(builder, vreg, asm_value(builder, instr->u.arg));
    } else {
      AsmValue vreg = asm_vreg(new_vreg(builder), instr->type.u.bit_width);
      assign_vreg(instr, vreg);
      emit_instr2(builder, MOVZX, vreg, asm_value(builder, instr->u.arg));
    }

    break;
  }
  case OP_SEXT: {
    PRECONDITION(instr->type.t == IR_INT && instr->u.arg.type.t == IR_INT);

    AsmValue vreg = asm_vreg(new_vreg(builder), instr->type.u.bit_width);
    assign_vreg(instr, vreg);
    emit_instr2(builder, MOVSX, vreg, asm_value(builder, instr->u.arg));
    break;
  }
  case OP_TRUNC: {
    PRECONDITION(instr->type.t == IR_INT && instr->u.arg.type.t == IR_INT);

    AsmValue value = asm_value(builder, instr->u.arg);

    // For register we can implicitly truncate by just using the smaller
    // version of the register.
    if (value.t == ASM_VALUE_REGISTER) {
      ASSERT(value.u.reg.t == V_REG);
      instr->vreg_number = value.u.reg.u.vreg_number;
    } else {
      UNIMPLEMENTED("Truncating value not in a register (type %u)", value.t);
    }

    break;
  }
  case OP_EXTF: {
    PRECONDITION(instr->type.t == IR_FLOAT && instr->u.arg.type.t == IR_FLOAT);

    if (instr->type.u.float_bits != IR_FLOAT_64) {
      UNIMPLEMENTED("Extending to %d-bit float", instr->type.u.float_bits);
    }

    AsmValue vreg =
        asm_float_vreg(new_float_vreg(builder), instr->type.u.float_bits);
    assign_vreg(instr, vreg);
    emit_instr2(builder, CVTSS2SD, vreg, asm_value(builder, instr->u.arg));
    break;
  }
  case OP_TRUNCF: {
    PRECONDITION(instr->type.t == IR_FLOAT && instr->u.arg.type.t == IR_FLOAT);

    if (instr->type.u.float_bits != IR_FLOAT_32) {
      UNIMPLEMENTED("Truncating to %d-bit float", instr->type.u.float_bits);
    }

    AsmValue vreg =
        asm_float_vreg(new_float_vreg(builder), instr->type.u.float_bits);
    assign_vreg(instr, vreg);
    emit_instr2(builder, CVTSD2SS, vreg, asm_value(builder, instr->u.arg));
    break;
  }
  case OP_SINT_TO_FLOAT: {
    PRECONDITION(instr->type.t == IR_FLOAT && instr->u.arg.type.t == IR_INT);

    AsmValue value = asm_value(builder, instr->u.arg);

    if (instr->type.u.float_bits == IR_FLOAT_32) {
      AsmValue vreg = asm_float_vreg(new_float_vreg(builder), 32);
      assign_vreg(instr, vreg);
      emit_instr2(builder, CVTSI2SS, vreg, value);
    } else if (instr->type.u.float_bits == IR_FLOAT_64) {
      AsmValue vreg = asm_float_vreg(new_float_vreg(builder), 64);
      assign_vreg(instr, vreg);
      emit_instr2(builder, CVTSI2SD, vreg, value);
    } else {
      UNIMPLEMENTED(
          "Converting signed int to %d-bit float", instr->type.u.float_bits);
    }
    break;
  }
  case OP_UINT_TO_FLOAT: {
    PRECONDITION(instr->type.t == IR_FLOAT && instr->u.arg.type.t == IR_INT);

    AsmValue value = asm_value(builder, instr->u.arg);
    AsmValue result =
        asm_float_vreg(new_float_vreg(builder), instr->type.u.float_bits);
    assign_vreg(instr, result);

    // There's no direct conversion from unsigned integer to float, so we have
    // to do this a bit indirectly. The best approach differs depending on the
    // size of the input integer, and the size of the output float.
    if (instr->u.arg.type.u.bit_width < 64) {
      // Small integers are easy -- we just zero extend them to 64 bits and then
      // use the 64-bit conversion, since the sign bit will be guaranteed to
      // be zero.
      AsmValue extended = asm_vreg(new_vreg(builder), 64);
      AsmValue matching_width = extended;
      matching_width.u.reg.width = instr->u.arg.type.u.bit_width;
      emit_mov(builder, matching_width, value);

      if (instr->type.u.float_bits == IR_FLOAT_32) {
        emit_instr2(builder, CVTSI2SS, result, extended);
      } else if (instr->type.u.float_bits == IR_FLOAT_64) {
        emit_instr2(builder, CVTSI2SD, result, extended);
      } else {
        UNIMPLEMENTED(
            "Converting < 64-bit unsigned int to %d-bit float",
            instr->type.u.float_bits);
      }
    } else {
      // 64-bit integers are harder. We shift the high bits down, convert, and
      // then multiply. Then we convert the low bits, and add them to get the
      // final result.
      AsmOp convert_op;
      AsmOp mul_op;
      AsmOp add_op;
      if (instr->type.u.float_bits == IR_FLOAT_32) {
        convert_op = CVTSI2SS;
        mul_op = MULSS;
        add_op = ADDSS;
      } else if (instr->type.u.float_bits == IR_FLOAT_64) {
        convert_op = CVTSI2SD;
        mul_op = MULSD;
        add_op = ADDSD;
      } else {
        UNIMPLEMENTED(
            "Converting 64-bit unsigned int to %d-bit float",
            instr->type.u.float_bits);
      }
      AsmValue shifted = asm_vreg(new_vreg(builder), 64);
      emit_mov(builder, shifted, value);
      emit_instr2(builder, SHR, shifted, asm_imm(32));
      emit_instr2(builder, convert_op, result, shifted);

      AsmValue shift_multiplier =
          asm_const_float(builder, 64, (double)(1ULL << 32));
      emit_instr2(builder, mul_op, result, shift_multiplier);

      // Reused shifted, since we don't need it anymore.
      emit_mov(builder, shifted, value);
      AsmValue mask = asm_vreg(new_vreg(builder), 64);
      emit_mov(builder, mask, asm_imm(0xFFFFFFFF));
      emit_instr2(builder, AND, shifted, mask);
      AsmValue low_bits = new_asm_vreg_of_type(builder, instr->type);
      emit_instr2(builder, convert_op, low_bits, shifted);
      emit_instr2(builder, add_op, result, low_bits);
    }

    break;
  }
  case OP_FLOAT_TO_SINT: {
    PRECONDITION(instr->type.t == IR_INT && instr->u.arg.type.t == IR_FLOAT);

    AsmValue value = asm_value(builder, instr->u.arg);

    if (instr->u.arg.type.u.float_bits == IR_FLOAT_32) {
      AsmValue vreg = asm_vreg(new_vreg(builder), instr->type.u.bit_width);
      assign_vreg(instr, vreg);
      emit_instr2(builder, CVTSS2SI, vreg, value);
    } else if (instr->u.arg.type.u.float_bits == IR_FLOAT_64) {
      AsmValue vreg = asm_vreg(new_vreg(builder), instr->type.u.bit_width);
      assign_vreg(instr, vreg);
      emit_instr2(builder, CVTSD2SI, vreg, value);
    } else {
      UNIMPLEMENTED(
          "Converting %d-bit float to signed int",
          instr->u.arg.type.u.float_bits);
    }

    break;
  }
  case OP_CALL: asm_gen_call(builder, instr); break;
  case OP_BIT_XOR: asm_gen_binary_instr(builder, instr, XOR); break;
  case OP_BIT_AND: asm_gen_binary_instr(builder, instr, AND); break;
  case OP_BIT_OR: asm_gen_binary_instr(builder, instr, OR); break;
  case OP_BIT_NOT: {
    PRECONDITION(instr->type.t == IR_INT);

    AsmValue vreg = asm_vreg(new_vreg(builder), instr->type.u.bit_width);
    assign_vreg(instr, vreg);
    AsmValue arg = asm_value(builder, instr->u.arg);

    emit_mov(builder, vreg, arg);
    emit_instr1(builder, NOT, vreg);

    break;
  }
  case OP_NEG: {
    PRECONDITION(instr->type.t == IR_INT);

    AsmValue vreg = asm_vreg(new_vreg(builder), instr->type.u.bit_width);
    assign_vreg(instr, vreg);
    AsmValue arg = asm_value(builder, instr->u.arg);

    emit_mov(builder, vreg, arg);
    emit_instr1(builder, NEG, vreg);

    break;
  }
  // SHL and SHR can't take an arbitrary register as the RHS. They can take
  // an immediate, or they can take CL. So if we have args (reg1, reg2), we
  // need to move reg2 into CL first.
  case OP_SHL:
  case OP_SHR: {
    AsmOp op = instr->op == OP_SHL ? SHL : SHR;

    AsmValue shiftee = asm_value(builder, instr->u.binary_op.arg1);
    AsmValue shift_amount = asm_value(builder, instr->u.binary_op.arg2);

    AsmValue target = asm_vreg(new_vreg(builder), instr->type.u.bit_width);
    assign_vreg(instr, target);
    emit_mov(builder, target, shiftee);

    switch (shift_amount.t) {
    case ASM_VALUE_CONST: emit_instr2(builder, op, target, shift_amount); break;
    case ASM_VALUE_REGISTER: {
      AsmValue reg_cl = pre_alloced_vreg(builder, REG_CLASS_C, 8);

      Register shift_amount_low = shift_amount.u.reg;
      shift_amount_low.width = 8;

      emit_mov(
          builder, reg_cl,
          (AsmValue){
              .t = ASM_VALUE_REGISTER,
              .u.reg = shift_amount_low,
          });

      emit_instr2(builder, op, target, reg_cl);
      break;
    }
    default: UNREACHABLE;
    }

    break;
  }
  case OP_ADD: asm_gen_binary_instr(builder, instr, ADD); break;
  case OP_SUB: asm_gen_binary_instr(builder, instr, SUB); break;
  case OP_MUL: {
    PRECONDITION(instr->type.t == IR_INT);

    AsmValue vreg = asm_vreg(new_vreg(builder), instr->type.u.bit_width);
    assign_vreg(instr, vreg);

    AsmValue factor1 = asm_value(builder, instr->u.binary_op.arg1);
    AsmValue factor2 = asm_value(builder, instr->u.binary_op.arg2);

    if (factor1.t != ASM_VALUE_CONST && factor2.t != ASM_VALUE_CONST) {
      emit_mov(builder, vreg, factor1);
      emit_instr2(builder, IMUL, vreg, factor2);
    } else {
      AsmValue const_arg;
      AsmValue non_const_arg;
      if (factor1.t == ASM_VALUE_CONST) {
        const_arg = factor1;
        non_const_arg = factor2;
      } else {
        const_arg = factor2;
        non_const_arg = factor1;
      }

      emit_instr3(builder, IMUL, vreg, non_const_arg, const_arg);
    }

    break;
  }
  case OP_DIV:
  case OP_MOD: {
    PRECONDITION(instr->type.t == IR_INT);

    u8 width = instr->type.u.bit_width;
    AsmValue numerator = asm_value(builder, instr->u.binary_op.arg1);
    AsmValue denominator = asm_value(builder, instr->u.binary_op.arg2);

    AsmValue reg_denominator = denominator;
    if (denominator.t == ASM_VALUE_CONST) {
      reg_denominator = asm_vreg(new_vreg(builder), width);
      emit_mov(builder, reg_denominator, denominator);
    }

    // @TODO: Precompute liveness.
    AsmValue quotient = pre_alloced_vreg(builder, REG_CLASS_A, width);
    AsmValue remainder = pre_alloced_vreg(builder, REG_CLASS_D, width);
    emit_mov(builder, quotient, numerator);

    AsmOp sign_extend_op;
    switch (width) {
    case 32: sign_extend_op = CDQ; break;
    case 64: sign_extend_op = CQO; break;
    default: UNIMPLEMENTED("Modulo with width %u", width);
    }

    AsmInstr *sign_extend = emit_instr0(builder, sign_extend_op);
    add_dep(sign_extend, quotient);
    add_dep(sign_extend, remainder);

    AsmInstr *idiv = emit_instr1(builder, IDIV, reg_denominator);
    add_dep(idiv, quotient);
    add_dep(idiv, remainder);

    AsmValue result = instr->op == OP_DIV ? quotient : remainder;
    AsmValue vreg = asm_vreg(new_vreg(builder), width);
    emit_mov(builder, vreg, result);
    assign_vreg(instr, vreg);

    break;
  }
  case OP_ADDF: asm_gen_binary_float_instr(builder, instr, ADDSS, ADDSD); break;
  // @NOTE: Handled specially for a similar reason to OP_FIELD and OP_LOCAL
  // above. This instruction is mostly used as the argument to OP_COND, so we
  // let OP_COND match them itself in those cases.
  case OP_CMP:
  case OP_CMPF: break;
  case OP_BUILTIN_VA_START: {
    AsmValue va_list_ptr = asm_vreg(new_vreg(builder), 64);
    emit_mov(builder, va_list_ptr, asm_value(builder, instr->u.arg));

    u32 int_arg_registers_already_used = 0;
    u32 float_arg_registers_already_used = 0;
    for (u32 i = 0; i < ir_global->type.u.function.arity; i++) {
      ArgClass *arg_class = ir_global->call_seq.arg_classes + i;
      if (arg_class->t == ARG_CLASS_REG) {
        if (reg_class_is_gpr(arg_class->u.reg.reg)) {
          int_arg_registers_already_used++;
        } else {
          float_arg_registers_already_used++;
        }
      }
    }

    // Initialise as specified in System V x86-64 section 3.5.7
    // The initial int register offset will point past the registers already
    // used to pass normal int arguments to the function.
    AsmValue initial_int_register_offset =
        asm_fixed_imm(int_arg_registers_already_used * 8, 32);
    emit_mov(builder, asm_deref(va_list_ptr), initial_int_register_offset);

    // @TODO: Support vregs with asm_offset_reg so we don't have to bump
    // va_list_ptr and can just use const offsets for each field.
    emit_instr2(builder, ADD, va_list_ptr, asm_imm(4));

    // Similarly, the initial float register offset will point past the
    // registers already used to pass normal float arguments to the function.
    AsmValue initial_float_register_offset = asm_fixed_imm(
        STATIC_ARRAY_LENGTH(int_argument_registers) * 8
            + float_arg_registers_already_used * 16,
        32);
    emit_mov(builder, asm_deref(va_list_ptr), initial_float_register_offset);
    emit_instr2(builder, ADD, va_list_ptr, asm_imm(4));

    // Stack args start at the bottom of the previous stack frame, which is
    // always rbp + 16
    AsmValue temp_vreg = asm_vreg(new_vreg(builder), 64);
    emit_mov(builder, temp_vreg, asm_phys_reg(REG_CLASS_BP, 64));
    emit_instr2(builder, ADD, temp_vreg, asm_imm(16));
    emit_mov(builder, asm_deref(va_list_ptr), temp_vreg);
    emit_instr2(builder, ADD, va_list_ptr, asm_imm(8));

    // The register save area is always at the bottom of our stack frame.
    emit_mov(builder, asm_deref(va_list_ptr), asm_phys_reg(REG_CLASS_SP, 64));

    break;
  }
  }
}

static void asm_gen_call(AsmBuilder *builder, IrInstr *instr)
{
  u32 call_arity = instr->u.call.arity;

  CallSeq call_seq;

  bool gen_new_call_seq;
  if (instr->u.call.callee.t == IR_VALUE_GLOBAL) {
    IrGlobal *target = instr->u.call.callee.u.global;
    IrType callee_type = instr->u.call.callee.u.global->type;
    ASSERT(target->type.t == IR_FUNCTION);
    ASSERT(callee_type.t == IR_FUNCTION);

    u32 callee_arity = callee_type.u.function.arity;
    ASSERT(
        call_arity == callee_arity
        || (call_arity > callee_arity
            && callee_type.u.function.variable_arity));

    if (!callee_type.u.function.variable_arity) {
      gen_new_call_seq = false;
      call_seq = target->call_seq;
    } else {
      // We always need to create a new CallSeq for varargs
      // functions, because they can be called differently at every
      // callsite.
      gen_new_call_seq = true;
    }
  } else {
    // We need to create a new CallSeq because with an indirect call we
    // have no global to grab the pre-generated CallSeq from.
    gen_new_call_seq = true;
  }

  if (gen_new_call_seq) {
    IrType *arg_types = malloc(call_arity * sizeof *arg_types);
    for (u32 i = 0; i < call_arity; i++)
      arg_types[i] = instr->u.call.arg_array[i].type;

    call_seq = classify_arguments(call_arity, arg_types);

    free(arg_types);
  }

  i32 args_stack_space = call_seq.stack_space;

  // As specified by the System V x86-64 ABI, we have to ensure that the
  // end of our stack frame is aligned to 16 bytes. The end of the
  // previous stack frame was aligned to 16 bytes, so this amounts to
  // making sure the size of our stack frame is a multiple of 16. The
  // prologue ensures that we're correctly aligned before the call, so we
  // just need to make sure that the space we use for passing arguments
  // is correctly aligned.
  //
  // See the System V x86-64 ABI spec, section 3.2.2
  u32 args_plus_padding = align_to(args_stack_space, 16);
  if (args_plus_padding > 0) {
    emit_instr2(
        builder, SUB, asm_phys_reg(REG_CLASS_SP, 64),
        asm_imm(args_plus_padding));
  }

  builder->curr_sp_diff = args_plus_padding;

  Array(u32) arg_vregs;
  ARRAY_INIT(&arg_vregs, u32, STATIC_ARRAY_LENGTH(int_argument_registers));

  for (u32 i = 0; i < call_arity; i++) {
    AsmValue arg = asm_value(builder, instr->u.call.arg_array[i]);
    // Use the largest version of the register, as all argument registers do.
    u8 value_width = size_of_ir_type(instr->u.call.arg_array[i].type) * 8;
    if (arg.t == ASM_VALUE_REGISTER) {
      arg.u.reg.width =
          instr->u.call.arg_array[i].type.t == IR_FLOAT ? 128 : 64;
      if (instr->u.call.arg_array[i].type.t != IR_FLOAT) value_width = 64;
    }

    ArgClass *arg_class = call_seq.arg_classes + i;

    switch (arg_class->t) {
    case ARG_CLASS_REG: {
      AsmValue arg_target_reg =
          pre_alloced_vreg(builder, arg_class->u.reg.reg, value_width);
      emit_mov(builder, arg_target_reg, arg);
      *ARRAY_APPEND(&arg_vregs, u32) = arg_target_reg.u.reg.u.vreg_number;
      VReg *vreg = ARRAY_REF(
          &builder->virtual_registers, VReg, *ARRAY_LAST(&arg_vregs, u32));
      vreg->live_range_start = builder->current_block->size - 1;
      break;
    }
    case ARG_CLASS_MEM: {
      AsmConst location = asm_const_imm(arg_class->u.mem.offset);
      // @TODO: "remains_in_memory" is kind of a proxy for what we
      // care about here, which is whether the original is in memory.
      // This is the case for structs, which are always pointers in
      // the IR. Maybe we should have a field which sets this
      // explicitly? We should probably just clean up the frontend
      // codegen for struct calls instead.
      if (!arg_class->u.mem.remains_in_memory) {
        // Stack args that don't remain in memory should always be
        // rounded up to 8 bytes.
        ASSERT(arg_class->u.mem.size == 8);

        // Need a temp vreg to do an 8-byte store into memory.
        AsmValue temp_vreg = asm_vreg(new_vreg(builder), 64);

        emit_mov(builder, temp_vreg, arg);
        emit_mov(
            builder, asm_deref(asm_offset_reg(REG_CLASS_SP, 64, location)),
            temp_vreg);
      } else {
        // @TODO: This is essentially a bad open-coding of memcpy.
        // Adding a call to memcpy would be really awkward since
        // we're in the middle of a call sequence. We should either
        // make this smarter or somehow make it go through the
        // regular memcpy code in IR, which we can optimise along
        // with other uses of memcpy.
        Register src;
        switch (arg.t) {
        case ASM_VALUE_REGISTER: src = arg.u.reg; break;
        case ASM_VALUE_OFFSET_REGISTER:
          if (arg.u.offset_register.offset.t == ASM_CONST_IMMEDIATE) {
            src = arg.u.offset_register.reg;
            break;
          }
          // fallthrough
        default: {
          AsmValue src_vreg = asm_vreg(new_vreg(builder), 64);
          emit_mov(builder, src_vreg, arg);

          src = src_vreg.u.reg;
          break;
        }
        }

        u32 temp_vreg = new_vreg(builder);
        u32 to_copy = arg_class->u.mem.size;
        u32 i = 0;
        for (; to_copy - i >= 8; i += 8) {
          AsmConst offset = asm_const_imm(location.u.immediate + i);
          emit_mov(
              builder, asm_vreg(temp_vreg, 64),
              asm_deref((AsmValue){
                  .t = ASM_VALUE_OFFSET_REGISTER,
                  .u.offset_register.reg = src,
                  .u.offset_register.offset = asm_const_imm(i),
              }));
          emit_mov(
              builder, asm_deref(asm_offset_reg(REG_CLASS_SP, 64, offset)),
              asm_vreg(temp_vreg, 64));
        }
        for (; i < to_copy; i++) {
          AsmConst offset = asm_const_imm(location.u.immediate + i);
          emit_mov(
              builder, asm_vreg(temp_vreg, 64),
              asm_deref((AsmValue){
                  .t = ASM_VALUE_OFFSET_REGISTER,
                  .u.offset_register.reg = src,
                  .u.offset_register.offset = asm_const_imm(i),
              }));
          emit_mov(
              builder, asm_deref(asm_offset_reg(REG_CLASS_SP, 64, offset)),
              asm_vreg(temp_vreg, 8));
        }
      }
      break;
    }
    }
  }

  // In the case that gen_new_call_seq is true, either this is a call to
  // a varargs function, or it's an indirect call and we conservatively
  // assume it could be a varargs function.
  //
  // The only difference on the caller side between varargs and regular
  // functions is that al must be set to (at least) the number of vector
  // registers used to pass arguments. We never use vector registers to
  // pass arguments because that isn't implemented yet, so we just set it
  // to zero.
  if (gen_new_call_seq) {
    u32 num_reg_float_args = 0;
    for (u32 i = 0; i < call_arity; i++) {
      IrValue arg = instr->u.call.arg_array[i];
      ArgClass *arg_class = call_seq.arg_classes + i;
      if (arg.type.t == IR_FLOAT && arg_class->t == ARG_CLASS_REG) {
        num_reg_float_args++;
      }
    }
    emit_mov(
        builder, pre_alloced_vreg(builder, REG_CLASS_A, 8),
        asm_imm(num_reg_float_args));

    // Done with call_seq now
    free(call_seq.arg_classes);
  }

  emit_instr1(builder, CALL, asm_value(builder, instr->u.call.callee));

  // The live range of arg vregs ends at the call instruction. Liveness
  // analysis won't be performed for these vregs, since we've set the
  // start and end already.
  for (u32 i = 0; i < arg_vregs.size; i++) {
    u32 arg_vreg = *ARRAY_REF(&arg_vregs, u32, i);
    VReg *vreg = ARRAY_REF(&builder->virtual_registers, VReg, arg_vreg);
    vreg->live_range_end = builder->current_block->size - 1;
  }

  array_free(&arg_vregs);

  if (args_plus_padding > 0) {
    emit_instr2(
        builder, ADD, asm_phys_reg(REG_CLASS_SP, 64),
        asm_imm(args_plus_padding));
  }
  builder->curr_sp_diff = 0;

  if (instr->u.call.return_type.t != IR_VOID) {
    // Move out of RAX into a new vreg. The live range of the result
    // might overlap another case where we need to use RAX.
    AsmValue return_vreg =
        return_reg_for_type(builder, instr->u.call.return_type);
    AsmValue temp_vreg =
        new_asm_vreg_of_type(builder, instr->u.call.return_type);
    emit_mov(builder, temp_vreg, return_vreg);

    VReg *vreg = ARRAY_REF(
        &builder->virtual_registers, VReg, return_vreg.u.reg.u.vreg_number);
    // Starts at the call, as that defines it. Ends at the mov into temp_vreg
    vreg->live_range_start = builder->current_block->size - 2;
    vreg->live_range_end = builder->current_block->size - 1;

    assign_vreg(instr, temp_vreg);
  }
}

// We deliberately exclude RBP from this list, as we don't support omitting
// the frame pointer and so we never use it for register allocation.
static RegClass callee_save_registers[] = {
    REG_CLASS_R15, REG_CLASS_R14, REG_CLASS_R13, REG_CLASS_R12, REG_CLASS_B,
};

static bool is_callee_save(RegClass reg)
{
  for (u32 i = 0; i < STATIC_ARRAY_LENGTH(callee_save_registers); i++) {
    if (callee_save_registers[i] == reg) {
      return true;
    }
  }

  return false;
}

void asm_gen_function(AsmBuilder *builder, IrGlobal *ir_global)
{
  AsmSymbol *global_symbol = ir_global->asm_symbol;
  PRECONDITION(global_symbol != NULL);
  PRECONDITION(ir_global->type.t == IR_FUNCTION);

  IrFunction *ir_func = &ir_global->initializer->u.function;

  global_symbol->defined = ir_global->initializer != NULL;
  if (!global_symbol->defined) return;

  Array(AsmInstr) body;
  ARRAY_INIT(&body, AsmInstr, 20);

  builder->current_function = ir_global;
  builder->current_block = &body;

  builder->local_stack_usage = 0;

  AsmSymbol *ret_label = add_label(builder, LS("ret"));
  builder->ret_label = ret_label;

  if (ARRAY_IS_VALID(&builder->virtual_registers))
    array_free(&builder->virtual_registers);
  ARRAY_INIT(&builder->virtual_registers, VReg, 20);
  IrType return_type = *ir_global->type.u.function.return_type;
  ASSERT(
      return_type.t == IR_INT || return_type.t == IR_FLOAT
      || return_type.t == IR_POINTER || return_type.t == IR_VOID);

  for (u32 block_index = 0; block_index < ir_func->blocks.size; block_index++) {
    IrBlock *block = *ARRAY_REF(&ir_func->blocks, IrBlock *, block_index);
    block->label = add_label(builder, block->name);
  }

  // Pre-allocate virtual registers for argument registers. This is to avoid
  // spills if this isn't a leaf function, since we'll need to use the same
  // registers for our own calls.
  for (u32 i = 0; i < ir_global->type.u.function.arity; i++) {
    ArgClass *arg_class = ir_global->call_seq.arg_classes + i;
    if (arg_class->t == ARG_CLASS_REG) {
      AsmValue arg_vreg = new_asm_vreg_of_type(
          builder, ir_global->type.u.function.arg_types[i]);
      emit_mov(
          builder, arg_vreg,
          asm_phys_reg(arg_class->u.reg.reg, arg_vreg.u.reg.width));
      arg_class->u.reg.vreg = arg_vreg.u.reg.u.vreg_number;
    }
  }

  // If this is a varargs function, add space for the register save area.
  // Since we add this before compiling the body of the function, the
  // register save area is always at the bottom of the stack frame, as long
  // as we're not in the process of calling a function. We don't care about
  // that case though, because we only need the location for va_start.
  if (ir_global->type.u.function.variable_arity) {
    builder->local_stack_usage += REGISTER_SAVE_AREA_SIZE;
  }

  for (u32 block_index = 0; block_index < ir_func->blocks.size; block_index++) {
    IrBlock *block = *ARRAY_REF(&ir_func->blocks, IrBlock *, block_index);

    u32 first_instr_of_block_index =
        block_index == 0 ? 0 : builder->current_block->size;
    block->label->offset = first_instr_of_block_index;

    Array(IrInstr *) *instrs = &block->instrs;
    for (u32 i = 0; i < instrs->size; i++) {
      IrInstr *instr = *ARRAY_REF(instrs, IrInstr *, i);
      asm_gen_instr(builder, ir_global, block, instr);
    }

    AsmInstr *first_instr_of_block =
        ARRAY_REF(builder->current_block, AsmInstr, first_instr_of_block_index);
    first_instr_of_block->label = block->label;
  }

  if (flag_print_pre_regalloc_stats) {
    u32 int_count = 0;
    u32 float_count = 0;
    for (u32 i = 0; i < builder->virtual_registers.size; i++) {
      VReg *vreg = ARRAY_REF(&builder->virtual_registers, VReg, i);
      if (vreg->type == REG_TYPE_INTEGER) int_count++;
      if (vreg->type == REG_TYPE_FLOAT) float_count++;
    }

    printf(
        "%.*s: %u instrs, %u vregs (%u int, %u float)\n", ir_global->name.len,
        ir_global->name.chars, body.size, builder->virtual_registers.size,
        int_count, float_count);
  }

  allocate_registers(builder);

  u32 used_callee_save_regs_bitset = 0;
  for (u32 i = 0; i < builder->current_block->size; i++) {
    AsmInstr *instr = ARRAY_REF(builder->current_block, AsmInstr, i);
    for (u32 j = 0; j < instr->arity; j++) {
      Register *reg = value_reg(instr->args + j);
      if (reg != NULL && reg->t == PHYS_REG && is_callee_save(reg->u.class)) {
        used_callee_save_regs_bitset |= 1 << reg->u.class;
      }
    }
  }

  u32 num_callee_save_regs = bit_count(used_callee_save_regs_bitset);

  builder->current_block = &builder->asm_module.text.instrs;

  AsmSymbol *entry_label = ir_global->asm_symbol;

  AsmInstr *prologue_first_instr =
      emit_instr1(builder, PUSH, asm_phys_reg(REG_CLASS_BP, 64));
  prologue_first_instr->label = entry_label;
  emit_mov(
      builder, asm_phys_reg(REG_CLASS_BP, 64), asm_phys_reg(REG_CLASS_SP, 64));
  u32 temp_regs_bitset = used_callee_save_regs_bitset;
  while (temp_regs_bitset != 0) {
    RegClass reg = lowest_set_bit(temp_regs_bitset);
    emit_instr1(builder, PUSH, asm_phys_reg(reg, 64));
    temp_regs_bitset &= ~(1 << reg);
  }

  u32 stack_adjustment = builder->local_stack_usage;

  // Ensure that the stack frame size is a multiple of 16 before entering the
  // body of the function. Then each "call" op just needs to ensure that its
  // stack arg area is correctly aligned. The return address and the previous
  // value of rbp are pushed, for a total of 16 bytes which doesn't affect
  // alignment. Then we push callee-save registers and subtract space for
  // locals. This may affect alignment, so we add padding if necessary.
  //
  // See the System V x86-64 ABI spec, section 3.2.2
  // @TODO: We can elide this for leaf functions.
  stack_adjustment +=
      16 - ((num_callee_save_regs * 8 + builder->local_stack_usage) % 16);

  if (stack_adjustment != 0) {
    emit_instr2(
        builder, SUB, asm_phys_reg(REG_CLASS_SP, 64),
        asm_imm(stack_adjustment));
  }

  // Argument registers are stored in increasing order in the register save
  // area. While the layout of the register save area is always the same (See
  // the System V x86-64 ABI spec, figure 3.33), we don't always fill it. Only
  // registers that could have been used to pass varargs arguments need to be
  // stored.
  //
  // For integer registers, those that were used to pass normal
  // arguments (before the "..." in the argument list) can be elided. e.g.: for
  // "void foo(int a, int b, ...)" we don't save rdi or rsi.
  //
  // For float registers the same thing applies, but also al contains the number
  // of vector registers used to pass varargs arguments. We just check if it's
  // zero, and if so skip storing float registers at all.
  if (ir_global->type.u.function.variable_arity) {
    u32 num_int_reg_args = 0;
    u32 num_float_reg_args = 0;
    for (u32 i = 0; i < ir_global->type.u.function.arity; i++) {
      ArgClass *arg_class = ir_global->call_seq.arg_classes + i;

      if (arg_class->t == ARG_CLASS_REG) {
        if (reg_class_is_gpr(arg_class->u.reg.reg)) {
          num_int_reg_args++;
        } else {
          num_float_reg_args++;
        }
      }
    }

    for (u32 i = num_int_reg_args;
         i < STATIC_ARRAY_LENGTH(int_argument_registers); i++) {
      AsmConst offset = asm_const_imm(i * 8);
      emit_mov(
          builder, asm_deref(asm_offset_reg(REG_CLASS_SP, 64, offset)),
          asm_phys_reg(int_argument_registers[i], 64));
    }

    // Only save vector registers if any were used. al contains the number of
    // vector registers used to pass arguments at the callsite. We just check if
    // it's zero, and if not, we save them. We don't bother checking the exact
    // number to skip saving some. We do skip those that were already used to
    // pass normal arguments, as above with int argument registers.
    AsmSymbol *skip_vector_save = add_label(builder, LS("skip_vector_save"));
    emit_instr2(builder, CMP, asm_phys_reg(REG_CLASS_A, 8), asm_imm(0));
    emit_instr1(builder, JE, asm_symbol(skip_vector_save));
    for (u32 i = num_float_reg_args;
         i < STATIC_ARRAY_LENGTH(float_argument_registers); i++) {
      AsmConst offset = asm_const_imm(
          STATIC_ARRAY_LENGTH(int_argument_registers) * 8 + i * 16);
      emit_instr2(
          builder, MOVAPS, asm_deref(asm_offset_reg(REG_CLASS_SP, 64, offset)),
          asm_phys_reg(float_argument_registers[i], 64));
    }

    // @TODO: Would be better to attach this to whatever the next instruction
    // happens to be.
    emit_instr0(builder, NOP)->label = skip_vector_save;
  }

  for (u32 j = 0; j < body.size; j++) {
    *ARRAY_APPEND(builder->current_block, AsmInstr) =
        *ARRAY_REF(&body, AsmInstr, j);
  }

  u32 epilogue_start = builder->current_block->size;

  if (stack_adjustment != 0) {
    emit_instr2(
        builder, ADD, asm_phys_reg(REG_CLASS_SP, 64),
        asm_imm(stack_adjustment));
  }
  temp_regs_bitset = used_callee_save_regs_bitset;
  while (temp_regs_bitset != 0) {
    // We use highest_set_bit so that we iterate in reverse order to the
    // PUSHes we emit above.
    RegClass reg = highest_set_bit(temp_regs_bitset);
    emit_instr1(builder, POP, asm_phys_reg(reg, 64));
    temp_regs_bitset &= ~(1 << reg);
  }
  emit_instr1(builder, POP, asm_phys_reg(REG_CLASS_BP, 64));
  emit_instr0(builder, RET);

  AsmInstr *epilogue_first_instr =
      ARRAY_REF(builder->current_block, AsmInstr, epilogue_start);
  epilogue_first_instr->label = ret_label;

  array_free(&body);
}

static void write_int_bytes(Array(u8) *out, u64 bytes, u32 size)
{
  for (u32 n = 0; n < size; n++) {
    u8 byte = (bytes >> (n * 8)) & 0xFF;
    *ARRAY_APPEND(out, u8) = byte;
  }
}

static void write_const(AsmModule *asm_module, IrConst *konst, Array(u8) *out)
{
  switch (konst->type.t) {
  case IR_INT:
    write_int_bytes(out, konst->u.integer, size_of_ir_type(konst->type));
    break;
  // @FLOAT_IMPL
  case IR_FLOAT:
    if (konst->type.u.bit_width == 32) {
      u32 raw_bits = float_to_raw_bits((float)konst->u.floatt);
      write_int_bytes(out, raw_bits, 4);
    } else if (konst->type.u.bit_width == 64) {
      u64 raw_bits = double_to_raw_bits(konst->u.floatt);
      write_int_bytes(out, raw_bits, 8);
    } else {
      // 80-bit long double
      UNIMPLEMENTED("long double float constant");
    }
    break;
  case IR_POINTER: {
    IrGlobal *global = konst->u.global_pointer;
    if (global != NULL) {
      Fixup *fixup = pool_alloc(&asm_module->pool, sizeof *fixup);
      *ARRAY_APPEND(&asm_module->fixups, Fixup *) = fixup;
      *fixup = (Fixup){
          .type = FIXUP_ABSOLUTE,
          .section = DATA_SECTION,
          .offset = out->size,
          // @PORT: Hardcoded pointer size.
          .size_bytes = 8,
          .symbol = konst->u.global_pointer->asm_symbol,
      };
    }

    // @PORT: Hardcoded pointer size.
    for (u32 i = 0; i < 8; i++) {
      *ARRAY_APPEND(out, u8) = 0;
    }

    break;
  }
  case IR_ARRAY:
    for (u32 i = 0; i < konst->type.u.array.size; i++)
      write_const(asm_module, konst->u.array_elems + i, out);
    break;
  case IR_STRUCT: {
    u32 prev_offset = 0;
    for (u32 i = 0; i < konst->type.u.strukt.num_fields; i++) {
      IrStructField *field = konst->type.u.strukt.fields + i;
      u32 field_offset = field->offset;
      u32 field_size = size_of_ir_type(field->type);

      for (u32 n = prev_offset; n < field_offset; n++) {
        *ARRAY_APPEND(out, u8) = 0;
      }

      write_const(asm_module, konst->u.struct_fields + i, out);

      prev_offset = field_offset + field_size;
    }
    break;
  }
  case IR_FUNCTION:
  case IR_VOID: UNREACHABLE;
  }
}

static bool is_zero(IrConst *konst)
{
  switch (konst->type.t) {
  case IR_INT: return konst->u.integer == 0;
  case IR_FLOAT: return konst->u.floatt == 0.0;
  case IR_POINTER: return false;
  case IR_ARRAY:
    for (u32 n = 0; n < konst->type.u.array.size; n++) {
      if (!is_zero(konst->u.array_elems + n)) {
        return false;
      }
    }
    return true;
  case IR_STRUCT:
    for (u32 n = 0; n < konst->type.u.strukt.num_fields; n++) {
      if (!is_zero(konst->u.struct_fields + n)) {
        return false;
      }
    }
    return true;
  case IR_FUNCTION:
  case IR_VOID: UNREACHABLE;
  }

  UNREACHABLE;
}

void generate_asm_module(AsmBuilder *builder, IrModule *module)
{
  AsmModule *asm_module = &builder->asm_module;

  // First do a pass collecting declarations, to support forward references.
  for (u32 i = 0; i < module->globals.size; i++) {
    IrGlobal *ir_global = *ARRAY_REF(&module->globals, IrGlobal *, i);

    AsmSymbolSection section;
    if (ir_global->initializer == NULL) {
      section = UNKNOWN_SECTION;
    } else if (ir_global->type.t == IR_FUNCTION) {
      section = TEXT_SECTION;
    } else if (is_zero(ir_global->initializer)) {
      section = BSS_SECTION;
    } else {
      section = DATA_SECTION;
    }

    AsmSymbol *asm_symbol = add_asm_symbol(builder);
    ir_global->asm_symbol = asm_symbol;
    asm_symbol->ir_global = ir_global;

    u32 name_len = ir_global->name.len;
    char *name_copy = pool_alloc(&builder->asm_module.pool, name_len);
    memcpy(name_copy, ir_global->name.chars, name_len);

    asm_symbol->name = (String){name_copy, name_len};
    asm_symbol->defined = ir_global->initializer != NULL;
    asm_symbol->section = section;
    asm_symbol->offset = 0;
    asm_symbol->pred = NULL;

    switch (ir_global->linkage) {
    case IR_GLOBAL_LINKAGE: asm_symbol->linkage = ASM_GLOBAL_LINKAGE; break;
    case IR_LOCAL_LINKAGE: asm_symbol->linkage = ASM_LOCAL_LINKAGE; break;
    }

    if (ir_global->type.t == IR_FUNCTION) {
      u32 arity = ir_global->type.u.function.arity;
      IrType *arg_types = ir_global->type.u.function.arg_types;

      // Note that we do this even for varargs functions where the
      // CallSeq needs to be computed per-call. This is so that when
      // compiling the function itself we have info on the args before
      // the ..., for e.g. register allocation.
      ir_global->call_seq = classify_arguments(arity, arg_types);
    }
  }

  for (u32 i = 0; i < asm_module->symbols.size; i++) {
    AsmSymbol *asm_symbol = *ARRAY_REF(&asm_module->symbols, AsmSymbol *, i);
    if (!asm_symbol->defined) continue;

    IrGlobal *ir_global = asm_symbol->ir_global;
    if (ir_global == NULL) continue;

    IrConst *konst = ir_global->initializer;
    if (ir_global->type.t == IR_FUNCTION) {
      asm_gen_function(builder, ir_global);
    } else if (asm_symbol->section == DATA_SECTION) {
      Array(u8) *data = &asm_module->data;
      // @TODO: Alignment
      asm_symbol->offset = data->size;
      write_const(asm_module, konst, data);
      asm_symbol->size = data->size - asm_symbol->offset;
    } else {
      ASSERT(asm_symbol->section == BSS_SECTION);
      u32 size = size_of_ir_type(ir_global->type);
      // @TODO: Alignment
      asm_symbol->offset = asm_module->bss_size;
      asm_symbol->size = size;

      asm_module->bss_size += size;
    }
  }
}
