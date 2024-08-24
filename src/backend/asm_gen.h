#ifndef NAIVE_BACKEND_ASM_GEN_H_
#define NAIVE_BACKEND_ASM_GEN_H_

#include "backend/asm.h"
#include "ir.h"
#include "util.h"

typedef struct VReg
{
  enum
  {
    UNASSIGNED,
    IN_REG,
    ON_STACK,
  } t;

  bool pre_alloced;
  RegType type;
  i32 live_range_start;
  i32 live_range_end;

  union
  {
    RegClass assigned_register;
    u32 assigned_stack_slot;
  } u;
} VReg;

typedef struct AsmBuilder
{
  AsmModule asm_module;
  IrGlobal *current_function;
  Array(AsmInstr) *current_block;
  AsmSymbol *ret_label;

  Array(VReg) virtual_registers;
  u32 local_stack_usage;
  u32 curr_sp_diff;
  u32 global_temp_floats;
} AsmBuilder;

void init_asm_builder(AsmBuilder *builder, String input_file_name);
void free_asm_builder(AsmBuilder *builder);
void generate_asm_module(AsmBuilder *builder, IrModule *module);

#endif
