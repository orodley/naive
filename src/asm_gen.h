#ifndef NAIVE_ASM_GEN_H_
#define NAIVE_ASM_GEN_H_

#include "asm.h"
#include "ir.h"

typedef struct StackSlot
{
	u32 ir_instr_id;
	u32 stack_offset;
} StackSlot;

typedef struct VRegInfo
{
	PhysicalRegister assigned_register;
	i32 live_range_start;
	i32 live_range_end;
	enum
	{
		INSTR,
		ARG,
	} source;

	union
	{
		u32 arg_index;
		IrInstr *defining_instr;
	} val;
} VRegInfo;

typedef struct AsmBuilder
{
	AsmModule asm_module;
	AsmFunction *current_function;

	Array(StackSlot) stack_slots;
	Array(VRegInfo) virtual_registers;
	u32 local_stack_usage;
} AsmBuilder;

void init_asm_builder(AsmBuilder *builder);
void free_asm_builder(AsmBuilder *builder);
void generate_asm_module(AsmBuilder *builder, TransUnit *trans_unit);

#endif
