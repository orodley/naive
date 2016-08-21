#include <assert.h>

#include "ir.h"
#include "asm.h"
#include "asm_gen.h"

void init_asm_builder(AsmBuilder *builder)
{
	init_asm_module(&builder->asm_module);

	builder->local_stack_usage = 0;
	builder->stack_slots = ARRAY_ZEROED;
	builder->virtual_registers = ARRAY_ZEROED;
}

void free_asm_builder(AsmBuilder *builder)
{
	free_asm_module(&builder->asm_module);

	if (ARRAY_IS_VALID(&builder->stack_slots))
		array_free(&builder->stack_slots);
	if (ARRAY_IS_VALID(&builder->virtual_registers))
		array_free(&builder->virtual_registers);
}

static void append_function(AsmBuilder *builder, char *name)
{
	AsmFunction *new_function = ARRAY_APPEND(&builder->asm_module.functions, AsmFunction);
	init_asm_function(new_function, name);
	builder->current_function = new_function;

	builder->local_stack_usage = 0;
	if (ARRAY_IS_VALID(&builder->stack_slots))
		array_free(&builder->stack_slots);
	ARRAY_INIT(&builder->stack_slots, StackSlot, 5);
}

AsmInstr *emit_instr0(AsmBuilder *builder, AsmOp op)
{
	AsmInstr *instr = ARRAY_APPEND(&builder->current_function->instrs, AsmInstr);
	instr->op = op;
	instr->num_args = 0;
	instr->label = NULL;

	return instr;
}

AsmInstr *emit_instr1(AsmBuilder *builder, AsmOp op, AsmArg arg1)
{
	AsmInstr *instr = ARRAY_APPEND(&builder->current_function->instrs, AsmInstr);
	instr->op = op;
	instr->num_args = 1;
	instr->args[0] = arg1;
	instr->label = NULL;

	return instr;
}

AsmInstr *emit_instr2(AsmBuilder *builder, AsmOp op, AsmArg arg1, AsmArg arg2)
{
	AsmInstr *instr = ARRAY_APPEND(&builder->current_function->instrs, AsmInstr);
	instr->op = op;
	instr->num_args = 2;
	instr->args[0] = arg1;
	instr->args[1] = arg2;
	instr->label = NULL;

	return instr;
}

static u32 size_of_ir_type(IrType type)
{
	switch (type.kind) {
	case IR_INT:
		return type.val.bit_width;
	case IR_POINTER: case IR_FUNCTION:
		return 8;
	}
}

static StackSlot *stack_slot_for_id(AsmBuilder *builder, u32 id)
{
	for (u32 i = 0; i < builder->stack_slots.size; i++) {
		StackSlot *stack_slot = ARRAY_REF(&builder->stack_slots, StackSlot, i);
		if (stack_slot->ir_instr_id == id)
			return stack_slot;
	}

	return NULL;
}

// @TODO: Rethink name? "next" kinda suggests side effects, i.e. "move to the
// next vreg number".
static inline u32 next_vreg(AsmBuilder *builder)
{
	return builder->virtual_registers.size;
}

static PhysicalRegister argument_registers[] = {
	RDI, RSI, RDX, RCX, R8, R9,
};

static AsmArg asm_value(IrValue value)
{
	switch (value.kind) {
	case VALUE_CONST:
		assert((value.val.constant & 0xFFFFFFFF) == value.val.constant);
		return asm_const32(value.val.constant & 0xFFFFFFFF);
	case VALUE_INSTR: {
		i32 vreg = value.val.instr->virtual_register;
		assert(vreg != -1);
		return asm_virtual_register(vreg);
	}
	case VALUE_ARG: {
		assert(value.type.kind == IR_INT);
		assert(value.val.arg_index < STATIC_ARRAY_LENGTH(argument_registers));

		// We always allocate virtual registers to arguments first, so arg at
		// index i = virtual register i.
		return asm_virtual_register(value.val.arg_index);
	}
	case VALUE_GLOBAL:
		return asm_label(global_label(value.val.global));
	}
}

static void assign_vreg(AsmBuilder *builder, IrInstr *instr)
{
	u32 vreg_number = next_vreg(builder);
	VRegInfo *vreg_info = ARRAY_APPEND(&builder->virtual_registers, VRegInfo);

	vreg_info->assigned_register = INVALID_REGISTER;
	vreg_info->source = INSTR;
	vreg_info->val.defining_instr = instr;

	instr->virtual_register = vreg_number;
}

static void asm_gen_instr(
		IrFunction *ir_func, AsmBuilder *builder, IrInstr *instr)
{
	switch (instr->op) {
	case OP_LOCAL: {
		StackSlot *slot = ARRAY_APPEND(&builder->stack_slots, StackSlot);
		slot->ir_instr_id = instr->id;

		// @TODO: Alignment of stack slots. This could probably use similar
		// logic to that of struct layout.
		slot->stack_offset = builder->local_stack_usage;
		builder->local_stack_usage += size_of_ir_type(instr->type);

		break;
	}
	case OP_RET: {
		IrValue arg = instr->val.arg;
		assert(ir_type_eq(ir_func->return_type, arg.type));

		emit_instr2(builder, ADD, asm_physical_register(RSP),
				asm_const32(builder->local_stack_usage));
		emit_instr2(builder, MOV, asm_physical_register(RAX), asm_value(arg));
		emit_instr1(builder, POP, asm_physical_register(RBP));
		emit_instr0(builder, RET);

		break;
	}
	case OP_BRANCH:
		emit_instr1(builder, JMP, asm_label(instr->val.target_block->label));
		break;
	case OP_COND:
		emit_instr2(builder, CMP, asm_value(instr->val.cond.condition), asm_const32(0));
		emit_instr1(builder, JE, asm_label(instr->val.cond.then_block->label));
		emit_instr1(builder, JMP, asm_label(instr->val.cond.else_block->label));
		break;
	case OP_STORE: {
		IrValue pointer = instr->val.store.pointer;
		IrValue value = instr->val.store.value;
		IrType type = instr->val.store.type;

		assert(ir_type_eq(value.type, type));
		assert(type.kind == IR_INT);
		assert(type.val.bit_width == 32);

		assert(pointer.kind == VALUE_INSTR);
		IrInstr *pointer_instr = pointer.val.instr;
		assert(pointer_instr->op == OP_LOCAL);

		StackSlot *slot = stack_slot_for_id(builder, pointer_instr->id);
		assert(slot != NULL);

		emit_instr2(builder, MOV,
				asm_deref(asm_offset_register(RSP, slot->stack_offset)),
				asm_value(value));
		break;
	}
	case OP_LOAD: {
		IrValue pointer = instr->val.load.pointer;
		IrType type = instr->val.load.type;

		assert(type.kind == IR_INT);
		assert(type.val.bit_width == 32);

		assert(pointer.kind == VALUE_INSTR);
		IrInstr *pointer_instr = pointer.val.instr;
		assert(pointer_instr->op == OP_LOCAL);

		StackSlot *slot = stack_slot_for_id(builder, pointer_instr->id);
		assert(slot != NULL);

		emit_instr2(builder, MOV,
				asm_virtual_register(next_vreg(builder)),
				asm_deref(asm_offset_register(RSP, slot->stack_offset)));
		assign_vreg(builder, instr);
		break;
	}
	case OP_CALL: {
		// @TODO: Save caller save registers. We could just push and pop
		// everything and rely on some later pass to eliminate saves of
		// registers that aren't live across the call, but that seems a little
		// messy.
		u32 arity = instr->val.call.arity;
		assert(arity <= STATIC_ARRAY_LENGTH(argument_registers));
		for (u32 i = 0; i < arity; i++) {
			AsmArg arg_current_reg = asm_value(instr->val.call.arg_array[i]);
			AsmArg arg_target_reg = asm_physical_register(argument_registers[i]);
			emit_instr2(builder, MOV, arg_target_reg, arg_current_reg);
		}

		emit_instr1(builder, CALL, asm_value(instr->val.call.callee));
		emit_instr2(builder, MOV, asm_virtual_register(next_vreg(builder)),
				asm_physical_register(RAX));

		assign_vreg(builder, instr);
		break;
	}
	case OP_BIT_XOR: {
		AsmArg arg1 = asm_value(instr->val.binary_op.arg1);
		AsmArg arg2 = asm_value(instr->val.binary_op.arg2);
		emit_instr2(builder, MOV, asm_virtual_register(next_vreg(builder)), arg1);
		emit_instr2(builder, XOR, asm_virtual_register(next_vreg(builder)), arg2);

		assign_vreg(builder, instr);
		break;
	}
	case OP_IMUL: {
		AsmArg arg1 = asm_value(instr->val.binary_op.arg1);
		AsmArg arg2 = asm_value(instr->val.binary_op.arg2);
		emit_instr2(builder, MOV, asm_virtual_register(next_vreg(builder)), arg1);
		emit_instr2(builder, IMUL, asm_virtual_register(next_vreg(builder)), arg2);

		assign_vreg(builder, instr);
		break;
	}
	}
}

#if 0
// We deliberately exclude RBP from this list, as we don't support omitting the
// frame pointer and so we never use it for register allocation.
static PhysicalRegister callee_save_registers[] = {
	RBX, R12, R13, R14, R15,
};
#endif

static PhysicalRegister caller_save_registers[] = {
	RAX, RCX, RDX, RDI, RSI, R8, R9, R10, R11,
};

static void allocate_registers(AsmBuilder *builder)
{
	u32 reg_to_assign_index = 0;
	for (u32 i = 0; i < builder->virtual_registers.size; i++) {
		VRegInfo *vreg = ARRAY_REF(&builder->virtual_registers, VRegInfo, i);

		if (vreg->assigned_register == INVALID_REGISTER) {
			assert(reg_to_assign_index <
					STATIC_ARRAY_LENGTH(caller_save_registers));
			vreg->assigned_register = caller_save_registers[reg_to_assign_index];
			reg_to_assign_index++;
		}
	}

	Array(AsmInstr) *instrs = &builder->current_function->instrs;
	for (u32 i = 0; i < instrs->size; i++) {
		AsmInstr *instr = ARRAY_REF(instrs, AsmInstr, i);

		for (u32 j = 0; j < instr->num_args; j++) {
			AsmArg *arg = &instr->args[j];

			Register *reg;
			if (arg->type == REGISTER) {
				reg = &arg->val.reg;
			} else if (arg->type == OFFSET_REGISTER) {
				reg = &arg->val.offset_register.reg;
			} else {
				continue;
			}

			if (reg->type == VIRTUAL_REGISTER) {
				u32 reg_num = reg->val.register_number;
				VRegInfo *vreg = ARRAY_REF(&builder->virtual_registers,
						VRegInfo, reg_num);
				PhysicalRegister phys_reg = vreg->assigned_register;

				reg->type = PHYSICAL_REGISTER;
				reg->val.physical_register = phys_reg;
			}
		}
	}
}

void asm_gen_function(AsmBuilder *builder, IrGlobal *ir_global)
{
	assert(ir_global->kind == IR_GLOBAL_FUNCTION);
	IrFunction *ir_func = &ir_global->val.function;
	append_function(builder, ir_global->name);

	if (ARRAY_IS_VALID(&builder->virtual_registers))
		array_free(&builder->virtual_registers);
	ARRAY_INIT(&builder->virtual_registers, VRegInfo, 20);
	IrType return_type = ir_func->return_type;
	assert(return_type.kind == IR_INT && return_type.val.bit_width == 32);

	assert(ir_func->arity <= STATIC_ARRAY_LENGTH(argument_registers));
	for (u32 i = 0; i < ir_func->arity; i++) {
		VRegInfo *vreg_info = ARRAY_APPEND(&builder->virtual_registers, VRegInfo);
		vreg_info->source = ARG;
		vreg_info->val.arg_index = i;
		vreg_info->assigned_register = argument_registers[i];
	}

	emit_instr1(builder, PUSH, asm_physical_register(RBP));
	emit_instr2(builder, MOV, asm_physical_register(RBP), asm_physical_register(RSP));
	AsmInstr *reserve_stack = emit_instr2(builder, SUB,
			asm_physical_register(RSP), asm_const32(0));

	for (u32 block_index = 0; block_index < ir_func->blocks.size; block_index++) {
		IrBlock *block = *ARRAY_REF(&ir_func->blocks, IrBlock *, block_index);
		AsmLabel *label = pool_alloc(&builder->asm_module.pool, sizeof *label);
		label->name = block->name;

		*ARRAY_APPEND(&builder->current_function->labels, AsmLabel *) = label;
		block->label = label;
	}

	for (u32 block_index = 0; block_index < ir_func->blocks.size; block_index++) {
		IrBlock *block = *ARRAY_REF(&ir_func->blocks, IrBlock *, block_index);

		u32 first_instr_of_block_index = builder->current_function->instrs.size;
		Array(IrInstr *) *instrs = &block->instrs;
		for (u32 i = 0; i < instrs->size; i++) {
			asm_gen_instr(ir_func, builder, *ARRAY_REF(instrs, IrInstr *, i));
		}

		AsmInstr *first_instr_of_block = ARRAY_REF(
				&builder->current_function->instrs,
				AsmInstr,
				first_instr_of_block_index);
		first_instr_of_block->label = block->label;
	}

	reserve_stack->args[1] = asm_const32(builder->local_stack_usage);

	allocate_registers(builder);
}

void generate_asm_module(AsmBuilder *builder, TransUnit *trans_unit)
{
	for (u32 i = 0; i < trans_unit->globals.size; i++) {
		IrGlobal *ir_global = *ARRAY_REF(&trans_unit->globals, IrGlobal *, i);
		AsmGlobal *asm_global =
			pool_alloc(&builder->asm_module.pool, sizeof *asm_global);
		*ARRAY_APPEND(&builder->asm_module.globals, AsmGlobal *) = asm_global;

		asm_global->name = ir_global->name;
		asm_global->offset = 0;

		switch (ir_global->kind) {
		case IR_GLOBAL_FUNCTION:
			asm_gen_function(builder, ir_global);
			break;
		case IR_GLOBAL_SCALAR: UNIMPLEMENTED;
		}
	}
}
