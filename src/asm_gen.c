#include <assert.h>

#include "ir.h"
#include "asm.h"
#include "asm_gen.h"

void init_asm_builder(AsmBuilder *builder)
{
	ARRAY_INIT(&builder->asm_module.blocks, AsmBlock, 10);
	builder->next_virtual_register = 0;
	builder->local_stack_usage = 0;
	builder->stack_slots = ARRAY_ZEROED;
}

void free_asm_builder(AsmBuilder *builder)
{
	for (u32 i = 0; i < builder->asm_module.blocks.size; i++)
		array_free(ARRAY_REF(&builder->asm_module.blocks, Array(AsmBlock), i));

	array_free(&builder->asm_module.blocks);
	if (builder->stack_slots.elements != NULL)
		array_free(&builder->stack_slots);
}

void append_block(AsmBuilder *builder)
{
	AsmBlock *new_block = ARRAY_APPEND(&builder->asm_module.blocks, AsmBlock);
	builder->current_block = new_block;

	builder->next_virtual_register = 0;
	builder->local_stack_usage = 0;
	if (builder->stack_slots.elements != NULL)
		array_free(&builder->stack_slots);
	ARRAY_INIT(&builder->stack_slots, StackSlot, 5);
}

void emit_instr0(AsmBuilder *builder, AsmOp op)
{
	AsmInstr *instr = ARRAY_APPEND(&builder->current_block->instrs, AsmInstr);
	instr->op = op;
}

AsmArg emit_instr2(AsmBuilder *builder, AsmOp op, AsmArg arg1, AsmArg arg2)
{
	AsmInstr *instr = ARRAY_APPEND(&builder->current_block->instrs, AsmInstr);
	instr->op = op;
	instr->args[0] = arg1;
	instr->args[1] = arg2;

	return asm_virtual_register(builder->next_virtual_register++);
}

static u32 size_of_ir_type(IrType type)
{
	switch (type.kind) {
	case IR_INT:
		return type.val.bit_width;
	case IR_POINTER:
		return 8;
	}
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
	case OP_BRANCH: {
		Block *target_block = instr->val.branch.target_block;
		assert(target_block == &ir_func->ret_block);

		Value *ret_value = &instr->val.branch.argument;
		assert(ret_value->kind == VALUE_CONST);

		i64 constant = ret_value->val.constant;
		assert(constant == (constant & 0xFFFFFFFF));
		emit_instr2(builder, MOV, asm_physical_register(EAX), asm_const32((i32)constant));
		emit_instr0(builder, RET);

		break;
	}
	default:
		UNIMPLEMENTED;
	}
}

void asm_gen_function(AsmBuilder *builder, IrFunction *ir_func)
{
	append_block(builder);
	IrType return_type = ir_function_return_type(ir_func);
	assert(return_type.kind == IR_INT && return_type.val.bit_width == 32);

	Block *block = &ir_func->entry_block;
	Array(IrInstr) *instrs = &block->instrs;
	for (u32 i = 0; i < instrs->size; i++) {
		asm_gen_instr(ir_func, builder, ARRAY_REF(instrs, IrInstr, i));
	}
}

void generate_asm_module(AsmBuilder *builder, TransUnit *trans_unit)
{
	for (u32 i = 0; i < trans_unit->functions.size; i++) {
		IrFunction *func = ARRAY_REF(&trans_unit->functions, IrFunction, i);
		asm_gen_function(builder, func);
	}
}
