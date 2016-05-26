#include <assert.h>

#include "ir.h"
#include "asm.h"

static void asm_gen_instr(
		IrFunction *ir_func, AsmModule *asm_module, IrInstr *instr);

void asm_gen_function(AsmModule *asm_module, IrFunction *ir_func)
{
	emit_label(asm_module, ir_func->name);

	IrType return_type = ir_function_return_type(ir_func);
	assert(return_type.kind == IR_INT && return_type.val.bit_width == 32);

	Block *block = &ir_func->entry_block;
	Array(IrInstr) *instrs = &block->instrs;
	for (u32 i = 0; i < instrs->size; i++) {
		asm_gen_instr(ir_func, asm_module, ARRAY_REF(instrs, IrInstr, i));
	}
}

static void asm_gen_instr(
		IrFunction *ir_func, AsmModule *asm_module, IrInstr *instr)
{
	switch (instr->op) {
	case OP_BRANCH: {
		Block *target_block = instr->val.branch.target_block;
		assert(target_block == &ir_func->ret_block);

		Value *ret_value = &instr->val.branch.argument;
		assert(ret_value->kind == VALUE_CONST);

		i64 constant = ret_value->val.constant;
		assert(constant == (constant & 0xFFFFFFFF));
		emit_instr2(asm_module, MOV, asm_reg(EAX), asm_const32((i32)constant));
		emit_instr0(asm_module, RET);

		break;
	}
	default:
		UNIMPLEMENTED;
	}
}
