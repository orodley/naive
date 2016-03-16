#include <assert.h>

#include "ir.h"
#include "asm.h"

static void asm_gen_instr(
		IrFunction *ir_func, AsmFunction *asm_func, IrInstr *instr);

void asm_gen_function(AsmModule *asm_module, IrFunction *ir_func)
{
	AsmFunction *asm_func = ARRAY_APPEND(&asm_module->functions, AsmFunction);
	init_asm_func(asm_func, ir_func->name);

	assert(function_return_type(ir_func).bit_width == 32);

	Block *block = &ir_func->entry_block;
	Array(IrInstr) *instrs = &block->instrs;
	for (u32 i = 0; i < instrs->size; i++) {
		asm_gen_instr(ir_func, asm_func, ARRAY_REF(instrs, IrInstr, i));
	}
}

static void asm_gen_instr(
		IrFunction *ir_func, AsmFunction *asm_func, IrInstr *instr)
{
	switch (instr->op) {
	case OP_BRANCH: {
		Block *target_block = instr->val.branch.target_block;
		assert(target_block == &ir_func->ret_block);

		Value *ret_value = &instr->val.branch.argument;
		assert(ret_value->kind == VALUE_CONST);

		i64 constant = ret_value->val.constant;
		assert(constant >= INT32_MIN && constant <= INT32_MAX);
		emit_instr2(asm_func, MOV, asm_reg(EAX), asm_const32((i32)constant));
		emit_instr0(asm_func, RET);

		break;
	}
	}
}
