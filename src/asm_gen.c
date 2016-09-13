#include <assert.h>
#include <stdlib.h>

#include "array.h"
#include "asm.h"
#include "asm_gen.h"
#include "flags.h"
#include "ir.h"
#include "util.h"

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

static AsmGlobal *append_function(AsmBuilder *builder, char *name)
{
	AsmGlobal *new_global = pool_alloc(&builder->asm_module.pool, sizeof *new_global);
	*ARRAY_APPEND(&builder->asm_module.globals, AsmGlobal *) = new_global;
	AsmFunction *new_function = &new_global->val.function;

	new_global->type = ASM_GLOBAL_FUNCTION;
	init_asm_function(new_function, name);
	builder->current_function = new_function;
	builder->current_block = &new_function->body;

	builder->local_stack_usage = 0;
	if (ARRAY_IS_VALID(&builder->stack_slots))
		array_free(&builder->stack_slots);
	ARRAY_INIT(&builder->stack_slots, StackSlot, 5);

	return new_global;
}

AsmInstr *emit_instr0(AsmBuilder *builder, AsmOp op)
{
	AsmInstr *instr = ARRAY_APPEND(builder->current_block, AsmInstr);
	instr->op = op;
	instr->num_args = 0;
	instr->label = NULL;

	return instr;
}

AsmInstr *emit_instr1(AsmBuilder *builder, AsmOp op, AsmArg arg1)
{
	AsmInstr *instr = ARRAY_APPEND(builder->current_block, AsmInstr);
	instr->op = op;
	instr->num_args = 1;
	instr->args[0] = arg1;
	instr->label = NULL;

	return instr;
}

AsmInstr *emit_instr2(AsmBuilder *builder, AsmOp op, AsmArg arg1, AsmArg arg2)
{
	AsmInstr *instr = ARRAY_APPEND(builder->current_block, AsmInstr);
	instr->op = op;
	instr->num_args = 2;
	instr->args[0] = arg1;
	instr->args[1] = arg2;
	instr->label = NULL;

	return instr;
}

AsmInstr *emit_instr3(AsmBuilder *builder, AsmOp op,
		AsmArg arg1, AsmArg arg2, AsmArg arg3)
{
	AsmInstr *instr = ARRAY_APPEND(builder->current_block, AsmInstr);
	instr->op = op;
	instr->num_args = 3;
	instr->args[0] = arg1;
	instr->args[1] = arg2;
	instr->args[2] = arg3;
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

static RegClass argument_registers[] = {
	REG_CLASS_DI, REG_CLASS_SI, REG_CLASS_D,
	REG_CLASS_C, REG_CLASS_R8, REG_CLASS_R9,
};

static AsmArg asm_value(IrValue value)
{
	switch (value.kind) {
	case VALUE_CONST:
		assert((value.val.constant & 0xFFFFFFFF) == value.val.constant);
		return asm_const32(value.val.constant & 0xFFFFFFFF);
	case VALUE_INSTR: {
		i32 vreg_number = value.val.instr->vreg_number;
		assert(vreg_number != -1);
		assert(value.type.kind == IR_INT);
		return asm_vreg(vreg_number, value.type.val.bit_width);
	}
	case VALUE_ARG: {
		assert(value.type.kind == IR_INT);
		assert(value.val.arg_index < STATIC_ARRAY_LENGTH(argument_registers));
		assert(value.type.kind == IR_INT);

		u8 width = value.type.val.bit_width;

		// We always allocate virtual registers to arguments first, so arg at
		// index i = virtual register i.
		return asm_vreg(value.val.arg_index, width);
	}
	case VALUE_GLOBAL:
		return asm_global(value.val.global->asm_global);
	}
}

static VRegInfo *append_vreg(AsmBuilder *builder)
{
	if (next_vreg(builder) == 4) {
		volatile int x = 3;
		x = x;
	}
	VRegInfo *vreg_info = ARRAY_APPEND(&builder->virtual_registers, VRegInfo);
	vreg_info->assigned_register = INVALID_REG_CLASS;
	vreg_info->live_range_start = vreg_info->live_range_end = -1;

	return vreg_info;
}

static VRegInfo *assign_vreg(AsmBuilder *builder, IrInstr *instr)
{
	u32 vreg_number = next_vreg(builder);
	VRegInfo *vreg = append_vreg(builder);
	instr->vreg_number = vreg_number;

	return vreg;
}

static AsmArg pre_alloced_vreg(AsmBuilder *builder, RegClass class, u8 width)
{
	u32 vreg_number = next_vreg(builder);

	VRegInfo *vreg_info = append_vreg(builder);
	vreg_info->assigned_register = class;

	return asm_vreg(vreg_number, width);
}

AsmLabel *append_label(AsmBuilder *builder, char *name)
{
	AsmLabel *label = pool_alloc(&builder->asm_module.pool, sizeof *label);
	label->name = name;
	*ARRAY_APPEND(&builder->current_function->labels, AsmLabel *) = label;

	return label;
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
		assert(ir_func->return_type.kind == IR_INT);

		emit_instr2(builder,
				MOV,
				asm_phys_reg(REG_CLASS_A, ir_func->return_type.val.bit_width),
				asm_value(arg));
		emit_instr1(builder, JMP, asm_label(builder->current_function->ret_label));

		break;
	}
	case OP_BRANCH:
		emit_instr1(builder, JMP, asm_label(instr->val.target_block->label));
		break;
	case OP_COND:
		emit_instr2(builder, CMP, asm_value(instr->val.cond.condition), asm_const32(0));
		emit_instr1(builder, JE, asm_label(instr->val.cond.else_block->label));
		emit_instr1(builder, JMP, asm_label(instr->val.cond.then_block->label));
		break;
	case OP_STORE: {
		IrValue pointer = instr->val.store.pointer;
		IrValue value = instr->val.store.value;
		IrType type = instr->val.store.type;

		assert(ir_type_eq(value.type, type));
		assert(type.kind == IR_INT);

		assert(pointer.kind == VALUE_INSTR);
		IrInstr *pointer_instr = pointer.val.instr;
		assert(pointer_instr->op == OP_LOCAL);

		StackSlot *slot = stack_slot_for_id(builder, pointer_instr->id);
		assert(slot != NULL);

		emit_instr2(builder, MOV,
				asm_deref(asm_offset_reg(REG_CLASS_SP, 64, slot->stack_offset)),
				asm_value(value));
		break;
	}
	case OP_LOAD: {
		IrValue pointer = instr->val.load.pointer;
		IrType type = instr->val.load.type;

		assert(type.kind == IR_INT);

		assert(pointer.kind == VALUE_INSTR);
		IrInstr *pointer_instr = pointer.val.instr;
		assert(pointer_instr->op == OP_LOCAL);

		StackSlot *slot = stack_slot_for_id(builder, pointer_instr->id);
		assert(slot != NULL);

		emit_instr2(builder, MOV,
				asm_vreg(next_vreg(builder), type.val.bit_width),
				asm_deref(asm_offset_reg(REG_CLASS_SP, 64, slot->stack_offset)));
		assign_vreg(builder, instr);
		break;
	}
	case OP_CALL: {
		u32 arity = instr->val.call.arity;
		assert(arity <= STATIC_ARRAY_LENGTH(argument_registers));
		for (u32 i = 0; i < arity; i++) {
			AsmArg arg = asm_value(instr->val.call.arg_array[i]);
			// Use the 64-bit version of the register, as all argument
			// registers are 64-bit.
			if (arg.type == ASM_ARG_REGISTER)
				arg.val.reg.width = 64;
			AsmArg arg_target_reg = pre_alloced_vreg(builder, argument_registers[i], 64);
			emit_instr2(builder, MOV, arg_target_reg, arg);
		}

		emit_instr1(builder, CALL, asm_value(instr->val.call.callee));
		assign_vreg(builder, instr)->assigned_register = REG_CLASS_A;
		break;
	}
	case OP_BIT_XOR: {
		assert(instr->type.kind == IR_INT);
		u8 width = instr->type.val.bit_width;

		AsmArg arg1 = asm_value(instr->val.binary_op.arg1);
		AsmArg arg2 = asm_value(instr->val.binary_op.arg2);
		emit_instr2(builder, MOV, asm_vreg(next_vreg(builder), width), arg1);
		emit_instr2(builder, XOR, asm_vreg(next_vreg(builder), width), arg2);

		assign_vreg(builder, instr);
		break;
	}
	case OP_ADD: {
		assert(instr->type.kind == IR_INT);
		u8 width = instr->type.val.bit_width;

		AsmArg arg1 = asm_value(instr->val.binary_op.arg1);
		AsmArg arg2 = asm_value(instr->val.binary_op.arg2);
		emit_instr2(builder, MOV, asm_vreg(next_vreg(builder), width), arg1);
		emit_instr2(builder, ADD, asm_vreg(next_vreg(builder), width), arg2);

		assign_vreg(builder, instr);
		break;
	}
	case OP_MUL: {
		assert(instr->type.kind == IR_INT);
		u8 width = instr->type.val.bit_width;

		AsmArg arg1 = asm_value(instr->val.binary_op.arg1);
		AsmArg arg2 = asm_value(instr->val.binary_op.arg2);

		if (!asm_arg_is_const(arg1) && !asm_arg_is_const(arg2)) {
			emit_instr2(builder, MOV, asm_vreg(next_vreg(builder), width), arg1);
			emit_instr2(builder, IMUL, asm_vreg(next_vreg(builder), width), arg2);
		} else {
			AsmArg const_arg;
			AsmArg non_const_arg;
			if (asm_arg_is_const(arg1)) {
				const_arg = arg1;
				non_const_arg = arg2;
			} else {
				const_arg = arg2;
				non_const_arg = arg1;
			}

			assert(!asm_arg_is_const(non_const_arg));

			emit_instr3(builder, IMUL,
					asm_vreg(next_vreg(builder), width),
					non_const_arg,
					const_arg);
		}

		assign_vreg(builder, instr);
		break;
	}
	case OP_EQ: {
		AsmArg arg1 = asm_value(instr->val.binary_op.arg1);
		AsmArg arg2 = asm_value(instr->val.binary_op.arg2);
		u32 vreg = next_vreg(builder);
		emit_instr2(builder, XOR, asm_vreg(vreg, 32), asm_vreg(vreg, 32));
		emit_instr2(builder, CMP, arg1, arg2);
		emit_instr1(builder, SETE, asm_vreg(vreg, 8));

		assign_vreg(builder, instr);
		break;
	}
	case OP_NEQ: {
		AsmArg arg1 = asm_value(instr->val.binary_op.arg1);
		AsmArg arg2 = asm_value(instr->val.binary_op.arg2);
		u32 vreg = next_vreg(builder);
		emit_instr2(builder, XOR, asm_vreg(vreg, 32), asm_vreg(vreg, 32));
		emit_instr2(builder, CMP, arg1, arg2);
		emit_instr1(builder, SETNE, asm_vreg(vreg, 8));

		assign_vreg(builder, instr);
		break;
	}
	}
}

static Register *arg_reg(AsmArg *arg)
{
	if (arg->type == ASM_ARG_REGISTER)
		return &arg->val.reg;
	if (arg->type == ASM_ARG_OFFSET_REGISTER)
		return &arg->val.offset_register.reg;
	return NULL;
}

#define ALLOCATION_ORDER \
	X(0, REG_CLASS_R11), \
	X(1, REG_CLASS_R10), \
	X(2, REG_CLASS_R9), \
	X(3, REG_CLASS_R8), \
	X(4, REG_CLASS_B), \
	X(5, REG_CLASS_R12), \
	X(6, REG_CLASS_R13), \
	X(7, REG_CLASS_R14), \
	X(8, REG_CLASS_R15), \
	X(9, REG_CLASS_C), \
	X(10, REG_CLASS_D), \
	X(11, REG_CLASS_SI), \
	X(12, REG_CLASS_DI), \
	X(13, REG_CLASS_A),

#define X(i, x) [i] = x
static RegClass alloc_index_to_reg[] = {
	ALLOCATION_ORDER
};
#undef X

#define X(i, x) [x] = i
static u32 reg_to_alloc_index[] = {
	ALLOCATION_ORDER
};
#undef X

// We deliberately exclude RBP from this list, as we don't support omitting the
// frame pointer and so we never use it for register allocation.
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

#define CALLER_SAVE_REGS_BITMASK \
	((1 << REG_CLASS_A) | (1 << REG_CLASS_DI) | (1 << REG_CLASS_SI) | \
	 (1 << REG_CLASS_D) | (1 << REG_CLASS_C) | (1 << REG_CLASS_R8) | \
	 (1 << REG_CLASS_R9) | (1 << REG_CLASS_R10) | (1 << REG_CLASS_R11))

typedef struct CallSite
{
	u32 instr_index;
	u32 active_caller_save_regs_bitset;
} CallSite;

int compare_live_range_start(const void *a, const void *b)
{
	const VRegInfo *live_range_a = (VRegInfo *)a, *live_range_b = (VRegInfo *)b;
	int x = live_range_a->live_range_start, y = live_range_b->live_range_start;
	if (x < y)
		return -1;
	if (x == y)
		return 0;
	return 1;
}

// @TODO: Save all caller save registers that are live across calls.
// @TODO: Do proper liveness analysis - right now we do nothing about jumps.
static void allocate_registers(AsmBuilder *builder)
{
	Array(AsmInstr) *body = &builder->current_function->body;
	for (u32 i = 0; i < body->size; i++) {
		AsmInstr *instr = ARRAY_REF(body, AsmInstr, i);
		for (u32 j = 0; j < instr->num_args; j++) {
			AsmArg *arg = instr->args + j;
			Register *reg = arg_reg(arg);
			if (reg != NULL && reg->type == V_REG) {
				u32 reg_num = reg->val.vreg_number;
				VRegInfo *vreg = ARRAY_REF(&builder->virtual_registers,
						VRegInfo, reg_num);
				if (vreg->live_range_start == -1) {
					vreg->live_range_start = vreg->live_range_end = i;
				} else {
					vreg->live_range_end = i;
				}
			}
		}
	}

	if (flag_dump_live_ranges) {
		dump_asm_function(builder->current_function);
		for (u32 i = 0; i < builder->virtual_registers.size; i++) {
			VRegInfo *vreg = ARRAY_REF(&builder->virtual_registers, VRegInfo, i);
			printf("#%u: [%d, %d]\n", i, vreg->live_range_start, vreg->live_range_end);
		}
		putchar('\n');
	}

	u32 free_regs_bitset;
	assert(STATIC_ARRAY_LENGTH(alloc_index_to_reg) < 8 * sizeof free_regs_bitset);

	// Start with all regs free
	free_regs_bitset = (1 << STATIC_ARRAY_LENGTH(alloc_index_to_reg)) - 1;

	// virtual_regsiters isn't necessarily sorted by live range start.
	// i.e. if RAX is pre-allocated from a function call and we don't use it
	// until later. Linear scan depends on this ordering, so we sort it first.
	// We can't sort it in place because the indices are used to look vregs up
	// by vreg number.
	VRegInfo **live_ranges = malloc(builder->virtual_registers.size * sizeof *live_ranges);
	u32 live_range_out_index = 0;
	for (u32 i = 0; i < builder->virtual_registers.size; i++) {
		VRegInfo *vreg = ARRAY_REF(&builder->virtual_registers, VRegInfo, i);
		// This indicates that we assigned a vreg to something that wasn't
		// used, e.g. a pre-alloced RAX for the return value of a function.
		if (vreg->live_range_start == -1) {
			assert(vreg->live_range_end == -1);
			continue;
		}

		live_ranges[live_range_out_index] = vreg;
		live_range_out_index++;
	}
	u32 num_live_ranges = live_range_out_index;
	qsort(live_ranges, num_live_ranges, sizeof live_ranges[0], compare_live_range_start);

	Array(VRegInfo *) active_vregs;
	ARRAY_INIT(&active_vregs, VRegInfo *, 16);
	for (u32 i = 0; i < num_live_ranges; i++) {
		VRegInfo *vreg = live_ranges[i];

		while (active_vregs.size != 0) {
			VRegInfo *active_vreg = *ARRAY_REF(&active_vregs, VRegInfo *, 0);
			if (active_vreg->live_range_end >= vreg->live_range_start)
				break;
			u32 alloc_index = reg_to_alloc_index[active_vreg->assigned_register];
			free_regs_bitset |= 1 << alloc_index;

			// @TODO: Remove all the invalidated vregs at once instead of
			// repeatedly shifting down.
			ARRAY_REMOVE(&active_vregs, VRegInfo *, 0);
		}

		if (vreg->assigned_register  == INVALID_REG_CLASS) {
			// @TODO: Insert spills when there are no free registers to assign.
			assert(free_regs_bitset != 0);
			u32 first_free_alloc_index = lowest_set_bit(free_regs_bitset);
			vreg->assigned_register = alloc_index_to_reg[first_free_alloc_index];
			free_regs_bitset &= ~(1 << first_free_alloc_index);
		} else {
			// This register has already been assigned, e.g. part of a call
			// sequence. We don't need to allocate it, but we do need to keep
			// track of it so it doesn't get clobbered.
			u32 alloc_index = reg_to_alloc_index[vreg->assigned_register];
			assert((free_regs_bitset & (1 << alloc_index)) != 0);
			free_regs_bitset &= ~(1 << alloc_index);
		}

		u32 insertion_point = active_vregs.size;
		for (u32 j = 0; j < active_vregs.size; j++) {
			VRegInfo *active_vreg = *ARRAY_REF(&active_vregs, VRegInfo *, j);
			if (active_vreg->live_range_end > vreg->live_range_end) {
				insertion_point = j;
				break;
			}
		}
		*ARRAY_INSERT(&active_vregs, VRegInfo *, insertion_point) = vreg;
	}
	free(live_ranges);

	array_clear(&active_vregs);
	Array(CallSite) callsites;
	ARRAY_INIT(&callsites, CallSite, 5);
	u32 live_regs_bitset = 0;
	u32 total_regs_to_save = 0;
	u32 vreg_index = 0;

	for (u32 i = 0; i < body->size; i++) {
		while (active_vregs.size != 0) {
			VRegInfo *active_vreg = *ARRAY_REF(&active_vregs, VRegInfo *, 0);
			if (active_vreg->live_range_end == -1)
				continue;
			if ((u32)active_vreg->live_range_end >= i)
				break;
			assert(active_vreg->assigned_register != INVALID_REG_CLASS);
			live_regs_bitset &= ~(1 << active_vreg->assigned_register);

			// @TODO: Remove all the invalidated vregs at once instead of
			// repeatedly shifting down.
			ARRAY_REMOVE(&active_vregs, VRegInfo *, 0);
		}
		if (vreg_index != builder->virtual_registers.size) {
			VRegInfo *next_vreg =
				ARRAY_REF(&builder->virtual_registers, VRegInfo, vreg_index);
			if ((u32)next_vreg->live_range_start == i) {
				assert(next_vreg->assigned_register != INVALID_REG_CLASS);
				live_regs_bitset |= 1 << next_vreg->assigned_register;

				u32 insertion_point = active_vregs.size;
				for (u32 j = 0; j < active_vregs.size; j++) {
					VRegInfo *active_vreg = *ARRAY_REF(&active_vregs, VRegInfo *, j);
					if (active_vreg->live_range_end == -1)
						continue;
					if ((u32)active_vreg->live_range_end < i) {
						insertion_point = j;
						break;
					}
				}
				*ARRAY_INSERT(&active_vregs, VRegInfo *, insertion_point) = next_vreg;
			}
		}

		AsmInstr *instr = ARRAY_REF(body, AsmInstr, i);
		if (instr->op == OP_CALL) {
			CallSite *callsite = ARRAY_APPEND(&callsites, CallSite);
			callsite->instr_index = i;
			callsite->active_caller_save_regs_bitset =
				live_regs_bitset & CALLER_SAVE_REGS_BITMASK;
			total_regs_to_save += bit_count(live_regs_bitset);
		}
	}
	array_free(&active_vregs);

#if 0
	printf("Total regs to save: %u\n", total_regs_to_save);
	puts("Callsites:");
	for (u32 i = 0; i < callsites.size; i++) {
		CallSite *callsite = ARRAY_REF(&callsites, CallSite, i);
		printf("%d: [", callsite->instr_index);
		u32 active_regs = callsite->active_caller_save_regs_bitset;
		while (active_regs != 0) {
			u32 lowest_bit = lowest_set_bit(active_regs);
			printf("%d ", lowest_bit);
			active_regs &= ~(1 << lowest_bit);
		}
		puts("]");
	}
#endif
	// @TODO: Actually write the code to save registers across callsites.
	assert(total_regs_to_save == 0);

	array_free(&callsites);

	for (u32 i = 0; i < body->size; i++) {
		AsmInstr *instr = ARRAY_REF(body, AsmInstr, i);

		for (u32 j = 0; j < instr->num_args; j++) {
			AsmArg *arg = instr->args + j;
			Register *reg = arg_reg(arg);
			if (reg == NULL)
				continue;

			if (reg->type == V_REG) {
				u32 vreg_number = reg->val.vreg_number;
				VRegInfo *vreg = ARRAY_REF(&builder->virtual_registers,
						VRegInfo, vreg_number);
				RegClass class = vreg->assigned_register;

				reg->type = PHYS_REG;
				reg->val.class = class;
			}
		}
	}
}

AsmGlobal *asm_gen_function(AsmBuilder *builder, IrGlobal *ir_global)
{
	assert(ir_global->kind == IR_GLOBAL_FUNCTION);
	IrFunction *ir_func = &ir_global->val.function;

	AsmGlobal *function = append_function(builder, ir_global->name);
	ir_global->asm_global = function;
	AsmLabel *ret_label = pool_alloc(&builder->asm_module.pool, sizeof *ret_label);
	ret_label->name = "ret";
	ret_label->file_location = 0;
	function->val.function.ret_label = ret_label;

	function->defined = ir_global->defined;
	if (!ir_global->defined) {
		return function;
	}

	if (ARRAY_IS_VALID(&builder->virtual_registers))
		array_free(&builder->virtual_registers);
	ARRAY_INIT(&builder->virtual_registers, VRegInfo, 20);
	IrType return_type = ir_func->return_type;
	assert(return_type.kind == IR_INT && return_type.val.bit_width == 32);

	assert(ir_func->arity <= STATIC_ARRAY_LENGTH(argument_registers));
	for (u32 i = 0; i < ir_func->arity; i++) {
		VRegInfo *vreg_info = ARRAY_APPEND(&builder->virtual_registers, VRegInfo);
		vreg_info->assigned_register = argument_registers[i];
		vreg_info->live_range_start = vreg_info->live_range_end = -1;
	}

	for (u32 block_index = 0; block_index < ir_func->blocks.size; block_index++) {
		IrBlock *block = *ARRAY_REF(&ir_func->blocks, IrBlock *, block_index);
		AsmLabel *label = append_label(builder, block->name);
		block->label = label;
	}

	for (u32 block_index = 0; block_index < ir_func->blocks.size; block_index++) {
		IrBlock *block = *ARRAY_REF(&ir_func->blocks, IrBlock *, block_index);

		u32 first_instr_of_block_index = builder->current_function->body.size;
		Array(IrInstr *) *instrs = &block->instrs;
		for (u32 i = 0; i < instrs->size; i++) {
			asm_gen_instr(ir_func, builder, *ARRAY_REF(instrs, IrInstr *, i));
		}

		AsmInstr *first_instr_of_block = ARRAY_REF(
				&builder->current_function->body,
				AsmInstr,
				first_instr_of_block_index);
		first_instr_of_block->label = block->label;
	}

	allocate_registers(builder);

	RegClass used_callee_save_regs[
		STATIC_ARRAY_LENGTH(callee_save_registers)] = { 0 };
	u32 used_callee_save_regs_size = 0;
	for (u32 i = 0; i < builder->current_function->body.size; i++) {
		AsmInstr *instr = ARRAY_REF(&builder->current_function->body, AsmInstr, i);
		for (u32 j = 0; j < instr->num_args; j++) {
			Register *reg = arg_reg(instr->args + j);
			if (reg != NULL &&
					reg->type == PHYS_REG &&
					is_callee_save(reg->val.class)) {
				used_callee_save_regs[used_callee_save_regs_size++] = reg->val.class;
			}
		}
	}

	builder->current_block = &builder->current_function->prologue;

	AsmLabel *entry_label = pool_alloc(&builder->asm_module.pool, sizeof *entry_label);
	entry_label->name = ir_global->name;
	entry_label->file_location = 0;
	ir_func->label = entry_label;

	AsmInstr *prologue_first_instr =
		emit_instr1(builder, PUSH, asm_phys_reg(REG_CLASS_BP, 64));
	prologue_first_instr->label = entry_label;
	emit_instr2(builder,
			MOV,
			asm_phys_reg(REG_CLASS_BP, 64),
			asm_phys_reg(REG_CLASS_SP, 64));
	for (u32 i = 0; i < used_callee_save_regs_size; i++) {
		emit_instr1(builder, PUSH, asm_phys_reg(used_callee_save_regs[i], 64));
	}
	emit_instr2(builder,
			SUB,
			asm_phys_reg(REG_CLASS_SP, 64),
			asm_const32(builder->local_stack_usage));

	builder->current_block = &builder->current_function->epilogue;

	AsmInstr *epilogue_first_instr =
		emit_instr2(builder,
				ADD,
				asm_phys_reg(REG_CLASS_SP, 64),
				asm_const32(builder->local_stack_usage));
	for (u32 i = 0; i < used_callee_save_regs_size; i++) {
		emit_instr1(builder, POP, asm_phys_reg(used_callee_save_regs[i], 64));
	}
	epilogue_first_instr->label = ret_label;
	emit_instr1(builder, POP, asm_phys_reg(REG_CLASS_BP, 64));
	emit_instr0(builder, RET);

	return function;
}

void generate_asm_module(AsmBuilder *builder, TransUnit *trans_unit)
{
	for (u32 i = 0; i < trans_unit->globals.size; i++) {
		IrGlobal *ir_global = *ARRAY_REF(&trans_unit->globals, IrGlobal *, i);
		AsmGlobal *asm_global = NULL;

		switch (ir_global->kind) {
		case IR_GLOBAL_FUNCTION:
			asm_global = asm_gen_function(builder, ir_global);
			break;
		case IR_GLOBAL_SCALAR: UNIMPLEMENTED;
		}

		asm_global->name = ir_global->name;
		asm_global->offset = 0;
	}
}
