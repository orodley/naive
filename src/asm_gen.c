#include <assert.h>
#include <stdlib.h>

#include "array.h"
#include "asm.h"
#include "asm_gen.h"
#include "flags.h"
#include "ir.h"
#include "util.h"

void init_asm_builder(AsmBuilder *builder, char *input_file_name)
{
	init_asm_module(&builder->asm_module, input_file_name);

	builder->local_stack_usage = 0;
	builder->curr_sp_diff = 0;
	builder->virtual_registers = EMPTY_ARRAY;
}

void free_asm_builder(AsmBuilder *builder)
{
	free_asm_module(&builder->asm_module);

	if (ARRAY_IS_VALID(&builder->virtual_registers))
		array_free(&builder->virtual_registers);
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

AsmInstr *emit_instr2(AsmBuilder *builder, AsmOp op, AsmValue arg1, AsmValue arg2)
{
	AsmInstr *instr = ARRAY_APPEND(builder->current_block, AsmInstr);
	instr->op = op;
	instr->arity = 2;
	instr->args[0] = arg1;
	instr->args[1] = arg2;
	instr->num_deps = 0;
	instr->label = NULL;

	if (op == MOV
			&& arg1.t == ASM_VALUE_REGISTER
			&& arg2.t == ASM_VALUE_REGISTER
			&& arg1.u.reg.width != arg2.u.reg.width
			&& !arg1.is_deref
			&& !arg2.is_deref) {
		assert(!"Bad MOV! Registers are different sizes.");
	}

	return instr;
}

AsmInstr *emit_instr3(AsmBuilder *builder, AsmOp op,
		AsmValue arg1, AsmValue arg2, AsmValue arg3)
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

static inline void add_dep(AsmInstr *instr, AsmValue dep)
{
	assert(instr->num_deps < STATIC_ARRAY_LENGTH(instr->vreg_deps));
	assert(dep.t == ASM_VALUE_REGISTER);
	assert(dep.u.reg.t == V_REG);

	instr->vreg_deps[instr->num_deps++] = dep.u.reg.u.vreg_number;
}

// @TODO: Rethink name? "next" kinda suggests side effects, i.e. "move to the
// next vreg number".
static inline u32 next_vreg(AsmBuilder *builder)
{
	return builder->virtual_registers.size;
}

static VRegInfo *append_vreg(AsmBuilder *builder)
{
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

static AsmValue pre_alloced_vreg(AsmBuilder *builder, RegClass class, u8 width)
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

static AsmValue asm_value(AsmBuilder *builder, IrValue value)
{
	switch (value.t) {
	case VALUE_CONST:
		assert((value.u.constant & 0xFFFFFFFF) == value.u.constant);
		return asm_const(value.u.constant & 0xFFFFFFFF);
	case VALUE_INSTR: {
		IrInstr *instr = value.u.instr;

		switch (instr->op) {
		case OP_LOCAL: {
			u32 vreg = next_vreg(builder);
			emit_instr2(builder,
					MOV,
					asm_vreg(vreg, 64),
					asm_phys_reg(REG_CLASS_SP, 64));
			emit_instr2(builder,
					ADD,
					asm_vreg(vreg, 64),
					asm_const(instr->u.local.stack_offset + builder->curr_sp_diff));
			append_vreg(builder);
			return asm_vreg(vreg, 64);
		}
		case OP_CAST: {
			AsmValue cast_value = asm_value(builder, instr->u.arg);
			if (cast_value.t == ASM_VALUE_REGISTER)
				cast_value.u.reg.width = size_of_ir_type(instr->type) * 8;

			return cast_value;
		}
		default: {
			i32 vreg_number = instr->vreg_number;
			assert(vreg_number != -1);

			assert(value.type.t == IR_INT || value.type.t == IR_POINTER);
			return asm_vreg(vreg_number, size_of_ir_type(instr->type) * 8);
		}
		}
	}
	case VALUE_ARG: {
		u32 index = value.u.arg_index;
		ArgClass *arg_class =
			builder->current_function->call_seq.arg_classes + index;

		switch (arg_class->t) {
		case ARG_CLASS_REG:
			return asm_vreg(arg_class->u.reg.vreg, size_of_ir_type(value.type) * 8);
		case ARG_CLASS_MEM: {
			// Arguments passed on the stack sit above the previous frame
			// pointer and return address (hence 16 bytes).
			// See the System V x86-64 ABI spec, figure 3.3
			AsmValue bp_offset = asm_const(arg_class->u.mem.offset + 16);

			u32 vreg = next_vreg(builder);
			append_vreg(builder);
			if (arg_class->u.mem.remains_in_memory) {
				emit_instr2(builder,
						MOV,
						asm_vreg(vreg, 64),
						asm_phys_reg(REG_CLASS_BP, 64));
				emit_instr2(builder, ADD, asm_vreg(vreg, 64), bp_offset);
				return asm_vreg(vreg, 64);
			} else {
				emit_instr2(builder,
						MOV,
						asm_vreg(vreg, 64),
						asm_deref(asm_offset_reg(REG_CLASS_BP, 64,
								bp_offset.u.constant)));
				return asm_vreg(vreg, size_of_ir_type(value.type) * 8);
			}
		}
		}
	}
	case VALUE_GLOBAL: {
		AsmGlobal *global = value.u.global->asm_global;
		assert(global != NULL);
		return asm_global(global);
	}
	}
}

static void asm_gen_binary_instr(AsmBuilder *builder, IrInstr *instr, AsmOp op)
{
	assert(instr->type.t == IR_INT);
	u8 width = instr->type.u.bit_width;

	AsmValue arg1 = asm_value(builder, instr->u.binary_op.arg1);
	AsmValue arg2 = asm_value(builder, instr->u.binary_op.arg2);
	emit_instr2(builder, MOV, asm_vreg(next_vreg(builder), width), arg1);
	emit_instr2(builder, op, asm_vreg(next_vreg(builder), width), arg2);

	assign_vreg(builder, instr);
}

static void asm_gen_relational_instr(AsmBuilder *builder, IrInstr *instr, AsmOp op)
{
	AsmValue arg1 = asm_value(builder, instr->u.binary_op.arg1);
	AsmValue arg2 = asm_value(builder, instr->u.binary_op.arg2);
	u32 vreg = next_vreg(builder);
	emit_instr2(builder, XOR, asm_vreg(vreg, 32), asm_vreg(vreg, 32));
	emit_instr2(builder, CMP, arg1, arg2);
	emit_instr1(builder, op, asm_vreg(vreg, 8));

	assign_vreg(builder, instr);
}

static void handle_phi_nodes(AsmBuilder *builder, IrBlock *src_block,
		IrBlock *dest_block)
{
	Array(IrInstr *) *dest_instrs = &dest_block->instrs;
	for (u32 ir_instr_index = 0;
			ir_instr_index < dest_instrs->size;
			ir_instr_index++) {
		IrInstr *instr = *ARRAY_REF(dest_instrs, IrInstr *, ir_instr_index);
		if (instr->op != OP_PHI)
			break;

		if (instr->vreg_number == -1) {
			assign_vreg(builder, instr);
		}

		bool encountered_src_block = false;
		for (u32 phi_param_index = 0;
				phi_param_index < instr->u.phi.arity;
				phi_param_index++) {
			IrPhiParam *param = instr->u.phi.params + phi_param_index;
			if (param->block == src_block) {
				emit_instr2(builder, MOV,
						asm_vreg(instr->vreg_number, size_of_ir_type(instr->type) * 8),
						asm_value(builder, param->value));

				encountered_src_block = true;
				break;
			}
		}
		assert(encountered_src_block);
	}
}

static RegClass argument_registers[] = {
	REG_CLASS_DI, REG_CLASS_SI, REG_CLASS_D,
	REG_CLASS_C, REG_CLASS_R8, REG_CLASS_R9,
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
	u32 curr_reg_index = 0;

	for (u32 i = 0; i < arity; i++) {
		ArgClass *arg_class = arg_classes + i;
		IrType *arg_type = arg_types + i;
		switch (arg_type->t) {
		case IR_INT: case IR_POINTER:
			if (curr_reg_index < STATIC_ARRAY_LENGTH(argument_registers)) {
				arg_class->t = ARG_CLASS_REG;
				arg_class->u.reg.reg = argument_registers[curr_reg_index++];
			} else {
				arg_class->t = ARG_CLASS_MEM;
				arg_class->u.mem.offset = curr_offset;
				arg_class->u.mem.size = 8;
				arg_class->u.mem.remains_in_memory = false;
				curr_offset += 8;
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
		case IR_ARRAY: case IR_FUNCTION: case IR_VOID:
			UNREACHABLE;
		}
	}

	return (CallSeq) {
		.stack_space = curr_offset,
		.arg_classes = arg_classes,
	};
}

static void asm_gen_instr(
		AsmBuilder *builder, IrGlobal *ir_global, IrBlock *curr_block, IrInstr *instr)
{
	switch (instr->op) {
	case OP_INVALID: UNREACHABLE;
	case OP_LOCAL: {
		// @TODO: Alignment of stack slots. This could probably use similar
		// logic to that of struct layout.
		instr->u.local.stack_offset = builder->local_stack_usage;
		builder->local_stack_usage += size_of_ir_type(instr->u.local.type);

		break;
	}
	case OP_FIELD: {
		IrValue struct_ptr = instr->u.field.struct_ptr;
		IrType struct_type = instr->u.field.struct_type;
		assert(struct_ptr.type.t == IR_POINTER);
		assert(struct_type.t == IR_STRUCT);
		IrStructField *field =
			struct_type.u.strukt.fields + instr->u.field.field_number;
		emit_instr2(builder,
				MOV,
				asm_vreg(next_vreg(builder), 64),
				asm_value(builder, struct_ptr));
		emit_instr2(builder,
				ADD,
				asm_vreg(next_vreg(builder), 64),
				asm_const(field->offset));
		assign_vreg(builder, instr);

		break;
	}
	case OP_RET_VOID:
		assert(ir_global->type.u.function.return_type->t == IR_VOID);
		emit_instr1(builder, JMP, asm_label(builder->current_function->ret_label));

		break;
	case OP_RET: {
		IrValue arg = instr->u.arg;
		assert(ir_global->type.t == IR_FUNCTION);
		assert(ir_type_eq(ir_global->type.u.function.return_type, &arg.type));
		IrType *return_type = ir_global->type.u.function.return_type;
		assert(return_type->t == IR_INT || return_type->t == IR_POINTER);

		emit_instr2(builder,
				MOV,
				asm_phys_reg(REG_CLASS_A, size_of_ir_type(*return_type) * 8),
				asm_value(builder, arg));
		emit_instr1(builder, JMP, asm_label(builder->current_function->ret_label));

		break;
	}
	case OP_BRANCH:
		handle_phi_nodes(builder, curr_block, instr->u.target_block);
		emit_instr1(builder, JMP, asm_label(instr->u.target_block->label));
		break;
	case OP_COND: {
		IrValue condition = instr->u.cond.condition;
		if (condition.t == VALUE_CONST) {
			IrBlock *block = condition.u.constant ?
				instr->u.cond.then_block :
				instr->u.cond.else_block;
			IrBlock *other_block = condition.u.constant ?
				instr->u.cond.else_block :
				instr->u.cond.then_block;

			handle_phi_nodes(builder, curr_block, block);
			emit_instr1(builder, JMP, asm_label(block->label));

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
			handle_phi_nodes(builder, curr_block, instr->u.cond.else_block);
			emit_instr2(builder, CMP, asm_value(builder, condition), asm_const(0));
			emit_instr1(builder, JE, asm_label(instr->u.cond.else_block->label));
			handle_phi_nodes(builder, curr_block, instr->u.cond.then_block);
			emit_instr1(builder, JMP, asm_label(instr->u.cond.then_block->label));
		}
		break;
	}
	case OP_PHI: {
		// Phi nodes are handled by asm_gen for the incoming branches, and
		// require no codegen in their containing block.
		assert(instr->vreg_number != -1);
		break;
	}
	case OP_STORE: {
		IrValue ir_pointer = instr->u.store.pointer;
		IrValue ir_value = instr->u.store.value;
		IrType type = instr->u.store.type;
		assert(ir_type_eq(&ir_value.type, &type));

		AsmValue pointer = asm_value(builder, ir_pointer);
		AsmValue value = asm_value(builder, ir_value);

		// Use offset_register directly where we can, to fold the addition into
		// the MOV rather than having a separate instruction.
		bool directly_storeable = value.t == ASM_VALUE_REGISTER ||
			(value.t == ASM_VALUE_CONST && value.u.constant.t == ASM_CONST_IMMEDIATE);
		if (ir_pointer.t == VALUE_INSTR && ir_pointer.u.instr->op == OP_LOCAL
				&& directly_storeable) {
			u32 offset = ir_pointer.u.instr->u.local.stack_offset
				+ builder->curr_sp_diff;
			AsmValue stack_address =
				asm_deref(asm_offset_reg(REG_CLASS_SP, 64, asm_const(offset).u.constant));
			emit_instr2(builder, MOV, stack_address, value);
		} else if (ir_pointer.t == VALUE_GLOBAL) {
			AsmValue rip_relative_addr =
				asm_offset_reg(
						REG_CLASS_IP,
						64,
						(AsmConst) {
							.t = ASM_CONST_GLOBAL,
							.u.global = ir_pointer.u.global->asm_global
						});
			emit_instr2(builder, MOV, asm_deref(rip_relative_addr), value);
		} else if (size_of_ir_type(type) < 8 || value.t != ASM_VALUE_CONST) {
			emit_instr2(builder, MOV, asm_deref(pointer), value);
		} else if (size_of_ir_type(type) == 8 && value.t == ASM_VALUE_CONST) {
			AsmValue temp_vreg = asm_vreg(next_vreg(builder), 64);
			emit_instr2(builder, MOV, temp_vreg, value);
			emit_instr2(builder, MOV, asm_deref(pointer), temp_vreg);
		} else {
			UNIMPLEMENTED;
		}

		break;
	}
	case OP_LOAD: {
		IrValue pointer = instr->u.load.pointer;
		IrType type = instr->u.load.type;
		AsmValue target = asm_vreg(next_vreg(builder), size_of_ir_type(type) * 8);
		assign_vreg(builder, instr);

		if (pointer.t == VALUE_INSTR && pointer.u.instr->op == OP_LOCAL) {
			u32 offset = pointer.u.instr->u.local.stack_offset
				+ builder->curr_sp_diff;
			AsmValue stack_address =
				asm_deref(asm_offset_reg(REG_CLASS_SP, 64, asm_const(offset).u.constant));
			emit_instr2(builder, MOV, target, stack_address);
		} else if (pointer.t == VALUE_GLOBAL) {
			AsmValue rip_relative_addr =
				asm_offset_reg(
						REG_CLASS_IP,
						64,
						(AsmConst) {
							.t = ASM_CONST_GLOBAL,
							.u.global = pointer.u.global->asm_global
						});
			emit_instr2(builder,
					MOV,
					target,
					asm_deref(rip_relative_addr));
		} else {
			emit_instr2(builder, MOV, target, asm_deref(asm_value(builder, pointer)));
		}

		break;
	}
	case OP_CAST: break; // cast is a type-theoretic operation only
	case OP_ZEXT: {
		assert(instr->type.t == IR_INT);
		assert(instr->u.arg.type.t == IR_INT);

		if (instr->type.u.bit_width == 64
				&& instr->u.arg.type.u.bit_width == 32) {
			// Implicit zero-extending of MOV 32 -> 32
			emit_instr2(builder,
					MOV,
					asm_vreg(next_vreg(builder), 32),
					asm_value(builder, instr->u.arg));
			assign_vreg(builder, instr);
		} else {
			emit_instr2(builder,
					MOVZX,
					asm_vreg(next_vreg(builder), instr->type.u.bit_width),
					asm_value(builder, instr->u.arg));
			assign_vreg(builder, instr);
		}

		break;
	}
	case OP_SEXT: {
		assert(instr->type.t == IR_INT);
		assert(instr->u.arg.type.t == IR_INT);

		emit_instr2(builder,
				MOVSX,
				asm_vreg(next_vreg(builder), instr->type.u.bit_width),
				asm_value(builder, instr->u.arg));
		assign_vreg(builder, instr);
		break;
	}
	case OP_TRUNC: {
		assert(instr->type.t == IR_INT);
		assert(instr->u.arg.type.t == IR_INT);

		AsmValue value = asm_value(builder, instr->u.arg);

		// For register we can implicitly truncate by just using the smaller
		// version of the register.
		if (value.t == ASM_VALUE_REGISTER) {
			assert(value.u.reg.t == V_REG);
			instr->vreg_number = value.u.reg.u.vreg_number;
		} else {
			UNIMPLEMENTED;
		}

		break;
	}
	case OP_CALL: {
		u32 call_arity = instr->u.call.arity;

		CallSeq call_seq;

		bool gen_new_call_seq;
		if (instr->u.call.callee.t == VALUE_GLOBAL) {
			AsmGlobal *target = instr->u.call.callee.u.global->asm_global;
			assert(target->t == ASM_GLOBAL_FUNCTION);

			IrType callee_type = instr->u.call.callee.u.global->type;
			assert(callee_type.t == IR_FUNCTION);

			u32 callee_arity = callee_type.u.function.arity;
			assert(call_arity == callee_arity
					|| (call_arity > callee_arity
						&& callee_type.u.function.variable_arity));

			if (!callee_type.u.function.variable_arity) {
				gen_new_call_seq = false;
				call_seq = target->u.function.call_seq;
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
		// making sure the size of our stack frame is a multiple of 16. We
		// calculate the size of our stack frame as follows:
		// * Return address pushed by call (8 bytes)
		// * rbp pushed by us (8 bytes)
		// * Space for our locals
		// * Space for the arguments we're about to pass
		// See the System V x86-64 ABI spec, section 3.2.2
		u32 stack_frame_size =
			8 + 8 + builder->local_stack_usage + args_stack_space;
		u32 stack_align_padding =
			align_to(stack_frame_size, 16) - stack_frame_size;

		u32 args_plus_padding = args_stack_space + stack_align_padding;
		if (args_plus_padding > 0) {
			emit_instr2(builder, SUB, asm_phys_reg(REG_CLASS_SP, 64),
					asm_const(args_plus_padding));
		}

		builder->curr_sp_diff = args_plus_padding;

		Array(u32) arg_vregs;
		ARRAY_INIT(&arg_vregs, u32, STATIC_ARRAY_LENGTH(argument_registers));

		for (u32 i = 0; i < call_arity; i++) {
			AsmValue arg = asm_value(builder, instr->u.call.arg_array[i]);
			// Use the 64-bit version of the register, as all argument
			// registers are 64-bit.
			if (arg.t == ASM_VALUE_REGISTER)
				arg.u.reg.width = 64;

			ArgClass *arg_class = call_seq.arg_classes + i;

			switch (arg_class->t) {
			case ARG_CLASS_REG: {
				AsmValue arg_target_reg =
					pre_alloced_vreg(builder, arg_class->u.reg.reg, 64);
				emit_instr2(builder, MOV, arg_target_reg, arg);
				*ARRAY_APPEND(&arg_vregs, u32) = arg_target_reg.u.reg.u.vreg_number;
				break;
			}
			case ARG_CLASS_MEM: {
				AsmValue location = asm_const(arg_class->u.mem.offset);
				if (arg_class->u.mem.size <= 8) {
					emit_instr2(builder,
							MOV, 
							asm_deref(asm_offset_reg(REG_CLASS_SP, 64,
									location.u.constant)),
							arg);
				} else {
					u32 src_vreg = next_vreg(builder);
					append_vreg(builder);
					u32 temp_vreg = next_vreg(builder);
					append_vreg(builder);
					emit_instr2(builder, MOV, asm_vreg(src_vreg, 64), arg);
					for (u32 i = 0; i < arg_class->u.mem.size; i++) {
						AsmConst offset =
							asm_const(location.u.constant.u.immediate + i).u.constant;
						emit_instr2(builder, MOV, asm_vreg(temp_vreg, 8),
								asm_deref(asm_vreg(src_vreg, 64)));
						emit_instr2(builder,
								MOV,
								asm_deref(asm_offset_reg(REG_CLASS_SP, 64, offset)),
								asm_vreg(temp_vreg, 8));
						emit_instr2(builder,
								ADD,
								asm_vreg(src_vreg, 64),
								asm_const(1));
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
		//
		// @TODO: Once we pass args in vector registers, set al appropriately.
		if (gen_new_call_seq) {
			emit_instr2(builder, MOV,
					pre_alloced_vreg(builder, REG_CLASS_A, 8), asm_const(0));

			// Done with call_seq now
			free(call_seq.arg_classes);
		}

		emit_instr1(builder, CALL, asm_value(builder, instr->u.call.callee));

		// Insert fake uses at the call instr, so that we don't allocate
		// registers used for arguments for any temporary registers used to
		// copy structs to the stack.
		for (u32 i = 0; i < arg_vregs.size; i++) {
			VRegInfo *vreg = ARRAY_REF(&builder->virtual_registers, VRegInfo, i);
			vreg->live_range_end = builder->current_block->size;
		}

		array_free(&arg_vregs);

		if (args_plus_padding > 0) {
			emit_instr2(builder, ADD, asm_phys_reg(REG_CLASS_SP, 64),
					asm_const(args_plus_padding));
		}
		builder->curr_sp_diff = 0;

		if (instr->u.call.return_type.t != IR_VOID)
			assign_vreg(builder, instr)->assigned_register = REG_CLASS_A;
		break;
	}
	case OP_BIT_XOR: asm_gen_binary_instr(builder, instr, XOR); break;
	case OP_BIT_AND: asm_gen_binary_instr(builder, instr, AND); break;
	case OP_BIT_OR: asm_gen_binary_instr(builder, instr, OR); break;
	case OP_BIT_NOT: {
		assert(instr->type.t == IR_INT);
		u8 width = instr->type.u.bit_width;

		AsmValue arg = asm_value(builder, instr->u.arg);
		emit_instr2(builder, MOV, asm_vreg(next_vreg(builder), width), arg);
		emit_instr1(builder, NOT, asm_vreg(next_vreg(builder), width));

		assign_vreg(builder, instr);
		break;
	}
	case OP_LOG_NOT: {
		assert(instr->type.t == IR_INT);
		u8 width = instr->type.u.bit_width;

		AsmValue arg = asm_value(builder, instr->u.arg);
		u32 vreg = next_vreg(builder);
		emit_instr2(builder, XOR, asm_vreg(vreg, width), asm_vreg(vreg, width));
		emit_instr2(builder, TEST, arg, arg);
		emit_instr1(builder, SETE, asm_vreg(next_vreg(builder), 8));

		assign_vreg(builder, instr);
		break;
	}
	// @TODO: This doesn't work in all cases because SHL is weird and can't
	// take an arbitrary register as the rhs.
	case OP_SHL: asm_gen_binary_instr(builder, instr, SHL); break;
	case OP_ADD: asm_gen_binary_instr(builder, instr, ADD); break;
	case OP_SUB: asm_gen_binary_instr(builder, instr, SUB); break;
	case OP_MUL: {
		assert(instr->type.t == IR_INT);
		u8 width = instr->type.u.bit_width;

		AsmValue arg1 = asm_value(builder, instr->u.binary_op.arg1);
		AsmValue arg2 = asm_value(builder, instr->u.binary_op.arg2);

		if (arg1.t != ASM_VALUE_CONST && arg2.t != ASM_VALUE_CONST) {
			emit_instr2(builder, MOV, asm_vreg(next_vreg(builder), width), arg1);
			emit_instr2(builder, IMUL, asm_vreg(next_vreg(builder), width), arg2);
		} else {
			AsmValue const_arg;
			AsmValue non_const_arg;
			if (arg1.t == ASM_VALUE_CONST) {
				const_arg = arg1;
				non_const_arg = arg2;
			} else {
				const_arg = arg2;
				non_const_arg = arg1;
			}

			assert(non_const_arg.t != ASM_VALUE_CONST);

			emit_instr3(builder, IMUL,
					asm_vreg(next_vreg(builder), width),
					non_const_arg,
					const_arg);
		}

		assign_vreg(builder, instr);
		break;
	}
	case OP_DIV: {
		assert(instr->type.t == IR_INT);
		u8 width = instr->type.u.bit_width;

		AsmValue arg1 = asm_value(builder, instr->u.binary_op.arg1);
		AsmValue arg2 = asm_value(builder, instr->u.binary_op.arg2);

		AsmValue reg_arg2 = arg2;
		if (arg2.t == ASM_VALUE_CONST) {
			reg_arg2 = asm_vreg(next_vreg(builder), width);
			emit_instr2(builder, MOV, reg_arg2, arg2);
			append_vreg(builder);
		}

		AsmValue quotient = pre_alloced_vreg(builder, REG_CLASS_A, width);
		instr->vreg_number = quotient.u.reg.u.vreg_number;
		emit_instr2(builder, MOV, quotient, arg1);

		AsmValue sign_extension = pre_alloced_vreg(builder, REG_CLASS_D, width);
		switch (width) {
		case 32: {
			AsmInstr *cdq = emit_instr0(builder, CDQ);
			add_dep(cdq, quotient);
			add_dep(cdq, sign_extension);
			break;
		}
		default: UNIMPLEMENTED;
		}

		AsmInstr *idiv = emit_instr1(builder, IDIV, reg_arg2);
		add_dep(idiv, sign_extension);
		break;
	}
	case OP_EQ: asm_gen_relational_instr(builder, instr, SETE); break;
	case OP_NEQ: asm_gen_relational_instr(builder, instr, SETNE); break;
	case OP_GT: asm_gen_relational_instr(builder, instr, SETG); break;
	case OP_GTE: asm_gen_relational_instr(builder, instr, SETGE); break;
	case OP_LT: asm_gen_relational_instr(builder, instr, SETL); break;
	case OP_LTE: asm_gen_relational_instr(builder, instr, SETLE); break;
	case OP_BUILTIN_VA_START: {
		AsmValue va_list_ptr = asm_vreg(next_vreg(builder), 64);
		append_vreg(builder);
		emit_instr2(builder, MOV, va_list_ptr, asm_value(builder, instr->u.arg));

		// Initialise as specified in System V x86-64 section 3.5.7

		// We haven't consumed any args yet, so register_save_area +
		// next_int_reg_offset should point to the start of the register save
		// area. However, we need to make sure that next_int_reg_offset is 48
		// when we run out of registers in the register save area, because
		// that's what the ABI says. So, we set next_int_reg_offset to 48 minus
		// the size of the register save area, and subtract the same amount
		// from register_save_area. This way we start out pointing at the
		// correct place, but still end up at 48 when we're out of space.
		AsmValue starting_offset = asm_const(48 - builder->register_save_area_size);

		emit_instr2(builder, MOV,
				asm_deref(va_list_ptr), starting_offset);

		// Skip next_vector_reg_offset since we don't use vector registers for
		// passing arguments yet.

		emit_instr2(builder, ADD, va_list_ptr, asm_const(8));

		// Stack args start at the bottom of the previous stack frame, which is
		// always rbp + 16
		emit_instr2(builder,
				MOV,
				asm_deref(va_list_ptr),
				asm_phys_reg(REG_CLASS_BP, 64));
		emit_instr2(builder, ADD, asm_deref(va_list_ptr), asm_const(16));

		emit_instr2(builder, ADD, va_list_ptr, asm_const(8));

		// The register save area is always at the bottom of our stack frame.
		// However, as mentioned above, we need to offset this value for ABI
		// reasons.
		AsmValue register_save_area = asm_vreg(next_vreg(builder), 64);
		append_vreg(builder);

		emit_instr2(builder,
				MOV,
				register_save_area,
				asm_phys_reg(REG_CLASS_SP, 64));
		emit_instr2(builder, SUB, register_save_area, starting_offset);
		emit_instr2(builder,
				MOV,
				asm_deref(va_list_ptr),
				register_save_area);

		break;
	}
	case OP_BUILTIN_VA_ARG: {
		AsmValue va_list_ptr = asm_value(builder, instr->u.binary_op.arg1);
		assert(instr->u.binary_op.arg2.t == VALUE_CONST);
		u64 size = instr->u.binary_op.arg2.u.constant;
		assert(size <= 8);

		u32 vreg1 = next_vreg(builder);
		AsmValue vreg_32 = asm_vreg(vreg1, 32);
		AsmValue vreg_64 = asm_vreg(vreg1, 64);
		assign_vreg(builder, instr);

		emit_instr2(builder, XOR, vreg_64, vreg_64);
		emit_instr2(builder, MOV, vreg_32, asm_deref(va_list_ptr));
		emit_instr2(builder, ADD, asm_deref(va_list_ptr), asm_const(size));
		emit_instr2(builder, ADD, vreg_64, asm_phys_reg(REG_CLASS_BP, 64));
		break;
	}
	}
}

static Register *arg_reg(AsmValue *arg)
{
	if (arg->t == ASM_VALUE_REGISTER)
		return &arg->u.reg;
	if (arg->t == ASM_VALUE_OFFSET_REGISTER)
		return &arg->u.offset_register.reg;
	return NULL;
}

#define ALLOCATION_ORDER \
	X(0, REG_CLASS_R12), \
	X(1, REG_CLASS_R13), \
	X(2, REG_CLASS_R14), \
	X(3, REG_CLASS_R15), \
	X(4, REG_CLASS_B), \
	X(5, REG_CLASS_R11), \
	X(6, REG_CLASS_R10), \
	X(7, REG_CLASS_R9), \
	X(8, REG_CLASS_R8), \
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
		for (u32 j = 0; j < instr->arity; j++) {
			AsmValue *arg = instr->args + j;
			Register *reg = arg_reg(arg);
			if (reg != NULL && reg->t == V_REG) {
				u32 reg_num = reg->u.vreg_number;
				VRegInfo *vreg = ARRAY_REF(&builder->virtual_registers,
						VRegInfo, reg_num);
				if (vreg->live_range_start == -1) {
					vreg->live_range_start = vreg->live_range_end = i;
				} else {
					vreg->live_range_end = i;
				}
			}
		}

		for (u32 j = 0; j < instr->num_deps; j++) {
			u32 reg_num = instr->vreg_deps[j];
			VRegInfo *vreg = ARRAY_REF(&builder->virtual_registers,
					VRegInfo, reg_num);
			if (vreg->live_range_start == -1) {
				vreg->live_range_start = vreg->live_range_end = i;
			} else {
				vreg->live_range_end = i;
			}
		}
	}

	if (flag_dump_live_ranges) {
		dump_asm_function(builder->current_function);
		for (u32 i = 0; i < builder->virtual_registers.size; i++) {
			VRegInfo *vreg = ARRAY_REF(&builder->virtual_registers, VRegInfo, i);
			printf("#%u: [%d, %d]", i, vreg->live_range_start, vreg->live_range_end);
			if (vreg->assigned_register != INVALID_REG_CLASS) {
				// @TODO: Move register dumping stuff we we can dump the name
				// here rather than just a number
				printf(" (%d)", vreg->assigned_register);
			}
			putchar('\n');
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

		if (vreg->assigned_register == INVALID_REG_CLASS) {
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
		if (instr->op == CALL) {
			CallSite *callsite = ARRAY_APPEND(&callsites, CallSite);
			callsite->instr_index = i;
			callsite->active_caller_save_regs_bitset =
				live_regs_bitset & CALLER_SAVE_REGS_BITMASK;
			total_regs_to_save += bit_count(callsite->active_caller_save_regs_bitset);
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

		for (u32 j = 0; j < instr->arity; j++) {
			AsmValue *arg = instr->args + j;
			Register *reg = arg_reg(arg);
			if (reg == NULL)
				continue;

			if (reg->t == V_REG) {
				u32 vreg_number = reg->u.vreg_number;
				VRegInfo *vreg = ARRAY_REF(&builder->virtual_registers,
						VRegInfo, vreg_number);
				RegClass class = vreg->assigned_register;

				reg->t = PHYS_REG;
				reg->u.class = class;
			}
		}
	}
}

void asm_gen_function(AsmBuilder *builder, IrGlobal *ir_global)
{
	assert(ir_global->type.t == IR_FUNCTION);
	IrFunction *ir_func = &ir_global->initializer->u.function;

	AsmGlobal *asm_func = ir_global->asm_global;
	assert(asm_func != NULL);

	asm_func->defined = ir_global->initializer != NULL;
	if (!asm_func->defined)
		return;

	builder->current_function = &asm_func->u.function;
	builder->current_block = &asm_func->u.function.body;

	builder->local_stack_usage = 0;

	AsmLabel *ret_label = pool_alloc(&builder->asm_module.pool, sizeof *ret_label);
	ret_label->name = "ret";
	ret_label->offset = 0;
	asm_func->u.function.ret_label = ret_label;

	if (ARRAY_IS_VALID(&builder->virtual_registers))
		array_free(&builder->virtual_registers);
	ARRAY_INIT(&builder->virtual_registers, VRegInfo, 20);
	IrType return_type = *ir_global->type.u.function.return_type;
	assert(return_type.t == IR_INT
			|| return_type.t == IR_POINTER
			|| return_type.t == IR_VOID);

	for (u32 block_index = 0; block_index < ir_func->blocks.size; block_index++) {
		IrBlock *block = *ARRAY_REF(&ir_func->blocks, IrBlock *, block_index);
		AsmLabel *label = append_label(builder, block->name);
		block->label = label;
	}

	// Pre-allocate virtual registers for argument registers. This is to avoid
	// spills if this isn't a leaf function, since we'll need to use the same
	// registers for our own calls.
	// In the same loop we figure out how much space we need for our register
	// save area if this is a varargs function. It defaults to 48 (6 registers
	// that are 8 bytes each), but we omit any that were already used for
	// arguments before the "..."
	u32 register_save_area_size = 48;
	for (u32 i = 0; i < ir_global->type.u.function.arity; i++) {
		ArgClass *arg_class = asm_func->u.function.call_seq.arg_classes + i;
		if (arg_class->t == ARG_CLASS_REG) {
			emit_instr2(builder, MOV, asm_vreg(next_vreg(builder), 64),
					asm_phys_reg(arg_class->u.reg.reg, 64));
			arg_class->u.reg.vreg = next_vreg(builder);
			append_vreg(builder);

			register_save_area_size -= 8;
		}
	}

	// If this is a varargs function, add space for the register save area.
	// Since we add this before compiling the body of the function, the
	// register save area is always at the bottom of the stack frame, as long
	// as we're not in the process of calling a function. We don't care about
	// that case though, because we only need the location for va_start.
	if (ir_global->type.u.function.variable_arity) {
		builder->local_stack_usage += register_save_area_size;
		builder->register_save_area_size = register_save_area_size;
	}

	for (u32 block_index = 0; block_index < ir_func->blocks.size; block_index++) {
		IrBlock *block = *ARRAY_REF(&ir_func->blocks, IrBlock *, block_index);

		u32 first_instr_of_block_index = block_index == 0
			? 0 : builder->current_function->body.size;
		Array(IrInstr *) *instrs = &block->instrs;
		for (u32 i = 0; i < instrs->size; i++) {
			IrInstr *instr = *ARRAY_REF(instrs, IrInstr *, i);
			asm_gen_instr(builder, ir_global, block, instr);
		}

		AsmInstr *first_instr_of_block = ARRAY_REF(
				&builder->current_function->body,
				AsmInstr,
				first_instr_of_block_index);
		first_instr_of_block->label = block->label;
	}

	allocate_registers(builder);

	u32 used_callee_save_regs_bitset = 0;
	for (u32 i = 0; i < builder->current_function->body.size; i++) {
		AsmInstr *instr = ARRAY_REF(&builder->current_function->body, AsmInstr, i);
		for (u32 j = 0; j < instr->arity; j++) {
			Register *reg = arg_reg(instr->args + j);
			if (reg != NULL &&
					reg->t == PHYS_REG &&
					is_callee_save(reg->u.class)) {
				used_callee_save_regs_bitset |= 1 << reg->u.class;
			}
		}
	}

	builder->current_block = &builder->current_function->prologue;

	AsmLabel *entry_label = pool_alloc(&builder->asm_module.pool, sizeof *entry_label);
	entry_label->name = ir_global->name;
	entry_label->offset = 0;
	ir_func->label = entry_label;


	emit_instr1(builder, PUSH, asm_phys_reg(REG_CLASS_BP, 64));
	emit_instr2(builder,
			MOV,
			asm_phys_reg(REG_CLASS_BP, 64),
			asm_phys_reg(REG_CLASS_SP, 64));
	u32 temp_regs_bitset = used_callee_save_regs_bitset;
	while (temp_regs_bitset != 0) {
		RegClass reg = lowest_set_bit(temp_regs_bitset);
		emit_instr1(builder, PUSH, asm_phys_reg(reg, 64));
		temp_regs_bitset &= ~(1 << reg);
	}

	if (builder->local_stack_usage != 0) {
		emit_instr2(builder,
				SUB,
				asm_phys_reg(REG_CLASS_SP, 64),
				asm_const(builder->local_stack_usage));
	}

	// Argument registers are stored in increasing order in the register save
	// area. We only bother storing the registers that could be used to pass
	// varargs arguments. e.g.: for "void foo(int a, int b, ...)" we don't save
	// rdi or rsi.
	// See the System V x86-64 ABI spec, figure 3.33
	if (ir_global->type.u.function.variable_arity) {
		u32 num_reg_args = 0;
		for (u32 i = 0; i < ir_global->type.u.function.arity; i++) {
			ArgClass *arg_class = asm_func->u.function.call_seq.arg_classes + i;

			if (arg_class->t == ARG_CLASS_REG) {
				num_reg_args++;
			}
		}

		for (u32 i = num_reg_args;
				i < STATIC_ARRAY_LENGTH(argument_registers);
				i++) {
			AsmConst offset = asm_const((i - num_reg_args) * 8).u.constant;
			emit_instr2(builder,
					MOV,
					asm_deref(asm_offset_reg(REG_CLASS_SP, 64, offset)),
					asm_phys_reg(argument_registers[i], 64));
		}
	}

	AsmInstr *prologue_first_instr = ARRAY_REF(builder->current_block, AsmInstr, 0);
	prologue_first_instr->label = entry_label;


	builder->current_block = &builder->current_function->epilogue;

	if (builder->local_stack_usage != 0) {
		emit_instr2(builder,
				ADD,
				asm_phys_reg(REG_CLASS_SP, 64),
				asm_const(builder->local_stack_usage));
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

	AsmInstr *epilogue_first_instr = ARRAY_REF(builder->current_block, AsmInstr, 0);
	epilogue_first_instr->label = ret_label;
}

static void write_const(IrConst *konst, u8 *value)
{
	switch (konst->type.t) {
	case IR_INT: case IR_POINTER:
		for (u32 n = 0; n < size_of_ir_type(konst->type); n++) {
			u8 byte = (konst->u.integer >> (n * 8)) & 0xFF;
			*value++ = byte;
		}
		break;
	case IR_ARRAY:
		for (u32 n = 0; n < konst->type.u.array.size; n++) {
			write_const(konst->u.array_elems + n, value);
			value += size_of_ir_type(*konst->type.u.array.elem_type);
		}
		break;
	case IR_STRUCT:
		for (u32 n = 0; n < konst->type.u.strukt.num_fields; n++) {
			write_const(konst->u.struct_fields + n, value);
			value += size_of_ir_type(konst->type.u.strukt.fields[n].type);
		}
		break;
	case IR_FUNCTION: case IR_VOID:
		UNREACHABLE;
	}
}

void generate_asm_module(AsmBuilder *builder, TransUnit *trans_unit)
{
	// First do a pass collecting declarations, to support forward references.
	for (u32 i = 0; i < trans_unit->globals.size; i++) {
		IrGlobal *ir_global = *ARRAY_REF(&trans_unit->globals, IrGlobal *, i);
		AsmGlobal *asm_global =
			pool_alloc(&builder->asm_module.pool, sizeof *asm_global);
		*ARRAY_APPEND(&builder->asm_module.globals, AsmGlobal *) = asm_global;

		ir_global->asm_global = asm_global;

		u32 name_len = strlen(ir_global->name);
		char *name_copy = pool_alloc(&builder->asm_module.pool, name_len + 1);
		memcpy(name_copy, ir_global->name, name_len);
		name_copy[name_len] = '\0';

		asm_global->name = name_copy;
		asm_global->defined = ir_global->initializer != NULL;
		asm_global->offset = 0;

		switch (ir_global->linkage) {
		case IR_GLOBAL_LINKAGE: asm_global->linkage = ASM_GLOBAL_LINKAGE; break;
		case IR_LOCAL_LINKAGE: asm_global->linkage = ASM_LOCAL_LINKAGE; break;
		}

		if (ir_global->type.t == IR_FUNCTION) {
			AsmFunction *new_function = &asm_global->u.function;
			u32 arity = ir_global->type.u.function.arity;
			IrType *arg_types = ir_global->type.u.function.arg_types;

			asm_global->t = ASM_GLOBAL_FUNCTION;
			init_asm_function(new_function, ir_global->name);

			// Note that we do this even for varargs functions where the
			// CallSeq needs to be computed per-call. This is so that when
			// compiling the function itself we have info on the args before
			// the ..., e.g.: for register allocation.
			new_function->call_seq = classify_arguments(arity, arg_types);
		} else {
			asm_global->t = ASM_GLOBAL_VAR;
			asm_global->u.var.size_bytes = size_of_ir_type(ir_global->type);
			asm_global->u.var.value = NULL;
		}
	}

	for (u32 i = 0; i < trans_unit->globals.size; i++) {
		IrGlobal *ir_global = *ARRAY_REF(&trans_unit->globals, IrGlobal *, i);
		if (ir_global->type.t == IR_FUNCTION) {
			asm_gen_function(builder, ir_global);
		} else {
			IrConst *konst = ir_global->initializer;
			if (konst != NULL) {
				AsmGlobal *asm_global = ir_global->asm_global;
				u32 size = asm_global->u.var.size_bytes;
				asm_global->u.var.value =
					pool_alloc(&builder->asm_module.pool, size);
				write_const(konst, asm_global->u.var.value);
			}
		}
	}
}
