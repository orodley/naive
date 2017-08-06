#include <assert.h>
#include <stdlib.h>

#include "array.h"
#include "bit_set.h"
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

static void add_dep(AsmInstr *instr, AsmValue dep)
{
	assert(instr->num_deps < STATIC_ARRAY_LENGTH(instr->vreg_deps));
	assert(dep.t == ASM_VALUE_REGISTER);
	assert(dep.u.reg.t == V_REG);

	instr->vreg_deps[instr->num_deps++] = dep.u.reg.u.vreg_number;
}

static u32 new_vreg(AsmBuilder *builder)
{
	VReg *vreg = ARRAY_APPEND(&builder->virtual_registers, VReg);
	vreg->t = UNASSIGNED;
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
	assert(vreg.t == ASM_VALUE_REGISTER);
	assert(vreg.u.reg.t == V_REG);

	instr->vreg_number = vreg.u.reg.u.vreg_number;
	return vreg;
}

static AsmValue pre_alloced_vreg(AsmBuilder *builder, RegClass class, u8 width)
{
	u32 vreg_number = new_vreg(builder);

	VReg *vreg = ARRAY_LAST(&builder->virtual_registers, VReg);
	vreg->t = IN_REG;
	vreg->u.assigned_register = class;
	vreg->pre_alloced = true;

	return asm_vreg(vreg_number, width);
}

static AsmValue asm_gen_relational_instr(AsmBuilder *builder, IrInstr *instr);

static AsmValue asm_value(AsmBuilder *builder, IrValue value)
{
	switch (value.t) {
	case VALUE_CONST:
		return asm_imm(value.u.constant);
	case VALUE_INSTR: {
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
			emit_instr2(builder, MOV, vreg, asm_phys_reg(REG_CLASS_SP, 64));
			emit_instr2(builder,
					ADD,
					vreg,
					asm_imm(instr->u.local.stack_offset + builder->curr_sp_diff));
			return vreg;
		}
		// @TODO: Call asm_gen_pointer_instr and use LEA.
		case OP_FIELD: {
			IrValue ptr = instr->u.field.ptr;
			IrType type = instr->u.field.type;
			u32 field_num = instr->u.field.field_number;
			assert(ptr.type.t == IR_POINTER);

			AsmValue temp_vreg = asm_vreg(new_vreg(builder), 64);
			assign_vreg(instr, temp_vreg);

			u32 offset;
			switch (type.t) {
			case IR_STRUCT:
				offset = type.u.strukt.fields[field_num].offset;
				break;
			case IR_ARRAY:
				offset = size_of_ir_type(*type.u.array.elem_type) * field_num;
				break;
			default:
				UNREACHABLE;
			}
			emit_instr2(builder, MOV, temp_vreg, asm_value(builder, ptr));
			emit_instr2(builder, ADD, temp_vreg, asm_imm(offset));

			return temp_vreg;
		}
		// @NOTE: Handled specially for a similar reason to OP_FIELD and
		// OP_LOCAL above. These instructions are mostly used as the argument
		// to OP_COND, so we let OP_COND match them itself in those cases.
		case OP_EQ: case OP_NEQ: case OP_GT: case OP_GTE: case OP_LT:
		case OP_LTE:
			return asm_gen_relational_instr(builder, instr);
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
			AsmValue bp_offset = asm_imm(arg_class->u.mem.offset + 16);

			u32 vreg = new_vreg(builder);
			AsmValue vreg64 = asm_vreg(vreg, 64);
			if (arg_class->u.mem.remains_in_memory) {
				emit_instr2(builder,
						MOV,
						vreg64,
						asm_phys_reg(REG_CLASS_BP, 64));
				emit_instr2(builder, ADD, vreg64, bp_offset);
				return vreg64;
			} else {
				emit_instr2(builder,
						MOV,
						vreg64,
						asm_deref(asm_offset_reg(REG_CLASS_BP, 64,
								bp_offset.u.constant)));
				return asm_vreg(vreg, size_of_ir_type(value.type) * 8);
			}
		}
		}

		UNREACHABLE;
	}
	case VALUE_GLOBAL: {
		AsmSymbol *symbol = value.u.global->asm_symbol;
		assert(symbol != NULL);
		return asm_symbol(symbol);
	}
	}
}

static AsmValue maybe_move_const_to_reg(AsmBuilder *builder,
		AsmValue asm_value, u32 other_operand_width, bool sext)
{
	if (asm_value.t == ASM_VALUE_CONST
			&& asm_value.u.constant.t == ASM_CONST_IMMEDIATE) {
		u64 c = asm_value.u.constant.u.immediate;

		if (!sext) {
			if ((u32)c == c)
				return asm_value;
		} else if ((i32)c == (i64)c) {
			return asm_value;
		}

		// Generally there's no imm64, so we have to spill the immediate
		// to a register here.
		AsmValue const_spill = asm_vreg(new_vreg(builder), 64);

		emit_instr2(builder, MOV, const_spill, asm_value);

		const_spill.u.reg.width = other_operand_width;
		return const_spill;
	}

	return asm_value;
}

static IrOp maybe_flip_conditional(IrOp op, IrValue *arg1, IrValue *arg2)
{
	if (arg1->t != VALUE_CONST)
		return op;

	// The only form of comparison between a register and an immediate has the
	// immediate on the RHS. So we need to swap the LHS and RHS if the
	// immediate is on the LHS.
	IrOp flipped;
	switch (op) {
	// Antisymmetric relations.
	case OP_GT: flipped = OP_LT; break;
	case OP_LT: flipped = OP_GT; break;
	case OP_GTE: flipped = OP_LTE; break;
	case OP_LTE: flipped = OP_GTE; break;
	// Symmetric relations.
	case OP_EQ: flipped = OP_EQ; break;
	case OP_NEQ: flipped = OP_NEQ; break;
	default: UNREACHABLE;
	}

	IrValue temp = *arg1;
	*arg1 = *arg2;
	*arg2 = temp;

	return flipped;
}

// @TODO: This should probably be done on the IR level instead.
static bool get_inner_cmp(AsmBuilder *builder, IrInstr *cmp, IrInstr **out)
{
	if (cmp->op != OP_EQ && cmp->op != OP_NEQ) {
		*out = cmp;
		return false;
	}

	IrValue arg1 = cmp->u.binary_op.arg1;
	IrValue arg2 = cmp->u.binary_op.arg2;

	u64 c;
	IrValue non_const_arg;
	if (arg1.t == VALUE_CONST) {
		assert(arg2.t != VALUE_CONST);
		c = arg1.u.constant;
		non_const_arg = arg2;
	} else if (arg2.t == VALUE_CONST) {
		assert(arg1.t != VALUE_CONST);
		c = arg2.u.constant;
		non_const_arg = arg1;
	} else {
		*out = cmp;
		return false;
	}

	if (non_const_arg.t != VALUE_INSTR || (c != 0 && c != 1)) {
		*out = cmp;
		return false;
	}

	IrInstr *inner = non_const_arg.u.instr;

	switch (inner->op) {
	case OP_EQ: case OP_NEQ: case OP_GT: case OP_GTE: case OP_LT: case OP_LTE:
		break;
	default:
		*out = cmp;
		return false;
	}

	if (cmp->op == OP_EQ && c == 0)
		return !get_inner_cmp(builder, inner, out);
	if (cmp->op == OP_EQ && c == 1)
		return get_inner_cmp(builder, inner, out);
	if (cmp->op == OP_NEQ && c == 0)
		return get_inner_cmp(builder, inner, out);
	if (cmp->op == OP_NEQ && c == 1)
		return !get_inner_cmp(builder, inner, out);

	UNREACHABLE;
}

static IrOp invert_cmp_op(IrOp ir_op)
{
	switch (ir_op) {
	case OP_EQ: return OP_NEQ;
	case OP_NEQ: return OP_EQ;
	case OP_GT: return OP_LTE;
	case OP_GTE: return OP_LT;
	case OP_LT: return OP_GTE;
	case OP_LTE: return OP_GT;
	default: return ir_op;
	}
}

static AsmValue asm_gen_relational_instr(AsmBuilder *builder, IrInstr *instr)
{
	bool invert = get_inner_cmp(builder, instr, &instr);

	IrValue arg1 = instr->u.binary_op.arg1;
	IrValue arg2 = instr->u.binary_op.arg2;
	IrOp ir_op = maybe_flip_conditional(instr->op, &arg1, &arg2);

	if (invert)
		ir_op = invert_cmp_op(ir_op);

	AsmOp op;
	switch (ir_op) {
	case OP_EQ: op = SETE; break;
	case OP_NEQ: op = SETNE; break;
	case OP_GT: op = SETG; break;
	case OP_GTE: op = SETGE; break;
	case OP_LT: op = SETL; break;
	case OP_LTE: op = SETLE; break;
	default: UNREACHABLE;
	}

	AsmValue asm_arg1 = asm_value(builder, arg1);
	AsmValue asm_arg2 = asm_value(builder, arg2);

	u32 vreg = new_vreg(builder);

	assert(asm_arg1.t == ASM_VALUE_REGISTER);

	emit_instr2(builder, XOR, asm_vreg(vreg, 32), asm_vreg(vreg, 32));
	emit_instr2(builder, CMP, asm_arg1,
			maybe_move_const_to_reg(builder, asm_arg2, asm_arg1.u.reg.width, true));
	emit_instr1(builder, op, asm_vreg(vreg, 8));

	return asm_vreg(vreg, 32);
}

static void asm_gen_binary_instr(AsmBuilder *builder, IrInstr *instr, AsmOp op)
{
	assert(instr->type.t == IR_INT);

	AsmValue arg1 = asm_value(builder, instr->u.binary_op.arg1);
	AsmValue arg2 = asm_value(builder, instr->u.binary_op.arg2);

	AsmValue target = asm_vreg(new_vreg(builder), instr->type.u.bit_width);
	assign_vreg(instr, target);

	emit_instr2(builder, MOV, target, arg1);
	emit_instr2(builder, op, target, maybe_move_const_to_reg(builder,
				arg2, instr->type.u.bit_width, is_sign_extending_op(op)));
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
			instr->vreg_number = new_vreg(builder);
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

static AsmValue asm_gen_pointer_instr(AsmBuilder *builder, IrValue pointer)
{
	switch (pointer.t) {
	case VALUE_INSTR: {
		IrInstr *instr = pointer.u.instr;
		switch (instr->op) {
		case OP_LOCAL: {
			u32 offset = instr->u.local.stack_offset + builder->curr_sp_diff;
			return asm_offset_reg(REG_CLASS_SP, 64, asm_const_imm(offset));
		}
		case OP_FIELD: {
			AsmValue inner =
				asm_gen_pointer_instr(builder, instr->u.field.ptr);
			IrType type = instr->u.field.type;
			u32 field_num = instr->u.field.field_number;

			u32 offset;
			switch (type.t) {
			case IR_STRUCT:
				offset = type.u.strukt.fields[field_num].offset;
				break;
			case IR_ARRAY:
				offset = size_of_ir_type(*type.u.array.elem_type) * field_num;
				break;
			default:
				UNREACHABLE;
			}

			if (offset == 0)
				return inner;

			switch (inner.t) {
			case ASM_VALUE_REGISTER:
				return (AsmValue) {
					.t = ASM_VALUE_OFFSET_REGISTER,
					.u.offset_register.reg = inner.u.reg,
					.u.offset_register.offset = asm_const_imm(offset),
				};
			case ASM_VALUE_OFFSET_REGISTER: {
				AsmConst reg_offset = inner.u.offset_register.offset;
				switch (reg_offset.t) {
				case ASM_CONST_IMMEDIATE:
					return (AsmValue) {
						.t = ASM_VALUE_OFFSET_REGISTER,
						.u.offset_register.reg = inner.u.offset_register.reg,
						.u.offset_register.offset = asm_const_imm(
								reg_offset.u.immediate + offset),
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

					emit_instr2(builder, MOV,
							temp_vreg, asm_symbol(reg_offset.u.symbol));
					return (AsmValue) {
						.t = ASM_VALUE_OFFSET_REGISTER,
						.u.offset_register.reg = temp_vreg.u.reg,
						.u.offset_register.offset = asm_const_imm(offset),
					};
				}
				case ASM_CONST_FIXED_IMMEDIATE:
					UNREACHABLE;
				}
				break;
			}
			default:
				break;
			}

			// Fall back to computing the offset using ADD in cases where we
			// can't use an offset register.
			AsmValue temp_vreg = asm_vreg(new_vreg(builder), 64);
			assign_vreg(instr, temp_vreg);

			emit_instr2(builder, MOV, temp_vreg, inner);
			emit_instr2(builder, ADD, temp_vreg, asm_imm(offset));

			return asm_value(builder, pointer);
		}
		// @TODO: Handle OP_ADD and OP_SUB. This is a bit trickier because I
		// think we have to reach through casts?
		default:
			return asm_value(builder, pointer);
		}
	}
	case VALUE_GLOBAL: {
		return asm_offset_reg(REG_CLASS_IP, 64,
				asm_const_symbol(pointer.u.global->asm_symbol));
	}
	// @TODO: Handle globals. This is essentially the same as OP_LOCAL, but
	// RIP-relative rather than RSP-relative.
	default:
		return asm_value(builder, pointer);
	}
}

static bool asm_gen_cond_of_cmp(AsmBuilder *builder, IrInstr *cond)
{
	assert(cond->op == OP_COND);
	IrValue condition = cond->u.cond.condition;

	if (condition.t != VALUE_INSTR)
		return false;

	IrInstr *cond_instr = condition.u.instr;
	bool invert = get_inner_cmp(builder, cond_instr, &cond_instr);

	IrValue arg1 = cond_instr->u.binary_op.arg1;
	IrValue arg2 = cond_instr->u.binary_op.arg2;
	IrOp ir_op = maybe_flip_conditional(cond_instr->op, &arg1, &arg2);

	if (invert)
		ir_op = invert_cmp_op(ir_op);

	AsmOp jcc;
	switch (ir_op) {
	case OP_EQ: jcc = JE; break;
	case OP_NEQ: jcc = JNE; break;
	case OP_GT: jcc = JG; break;
	case OP_GTE: jcc = JGE; break;
	case OP_LT: jcc = JL; break;
	case OP_LTE: jcc = JLE; break;
	default: return false;
	}

	AsmValue arg2_value = maybe_move_const_to_reg(builder,
			asm_value(builder, arg2), size_of_ir_type(arg1.type) * 8, true);

	emit_instr2(builder, CMP, asm_value(builder, arg1), arg2_value);
	emit_instr1(builder, jcc, asm_symbol(cond->u.cond.then_block->label));
	// The "else" case is handled by the caller.

	return true;
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
	case OP_FIELD:
		// Don't do anything. OP_STORE and OP_LOAD handle args of type OP_FIELD
		// directly, and others uses are handled by asm_value.
		break;
	case OP_RET_VOID:
		emit_instr1(builder, JMP, asm_symbol(builder->ret_label));

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
		emit_instr1(builder, JMP, asm_symbol(builder->ret_label));

		break;
	}
	case OP_BRANCH:
		handle_phi_nodes(builder, curr_block, instr->u.target_block);
		emit_instr1(builder, JMP, asm_symbol(instr->u.target_block->label));
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
		if (instr->vreg_number == -1)
			instr->vreg_number = new_vreg(builder);

		break;
	}
	case OP_STORE: {
		IrValue ir_pointer = instr->u.binary_op.arg1;
		IrValue ir_value = instr->u.binary_op.arg2;
		IrType type = ir_value.type;
		u32 bit_width = size_of_ir_type(type) * 8;

		AsmValue value = asm_value(builder, ir_value);
		switch (value.t) {
		case ASM_VALUE_REGISTER:
			value.u.reg.width = bit_width;
			break;
		case ASM_VALUE_CONST: {
			// @TODO: Use maybe_move_const_to_reg instead?
			if (size_of_ir_type(type) == 8) {
				AsmValue temp_vreg = asm_vreg(new_vreg(builder), 64);
				emit_instr2(builder, MOV, temp_vreg, value);
				value = temp_vreg;

				break;
			}

			AsmConst konst = value.u.constant;
			switch (konst.t) {
			case ASM_CONST_SYMBOL: break;
			case ASM_CONST_FIXED_IMMEDIATE:
				assert(konst.u.fixed_immediate.width == bit_width);
				break;
			case ASM_CONST_IMMEDIATE:
				value.u.constant = asm_const_fixed_imm(konst.u.immediate, bit_width);
				break;
			}
			}
			break;
		case ASM_VALUE_OFFSET_REGISTER:
			UNREACHABLE;
		}

		AsmValue pointer = asm_gen_pointer_instr(builder, ir_pointer);

		if (ir_pointer.t == VALUE_GLOBAL) {
			AsmValue rip_relative_addr =
				asm_offset_reg(
						REG_CLASS_IP,
						64,
						asm_const_symbol(ir_pointer.u.global->asm_symbol));
			emit_instr2(builder, MOV, asm_deref(rip_relative_addr), value);
		} else {
			emit_instr2(builder, MOV, asm_deref(pointer), value);
		}

		break;
	}
	case OP_LOAD: {
		IrValue ir_pointer = instr->u.load.pointer;
		IrType type = instr->u.load.type;
		AsmValue target = asm_vreg(new_vreg(builder), size_of_ir_type(type) * 8);
		assign_vreg(instr, target);

		AsmValue pointer = asm_gen_pointer_instr(builder, ir_pointer);

		if (pointer.t == VALUE_GLOBAL) {
			AsmValue rip_relative_addr =
				asm_offset_reg(
						REG_CLASS_IP,
						64,
						asm_const_symbol(ir_pointer.u.global->asm_symbol));
			emit_instr2(builder,
					MOV,
					target,
					asm_deref(rip_relative_addr));
		} else {
			emit_instr2(builder, MOV, target, asm_deref(pointer));
		}

		break;
	}
	case OP_CAST: break; // cast is a type-theoretic operation only
	case OP_ZEXT: {
		assert(instr->type.t == IR_INT);
		assert(instr->u.arg.type.t == IR_INT);

		if (instr->type.u.bit_width == 64
				&& instr->u.arg.type.u.bit_width == 32) {
			AsmValue vreg = asm_vreg(new_vreg(builder), 32);
			assign_vreg(instr, vreg);

			// Implicit zero-extending of MOV 32 -> 32
			emit_instr2(builder, MOV, vreg, asm_value(builder, instr->u.arg));
		} else {
			AsmValue vreg = asm_vreg(new_vreg(builder), instr->type.u.bit_width);
			assign_vreg(instr, vreg);
			emit_instr2(builder, MOVZX, vreg, asm_value(builder, instr->u.arg));
		}

		break;
	}
	case OP_SEXT: {
		assert(instr->type.t == IR_INT);
		assert(instr->u.arg.type.t == IR_INT);

		AsmValue vreg = asm_vreg(new_vreg(builder), instr->type.u.bit_width);
		assign_vreg(instr, vreg);
		emit_instr2(builder, MOVSX, vreg, asm_value(builder, instr->u.arg));
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
			IrGlobal *target = instr->u.call.callee.u.global;
			assert(target->type.t == IR_FUNCTION);

			IrType callee_type = instr->u.call.callee.u.global->type;
			assert(callee_type.t == IR_FUNCTION);

			u32 callee_arity = callee_type.u.function.arity;
			assert(call_arity == callee_arity
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
			emit_instr2(builder, SUB, asm_phys_reg(REG_CLASS_SP, 64),
					asm_imm(args_plus_padding));
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
				VReg *vreg = ARRAY_REF(&builder->virtual_registers, VReg,
						*ARRAY_LAST(&arg_vregs, u32));
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
					assert(arg_class->u.mem.size == 8);

					// Need a temp vreg to do an 8-byte store into memory.
					AsmValue temp_vreg = asm_vreg(new_vreg(builder), 64);

					emit_instr2(builder, MOV, temp_vreg, arg);
					emit_instr2(builder,
							MOV, 
							asm_deref(asm_offset_reg(REG_CLASS_SP, 64, location)),
							temp_vreg);
				} else {
					// @TODO: This is essentially a bad open-coding of memcpy.
					// Adding a call to memcpy would be really awkward since
					// we're in the middle of a call sequence. We should either
					// make this smarter or somehow make it go through the
					// regular memcpy code in IR, which we can optimise along
					// with other uses of memcpy.
					Register src;
					u32 initial_offset;
					switch (arg.t) {
					case ASM_VALUE_REGISTER:
						src = arg.u.reg;
						initial_offset = 0;
						break;
					case ASM_VALUE_OFFSET_REGISTER:
						if (arg.u.offset_register.offset.t == ASM_CONST_IMMEDIATE) {
							src = arg.u.offset_register.reg;
							initial_offset = arg.u.offset_register.offset.u.immediate;
							break;
						}
						// Deliberate fallthrough
					default: {
						AsmValue src_vreg = asm_vreg(new_vreg(builder), 64);
						emit_instr2(builder, MOV, src_vreg, arg);

						src = src_vreg.u.reg;
						initial_offset = 0;
						break;
					}
					}

					u32 temp_vreg = new_vreg(builder);
					u32 to_copy = arg_class->u.mem.size;
					u32 i = 0;
					for (; to_copy - i >= 8; i += 8) {
						AsmConst offset = asm_const_imm(location.u.immediate + i);
						emit_instr2(builder, MOV, asm_vreg(temp_vreg, 64),
								asm_deref((AsmValue) {
										.t = ASM_VALUE_OFFSET_REGISTER,
										.u.offset_register.reg = src,
										.u.offset_register.offset = asm_const_imm(i),
										}));
						emit_instr2(builder,
								MOV,
								asm_deref(asm_offset_reg(REG_CLASS_SP, 64, offset)),
								asm_vreg(temp_vreg, 64));
					}
					for (; i < to_copy; i++) {
						AsmConst offset = asm_const_imm(location.u.immediate + i);
						emit_instr2(builder, MOV, asm_vreg(temp_vreg, 64),
								asm_deref((AsmValue) {
										.t = ASM_VALUE_OFFSET_REGISTER,
										.u.offset_register.reg = src,
										.u.offset_register.offset = asm_const_imm(i),
										}));
						emit_instr2(builder,
								MOV,
								asm_deref(asm_offset_reg(REG_CLASS_SP, 64, offset)),
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
		//
		// @TODO: Once we pass args in vector registers, set al appropriately.
		if (gen_new_call_seq) {
			emit_instr2(builder, MOV,
					pre_alloced_vreg(builder, REG_CLASS_A, 8), asm_imm(0));

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
			emit_instr2(builder, ADD, asm_phys_reg(REG_CLASS_SP, 64),
					asm_imm(args_plus_padding));
		}
		builder->curr_sp_diff = 0;

		if (instr->u.call.return_type.t != IR_VOID) {
			// Move out of RAX into a new vreg. The live range of the result
			// might overlap another case where we need to use RAX.
			AsmValue rax_vreg = pre_alloced_vreg(builder, REG_CLASS_A, 64);
			AsmValue temp_vreg = asm_vreg(new_vreg(builder), 64);
			emit_instr2(builder, MOV, temp_vreg, rax_vreg);

			assign_vreg(instr, temp_vreg);
		}

		break;
	}
	case OP_BIT_XOR: asm_gen_binary_instr(builder, instr, XOR); break;
	case OP_BIT_AND: asm_gen_binary_instr(builder, instr, AND); break;
	case OP_BIT_OR: asm_gen_binary_instr(builder, instr, OR); break;
	case OP_BIT_NOT: {
		assert(instr->type.t == IR_INT);
		u8 width = instr->type.u.bit_width;

		AsmValue vreg = asm_vreg(new_vreg(builder), width);
		assign_vreg(instr, vreg);
		AsmValue arg = asm_value(builder, instr->u.arg);

		emit_instr2(builder, MOV, vreg, arg);
		emit_instr1(builder, NOT, vreg);

		break;
	}
	case OP_NEG: {
		assert(instr->type.t == IR_INT);
		u8 width = instr->type.u.bit_width;

		AsmValue vreg = asm_vreg(new_vreg(builder), width);
		assign_vreg(instr, vreg);
		AsmValue arg = asm_value(builder, instr->u.arg);

		emit_instr2(builder, MOV, vreg, arg);
		emit_instr1(builder, NEG, vreg);

		break;
	}
	// SHL and SHR can't take an arbitrary register as the RHS. They can take
	// an immediate, or they can take CL. So if we have args (reg1, reg2), we
	// need to move reg2 into CL first.
	case OP_SHL:
	case OP_SHR: {
		AsmOp op = instr->op == OP_SHL ? SHL : SHR;

		AsmValue arg1 = asm_value(builder, instr->u.binary_op.arg1);
		AsmValue arg2 = asm_value(builder, instr->u.binary_op.arg2);
		u8 width = instr->type.u.bit_width;

		AsmValue target = asm_vreg(new_vreg(builder), width);
		assign_vreg(instr, target);
		emit_instr2(builder, MOV, target, arg1);

		switch (arg2.t) {
		case ASM_VALUE_CONST:
			emit_instr2(builder, op, target, arg2);
			break;
		case ASM_VALUE_REGISTER: {
			AsmValue reg_cl = pre_alloced_vreg(builder, REG_CLASS_C, 8);

			Register arg2_low = arg2.u.reg;
			arg2_low.width = 8;

			emit_instr2(builder, MOV, reg_cl,
					(AsmValue) {
						.t = ASM_VALUE_REGISTER,
						.u.reg = arg2_low,
					});

			emit_instr2(builder, op, target, reg_cl);
			break;
		}
		default:
			UNREACHABLE;
		}

		break;
	}
	case OP_ADD: asm_gen_binary_instr(builder, instr, ADD); break;
	case OP_SUB: asm_gen_binary_instr(builder, instr, SUB); break;
	case OP_MUL: {
		assert(instr->type.t == IR_INT);
		u8 width = instr->type.u.bit_width;

		AsmValue vreg = asm_vreg(new_vreg(builder), width);
		assign_vreg(instr, vreg);

		AsmValue arg1 = asm_value(builder, instr->u.binary_op.arg1);
		AsmValue arg2 = asm_value(builder, instr->u.binary_op.arg2);

		if (arg1.t != ASM_VALUE_CONST && arg2.t != ASM_VALUE_CONST) {
			emit_instr2(builder, MOV, vreg, arg1);
			emit_instr2(builder, IMUL, vreg, arg2);
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

			emit_instr3(builder, IMUL, vreg, non_const_arg, const_arg);
		}

		break;
	}
	case OP_DIV: case OP_MOD: {
		assert(instr->type.t == IR_INT);
		u8 width = instr->type.u.bit_width;

		AsmValue arg1 = asm_value(builder, instr->u.binary_op.arg1);
		AsmValue arg2 = asm_value(builder, instr->u.binary_op.arg2);

		AsmValue reg_arg2 = arg2;
		if (arg2.t == ASM_VALUE_CONST) {
			reg_arg2 = asm_vreg(new_vreg(builder), width);
			emit_instr2(builder, MOV, reg_arg2, arg2);
		}

		AsmValue quotient = pre_alloced_vreg(builder, REG_CLASS_A, width);
		AsmValue remainder = pre_alloced_vreg(builder, REG_CLASS_D, width);

		instr->vreg_number =
			(instr->op == OP_DIV ? quotient : remainder).u.reg.u.vreg_number;
		emit_instr2(builder, MOV, quotient, arg1);

		AsmOp sign_extend_op;
		switch (width) {
		case 32: sign_extend_op = CDQ; break;
		case 64: sign_extend_op = CQO; break;
		}

		AsmInstr *sign_extend = emit_instr0(builder, sign_extend_op);
		add_dep(sign_extend, quotient);
		add_dep(sign_extend, remainder);

		AsmInstr *idiv = emit_instr1(builder, IDIV, reg_arg2);
		add_dep(idiv, quotient);
		add_dep(idiv, remainder);
		break;
	}
	// @NOTE: Handled specially for a similar reason to OP_FIELD and OP_LOCAL
	// above. These instructions are mostly used as the argument to OP_COND, so
	// we let OP_COND match them itself in those cases.
	case OP_EQ: case OP_NEQ: case OP_GT: case OP_GTE: case OP_LT: case OP_LTE: break;
	case OP_BUILTIN_VA_START: {
		AsmValue va_list_ptr = asm_vreg(new_vreg(builder), 64);
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
		AsmValue starting_offset = asm_fixed_imm(
				48 - builder->register_save_area_size, 32);

		emit_instr2(builder, MOV, asm_deref(va_list_ptr), starting_offset);

		// Skip next_vector_reg_offset since we don't use vector registers for
		// passing arguments yet.

		emit_instr2(builder, ADD, va_list_ptr, asm_imm(8));

		// Stack args start at the bottom of the previous stack frame, which is
		// always rbp + 16
		AsmValue temp_vreg = asm_vreg(new_vreg(builder), 64);
		emit_instr2(builder, MOV, temp_vreg, asm_phys_reg(REG_CLASS_BP, 64));
		emit_instr2(builder, ADD, temp_vreg, asm_imm(16));
		emit_instr2(builder, MOV, asm_deref(va_list_ptr), temp_vreg);

		emit_instr2(builder, ADD, va_list_ptr, asm_imm(8));

		// The register save area is always at the bottom of our stack frame.
		// However, as mentioned above, we need to offset this value for ABI
		// reasons.
		AsmValue register_save_area = asm_vreg(new_vreg(builder), 64);

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

		u32 vreg = new_vreg(builder);
		instr->vreg_number = vreg;
		AsmValue vreg_32 = asm_vreg(vreg, 32);
		AsmValue vreg_64 = asm_vreg(vreg, 64);

		emit_instr2(builder, XOR, vreg_64, vreg_64);
		emit_instr2(builder, MOV, vreg_32, asm_deref(va_list_ptr));
		emit_instr2(builder, ADD, asm_deref(va_list_ptr), asm_imm(size));
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

// Reserved for spills and fills.
#define SPILL_REGISTER REG_CLASS_R12

#define ALLOCATION_ORDER \
	X(0,  REG_CLASS_R13), \
	X(1,  REG_CLASS_R14), \
	X(2,  REG_CLASS_R15), \
	X(3,  REG_CLASS_B), \
	X(4,  REG_CLASS_R11), \
	X(5,  REG_CLASS_R10), \
	X(6,  REG_CLASS_R9), \
	X(7,  REG_CLASS_R8), \
	X(8,  REG_CLASS_C), \
	X(9,  REG_CLASS_D), \
	X(10, REG_CLASS_SI), \
	X(11, REG_CLASS_DI), \
	X(12, REG_CLASS_A),

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

typedef struct Pred
{
	u32 dest_offset;
	u32 src_offset;

	struct Pred *next;
} Pred;

bool references_vreg(AsmValue value, u32 vreg)
{
	Register reg;
	switch (value.t) {
	case ASM_VALUE_REGISTER: reg = value.u.reg; break;
	case ASM_VALUE_OFFSET_REGISTER: reg = value.u.offset_register.reg; break;
	default: return false;
	}

	return reg.t == V_REG && reg.u.vreg_number == vreg;
}

bool is_use(AsmInstr *instr, u32 vreg)
{
	for (u32 i = 0; i < instr->arity; i++) {
		if (references_vreg(instr->args[i], vreg)) {
			return true;
		}
	}
	for (u32 i = 0; i < instr->num_deps; i++) {
		if (instr->vreg_deps[i] == vreg) {
			return true;
		}
	}

	return false;
}

bool is_def(AsmInstr *instr, u32 vreg_num, VReg *vreg)
{
	switch (instr->op) {
	// Special case for pre-allocated return value vregs.
	case CALL: return vreg->pre_alloced && vreg->u.assigned_register == REG_CLASS_A;

	case CDQ: case CQO:
		return vreg->pre_alloced && vreg->u.assigned_register == REG_CLASS_D
			// We're using "is_use" just to check that we're in the vreg_deps
			// for this instr - we don't want to return true for some random
			// register that is also pre-allocated to REG_CLASS_D.
			&& is_use(instr, vreg_num);

	// Special case for zeroing a register by XOR'ing with itself
	case XOR:
		return references_vreg(instr->args[0], vreg_num)
			&& references_vreg(instr->args[1], vreg_num);

	case MOV: case MOVSX: case MOVZX:
	case POP:
	case IMUL:
	case SETE: case SETNE: case SETG: case SETGE: case SETL: case SETLE:
		return references_vreg(instr->args[0], vreg_num);

	default: return false;
	}
}

static void compute_live_ranges(AsmBuilder *builder)
{
	Array(AsmInstr) *body = builder->current_block;

	// Liveness is a backwards analysis, so we need access to predecessors for
	// each instruction. These are stored intrusively on AsmSymbol.
	//
	// @NOTE: I'm not sure if this will still work correctly if we add
	// fallthrough, i.e.: eliminating redundant jumps like:
	//     jmp a
	// a:  ...
	Pool preds_pool;
	pool_init(&preds_pool, 512);
	for (u32 i = 0; i < body->size; i++) {
		AsmInstr *instr = ARRAY_REF(body, AsmInstr, i);
		switch (instr->op) {
		case JMP: case JE: case JNE: case JG: case JGE: case JL: case JLE:
			break;
		default: continue;
		}

		assert(instr->args[0].t == ASM_VALUE_CONST);
		AsmConst c = instr->args[0].u.constant;
		assert(c.t == ASM_CONST_SYMBOL);
		AsmSymbol *target = c.u.symbol;

		if (target != builder->ret_label) {
			assert(target->offset < body->size);

			Pred **location = &target->pred;
			Pred *pred = target->pred;
			while (pred != NULL && pred->next != NULL) {
				pred = pred->next;
				location = &pred->next;
			}

			Pred *new_pred = pool_alloc(&preds_pool, sizeof *pred);
			new_pred->src_offset = i;
			new_pred->dest_offset = target->offset;
			new_pred->next = NULL;

			*location = new_pred;
		}
	}

	// This implements the algorithm described in Mohnen 2002, "A Graph-Free
	// Approach to Data-Flow Analysis". As the name suggests, we do without
	// constructing an explicit CFG. See Section 4 in the paper for details.
	// Note the paragraph at the end of Section 4 - in this case the analysis
	// is backwards and existential, so both of the adaptations in this
	// paragraph are applied to the algorithm in Figure 5. The preprocessing to
	// connect jumps to jump targets is done just above.

	BitSet liveness;
	bit_set_init(&liveness, body->size);
	BitSet working_set;
	bit_set_init(&working_set, body->size);

	for (u32 vreg_num = 0;
			vreg_num < builder->virtual_registers.size;
			vreg_num++) {
		VReg *vreg = ARRAY_REF(&builder->virtual_registers, VReg, vreg_num);
		if (vreg->live_range_start != -1 && vreg->live_range_end != -1)
			continue;

		bit_set_clear_all(&liveness);
		bit_set_set_all(&working_set);
		u32 largest_working_set_elem = body->size - 1;

		while (!bit_set_is_empty(&working_set)) {
			u32 pc = largest_working_set_elem;

			for (;;) {
				bit_set_set_bit(&working_set, pc, false);
				if (pc == largest_working_set_elem) {
					for (i32 i = pc / 64; i >= 0; i--) {
						u64 bits = working_set.bits[i];
						if (bits != 0) {
							largest_working_set_elem =
								64 * i + highest_set_bit(bits);
							break;
						}
					}
				}

				// We shouldn't be re-examining points that are already live.
				// If we've shown a vreg to be live at pc the analysis below
				// will add nothing.
				assert(!bit_set_get_bit(&liveness, pc));

				// @TODO: We might be able to simplify this somewhat based on
				// the fact that (I think) our instruction selection always
				// produces SSA form for vregs.

				AsmInstr *instr = ARRAY_REF(body, AsmInstr, pc);
				i32 succ0 = -1, succ1 = -1;
				switch (instr->op) {
				case JE: case JNE: case JG: case JGE: case JL: case JLE:
					succ1 = pc + 1;
					// @NOTE: Deliberate fallthrough.
				case JMP: {
					assert(instr->args[0].t == ASM_VALUE_CONST);
					AsmConst c = instr->args[0].u.constant;
					assert(c.t == ASM_CONST_SYMBOL);
					AsmSymbol *target = c.u.symbol;

					if (target != builder->ret_label) {
						assert(target->offset < body->size);
						succ0 = target->offset;
					}
					break;
				}
				default:
					if (pc != body->size - 1)
						succ0 = pc + 1;
					break;
				}

				bool new;
				if (is_use(instr, vreg_num)) {
					new = true;
				} else if (!((succ0 != -1 && (
									bit_set_get_bit(&liveness, succ0) && !is_def(
										ARRAY_REF(body, AsmInstr, succ0), vreg_num, vreg)))
							|| (succ1 != -1 && (
									bit_set_get_bit(&liveness, succ1) && !is_def(
										ARRAY_REF(body, AsmInstr, succ1), vreg_num, vreg))))) {
					// The big hairy condition above checks if all of our
					// successors are either not live or a def. If so then
					// we're not live.
					new = false;
				} else {
					// Otherwise, we have a successor that is live and not a
					// def, hence we're live.
					new = true;
				}
				bit_set_set_bit(&liveness, pc, new);

				u32 prev;
				Pred *pred = NULL;
				if (instr->label != NULL)
					pred = instr->label->pred;

				if (pred != NULL && pred->dest_offset == pc) {
					prev = pred->src_offset;
				} else if (pc == 0) {
					break;
				} else {
					prev = pc - 1;
				}

				// Check if we need to add stuff to the working set to deal
				// with multiple predecessors. We only bother adding
				// predecessors to the working set if we've refined. If !new
				// then we definitely haven't, and we can skip the whole loop.
				// The second half of the refinement check is the liveness
				// check before adding to working_set below.
				if (new && pred != NULL) {
					pred = pred->next;
					while (pred != NULL) {
						u32 src = pred->src_offset;
						u32 dest = pred->dest_offset;

						if (dest != pc)
							break;

						if (!bit_set_get_bit(&liveness, src)) {
							bit_set_set_bit(&working_set, src, true);
							if (src > largest_working_set_elem)
								largest_working_set_elem = src;
						}
					}
				}

				if (!(new && !bit_set_get_bit(&liveness, prev)))
					break;

				pc = prev;
			}
		}

		i32 lowest = bit_set_lowest_set_bit(&liveness);
		if (vreg->live_range_start == -1 || vreg->live_range_start > lowest)
			vreg->live_range_start = lowest;

		i32 highest = bit_set_highest_set_bit(&liveness);
		if (vreg->live_range_end == -1 || (highest != -1 && vreg->live_range_end < highest))
			vreg->live_range_end = highest;
	}

	pool_free(&preds_pool);
	bit_set_free(&liveness);
	bit_set_free(&working_set);

	if (flag_dump_live_ranges) {
		printf("%s:\n", builder->current_function->name);
		for (u32 i = 0; i < body->size; i++) {
			AsmInstr *instr = ARRAY_REF(body, AsmInstr, i);
			printf("%u ", i);
			dump_asm_instr(instr);
		}

		for (u32 i = 0; i < builder->virtual_registers.size; i++) {
			VReg *vreg = ARRAY_REF(&builder->virtual_registers, VReg, i);
			printf("#%u: [%d, %d]", i, vreg->live_range_start, vreg->live_range_end);
			switch (vreg->t) {
			// @TODO: Move register dumping stuff we we can dump the name here
			// rather than just a number
			case IN_REG: printf(" (%d)", vreg->u.assigned_register); break;
			case ON_STACK: printf(" [%d]", vreg->u.assigned_stack_slot); break;
			case UNASSIGNED: break;
			}
			putchar('\n');
		}
		putchar('\n');
	}
}

int compare_live_range_start(const void *a, const void *b)
{
	VReg *live_range_a = *(VReg **)a, *live_range_b = *(VReg **)b;
	int x = live_range_a->live_range_start, y = live_range_b->live_range_start;
	if (x < y) return -1;
	if (x == y) return 0;
	return 1;
}

// @TODO: Save all caller save registers that are live across calls.
static void allocate_registers(AsmBuilder *builder)
{
	compute_live_ranges(builder);

	Array(AsmInstr) *body = builder->current_block;

	u32 free_regs_bitset;
	assert(STATIC_ARRAY_LENGTH(alloc_index_to_reg) < 8 * sizeof free_regs_bitset);

	// Start with all regs free
	free_regs_bitset = (1 << STATIC_ARRAY_LENGTH(alloc_index_to_reg)) - 1;

	// virtual_registers isn't necessarily sorted by live range start.
	// i.e. if RAX is pre-allocated from a function call and we don't use it
	// until later. Linear scan depends on this ordering, so we sort it first.
	// We can't sort it in place because the indices are used to look vregs up
	// by vreg number.
	VReg **live_ranges = malloc(builder->virtual_registers.size * sizeof *live_ranges);
	u32 live_range_out_index = 0;
	for (u32 i = 0; i < builder->virtual_registers.size; i++) {
		VReg *vreg = ARRAY_REF(&builder->virtual_registers, VReg, i);
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
	qsort(live_ranges, num_live_ranges, sizeof *live_ranges, compare_live_range_start);

	Array(VReg *) active_vregs;
	ARRAY_INIT(&active_vregs, VReg *, 16);
	for (u32 i = 0; i < num_live_ranges; i++) {
		VReg *vreg = live_ranges[i];

		while (active_vregs.size != 0) {
			VReg *active_vreg = *ARRAY_REF(&active_vregs, VReg *, 0);
			if (active_vreg->live_range_end >= vreg->live_range_start)
				break;

			switch (active_vreg->t) {
			case UNASSIGNED: UNREACHABLE;
			case IN_REG: {
				u32 alloc_index =
					reg_to_alloc_index[active_vreg->u.assigned_register];
				free_regs_bitset |= 1 << alloc_index;
				break;
			}
			case ON_STACK:
				// @TODO: Reuse the spilled stack slot?
				break;
			}

			// @TODO: Remove all the invalidated vregs at once instead of
			// repeatedly shifting down.
			ARRAY_REMOVE(&active_vregs, VReg *, 0);
		}

		if (vreg->t == UNASSIGNED) {
			if (free_regs_bitset == 0) {
				vreg->t = ON_STACK;
				vreg->u.assigned_stack_slot = builder->local_stack_usage;
				builder->local_stack_usage += 8;
			} else {
				u32 first_free_alloc_index = lowest_set_bit(free_regs_bitset);
				vreg->t = IN_REG;
				vreg->u.assigned_register = alloc_index_to_reg[first_free_alloc_index];
				free_regs_bitset &= ~(1 << first_free_alloc_index);
			}
		} else {
			// This register has already been assigned, e.g. part of a call
			// sequence. We don't need to allocate it, but we do need to keep
			// track of it so it doesn't get clobbered.
			assert(vreg->t == IN_REG);
			assert(vreg->pre_alloced);

			u32 alloc_index = reg_to_alloc_index[vreg->u.assigned_register];
			if ((free_regs_bitset & (1 << alloc_index)) == 0) {
				// Already allocated to something else. We need to spill the
				// existing value, because we're pre-alloced and we need this
				// specific register.

				VReg *existing = NULL;
				for (u32 i = 0; i < active_vregs.size; i++) {
					VReg *active_vreg = *ARRAY_REF(&active_vregs, VReg *, i);
					if (active_vreg->t == IN_REG &&
							active_vreg->u.assigned_register == vreg->u.assigned_register) {
						existing = active_vreg;
						break;
					}
				}
				assert(existing != NULL);

				// If the existing register is also pre-alloced, we have two
				// vregs with overlapping live ranges that need the same
				// physical register. This should never happen.
				assert(!existing->pre_alloced);

				existing->t = ON_STACK;
				// @TODO: Alignment
				existing->u.assigned_stack_slot = builder->local_stack_usage;
				builder->local_stack_usage += 8;
			} else {
				free_regs_bitset &= ~(1 << alloc_index);
			}
		}

		u32 insertion_point = active_vregs.size;
		for (u32 j = 0; j < active_vregs.size; j++) {
			VReg *active_vreg = *ARRAY_REF(&active_vregs, VReg *, j);
			if (active_vreg->live_range_end > vreg->live_range_end) {
				insertion_point = j;
				break;
			}
		}
		*ARRAY_INSERT(&active_vregs, VReg *, insertion_point) = vreg;
	}
	free(live_ranges);

	array_clear(&active_vregs);
	u32 vreg_index = 0;
	for (u32 i = 0; i < body->size; i++) {
		while (active_vregs.size != 0) {
			VReg *active_vreg = *ARRAY_REF(&active_vregs, VReg *, 0);
			if (active_vreg->live_range_end == -1)
				continue;
			if ((u32)active_vreg->live_range_end >= i)
				break;
			assert(active_vreg->t == IN_REG);

			// @TODO: Remove all the invalidated vregs at once instead of
			// repeatedly shifting down.
			ARRAY_REMOVE(&active_vregs, VReg *, 0);
		}
		if (vreg_index != builder->virtual_registers.size) {
			VReg *next_vreg =
				ARRAY_REF(&builder->virtual_registers, VReg, vreg_index);
			// Pre-alloced vregs aren't counted. Otherwise we'd think we need
			// to spill registers we just used to pass arguments.
			// @TODO: Perhaps this is too weak of a condition? It'd be nice if
			// we could still catch errors, where we accidentally pre-allocated
			// a caller-save register across a callsite.
			if (next_vreg->t == IN_REG
					&& !next_vreg->pre_alloced
					&& (u32)next_vreg->live_range_start == i) {
				u32 insertion_point = active_vregs.size;
				for (u32 j = 0; j < active_vregs.size; j++) {
					VReg *active_vreg = *ARRAY_REF(&active_vregs, VReg *, j);
					if (active_vreg->live_range_end == -1)
						continue;
					if ((u32)active_vreg->live_range_end < i) {
						insertion_point = j;
						break;
					}
				}
				*ARRAY_INSERT(&active_vregs, VReg *, insertion_point) = next_vreg;

				vreg_index++;
			}
		}

		AsmInstr *instr = ARRAY_REF(body, AsmInstr, i);
		if (instr->op == CALL) {
			for (u32 j = 0; j < active_vregs.size; j++) {
				VReg *vreg = *ARRAY_REF(&active_vregs, VReg *, j);
				if (vreg->t == IN_REG) {
					RegClass reg = vreg->u.assigned_register;
					if (((1 << reg) & CALLER_SAVE_REGS_BITMASK) == 0)
						continue;

					vreg->t = ON_STACK;
					vreg->u.assigned_stack_slot = builder->local_stack_usage;
					builder->local_stack_usage += 8;

					// @TODO: Remove all the spilled vregs at once instead of
					// repeatedly shifting down.
					ARRAY_REMOVE(&active_vregs, VReg *, j);
					j--;
				}
			}
		}
	}
	array_free(&active_vregs);

#if 0
	for (u32 i = 0; i < builder->virtual_registers.size; i++) {
		VReg *vreg = ARRAY_REF(&builder->virtual_registers, VReg, i);
		printf("#%u =", i);
		switch (vreg->t) {
		case IN_REG: printf(" (%d)\n", vreg->u.assigned_register); break;
		case ON_STACK: printf(" [%d]\n", vreg->u.assigned_stack_slot); break;
		case UNASSIGNED: UNREACHABLE;
		}
	}
#endif

	u32 curr_sp_diff = 0;
	for (u32 i = 0; i < body->size; i++) {
		AsmInstr *instr = ARRAY_REF(body, AsmInstr, i);

		if (instr->op == SUB && instr->args[0].t == ASM_VALUE_REGISTER
				&& instr->args[0].u.reg.t == PHYS_REG
				&& instr->args[0].u.reg.u.class == REG_CLASS_SP) {
			assert(instr->args[1].t == ASM_VALUE_CONST);

			AsmConst c = instr->args[1].u.constant;
			assert(c.t == ASM_CONST_IMMEDIATE);
			curr_sp_diff += c.u.immediate;
		}

		for (u32 j = 0; j < instr->arity; j++) {
			AsmValue *arg = instr->args + j;
			Register *reg = arg_reg(arg);
			if (reg == NULL)
				continue;

			if (reg->t == V_REG) {
				u32 vreg_number = reg->u.vreg_number;
				VReg *vreg = ARRAY_REF(&builder->virtual_registers,
						VReg, vreg_number);

				switch (vreg->t) {
				case IN_REG:
					reg->t = PHYS_REG;
					reg->u.class = vreg->u.assigned_register;
					break;
				case ON_STACK:
					reg->t = PHYS_REG;
					reg->u.class = SPILL_REGISTER;
					// @TODO: Insert all at once, rather than shifting along
					// every time.
					// @TODO: Elide this when we just write to the register and
					// don't use the previous value
					*ARRAY_INSERT(body, AsmInstr, i) = (AsmInstr) {
						.op = MOV,
						.arity = 2,
						.args[0] = asm_phys_reg(SPILL_REGISTER, 64),
						.args[1] = asm_deref(asm_offset_reg(REG_CLASS_SP, 64,
								asm_const_imm(vreg->u.assigned_stack_slot + curr_sp_diff))),
					};
					// @TODO: Elide this when we just read the register and
					// don't write anything back.
					*ARRAY_INSERT(body, AsmInstr, i + 2) = (AsmInstr) {
						.op = MOV,
						.arity = 2,
						.args[0] = asm_deref(asm_offset_reg(REG_CLASS_SP, 64,
								asm_const_imm(vreg->u.assigned_stack_slot + curr_sp_diff))),
						.args[1] = asm_phys_reg(SPILL_REGISTER, 64),
					};
					break;
				case UNASSIGNED:
					UNREACHABLE;
				}
			}
		}
	}
}

void asm_gen_function(AsmBuilder *builder, IrGlobal *ir_global)
{
	assert(ir_global->type.t == IR_FUNCTION);
	IrFunction *ir_func = &ir_global->initializer->u.function;

	AsmSymbol *asm_symbol = ir_global->asm_symbol;
	assert(asm_symbol != NULL);

	asm_symbol->defined = ir_global->initializer != NULL;
	if (!asm_symbol->defined)
		return;

	Array(AsmInstr) body;
	ARRAY_INIT(&body, AsmInstr, 20);

	builder->current_function = ir_global;
	builder->current_block = &body;

	builder->local_stack_usage = 0;

	AsmSymbol *ret_label = pool_alloc(&builder->asm_module.pool, sizeof *ret_label);
	*ret_label = (AsmSymbol) {
		.name = "ret",
		.section = TEXT_SECTION,
		.defined = true,
		.linkage = ASM_LOCAL_LINKAGE,
		.pred = NULL,
	};
	builder->ret_label = ret_label;

	if (ARRAY_IS_VALID(&builder->virtual_registers))
		array_free(&builder->virtual_registers);
	ARRAY_INIT(&builder->virtual_registers, VReg, 20);
	IrType return_type = *ir_global->type.u.function.return_type;
	assert(return_type.t == IR_INT
			|| return_type.t == IR_POINTER
			|| return_type.t == IR_VOID);

	for (u32 block_index = 0; block_index < ir_func->blocks.size; block_index++) {
		IrBlock *block = *ARRAY_REF(&ir_func->blocks, IrBlock *, block_index);
		AsmSymbol *label = pool_alloc(&builder->asm_module.pool, sizeof *label);
		block->label = label;

		*label = (AsmSymbol) {
			.name = block->name,
			.section = TEXT_SECTION,
			.defined = true,
			.linkage = ASM_LOCAL_LINKAGE,
			.pred = NULL,
		};
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
		ArgClass *arg_class = ir_global->call_seq.arg_classes + i;
		if (arg_class->t == ARG_CLASS_REG) {
			u32 vreg_num = new_vreg(builder);
			emit_instr2(builder, MOV, asm_vreg(vreg_num, 64),
					asm_phys_reg(arg_class->u.reg.reg, 64));
			arg_class->u.reg.vreg = vreg_num;

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
			? 0 : builder->current_block->size;
		block->label->offset = first_instr_of_block_index;

		Array(IrInstr *) *instrs = &block->instrs;
		for (u32 i = 0; i < instrs->size; i++) {
			IrInstr *instr = *ARRAY_REF(instrs, IrInstr *, i);
			asm_gen_instr(builder, ir_global, block, instr);
		}

		AsmInstr *first_instr_of_block = ARRAY_REF(
				builder->current_block, AsmInstr, first_instr_of_block_index);
		first_instr_of_block->label = block->label;
	}

	if (flag_print_pre_regalloc_stats) {
		printf("%s: %u instrs, %u vregs\n",
				ir_global->name, body.size, builder->virtual_registers.size);
	}

	allocate_registers(builder);

	u32 used_callee_save_regs_bitset = 0;
	for (u32 i = 0; i < builder->current_block->size; i++) {
		AsmInstr *instr = ARRAY_REF(builder->current_block, AsmInstr, i);
		for (u32 j = 0; j < instr->arity; j++) {
			Register *reg = arg_reg(instr->args + j);
			if (reg != NULL &&
					reg->t == PHYS_REG &&
					is_callee_save(reg->u.class)) {
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

	u32 stack_adjustment = builder->local_stack_usage;

	// Ensure that the stack frame size is a multiple of 16 before entering the
	// body of the function. Then each "call" op just needs to ensure that its
	// stack arg area is correctly aligned. The return address and the previous
	// value of rbp are pushed, for a total of 16 bytes which doesn't affect
	// alignment. Then we push callee-save registers and subtract space for
	// locals. This may affect alignment, so we add padding if necessary.
	//
	// See the System V x86-64 ABI spec, section 3.2.2
	stack_adjustment +=
		16 - ((num_callee_save_regs * 8 + builder->local_stack_usage) % 16);

	if (stack_adjustment != 0) {
		emit_instr2(builder,
				SUB,
				asm_phys_reg(REG_CLASS_SP, 64),
				asm_imm(stack_adjustment));
	}

	// Argument registers are stored in increasing order in the register save
	// area. We only bother storing the registers that could be used to pass
	// varargs arguments. e.g.: for "void foo(int a, int b, ...)" we don't save
	// rdi or rsi.
	// See the System V x86-64 ABI spec, figure 3.33
	if (ir_global->type.u.function.variable_arity) {
		u32 num_reg_args = 0;
		for (u32 i = 0; i < ir_global->type.u.function.arity; i++) {
			ArgClass *arg_class = ir_global->call_seq.arg_classes + i;

			if (arg_class->t == ARG_CLASS_REG) {
				num_reg_args++;
			}
		}

		for (u32 i = num_reg_args;
				i < STATIC_ARRAY_LENGTH(argument_registers);
				i++) {
			AsmConst offset = asm_const_imm((i - num_reg_args) * 8);
			emit_instr2(builder,
					MOV,
					asm_deref(asm_offset_reg(REG_CLASS_SP, 64, offset)),
					asm_phys_reg(argument_registers[i], 64));
		}
	}

	for (u32 j = 0; j < body.size; j++) {
		*ARRAY_APPEND(builder->current_block, AsmInstr) =
			*ARRAY_REF(&body, AsmInstr, j);
	}

	u32 epilogue_start = builder->current_block->size;

	if (stack_adjustment != 0) {
		emit_instr2(builder,
				ADD,
				asm_phys_reg(REG_CLASS_SP, 64),
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

	AsmInstr *epilogue_first_instr = ARRAY_REF(builder->current_block,
			AsmInstr, epilogue_start);
	epilogue_first_instr->label = ret_label;

	array_free(&body);
}

static void write_const(AsmModule *asm_module, IrConst *konst, Array(u8) *out)
{
	switch (konst->type.t) {
	case IR_INT:
		for (u32 n = 0; n < size_of_ir_type(konst->type); n++) {
			u8 byte = (konst->u.integer >> (n * 8)) & 0xFF;
			*ARRAY_APPEND(out, u8) = byte;
		}
		break;
	case IR_POINTER: {
		IrGlobal *global = konst->u.global_pointer;
		if (global != NULL) {
			Fixup *fixup = pool_alloc(&asm_module->pool, sizeof *fixup);
			*ARRAY_APPEND(&asm_module->fixups, Fixup *) = fixup;
			*fixup = (Fixup) {
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
		for (u32 n = 0; n < konst->type.u.array.size; n++)
			write_const(asm_module, konst->u.array_elems + n, out);
		break;
	case IR_STRUCT:
		for (u32 n = 0; n < konst->type.u.strukt.num_fields; n++)
			write_const(asm_module, konst->u.struct_fields + n, out);
		break;
	case IR_FUNCTION: case IR_VOID:
		UNREACHABLE;
	}
}

static bool is_zero(IrConst *konst)
{
	switch (konst->type.t) {
	case IR_INT:
		return konst->u.integer == 0;
	case IR_POINTER:
		return false;
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
	case IR_FUNCTION: case IR_VOID:
		UNREACHABLE;
	}
}

void generate_asm_module(AsmBuilder *builder, TransUnit *trans_unit)
{
	AsmModule *asm_module = &builder->asm_module;

	// First do a pass collecting declarations, to support forward references.
	for (u32 i = 0; i < trans_unit->globals.size; i++) {
		IrGlobal *ir_global = *ARRAY_REF(&trans_unit->globals, IrGlobal *, i);

		Array(AsmSymbol *) *symbols = &asm_module->symbols;
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

		AsmSymbol *asm_symbol = pool_alloc(&asm_module->pool, sizeof *asm_symbol);
		ZERO_STRUCT(asm_symbol);
		*ARRAY_APPEND(symbols, AsmSymbol *) = asm_symbol;
		ir_global->asm_symbol = asm_symbol;

		u32 name_len = strlen(ir_global->name);
		char *name_copy = pool_alloc(&builder->asm_module.pool, name_len + 1);
		memcpy(name_copy, ir_global->name, name_len);
		name_copy[name_len] = '\0';

		asm_symbol->name = name_copy;
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

	for (u32 i = 0; i < trans_unit->globals.size; i++) {
		IrGlobal *ir_global = *ARRAY_REF(&trans_unit->globals, IrGlobal *, i);
		AsmSymbol *asm_symbol = ir_global->asm_symbol;
		if (!asm_symbol->defined)
			continue;

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
			assert(asm_symbol->section == BSS_SECTION);
			u32 size = size_of_ir_type(ir_global->type);
			// @TODO: Alignment
			asm_symbol->offset = asm_module->bss_size;
			asm_symbol->size = size;

			asm_module->bss_size += size;
		}
	}
}
