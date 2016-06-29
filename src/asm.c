#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "array.h"
#include "asm.h"
#include "asm_gen.h"

void init_asm_function(AsmFunction *function, char *name)
{
	ARRAY_INIT(&function->instrs, AsmInstr, 20);
	function->name = name;
}

AsmArg asm_virtual_register(u32 n)
{
	AsmArg asm_arg = {
		.is_deref = false,
		.type = REGISTER,
		.val.reg.type = VIRTUAL_REGISTER,
		.val.reg.val.register_number = n,
	};

	return asm_arg;
}

AsmArg asm_physical_register(PhysicalRegister reg)
{
	AsmArg asm_arg = {
		.is_deref = false,
		.type = REGISTER,
		.val.reg.type = PHYSICAL_REGISTER,
		.val.reg.val.physical_register = reg,
	};

	return asm_arg;
}

AsmArg asm_deref(AsmArg asm_arg)
{
	asm_arg.is_deref = true;
	return asm_arg;
}

AsmArg asm_offset_register(PhysicalRegister reg, u64 offset)
{
	if (offset == 0) {
		return asm_deref(asm_physical_register(reg));
	} else {
		AsmArg asm_arg = {
			.is_deref = false,
			.type = OFFSET_REGISTER,
			.val.offset_register.reg.type = PHYSICAL_REGISTER,
			.val.offset_register.reg.val.physical_register = reg,
			.val.offset_register.offset = offset,
		};

		return asm_arg;
	}
}

AsmArg asm_const32(i32 constant)
{
	AsmArg asm_arg = {
		.is_deref = false,
		.type = CONST32,
		.val.constant = constant,
	};

	return asm_arg;
}

static inline bool asm_arg_is_const(AsmArg asm_arg)
{
	return (asm_arg.type == CONST8) || (asm_arg.type == CONST16) ||
		(asm_arg.type == CONST32) || (asm_arg.type == CONST64);
}

// @TODO: Negative numbers
static bool is_const_and_fits(AsmArg asm_arg, u32 bits)
{
	if (!asm_arg_is_const(asm_arg))
		return false;

	u64 constant = asm_arg.val.constant;
	u64 mask = (1 << bits) - 1;
	return (constant & mask) == constant;
}

static void dump_asm_args(AsmArg *args, u32 num_args);

#define X(x) #x
static char *asm_op_names[] = {
	ASM_OPS
};
#undef X

static void dump_asm_instr(AsmInstr *instr)
{
	putchar('\t');
	char *op_name = asm_op_names[instr->op];
	for (u32 i = 0; op_name[i] != '\0'; i++)
		putchar(tolower(op_name[i]));

	switch (instr->op) {
	case MOV: case XOR: case ADD: case SUB:
		putchar(' ');
		dump_asm_args(instr->args, 2);
		break;
	case PUSH: case POP:
		putchar(' ');
		dump_asm_args(instr->args, 1);
		break;
	case RET: break;
	}

	putchar('\n');
}

#define X(x) #x
static char *physical_register_names[] = {
	PHYSICAL_REGISTERS
};
#undef X

static void dump_register(Register reg)
{
	switch (reg.type) {
	case PHYSICAL_REGISTER: {
		char *reg_name = physical_register_names[reg.val.physical_register];
		for (u32 i = 0; reg_name[i] != '\0'; i++)
			putchar(tolower(reg_name[i]));
		break;
	}
	case VIRTUAL_REGISTER:
		printf("#%u", reg.val.register_number);
		break;
	}
}

static void dump_asm_args(AsmArg *args, u32 num_args)
{
	for (u32 i = 0; i < num_args; i++) {
		if (i != 0)
			fputs(", ", stdout);

		AsmArg *arg = &args[i];
		if (arg->is_deref)
			putchar('[');
		switch (arg->type) {
		case REGISTER:
			dump_register(arg->val.reg);
			break;
		case OFFSET_REGISTER:
			dump_register(arg->val.offset_register.reg);
			printf(" + %" PRIu64, arg->val.offset_register.offset);
			break;
		case CONST8:
			if ((i8)arg->val.constant < 0)
				printf("%" PRId8, (i8)arg->val.constant);
			else
				printf("%" PRIu8, (i8)arg->val.constant);
		case CONST16:
			if ((i16)arg->val.constant < 0)
				printf("%" PRId16, (i16)arg->val.constant);
			else
				printf("%" PRIu16, (i16)arg->val.constant);
		case CONST32:
			if ((i32)arg->val.constant < 0)
				printf("%" PRId32, (i32)arg->val.constant);
			else
				printf("%" PRIu32, (i32)arg->val.constant);
			break;
		case CONST64:
			if ((i64)arg->val.constant < 0)
				printf("%" PRId64, (i64)arg->val.constant);
			else
				printf("%" PRIu64, (i64)arg->val.constant);
			break;
		}
		if (arg->is_deref)
			putchar(']');
	}
}

void dump_asm_module(AsmModule *asm_module)
{
	for (u32 i = 0; i < asm_module->functions.size; i++) {
		AsmFunction *block = ARRAY_REF(&asm_module->functions, AsmFunction, i);

		for (u32 j = 0; j < block->instrs.size; j++) {
			AsmInstr *instr = ARRAY_REF(&block->instrs, AsmInstr, j);
			dump_asm_instr(instr);
		}
	}
}

// @TODO: Probably easier to have these not return the amount of bytes written,
// and instead just ftell to determine how much was written.

static inline u32 write_u8(FILE *file, u8 x)
{
	fwrite(&x, 1, 1, file);
	return 1;
}

static inline u32 write_u16(FILE *file, u16 x)
{
	u8 out[] = {
		(x >> 0) & 0xFF,
		(x >> 8) & 0xFF,
	};

	fwrite(out, 1, sizeof out, file);
	return sizeof out;
}

static inline u32 write_u32(FILE *file, u32 x)
{
	u8 out[] = {
		(x >>  0) & 0xFF,
		(x >>  8) & 0xFF,
		(x >> 16) & 0xFF,
		(x >> 24) & 0xFF,
	};

	fwrite(out, 1, sizeof out, file);
	return sizeof out;
}

static inline u32 write_u64(FILE *file, u64 x)
{
	u8 out[] = {
		(x >>  0) & 0xFF,
		(x >>  8) & 0xFF,
		(x >> 16) & 0xFF,
		(x >> 24) & 0xFF,
		(x >> 32) & 0xFF,
		(x >> 40) & 0xFF,
		(x >> 48) & 0xFF,
		(x >> 56) & 0xFF,
	};

	fwrite(out, 1, sizeof out, file);
	return sizeof out;
}

static inline PhysicalRegister get_register(AsmArg *arg)
{
	Register reg;
	if (arg->type == REGISTER) {
		reg = arg->val.reg;
	} else {
		assert(arg->type == OFFSET_REGISTER);
		reg = arg->val.offset_register.reg;
	}

	assert(reg.type == PHYSICAL_REGISTER);
	return reg.val.physical_register;
}

static inline u32 write_mod_rm_byte(FILE *file, u8 mod, u8 reg, u8 rm)
{
	u8 mod_rm_byte = (mod << 6) | (reg << 3) | rm;
	return write_u8(file, mod_rm_byte);
}

static u32 write_mod_rm_arg(FILE *file, AsmArg *arg, u8 reg_field)
{
	if (arg->type == REGISTER) {
		if (arg->is_deref) {
			switch (get_register(arg)) {
			case RAX: return write_mod_rm_byte(file, 0, reg_field, 0);
			case RCX: return write_mod_rm_byte(file, 0, reg_field, 1);
			case RDX: return write_mod_rm_byte(file, 0, reg_field, 2);
			case RBX: return write_mod_rm_byte(file, 0, reg_field, 3);
			case RSI: return write_mod_rm_byte(file, 0, reg_field, 6);
			case RDI: return write_mod_rm_byte(file, 0, reg_field, 7);
			case RSP: {
				// Mod = 0, R/M = 4 means SIB addressing
				u32 size = write_mod_rm_byte(file, 0, reg_field, 4);
				// SIB byte, with RSP as base and no index/scale
				size += write_u8(file, 0x24);

				return size;
			}
			case RBP: {
				// Mod = 1, R/M = 5 means RBP + disp8
				u32 size = write_mod_rm_byte(file, 1, reg_field, 5);
				// 0 displacement
				size += write_u8(file, 0);

				return size;
			}
			default: UNIMPLEMENTED;
			}
		} else {
			switch (get_register(arg)) {
			case RAX: return write_mod_rm_byte(file, 3, reg_field, 0);
			case RCX: return write_mod_rm_byte(file, 3, reg_field, 1);
			case RDX: return write_mod_rm_byte(file, 3, reg_field, 2);
			case RBX: return write_mod_rm_byte(file, 3, reg_field, 3);
			case RSP: return write_mod_rm_byte(file, 3, reg_field, 4);
			case RBP: return write_mod_rm_byte(file, 3, reg_field, 5);
			case RSI: return write_mod_rm_byte(file, 3, reg_field, 6);
			case RDI: return write_mod_rm_byte(file, 3, reg_field, 7);
			default: UNIMPLEMENTED;
			}
		}
	} else if (arg->type == OFFSET_REGISTER) {
		assert(arg->is_deref);

		u64 offset = arg->val.offset_register.offset;
		u8 mod;
		// @TODO: Negative numbers
		if ((offset & 0xFF) == offset)
			mod = 1;
		else if ((offset & 0xFFFFFFFF) == offset)
			mod = 2;
		else
			assert(!"Offset too large!");

		u32 size = 0;
		switch (get_register(arg)) {
		case RAX: size += write_mod_rm_byte(file, mod, reg_field, 0); break;
		case RCX: size += write_mod_rm_byte(file, mod, reg_field, 1); break;
		case RDX: size += write_mod_rm_byte(file, mod, reg_field, 2); break;
		case RBX: size += write_mod_rm_byte(file, mod, reg_field, 3); break;
		case RBP: size += write_mod_rm_byte(file, mod, reg_field, 5); break;
		case RSI: size += write_mod_rm_byte(file, mod, reg_field, 6); break;
		case RDI: size += write_mod_rm_byte(file, mod, reg_field, 7); break;
		case RSP:
			// Same as above: SIB addressing
			size += write_mod_rm_byte(file, mod, reg_field, 4);
			size += write_u8(file, 0x24);
			break;
		default:
			UNIMPLEMENTED;
		}

		// Displacement byte/dword
		if (mod == 1)
			size += write_u8(file, (u8)offset);
		else
			size += write_u32(file, (u32)offset);

		return size;
	} else {
		UNIMPLEMENTED;
	}
}

typedef enum ArgOrder { INVALID, RM, MR } ArgOrder;

// Called into by the generated function "assemble_instr".
static u32 encode_instr(FILE *file, AsmInstr *instr, ArgOrder arg_order,
		i32 rex_prefix, u8 opcode_num, bool reg_and_rm, i32 opcode_extension,
		i32 immediate_size,  bool reg_in_opcode)
{
#if 0
	puts("Encoding instr:");
	dump_asm_instr(instr);
#endif

	u32 size = 0;

	if (rex_prefix != -1)
		size += write_u8(file, (u8)rex_prefix);

	if (reg_in_opcode) {
		assert(instr->num_args == 1);
		u8 reg;
		// @TODO: We do this like fifty times. Pull it out somewhere.
		switch (get_register(instr->args)) {
		case RAX: reg = 0; break;
		case RCX: reg = 1; break;
		case RDX: reg = 2; break;
		case RBX: reg = 3; break;
		case RSP: reg = 4; break;
		case RBP: reg = 5; break;
		case RSI: reg = 6; break;
		case RDI: reg = 7; break;
		default: UNIMPLEMENTED;
		}

		size += write_u8(file, opcode_num | reg);
		return size;
	}

	size += write_u8(file, opcode_num);

	if (reg_and_rm) {
		AsmArg *register_operand;
		AsmArg *memory_operand;
		if (arg_order == RM) {
			register_operand = instr->args;
			memory_operand = instr->args + 1;
		} else {
			memory_operand = instr->args;
			register_operand = instr->args + 1;
		}

		PhysicalRegister r_register = get_register(register_operand);

		u8 reg;
		switch (r_register) {
		case RAX: reg = 0; break;
		case RCX: reg = 1; break;
		case RDX: reg = 2; break;
		case RBX: reg = 3; break;
		case RSP: reg = 4; break;
		case RBP: reg = 5; break;
		case RSI: reg = 6; break;
		case RDI: reg = 7; break;
		default: UNIMPLEMENTED;
		}
		size += write_mod_rm_arg(file, memory_operand, reg);
	} else if (opcode_extension != -1) {
		size += write_mod_rm_arg(file, instr->args, opcode_extension);
	} else {
		// @NOTE: I'm not sure this is true in general, but it seems like there
		// are three cases:
		//   * The ModR/M byte contains a register and an r/m operand
		//   * The ModR/M byte contains an opcode extension and an r/m operand
		//   * The ModR/M byte is not present
		// This branch represents the third case. Since we already wrote the
		// opcode above there's nothing left to do.
	}

	// @TODO: This seems kinda redundant considering we already encode the
	// immediate size in AsmArg.
	if (immediate_size != -1) {
		u64 immediate;
		if (asm_arg_is_const(instr->args[0])) {
			immediate = instr->args[0].val.constant;
		} else {
			assert(asm_arg_is_const(instr->args[1]));
			immediate = instr->args[1].val.constant;
		}

		switch (immediate_size) {
		case  8: size += write_u8 (file,  (u8)immediate); break;
		case 16: size += write_u16(file, (u16)immediate); break;
		case 32: size += write_u32(file, (u32)immediate); break;
		case 64: size += write_u64(file, (u64)immediate); break;
		default: UNREACHABLE;
		}
	}

	return size;
}

// This is generated from "x64.enc", and defines the function "assemble_instr".
#include "x64.inc"

void assemble(AsmModule *asm_module, FILE *output_file,
		Array(AsmSymbol) *symbols, u64 base_virtual_address)
{
	u64 current_offset = base_virtual_address;

	for (u32 i = 0; i < asm_module->functions.size; i++) {
		AsmFunction *function = ARRAY_REF(&asm_module->functions, AsmFunction, i);

		u32 function_offset = current_offset;
		for (u32 j = 0; j < function->instrs.size; j++) {
			AsmInstr *instr = ARRAY_REF(&function->instrs, AsmInstr, j);
			current_offset += assemble_instr(output_file, instr);
		}
		u32 function_size = current_offset - function_offset;

		AsmSymbol *symbol = ARRAY_APPEND(symbols, AsmSymbol);
		symbol->name = function->name;
		// Offset is relative to the start of the section.
		symbol->offset = function_offset - base_virtual_address;
		symbol->size = function_size;
	}
}
