#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "array.h"
#include "asm.h"
#include "asm_gen.h"
#include "file.h"

void init_asm_module(AsmModule *asm_module)
{
	ARRAY_INIT(&asm_module->functions, AsmFunction, 10);
	ARRAY_INIT(&asm_module->globals, AsmGlobal *, 10);
	ARRAY_INIT(&asm_module->global_references, GlobalReference, 10);

	pool_init(&asm_module->pool, 1024);
}

void free_asm_module(AsmModule *asm_module)
{
	for (u32 i = 0; i < asm_module->functions.size; i++)
		array_free(ARRAY_REF(&asm_module->functions, Array(AsmFunction), i));
	array_free(&asm_module->functions);
	array_free(&asm_module->globals);
	array_free(&asm_module->global_references);

	pool_free(&asm_module->pool);
}

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

AsmArg asm_global(AsmGlobal *global)
{
	AsmArg asm_arg = {
		.is_deref = false,
		.type = GLOBAL,
		.val.global = global,
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
	u64 truncated;

	// Handle this case specially, as 1 << 64 - 1 doesn't work to get a 64-bit
	// mask.
	if (bits == 64)
		truncated = constant;
	else
		truncated = constant & ((1ull << bits) - 1);
	return truncated == constant;
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
	case MOV: case XOR: case ADD: case SUB: case IMUL:
		putchar(' ');
		dump_asm_args(instr->args, 2);
		break;
	case PUSH: case POP: case CALL:
		putchar(' ');
		dump_asm_args(instr->args, 1);
		break;
	case RET:
		// Print an extra newline, to visually separate functions
		putchar('\n');
		break;
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
		case GLOBAL:
			printf("$%s", arg->val.global->name);
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

static inline void write_u8(FILE *file, u8 x)
{
	size_t items_written = fwrite(&x, 1, 1, file);
	assert(items_written == 1);
}

#if 0
static inline void write_u16(FILE *file, u16 x)
{
	u8 out[] = {
		(x >> 0) & 0xFF,
		(x >> 8) & 0xFF,
	};

	size_t items_written = fwrite(out, 1, sizeof out, file);
	assert(items_written == sizeof out);
}
#endif

static inline void write_u32(FILE *file, u32 x)
{
	u8 out[] = {
		(x >>  0) & 0xFF,
		(x >>  8) & 0xFF,
		(x >> 16) & 0xFF,
		(x >> 24) & 0xFF,
	};

	size_t items_written = fwrite(out, 1, sizeof out, file);
	assert(items_written == sizeof out);
}

#if 0
static inline void write_u64(FILE *file, u64 x)
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

	size_t items_written = fwrite(out, 1, sizeof out, file);
	assert(items_written == sizeof out);
}
#endif

static inline void write_int(FILE *file, u64 x, u32 size)
{
	for (u32 n = 0; n < size; n ++) {
		u8 byte = (x >> (n * 8)) & 0xFF;
		size_t items_written = fwrite(&byte, 1, 1, file);
		assert(items_written == 1);
	}
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

static inline void write_mod_rm_byte(FILE *file, u8 mod, u8 reg, u8 rm)
{
	u8 mod_rm_byte = (mod << 6) | (reg << 3) | rm;
	write_u8(file, mod_rm_byte);
}

static u32 encoded_register_number(PhysicalRegister reg)
{
	switch (reg) {
	case RAX: return 0;
	case RCX: return 1;
	case RDX: return 2;
	case RBX: return 3;
	case RSP: return 4;
	case RBP: return 5;
	case RSI: return 6;
	case RDI: return 7;
	default: UNIMPLEMENTED;
	}
}

static void write_mod_rm_arg(FILE *file, AsmArg *arg, u8 reg_field)
{
	if (arg->type == REGISTER) {
		PhysicalRegister reg = get_register(arg);

		if (arg->is_deref) {
			switch (reg) {
			case RAX: case RCX: case RDX: case RBX: case RSI: case RDI: {
				u8 encoded_reg = encoded_register_number(reg);
				write_mod_rm_byte(file, 0, reg_field, encoded_reg);
				return;
			}
			case RSP: {
				// Mod = 0, R/M = 4 means SIB addressing
				write_mod_rm_byte(file, 0, reg_field, 4);
				// SIB byte, with RSP as base and no index/scale
				write_u8(file, 0x24);

				return;
			}
			case RBP: {
				// Mod = 1, R/M = 5 means RBP + disp8
				write_mod_rm_byte(file, 1, reg_field, 5);
				// 0 displacement
				write_u8(file, 0);

				return;
			}
			default: UNIMPLEMENTED;
			}
		} else {
			u8 encoded_reg = encoded_register_number(reg);
			write_mod_rm_byte(file, 3, reg_field, encoded_reg);
			return;
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

		PhysicalRegister reg = get_register(arg);

		switch (reg) {
		case RAX: case RCX: case RDX: case RBX: case RBP: case RSI: case RDI: {
			u8 encoded_reg = encoded_register_number(reg);
			write_mod_rm_byte(file, mod, reg_field, encoded_reg);
			return;
		}
		case RSP:
			// Same as above: SIB addressing
			write_mod_rm_byte(file, mod, reg_field, 4);
			write_u8(file, 0x24);
			break;
		default:
			UNIMPLEMENTED;
		}

		// Displacement byte/dword
		if (mod == 1)
			write_u8(file, (u8)offset);
		else
			write_u32(file, (u32)offset);
	} else {
		UNIMPLEMENTED;
	}
}

typedef enum ArgOrder { INVALID, RM, MR } ArgOrder;

static inline void write_bytes(FILE *file, u32 size, u8 *bytes)
{
	size_t items_written = fwrite(bytes, 1, size, file);
	assert(items_written == size);
}

// Called by the generated function "assemble_instr".
static void encode_instr(FILE *file, AsmModule *asm_module, AsmInstr *instr,
		ArgOrder arg_order, i32 rex_prefix, u32 opcode_size, u8 opcode[],
		bool reg_and_rm, i32 opcode_extension, i32 immediate_size, bool reg_in_opcode)
{
#if 0
	puts("Encoding instr:");
	dump_asm_instr(instr);
#endif

	if (rex_prefix != -1)
		write_u8(file, (u8)rex_prefix);

	if (reg_in_opcode) {
		assert(opcode_size == 1);
		u8 reg = encoded_register_number(get_register(instr->args));
		write_u8(file, opcode[0] | reg);
	} else {
		write_bytes(file, opcode_size, opcode);
	}

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
		u8 reg = encoded_register_number(r_register);
		write_mod_rm_arg(file, memory_operand, reg);
	} else if (opcode_extension != -1) {
		write_mod_rm_arg(file, instr->args, opcode_extension);
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
		AsmArg* immediate_arg = NULL;
		for (u32 i = 0; i < instr->num_args; i++) {
			if (asm_arg_is_const(instr->args[i]) || instr->args[i].type == GLOBAL) {
				// Check that we only have one immediate.
				assert(immediate_arg == NULL);
				immediate_arg = instr->args + i;
			}
		}
		assert(immediate_arg != NULL);

		u64 immediate;
		if (asm_arg_is_const(*immediate_arg)) {
			immediate = immediate_arg->val.constant;
		} else if (immediate_arg->type == GLOBAL) {
			GlobalReference *ref =
				ARRAY_APPEND(&asm_module->global_references, GlobalReference);

			ref->file_location = (u32)checked_ftell(file);
			ref->size_bytes = 4;
			ref->global = immediate_arg->val.global;

			// Dummy value, gets patched later.
			immediate = 0;
		} else {
			assert(asm_arg_is_const(*immediate_arg));
			immediate = immediate_arg->val.constant;
		}

		write_int(file, immediate, immediate_size);
	}
}

// This is generated from "x64.enc", and defines the function "assemble_instr".
#include "x64.inc"

void assemble(AsmModule *asm_module, FILE *output_file,
		Array(AsmSymbol) *symbols, u64 base_virtual_address)
{
	u64 current_offset = base_virtual_address;
	u64 initial_file_location = checked_ftell(output_file);

	for (u32 i = 0; i < asm_module->functions.size; i++) {
		AsmFunction *function = ARRAY_REF(&asm_module->functions, AsmFunction, i);

		u32 function_offset = current_offset;
		for (u32 j = 0; j < function->instrs.size; j++) {
			AsmInstr *instr = ARRAY_REF(&function->instrs, AsmInstr, j);
			assemble_instr(output_file, asm_module, instr);

			current_offset = base_virtual_address +
				checked_ftell(output_file) - initial_file_location;
		}
		u32 function_size = current_offset - function_offset;

		AsmSymbol *symbol = ARRAY_APPEND(symbols, AsmSymbol);
		symbol->name = function->name;
		// Offset is relative to the start of the section.
		symbol->offset = function_offset - base_virtual_address;
		symbol->size = function_size;

		AsmGlobal *corresponding_global =
			*ARRAY_REF(&asm_module->globals, AsmGlobal *, i);
		corresponding_global->symbol = symbol;
	}
	u64 final_file_position = checked_ftell(output_file);

	for (u32 i = 0; i < asm_module->global_references.size; i++) {
		GlobalReference *ref =
			ARRAY_REF(&asm_module->global_references, GlobalReference, i);

		int ret = fseek(output_file, ref->file_location, SEEK_SET);
		assert(ret == 0);

		AsmSymbol *referenced_symbol = ref->global->symbol;

		// Relative accesses are relative to the start of the next instruction,
		// so we add on the size of the reference itself first.
		u32 position_in_section =
			(ref->file_location + ref->size_bytes) - initial_file_location;
		i32 offset = (i32)referenced_symbol->offset - (i32)position_in_section;
		write_int(output_file, (u64)offset, ref->size_bytes);
	}

	int ret = fseek(output_file, final_file_position, SEEK_SET);
	assert(ret == 0);
}
