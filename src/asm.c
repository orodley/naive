#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "array.h"
#include "asm.h"
#include "asm_gen.h"
#include "file.h"
#include "util.h"

void init_asm_module(AsmModule *asm_module)
{
	ARRAY_INIT(&asm_module->globals, AsmGlobal *, 10);
	ARRAY_INIT(&asm_module->fixups, Fixup, 10);

	pool_init(&asm_module->pool, 1024);
}

void free_asm_module(AsmModule *asm_module)
{
	for (u32 i = 0; i < asm_module->globals.size; i++) {
		AsmGlobal *global = *ARRAY_REF(&asm_module->globals, AsmGlobal *, i);

		if (global->type == ASM_GLOBAL_FUNCTION) {
			AsmFunction *function = &global->val.function;
			array_free(&function->instrs);
			array_free(&function->labels);
		}
	}
	array_free(&asm_module->globals);
	array_free(&asm_module->fixups);

	pool_free(&asm_module->pool);
}

void init_asm_function(AsmFunction *function, char *name)
{
	function->name = name;

	ARRAY_INIT(&function->instrs, AsmInstr, 20);
	ARRAY_INIT(&function->labels, AsmLabel *, 10);
}

AsmArg asm_virtual_register(u32 n)
{
	AsmArg asm_arg = {
		.is_deref = false,
		.type = ASM_ARG_REGISTER,
		.val.reg.type = VIRTUAL_REGISTER,
		.val.reg.val.register_number = n,
	};

	return asm_arg;
}

AsmArg asm_physical_register(PhysicalRegister reg)
{
	AsmArg asm_arg = {
		.is_deref = false,
		.type = ASM_ARG_REGISTER,
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
			.type = ASM_ARG_OFFSET_REGISTER,
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
		.type = ASM_ARG_CONST32,
		.val.constant = constant,
	};

	return asm_arg;
}

AsmArg asm_label(AsmLabel *label)
{
	AsmArg asm_arg = {
		.is_deref = false,
		.type = ASM_ARG_LABEL,
		.val.label = label,
	};

	return asm_arg;
}

AsmArg asm_global(AsmGlobal *global)
{
	AsmArg asm_arg = {
		.is_deref = false,
		.type = ASM_ARG_GLOBAL,
		.val.global = global,
	};

	return asm_arg;
}

static inline bool asm_arg_is_const(AsmArg asm_arg)
{
	return (asm_arg.type == ASM_ARG_CONST8) || (asm_arg.type == ASM_ARG_CONST16) ||
		(asm_arg.type == ASM_ARG_CONST32) || (asm_arg.type == ASM_ARG_CONST64);
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
	if (instr->label != NULL)
		printf("%s:\n", instr->label->name);

	putchar('\t');
	char *op_name = asm_op_names[instr->op];
	for (u32 i = 0; op_name[i] != '\0'; i++)
		putchar(tolower(op_name[i]));

	switch (instr->op) {
	case MOV: case XOR: case ADD: case SUB: case IMUL: case CMP:
		putchar(' ');
		dump_asm_args(instr->args, 2);
		break;
	case PUSH: case POP: case CALL: case JMP: case JE:
		putchar(' ');
		dump_asm_args(instr->args, 1);
		break;
	case RET:
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
		case ASM_ARG_REGISTER:
			dump_register(arg->val.reg);
			break;
		case ASM_ARG_OFFSET_REGISTER:
			dump_register(arg->val.offset_register.reg);
			printf(" + %" PRIu64, arg->val.offset_register.offset);
			break;
		case ASM_ARG_LABEL:
			printf("%s", arg->val.label->name);
			break;
		case ASM_ARG_GLOBAL:
			printf("%s", arg->val.global->name);
			break;
		case ASM_ARG_CONST8:
			if ((i8)arg->val.constant < 0)
				printf("%" PRId8, (i8)arg->val.constant);
			else
				printf("%" PRIu8, (i8)arg->val.constant);
		case ASM_ARG_CONST16:
			if ((i16)arg->val.constant < 0)
				printf("%" PRId16, (i16)arg->val.constant);
			else
				printf("%" PRIu16, (i16)arg->val.constant);
		case ASM_ARG_CONST32:
			if ((i32)arg->val.constant < 0)
				printf("%" PRId32, (i32)arg->val.constant);
			else
				printf("%" PRIu32, (i32)arg->val.constant);
			break;
		case ASM_ARG_CONST64:
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
	for (u32 i = 0; i < asm_module->globals.size; i++) {
		AsmGlobal *global = *ARRAY_REF(&asm_module->globals, AsmGlobal *, i);

		if (global->type == ASM_GLOBAL_FUNCTION) {
			AsmFunction *func = &global->val.function;

			for (u32 j = 0; j < func->instrs.size; j++) {
				AsmInstr *instr = ARRAY_REF(&func->instrs, AsmInstr, j);
				dump_asm_instr(instr);
			}
		}

		if (i == asm_module->globals.size - 1)
			putchar('\n');
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
	if (arg->type == ASM_ARG_REGISTER) {
		reg = arg->val.reg;
	} else {
		assert(arg->type == ASM_ARG_OFFSET_REGISTER);
		reg = arg->val.offset_register.reg;
	}

	assert(reg.type == PHYSICAL_REGISTER);
	return reg.val.physical_register;
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
	case R8:  return 8;
	case R9:  return 9;
	case R10: return 10;
	case R11: return 11;
	case R12: return 12;
	case R13: return 13;
	case R14: return 14;
	case R15: return 15;
	default: UNIMPLEMENTED;
	}
}

#define MAX_OPCODE_SIZE 2

typedef struct EncodedInstr
{
	u8 rex_prefix;
	u8 opcode_size;
	u8 opcode[MAX_OPCODE_SIZE];
	bool has_modrm;
	u8 mod;
	u8 reg;
	u8 rm;
	bool has_sib;
	u8 scale;
	u8 index;
	u8 base;
	i8 displacement_size;
	u64 displacement;
	i8 immediate_size;
	u64 immediate;
} EncodedInstr;

static void add_mod_rm_arg(EncodedInstr *encoded_instr, AsmArg *arg)
{
	encoded_instr->has_modrm = true;

	if (arg->type == ASM_ARG_REGISTER) {
		PhysicalRegister reg = get_register(arg);

		if (arg->is_deref) {
			switch (reg) {
			case RAX: case RCX: case RDX: case RBX: case RSI: case RDI: {
				encoded_instr->mod = 0;
				encoded_instr->rm = encoded_register_number(reg);
				return;
			}
			case RSP: {
				// Mod = 0, R/M = 4 means SIB addressing
				encoded_instr->mod = 0;
				encoded_instr->rm = 4;
				// RSP base with no index/scale
				encoded_instr->has_sib = true;
				encoded_instr->scale = 0;
				encoded_instr->index = 4;
				encoded_instr->base = 4;

				return;
			}
			case RBP: {
				// Mod = 1, R/M = 5 means RBP + disp8
				encoded_instr->mod = 1;
				encoded_instr->rm = 5;
				encoded_instr->displacement_size = 1;
				encoded_instr->displacement = 0;

				return;
			}
			default: UNIMPLEMENTED;
			}
		} else {
			encoded_instr->mod = 3;
			encoded_instr->rm = encoded_register_number(reg);
			return;
		}
	} else if (arg->type == ASM_ARG_OFFSET_REGISTER) {
		assert(arg->is_deref);

		u64 offset = arg->val.offset_register.offset;
		// @TODO: Negative numbers
		if ((offset & 0xFF) == offset)
			encoded_instr->mod = 1;
		else if ((offset & 0xFFFFFFFF) == offset)
			encoded_instr->mod = 2;
		else
			assert(!"Offset too large!");

		PhysicalRegister reg = get_register(arg);

		switch (reg) {
		case RAX: case RCX: case RDX: case RBX: case RBP: case RSI: case RDI: {
			encoded_instr->rm = encoded_register_number(reg);
			return;
		}
		case RSP:
			// Same as above: SIB addressing
			encoded_instr->rm = 4;
			encoded_instr->has_sib = true;
			encoded_instr->scale = 0;
			encoded_instr->index = 4;
			encoded_instr->base = 4;
			break;
		default:
			UNIMPLEMENTED;
		}

		if (encoded_instr->mod == 1)
			encoded_instr->displacement_size = 1;
		else
			encoded_instr->displacement_size = 4;
		encoded_instr->displacement = offset;
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
// @TODO: Rename rex_prefix? It seems like the only REX prefix we can get
// passed is REX.W, all the rest are determined by us.
static void encode_instr(FILE *file, AsmModule *asm_module, AsmInstr *instr,
		ArgOrder arg_order, i32 rex_prefix, u32 opcode_size, u8 opcode[],
		bool reg_and_rm, i32 opcode_extension, i32 immediate_size, bool reg_in_opcode)
{
	EncodedInstr encoded_instr;
	ZERO_STRUCT(&encoded_instr);
	encoded_instr.displacement_size = -1;
	encoded_instr.immediate_size = -1;

	if (rex_prefix != -1)
		encoded_instr.rex_prefix = (u8)rex_prefix;

	encoded_instr.opcode_size = opcode_size;
	memcpy(encoded_instr.opcode, opcode, opcode_size);
	if (reg_in_opcode) {
		u8 reg = encoded_register_number(get_register(instr->args));
		assert(reg <= 8);
		encoded_instr.opcode[0] |= reg;
	}

	if (reg_and_rm) {
		AsmArg *register_operand;
		AsmArg *memory_operand;
		if (arg_order == RM) {
			register_operand = instr->args;
			memory_operand = instr->args + 1;
		} else if (arg_order == MR) {
			memory_operand = instr->args;
			register_operand = instr->args + 1;
		} else if (instr->num_args == 1) {
			memory_operand = instr->args;
			register_operand = NULL;
		} else {
			UNREACHABLE;
		}

		if (register_operand == NULL) {
			encoded_instr.reg = 0;
		} else {
			PhysicalRegister r_register = get_register(register_operand);
			encoded_instr.reg = encoded_register_number(r_register);
		}
		add_mod_rm_arg(&encoded_instr, memory_operand);
	} else if (opcode_extension != -1) {
		encoded_instr.reg = opcode_extension;
		add_mod_rm_arg(&encoded_instr, instr->args);
	} else {
		// @NOTE: I'm not sure this is true in general, but it seems like there
		// are three cases:
		//   * The ModR/M byte contains a register and an r/m operand
		//   * The ModR/M byte contains an opcode extension and an r/m operand
		//   * The ModR/M byte is not present
		// This branch represents the third case. Since we already wrote the
		// opcode above there's nothing left to do.
	}

	Fixup *fixup = NULL;

	// @TODO: This seems kinda redundant considering we already encode the
	// immediate size in AsmArg.
	if (immediate_size != -1) {
		encoded_instr.immediate_size = immediate_size;
		AsmArg* immediate_arg = NULL;
		for (u32 i = 0; i < instr->num_args; i++) {
			if (asm_arg_is_const(instr->args[i])
					|| instr->args[i].type == ASM_ARG_LABEL
					|| instr->args[i].type == ASM_ARG_GLOBAL) {
				// Check that we only have one immediate.
				assert(immediate_arg == NULL);
				immediate_arg = instr->args + i;
			}
		}
		assert(immediate_arg != NULL);

		if (asm_arg_is_const(*immediate_arg)) {
			encoded_instr.immediate = immediate_arg->val.constant;
		} else if (immediate_arg->type == ASM_ARG_LABEL ||
				immediate_arg->type == ASM_ARG_GLOBAL) {
			fixup = ARRAY_APPEND(&asm_module->fixups, Fixup);

			fixup->size_bytes = 4;
			if (immediate_arg->type == ASM_ARG_LABEL) {
				fixup->type = FIXUP_LABEL;
				fixup->val.label = immediate_arg->val.label;
			} else {
				fixup->type = FIXUP_GLOBAL;
				fixup->val.global = immediate_arg->val.global;
			}

			// Dummy value, gets patched later.
			encoded_instr.immediate = 0;
		} else {
			assert(asm_arg_is_const(*immediate_arg));
			encoded_instr.immediate = immediate_arg->val.constant;
		}
	}


	if (encoded_instr.has_modrm) {
		if ((encoded_instr.reg & (1 << 3)) != 0)
			encoded_instr.rex_prefix |= 1 << 2;
		if ((encoded_instr.rm & (1 << 3)) != 0)
			encoded_instr.rex_prefix |= 1 << 0;
	}
	if (encoded_instr.has_sib) {
		if ((encoded_instr.index & (1 << 3)) != 0)
			encoded_instr.rex_prefix |= 1 << 1;
		if ((encoded_instr.base & (1 << 3)) != 0) {
			// Make sure we didn't already use REX.B for the RM field.
			assert((encoded_instr.rex_prefix & (1 << 0)) == 0);
			encoded_instr.rex_prefix |= 1 << 0;
		}
	}
	if (encoded_instr.rex_prefix != 0)
		write_u8(file, encoded_instr.rex_prefix);
	write_bytes(file, encoded_instr.opcode_size, encoded_instr.opcode);
	if (encoded_instr.has_modrm) {
		u8 mod = encoded_instr.mod;
		u8 reg = encoded_instr.reg;
		u8 rm = encoded_instr.rm;
		u8 mod_rm_byte = ((mod & 3) << 6) | ((reg & 7) << 3) | (rm & 7);

		write_u8(file, mod_rm_byte);
	}
	if (encoded_instr.has_sib) {
		u8 scale = encoded_instr.scale;
		u8 index = encoded_instr.index;
		u8 base = encoded_instr.base;
		u8 sib_byte = ((scale  & 3) << 6) | ((index & 7) << 3) | (base & 7);
		write_u8(file, sib_byte);
	}
	if (encoded_instr.displacement_size != -1)
		write_int(file, encoded_instr.displacement, encoded_instr.displacement_size);
	if (fixup != NULL)
		fixup->file_location = (u32)checked_ftell(file);
	if (encoded_instr.immediate_size != -1)
		write_int(file, encoded_instr.immediate, encoded_instr.immediate_size);
}

// This is generated from "x64.enc", and defines the function "assemble_instr".
#include "x64.inc"

void assemble(AsmModule *asm_module, FILE *output_file, Array(AsmSymbol) *symbols)
{
	u64 initial_file_location = checked_ftell(output_file);

	for (u32 i = 0; i < asm_module->globals.size; i++) {
		AsmGlobal *global = *ARRAY_REF(&asm_module->globals, AsmGlobal *, i);
		if (global->type != ASM_GLOBAL_FUNCTION)
			continue;
		AsmFunction *function = &global->val.function;

		u32 function_start = initial_file_location;
		u32 function_size = 0;
		if (global->defined) {
			function_start = checked_ftell(output_file);
			for (u32 j = 0; j < function->instrs.size; j++) {
				AsmInstr *instr = ARRAY_REF(&function->instrs, AsmInstr, j);
				if (instr->label != NULL) {
					instr->label->file_location = checked_ftell(output_file);
				}
				assemble_instr(output_file, asm_module, instr);
			}
			function_size = checked_ftell(output_file) - function_start;
		}

		AsmSymbol *symbol = ARRAY_APPEND(symbols, AsmSymbol);
		symbol->defined = global->defined;
		symbol->name = function->name;
		// Offset is relative to the start of the section.
		symbol->offset = function_start - initial_file_location;
		symbol->size = function_size;
		// Add one to account for 0 = undef symbol index
		symbol->symtab_index = i + 1;

		global->symbol = symbol;
	}
	u64 final_file_position = checked_ftell(output_file);

	// @TODO: Emit relocations here instead?
	for (u32 i = 0; i < asm_module->fixups.size; i++) {
		Fixup *fixup = ARRAY_REF(&asm_module->fixups, Fixup, i);
		u32 file_location;
		if (fixup->type == FIXUP_GLOBAL) {
			AsmGlobal *global = fixup->val.global;
			if (global->defined)
				file_location = global->offset + initial_file_location;
			else
				continue;
		} else {
			file_location = fixup->val.label->file_location;
		}

		// Relative accesses are relative to the start of the next instruction,
		// so we add on the size of the reference itself first.
		i32 offset = (i32)file_location -
			(i32)(fixup->file_location + fixup->size_bytes);
		checked_fseek(output_file, fixup->file_location, SEEK_SET);
		write_int(output_file, (u64)offset, fixup->size_bytes);
	}

	int ret = fseek(output_file, final_file_position, SEEK_SET);
	assert(ret == 0);
}
