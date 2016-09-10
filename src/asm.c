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
			array_free(&function->prologue);
			array_free(&function->body);
			array_free(&function->epilogue);
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

	ARRAY_INIT(&function->prologue, AsmInstr, 10);
	ARRAY_INIT(&function->body, AsmInstr, 20);
	ARRAY_INIT(&function->epilogue, AsmInstr, 10);
	ARRAY_INIT(&function->labels, AsmLabel *, 10);
	function->ret_label = NULL;
}

AsmArg asm_vreg(u32 vreg_number, u8 width)
{
	AsmArg asm_arg = {
		.is_deref = false,
		.type = ASM_ARG_REGISTER,
		.val.reg.width = width,
		.val.reg.type = V_REG,
		.val.reg.val.vreg_number = vreg_number,
	};

	return asm_arg;
}

AsmArg asm_phys_reg(RegClass reg, u8 width)
{
	AsmArg asm_arg = {
		.is_deref = false,
		.type = ASM_ARG_REGISTER,
		.val.reg.width = width,
		.val.reg.type = PHYS_REG,
		.val.reg.val.class = reg,
	};

	return asm_arg;
}

AsmArg asm_deref(AsmArg asm_arg)
{
	asm_arg.is_deref = true;
	return asm_arg;
}

AsmArg asm_offset_reg(RegClass reg, u8 width, u64 offset)
{
	if (offset == 0) {
		return asm_deref(asm_phys_reg(reg, width));
	} else {
		AsmArg asm_arg = {
			.is_deref = false,
			.type = ASM_ARG_OFFSET_REGISTER,
			.val.offset_register.reg.width = width,
			.val.offset_register.reg.type = PHYS_REG,
			.val.offset_register.reg.val.class = reg,
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

extern inline bool asm_arg_is_const(AsmArg asm_arg);

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

	putchar(' ');
	dump_asm_args(instr->args, instr->num_args);
	putchar('\n');
}

#define X(x, b, d, w, o) { b, d, w, o }
static char *physical_register_names[][4] = {
	REG_CLASSES
};
#undef X

static void dump_register(Register reg)
{
	switch (reg.type) {
	case PHYS_REG: {
		char *reg_name = physical_register_names
				[reg.val.class]
				[lowest_set_bit(reg.width) - 3];
		for (u32 i = 0; reg_name[i] != '\0'; i++)
			putchar(tolower(reg_name[i]));
		break;
	}
	case V_REG:
		printf("#%u", reg.val.vreg_number);
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

void dump_asm_function(AsmFunction *asm_function)
{
	Array(AsmInstr) *instr_blocks[] = {
		&asm_function->prologue,
		&asm_function->body,
		&asm_function->epilogue,
	};

	for (u32 i = 0; i < STATIC_ARRAY_LENGTH(instr_blocks); i++) {
		Array(AsmInstr) *instr_block = instr_blocks[i];
		for (u32 j = 0; j < instr_block->size; j++) {
			AsmInstr *instr = ARRAY_REF(instr_block, AsmInstr, j);
			dump_asm_instr(instr);
		}
	}
}

void dump_asm_module(AsmModule *asm_module)
{
	for (u32 i = 0; i < asm_module->globals.size; i++) {
		AsmGlobal *global = *ARRAY_REF(&asm_module->globals, AsmGlobal *, i);

		if (!global->defined) {
			printf("extern %s\n", global->name);
		} else {
			if (global->type == ASM_GLOBAL_FUNCTION) {
				AsmFunction *func = &global->val.function;
				dump_asm_function(func);
			}
		}

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

static inline RegClass get_reg_class(AsmArg *arg)
{
	Register reg;
	if (arg->type == ASM_ARG_REGISTER) {
		reg = arg->val.reg;
	} else {
		assert(arg->type == ASM_ARG_OFFSET_REGISTER);
		reg = arg->val.offset_register.reg;
	}

	assert(reg.type == PHYS_REG);
	return reg.val.class;
}

static u32 encoded_register_number(RegClass reg)
{
	switch (reg) {
	case INVALID_REG_CLASS: UNREACHABLE;
	case REG_CLASS_A: return 0;
	case REG_CLASS_C: return 1;
	case REG_CLASS_D: return 2;
	case REG_CLASS_B: return 3;
	case REG_CLASS_SP: return 4;
	case REG_CLASS_BP: return 5;
	case REG_CLASS_SI: return 6;
	case REG_CLASS_DI: return 7;
	case REG_CLASS_R8:  return 8;
	case REG_CLASS_R9:  return 9;
	case REG_CLASS_R10: return 10;
	case REG_CLASS_R11: return 11;
	case REG_CLASS_R12: return 12;
	case REG_CLASS_R13: return 13;
	case REG_CLASS_R14: return 14;
	case REG_CLASS_R15: return 15;
	}
}

#define MAX_OPCODE_SIZE 2

typedef struct EncodedInstr
{
	u8 rex_prefix;
	u8 opcode_size;
	u8 opcode[MAX_OPCODE_SIZE];
	u8 opcode_extension;
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
		RegClass class = get_reg_class(arg);

		if (arg->is_deref) {
			switch (class) {
			case REG_CLASS_A: case REG_CLASS_C: case REG_CLASS_D:
			case REG_CLASS_B: case REG_CLASS_SI: case REG_CLASS_DI: {
				encoded_instr->mod = 0;
				encoded_instr->rm = encoded_register_number(class);
				return;
			}
			case REG_CLASS_SP: {
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
			case REG_CLASS_BP: {
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
			encoded_instr->rm = encoded_register_number(class);
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

		RegClass reg = get_reg_class(arg);

		switch (reg) {
		case REG_CLASS_A: case REG_CLASS_C: case REG_CLASS_D: case REG_CLASS_B:
		case REG_CLASS_BP: case REG_CLASS_SI: case REG_CLASS_DI: {
			encoded_instr->rm = encoded_register_number(reg);
			return;
		}
		case REG_CLASS_SP:
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

typedef enum RexPrefix
{
	REX_B = 1 << 0,
	REX_X = 1 << 1,
	REX_R = 1 << 2,
	REX_W = 1 << 3,
	REX_HIGH = 0x40,
} RexPrefix;

// Called by the generated function "assemble_instr".
// @TODO: Rename rex_prefix? It seems like the only REX prefix we can get
// passed is REX.W, all the rest are determined by us.
static void encode_instr(FILE *file, AsmModule *asm_module, AsmInstr *instr,
		ArgOrder arg_order, bool use_rex_w, u32 opcode_size, u8 opcode[],
		bool reg_and_rm, i32 opcode_extension, i32 immediate_size, bool reg_in_opcode)
{
	EncodedInstr encoded_instr;
	ZERO_STRUCT(&encoded_instr);
	encoded_instr.displacement_size = -1;
	encoded_instr.immediate_size = -1;

	if (use_rex_w)
		encoded_instr.rex_prefix |= REX_W;

	encoded_instr.opcode_size = opcode_size;
	memcpy(encoded_instr.opcode, opcode, opcode_size);
	if (reg_in_opcode) {
		encoded_instr.opcode_extension =
			encoded_register_number(get_reg_class(instr->args));
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
			encoded_instr.reg =
				encoded_register_number(get_reg_class(register_operand));
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
		if (encoded_instr.reg >= 8)
			encoded_instr.rex_prefix |= REX_R;
		if (encoded_instr.rm >= 8)
			encoded_instr.rex_prefix |= REX_B;
	}
	if (encoded_instr.has_sib) {
		if (encoded_instr.index >= 8)
			encoded_instr.rex_prefix |= REX_X;
		if (encoded_instr.base >= 8) {
			// Make sure we didn't already use REX.B for the RM field.
			assert((encoded_instr.rex_prefix & REX_B) == 0);
			encoded_instr.rex_prefix |= REX_B;
		}
	}
	if (encoded_instr.opcode_extension >= 8) {
		// Make sure we didn't already use REX.B for the RM field or SIB.
		assert((encoded_instr.rex_prefix & REX_B) == 0);
		encoded_instr.rex_prefix |= REX_B;
	}
	if (encoded_instr.rex_prefix != 0) {
		encoded_instr.rex_prefix |= REX_HIGH;
		write_u8(file, encoded_instr.rex_prefix);
	}
	encoded_instr.opcode[0] |= encoded_instr.opcode_extension & 7;
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

			Array(AsmInstr) *instr_blocks[] = {
				&function->prologue,
				&function->body,
				&function->epilogue,
			};
			for (u32 i = 0; i < STATIC_ARRAY_LENGTH(instr_blocks); i++) {
				Array(AsmInstr) *instr_block = instr_blocks[i];

				for (u32 j = 0; j < instr_block->size; j++) {
					AsmInstr *instr = ARRAY_REF(instr_block, AsmInstr, j);
					if (instr->label != NULL) {
						instr->label->file_location = checked_ftell(output_file);
					}
					assemble_instr(output_file, asm_module, instr);
				}
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
