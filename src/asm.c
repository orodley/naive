#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "array.h"
#include "asm.h"
#include "asm_gen.h"
#include "file.h"
#include "util.h"

void init_asm_module(AsmModule *asm_module, char *input_file_name)
{
	asm_module->input_file_name = input_file_name;
	pool_init(&asm_module->pool, 1024);

	ARRAY_INIT(&asm_module->text_section.symbols, AsmSymbol *, 20);
	ARRAY_INIT(&asm_module->text_section.instrs, AsmInstr, 100);

	ARRAY_INIT(&asm_module->data_section.symbols, AsmSymbol *, 20);
	ARRAY_INIT(&asm_module->data_section.data, u8, 100);

	ARRAY_INIT(&asm_module->bss_section.symbols, AsmSymbol *, 20);
	asm_module->bss_section.size = 0;

	ARRAY_INIT(&asm_module->externs, AsmSymbol *, 5);
	ARRAY_INIT(&asm_module->fixups, Fixup, 10);
}

void free_asm_module(AsmModule *asm_module)
{
	array_free(&asm_module->text_section.instrs);
	array_free(&asm_module->text_section.symbols);

	array_free(&asm_module->data_section.data);
	array_free(&asm_module->data_section.symbols);

	array_free(&asm_module->bss_section.symbols);

	array_free(&asm_module->externs);

	array_free(&asm_module->fixups);

	pool_free(&asm_module->pool);
}

AsmValue asm_vreg(u32 vreg_number, u8 width)
{
	return (AsmValue) {
		.is_deref = false,
		.t = ASM_VALUE_REGISTER,
		.u.reg.width = width,
		.u.reg.t = V_REG,
		.u.reg.u.vreg_number = vreg_number,
	};
}

AsmValue asm_phys_reg(RegClass reg, u8 width)
{
	return (AsmValue) {
		.is_deref = false,
		.t = ASM_VALUE_REGISTER,
		.u.reg.width = width,
		.u.reg.t = PHYS_REG,
		.u.reg.u.class = reg,
	};
}

AsmValue asm_deref(AsmValue asm_value)
{
	asm_value.is_deref = true;
	return asm_value;
}

AsmValue asm_offset_reg(RegClass reg, u8 width, AsmConst offset)
{
	if (offset.t == ASM_CONST_IMMEDIATE && offset.u.immediate == 0) {
		return asm_phys_reg(reg, width);
	} else {
		return (AsmValue) {
			.is_deref = false,
			.t = ASM_VALUE_OFFSET_REGISTER,
			.u.offset_register.reg.width = width,
			.u.offset_register.reg.t = PHYS_REG,
			.u.offset_register.reg.u.class = reg,
			.u.offset_register.offset = offset,
		};
	}
}

AsmValue asm_const(u64 constant)
{
	return (AsmValue) {
		.is_deref = false,
		.t = ASM_VALUE_CONST,
		.u.constant = (AsmConst) {
			.t = ASM_CONST_IMMEDIATE,
			.u.immediate = constant,
		},
	};
}

AsmValue asm_symbol(AsmSymbol *symbol)
{
	return (AsmValue) {
		.is_deref = false,
		.t = ASM_VALUE_CONST,
		.u.constant = (AsmConst) {
			.t = ASM_CONST_SYMBOL,
			.u.symbol = symbol,
		},
	};
}

// Certain instructions sign-extend their immediates. We need to know which
// ones so that we can correctly determine the width we need for the immediate.
static bool is_sign_extending_instr(AsmInstr *instr)
{
	AsmOp op = instr->op;
	return op == ADD || op == AND || op == ADC || op == CMP || op == IMUL
		|| op == OR || op == SBB || op == SUB || op == TEST;
}

// We need to deal with negative numbers here. For example, -4 should fit into
// an 8-bit immediate, even though as a 64-bit int (which is how we store
// constants) this is 0xFFFFFFFFFFFFFFFC, which looks like it doesn't fit into
// 8 bits. Really the issue here isn't negative vs. positive numbers though -
// numbers are just patterns of bits, and all we care about is that we get the
// correct pattern of bits out the end. To deal with this, all we need is:
// * A canonical pattern of bits that we expect to end up with.
// * The bit width we're trying to fit into.
// * The bit width we'll be implicitly extended to, e.g.: for ADD r/m64, imm8
//   the immediate will be extended to 64 bits.
// * Whether the immediate is sign-extended. This is an important point because
//   without it we try to put 255 into an 8-bit immediate, and if it's
//   sign-extended to a larger width we end up with a different value since the
//   MSB is 1.
//
// Then to check if it fits we can do something like this:
// a = canonical input value
// b = truncate a to ext_width
// c = truncate a to imm_width
// d = sign- or zero-extend c to ext_width
// a fits iff d == b
static bool is_const_and_fits(AsmValue asm_value, u32 ext_width,
		u32 imm_width, bool sext)
{
	assert(ext_width >= imm_width);

	if (asm_value.t != ASM_VALUE_CONST) {
		return false;
	}

	AsmConst constant = asm_value.u.constant;
	switch (constant.t) {
	case ASM_CONST_IMMEDIATE: {
		// Handle this case specially, as various bitwise calculations are
		// harder if we don't have any extra bits to work with.
		if (imm_width == 64)
			return true;

		u64 imm = constant.u.immediate;
		u64 canonical_value;

		if (ext_width == 64) {
			canonical_value = imm;
		} else {
			// Sanity check the truncation - our canonical value should always
			// fit into ext_width, e.g.: we shouldn't be trying to add 1<<63 to
			// a 32-bit value. "fitting" here means either the bits we will
			// truncate are all zero, or they are all one and the MSB is one,
			// so sign-extension would produce the same value.
			u64 ext_trunc_bits = imm >> ext_width;
			assert(ext_trunc_bits == 0
					|| (ext_trunc_bits == (1ULL << (64 - ext_width)) - 1)
						&& ((imm >> (ext_width - 1)) & 1) == 1);

			canonical_value = imm & ((1ULL << ext_width) - 1);
		}

		u64 truncated = imm & ((1ULL << imm_width) - 1);
		u64 extended = truncated;
		if (sext && ((truncated & (1ULL << (imm_width - 1))) != 0)) {
			extended |= ((1ULL << (ext_width - imm_width)) - 1) << imm_width;
		}
		return canonical_value == extended;
	}
	case ASM_CONST_SYMBOL:
		return imm_width == 64;
	}
}

#define X(x) #x
static char *asm_op_names[] = {
	ASM_OPS
};
#undef X

#define X(x, b, d, w, o) { b, d, w, o }
static char *physical_register_names[][4] = {
	REG_CLASSES
};
#undef X

static void dump_register(Register reg)
{
	switch (reg.t) {
	case PHYS_REG: {
		char *reg_name = physical_register_names
				[reg.u.class]
				[lowest_set_bit(reg.width) - 3];
		for (u32 i = 0; reg_name[i] != '\0'; i++)
			putchar(tolower(reg_name[i]));
		break;
	}
	case V_REG:
		printf("#%u", reg.u.vreg_number);
		break;
	}
}

static void dump_asm_const(AsmConst constant)
{
	switch (constant.t) {
	case ASM_CONST_IMMEDIATE: {
		u64 x = constant.u.immediate;
		if ((i64)x < 0)
			printf("%" PRId64, (i64)x);
		else
			printf("%" PRIu64, (i64)x);
		break;
	}
	case ASM_CONST_SYMBOL:
		printf("%s", constant.u.symbol->name);
		break;
	default:
		UNREACHABLE;
	}
}

static void dump_symbol(AsmSymbol *symbol)
{
	char *name = symbol->name;
	if (symbol->linkage == ASM_GLOBAL_LINKAGE)
		printf("global %s\n", name);
	printf("%s:\n", name);
}

static void dump_asm_instr(AsmInstr *instr)
{
	if (instr->label != NULL)
		dump_symbol(instr->label);

	putchar('\t');
	char *op_name = asm_op_names[instr->op];
	for (u32 i = 0; op_name[i] != '\0'; i++)
		putchar(tolower(op_name[i]));

	putchar(' ');

	for (u32 i = 0; i < instr->arity; i++) {
		if (i != 0)
			fputs(", ", stdout);

		AsmValue *arg = instr->args + i;
		if (arg->is_deref)
			putchar('[');
		switch (arg->t) {
		case ASM_VALUE_REGISTER:
			dump_register(arg->u.reg);
			break;
		case ASM_VALUE_OFFSET_REGISTER:
			dump_register(arg->u.offset_register.reg);
			fputs(" + ", stdout);
			dump_asm_const(arg->u.offset_register.offset);
			break;
		case ASM_VALUE_CONST:
			dump_asm_const(arg->u.constant);
			break;
		}
		if (arg->is_deref)
			putchar(']');
	}
	putchar('\n');
}

void dump_asm_module(AsmModule *asm_module)
{
	for (u32 i = 0; i < asm_module->externs.size; i++) {
		AsmSymbol *symbol = *ARRAY_REF(&asm_module->externs, AsmSymbol *, i);
		printf("extern %s\n", symbol->name);
	}
	putchar('\n');

	puts("section .text");
	Array(AsmInstr) *instrs = &asm_module->text_section.instrs;
	for (u32 i = 0; i < instrs->size; i++) {
		AsmInstr *instr = ARRAY_REF(instrs, AsmInstr, i);
		dump_asm_instr(instr);
	}
	putchar('\n');

	puts("section .data");
	Array(u8) *data = &asm_module->data_section.data;
	Array(AsmSymbol *) *symbols = &asm_module->data_section.symbols;
	u32 next_symbol = 0;
	for (u32 i = 0; i < data->size; i++) {
		AsmSymbol *symbol = *ARRAY_REF(symbols, AsmSymbol *, next_symbol);
		if (next_symbol < symbols->size && symbol->offset == i) {
			dump_symbol(symbol);
			next_symbol++;
		}

		// @TODO: Use d[wdq] as well when possible.
		printf("\tdb 0x%x\n", (unsigned int)*ARRAY_REF(data, u8, i));
	}

	puts("section .bss");
	u32 pos = 0;
	symbols = &asm_module->bss_section.symbols;
	for (u32 i = 0; i < symbols->size; i++) {
		AsmSymbol *symbol = *ARRAY_REF(symbols, AsmSymbol *, i);
		assert(symbol->offset == pos);

		dump_symbol(symbol);
		printf("\tresb 0x%x\n", symbol->size);

		pos += symbol->size;
	}
}

static inline void write_u8(Array(u8) *output, u8 x)
{
	*ARRAY_APPEND(output, u8) = x;
}

static inline void write_int_at(Array(u8) *output, u32 offset, u64 x, u32 size)
{
	ARRAY_ENSURE_ROOM(output, u8, offset + size);
	for (u32 n = 0; n < size; n ++) {
		u8 byte = (x >> (n * 8)) & 0xFF;
		*ARRAY_REF(output, u8, offset + n) = byte;
	}
	if (output->size < offset + size)
		output->size = offset + size;
}

static inline void write_int(Array(u8) *output, u64 x, u32 size)
{
	write_int_at(output, output->size, x, size);
}

static inline RegClass get_reg_class(AsmValue *asm_value)
{
	Register reg;
	if (asm_value->t == ASM_VALUE_REGISTER) {
		reg = asm_value->u.reg;
	} else {
		assert(asm_value->t == ASM_VALUE_OFFSET_REGISTER);
		reg = asm_value->u.offset_register.reg;
	}

	assert(reg.t == PHYS_REG);
	return reg.u.class;
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
	case REG_CLASS_IP: UNIMPLEMENTED;
	}
}

#define MAX_OPCODE_SIZE 3

typedef struct EncodedInstr
{
	u8 rex_prefix;
	bool has_oso;
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
	Fixup *disp_fixup;
	i8 immediate_size;
	u64 immediate;
	Fixup *imm_fixup;
} EncodedInstr;

static void add_mod_rm_arg(AsmModule *asm_module, EncodedInstr *encoded_instr,
		AsmValue *asm_value, FixupType fixup_type)
{
	encoded_instr->has_modrm = true;

	if (asm_value->t == ASM_VALUE_REGISTER) {
		RegClass class = get_reg_class(asm_value);

		if (asm_value->is_deref) {
			switch (class) {
			default: {
				encoded_instr->mod = 0;
				encoded_instr->rm = encoded_register_number(class);
				return;
			}
			case REG_CLASS_SP: case REG_CLASS_R12: {
				// Mod = 0, R/M = 4 means SIB addressing
				encoded_instr->mod = 0;
				encoded_instr->rm = 4;
				// no index/scale
				encoded_instr->has_sib = true;
				encoded_instr->scale = 0;
				encoded_instr->index = 4;
				encoded_instr->base = encoded_register_number(class);

				return;
			}
			case REG_CLASS_BP: case REG_CLASS_R13: {
				// Mod = 1 means 8-bit displacement
				encoded_instr->mod = 1;
				encoded_instr->rm = encoded_register_number(class);
				encoded_instr->displacement_size = 1;
				encoded_instr->displacement = 0;

				return;
			}
			}
		} else {
			encoded_instr->mod = 3;
			encoded_instr->rm = encoded_register_number(class);
			return;
		}
	} else if (asm_value->t == ASM_VALUE_OFFSET_REGISTER) {
		assert(asm_value->is_deref);

		AsmConst asm_const = asm_value->u.offset_register.offset;
		RegClass reg = get_reg_class(asm_value);

		u64 offset;
		if (asm_const.t == ASM_CONST_SYMBOL) {
			encoded_instr->mod = 2;

			// Dummy value, gets patched later.
			offset = 0;

			if (reg == REG_CLASS_IP)
				fixup_type = FIXUP_RELATIVE;

			Fixup *fixup = pool_alloc(&asm_module->pool, sizeof *fixup);
			*ARRAY_APPEND(&asm_module->fixups, Fixup *) = fixup;
			encoded_instr->disp_fixup = fixup;
			fixup->type = fixup_type;
			fixup->section = TEXT_SECTION;
			fixup->size_bytes = 4;
			fixup->symbol = asm_const.u.symbol;
		} else {
			assert(asm_const.t == ASM_CONST_IMMEDIATE);
			offset = asm_const.u.immediate;
			encoded_instr->displacement = offset;
			// @TODO: Negative numbers
			if ((offset & 0xFF) == offset) {
				encoded_instr->mod = 1;
				encoded_instr->displacement_size = 1;
			} else if ((offset & 0xFFFFFFFF) == offset) {
				encoded_instr->mod = 2;
				encoded_instr->displacement_size = 4;
			} else {
				assert(!"Offset too large!");
			}
		}

		switch (reg) {
		case REG_CLASS_A: case REG_CLASS_C: case REG_CLASS_D: case REG_CLASS_B:
		case REG_CLASS_BP: case REG_CLASS_SI: case REG_CLASS_DI: {
			encoded_instr->rm = encoded_register_number(reg);
			break;
		}
		case REG_CLASS_SP:
			// Same as above: SIB addressing
			encoded_instr->rm = 4;
			encoded_instr->has_sib = true;
			encoded_instr->scale = 0;
			encoded_instr->index = 4;
			encoded_instr->base = 4;

			if (encoded_instr->mod == 1)
				encoded_instr->displacement_size = 1;
			else
				encoded_instr->displacement_size = 4;
			encoded_instr->displacement = offset;

			break;
		case REG_CLASS_IP:
			encoded_instr->mod = 0;
			encoded_instr->rm = 5;
			encoded_instr->displacement_size = 4;
			encoded_instr->displacement = 0;
			break;
		default:
			UNIMPLEMENTED;
		}
	} else {
		UNIMPLEMENTED;
	}
}

typedef enum ArgOrder { INVALID, RM, MR } ArgOrder;

static inline void write_bytes(Array(u8) *output, u32 size, u8 *bytes)
{
	ARRAY_APPEND_ELEMS(output, u8, size, bytes);
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
static void encode_instr(Array(u8) *output, AsmModule *asm_module,
		AsmInstr *instr, ArgOrder arg_order, bool use_rex_w, bool use_oso,
		u32 opcode_size, u8 opcode[], bool reg_and_rm, i32 opcode_extension,
		i32 immediate_size, bool reg_in_opcode, FixupType fixup_type)
{
	EncodedInstr encoded_instr;
	ZERO_STRUCT(&encoded_instr);
	encoded_instr.displacement_size = -1;
	encoded_instr.immediate_size = -1;

	if (use_rex_w)
		encoded_instr.rex_prefix |= REX_W;

	encoded_instr.has_oso = use_oso;

	encoded_instr.opcode_size = opcode_size;
	memcpy(encoded_instr.opcode, opcode, opcode_size);
	if (reg_in_opcode) {
		encoded_instr.opcode_extension =
			encoded_register_number(get_reg_class(instr->args));
	}

	if (reg_and_rm) {
		AsmValue *register_operand;
		AsmValue *memory_operand;
		if (arg_order == RM) {
			register_operand = instr->args;
			memory_operand = instr->args + 1;
		} else if (arg_order == MR) {
			memory_operand = instr->args;
			register_operand = instr->args + 1;
		} else if (instr->arity == 1) {
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
		add_mod_rm_arg(asm_module, &encoded_instr, memory_operand, fixup_type);
	} else if (opcode_extension != -1) {
		encoded_instr.reg = opcode_extension;
		add_mod_rm_arg(asm_module, &encoded_instr, instr->args, fixup_type);
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
	// immediate size in AsmValue.
	if (immediate_size != -1) {
		encoded_instr.immediate_size = immediate_size;
		AsmValue* immediate_arg = NULL;
		for (u32 i = 0; i < instr->arity; i++) {
			if (instr->args[i].t == ASM_VALUE_CONST) {
				// Check that we only have one immediate.
				assert(immediate_arg == NULL);
				immediate_arg = instr->args + i;
			}
		}
		assert(immediate_arg != NULL);

		if (immediate_arg->t == ASM_VALUE_CONST) {
			AsmConst constant = immediate_arg->u.constant;
			switch (constant.t) {
			case ASM_CONST_SYMBOL: {
				Fixup *fixup = pool_alloc(&asm_module->pool, sizeof *fixup);
				*ARRAY_APPEND(&asm_module->fixups, Fixup *) = fixup;
				encoded_instr.imm_fixup = fixup;
				fixup->type = fixup_type;
				fixup->section = TEXT_SECTION;
				fixup->size_bytes = 4;
				fixup->symbol = constant.u.symbol;

				// Dummy value, gets patched later.
				encoded_instr.immediate = 0;
				break;
			}
			case ASM_CONST_IMMEDIATE:
				encoded_instr.immediate = constant.u.immediate;
				break;
			}

		} else {
			UNREACHABLE;
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
	if (encoded_instr.has_oso) {
		write_u8(output, 0x66);
	}
	if (encoded_instr.rex_prefix != 0) {
		encoded_instr.rex_prefix |= REX_HIGH;
		write_u8(output, encoded_instr.rex_prefix);
	}
	encoded_instr.opcode[0] |= encoded_instr.opcode_extension & 7;
	write_bytes(output, encoded_instr.opcode_size, encoded_instr.opcode);
	if (encoded_instr.has_modrm) {
		u8 mod = encoded_instr.mod;
		u8 reg = encoded_instr.reg;
		u8 rm = encoded_instr.rm;
		u8 mod_rm_byte = ((mod & 3) << 6) | ((reg & 7) << 3) | (rm & 7);

		write_u8(output, mod_rm_byte);
	}
	if (encoded_instr.has_sib) {
		u8 scale = encoded_instr.scale;
		u8 index = encoded_instr.index;
		u8 base = encoded_instr.base;
		u8 sib_byte = ((scale  & 3) << 6) | ((index & 7) << 3) | (base & 7);
		write_u8(output, sib_byte);
	}
	if (encoded_instr.disp_fixup != NULL)
		encoded_instr.disp_fixup->offset = output->size;
	if (encoded_instr.displacement_size != -1)
		write_int(output, encoded_instr.displacement, encoded_instr.displacement_size);
	if (encoded_instr.imm_fixup != NULL)
		encoded_instr.imm_fixup->offset = output->size;
	if (encoded_instr.immediate_size != -1)
		write_int(output, encoded_instr.immediate, encoded_instr.immediate_size);

	if (encoded_instr.disp_fixup != NULL)
		encoded_instr.disp_fixup->next_instr_offset = output->size;
	if (encoded_instr.imm_fixup != NULL)
		encoded_instr.imm_fixup->next_instr_offset = output->size;
}

// This is generated from "x64.enc", and defines the function "assemble_instr".
#include "x64.inc"

void init_binary(Binary *binary)
{
	ARRAY_INIT(&binary->text, u8, 1024);
	ARRAY_INIT(&binary->data, u8, 1024);
	ARRAY_INIT(&binary->symbols, AsmSymbol, 10);

	binary->bss_size = 0;
}

void free_binary(Binary *binary)
{
	array_free(&binary->text);
	array_free(&binary->data);
	array_free(&binary->symbols);
}

void assemble(AsmModule *asm_module, Binary *binary)
{
	// @TODO: We know ahead of time exactly how large binary->data and
	// binary->symbols are going to be, we should reserve them.
	init_binary(binary);

	Array(AsmSymbol *) *symbols = &binary->symbols;

	AsmSymbol *prev_symbol = NULL;
	Array(AsmInstr) *instrs = &asm_module->text_section.instrs;
	for (u32 i = 0; i < instrs->size; i++) {
		AsmInstr *instr = ARRAY_REF(instrs, AsmInstr, i);

		u32 instr_start = binary->text.size;
		assemble_instr(&binary->text, asm_module, instr);

		AsmSymbol *symbol = instr->label;
		if (symbol != NULL) {
			assert(symbol->section == TEXT_SECTION);
			assert(symbol->defined);

			symbol->offset = instr_start;

			if (symbol->linkage == ASM_GLOBAL_LINKAGE) {
				if (prev_symbol != NULL) {
					prev_symbol->size = instr_start - prev_symbol->offset;
				}

				prev_symbol = symbol;
			}
		}
	}
	if (prev_symbol != NULL) {
		prev_symbol->size = binary->text.size - prev_symbol->offset;
	}

	ARRAY_APPEND_ELEMS(&binary->data, u8,
			asm_module->data_section.data.size,
			asm_module->data_section.data.elements);

	binary->bss_size = asm_module->bss_section.size;

	Array(AsmSymbol *) *sections[] = {
		&asm_module->text_section.symbols,
		&asm_module->data_section.symbols,
		&asm_module->bss_section.symbols,
		&asm_module->externs,
	};
	for (u32 i = 0; i < STATIC_ARRAY_LENGTH(sections); i++) {
		Array(AsmSymbol *) *section = sections[i];

		for (u32 j = 0; j < section->size; j++) {
			AsmSymbol *symbol = *ARRAY_REF(section, AsmSymbol *, j);
			// Add one to account for 0 = undef symbol index
			symbol->symtab_index = symbols->size + 1;
			*ARRAY_APPEND(symbols, AsmSymbol *) = symbol;
		}
	}

	// @TODO: Emit relocations here instead?
	// @TODO: Remove the fixups we process here, so that write_elf_object_file
	// doesn't have to ignore them.
	for (u32 i = 0; i < asm_module->fixups.size; i++) {
		Fixup *fixup = *ARRAY_REF(&asm_module->fixups, Fixup *, i);
		AsmSymbol *symbol = fixup->symbol;
		if (fixup->type == FIXUP_ABSOLUTE
				|| !symbol->defined
				|| fixup->section != TEXT_SECTION
				|| symbol->section != TEXT_SECTION) {
			continue;
		}

		// Relative accesses are relative to the start of the next instruction.
		i32 value = (i32)symbol->offset - (i32)fixup->next_instr_offset;
		write_int_at(&binary->text, fixup->offset, (u64)value, fixup->size_bytes);
	}
}
