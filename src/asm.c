#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "array.h"
#include "asm.h"

void init_asm_block(AsmBlock *block)
{
	ARRAY_INIT(&block->instrs, AsmInstr, 20);
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

AsmArg asm_offset_register(PhysicalRegister reg, u64 offset)
{
	AsmArg asm_arg = {
		.is_deref = false,
		.type = OFFSET_REGISTER,
		.val.offset_register.reg.type = PHYSICAL_REGISTER,
		.val.offset_register.reg.val.physical_register = reg,
		.val.offset_register.offset = offset,
	};

	return asm_arg;
}

AsmArg asm_const32(i32 constant)
{
	AsmArg asm_arg = {
		.is_deref = false,
		.type = CONST32,
		.val.const32 = constant,
	};

	return asm_arg;
}

AsmArg asm_deref(AsmArg asm_arg)
{
	asm_arg.is_deref = true;
	return asm_arg;
}

static void dump_asm_args(AsmArg *args, u32 num_args);

static void dump_asm_instr(AsmInstr *instr)
{
	putchar('\t');

	switch (instr->op) {
		case MOV:
			fputs("mov ", stdout);
			dump_asm_args(instr->args, 2);
			break;
		case RET:
			fputs("ret", stdout);
			break;
		case XOR:
			fputs("xor ", stdout);
			dump_asm_args(instr->args, 2);
			break;
	}

	putchar('\n');
}

static void dump_register(Register reg)
{
	switch (reg.type) {
	case PHYSICAL_REGISTER:
		switch (reg.val.physical_register) {
		case EAX: fputs("eax", stdout); break;
		case RSP: fputs("rsp", stdout); break;
		}
		break;
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
		switch (arg->type) {
		case REGISTER:
			dump_register(arg->val.reg);
			break;
		case OFFSET_REGISTER:
			printf("%" PRIu64 "(", arg->val.offset_register.offset);
			dump_register(arg->val.offset_register.reg);
			putchar(')');
			break;
		case CONST32:
			if ((i32)arg->val.const32 < 0)
				printf("%" PRId32, (i32)arg->val.const32);
			else
				printf("%" PRIu32, arg->val.const32);
			break;
		case CONST64:
			if ((i64)arg->val.const64 < 0)
				printf("%" PRId64, (i64)arg->val.const64);
			else
				printf("%" PRIu64, arg->val.const64);
			break;
		}
	}
}

void dump_asm_module(AsmModule *asm_module)
{
	for (u32 i = 0; i < asm_module->blocks.size; i++) {
		AsmBlock *block = ARRAY_REF(&asm_module->blocks, AsmBlock, i);

		for (u32 j = 0; j < block->instrs.size; j++) {
			AsmInstr *instr = ARRAY_REF(&block->instrs, AsmInstr, j);
			dump_asm_instr(instr);
		}
	}
}

static inline u32 write_byte(FILE *file, u8 byte)
{
	fwrite(&byte, 1, 1, file);
	return 1;
}

static inline u32 write_u32(FILE *file, u32 x)
{
	u8 out[] = {
		((u32)x >> 0) & 0xFF,
		((u32)x >> 8) & 0xFF,
		((u32)x >> 16) & 0xFF,
		((u32)x >> 24) & 0xFF,
	};

	fwrite(out, 1, sizeof(out), file);
	return sizeof(out);
}

u64 assemble(AsmModule *asm_module, FILE *output_file, u64 base_virtual_address)
{
	u64 current_address = base_virtual_address;
	u64 main_virtual_addr = 0;

	for (u32 i = 0; i < asm_module->blocks.size; i++) {
		AsmBlock *block = ARRAY_REF(&asm_module->blocks, AsmBlock, i);

		for (u32 j = 0; j < block->instrs.size; j++) {
			AsmInstr *instr = ARRAY_REF(&block->instrs, AsmInstr, j);

			switch (instr->op) {
			case MOV:
				assert(instr->args[0].type == REGISTER);
				assert(instr->args[0].val.reg.type == PHYSICAL_REGISTER);
				assert(instr->args[0].val.reg.val.physical_register == EAX);
				assert(instr->args[1].type == CONST32);

				current_address += write_byte(output_file, 0xB8);
				current_address += write_u32(
						output_file,
						instr->args[1].val.const32);
				break;
			case RET:
				current_address += write_byte(output_file, 0xC3);
				break;
			case XOR:
				UNIMPLEMENTED;
			}
		}
	}

	assert(main_virtual_addr != 0);

	return main_virtual_addr;
}
