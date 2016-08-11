#include <assert.h>
#include <stdio.h>

#include "asm.h"
#include "misc.h"
#include "util.h"

typedef enum ELFIdentIndex
{
	ELF_IDENT_MAGIC0,
	ELF_IDENT_MAGIC1,
	ELF_IDENT_MAGIC2,
	ELF_IDENT_MAGIC3,
	ELF_IDENT_FILE_CLASS,
	ELF_IDENT_DATA_ENCODING,
	ELF_IDENT_ELF_VERSION,

	ELF_IDENT_NIDENT = 16,
} ELFIdentIndex;

// @TODO: Turn all these #defines into enums.

#define ELFCLASS64 2
#define ELFDATA2LSB 1
#define EV_CURRENT 1

#define ET_REL 1
#define ET_EXEC 2

// Specified by System V x86-64 spec
#define EM_X86_64 62

typedef struct ELFHeader
{
	u8 identifier[ELF_IDENT_NIDENT];

	u16 object_file_type;
	u16 target_architecture;
	u32 elf_version;

	u64 entry_point_virtual_address;
	u64 program_header_table_location;
	u64 section_header_table_location;

	u32 architecture_specific_flags;
	u16 header_size;

	u16 program_header_entry_size;
	u16 program_header_entries;
	u16 section_header_entry_size;
	u16 section_header_entries;

	u16 section_header_table_string_table_index;
} __attribute__((packed)) ELFHeader;

#define PT_LOAD 1

#define PF_X (1 << 0)
#define PF_W (1 << 1)
#define PF_R (1 << 2)

typedef struct ELFProgramHeader
{
	u32 type;
	u32 flags;
	u64 segment_location;
	u64 base_virtual_address;
	u64 base_physical_address;
	u64 segment_size_in_file;
	u64 segment_size_in_process;
	u64 alignment;
} __attribute__((packed)) ELFProgramHeader;

typedef struct ELFSectionHeader
{
	u32 shstrtab_index_for_name;
	u32 type;
	u64 flags;
	u64 base_virtual_address;
	u64 section_location;
	u64 section_size;
	u32 linked_section;
	u32 misc_info;
	u64 alignment;
	u64 entry_size;
} __attribute__((packed)) ELFSectionHeader;

#define SHN_UNDEF 0
#define SHN_ABS 0xFFF1

#define SHT_NULL 0
#define SHT_PROGBITS 1
#define SHT_SYMTAB 2
#define SHT_STRTAB 3

#define SHF_ALLOC 2
#define SHF_EXECINSTR 4

typedef struct ELF64Symbol
{
	u32 strtab_index_for_name;
	u8 type_and_binding;
	u8 reserved;
	u16 section;
	u64 value;
	u64 size;
} __attribute__((packed)) ELF64Symbol;

#define STB_LOCAL 0
#define STB_GLOBAL 1

#define STT_NOTYPE 0
#define STT_FUNC 2
#define STT_FILE 4

void write_elf_file(FILE *output_file, AsmModule *asm_module)
{
	ELFHeader header;

	header.identifier[ELF_IDENT_MAGIC0] = 0x7F;
	header.identifier[ELF_IDENT_MAGIC1] = 'E';
	header.identifier[ELF_IDENT_MAGIC2] = 'L';
	header.identifier[ELF_IDENT_MAGIC3] = 'F';
	header.identifier[ELF_IDENT_ELF_VERSION] = EV_CURRENT;
	header.elf_version = EV_CURRENT;

	header.header_size = sizeof(ELFHeader);
	header.program_header_entry_size = sizeof(ELFProgramHeader);
	header.section_header_entry_size = sizeof(ELFSectionHeader);

	header.identifier[ELF_IDENT_FILE_CLASS] = ELFCLASS64;
	header.identifier[ELF_IDENT_DATA_ENCODING] = ELFDATA2LSB;
	header.target_architecture = EM_X86_64;
	header.architecture_specific_flags = 0;

	header.object_file_type = ET_REL;

	header.program_header_table_location = 0;
	header.program_header_entries = 0;
	header.program_header_entry_size = 0;

	header.section_header_table_location = sizeof(ELFHeader);
	// We write out the following sections (in this order):
	//   * .shstrtab
	//   * .text
	//   * .strtab
	//   * .symtab
	//
	// The headers are in the same order, but with a NULL header at the start
	// that has no corresponding sections, as required by the spec.
	//
	// @TODO: Add .note.GNU-STACK section header to prevent executable stack.
	u32 num_sections = 5;
	header.section_header_entries = num_sections;
	header.section_header_table_string_table_index = 1;
	u32 text_section_index = 2;
	u32 strtab_section_index = 3;

	assert(checked_ftell(output_file) == 0);
	fwrite(&header, sizeof header, 1, output_file);

	u32 first_section_offset = header.section_header_table_location +
		sizeof(ELFSectionHeader) * num_sections;
	fseek(output_file, first_section_offset, SEEK_SET);

	// .shstrtab
	u32 shstrtab_offset = checked_ftell(output_file);
	//                          0          1           2          3
	//                          0 1234567890 123456 78901234 5678901
	char shstrtab_contents[] = "\0.shstrtab\0.text\0.strtab\0.symtab";
	u32 shstrtab_name = 1;
	u32 text_name = 11;
	u32 strtab_name = 17;
	u32 symtab_name = 25;
	fwrite(shstrtab_contents, sizeof shstrtab_contents, 1, output_file);
	u32 shstrtab_size = checked_ftell(output_file) - shstrtab_offset;

	// .text
	u32 text_offset = checked_ftell(output_file);
	u32 base_virtual_address = 0;
	Array(AsmSymbol) symbols;
	ARRAY_INIT(&symbols, AsmSymbol, 10);
	assemble(asm_module, output_file, &symbols, base_virtual_address);
	u32 text_size = checked_ftell(output_file) - text_offset;

	// .strtab
	u32 strtab_offset = checked_ftell(output_file);
	fputc('\0', output_file);

	// @TODO: Pass the file name through so we have it here
	char *dummy_filename = "foo.c";
	fputs(dummy_filename, output_file);
	fputc('\0', output_file);

	for (u32 i = 0; i < symbols.size; i++) {
		AsmSymbol *symbol = ARRAY_REF(&symbols, AsmSymbol, i);
		u32 current_strtab_position = checked_ftell(output_file) - strtab_offset;
		symbol->string_table_offset_for_name = current_strtab_position;

		fputs(symbol->name, output_file);
		fputc('\0', output_file);
	}
	u32 strtab_size = checked_ftell(output_file) - strtab_offset;

	// .symtab
	u32 symtab_offset = checked_ftell(output_file);

	{
		ELF64Symbol undef_symbol;
		ZERO_STRUCT(&undef_symbol);
		fwrite(&undef_symbol, sizeof undef_symbol, 1, output_file);
	}

	{
		ELF64Symbol file_symbol;
		ZERO_STRUCT(&file_symbol);
		// We write out the filename right at the start just above. This is
		// 1 rather than 0 as the string table starts with '\0'.
		file_symbol.strtab_index_for_name = 1;
		file_symbol.type_and_binding = (STB_LOCAL << 4) | STT_FILE;
		file_symbol.section = SHN_ABS;
		file_symbol.value = 0;
		file_symbol.size = 0;
		fwrite(&file_symbol, sizeof file_symbol, 1, output_file);
	}

	for (u32 i = 0; i < symbols.size; i++) {
		AsmSymbol *symbol = ARRAY_REF(&symbols, AsmSymbol, i);

		ELF64Symbol elf_symbol;
		ZERO_STRUCT(&elf_symbol);
		elf_symbol.strtab_index_for_name = symbol->string_table_offset_for_name;
		elf_symbol.type_and_binding = (STB_GLOBAL << 4) | STT_FUNC;
		elf_symbol.section = text_section_index;
		elf_symbol.value = symbol->offset;
		elf_symbol.size = symbol->size;

		fwrite(&elf_symbol, sizeof elf_symbol, 1, output_file);
	}
	u32 symtab_size = checked_ftell(output_file) - symtab_offset;

	// Then write the corresponding section headers
	fseek(output_file, header.section_header_table_location, SEEK_SET);

	// NULL header
	{
		ELFSectionHeader null_header;
		ZERO_STRUCT(&null_header);
		null_header.type = SHT_NULL;
		fwrite(&null_header, sizeof null_header, 1, output_file);
	}

	// .shstrtab
	{
		ELFSectionHeader shstrtab_header;
		ZERO_STRUCT(&shstrtab_header);
		shstrtab_header.shstrtab_index_for_name = shstrtab_name;
		shstrtab_header.type = SHT_STRTAB;
		shstrtab_header.section_location = first_section_offset;
		shstrtab_header.section_size = shstrtab_size;
		fwrite(&shstrtab_header, sizeof shstrtab_header, 1, output_file);
	}

	// .text
	{
		ELFSectionHeader text_header;
		ZERO_STRUCT(&text_header);
		text_header.shstrtab_index_for_name = text_name;
		text_header.type = SHT_PROGBITS;
		text_header.flags = SHF_ALLOC | SHF_EXECINSTR;
		text_header.base_virtual_address = base_virtual_address;
		text_header.section_location = text_offset;
		text_header.section_size = text_size;
		fwrite(&text_header, sizeof text_header, 1, output_file);
	}

	// .strtab
	{
		ELFSectionHeader strtab_header;
		ZERO_STRUCT(&strtab_header);
		strtab_header.shstrtab_index_for_name = strtab_name;
		strtab_header.type = SHT_STRTAB;
		strtab_header.section_location = strtab_offset;
		strtab_header.section_size = strtab_size;
		fwrite(&strtab_header, sizeof strtab_header, 1, output_file);
	}

	// .symtab
	{
		ELFSectionHeader symtab_header;
		ZERO_STRUCT(&symtab_header);
		symtab_header.shstrtab_index_for_name = symtab_name;
		symtab_header.type = SHT_SYMTAB;
		// For symbol tables, this field contains 1 + the index of the last
		// local symbol. For now we just hardcode this to 2: 0 is undef
		// symbol, 1 is the symbol for the filename.
		symtab_header.misc_info = 2;
		symtab_header.linked_section = strtab_section_index;
		symtab_header.section_location = symtab_offset;
		symtab_header.section_size = symtab_size;
		symtab_header.entry_size = sizeof(ELF64Symbol);
		fwrite(&symtab_header, sizeof symtab_header, 1, output_file);
	}
}

#if 0
static void write_crt0(FILE *output_file, i32 main_address_offset)
{
	// This is really hacky. We can't link object files together yet, nor do we
	// have a proper assembler. So instead we just directly write preassembled
	// machine code into the file for our tiny crt0.
	
	u8 crt0[] = {
		0x48, 0x31, 0xED,             // xor ebp, ebp
		// The zeroes are a placeholder for the actual offset, which we insert
		// below
		0xE8, 0x00, 0x00, 0x00, 0x00, // call main
		0x8A, 0xD8,                   // mov bl, al
		0xB8, 0x01, 0x00, 0x00, 0x00, // mov eax, 1
		0xCD, 0x80,                   // int 0x80
	};

	// The offset is from the instruction after the call.
	main_address_offset -= 8;

	crt0[4] = ((u32)main_address_offset >> 0) & 0xFF;
	crt0[5] = ((u32)main_address_offset >> 8) & 0xFF;
	crt0[6] = ((u32)main_address_offset >> 16) & 0xFF;
	crt0[7] = ((u32)main_address_offset >> 24) & 0xFF;

	fwrite(crt0, 1, sizeof crt0, output_file);
}
#endif


void link_elf_executable(char *executable_filename, Array(char *) *linker_input_filenames)
{
	IGNORE(executable_filename); IGNORE(linker_input_filenames);

	UNIMPLEMENTED;
#if 0
// Starting point for the output part of link_elf_executable
	if (do_link) {
		header.section_header_table_location = 0;
		header.section_header_entries = 0;
		header.section_header_table_string_table_index = SHN_UNDEF;

		header.program_header_table_location = sizeof(ELFHeader);
		header.program_header_entries = 1;

		u32 first_segment_location =
			header.program_header_table_location +
			(header.program_header_entries * sizeof(ELFProgramHeader));

		ELFProgramHeader executable_segment_header;
		executable_segment_header.type = PT_LOAD;
		executable_segment_header.segment_location = first_segment_location;
		executable_segment_header.base_virtual_address = 0x8000000 + first_segment_location;
		executable_segment_header.flags = PF_R | PF_X;
		executable_segment_header.alignment = 0x1000;

		fseek(output_file, first_segment_location, SEEK_SET);
		Array(AsmSymbol) symbols;
		ARRAY_INIT(&symbols, AsmSymbol, 10);
		assemble(asm_module, output_file, &symbols, executable_segment_header.base_virtual_address);

		u32 main_virtual_addr = 0;
		for (u32 i = 0; i < symbols.size; i++) {
			AsmSymbol *symbol = ARRAY_REF(&symbols, AsmSymbol, i);
			if (streq(symbol->name, "main")) {
				main_virtual_addr = symbol->offset;
				break;
			}
		}
		assert(main_virtual_addr != 0);

		i32 crt_offset = checked_ftell(output_file) - first_segment_location;
		header.entry_point_virtual_address =
			executable_segment_header.base_virtual_address + crt_offset;
		write_crt0(output_file, main_virtual_addr - header.entry_point_virtual_address);

		u32 executable_segment_size = checked_ftell(output_file) - first_segment_location;
		executable_segment_header.segment_size_in_file =
			executable_segment_header.segment_size_in_process = executable_segment_size;

		fseek(output_file, 0, SEEK_SET);
		fwrite(&header, sizeof header, 1, output_file);
		fwrite(&executable_segment_header, sizeof executable_segment_header, 1, output_file);
	} else {
#endif
}
