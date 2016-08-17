#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "asm.h"
#include "file.h"
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

	u16 shstrtab_index;
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
#define SHT_RELA 4

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

#define ELF64_SYMBOL_BINDING(x) ((x) >> 4)
#define ELF64_SYMBOL_TYPE(x) ((x) & 0xF)

#define STT_NOTYPE 0
#define STT_FUNC 2
#define STT_FILE 4

typedef struct ELF64Rela
{
	u64 section_offset;
	u64 type_and_symbol;
	i64 addend;
} __attribute__((packed)) ELF64Rela;

#define ELF64_RELA_SYMBOL(x) ((x) >> 32)
#define ELF64_RELA_TYPE(x) ((x) & 0xFFFFFFFF)

// @NOTE: These are defined in the System V x86-64 spec. They basically
// correspond to the entries in the ELF spec, but with the R_X86_64 prefix
// instead of R_386.
typedef enum ELF64RelocType
{
	R_X86_64_PC32 = 2,
} ELF64RelocType;


// Writes all the standard boilerplate in the header.
static void init_elf_header(ELFHeader *header)
{
	ZERO_STRUCT(header);

	header->identifier[ELF_IDENT_MAGIC0] = 0x7F;
	header->identifier[ELF_IDENT_MAGIC1] = 'E';
	header->identifier[ELF_IDENT_MAGIC2] = 'L';
	header->identifier[ELF_IDENT_MAGIC3] = 'F';
	header->identifier[ELF_IDENT_ELF_VERSION] = EV_CURRENT;
	header->elf_version = EV_CURRENT;

	header->header_size = sizeof(ELFHeader);
	header->program_header_entry_size = sizeof(ELFProgramHeader);
	header->section_header_entry_size = sizeof(ELFSectionHeader);

	header->identifier[ELF_IDENT_FILE_CLASS] = ELFCLASS64;
	header->identifier[ELF_IDENT_DATA_ENCODING] = ELFDATA2LSB;
	header->target_architecture = EM_X86_64;
	header->architecture_specific_flags = 0;
}

void write_elf_file(FILE *output_file, AsmModule *asm_module)
{
	ELFHeader header;
	init_elf_header(&header);

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
	u32 num_sections = 5;
	header.section_header_entries = num_sections;
	header.shstrtab_index = 1;
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
		// local symbol. 0 is undef symbol, 1 is the symbol for the filename,
		// + all the rest of the symbols.
		symtab_header.misc_info = 2 + symbols.size;;
		symtab_header.linked_section = strtab_section_index;
		symtab_header.section_location = symtab_offset;
		symtab_header.section_size = symtab_size;
		symtab_header.entry_size = sizeof(ELF64Symbol);
		fwrite(&symtab_header, sizeof symtab_header, 1, output_file);
	}

	array_free(&symbols);
}

typedef struct Relocation
{
	ELF64RelocType type;
	u32 file_offset;
	i32 addend;
} Relocation;

typedef struct Symbol
{
	bool defined;
	char *name;

	// @NOTE: This doesn't tell you which object file the symbol table belongs
	// to. This is just used for locating symbols from within the same object
	// file when processing relocation entries.
	u32 symtab_index;

	union
	{
		struct
		{
			u32 file_offset;
		} def;
		struct
		{
			Array(Relocation) relocs;
		} undef;
	} val;
} Symbol;

static void apply_relocation(FILE *output_file, Relocation *reloc,
		u32 symbol_file_offset)
{
	u32 prev_location = checked_ftell(output_file);

	switch (reloc->type) {
	case R_X86_64_PC32: {
		u32 final_value = (i32)symbol_file_offset + reloc->addend - (i32)reloc->file_offset;
		u8 final_value_bytes[] = {
			final_value & 0xFF,
			(final_value >> 8) & 0xFF,
			(final_value >> 16) & 0xFF,
			(final_value >> 24) & 0xFF,
		};

		checked_fseek(output_file, reloc->file_offset, SEEK_SET);
		checked_fwrite(final_value_bytes,
				1,
				sizeof final_value_bytes,
				output_file);

		break;
	}
	default:
		UNIMPLEMENTED;
	}

	checked_fseek(output_file, prev_location, SEEK_SET);
}

// @TODO: Handle archives correctly. Currently we just treat them as a series
// of object files, but archives have different semantics.
// See http://eli.thegreenplace.net/2013/07/09/library-order-in-static-linking
// for details.
static bool process_elf_file(FILE *input_file, FILE *output_file,
		Array(Symbol) *symbol_table)
{
	u32 initial_location = checked_ftell(input_file);

	ELFHeader file_header;

	// We check this the first time to get nicer error handling if the file is
	// unreadable. However, we assume that its unlikely for a file to become
	// unreadable partway through reading, so for the rest we just use
	// asserting functions.
	if (fread(&file_header, sizeof file_header, 1, input_file) != 1) {
		perror("Failed to read from ELF file");
		return false;
	}

	// This should have been checked already
	assert(file_header.identifier[0] == 0x7F && file_header.identifier[1] == 'E' &&
			file_header.identifier[2] == 'L' && file_header.identifier[3] == 'F');

	if (file_header.target_architecture != EM_X86_64) {
		fprintf(stderr,
				"Invalid architecture (%d given, %d expected)\n",
				file_header.target_architecture, EM_X86_64);
		return false;
	}
	assert(file_header.section_header_entry_size == sizeof(ELFSectionHeader));

	u32 sht_offset = file_header.section_header_table_location;
	checked_fseek(input_file, initial_location + sht_offset, SEEK_SET);

	bool ret = true;
	ELFSectionHeader *headers =
		malloc(sizeof *headers * file_header.section_header_entries);
	for (u32 i = 0; i < file_header.section_header_entries; i++) {
		ELFSectionHeader *header = headers + i;
		checked_fread(header, sizeof *header, 1, input_file);
	}

	ELFSectionHeader *shstrtab_header = headers + file_header.shstrtab_index;
	assert(shstrtab_header->type == SHT_STRTAB);
	char *shstrtab = malloc(shstrtab_header->section_size);
	checked_fseek(input_file,
				initial_location + shstrtab_header->section_location,
				SEEK_SET);
	checked_fread(shstrtab, 1, shstrtab_header->section_size, input_file);

	ELFSectionHeader *text_header = NULL;
	u32 text_section_index;
	ELFSectionHeader *symtab_header = NULL;
	u32 symtab_section_index;
	ELFSectionHeader *strtab_header = NULL;
	ELFSectionHeader *rela_text_header = NULL;
	for (u32 i = 0; i < file_header.section_header_entries; i++) {
		ELFSectionHeader *curr_header = headers + i;
		char *section_name = shstrtab + curr_header->shstrtab_index_for_name;

		if (streq(section_name, ".text")) {
			text_header = curr_header;
			text_section_index = i;
		} else if (streq(section_name, ".symtab")) {
			symtab_header = curr_header;
			symtab_section_index = i;
		} else if (streq(section_name, ".strtab")) {
			strtab_header = curr_header;
		} else if (strneq(section_name, ".rela", 5)) {
			if (!streq(section_name, ".rela.text")) {
				fprintf(stderr, "Relocations for non-text sections are not"
						" supported (found section %s)\n", section_name);
				goto cleanup1;
			}

			rela_text_header = curr_header;
		}
	}

	if (text_header == NULL) {
		fputs("Missing .text section", stderr);
		ret = false;
		goto cleanup1;
	}
	if (symtab_header == NULL) {
		fputs("Missing .symtab section", stderr);
		ret = false;
		goto cleanup1;
	}
	if (strtab_header == NULL) {
		fputs("Missing .strtab section", stderr);
		ret = false;
		goto cleanup1;
	}

	assert(text_header->type == SHT_PROGBITS);

	assert(symtab_header->type == SHT_SYMTAB);
	assert(symtab_header->entry_size == sizeof(ELF64Symbol));

	assert(strtab_header->type == SHT_STRTAB);

	if (rela_text_header != NULL) {
		assert(rela_text_header->type == SHT_RELA);
		assert(rela_text_header->linked_section == symtab_section_index);
		assert(rela_text_header->misc_info == text_section_index);
		assert(rela_text_header->entry_size == sizeof(ELF64Rela));
	}

	char *strtab = malloc(strtab_header->section_size);
	checked_fseek(input_file,
				initial_location + strtab_header->section_location,
				SEEK_SET);
	checked_fread(strtab, 1, strtab_header->section_size, input_file);

	u32 start_of_new_text_segment_data = checked_ftell(output_file);
	checked_fseek(input_file,
			initial_location + text_header->section_location,
			SEEK_SET);
	u8 *temp_buffer = malloc(text_header->section_size);
	checked_fread(temp_buffer, text_header->section_size, 1, input_file);
	checked_fwrite(temp_buffer, text_header->section_size, 1, output_file);
	free(temp_buffer);

	u32 symbols_in_symtab = symtab_header->section_size / symtab_header->entry_size;
	checked_fseek(input_file,
			initial_location + symtab_header->section_location,
			SEEK_SET);

	Symbol **file_symbols = calloc(sizeof(*file_symbols) * symbols_in_symtab, 1);

	// @TODO: We can iterate across this more efficiently by using the
	// information in the header about the last local symbol. We can also use
	// this to allocate fewer symbols.
	for (u32 symtab_index = 0; symtab_index < symbols_in_symtab; symtab_index++) {
		ELF64Symbol symtab_symbol;
		checked_fread(&symtab_symbol, sizeof symtab_symbol, 1, input_file);

		char *symbol_name = strtab + symtab_symbol.strtab_index_for_name;
		u8 binding = ELF64_SYMBOL_BINDING(symtab_symbol.type_and_binding);

		if (binding != STB_GLOBAL)
			continue;

		i32 found_symbol_index = -1;
		for (u32 symbol_index = 0;
				symbol_index < symbol_table->size;
				symbol_index++) {
			Symbol *symbol = ARRAY_REF(symbol_table, Symbol, symbol_index);
			if (streq(symbol->name, symbol_name)) {
				found_symbol_index = symbol_index;
				break;
			}
		}

		if (symtab_symbol.section == SHN_UNDEF) {
			Symbol *symbol;
			if (found_symbol_index == -1) {
				symbol = ARRAY_APPEND(symbol_table, Symbol);
				symbol->defined = false;
				symbol->name = strndup(symbol_name, strlen(symbol_name));
				ARRAY_INIT(&symbol->val.undef.relocs, Relocation, 5);
			} else {
				symbol = ARRAY_REF(symbol_table, Symbol, found_symbol_index);
			}

			file_symbols[symtab_index] = symbol;
		} else {
			assert(symtab_symbol.section == text_section_index);

			u32 symbol_file_offset = start_of_new_text_segment_data
				+ symtab_symbol.value;

			Symbol *symbol;
			if (found_symbol_index == -1) {
				symbol = ARRAY_APPEND(symbol_table, Symbol);
			} else {
				symbol = ARRAY_REF(symbol_table, Symbol, found_symbol_index);
				if (symbol->defined) {
					fprintf(stderr,
							"Multiple definitions of symbol '%s'\n",
							symbol_name);
					goto cleanup2;
				}

				Array(Relocation) *relocs = &symbol->val.undef.relocs;
				for (u32 reloc_index = 0;
						reloc_index < relocs->size;
						reloc_index++)  {
					Relocation *reloc =
						ARRAY_REF(relocs, Relocation, reloc_index);

					apply_relocation(output_file, reloc, symbol_file_offset);
				}

				array_free(relocs);
			}

			symbol->defined = true;
			symbol->name = strndup(symbol_name, strlen(symbol_name));
			symbol->val.def.file_offset = symbol_file_offset;

			file_symbols[symtab_index] = symbol;
		}
	}

	if (rela_text_header != NULL) {
		checked_fseek(input_file,
				initial_location + rela_text_header->section_location,
				SEEK_SET);
		u32 rela_entries = rela_text_header->section_size / rela_text_header->entry_size;
		for (u32 rela_index = 0; rela_index < rela_entries; rela_index++) {
			ELF64Rela rela;
			checked_fread(&rela, sizeof rela, 1, input_file);

			ELF64RelocType type = ELF64_RELA_TYPE(rela.type_and_symbol);
			u32 symtab_index = ELF64_RELA_SYMBOL(rela.type_and_symbol);
			Symbol *corresponding_symbol = file_symbols[symtab_index];
			assert(corresponding_symbol != NULL);

			Relocation *reloc;
			Relocation _reloc;
			if (!corresponding_symbol->defined) {
				reloc = ARRAY_APPEND(&corresponding_symbol->val.undef.relocs, Relocation);
			} else {
				reloc = &_reloc;
			}
			reloc->type = type;
			reloc->addend = rela.addend;
			reloc->file_offset =
				rela.section_offset + start_of_new_text_segment_data;

			if (corresponding_symbol->defined) {
				apply_relocation(output_file, reloc,
						corresponding_symbol->val.def.file_offset);
			}
		}
	}


cleanup2:
	free(file_symbols);
	free(strtab);
cleanup1:
	free(shstrtab);
	free(headers);
	return ret;
}

// @TODO: Write symbol table. We'll need to keep track of local symbols too for
// full coverage.
// @TODO: Add .note.GNU-STACK section header to prevent executable stack.
bool link_elf_executable(char *executable_filename, Array(char *) *linker_input_filenames)
{
	bool ret = true;

	FILE *output_file = fopen(executable_filename, "wb");
	if (output_file == NULL) {
		perror("Failed to open linker output");
		return false;
	}

	Array(Symbol) symbol_table;
	ARRAY_INIT(&symbol_table, Symbol, 100);

	u32 program_header_table_location = sizeof(ELFHeader);
	u32 program_header_entries = 1;

	u32 first_segment_location =
		program_header_table_location +
		(program_header_entries * sizeof(ELFProgramHeader));

	fseek(output_file, first_segment_location, SEEK_SET);

	for (u32 i = 0; i < linker_input_filenames->size; i++) {
		char *input_filename = *ARRAY_REF(linker_input_filenames, char *, i);
		FILE *input_file = fopen(input_filename, "rb");
		if (input_file == NULL) {
			perror("Failed to open linker input");
			ret = false;
			goto cleanup;
		}

		FileType type = file_type(input_file);
		checked_fseek(input_file, 0, SEEK_SET);
		switch (type) {
		case ELF_FILE_TYPE:
			if (!process_elf_file(input_file, output_file, &symbol_table)) {
				ret = false;
				goto cleanup;
			}
			break;
		case AR_FILE_TYPE:
			UNIMPLEMENTED;
			break;
		// We should have checked it was an object file or archive before
		// putting it on the linker input list.
		case UNKNOWN_FILE_TYPE:
			UNREACHABLE;
		}

		fclose(input_file);
	}

	Symbol *start_symbol = NULL;
	for (u32 i = 0; i < symbol_table.size; i++) {
		Symbol *symbol = ARRAY_REF(&symbol_table, Symbol, i);
		if (!symbol->defined) {
			fprintf(stderr, "Undefined symbol '%s'\n", symbol->name);
			ret = false;
			goto cleanup;
		}

		if (streq(symbol->name, "_start")) {
			start_symbol = symbol;
		}
	}

	assert(start_symbol != NULL);
	// @NOTE: In System V, file offsets and base virtual addresses for segments
	// must be congruent modulo the page size.
	u32 base_virtual_address = 0x8000000 + first_segment_location; 
	u32 entry_point_virtual_address = base_virtual_address
		- first_segment_location
		+ start_symbol->val.def.file_offset;

	u32 executable_segment_size = checked_ftell(output_file) - first_segment_location;

	ELFHeader header;
	init_elf_header(&header);

	header.object_file_type = ET_EXEC;
	header.entry_point_virtual_address = entry_point_virtual_address;

	// We don't write any sections, only segments.
	header.section_header_table_location = 0;
	header.section_header_entries = 0;
	header.shstrtab_index = SHN_UNDEF;
	header.program_header_table_location = program_header_table_location;
	header.program_header_entries = program_header_entries;

	checked_fseek(output_file, 0, SEEK_SET);
	checked_fwrite(&header, sizeof header, 1, output_file);

	ELFProgramHeader executable_segment_header;
	ZERO_STRUCT(&executable_segment_header);
	executable_segment_header.type = PT_LOAD;
	executable_segment_header.segment_location = first_segment_location;
	executable_segment_header.base_virtual_address = base_virtual_address;
	executable_segment_header.flags = PF_R | PF_X;
	executable_segment_header.alignment = 0x1000;

	// @TODO: Search symbols from input files, set entry point to _start
	header.entry_point_virtual_address = 0;

	executable_segment_header.segment_size_in_file =
		executable_segment_header.segment_size_in_process = executable_segment_size;

	checked_fseek(output_file, header.program_header_table_location, SEEK_SET);
	checked_fwrite(&executable_segment_header, sizeof executable_segment_header, 1, output_file);

cleanup:
	fclose(output_file);
	array_free(&symbol_table);
	return ret;
}
