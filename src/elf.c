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

#define ELFCLASS64 2
#define ELFDATA2LSB 1
#define EV_CURRENT 1

typedef enum ELFFileType
{
	ET_REL = 1,
	ET_EXEC = 2,
} ELFFileType;

// Specified by System V x86-64 spec
#define EM_X86_64 62

typedef struct ELFHeader
{
	u8 identifier[ELF_IDENT_NIDENT];

	u16 object_file_type;
	u16 target_architecture;
	u32 elf_version;

	u64 entry_point_virtual_address;
	u64 pht_location;
	u64 sht_location;

	u32 architecture_specific_flags;
	u16 header_size;

	u16 program_header_size;
	u16 pht_entries;
	u16 section_header_size;
	u16 sht_entries;

	u16 shstrtab_index;
} __attribute__((packed)) ELFHeader;

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

typedef enum ELFProgramHeaderType
{
	PT_LOAD = 1,
} ELFProgramHeaderType;

typedef enum ELFProgramHeaderFlags
{
	PF_X = 1 << 0,
	PF_W = 1 << 1,
	PF_R = 1 << 2,
} ELFProgramHeaderFlags;

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

typedef enum ELFSectionHeaderType
{
	SHT_NULL = 0,
	SHT_PROGBITS = 1,
	SHT_SYMTAB = 2,
	SHT_STRTAB = 3,
	SHT_RELA = 4,
	SHT_NOBITS = 8,
} ELFSectionHeaderType;

typedef enum ELFSectionHeaderFlags
{
	SHF_WRITE = 1,
	SHF_ALLOC = 2,
	SHF_EXECINSTR = 4,
} ELFSectionHeaderFlags;

#define SHN_UNDEF 0
#define SHN_ABS 0xFFF1

typedef struct ELF64Symbol
{
	u32 strtab_index_for_name;
	u8 type_and_binding;
	u8 reserved;
	u16 section;
	u64 value;
	u64 size;
} __attribute__((packed)) ELF64Symbol;

typedef enum ELFSymbolBinding
{
	STB_LOCAL = 0,
	STB_GLOBAL = 1,
} ELFSymbolBinding;

typedef enum ELFSymbolType
{
	STT_NOTYPE = 0,
	STT_OBJECT = 1,
	STT_FUNC = 2,
	STT_SECTION = 3,
	STT_FILE = 4,
} ELFSymbolType;

#define ELF64_SYMBOL_TYPE_AND_BINDING(t, b) (((b) << 4) | (t))
#define ELF64_SYMBOL_BINDING(x) ((x) >> 4)
#define ELF64_SYMBOL_TYPE(x) ((x) & 0xF)

typedef struct ELF64Rela
{
	u64 section_offset;
	u64 type_and_symbol;
	i64 addend;
} __attribute__((packed)) ELF64Rela;

#define ELF64_RELA_TYPE_AND_SYMBOL(t, s) (((u64)(s) << 32) | ((t) & 0xFFFFFFFF))
#define ELF64_RELA_SYMBOL(x) ((x) >> 32)
#define ELF64_RELA_TYPE(x) ((x) & 0xFFFFFFFF)

// @NOTE: These are defined in the System V x86-64 spec. They basically
// correspond to the entries in the ELF spec, but with the R_X86_64 prefix
// instead of R_386.
typedef enum ELF64RelocType
{
	R_X86_64_64 = 1,
	R_X86_64_PC32 = 2,
} ELF64RelocType;


// The following have nothing to do with the spec - they're just constants
// regarding the object files we want to create


// We write the following sections (in this order):
//   * .text
//   * .rela.text
//   * .bss
//   * .data
//   * .shstrtab
//   * .symtab
//   * .strtab
//
// The headers are in the same order, but with a NULL header at the start that
// has no corresponding sections, as required by the spec.
//
// Note that we write out .rela.text even if there are no relocations. In this
// case we set the size to zero in the header and don't write any data for it.
// This is simpler than having to handle it not being present, and it costs 
// only 64 bytes in object files without relocations. We do the same thing for
// .bss and .data, with the same tradeoff.

#define NUM_SECTIONS 8

//                         0           1          2           3          4
//                         0 123456 78901234567 89012 345678 9012345678 90123456 7
#define SHSTRTAB_CONTENTS "\0.text\0.rela.text\0.bss\0.data\0.shstrtab\0.symtab\0.strtab"
#define TEXT_NAME 1
#define RELA_TEXT_NAME 7
#define BSS_NAME 18
#define DATA_NAME 23
#define SHSTRTAB_NAME 29
#define SYMTAB_NAME 39
#define STRTAB_NAME 47

// Starts from 1 because the NULL header is at 0.
#define TEXT_INDEX 1
#define RELA_TEXT_INDEX 2
#define BSS_INDEX 3
#define DATA_INDEX 4
#define SHSTRTAB_INDEX 5
#define SYMTAB_INDEX 6
#define STRTAB_INDEX 7


typedef struct SectionInfo
{
	u32 size;
	u32 offset;
	u32 virtual_address;
	u8 *contents;
} SectionInfo;

typedef struct ELFFile
{
	FILE *output_file;
	ELFFileType type;
	i32 entry_point_virtual_address;
	u32 next_string_index;

	SectionInfo section_info[NUM_SECTIONS];
} ELFFile;

static void init_elf_file(ELFFile *elf_file, FILE *output_file, ELFFileType type)
{
	ZERO_STRUCT(elf_file);
	memset(elf_file->section_info, 0, sizeof elf_file->section_info);

	elf_file->output_file = output_file;
	elf_file->type = type;
	elf_file->next_string_index = 1;
}

static void write_contents(ELFFile *elf_file, Array(Fixup) *fixups)
{
	u32 pht_entries = elf_file->type == ET_EXEC ? 3 : 0;
	u32 first_section_offset = sizeof(ELFHeader) +
		sizeof(ELFSectionHeader) * NUM_SECTIONS +
		sizeof(ELFProgramHeader) * pht_entries;

	SectionInfo *text_info = elf_file->section_info + TEXT_INDEX;
	text_info->offset = first_section_offset;

	checked_fseek(elf_file->output_file, first_section_offset, SEEK_SET);

	// @NOTE: In System V, file offsets and base virtual addresses for segments
	// must be congruent modulo the page size.
	text_info->virtual_address = 0x8000000 + text_info->offset; 
	assert(text_info->contents != NULL);
	checked_fwrite(text_info->contents, 1, text_info->size, elf_file->output_file);

	for (u32 i = 0; i < fixups->size; i++) {
		Fixup *fixup = *ARRAY_REF(fixups, Fixup *, i);
		if (fixup->t == FIXUP_LABEL)
			continue;

		u32 symbol = fixup->u.global->symbol->symtab_index;
		assert(symbol != 0);

		ELF64RelocType reloc_type;
		u32 addend;
		switch (fixup->type) {
		case FIXUP_RELATIVE:
			reloc_type = R_X86_64_PC32;
			addend = (i64)fixup->offset - (i64)fixup->next_instr_offset;
			break;
		case FIXUP_ABSOLUTE:
			reloc_type = R_X86_64_64;
			addend = 0;
			break;
		}
		ELF64Rela rela = {
			.section_offset = fixup->offset,
			.type_and_symbol = ELF64_RELA_TYPE_AND_SYMBOL(reloc_type, symbol),
			.addend = addend,
		};

		checked_fwrite(&rela, sizeof rela, 1, elf_file->output_file);
	}

	SectionInfo *rela_text_info = elf_file->section_info + RELA_TEXT_INDEX;
	rela_text_info->offset = text_info->offset + text_info->size;
	rela_text_info->size =
		checked_ftell(elf_file->output_file) - rela_text_info->offset;

	SectionInfo *bss_info = elf_file->section_info + BSS_INDEX;
	bss_info->offset = rela_text_info->offset + rela_text_info->size;
	bss_info->virtual_address =
		align_to(text_info->virtual_address, 0x1000000) + bss_info->offset;

	SectionInfo *data_info = elf_file->section_info + DATA_INDEX;
	data_info->offset = bss_info->offset;
	data_info->virtual_address =
		align_to(elf_file->section_info[BSS_INDEX].virtual_address, 0x1000000)
		+ data_info->offset;
	assert(data_info->contents != NULL);
	checked_fwrite(data_info->contents, 1, data_info->size, elf_file->output_file);

	SectionInfo shstrtab_info = (SectionInfo) {
		.offset = data_info->offset + data_info->size,
		.size = sizeof SHSTRTAB_CONTENTS,
	};
	elf_file->section_info[SHSTRTAB_INDEX] = shstrtab_info;
	checked_fwrite(SHSTRTAB_CONTENTS, sizeof SHSTRTAB_CONTENTS, 1, elf_file->output_file);

	elf_file->section_info[SYMTAB_INDEX].offset =
		shstrtab_info.offset + shstrtab_info.size;

	ELF64Symbol undef_symbol;
	ZERO_STRUCT(&undef_symbol);
	checked_fwrite(&undef_symbol, sizeof undef_symbol, 1, elf_file->output_file);
}

static void add_symbol(ELFFile *elf_file, ELFSymbolType type,
		ELFSymbolBinding binding, u32 section, char *name, i32 value, u32 size)
{
	if (elf_file->type == ET_EXEC && streq(name, "_start")) {
		elf_file->entry_point_virtual_address = value;
	}

	ELF64Symbol symbol;
	ZERO_STRUCT(&symbol);
	symbol.strtab_index_for_name = elf_file->next_string_index;
	symbol.type_and_binding = ELF64_SYMBOL_TYPE_AND_BINDING(type, binding);
	symbol.section = section;
	symbol.value = value;
	symbol.size = size;

	elf_file->next_string_index += strlen(name) + 1;

	checked_fwrite(&symbol, sizeof symbol, 1, elf_file->output_file);
}

static void finish_symtab_section(ELFFile *elf_file)
{
	SectionInfo *symtab_info = elf_file->section_info + SYMTAB_INDEX;
	symtab_info->size = checked_ftell(elf_file->output_file) - symtab_info->offset;
	elf_file->section_info[STRTAB_INDEX].offset =
		symtab_info->offset + symtab_info->size;

	// The string table has to start with a 0 byte.
	fputc('\0', elf_file->output_file);
}

static void add_string(ELFFile *elf_file, char *string)
{
	fputs(string, elf_file->output_file);
	fputc('\0', elf_file->output_file);
}

static void finish_strtab_section(ELFFile *elf_file)
{
	FILE *output_file = elf_file->output_file;

	SectionInfo *strtab_info = elf_file->section_info + STRTAB_INDEX;
	strtab_info->size = checked_ftell(output_file) - strtab_info->offset;


	// .strtab is the last section, so now we write out all the headers and
	// finish off the file
	ELFHeader header;
	ZERO_STRUCT(&header);

	if (elf_file->type == ET_EXEC) {
		assert(elf_file->entry_point_virtual_address != -1);

		header.entry_point_virtual_address = elf_file->entry_point_virtual_address;
		header.pht_entries = 3;
		header.pht_location =
			sizeof(ELFHeader) + sizeof(ELFSectionHeader) * NUM_SECTIONS;
		checked_fseek(output_file, header.pht_location, SEEK_SET);

		// Text segment
		{
			SectionInfo *section_info = elf_file->section_info + TEXT_INDEX;
			ELFProgramHeader executable_segment_header;
			ZERO_STRUCT(&executable_segment_header);
			executable_segment_header.type = PT_LOAD;
			executable_segment_header.segment_location = section_info->offset;
			executable_segment_header.segment_size_in_file =
				executable_segment_header.segment_size_in_process =
				section_info->size;
			executable_segment_header.base_virtual_address =
				section_info->virtual_address;
			executable_segment_header.flags = PF_R | PF_X;
			executable_segment_header.alignment = 0x1000;

			checked_fwrite(&executable_segment_header,
					sizeof executable_segment_header,
					1,
					output_file);
		}

		// Zeroed data segment
		{
			SectionInfo *section_info = elf_file->section_info + BSS_INDEX;
			ELFProgramHeader bss_segment_header;
			ZERO_STRUCT(&bss_segment_header);
			bss_segment_header.type = PT_LOAD;
			bss_segment_header.segment_location = section_info->offset;
			bss_segment_header.segment_size_in_file = 0;
			bss_segment_header.segment_size_in_process = section_info->size;
			bss_segment_header.base_virtual_address = section_info->virtual_address;
			bss_segment_header.flags = PF_R | PF_W;
			bss_segment_header.alignment = 0x1000;

			checked_fwrite(&bss_segment_header,
					sizeof bss_segment_header,
					1,
					output_file);
		}

		// Initialized data segment
		{
			SectionInfo *section_info = elf_file->section_info + DATA_INDEX;
			ELFProgramHeader data_segment_header;
			ZERO_STRUCT(&data_segment_header);
			data_segment_header.type = PT_LOAD;
			data_segment_header.segment_location = section_info->offset;
			data_segment_header.segment_size_in_file = section_info->size;
			data_segment_header.segment_size_in_process = section_info->size;
			data_segment_header.base_virtual_address = section_info->virtual_address;
			data_segment_header.flags = PF_R | PF_W;
			data_segment_header.alignment = 0x1000;

			checked_fwrite(&data_segment_header,
					sizeof data_segment_header,
					1,
					output_file);
		}
	} else {
		header.pht_entries = 0;
		header.pht_location = 0;
	}

	checked_fseek(output_file, 0, SEEK_SET);

	header.identifier[ELF_IDENT_MAGIC0] = 0x7F;
	header.identifier[ELF_IDENT_MAGIC1] = 'E';
	header.identifier[ELF_IDENT_MAGIC2] = 'L';
	header.identifier[ELF_IDENT_MAGIC3] = 'F';
	header.identifier[ELF_IDENT_ELF_VERSION] = EV_CURRENT;
	header.elf_version = EV_CURRENT;

	header.header_size = sizeof(ELFHeader);
	header.program_header_size = sizeof(ELFProgramHeader);
	header.section_header_size = sizeof(ELFSectionHeader);

	header.identifier[ELF_IDENT_FILE_CLASS] = ELFCLASS64;
	header.identifier[ELF_IDENT_DATA_ENCODING] = ELFDATA2LSB;
	header.target_architecture = EM_X86_64;
	header.architecture_specific_flags = 0;
	header.object_file_type = elf_file->type;

	header.sht_location = sizeof(ELFHeader);

	header.sht_entries = NUM_SECTIONS;
	header.shstrtab_index = SHSTRTAB_INDEX;

	assert(checked_ftell(output_file) == 0);
	checked_fwrite(&header, sizeof header, 1, output_file);

	checked_fseek(output_file, header.sht_location, SEEK_SET);

	// NULL header
	{
		ELFSectionHeader null_header;
		ZERO_STRUCT(&null_header);
		null_header.type = SHT_NULL;
		checked_fwrite(&null_header, sizeof null_header, 1, output_file);
	}

	// .text
	{
		ELFSectionHeader text_header;
		ZERO_STRUCT(&text_header);
		text_header.shstrtab_index_for_name = TEXT_NAME;
		text_header.type = SHT_PROGBITS;
		text_header.flags = SHF_ALLOC | SHF_EXECINSTR;
		text_header.base_virtual_address =
			elf_file->section_info[TEXT_INDEX].virtual_address;
		text_header.section_location = elf_file->section_info[TEXT_INDEX].offset;
		text_header.section_size = elf_file->section_info[TEXT_INDEX].size;
		checked_fwrite(&text_header, sizeof text_header, 1, output_file);
	}

	// .rela.text
	{
		ELFSectionHeader rela_text_header;
		ZERO_STRUCT(&rela_text_header);
		rela_text_header.shstrtab_index_for_name = RELA_TEXT_NAME;
		rela_text_header.type = SHT_RELA;
		rela_text_header.misc_info = TEXT_INDEX;
		rela_text_header.linked_section = SYMTAB_INDEX;
		rela_text_header.section_location = elf_file->section_info[RELA_TEXT_INDEX].offset;
		rela_text_header.section_size = elf_file->section_info[RELA_TEXT_INDEX].size;
		rela_text_header.entry_size = sizeof(ELF64Rela);
		checked_fwrite(&rela_text_header, sizeof rela_text_header, 1, output_file);
	}

	// .bss
	{
		ELFSectionHeader bss_header;
		ZERO_STRUCT(&bss_header);
		bss_header.shstrtab_index_for_name = BSS_NAME;
		bss_header.type = SHT_NOBITS;
		bss_header.flags = SHF_ALLOC | SHF_WRITE;
		bss_header.base_virtual_address =
			elf_file->section_info[BSS_INDEX].virtual_address;
		bss_header.section_location = elf_file->section_info[BSS_INDEX].offset;
		bss_header.section_size = elf_file->section_info[BSS_INDEX].size;
		checked_fwrite(&bss_header, sizeof bss_header, 1, output_file);
	}

	// .data
	{
		ELFSectionHeader data_header;
		ZERO_STRUCT(&data_header);
		data_header.shstrtab_index_for_name = DATA_NAME;
		data_header.type = SHT_PROGBITS;
		data_header.flags = SHF_ALLOC | SHF_WRITE;
		data_header.base_virtual_address =
			elf_file->section_info[DATA_INDEX].virtual_address;
		data_header.section_location = elf_file->section_info[DATA_INDEX].offset;
		data_header.section_size = elf_file->section_info[DATA_INDEX].size;
		checked_fwrite(&data_header, sizeof data_header, 1, output_file);
	}

	// .shstrtab
	{
		ELFSectionHeader shstrtab_header;
		ZERO_STRUCT(&shstrtab_header);
		shstrtab_header.shstrtab_index_for_name = SHSTRTAB_NAME;
		shstrtab_header.type = SHT_STRTAB;
		shstrtab_header.section_location = elf_file->section_info[SHSTRTAB_INDEX].offset;
		shstrtab_header.section_size = elf_file->section_info[SHSTRTAB_INDEX].size;
		checked_fwrite(&shstrtab_header, sizeof shstrtab_header, 1, output_file);
	}

	// .symtab
	{
		ELFSectionHeader symtab_header;
		ZERO_STRUCT(&symtab_header);
		symtab_header.shstrtab_index_for_name = SYMTAB_NAME;
		symtab_header.type = SHT_SYMTAB;
		// For symbol tables, this field contains 1 + the index of the last
		// local symbol. 0 is undef symbol, 1 is the symbol for the filename,
		// then all the rest of the symbols are global (at the moment).
		// @TODO: Determine this correctly when we handle static correctly.
		symtab_header.misc_info = 2;
		symtab_header.linked_section = STRTAB_INDEX;
		symtab_header.section_location = elf_file->section_info[SYMTAB_INDEX].offset;
		symtab_header.section_size = elf_file->section_info[SYMTAB_INDEX].size;
		symtab_header.entry_size = sizeof(ELF64Symbol);
		checked_fwrite(&symtab_header, sizeof symtab_header, 1, output_file);
	}

	// .strtab
	{
		ELFSectionHeader strtab_header;
		ZERO_STRUCT(&strtab_header);
		strtab_header.shstrtab_index_for_name = STRTAB_NAME;
		strtab_header.type = SHT_STRTAB;
		strtab_header.section_location = elf_file->section_info[STRTAB_INDEX].offset;
		strtab_header.section_size = elf_file->section_info[STRTAB_INDEX].size;
		checked_fwrite(&strtab_header, sizeof strtab_header, 1, output_file);
	}
}

bool write_elf_object_file(char *output_file_name, AsmModule *asm_module)
{
	FILE *output_file = fopen(output_file_name, "wb");
	if (output_file == NULL) {
		perror("Unable to open output file");
		return false;
	}

	ELFFile _elf_file;
	ELFFile *elf_file = &_elf_file;
	init_elf_file(elf_file, output_file, ET_REL);

	Binary binary;
	assemble(asm_module, &binary);
	elf_file->section_info[TEXT_INDEX].size = binary.text.size;
	elf_file->section_info[TEXT_INDEX].contents = binary.text.elements;

	elf_file->section_info[BSS_INDEX].size = binary.bss_size;

	elf_file->section_info[DATA_INDEX].size = binary.data.size;
	elf_file->section_info[DATA_INDEX].contents = binary.data.elements;

	write_contents(elf_file, &asm_module->fixups);

	Array(AsmSymbol *) *symbols = &binary.symbols;
	for (u32 i = 0; i < symbols->size; i++) {
		AsmSymbol *symbol = *ARRAY_REF(symbols, AsmSymbol *, i);
		u32 section = SHN_UNDEF;
		ELFSymbolBinding binding = STB_GLOBAL;
		if (!symbol->defined) {
			section = SHN_UNDEF;
		} else {
			switch (symbol->section) {
			case TEXT_SECTION: section = TEXT_INDEX; break;
			case BSS_SECTION: section = BSS_INDEX; break;
			case DATA_SECTION: section = DATA_INDEX; break;
			}

			switch (symbol->linkage) {
			case ASM_GLOBAL_LINKAGE: binding = STB_GLOBAL; break;
			case ASM_LOCAL_LINKAGE: binding = STB_LOCAL; break;
			}
		}
		add_symbol(elf_file, STT_FUNC, binding, section, symbol->name,
				symbol->offset, symbol->size);
	}
	add_symbol(elf_file, STT_FILE, STB_LOCAL, SHN_ABS, asm_module->input_file_name, 0, 0);
	finish_symtab_section(elf_file);

	for (u32 i = 0; i < symbols->size; i++) {
		AsmSymbol *symbol = *ARRAY_REF(symbols, AsmSymbol *, i);
		add_string(elf_file, symbol->name);
	}
	add_string(elf_file, asm_module->input_file_name);
	finish_strtab_section(elf_file);

	free_binary(&binary);

	fclose(output_file);
	return true;
}

typedef struct Relocation
{
	ELF64RelocType type;
	u32 section_index;
	u32 section_offset;
	i32 addend;
} Relocation;

typedef struct Symbol
{
	bool defined;
	char *name;
	Array(Relocation) relocs;

	// @NOTE: None of the following fields have well-defined values if defined
	// is false.

	// @NOTE: This doesn't tell you which object file the symbol table belongs
	// to. This is just used for locating symbols from within the same object
	// file when processing relocation entries.
	u32 symtab_index;

	ELFSymbolBinding binding;
	u32 section_index;
	u32 section_offset;
	u32 size;
	u8 *contents;
} Symbol;

// @TODO: Handle archives correctly. Currently we just treat them as a series
// of object files, but archives have different semantics.
// See http://eli.thegreenplace.net/2013/07/09/library-order-in-static-linking
// for details.
static bool process_elf_file(FILE *input_file, Binary *binary,
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
	assert(file_header.shstrtab_index < file_header.sht_entries);

	// This should have been checked already
	assert(strneq((char *)file_header.identifier, "\x7F" "ELF", 4));

	if (file_header.target_architecture != EM_X86_64) {
		fprintf(stderr,
				"Invalid architecture (%d given, %d expected)\n",
				file_header.target_architecture, EM_X86_64);
		return false;
	}
	assert(file_header.section_header_size == sizeof(ELFSectionHeader));

	u32 sht_offset = file_header.sht_location;
	checked_fseek(input_file, initial_location + sht_offset, SEEK_SET);

	bool ret = true;
	ELFSectionHeader *headers =
		malloc(sizeof *headers * file_header.sht_entries);
	checked_fread(headers, sizeof *headers, file_header.sht_entries, input_file);

	ELFSectionHeader *shstrtab_header = headers + file_header.shstrtab_index;
	assert(shstrtab_header->type == SHT_STRTAB);
	char *shstrtab = malloc(shstrtab_header->section_size);
	checked_fseek(input_file,
				initial_location + shstrtab_header->section_location,
				SEEK_SET);
	checked_fread(shstrtab, 1, shstrtab_header->section_size, input_file);

	ELFSectionHeader *text_header = NULL;
	u32 text_section_index = SHN_UNDEF;
	ELFSectionHeader *symtab_header = NULL;
	u32 symtab_section_index = SHN_UNDEF;
	ELFSectionHeader *strtab_header = NULL;
	ELFSectionHeader *rela_text_header = NULL;
	u32 data_section_index = SHN_UNDEF;
	ELFSectionHeader *data_header = NULL;
	u32 bss_section_index = SHN_UNDEF;
	ELFSectionHeader *bss_header = NULL;
	for (u32 i = 0; i < file_header.sht_entries; i++) {
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
		} else if (streq(section_name, ".bss")) {
			bss_header = curr_header;
			bss_section_index = i;
		} else if (streq(section_name, ".data")) {
			data_header = curr_header;
			data_section_index = i;
		}
	}

	if (text_header == NULL) {
		fputs("Missing .text section\n", stderr);
		ret = false;
		goto cleanup1;
	}
	if (symtab_header == NULL) {
		fputs("Missing .symtab section\n", stderr);
		ret = false;
		goto cleanup1;
	}
	if (strtab_header == NULL) {
		fputs("Missing .strtab section\n", stderr);
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

	u32 existing_text_size = binary->text.size;
	u32 existing_bss_size = binary->bss_size;
	u32 existing_data_size = binary->data.size;

	u8 *temp_buffer = NULL;

	if (text_header != NULL && text_header->section_size != 0) {
		checked_fseek(input_file,
				initial_location + text_header->section_location,
				SEEK_SET);
		temp_buffer = realloc(temp_buffer, text_header->section_size);
		checked_fread(temp_buffer, text_header->section_size, 1, input_file);
		ARRAY_APPEND_ELEMS(&binary->text, u8, text_header->section_size, temp_buffer);
	}

	if (data_header != NULL && data_header->section_size != 0) {
		checked_fseek(input_file,
				initial_location + data_header->section_location,
				SEEK_SET);
		temp_buffer = realloc(temp_buffer, data_header->section_size);
		checked_fread(temp_buffer, data_header->section_size, 1, input_file);
		ARRAY_APPEND_ELEMS(&binary->data, u8, data_header->section_size, temp_buffer);
	}

	free(temp_buffer);

	if (bss_header != NULL)
		binary->bss_size += bss_header->section_size;

	u32 symbols_in_symtab = symtab_header->section_size / symtab_header->entry_size;
	checked_fseek(input_file,
			initial_location + symtab_header->section_location,
			SEEK_SET);

	Array(Symbol) section_symbols;
	ARRAY_INIT(&section_symbols, Symbol, file_header.sht_entries);
	Symbol **file_symbols = calloc(sizeof(*file_symbols) * symbols_in_symtab, 1);

	// @TODO: We can iterate across this more efficiently by using the
	// information in the header about the last local symbol. We can also use
	// this to allocate fewer symbols.
	for (u32 symtab_index = 0; symtab_index < symbols_in_symtab; symtab_index++) {
		ELF64Symbol symtab_symbol;
		checked_fread(&symtab_symbol, sizeof symtab_symbol, 1, input_file);

		char *symbol_name = strtab + symtab_symbol.strtab_index_for_name;
		ELFSymbolType type = ELF64_SYMBOL_TYPE(symtab_symbol.type_and_binding);
		ELFSymbolBinding binding = ELF64_SYMBOL_BINDING(symtab_symbol.type_and_binding);

		if (symtab_index == 0 || type == STT_FILE
				|| (type == STT_SECTION
					&& symtab_symbol.section != text_section_index
					&& symtab_symbol.section != bss_section_index
					&& symtab_symbol.section != data_section_index)) {
			continue;
		}

		i32 found_symbol_index = -1;
		if (binding == STB_GLOBAL) {
			for (u32 symbol_index = 0;
					symbol_index < symbol_table->size;
					symbol_index++) {
				Symbol *symbol = ARRAY_REF(symbol_table, Symbol, symbol_index);
				if (symbol->binding == STB_GLOBAL
						&& streq(symbol->name, symbol_name)) {
					found_symbol_index = symbol_index;
					break;
				}
			}
		}

		if (symtab_symbol.section == SHN_UNDEF) {
			Symbol *symbol;
			if (found_symbol_index == -1) {
				symbol = ARRAY_APPEND(symbol_table, Symbol);
				symbol->defined = false;
				symbol->name = strdup(symbol_name);
				symbol->binding = binding;
				symbol->contents = NULL;
				ARRAY_INIT(&symbol->relocs, Relocation, 5);
			} else {
				symbol = ARRAY_REF(symbol_table, Symbol, found_symbol_index);
			}

			file_symbols[symtab_index] = symbol;
		} else if (symtab_symbol.section == text_section_index
				|| symtab_symbol.section == bss_section_index
				|| symtab_symbol.section == data_section_index) {
			Symbol *symbol;
			if (found_symbol_index == -1) {
				symbol = ARRAY_APPEND(symbol_table, Symbol);
				ZERO_STRUCT(symbol);
				ARRAY_INIT(&symbol->relocs, Relocation, 5);
			} else if (type == STT_SECTION) {
				symbol = ARRAY_APPEND(&section_symbols, Symbol);
			} else {
				symbol = ARRAY_REF(symbol_table, Symbol, found_symbol_index);
				if (symbol->defined) {
					fprintf(stderr,
							"Multiple definitions of symbol '%s'\n",
							symbol_name);
					goto cleanup2;
				}
			}

			if (symtab_symbol.section == text_section_index) {
				symbol->section_index = TEXT_INDEX;
				symbol->section_offset = existing_text_size + symtab_symbol.value;
			} else if (symtab_symbol.section == bss_section_index) {
				symbol->section_index = BSS_INDEX;
				symbol->section_offset = existing_bss_size + symtab_symbol.value;
			} else if (symtab_symbol.section == data_section_index) {
				symbol->section_index = DATA_INDEX;
				symbol->section_offset = existing_data_size + symtab_symbol.value;
			} else {
				UNREACHABLE;
			}

			symbol->defined = true;
			symbol->name = strdup(symbol_name);
			symbol->binding = binding;
			symbol->size = symtab_symbol.size;
			symbol->contents = NULL;

			file_symbols[symtab_index] = symbol;
		} else {
			UNREACHABLE;
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

			Relocation *reloc = ARRAY_APPEND(&corresponding_symbol->relocs, Relocation);
			reloc->type = type;
			reloc->addend = rela.addend;
			reloc->section_index = TEXT_INDEX;
			reloc->section_offset = existing_text_size + rela.section_offset;
		}
	}


cleanup2:
	array_free(&section_symbols);
	free(file_symbols);
	free(strtab);
cleanup1:
	free(shstrtab);
	free(headers);
	return ret;
}

typedef struct ArFileHeader
{
	char name[16];
	char modification_timestamp_decimal[12];
	char owner_id_decimal[6];
	char group_id_decimal[6];
	char mode_octal[8];
	char size_bytes_decimal[10];
	char magic[2];
} __attribute__((packed)) ArFileHeader;

// @TODO: Add .note.GNU-STACK section header to prevent executable stack.
bool link_elf_executable(char *executable_file_name, Array(char *) *linker_input_filenames)
{
	bool ret = true;

	FILE *output_file = fopen(executable_file_name, "wb");
	if (output_file == NULL) {
		perror("Failed to open linker output");
		return false;
	}

	Binary binary;
	init_binary(&binary);
	// @TODO: Merge with AsmSymbol so we can just use Binary?
	Array(Symbol) symbol_table;
	ARRAY_INIT(&symbol_table, Symbol, 100);

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
			if (!process_elf_file(input_file, &binary, &symbol_table)) {
				ret = false;
				goto cleanup;
			}
			break;
		case AR_FILE_TYPE: {
			u32 global_header_length = sizeof "!<arch>\n" - 1;
			checked_fseek(input_file, global_header_length, SEEK_SET);

			for (;;) {
				// File headers are aligned to even byte boundaries.
				if (checked_ftell(input_file) % 2 == 1)
					checked_fseek(input_file, 1, SEEK_CUR);

				ArFileHeader header;
				int result = fread(&header, sizeof header, 1, input_file);
				if (result == 0) {
					assert(feof(input_file));
					break;
				}
				long file_start = checked_ftell(input_file);

				assert(header.magic[0] == 0x60 && header.magic[1] == 0x0A);

				// Filenames are terminated with '/'
				for (i32 i = sizeof header.name - 1; i >= 0; i--) {
					if (header.name[i] == '/') {
						header.name[i] = '\0';
						break;
					}
				}

				char file_size_bytes_decimal[sizeof header.size_bytes_decimal + 1];
				memcpy(file_size_bytes_decimal, header.size_bytes_decimal,
						sizeof header.size_bytes_decimal);
				file_size_bytes_decimal[sizeof file_size_bytes_decimal - 1] = '\0';
				long file_size_bytes = atol(file_size_bytes_decimal);

				// The file with an empty name is a special file, used to store
				// a symbol index in System V ar. We don't care about it for
				// now.
				if (!streq(header.name, "") &&
						!process_elf_file(input_file, &binary, &symbol_table)) {
					ret = false;
					goto cleanup;
				}

				fseek(input_file, file_start + file_size_bytes, SEEK_SET);
			}

			break;
		}
		// We should have checked it was an object file or archive before
		// putting it on the linker input list.
		case UNKNOWN_FILE_TYPE:
			UNREACHABLE;
		}

		fclose(input_file);
	}

	ELFFile _elf_file;
	ELFFile *elf_file = &_elf_file;
	init_elf_file(elf_file, output_file, ET_EXEC);


	elf_file->section_info[TEXT_INDEX].size = binary.text.size;
	elf_file->section_info[TEXT_INDEX].contents = binary.text.elements;
	Array(Fixup) empty_array = EMPTY_ARRAY;

	elf_file->section_info[BSS_INDEX].size = binary.bss_size;

	elf_file->section_info[DATA_INDEX].size = binary.data.size;
	elf_file->section_info[DATA_INDEX].contents = binary.data.elements;

	write_contents(elf_file, &empty_array);

	add_symbol(elf_file, STT_FILE, STB_LOCAL, SHN_ABS, executable_file_name, 0, 0);
	for (u32 i = 0; i < symbol_table.size; i++) {
		Symbol *symbol = ARRAY_REF(&symbol_table, Symbol, i);

		if (!symbol->defined) {
			fprintf(stderr, "Undefined symbol '%s'\n", symbol->name);
			ret = false;
			goto cleanup;
		}

		SectionInfo *symbol_section_info =
			elf_file->section_info + symbol->section_index;

		// Since this is an executable, the symbol value is the location of the
		// symbol in memory once loaded, not an offset into the corresponding
		// section.
		u32 symbol_mem_location =
			symbol_section_info->virtual_address + symbol->section_offset;

		ELFSymbolType type;
		switch (symbol->section_index) {
		case TEXT_INDEX: type = STT_FUNC; break;
		case BSS_INDEX: case DATA_INDEX: type = STT_OBJECT; break;
		default: UNREACHABLE;
		}

		add_symbol(elf_file, type, symbol->binding, symbol->section_index,
				symbol->name, symbol_mem_location, symbol->size);

		u32 prev_location = checked_ftell(output_file);
		for (u32 i = 0; i < symbol->relocs.size; i++) {
			Relocation *reloc = ARRAY_REF(&symbol->relocs, Relocation, i);
			SectionInfo *reloc_section_info =
				elf_file->section_info + reloc->section_index;
			u32 reloc_mem_location =
				reloc_section_info->virtual_address + reloc->section_offset;

			u32 final_value;
			switch (reloc->type) {
			case R_X86_64_PC32:
				final_value =
					symbol_mem_location + reloc->addend - (i32)reloc_mem_location;
				break;
			case R_X86_64_64:
				final_value = symbol_mem_location + reloc->addend;
				break;
			}

			u8 final_value_bytes[] = {
				final_value & 0xFF,
				(final_value >> 8) & 0xFF,
				(final_value >> 16) & 0xFF,
				(final_value >> 24) & 0xFF,
			};

			u32 file_offset =
				reloc_mem_location -
				reloc_section_info->virtual_address +
				reloc_section_info->offset;
			checked_fseek(output_file, file_offset, SEEK_SET);
			checked_fwrite(final_value_bytes,
					1,
					sizeof final_value_bytes,
					output_file);
#if 0
			printf("Relocation %u for '%s': wrote 0x%x at offset 0x%x\n",
					i, symbol->name, final_value, file_offset);
			printf("rml = 0x%x, sml = 0x%x, add = %d, va = 0x%x, o = 0x%x\n",
					reloc_mem_location, symbol_mem_location, reloc->addend,
					reloc_section_info->virtual_address, reloc_section_info->offset);
#endif
		}
		checked_fseek(output_file, prev_location, SEEK_SET);

		array_free(&symbol->relocs);
	}
	finish_symtab_section(elf_file);

	add_string(elf_file, executable_file_name);
	for (u32 i = 0; i < symbol_table.size; i++) {
		Symbol *symbol = ARRAY_REF(&symbol_table, Symbol, i);
		add_string(elf_file, symbol->name);
	}
	finish_strtab_section(elf_file);

cleanup:
	fclose(output_file);
	array_free(&symbol_table);
	free_binary(&binary);
	return ret;
}
