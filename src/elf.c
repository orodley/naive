#include <stdio.h>

#include "asm.h"
#include "misc.h"

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
	u32 string_table_index_for_name;
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

static void write_crt0(FILE *output_file, i32 main_address_offset);

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

	// @TODO: We'll want to use ET_REL when we are passed -c
	header.object_file_type = ET_EXEC;

	// We aren't producing linkable files yet, so we don't need a section table
	// or any sections
	header.section_header_table_location = 0;
	header.section_header_entries = 0;
	header.section_header_table_string_table_index = SHN_UNDEF;

	header.program_header_table_location = sizeof(ELFHeader);
	header.program_header_entries = 1;

	u32 first_segment_location =
		header.program_header_table_location +
		(header.program_header_entries * sizeof(ELFProgramHeader));

	ELFProgramHeader executable_segment;
	executable_segment.type = PT_LOAD;
	executable_segment.segment_location = first_segment_location;
	executable_segment.base_virtual_address = 0x8000000 + first_segment_location;
	executable_segment.flags = PF_R | PF_X;
	executable_segment.alignment = 0x1000;

	fseek(output_file, first_segment_location, SEEK_SET);
	u64 main_virtual_addr = assemble(asm_module, output_file,
			executable_segment.base_virtual_address);

	i32 crt_offset = ftell(output_file) - first_segment_location;
	header.entry_point_virtual_address =
		executable_segment.base_virtual_address + crt_offset;

	write_crt0(output_file, main_virtual_addr - header.entry_point_virtual_address);

	u32 executable_segment_size = ftell(output_file) - first_segment_location;
	executable_segment.segment_size_in_file =
		executable_segment.segment_size_in_process = executable_segment_size;

	fseek(output_file, 0, SEEK_SET);
	fwrite(&header, sizeof(header), 1, output_file);
	fwrite(&executable_segment, sizeof(executable_segment), 1, output_file);
}

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

	fwrite(crt0, 1, sizeof(crt0), output_file);
}
