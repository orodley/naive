#ifndef NAIVE_ELF_H_
#define NAIVE_ELF_H_

#include "array.h"
#include "asm.h"
#include "assertions.h"

ExitCode write_elf_object_file(char *output_file_name, AsmModule *asm_module);
ExitCode link_elf_executable(
    char *executable_filename, Array(char *) *linker_input_filenames);

#endif
