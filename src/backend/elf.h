#ifndef NAIVE_BACKEND_ELF_H_
#define NAIVE_BACKEND_ELF_H_

#include "array.h"
#include "assertions.h"
#include "backend/asm.h"

ExitCode write_elf_object_file(char *output_file_name, AsmModule *asm_module);
ExitCode link_elf_executable(
    char *executable_filename, Array(char *) *linker_input_filenames);

#endif
