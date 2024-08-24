#ifndef NAIVE_BACKEND_ELF_H_
#define NAIVE_BACKEND_ELF_H_

#include "array.h"
#include "assertions.h"
#include "backend/asm.h"

ExitCode write_elf_object_file(String output_file_name, AsmModule *asm_module);
ExitCode link_elf_executable(
    String executable_filename, Array(String) *linker_input_filenames);

#endif
