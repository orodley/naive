#ifndef NAIVE_ELF_H_
#define NAIVE_ELF_H_

#include <stdio.h>

#include "array.h"
#include "asm.h"

void write_elf_object_file(FILE *output_file, AsmModule *asm_module);

bool link_elf_executable(char *executable_filename, Array(char *) *linker_input_filenames);

#endif
