#ifndef NAIVE_ELF_H_
#define NAIVE_ELF_H_

#include <stdio.h>

#include "asm.h"

void write_elf_file(FILE *output_file, AsmModule *asm_module, bool do_link);

#endif
