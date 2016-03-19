#ifndef ELF_H_
#define ELF_H_

#include <stdio.h>

#include "asm.h"

void write_elf_file(FILE *output_file, AsmModule *asm_module);

#endif
