#ifndef NAIVE_FILE_H_
#define NAIVE_FILE_H_

#include <stdio.h>

#include "misc.h"

typedef enum FileType
{
	ELF_FILE_TYPE,
	AR_FILE_TYPE,
	UNKNOWN_FILE_TYPE,
} FileType;

FileType file_type_of_bytes(u8 *bytes, u32 length);
FileType file_type(FILE *file);

#endif
