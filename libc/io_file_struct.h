#ifndef _IO_FILE_STRUCT
#define _IO_FILE_STRUCT

#include <stdbool.h>

typedef struct _IO_FILE
{
	int fd;
	bool eof;
} _IO_FILE;

#endif
