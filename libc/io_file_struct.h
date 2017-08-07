#ifndef _IO_FILE_STRUCT
#define _IO_FILE_STRUCT

#include <stdbool.h>

struct _IO_FILE
{
	int fd;
	bool eof;
};

#endif
