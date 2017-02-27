#include <stdio.h>
#include <unistd.h>

#include "io_file_struct.h"

int fseek(FILE *stream, long offset, int whence)
{
	return lseek(stream->fd, offset, whence) != -1;
}
