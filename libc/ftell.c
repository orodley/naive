#include <stdio.h>
#include <unistd.h>

#include "io_file_struct.h"

long ftell(FILE *stream)
{
	return lseek(stream->fd, 0, SEEK_CUR);
}
