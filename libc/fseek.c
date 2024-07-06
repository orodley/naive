#include <stdio.h>
#include <unistd.h>

#include "io_file_struct.h"

int fseek(struct _IO_FILE *stream, long offset, int whence)
{
  return lseek(stream->fd, offset, whence) != -1;
}
