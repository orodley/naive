#include "io_file_struct.h"

int feof(struct _IO_FILE *stream)
{
  return stream->eof;
}
