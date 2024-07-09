#include "io_file_struct.h"

int ferror(struct _IO_FILE *stream) { return stream->error; }
