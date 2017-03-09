#include "io_file_struct.h"

int feof(_IO_FILE *stream)
{
	return stream->eof;
}
