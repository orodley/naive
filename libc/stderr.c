#include "io_file_struct.h"

static _IO_FILE __stderr = {
	.fd = 2,
	.eof = false,
};

_IO_FILE *stderr = &__stderr;
