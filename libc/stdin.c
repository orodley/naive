#include "io_file_struct.h"

static _IO_FILE __stdin = {
	.fd = 0
};

_IO_FILE *stdin = &__stdin;
