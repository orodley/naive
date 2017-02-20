#include "io_file_struct.h"

static _IO_FILE __stdout = {
	.fd = 1
};

_IO_FILE *stdout = &__stdout;
