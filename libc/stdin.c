#include "io_file_struct.h"

static struct _IO_FILE __stdin = {
    .fd = 0,
    .eof = false,
};

struct _IO_FILE *stdin = &__stdin;
