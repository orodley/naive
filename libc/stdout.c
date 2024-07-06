#include "io_file_struct.h"

static struct _IO_FILE __stdout = {
    .fd = 1,
    .eof = false,
};

struct _IO_FILE *stdout = &__stdout;
