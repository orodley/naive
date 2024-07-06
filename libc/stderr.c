#include "io_file_struct.h"

static struct _IO_FILE __stderr = {
    .fd = 2,
    .eof = false,
};

struct _IO_FILE *stderr = &__stderr;
