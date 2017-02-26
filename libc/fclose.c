#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

#include "io_file_struct.h"

int fclose(_IO_FILE *fp)
{
	int ret = close(fp->fd);
	free(fp);

	return ret < 0 ? EOF : 0;
}
