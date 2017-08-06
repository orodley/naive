#include <assert.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>

#include "io_file_struct.h"

_IO_FILE *fopen(const char *path, const char *mode)
{
	int flags = 0;
	size_t mode_len = strlen(mode);

	assert(mode_len == 1 || mode_len == 2);
	switch (mode[0]) {
	case 'r':
		flags = O_RDONLY;
		break;
	case 'w':
		flags = O_WRONLY | O_CREAT | O_TRUNC;
		break;
	default:
		assert(!"Unimplemented");
	}

	if (mode_len == 2) {
		assert(mode[1] == 'b');
		// NOP on Linux - text and binary files are treated the same
	}

	int fd;
	if (flags & O_CREAT) {
		fd = open(path, flags, 0666);
	} else {
		fd = open(path, flags);
	}

	if (fd == -1) {
		return NULL;
	} else {
		_IO_FILE *fp = malloc(sizeof *fp);
		fp->fd = fd;
		return fp;
	}
}
