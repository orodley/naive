#include <fcntl.h>
#include <stdlib.h>
#include <string.h>

#include "io_file_struct.h"

struct _IO_FILE *fopen(const char *path, const char *mode)
{
	int flags = 0;
	size_t mode_len = strlen(mode);

	if (mode_len != 1 && mode_len != 2) {
		// Unimplemented
		abort();
	}
	switch (mode[0]) {
	case 'r':
		flags = O_RDONLY;
		break;
	case 'w':
		flags = O_WRONLY | O_CREAT | O_TRUNC;
		break;
	default:
		// Unimplemented
		abort();
	}

	// NOP on Linux - text and binary files are treated the same
	if (mode_len == 2 && mode[1] != 'b') {
		// Unimplemented
		abort();
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
		struct _IO_FILE *fp = malloc(sizeof *fp);
		fp->fd = fd;
		return fp;
	}
}
