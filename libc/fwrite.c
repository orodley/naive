#include <unistd.h>
#include <stdio.h>

#include "io_file_struct.h"

// The world's dumbest unbuffered fwrite implementation.
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream)
{
	// Yes, this could overflow. Whatever.
	size_t bytes_to_write = size * nmemb;
	size_t written_so_far = 0;
	const char *buf = ptr;

	while (written_so_far != bytes_to_write) {
		ssize_t written_this_time = write(stream->fd, buf, bytes_to_write);
		if (written_this_time == -1)
			return written_so_far / size;

		written_so_far += written_this_time;
		buf += written_this_time;
	}

	return nmemb;
}
