#include <unistd.h>
#include <stdio.h>

#include "io_file_struct.h"

// The world's dumbest unbuffered fread implementation.
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream)
{
	// Yes, this could overflow. Whatever.
	size_t bytes_to_read = size * nmemb;
	size_t read_so_far = 0;
	char *buf = ptr;

	while (read_so_far != bytes_to_read) {
		ssize_t read_this_time = read(stream->fd, buf, bytes_to_read);
		if (read_this_time == -1)
			return read_so_far / size;

		read_so_far += read_this_time;
		buf += read_this_time;
	}

	return nmemb;
}
