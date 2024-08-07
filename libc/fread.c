#include <errno.h>
#include <stdio.h>
#include <unistd.h>

#include "io_file_struct.h"

// The world's dumbest unbuffered fread implementation.
size_t fread(void *ptr, size_t size, size_t nmemb, struct _IO_FILE *stream)
{
  // Yes, this could overflow. Whatever.
  size_t bytes_to_read = size * nmemb;
  size_t read_so_far = 0;
  char *buf = ptr;

  while (read_so_far != bytes_to_read) {
    ssize_t read_this_time = read(stream->fd, buf, bytes_to_read - read_so_far);
    if (read_this_time == 0) {
      stream->eof = true;
      stream->error = false;
      return read_so_far / size;
    } else if (read_this_time < 0) {
      stream->eof = false;
      stream->error = true;
      errno = -read_this_time;
      return -1;
    }

    read_so_far += read_this_time;
    buf += read_this_time;
  }

  return nmemb;
}
