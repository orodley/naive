#include <stdio.h>

int file_sink(void *sink_arg, char c)
{
  FILE *stream = sink_arg;
  return fputc(c, stream) == EOF ? -1 : 0;
}
