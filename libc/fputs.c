#include <stdio.h>
#include <string.h>

int fputs(const char *s, FILE *stream)
{
  return fwrite(s, 1, strlen(s), stream);
}
