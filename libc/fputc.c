#include <stdio.h>

int fputc(int c, FILE *stream)
{
  unsigned char ch = c;
  return fwrite(&ch, 1, 1, stream) == 1 ? ch : EOF;
}
