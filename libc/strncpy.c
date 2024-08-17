#include <stddef.h>

char *strncpy(char *dest, const char *src, size_t count)
{
  size_t i;
  for (i = 0; i < count && src[i] != '\0'; i++) dest[i] = src[i];
  for (; i < count; i++) dest[i] = '\0';
  return dest;
}