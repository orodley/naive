#include <stddef.h>
#include <string.h>
#include <unistd.h>

void *memmove(void *_dest, const void *_src, size_t n)
{
  unsigned char *dest = (unsigned char *)_dest;
  unsigned char *src = (unsigned char *)_src;

  unsigned char *dest_end = dest + n;
  unsigned char *src_end = src + n;

  if (dest_end > src && dest_end < src_end) {
    // dest   |-----|
    // src      |-----|
    // Copy one-by-one from the start to avoid overwriting.
    for (size_t i = 0; i < n; i++) {
      dest[i] = src[i];
    }
  } else if (src_end > dest && src_end < dest_end) {
    // dest   |-----|
    // src  |-----|
    // Copy one-by-one from the end to avoid overwriting.
    for (ssize_t i = n - 1; i >= 0; i--) {
      dest[i] = src[i];
    }
  } else if (dest != src) {
    memcpy(dest, src, n);
  }

  return dest;
}
