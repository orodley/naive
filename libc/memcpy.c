#include <stddef.h>

void *memcpy(void *_dest, const void *_src, size_t n)
{
	unsigned char *dest = (unsigned char *)_dest;
	unsigned char *src = (unsigned char *)_src;
	for (size_t i = 0; i < n; i++) {
		dest[i] = src[i];
	}

	return dest;
}
