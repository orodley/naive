#include <stddef.h>

void *memcpy(void *dest, const void *src, size_t n)
{
	unsigned char *dest_bytes = (unsigned char *)dest;
	unsigned char *src_bytes = (unsigned char *)src;
	for (size_t i = 0; i < n; i++) {
		dest_bytes[i] = src_bytes[i];
	}

	return dest;
}
