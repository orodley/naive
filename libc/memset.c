#include <stddef.h>

void *memset(void *s, int c, size_t n)
{
	unsigned char *bytes = (unsigned char *)s;
	for (size_t i = 0; i < n; i++) {
		bytes[i] = (unsigned char)c;
	}

	return s;
}
