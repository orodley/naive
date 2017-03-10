#include <stddef.h>

long atol(const char *nptr)
{
	long x = 0;
	for (size_t i = 0; nptr[i] != '\0'; i++) {
		int digit = nptr[i] - '0';
		x = x * 10 + digit;
	}

	return x;
}
