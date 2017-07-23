#include <stdlib.h>
#include <string.h>

char *strndup(const char *s, size_t n)
{
	char *result = malloc(n + 1);
	memcpy(result, s, n);
	result[n] = '\0';

	return result;
}
