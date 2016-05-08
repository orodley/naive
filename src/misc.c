#include <stdlib.h>
#include <string.h>

#include "misc.h"

char *strndup(char *str, u32 length)
{
	char *result = malloc(length + 1);
	strncpy(result, str, length);
	result[length] = '\0';

	return result;
}

extern inline bool streq(char *a, char *b);
