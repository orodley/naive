#include <stdlib.h>

#include "util.h"

char *strndup(char *str, u32 length)
{
	char *result = malloc(length + 1);
	strncpy(result, str, length);
	result[length] = '\0';

	return result;
}

char *strdup(char *str)
{
	return strndup(str, strlen(str));
}

extern inline bool streq(char *a, char *b);
extern inline bool strneq(char *a, char *b, u32 length);

extern inline u32 lowest_set_bit(u32 x);
extern inline u32 bit_count(u32 x);
