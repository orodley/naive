// Misc. utility functions

#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>

#include "misc.h"

#define ZERO_STRUCT(s) memset(s, 0, sizeof *s);

char *strndup(char *str, u32 length);
char *strdup(char *str);

inline bool streq(char *a, char *b)
{
	return strcmp(a, b) == 0;
}

inline bool strneq(char *a, char *b, u32 length)
{
	return strncmp(a, b, length) == 0;
}
