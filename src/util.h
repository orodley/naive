// Misc. utility functions

#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>

#include "misc.h"

#define ZERO_STRUCT(s) memset(s, 0, sizeof *s);

char *strndup(char *str, u32 length);

inline bool streq(char *a, char *b)
{
	return strcmp(a, b) == 0;
}


static inline u64 checked_ftell(FILE *file)
{
	long ret = ftell(file);
	assert(ret != -1);

	return (u64)ret;
}
