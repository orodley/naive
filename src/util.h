// Misc. utility functions

#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>

#include "misc.h"

#define ZERO_STRUCT(s) memset(s, 0, sizeof *s);

inline u32 max(u32 a, u32 b)
{
	return (a < b) ? b : a;
}

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

inline u32 lowest_set_bit(u32 x)
{
	assert(x != 0);

	u32 i = 0;
	while ((x & 1) != 1) {
		x >>= 1;
		i++;
	}

	return i;
}

inline u32 highest_set_bit(u32 x)
{
	assert(x != 0);

	u32 n;
	u32 i = 0;
	while (x != 0) {
		if ((x & 1) == 1)
			n = i;

		i++;
		x >>= 1;
	}

	return n;
}

inline u32 bit_count(u32 x)
{
	u32 n = 0;
	for (; x != 0; n++)
	{
		x &= x - 1; // clear the least significant bit set
	}

	return n;
}

inline u32 align_to(u32 n, u32 align)
{
	if (n % align != 0)
		n += align - (n % align);

	return n;
}
