// Misc. utility functions

#ifndef NAIVE_UTIL_H_
#define NAIVE_UTIL_H_

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

// @TODO: Maybe use this more widely.
typedef struct String
{
	char *chars;
	u32 len;
} String;

#define INVALID_STRING ((String) { NULL, -1 })
#define EMPTY_STRING ((String) { NULL, 0 })

inline bool is_valid(String str)
{
	return !((str.chars == INVALID_STRING.chars) && (str.len == INVALID_STRING.len));
}

char *strndup(const char *str, size_t length);
char *strdup(const char *str);

inline bool streq(char *a, char *b)
{
	return strcmp(a, b) == 0;
}

inline bool strneq(char *a, char *b, u32 length)
{
	return strncmp(a, b, length) == 0;
}

inline u32 lowest_set_bit(u64 x)
{
	assert(x != 0);

	u32 i = 0;
	while ((x & 1UL) != 1) {
		x >>= 1;
		i++;
	}

	return i;
}

inline u32 highest_set_bit(u64 x)
{
	assert(x != 0);

	u32 n;
	u32 i = 0;
	while (x != 0) {
		if ((x & 1UL) == 1)
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

// @NOTE: align must be a power of two
inline u32 align_to(u32 n, u32 align)
{
	u32 x = align - 1;
	return (n + x) & ~x;
}

#endif
