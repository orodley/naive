// Include this everywhere for types and stuff

#ifndef NAIVE_MISC_H_
#define NAIVE_MISC_H_

#include <stdint.h>
#include <inttypes.h>

typedef uint8_t  u8;
typedef  int8_t  i8;
typedef uint16_t u16;
typedef  int16_t i16;
typedef uint32_t u32;
typedef  int32_t i32;
typedef uint64_t u64;
typedef  int64_t i64;

#include <stdbool.h>
#include <stddef.h>
#include <string.h>

#define IGNORE(x) (void)x
#define STATIC_ARRAY_LENGTH(array) (sizeof(array) / sizeof((array)[0]))
#define ZERO_STRUCT(s) memset(&s, 0, sizeof s);
#define UNREACHABLE assert(!"This should never be reached")
#define UNIMPLEMENTED assert(!"Not implemented")


char *strndup(char *str, u32 length);

inline bool streq(char *a, char *b)
{
	return strcmp(a, b) == 0;
}

#endif
