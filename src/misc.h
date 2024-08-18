// Include this everywhere for types and macros

#ifndef NAIVE_MISC_H_
#define NAIVE_MISC_H_

#include <inttypes.h>
#include <stdint.h>

typedef uint8_t u8;
typedef int8_t i8;
typedef uint16_t u16;
typedef int16_t i16;
typedef uint32_t u32;
typedef int32_t i32;
typedef uint64_t u64;
typedef int64_t i64;

#include <stdbool.h>
#include <stddef.h>

#define IGNORE(x) (void)x
#define STATIC_ARRAY_LENGTH(array) (sizeof(array) / sizeof((array)[0]))
#define UNREACHABLE assert(!"This should never be reached")

#define RUNNING_UNDER_SANITIZER 0
#ifdef __has_feature
#if __has_feature(memory_sanitizer) || __has_feature(address_sanitizer)
#undef RUNNING_UNDER_SANITIZER
#define RUNNING_UNDER_SANITIZER 1
#endif
#endif

#define NORETURN
#ifdef __has_attribute
#if __has_attribute(noreturn)
#undef NORETURN
#define NORETURN __attribute__((noreturn))
#endif
#endif

#endif
