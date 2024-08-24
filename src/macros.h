#ifndef NAIVE_MACROS_H_
#define NAIVE_MACROS_H_

#define IGNORE(x) (void)x
#define STATIC_ARRAY_LENGTH(array) (sizeof(array) / sizeof((array)[0]))
#define ZERO_STRUCT(s) memset(s, 0, sizeof *(s));

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