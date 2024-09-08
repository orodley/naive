#ifndef NAIVE_STRINGS_H_
#define NAIVE_STRINGS_H_

#include <stdlib.h>
#include <string.h>

#include "types.h"

// @TODO: Maybe use this more widely.
typedef struct String
{
  char *chars;
  u32 len;
} String;

#define INVALID_STRING ((String){NULL, -1})
#define EMPTY_STRING ((String){NULL, 0})
#define STRING(s) ((String){(s), strlen(s)})
#define LS(s) ((String){s, sizeof(s) - 1})

inline char *string_to_c_string(String str)
{
  char *result = malloc(str.len + 1);
  memcpy(result, str.chars, str.len);
  result[str.len] = '\0';

  return result;
}

inline bool is_valid(String str)
{
  return !(
      (str.chars == INVALID_STRING.chars) && (str.len == INVALID_STRING.len));
}

inline bool streq(char *a, char *b) { return strcmp(a, b) == 0; }

// @TODO: There are lots of users of this function that could be replaced with
// string_eq.
inline bool strneq(char *a, char *b, u32 length)
{
  return strncmp(a, b, length) == 0;
}

inline bool string_eq(String a, String b)
{
  if (a.len != b.len) return false;
  return strneq(a.chars, b.chars, a.len);
}

inline String string_dup(String str)
{
  String result = {
      .chars = malloc(str.len),
      .len = str.len,
  };
  memcpy(result.chars, str.chars, str.len);

  return result;
}

String string_concat(String str_a, String str_b);

#endif