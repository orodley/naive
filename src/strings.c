#include "strings.h"

#include <stdarg.h>
#include <stdio.h>

extern inline char *string_to_c_string(String str);
extern inline bool is_valid(String str);

extern inline bool streq(char *a, char *b);
extern inline bool strneq(char *a, char *b, u32 length);
extern inline bool string_eq(String a, String b);
extern inline String string_dup(String str);

String string_concat(String str_a, String str_b)
{
  u32 result_length = str_a.len + str_b.len;
  char *result = malloc(result_length + 1);
  memcpy(result, str_a.chars, str_a.len);
  memcpy(result + str_a.len, str_b.chars, str_b.len);
  result[result_length] = '\0';

  return (String){result, result_length};
}

String string_printf(char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);

  va_list varargs_copy;
  va_copy(varargs_copy, varargs);

  int length = vsnprintf(NULL, 0, fmt, varargs);
  va_end(varargs);

  char *result = malloc(length + 1);
  vsnprintf(result, length + 1, fmt, varargs_copy);
  va_end(varargs_copy);

  return (String){result, length};
}