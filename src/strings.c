#include "strings.h"

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