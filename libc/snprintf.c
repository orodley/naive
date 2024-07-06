#include <stdarg.h>

#include "printf_impl.h"
#include "string_sink.h"

int snprintf(char *str, size_t size, const char *format, ...)
{
  va_list ap;
  va_start(ap, format);

  StringSinkArg string_sink_arg = {
      .string = str,
      .index = 0,
      .max_chars = size,
      .has_limit = true,
  };

  int ret = printf_impl(string_sink, &string_sink_arg, format, ap);
  if ((size_t)ret == size) {
    str[ret - 1] = '\0';
  } else {
    str[ret++] = '\0';
  }

  va_end(ap);
  return ret;
}
