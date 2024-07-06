#include <stdarg.h>

#include "printf_impl.h"
#include "string_sink.h"

int sprintf(char *str, const char *format, ...)
{
  va_list ap;
  va_start(ap, format);

  StringSinkArg string_sink_arg = {
    .string = str,
    .index = 0,
    .has_limit = false,
  };

  int ret = printf_impl(string_sink, &string_sink_arg, format, ap);
  str[ret++] = '\0';

  va_end(ap);
  return ret;
}
