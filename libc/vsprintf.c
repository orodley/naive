#include <stdarg.h>

#include "printf_impl.h"
#include "string_sink.h"

int vsprintf(char *str, const char *format, va_list ap)
{
  StringSinkArg string_sink_arg = {
      .string = str,
      .index = 0,
      .has_limit = false,
  };

  return printf_impl(string_sink, &string_sink_arg, format, ap);
}
