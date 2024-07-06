#include <stdarg.h>

#include "printf_impl.h"
#include "string_sink.h"

int vsnprintf(char *str, size_t size, const char *format, va_list ap)
{
  StringSinkArg string_sink_arg = {
      .string = str,
      .index = 0,
      .max_chars = size,
      .has_limit = true,
  };

  return printf_impl(string_sink, &string_sink_arg, format, ap);
}
