#include "string_sink.h"

int string_sink(void *sink_arg, char c)
{
  StringSinkArg *string_sink_arg = sink_arg;

  if (!string_sink_arg->has_limit
      || string_sink_arg->index < string_sink_arg->max_chars) {
    string_sink_arg->string[string_sink_arg->index] = c;
    string_sink_arg->index++;
  }

  return 0;
}
