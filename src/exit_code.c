#include "exit_code.h"

#include <stdio.h>

extern inline void exit_with_code(ExitCode code);

void _exit_unimplemented(const char *file, int line, const char *format, ...)
{
  va_list args;
  va_start(args, format);

  fprintf(stderr, "%s:%d: Unimplemented: ", file, line);
  vfprintf(stderr, format, args);
  fputc('\n', stderr);

  exit_with_code(EXIT_CODE_UNIMPLEMENTED);
}