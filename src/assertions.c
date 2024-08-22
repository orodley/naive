#include "assertions.h"

#include <stdarg.h>
#include <stdio.h>

#include "types.h"

extern inline void exit_with_code(ExitCode code);

NORETURN void exit_unimplemented(
    const char *file, int line, const char *func, const char *format, ...)
{
  va_list args;
  va_start(args, format);

  fprintf(stderr, "%s:%d:%s: Unimplemented: ", file, line, func);
  vfprintf(stderr, format, args);
  fputc('\n', stderr);

  exit_with_code(EXIT_CODE_UNIMPLEMENTED);
}

NORETURN void exit_internal_compiler_error(
    const char *file, int line, const char *func, const char *format, ...)
{
  va_list args;
  va_start(args, format);

  fprintf(stderr, "%s:%d:%s: Internal compiler error: ", file, line, func);
  vfprintf(stderr, format, args);
  fputc('\n', stderr);

  exit_with_code(EXIT_CODE_INTERNAL_COMPILER_ERROR);
}