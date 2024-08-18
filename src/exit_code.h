#ifndef NAIVE_EXIT_CODE_H_
#define NAIVE_EXIT_CODE_H_

#include <stdlib.h>

#include "misc.h"

typedef enum ExitCode
{
  EXIT_CODE_SUCCESS = 0,
  EXIT_CODE_BAD_CLI = 1,
  EXIT_CODE_IO_ERROR = 2,
  EXIT_CODE_INVALID_SOURCE = 3,
  EXIT_CODE_LINKER_ERROR = 4,
  EXIT_CODE_UNIMPLEMENTED = 5,
  EXIT_CODE_INTERNAL_COMPILER_ERROR = 6,
} ExitCode;

NORETURN inline void exit_with_code(ExitCode code) { exit((int)code); }

#define UNIMPLEMENTED(fmt, ...) \
  _exit_unimplemented(__FILE__, __LINE__, fmt, __VA_ARGS__)
NORETURN void _exit_unimplemented(
    const char *file, int line, const char *format, ...);

#endif