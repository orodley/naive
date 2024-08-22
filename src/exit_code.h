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

NORETURN void exit_unimplemented(
    const char *file, int line, const char *func, const char *format, ...);

NORETURN void exit_internal_compiler_error(
    const char *file, int line, const char *func, const char *format, ...);

#endif