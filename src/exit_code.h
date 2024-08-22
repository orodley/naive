#ifndef NAIVE_EXIT_CODE_H_
#define NAIVE_EXIT_CODE_H_

#include <stdlib.h>

#include "macros.h"

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

// Assertions, etc.

#define ASSERT_FAIL(...) \
  exit_internal_compiler_error(__FILE__, __LINE__, __func__, __VA_ARGS__)
#define ASSERT_IMPL(expr, prefix, ...)            \
  do {                                            \
    if (!(expr)) {                                \
      ASSERT_FAIL(prefix #expr ": " __VA_ARGS__); \
    }                                             \
  } while (0)

#define ASSERT(expr, ...) ASSERT_IMPL(expr, "Failed assertion: ", __VA_ARGS__)
#define PRECONDITION(expr, ...) \
  ASSERT_IMPL(expr, "Failed precondition: ", __VA_ARGS__)
#define POSTCONDITION(expr, ...) \
  ASSERT_IMPL(expr, "Failed postcondition: ", __VA_ARGS__)
#define UNREACHABLE ASSERT_FAIL("This should never be reached")

#define UNIMPLEMENTED(...) \
  exit_unimplemented(__FILE__, __LINE__, __func__, __VA_ARGS__)

#endif