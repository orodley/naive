#ifndef NAIVE_MACROS_H_
#define NAIVE_MACROS_H_

#include "exit_code.h"

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