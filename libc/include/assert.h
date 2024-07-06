#ifndef _ASSERT_H
#define _ASSERT_H

#define assert(x) \
  if (!(x)) __assert_fail(__FILE__, __LINE__)

void __assert_fail(const char *file, int line);

#endif
