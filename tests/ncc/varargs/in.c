#include <assert.h>
#include <stdarg.h>

void foo(int a, int b, ...)
{
  va_list args;
  va_start(args, b);

  assert(a == 0);
  assert(b == 1);
  assert(va_arg(args, int) == 2);
  assert(va_arg(args, const int) == 3);
  assert(va_arg(args, int) == 4);
  assert(va_arg(args, int) == 5);
  assert(va_arg(args, int) == 6);
  assert(va_arg(args, int) == 7);

  va_end(args);
}

int main()
{
  // We pass 8 args so that we test va_arg more thoroughly. We test that:
  // * Args are fetched from the register save area
  // * The gp_offset is adjusted correctly
  // * We correctly detect when we are out of space in the register save
  //   area, even when there are only 4 args in there.
  // * We read stack args correctly
  // * We adjust the stack arg pointer correctly to fetch the second stack
  //   arg
  foo(0, 1, 2, 3, 4, 5, 6, 7);
  return 0;
}
