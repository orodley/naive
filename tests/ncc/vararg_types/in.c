#include <assert.h>
#include <stdarg.h>

void foo(int a, ...)
{
  va_list args;
  va_start(args, a);

  assert(va_arg(args, int) == 1);
  assert(va_arg(args, long) == 2);
  assert(*va_arg(args, int *) == 1);
}

int main()
{
  int i = 1;
  long l = 2;
  int *p = &i;
  foo(0, i, l, p);

  return 0;
}
