#include <assert.h>

int foo(int a, int b)
{
  assert(a == 3);
  assert(b == 5);

  return 9;
}

int main()
{
  int (*x)(int, int) = foo;
  assert(x(3, 5) == 9);

  return 0;
}
