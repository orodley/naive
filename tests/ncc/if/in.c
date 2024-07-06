#include <assert.h>

int foo(int a)
{
  if (a) {
    return 3;
  }

  return 1;
}

int main()
{
  assert(foo(0) == 1);
  assert(foo(1) == 3);

  return 0;
}
