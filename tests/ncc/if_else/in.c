#include <assert.h>

int foo(int a)
{
  if (a == 0) {
    return 1;
  } else if (a == 1) {
    return 9;
  } else {
    return 3;
  }
}

int main()
{
  assert(foo(0) == 1);
  assert(foo(1) == 9);
  assert(foo(2) == 3);

  return 0;
}
