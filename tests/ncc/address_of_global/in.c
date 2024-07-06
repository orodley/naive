#include <assert.h>

int foo(int *x)
{
  *x = *x + 3;
  return *x;
}

int a;

int main()
{
  int *x = &a;
  foo(x);
  assert(a == 3);

  foo(&a);
  assert(a == 6);

  return 0;
}
