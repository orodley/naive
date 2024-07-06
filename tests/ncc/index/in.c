#include <assert.h>

int main()
{
  int foo[3];
  foo[0] = 1;
  foo[1] = 2;
  foo[2] = 3;
  assert(foo[0] == 1);
  assert(foo[1] == 2);
  assert(foo[2] == 3);

  int x;
  int *p = &x;
  p[0] = 4;
  assert(p[0] == 4);

  0 [p] = 5;
  assert(p[0] == 5);

  return 0;
}
