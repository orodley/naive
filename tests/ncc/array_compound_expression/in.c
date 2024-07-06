#include <assert.h>

int main()
{
  int foo[5] = (int[5]){0, 1, 2, 3, 4};
  assert(foo[0] == 0);
  assert(foo[1] == 1);
  assert(foo[2] == 2);
  assert(foo[3] == 3);
  assert(foo[4] == 4);

  int bar[5] = (int[5]){5, [0] = 4, [2] = 3, 6};
  assert(bar[0] == 4);
  assert(bar[1] == 0);
  assert(bar[2] == 3);
  assert(bar[3] == 6);
  assert(bar[4] == 0);

  return 0;
}
