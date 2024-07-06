#include <assert.h>

int foo[5] = { 1, 2, 3, 4, 5 };
struct { int a, b; } bar[2] = {
  {
    .a = 1,
    .b = 2,
  },
  {
    .a = 3,
    .b = 4,
  },
};

int main()
{
  assert(foo[0] == 1);
  assert(foo[1] == 2);
  assert(foo[2] == 3);
  assert(foo[3] == 4);
  assert(foo[4] == 5);

  assert(bar[0].a == 1);
  assert(bar[0].b == 2);
  assert(bar[1].a == 3);
  assert(bar[1].b == 4);

  return 0;
}
