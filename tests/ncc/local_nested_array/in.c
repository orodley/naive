#include <assert.h>

int main()
{
  int foo[][2] = {{1, 2}, {3, 4}};
  assert(foo[0][0] == 1);
  assert(foo[0][1] == 2);
  assert(foo[1][0] == 3);
  assert(foo[1][1] == 4);

  return 0;
}
