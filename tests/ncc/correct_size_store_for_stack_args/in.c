#include <assert.h>

void foo(int a, int b, int c, int d, int e, int f, int g)
{
  assert(g == -1);
}

int main()
{
  foo(0, 0, 0, 0, 0, 0, -1);

  return 0;
}
