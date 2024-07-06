#include <assert.h>

void foo(int *x[]) { **x = 3; }

int main()
{
  int x;
  int *y = &x;
  foo(&y);
  assert(x == 3);

  return 0;
}
