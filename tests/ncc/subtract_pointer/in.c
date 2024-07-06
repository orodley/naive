#include <assert.h>

int main()
{
  int x;
  int *q = &x + 2;
  int *r = &x + 4;
  assert(r - q == 2);
  assert(r - 4 == &x);

  int a[5];
  int *b = a + 3;
  assert(b - a == 3);
  assert(b - 3 == a);

  return 0;
}
