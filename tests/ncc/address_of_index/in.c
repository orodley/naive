#include <assert.h>

int main()
{
  int x[3];
  int *p = &x[1];
  x[0] = 1;
  *p = 2;
  x[2] = 3;
  assert(x[0] == 1);
  assert(x[1] == 2);
  assert(x[2] == 3);

  return 0;
}
