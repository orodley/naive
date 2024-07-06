#include <assert.h>

int x = 3;
int *y = &x;

int main()
{
  assert(*y == 3);
  *y = 2;
  assert(x == 2);

  return 0;
}
