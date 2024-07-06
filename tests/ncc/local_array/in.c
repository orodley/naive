#include <assert.h>

int main()
{
  int x[6];
  *x = 3;
  assert(*x == 3);

  return 0;
}
