#include <assert.h>

int main()
{
  int x = 1;
  int *p = &x;
  p = p + 0;

  assert(*p == 1);

  return 0;
}
