#include <assert.h>

int x;

int main()
{
  assert(x == 0);
  x = 1;
  assert(x == 1);

  return 0;
}
