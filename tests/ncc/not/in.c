#include <assert.h>

int main()
{
  int x = 0;
  int y = 234;
  assert(!x);
  assert(!!y);

  return 0;
}
