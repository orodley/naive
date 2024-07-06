#include <assert.h>

int main()
{
  unsigned int x = 0xFFFFFFFF;
  assert(sizeof x == 4);
  assert(x + 1 == 0);

  return 0;
}
