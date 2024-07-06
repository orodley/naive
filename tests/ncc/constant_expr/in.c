#include <assert.h>

int main()
{
  char foo[(1 + sizeof "foo") / 2];
  assert(sizeof foo == 2);

  return 0;
}
