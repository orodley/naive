#include <assert.h>

int main()
{
  int bar[5];
  assert(sizeof *bar == sizeof(int));

  return 0;
}
