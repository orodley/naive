#include <assert.h>

int main()
{
  int a = 1;
  short b = 2;
  char c = 3;
  assert((a ? b : c) == b);

  return 0;
}
