#include <assert.h>

int main()
{
  int a;
  int *b = &a;
  if (1) *b = 3;

  assert(*b == 3);
  return 0;
}
