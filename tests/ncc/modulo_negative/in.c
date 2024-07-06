#include <assert.h>

int main()
{
  int n = 5, d = 3;
  assert(n % -d == 2);
  assert(-n % d == -2);

  assert(5 % -3 == 2);
  assert(-5 % 3 == -2);

  return 0;
}
