#include <assert.h>

int main()
{
  int a = 0, b = 0;

  assert((0 ? (a = 23) : (b = 34)) == 34);
  assert(a == 0);
  assert(b == 34);

  assert((1 ? (a = 23) : (b = 56)) == 23);
  assert(a == 23);
  assert(b == 34);

  return 0;
}
