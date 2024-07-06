#include <assert.h>

int main()
{
  int a = 1;
  int b = 0;

  assert((a = 2) || (b = 1));
  assert(a == 2);
  assert(b == 0);

  assert((a = 0) || (b = 1));
  assert(a == 0);
  assert(b == 1);

  assert((a = 1) || (b = 0));
  assert(a == 1);
  assert(b == 1);

  assert(!((a = 0) || (b = 0)));
  assert(a == 0);
  assert(b == 0);

  a = 1;
  b = 0;

  assert((a = 2) && (b = 1));
  assert(a == 2);
  assert(b == 1);

  assert(!((a = 0) && (b = 2)));
  assert(a == 0);
  assert(b == 1);

  assert(!((a = 2) && (b = 0)));
  assert(a == 2);
  assert(b == 0);

  b = 1;
  assert(!((a = 0) && (b = 0)));
  assert(a == 0);
  assert(b == 1);

  return 0;
}
