#include <assert.h>

int baz;
int quux;

int *foo()
{
  // Make sure that += doesn't evaluate the lvalue twice.
  assert(baz == 0);
  baz = 1;

  return &quux;
}

int main()
{
  int x = 3;
  assert((x += 3) == 6);
  assert(x == 6);

  assert((x *= 2) == 12);
  assert(x == 12);

  assert((x -= 2) == 10);
  assert(x == 10);

  assert((*foo() += 5) == 5);
  assert(quux == 5);

  return 0;
}
