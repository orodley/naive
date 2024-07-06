#include <assert.h>

enum Foo
{
  A,
  B = 5,
  C,
  D,
  E = 6,
  F,
};

int main()
{
  assert(A == 0);
  assert(B == 5);
  assert(C == 6);
  assert(D == 7);
  assert(E == 6);
  assert(F == 7);

  return 0;
}
