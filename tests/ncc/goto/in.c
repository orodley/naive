#include <assert.h>

int main()
{
  int a = 0, b = 0, c = 0, d = 0;

  goto foo;
  assert(0);

  while (a != 1) {
bar:
    b = 1;
  }

  c = 1;
  goto baz;
  assert(0);

foo:
  a = 1;
  goto bar;

  assert(0);

  switch (3) {
  case 3:
baz:
    d = 1;
    break;
  }

  assert(a == 1);
  assert(b == 1);
  assert(c == 1);
  assert(d == 1);

  return 0;
}
