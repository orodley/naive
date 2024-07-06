#include <assert.h>

struct Foo
{
  int a, b, c, d, e;
};

int main()
{
  struct Foo foo;
  foo.a = 235;
  foo.b = 2462;
  foo.c = 12736;
  foo.d = 1276;
  foo.e = 9754;

  struct Foo bar;
  bar = foo;

  assert(bar.a == 235);
  assert(bar.b == 2462);
  assert(bar.c == 12736);
  assert(bar.d == 1276);
  assert(bar.e == 9754);

  return 0;
}
