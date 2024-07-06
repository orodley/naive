#include <assert.h>

struct Foo
{
  int a, b, c, d, e, f, g, h, i;
};

void foo(struct Foo f)
{
  assert(f.a == 0);
  assert(f.b == 1);
  assert(f.c == 2);
  assert(f.d == 3);
  assert(f.e == 4);
  assert(f.f == 5);
  assert(f.g == 6);
  assert(f.h == 7);
  assert(f.i == 8);
}

int main()
{
  foo((struct Foo) { 0, 1, 2, 3, 4, 5, 6, 7, 8 });
  return 0;
}
