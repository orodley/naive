#include <assert.h>

struct Foo
{
  int a, b, c, d;
};

void foo(struct Foo f)
{
  assert(f.a == 0);
  assert(f.b == 1);
  assert(f.c == 2);
  assert(f.d == 3);
}

int main()
{
  foo((struct Foo){0, 1, 2, 3});
  return 0;
}
