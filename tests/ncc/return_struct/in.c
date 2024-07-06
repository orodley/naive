#include <assert.h>

struct Foo
{
  int a, b, c, d;
};

struct Foo foo() { return (struct Foo){1, 2, 3, 4}; }

int main()
{
  struct Foo f = foo();
  assert(f.a == 1);
  assert(f.b == 2);
  assert(f.c == 3);
  assert(f.d == 4);

  return 0;
}
