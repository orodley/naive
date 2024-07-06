#include <assert.h>

struct Foo
{
  struct
  {
    int a, b;
  } foo;
};

int main()
{
  struct
  {
    int a, b;
  } foo;

  foo.a = 1;
  foo.b = 2;

  assert(foo.a == 1);
  assert(foo.b == 2);

  struct Foo f;
  f.foo.a = 1;
  f.foo.b = 2;

  assert(f.foo.a == 1);
  assert(f.foo.b == 2);

  return 0;
}
