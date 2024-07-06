#include <assert.h>

struct Foo
{
  int a, b;
};

int main()
{
  struct Foo foo = { 1, 2 };
  assert(foo.a == 1);
  assert(foo.b == 2);

  struct Foo bar = { 1, .b = 2, .a = 3, 4 };
  assert(bar.a == 3);
  assert(bar.b == 4);

  return 0;
}
