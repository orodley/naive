#include <assert.h>

int main()
{
  struct Foo
  {
    int a;
  } foo;
  struct Foo bar;

  foo.a = 1;
  bar.a = 2;

  assert(foo.a == 1);
  assert(bar.a == 2);

  return 0;
}
