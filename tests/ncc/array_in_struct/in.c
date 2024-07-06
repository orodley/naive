#include <assert.h>

struct Foo
{
  int a;
  int b[3];
  int c;
};

int main()
{
  struct Foo foo;
  foo.a = 1;
  *foo.b = 2;
  foo.c = 3;

  assert(foo.a == 1);
  assert(*foo.b == 2);
  assert(foo.c == 3);

  return 0;
}
