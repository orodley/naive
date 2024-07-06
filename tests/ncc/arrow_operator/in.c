#include <assert.h>

struct Foo
{
  int a;
};

int main()
{
  struct Foo foo;
  struct Foo *f = &foo;

  f->a = 3;
  assert(f->a == 3);

  return 0;
}
