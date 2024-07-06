#include <assert.h>

struct Foo
{
  int a, b;
};

int main()
{
  struct Foo f = { 1, 2 };
  struct Foo *fp = &f;
  struct Foo f2 = *fp;

  assert(f2.a == 1);
  assert(f2.b == 2);

  return 0;
}
