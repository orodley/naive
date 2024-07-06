#include <assert.h>

struct Foo { int a; int b; };

struct Foo foo(int *a, int b)
{
  struct Foo f;
  f.a = *a;
  f.b = b;

  return f;
}

int main()
{
  int a = 1;
  struct Foo f = foo(&a, 2);
  assert(f.a == 1);
  assert(f.b == 2);

  return 0;
}
