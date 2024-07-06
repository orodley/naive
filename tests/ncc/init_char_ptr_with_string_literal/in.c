#include <assert.h>

struct Foo
{
  char *a;
};

int main()
{
  struct Foo foo = {"foo"};
  assert(foo.a[0] == 'f');
  assert(foo.a[1] == 'o');
  assert(foo.a[2] == 'o');

  return 0;
}
