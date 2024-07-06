#include <assert.h>

char *foo = "foo";
char bar[] = "bar";
struct
{
  char *foo;
} foo_s = {"foo_s"};
struct
{
  char bar[5];
} bar_s = {"bar_s"};
char *baz[] = {"baz"};

int main()
{
  assert(foo[0] == 'f');
  assert(foo[1] == 'o');
  assert(foo[2] == 'o');

  assert(bar[0] == 'b');
  assert(bar[1] == 'a');
  assert(bar[2] == 'r');

  assert(foo_s.foo[0] == 'f');
  assert(foo_s.foo[1] == 'o');
  assert(foo_s.foo[2] == 'o');
  assert(foo_s.foo[3] == '_');
  assert(foo_s.foo[4] == 's');

  assert(bar_s.bar[0] == 'b');
  assert(bar_s.bar[1] == 'a');
  assert(bar_s.bar[2] == 'r');
  assert(bar_s.bar[3] == '_');
  assert(bar_s.bar[4] == 's');

  assert(baz[0][0] == 'b');
  assert(baz[0][1] == 'a');
  assert(baz[0][2] == 'z');

  return 0;
}
