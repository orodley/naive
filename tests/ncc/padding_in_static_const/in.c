#include <assert.h>

static struct { int a; char *b; } foo = {0, "a" };

typedef unsigned u32;

int main()
{
  assert(foo.b[0] == 'a');

  return 0;
}
