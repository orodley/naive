#include <assert.h>
#include <string.h>

int main()
{
  assert(strncmp("foo", "foob", 4) < 0);
  assert(strncmp("foob", "foo", 4) > 0);
  assert(strncmp("fop", "foo", 3) > 0);
  assert(strncmp("foo", "fop", 3) < 0);
  assert(strncmp("foo", "foo", 3) == 0);
  assert(strncmp("foobar", "foorab", 3) == 0);

  return 0;
}
