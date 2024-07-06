#include <assert.h>

char *foo[][2] = {{"a", "b"}, {"c", "d"}};

int main()
{
  assert(foo[0][0][0] == 'a');
  assert(foo[0][0][1] == '\0');
  assert(foo[0][1][0] == 'b');
  assert(foo[0][1][1] == '\0');
  assert(foo[1][0][0] == 'c');
  assert(foo[1][0][1] == '\0');
  assert(foo[1][1][0] == 'd');
  assert(foo[1][1][1] == '\0');

  return 0;
}
