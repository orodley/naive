#include <assert.h>

struct Foo
{
  int elems[5];
};

int main()
{
  // @PORT
  assert(sizeof(int) == 4);
  assert(sizeof(char) == 1);
  assert(sizeof(struct Foo) == 20);

  return 0;
}
