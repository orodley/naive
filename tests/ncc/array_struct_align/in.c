#include <assert.h>

struct Foo
{
  int a;
  char b;
};

int main()
{
  assert(sizeof(struct Foo) == 8);
  assert(sizeof(struct Foo[2]) == 16);

  return 0;
}
