#include <assert.h>

struct Foo
{
  int a;
  char b;
};

int main()
{
  assert(sizeof(struct Foo) == 8);
  return 0;
}
