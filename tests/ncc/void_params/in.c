#include <assert.h>

int foo(void);

int foo(void)
{
  return 253;
}

int main()
{
  assert(foo() == 253);
  return 0;
}
