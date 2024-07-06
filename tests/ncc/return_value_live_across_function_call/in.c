#include <assert.h>

int foo()
{
  return 1;
}

int main()
{
  switch (foo()) {
  case 1:
    assert(foo() == 1);
    break;
  case 2:
    assert(foo() == 2);
    break;
  }

  return 0;
}
