#include <assert.h>

int main()
{
  int a = 0, b = 1;
  switch (b) {
  case 0:
    assert(0);
  case 1:
    a++;
  case 2:
    a++;
    break;
  case 3:
    assert(0);
  default:
    assert(0);
  }

  assert(a == 2);
  a = 0;
  switch (b) {
  case 0:
    assert(0);
  case 2:
    assert(0);
  default:
    a = 1;
  }
  assert(a == 1);

  return 0;
}
