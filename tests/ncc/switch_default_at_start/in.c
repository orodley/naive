#include <assert.h>

int main()
{
  int x = 2;
  switch (x) {
  default:
    assert(0);
    break;
  case 2:
    break;
  }

  return 0;
}
