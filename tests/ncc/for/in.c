#include <assert.h>

int main()
{
  int x = 0;
  for (int i = 0; i != 10; i = i + 1) {
    x = x + i;
  }

  assert(x == 45);

  return 0;
}
