#include <assert.h>

int main()
{
  int i = 0;
  for (; i != 100; i = i + 1) {
    if (i == 7)
      break;
  }
  assert(i == 7);

  i = 0;
  while (1) {
    if (i == 10)
      break;
    i = i + 1;
  }
  assert(i == 10);

  return 0;
}
