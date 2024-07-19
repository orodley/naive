#include <assert.h>

int main()
{
  int i = 0;
  for (; i != 10;) i = i + 1;
  assert(i == 10);

  int n = 0;
  for (;;) {
    if (n > 20) break;
    n++;
  }
  assert(n == 21);

  return 0;
}
