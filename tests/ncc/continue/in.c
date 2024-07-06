#include <assert.h>

int main()
{
  int i = 0;
  int j = 0;
  for (; i != 6; i = i * 2) {
    j = 1;
    if (i == 0) {
      i = 3;
      continue;
    }
    j = 2;
  }
  assert(i == 6);
  assert(j == 1);

  i = 0;
  j = 0;
  while (i != 6) {
    j = 1;
    if (i == 0)  {
      i = 6;
      continue;
    }
    j = 2;
  }
  assert(i == 6);
  assert(j == 1);

  return 0;
}
