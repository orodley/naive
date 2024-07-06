#include <assert.h>

int main()
{
  int foo[2][4];
  assert(sizeof foo == 8 * sizeof(int));
  assert(sizeof foo[0] == 4 * sizeof(int));

  struct
  {
    int a[2][2];
    int b;
  } bar;

  bar.b = 1;
  bar.a[1][0] = 2;
  assert(bar.b == 1);
  assert(bar.a[1][0] == 2);

  return 0;
}
