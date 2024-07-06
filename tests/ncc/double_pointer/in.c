#include <assert.h>

int foo(int **b) { return **b; }

int main()
{
  int a = 3;
  int *b = &a;
  assert(foo(&b) == 3);

  return 0;
}
