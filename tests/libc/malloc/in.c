#include <assert.h>
#include <stdlib.h>

int main()
{
  int *x = malloc(sizeof *x);
  assert(x != NULL);
  *x = 3;
  int *y = malloc(sizeof *y);
  assert(y != NULL);
  *y = 4;

  assert(*x == 3);
  assert(*y == 4);

  free(x);
  assert(*y == 4);

  int *z = malloc(sizeof *z);
  // Because of how our malloc implementation works, we should always reuse
  // the same memory location here.
  assert(z == x);

  free(z);
  free(y);

  return 0;
}
