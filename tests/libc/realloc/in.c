#include <assert.h>
#include <stdlib.h>

int main()
{
  int *a = malloc(32);

  free(a);
  int *b = malloc(4);
  assert(a == b);

  *b = 373;

  int *c = realloc(b, 20);
  assert(b == c);
  assert(*c == 373);

  int *d = realloc(c, 40);
  assert(b != d);
  assert(*d == 373);

  free(d);

  int *e = realloc(NULL, sizeof *e);
  *e = 18962;
  assert(*e == 18962);

  return 0;
}
