#include <assert.h>
#include <stddef.h>

int main()
{
  unsigned x = 3;
  signed y = 4;

  assert((0 ? x : y) == 4);
  assert((1 ? x : y) == 3);
  assert((0 ? &x : NULL) == NULL);

  return 0;
}
