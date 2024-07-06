#include <assert.h>
#include <stdbool.h>

int main()
{
  bool t = true;
  assert(!(!t && !t));

  return 0;
}
