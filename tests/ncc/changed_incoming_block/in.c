#include <assert.h>

int main()
{
  int a = 1, b = 1, c = 1, d = 1;
  assert((a || b) || (c || d));
  assert((a && b) && (c && d));
  assert((a ? b : c) ? (b ? c : d) : (c ? d : a));

  return 0;
}
