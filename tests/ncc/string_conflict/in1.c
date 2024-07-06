// Manipulate the temp string index to be the same across the two TUs.
#include <assert.h>

int main();

char foo()
{
  char *x = " ";
  return x[0];
}
