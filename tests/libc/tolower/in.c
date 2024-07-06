#include <assert.h>
#include <ctype.h>

int main()
{
  assert(tolower('!') == '!');
  assert(tolower('A') == 'a');
  assert(tolower('M') == 'm');
  assert(tolower('Z') == 'z');
  assert(tolower('a') == 'a');
  assert(tolower('m') == 'm');
  assert(tolower('z') == 'z');

  return 0;
}
