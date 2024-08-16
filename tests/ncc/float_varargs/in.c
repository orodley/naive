#include <stdarg.h>

int add(int dummy, ...)
{
  va_list ap;
  va_start(ap, dummy);

  double x = va_arg(ap, double);
  float y = va_arg(ap, float);

  return (int)x + (int)y;
}

int main(void) { return add(0, 2.0, 2.0f) - 4; }
