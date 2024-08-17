#include <math.h>
#include <stdint.h>

double ldexp(double arg, int exp)
{
  union Convert
  {
    double d;
    uint64_t x;
  };

  union Convert u = {.d = arg};
  uint64_t bits = u.x;
  uint64_t exponent = (bits >> 52) & 0x7ff;
  exponent += exp;
  if (exponent >= 0x7ff) {
    return HUGE_VAL;
  }

  bits &= ~(0x7ffull << 52);
  bits |= exponent << 52;

  union Convert result = {.x = bits};
  return result.d;
}