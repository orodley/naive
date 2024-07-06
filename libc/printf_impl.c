#include "printf_impl.h"

#include <stdlib.h>

static char digits[] = "0123456789abcdef";

int print_unsigned(Sink *sink, void *sink_arg, unsigned long x, int radix)
{
  if (x == 0) {
    int ret = sink(sink_arg, '0');
    return ret != 0 ? -1 : 1;
  }

  unsigned long divisor = 1;
  unsigned long x0 = x / radix;
  while (x0 != 0) {
    divisor *= radix;
    x0 /= radix;
  }

  int chars_printed = 0;
  while (divisor != 0) {
    char c = digits[(x / divisor) % radix];
    int ret = sink(sink_arg, c);
    if (ret != 0) return ret;

    chars_printed++;
    divisor /= radix;
  }

  return chars_printed;
}

int print_integer(Sink *sink, void *sink_arg, long x, int radix)
{
  int chars_printed = 0;
  if (x < 0) {
    x = -x;
    int ret = sink(sink_arg, '-');
    if (ret != 0) return -1;

    chars_printed++;
  }

  return chars_printed + print_unsigned(sink, sink_arg, x, radix);
}

int printf_impl(Sink *sink, void *sink_arg, const char *format, va_list ap)
{
  int chars_printed = 0;

  for (int i = 0; format[i] != '\0'; i++) {
    char c = format[i];
    if (c != '%') {
      int ret = sink(sink_arg, c);
      if (ret != 0) return -1;

      chars_printed++;
    } else {
      i++;
      if (format[i] == '\0') return -1;

      switch (format[i]) {
      case 'c': {
        unsigned char c = (unsigned char)va_arg(ap, int);
        int ret = sink(sink_arg, c);
        if (ret != 0) return -1;

        chars_printed++;
        break;
      }
      case 'd': {
        int x = va_arg(ap, int);
        int ret = print_integer(sink, sink_arg, x, 10);
        if (ret < 0) return ret;

        chars_printed += ret;
        break;
      }
      case 'u': {
        unsigned x = va_arg(ap, unsigned);
        int ret = print_unsigned(sink, sink_arg, x, 10);
        if (ret < 0) return ret;

        chars_printed += ret;
        break;
      }
      case 'x': {
        unsigned x = va_arg(ap, unsigned);
        int ret = print_unsigned(sink, sink_arg, x, 16);
        if (ret < 0) return ret;

        chars_printed += ret;
        break;
      }
      case 's': {
        char *str = va_arg(ap, char *);
        for (int i = 0; str[i] != '\0'; i++) {
          int ret = sink(sink_arg, str[i]);
          if (ret != 0) return -1;

          chars_printed++;
        }
        break;
      }
      case 'l': {
        i++;
        if (format[i] == '\0') return -1;

        switch (format[i]) {
        case 'u': {
          unsigned long x = va_arg(ap, unsigned long);
          int ret = print_unsigned(sink, sink_arg, x, 10);
          if (ret < 0) return ret;

          chars_printed += ret;
          break;
        }
        case 'd': {
          long x = va_arg(ap, long);
          int ret = print_integer(sink, sink_arg, x, 10);
          if (ret < 0) return ret;

          chars_printed += ret;
          break;
        }
        default:
          // Unimplemented
          abort();
        }
        break;
      }
      }
    }
  }

  return chars_printed;
}
