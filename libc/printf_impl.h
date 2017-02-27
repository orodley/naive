#ifndef _PRINTF_IMPL_H
#define _PRINTF_IMPL_H

#include <stdarg.h>

typedef int Sink(void *sink_arg, char c);

int printf_impl(Sink *sink, void *sink_arg, const char *format, va_list ap);

#endif
