#include <stdarg.h>
#include <stdio.h>

#include "file_sink.h"
#include "printf_impl.h"

int vprintf(const char *format, va_list ap)
{
	return printf_impl(file_sink, stdout, format, ap);
}
