#include <stdarg.h>
#include <stdio.h>

#include "file_sink.h"
#include "printf_impl.h"

int printf(const char *format, ...)
{
	va_list ap;
	va_start(ap, format);

	int ret = printf_impl(file_sink, stdout, format, ap);

	va_end(ap);
	return ret;
}
