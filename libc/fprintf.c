#include <stdarg.h>
#include <stdio.h>

#include "file_sink.h"
#include "printf_impl.h"

int fprintf(FILE *stream, const char *format, ...)
{
	va_list ap;
	va_start(ap, format);

	int ret = printf_impl(file_sink, stream, format, ap);

	va_end(ap);
	return ret;
}
