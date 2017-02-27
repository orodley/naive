#include <stdarg.h>
#include <stdio.h>

#include "file_sink.h"
#include "printf_impl.h"

int vfprintf(FILE *stream, const char *format, va_list ap)
{
	return printf_impl(file_sink, stream, format, ap);
}
