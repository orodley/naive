#include <stdarg.h>
#include <stdio.h>
#include "diagnostics.h"

static void v_issue_diagnostic(ErrorLevel err_level,
		SourceLoc *context, const char *fmt, va_list varargs)
{
	printf("%s:%d:%d: ", context->filename, context->line, context->column);
	switch (err_level) {
	case WARNING:
		fputs("Warning: ", stdout);
		break;
	case ERROR:
		fputs("Error: ", stdout);
		break;
	}

	vprintf(fmt, varargs);
	putchar('\n');
}

void issue_diagnostic(ErrorLevel err_level, SourceLoc *context,
		const char *fmt, ...)
{
	va_list varargs;
	va_start(varargs, fmt);
	v_issue_diagnostic(err_level, context, fmt, varargs);
	va_end(varargs);
}

void issue_error(SourceLoc *context, const char *fmt, ...)
{
	va_list varargs;
	va_start(varargs, fmt);
	v_issue_diagnostic(ERROR, context, fmt, varargs);
	va_end(varargs);
}

void issue_warning(SourceLoc *context, const char *fmt, ...)
{
	va_list varargs;
	va_start(varargs, fmt);
	v_issue_diagnostic(WARNING, context, fmt, varargs);
	va_end(varargs);
}
