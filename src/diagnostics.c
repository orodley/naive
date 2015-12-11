#include <stdarg.h>
#include <stdio.h>
#include "diagnostics.h"

// TODO: We should pass some context for this so we can print line numbers and
// stuff.

static void v_issue_diagnostic(ErrorLevel err_level,
		const char *fmt, va_list varargs)
{
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

void issue_diagnostic(ErrorLevel err_level, const char *fmt, ...)
{
	va_list varargs;
	va_start(varargs, fmt);
	v_issue_diagnostic(err_level, fmt, varargs);
	va_end(varargs);
}

void issue_error(const char *fmt, ...)
{
	va_list varargs;
	va_start(varargs, fmt);
	v_issue_diagnostic(ERROR, fmt, varargs);
	va_end(varargs);
}

void issue_warning(const char *fmt, ...)
{
	va_list varargs;
	va_start(varargs, fmt);
	v_issue_diagnostic(WARNING, fmt, varargs);
	va_end(varargs);
}
