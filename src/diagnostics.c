#include <stdarg.h>
#include <stdio.h>
#include "diagnostics.h"

static void v_issue_diagnostic(ErrorLevel err_level,
    SourceLoc *context, char *fmt, va_list varargs)
{
  fprintf(stderr, "%s:%d:%d: ", context->filename, context->line, context->column);
  switch (err_level) {
  case WARNING:
    fputs("Warning: ", stderr);
    break;
  case ERROR:
    fputs("Error: ", stderr);
    break;
  }

  vfprintf(stderr, fmt, varargs);
  putc('\n', stderr);
}

void issue_diagnostic(ErrorLevel err_level, SourceLoc *context,
    char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  v_issue_diagnostic(err_level, context, fmt, varargs);
  va_end(varargs);
}

void issue_error(SourceLoc *context, char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  v_issue_diagnostic(ERROR, context, fmt, varargs);
  va_end(varargs);
}

void issue_warning(SourceLoc *context, char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  v_issue_diagnostic(WARNING, context, fmt, varargs);
  va_end(varargs);
}
