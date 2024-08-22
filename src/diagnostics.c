#include "diagnostics.h"

#include <stdarg.h>
#include <stdio.h>

#include "exit_code.h"
#include "macros.h"

static void v_issue_diagnostic(
    ErrorLevel err_level, SourceLoc *context, char *fmt, va_list varargs)
{
  fprintf(
      stderr, "%s:%d:%d: ", context->filename, context->line, context->column);
  switch (err_level) {
  case WARNING: fputs("Warning: ", stderr); break;
  case ERROR: fputs("Error: ", stderr); break;
  }

  vfprintf(stderr, fmt, varargs);
  putc('\n', stderr);
}

void emit_diagnostic(ErrorLevel err_level, SourceLoc *context, char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  v_issue_diagnostic(err_level, context, fmt, varargs);
  va_end(varargs);
}

NORETURN void emit_fatal_error(SourceLoc *context, char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  v_issue_diagnostic(ERROR, context, fmt, varargs);
  va_end(varargs);

  exit_with_code(EXIT_CODE_INTERNAL_COMPILER_ERROR);
}

NORETURN void emit_fatal_error_no_loc(char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  SourceLoc context = (SourceLoc){"unknown", 0, 0};
  v_issue_diagnostic(ERROR, &context, fmt, varargs);
  va_end(varargs);

  exit_with_code(EXIT_CODE_INTERNAL_COMPILER_ERROR);
}

void emit_error(SourceLoc *context, char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  v_issue_diagnostic(ERROR, context, fmt, varargs);
  va_end(varargs);
}

void emit_warning(SourceLoc *context, char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  v_issue_diagnostic(WARNING, context, fmt, varargs);
  va_end(varargs);
}
