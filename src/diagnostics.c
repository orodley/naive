#include "diagnostics.h"

#include <stdarg.h>
#include <stdio.h>

#include "assertions.h"
#include "file.h"
#include "macros.h"

static void v_issue_diagnostic(
    ErrorLevel err_level, SourceLoc context, char *fmt, va_list varargs)
{
  u32 line = 1;
  u32 column = 1;
  // @LEAK
  String source_file =
      map_file_into_memory(string_to_c_string(context.filename));
  if (!is_valid(source_file)) {
    column = context.offset + 1;
  } else {
    ASSERT(context.offset < source_file.len);
    for (u32 i = 0; i != context.offset;) {
      i++;
      if (source_file.chars[i] == '\n') {
        line++;
        column = 0;
      } else {
        column++;
      }
    }
  }

  fprintf(
      stderr, "%.*s:%u:%u: ", context.filename.len, context.filename.chars,
      line, column);
  switch (err_level) {
  case WARNING: fputs("Warning: ", stderr); break;
  case ERROR: fputs("Error: ", stderr); break;
  }

  vfprintf(stderr, fmt, varargs);
  putc('\n', stderr);
}

void emit_diagnostic(ErrorLevel err_level, SourceLoc context, char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  v_issue_diagnostic(err_level, context, fmt, varargs);
  va_end(varargs);
}

NORETURN void emit_fatal_error(SourceLoc context, char *fmt, ...)
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
  SourceLoc context = (SourceLoc){STRING("unknown"), 0};
  v_issue_diagnostic(ERROR, context, fmt, varargs);
  va_end(varargs);

  exit_with_code(EXIT_CODE_INTERNAL_COMPILER_ERROR);
}

void emit_error(SourceLoc context, char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  v_issue_diagnostic(ERROR, context, fmt, varargs);
  va_end(varargs);
}

void emit_warning(SourceLoc context, char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  v_issue_diagnostic(WARNING, context, fmt, varargs);
  va_end(varargs);
}
