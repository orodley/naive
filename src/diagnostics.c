#include "diagnostics.h"

#include <stdarg.h>
#include <stdio.h>

#include "assertions.h"
#include "file.h"
#include "macros.h"

static bool color_diagnostics = false;

#define ESCAPE "\x1b"
#define CSI ESCAPE "["
#define SGR(code) CSI code "m"
#define COLOR(code) (color_diagnostics ? SGR(code) : "")
#define BOLD_COLOR(code) (color_diagnostics ? SGR(code) SGR(BOLD) : "")

#define RESET "0"
#define BOLD "1"
#define RED "31"
#define GREEN "32"
#define YELLOW "33"
#define BLUE "34"
#define MAGENTA "35"
#define CYAN "36"
#define WHITE "37"

static void v_emit_diagnostic(
    ErrorLevel err_level, SourceRange context, char *fmt, va_list varargs)
{
  ASSERT(context.start.offset <= context.end.offset);

  u32 start_of_start_line = 0, start_of_end_line = 0, start_of_current_line = 0;
  u32 curr_line = 1, start_line = 1, end_line = 1;
  u32 start_column, end_column, curr_column = 1;

  if (!string_eq(context.start.filename, context.end.filename)) {
    UNIMPLEMENTED("Diagnostic spans multiple files");
  }

  String filename = context.start.filename;

  // @LEAK
  String source_file = map_file_into_memory(string_to_c_string(filename));
  if (!is_valid(source_file)) {
    start_column = context.start.offset + 1;
    end_column = context.end.offset + 1;
  } else {
    ASSERT(context.end.offset < source_file.len);
    for (u32 i = 0; i != context.end.offset;) {
      i++;
      if (source_file.chars[i] == '\n') {
        curr_line++;
        curr_column = 0;
        start_of_current_line = i + 1;
      } else {
        curr_column++;
      }

      if (i == context.start.offset) {
        start_of_start_line = start_of_current_line;
        start_line = curr_line;
        start_column = curr_column;
      }
    }

    start_of_end_line = start_of_current_line;
    end_line = curr_line;
    end_column = curr_column;
  }

  // @TODO: Use the end line/column for richer context printing.
  // Some initial ideas:
  // * If the end line is different but not too much larger (some threshold)
  //   print all the lines in the range, with an indicator above the first and
  //   below the last to show the range.
  // * If the end line is too far away, do the same but put some "X lines
  //   omitted" indicator in the middle
  IGNORE(start_of_end_line);

  fprintf(
      stderr, "%.*s:%u:%u: ", filename.len, filename.chars, start_line,
      start_column);

  switch (err_level) {
  case WARNING:
    fprintf(stderr, "%sWarning%s: ", BOLD_COLOR(YELLOW), COLOR(RESET));
    break;
  case ERROR:
    fprintf(stderr, "%sError%s: ", BOLD_COLOR(RED), COLOR(RESET));
    break;
  }

  vfprintf(stderr, fmt, varargs);
  putc('\n', stderr);

  if (start_line == end_line) {
    for (u32 i = start_of_start_line; i < source_file.len; i++) {
      if (source_file.chars[i] == '\n') {
        break;
      }

      putc(source_file.chars[i], stderr);
    }
    putc('\n', stderr);

    for (u32 i = 1; i <= end_column; i++) {
      if (i < start_column) {
        putc(' ', stderr);
      } else {
        char c;
        if (i == start_column) {
          c = '^';
        } else if (i == end_column) {
          c = '|';
        } else {
          c = '-';
        }
        fprintf(stderr, "%s%c%s", BOLD_COLOR(RED), c, COLOR(RESET));
      }
    }
    putc('\n', stderr);
  }
}

void emit_diagnostic(ErrorLevel err_level, SourceRange context, char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  v_emit_diagnostic(err_level, context, fmt, varargs);
  va_end(varargs);
}

NORETURN void emit_fatal_error(SourceRange context, char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  v_emit_diagnostic(ERROR, context, fmt, varargs);
  va_end(varargs);

  exit_with_code(EXIT_CODE_INTERNAL_COMPILER_ERROR);
}

NORETURN void emit_fatal_error_no_loc(char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  v_emit_diagnostic(ERROR, UNKNOWN_RANGE, fmt, varargs);
  va_end(varargs);

  exit_with_code(EXIT_CODE_INTERNAL_COMPILER_ERROR);
}

void emit_error(SourceRange context, char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  v_emit_diagnostic(ERROR, context, fmt, varargs);
  va_end(varargs);
}

void emit_warning(SourceRange context, char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);
  v_emit_diagnostic(WARNING, context, fmt, varargs);
  va_end(varargs);
}

SourceRange point_range(SourceLoc loc) { return (SourceRange){loc, loc}; }

void enable_color_diagnostics(bool enable) { color_diagnostics = enable; }