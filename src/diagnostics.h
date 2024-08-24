#ifndef NAIVE_DIAGNOSTICS_H_
#define NAIVE_DIAGNOSTICS_H_

#include "macros.h"
#include "strings.h"
#include "types.h"

typedef struct SourceLoc
{
  String filename;
  u32 offset;
} SourceLoc;

typedef enum ErrorLevel
{
  WARNING,
  ERROR,
} ErrorLevel;

void emit_diagnostic(ErrorLevel err_level, SourceLoc context, char *fmt, ...);

NORETURN void emit_fatal_error(SourceLoc context, char *fmt, ...);
// @TODO: Propagate SourceLoc to everywhere that calls this so we can use
// emit_fatal_error instead.
NORETURN void emit_fatal_error_no_loc(char *fmt, ...);
void emit_error(SourceLoc context, char *fmt, ...);
void emit_warning(SourceLoc context, char *fmt, ...);

#endif
