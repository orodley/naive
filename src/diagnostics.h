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

#define UNKNOWN_LOC ((SourceLoc){.filename = STRING("<unknown>"), .offset = 0})

typedef struct SourceRange
{
  SourceLoc start;
  SourceLoc end;  // inclusive
} SourceRange;

#define UNKNOWN_RANGE ((SourceRange){.start = UNKNOWN_LOC, .end = UNKNOWN_LOC})

typedef enum ErrorLevel
{
  WARNING,
  ERROR,
} ErrorLevel;

void emit_diagnostic(ErrorLevel err_level, SourceRange context, char *fmt, ...);

NORETURN void emit_fatal_error(SourceRange context, char *fmt, ...);
// @TODO: Propagate SourceLoc to everywhere that calls this so we can use
// emit_fatal_error instead.
NORETURN void emit_fatal_error_no_loc(char *fmt, ...);
void emit_error(SourceRange context, char *fmt, ...);
void emit_warning(SourceRange context, char *fmt, ...);

SourceRange point_range(SourceLoc loc);

#endif
