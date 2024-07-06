#ifndef NAIVE_DIAGNOSTICS_H_
#define NAIVE_DIAGNOSTICS_H_

#include "misc.h"

typedef struct SourceLoc
{
  char *filename;
  u32 line;
  u32 column;
} SourceLoc;

typedef enum ErrorLevel
{
  WARNING,
  ERROR,
} ErrorLevel;

void issue_diagnostic(ErrorLevel err_level, SourceLoc *context, char *fmt, ...);
void issue_error(SourceLoc *context, char *fmt, ...);
void issue_warning(SourceLoc *context, char *fmt, ...);

#endif
