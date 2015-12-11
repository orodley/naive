#ifndef NAIVE_DIAGNOSTICS_H_
#define NAIVE_DIAGNOSTICS_H_

#include "tokenise.h"

typedef enum ErrorLevel
{
	WARNING,
	ERROR,
} ErrorLevel;

void issue_diagnostic(ErrorLevel err_level, SourceLoc *context, const char *fmt, ...);
void issue_error(SourceLoc *context, const char *fmt, ...);
void issue_warning(SourceLoc *context, const char *fmt, ...);

#endif
