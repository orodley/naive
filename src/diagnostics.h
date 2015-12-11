#ifndef NAIVE_DIAGNOSTICS_H_
#define NAIVE_DIAGNOSTICS_H_

typedef enum ErrorLevel
{
	WARNING,
	ERROR,
} ErrorLevel;

void issue_diagnostic(ErrorLevel err_level, const char *fmt, ...);
void issue_error(const char *fmt, ...);
void issue_warning(const char *fmt, ...);

#endif
