#ifndef _STRING_SINK_H
#define _STRING_SINK_H

#include <stdbool.h>
#include <stddef.h>

typedef struct StringSinkArg
{
	char *string;
	size_t index;
	size_t max_chars;
	bool has_limit;
} StringSinkArg;

int string_sink(void *sink_arg, char c);

#endif
