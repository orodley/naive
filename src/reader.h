#ifndef NAIVE_READER_H_
#define NAIVE_READER_H_

#include <stdio.h>

#include "array.h"
#include "diagnostics.h"

// @TODO: This and "Symbol" could be unified into something like "String".
// If we do that it's probably worth using this much more widely, in places
// where we currently use a bare "char *".
typedef struct InputBuffer
{
	char *chars;
	u32 length;
} InputBuffer;

#define INVALID_INPUT_BUFFER ((InputBuffer) { NULL, -1 })

inline bool is_valid(InputBuffer ib)
{
	return !((ib.chars == INVALID_INPUT_BUFFER.chars) &&
		(ib.length == INVALID_INPUT_BUFFER.length));
}

typedef enum AdjustmentType
{
	NORMAL_ADJUSTMENT,
	BEGIN_MACRO_ADJUSTMENT,
	END_MACRO_ADJUSTMENT,
} AdjustmentType;

typedef struct Adjustment
{
	AdjustmentType type;
	u32 location;
	SourceLoc new_source_loc;
} Adjustment;

typedef struct Reader
{
	InputBuffer buffer;
	u32 position;

	Array(Adjustment) adjustments;
	u32 next_adjustment;

	bool at_start_of_line;

	SourceLoc source_loc;
	SourceLoc prev_char_source_loc;
} Reader;

void reader_init(Reader *reader, InputBuffer buffer,
		Array(Adjustment) adjustments, bool at_start_of_line,
		char *source_filename);
void back_up(Reader *reader);
void advance(Reader *reader);

typedef struct Symbol
{
	char *str;
	u32 length;
} Symbol;

Symbol read_symbol(Reader *reader);

inline bool at_end(Reader *reader)
{
	return reader->position >= reader->buffer.length;
}

inline char peek_char(Reader *reader)
{
	return at_end(reader) ? EOF : reader->buffer.chars[reader->position];
}

inline char read_char(Reader *reader)
{
	char c = peek_char(reader);
	advance(reader);

	return c;
}

inline bool initial_ident_char(char c)
{
	return  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_');
}

inline bool ident_char(char c)
{
	return initial_ident_char(c) || (c >= '0' && c <= '9');
}

#endif
