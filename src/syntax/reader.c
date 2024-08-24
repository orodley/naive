#include "syntax/reader.h"

#include <assert.h>

#include "util.h"

void reader_init(
    Reader *reader, String buffer, Array(Adjustment) adjustments,
    bool at_start_of_line, String source_filename)
{
  *reader = (Reader){
      .filename = source_filename,
      .buffer = buffer,
      .position = 0,
      .adjusted_position = 0,
      .adjustments = adjustments,
      .next_adjustment = 0,
      .at_start_of_line = at_start_of_line,
  };
}

SourceLoc reader_source_loc(Reader *reader)
{
  return (SourceLoc){
      .filename = reader->filename,
      .offset = reader->adjusted_position,
  };
}

void back_up(Reader *reader)
{
  Adjustment *prev_adjustment =
      ARRAY_REF(&reader->adjustments, Adjustment, reader->next_adjustment - 1);
  if (reader->next_adjustment >= 1
      && prev_adjustment->location == reader->position) {
    reader->next_adjustment--;
  }

  reader->position--;
  reader->adjusted_position--;
}

void advance(Reader *reader)
{
  reader->at_start_of_line = peek_char(reader) == '\n';

  u32 start = reader->position;
  for (;;) {
    reader->position++;

    if (at_end(reader)) break;
    if (peek_char(reader) != '\\') break;

    reader->position++;
    if (peek_char(reader) == '\n') {
      continue;
    }

    reader->position--;
    break;
  }
  u32 position_diff = reader->position - start;

  Adjustment *prev_adjustment = NULL;
  if (reader->next_adjustment >= 1
      && reader->next_adjustment <= reader->adjustments.size) {
    prev_adjustment = ARRAY_REF(
        &reader->adjustments, Adjustment, reader->next_adjustment - 1);
  }
  Adjustment *next_adjustment = NULL;
  if (reader->next_adjustment < reader->adjustments.size) {
    next_adjustment =
        ARRAY_REF(&reader->adjustments, Adjustment, reader->next_adjustment);
  }

  if (next_adjustment != NULL
      && next_adjustment->location == reader->position) {
    reader->adjusted_position = next_adjustment->new_source_loc.offset;
    reader->next_adjustment++;
  } else if (
      prev_adjustment != NULL
      && prev_adjustment->type == BEGIN_MACRO_ADJUSTMENT) {
    reader->adjusted_position = prev_adjustment->new_source_loc.offset;
  } else {
    reader->adjusted_position += position_diff;
  }
}

// @TODO: Handle backslash newline in the middle of a symbol.
String read_symbol(Reader *reader)
{
  u32 start_index = reader->position;
  if (!initial_ident_char(peek_char(reader))) return INVALID_STRING;

  while (!at_end(reader)) {
    char c = peek_char(reader);
    if (!ident_char(c)) break;

    advance(reader);
  }

  return (String){
      .chars = reader->buffer.chars + start_index,
      .len = reader->position - start_index,
  };
}

u32 reader_current_line(Reader *reader)
{
  u32 line = 1;
  for (u32 i = 0; i < reader->position; i++) {
    if (reader->buffer.chars[i] == '\n') line++;
  }

  return line;
}

extern inline bool at_end(Reader *reader);
extern inline char peek_char(Reader *reader);
extern inline char read_char(Reader *reader);
extern inline bool expect_char(Reader *reader, char c);
extern inline bool initial_ident_char(char c);
extern inline bool ident_char(char c);
