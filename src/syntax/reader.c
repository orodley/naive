#include "syntax/reader.h"

#include <assert.h>

#include "util.h"

void reader_init(
    Reader *reader, String buffer, Array(Adjustment) adjustments,
    bool at_start_of_line, char *source_filename)
{
  *reader = (Reader){
      .buffer = buffer,
      .position = 0,
      .adjustments = adjustments,
      .next_adjustment = 0,
      .at_start_of_line = at_start_of_line,
      .source_loc =
          {
              .filename = source_filename,
              .line = 1,
              .column = 1,
          },
      .prev_char_source_loc = {NULL, 0, 0},
  };
}

// @TODO: Maybe we should remove all uses of back_up (instead doing peek_char
// followed by advance if we don't want to back up). Then we don't have to keep
// track of prev_source_loc + remove some edge cases.
void back_up(Reader *reader)
{
  Adjustment *prev_adjustment =
      ARRAY_REF(&reader->adjustments, Adjustment, reader->next_adjustment - 1);
  if (reader->next_adjustment >= 1
      && prev_adjustment->location == reader->position) {
    reader->next_adjustment--;
  }

  assert(reader->prev_char_source_loc.filename != NULL);
  reader->source_loc = reader->prev_char_source_loc;
  reader->prev_char_source_loc = (SourceLoc){NULL, 0, 0};

  reader->position--;
}

void advance(Reader *reader)
{
  reader->prev_char_source_loc = reader->source_loc;
  reader->at_start_of_line = peek_char(reader) == '\n';

  for (;;) {
    reader->position++;
    reader->source_loc.column++;

    if (at_end(reader)) break;

    if (peek_char(reader) == '\\') {
      reader->position++;
      if (peek_char(reader) == '\n') {
        reader->source_loc.line++;
        reader->source_loc.column = 0;
        continue;
      } else {
        reader->position--;
        break;
      }
    } else {
      break;
    }
  }

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
    reader->source_loc = next_adjustment->new_source_loc;
    reader->next_adjustment++;
  } else if (
      prev_adjustment != NULL
      && prev_adjustment->type == BEGIN_MACRO_ADJUSTMENT) {
    reader->source_loc = prev_adjustment->new_source_loc;
  } else if (peek_char(reader) == '\n') {
    reader->source_loc.line++;
    reader->source_loc.column = 0;
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

extern inline bool at_end(Reader *reader);
extern inline char peek_char(Reader *reader);
extern inline char read_char(Reader *reader);
extern inline bool expect_char(Reader *reader, char c);
extern inline bool initial_ident_char(char c);
extern inline bool ident_char(char c);
