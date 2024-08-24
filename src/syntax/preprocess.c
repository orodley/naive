#define _POSIX_C_SOURCE 200809L
#include "syntax/preprocess.h"

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

#include "array.h"
#include "diagnostics.h"
#include "file.h"
#include "macros.h"
#include "syntax/lex.h"
#include "syntax/parse.h"
#include "syntax/reader.h"
#include "util.h"

// @TODO: This whole thing is getting pretty overdue for a rewrite. Doing
// everything character-by-character without lexing into preprocessor tokens
// gets very verbose and hard to manage. It also means we have to handle
// comments and whitespace all over the place. We can't lex everything up-front
// because false conditions can contain invalid stuff (unterminated strings for
// instance). But perhaps we can lex it one line at a time? We should also be
// more consistent about using Strings pointing into the file where possible
// rather than allocating and copying all over the place, and be more principled
// with ownership in general.

typedef struct Macro
{
  String name;
  char *value;
  Array(String) arg_names;
  bool variadic;
} Macro;

static Macro *look_up_macro(Array(Macro) *macro_env, String name)
{
  for (u32 i = 0; i < macro_env->size; i++) {
    Macro *m = ARRAY_REF(macro_env, Macro, i);
    if (m->name.len == name.len
        && strneq(m->name.chars, name.chars, name.len)) {
      return m;
    }
  }

  return NULL;
}

typedef struct PPCondScope
{
  bool condition;
  enum
  {
    THEN,
    ELSE
  } position;
} PPCondScope;

typedef struct PP
{
  Reader reader;

  Array(char) out_chars;
  Array(Adjustment) out_adjustments;
  u32 macro_depth;

  Array(InputBuffer) mapped_files;

  Array(String) *include_dirs;
  Array(PPCondScope) pp_scope_stack;
  Array(Macro) macro_env;
  Array(Macro) curr_macro_params;
} PP;

static bool preprocess_string(PP *pp, char *string);
static char *macroexpand(PP *pp, char *string);
static bool preprocess_aux(PP *pp);

static void start_pp_if(PP *pp, bool condition)
{
  Array(PPCondScope) *stack = &pp->pp_scope_stack;
  if (stack->size >= 1 && !ARRAY_LAST(stack, PPCondScope)->condition) {
    condition = false;
  }

  *ARRAY_APPEND(stack, PPCondScope) = (PPCondScope){
      .condition = condition,
      .position = THEN,
  };
}

static bool ignoring_chars(PP *pp)
{
  Array(PPCondScope) *pp_scope_stack = &pp->pp_scope_stack;
  if (pp_scope_stack->size == 0) return false;

  return !ARRAY_LAST(pp_scope_stack, PPCondScope)->condition;
}

static void add_adjustment_to(PP *pp, AdjustmentType type, SourceLoc source_loc)
{
  u32 location = pp->out_chars.size;
  Adjustment *adjustment;

  // Don't bother tracking nested BEGIN_MACRO's - we just want to report all
  // tokens as being at the location of the top-level macro. Just keep track
  // of the depth so we know when we're back to the top-level one.
  switch (type) {
  case BEGIN_MACRO_ADJUSTMENT:
    pp->macro_depth++;
    if (pp->macro_depth != 1) return;

    break;
  case END_MACRO_ADJUSTMENT:
    ASSERT(pp->macro_depth != 0);
    pp->macro_depth--;
    if (pp->macro_depth != 0) return;

    break;
  case NORMAL_ADJUSTMENT: break;
  }

  if (pp->out_adjustments.size == 0) {
    adjustment = ARRAY_APPEND(&pp->out_adjustments, Adjustment);
  } else {
    Adjustment *prev = ARRAY_LAST(&pp->out_adjustments, Adjustment);

    // Again, we don't bother with any adjustments inside macros since
    // everything is reported at the top-level macro location.
    if (prev->type == BEGIN_MACRO_ADJUSTMENT && type == NORMAL_ADJUSTMENT) {
      return;
    }

    if (prev->location == location) {
      adjustment = prev;

      // If we have a normal adjustment directly after an END_MACRO, we
      // don't want to remove the END_MACRO, even if they're at the same
      // location.
      if (prev->type == END_MACRO_ADJUSTMENT && type == NORMAL_ADJUSTMENT)
        type = END_MACRO_ADJUSTMENT;
    } else {
      adjustment = ARRAY_APPEND(&pp->out_adjustments, Adjustment);
    }
  }

  *adjustment = (Adjustment){
      .type = type,
      .location = location,
      .new_source_loc = source_loc,
  };
}

static void add_adjustment(PP *pp, AdjustmentType type)
{
  add_adjustment_to(pp, type, reader_source_loc(&pp->reader));
}

static void skip_whitespace_and_comments_from_reader(
    Reader *reader, Array(char) *out_chars, bool skip_newline)
{
  bool initially_at_start_of_line = reader->at_start_of_line;
  while (!at_end(reader)) {
    switch (peek_char(reader)) {
    case '\n':
      if (!skip_newline) return;

      advance(reader);

      // Retain any newlines - we need them in the output so that the
      // lexer can correctly track source location without needing
      // an adjustment for every single line in the source.
      if (out_chars != NULL) {
        *ARRAY_APPEND(out_chars, char) = '\n';
      }

      initially_at_start_of_line = true;

      break;
    case ' ':
    case '\t': advance(reader); break;
    case '/':
      advance(reader);
      switch (peek_char(reader)) {
      case '/':
        while (peek_char(reader) != '\n' && !at_end(reader)) {
          advance(reader);
        }
        break;
      case '*':
        advance(reader);
        while (!at_end(reader)) {
          if (read_char(reader) == '*') {
            if (read_char(reader) == '/') break;

            back_up(reader);
          }
        }

        if (at_end(reader))
          emit_error(reader_source_loc(reader), "Unterminated /* comment");

        break;
      default:
        back_up(reader);
        reader->at_start_of_line = initially_at_start_of_line;
        return;
      }
      break;
    default: reader->at_start_of_line = initially_at_start_of_line; return;
    }
  }
}

static void skip_whitespace_and_comments(PP *pp, bool skip_newline)
{
  skip_whitespace_and_comments_from_reader(
      &pp->reader, &pp->out_chars, skip_newline);
}

static bool append_string_or_char_literal(
    Reader *reader, char start_char, Array(char) *out_chars)
{
  u32 literal_start = reader->position - 1;
  SourceLoc literal_start_source_loc = reader_source_loc(reader);
  while (peek_char(reader) != start_char) {
    if (at_end(reader)) {
      emit_error(
          literal_start_source_loc, start_char == '"'
                                        ? "Unterminated string literal"
                                        : "Unterminated character literal");
      return false;
    }
    expect_char(reader, '\\');
    advance(reader);
  }
  advance(reader);

  if (out_chars != NULL) {
    ARRAY_APPEND_ELEMS(
        out_chars, char, reader->position - literal_start,
        reader->buffer.chars + literal_start);
  }
  return true;
}

// @TODO: This is probably too conservative - fopen can fail for other reasons.
static bool file_exists(String path)
{
  char *c_str = string_to_c_string(path);
  FILE *f = fopen(c_str, "r");
  free(c_str);
  if (f != NULL) fclose(f);

  return f != NULL;
}

static String look_up_include_path(
    Array(String) *include_dirs, String including_file, String include_path)
{
  // If absolute, just try the exact path.
  if (include_path.chars[0] == '/') {
    if (file_exists(include_path))
      return include_path;
    else
      return INVALID_STRING;
  }

  // Try relative to the including file
  i32 i = including_file.len - 1;
  for (; i >= 0 && including_file.chars[i] != '/'; i--)
    ;

  String base_path;
  // Path without any slashes
  if (i == -1) {
    base_path = STRING("./");
  } else {
    base_path = (String){
        .chars = including_file.chars,
        .len = i + 1,
    };
  }

  String potential_path = string_concat(base_path, include_path);
  if (file_exists(potential_path)) return potential_path;
  free(potential_path.chars);

  // Try include dirs
  for (u32 i = 0; i < include_dirs->size; i++) {
    base_path = *ARRAY_REF(include_dirs, String, i);

    potential_path = string_concat(base_path, include_path);
    if (file_exists(potential_path)) return potential_path;
    free(potential_path.chars);
  }

  return INVALID_STRING;
}

static bool preprocess_file(
    PP *pp, String input_filename, SourceLoc blame_source_loc);

static unsigned long eval_pp_expr(PP *pp, ASTExpr *expr, bool *okay);

static bool eval_pp_condition(PP *pp, bool *result)
{
  Reader *reader = &pp->reader;
  SourceLoc start_source_loc = reader_source_loc(reader);
  skip_whitespace_and_comments(pp, false);

  Array(char) condition_chars;
  ARRAY_INIT(&condition_chars, char, 10);

  for (;;) {
    skip_whitespace_and_comments_from_reader(reader, &condition_chars, false);
    char c = peek_char(reader);
    if (initial_ident_char(c)) {
      String ident = read_symbol(reader);
      if (strneq("defined", ident.chars, ident.len)) {
        skip_whitespace_and_comments_from_reader(reader, NULL, false);
        bool bracketed = false;
        if (expect_char(reader, '(')) {
          bracketed = true;
          skip_whitespace_and_comments_from_reader(reader, NULL, false);
        }
        String macro_name = read_symbol(reader);

        if (bracketed) {
          skip_whitespace_and_comments_from_reader(reader, NULL, false);

          if (!expect_char(reader, ')')) {
            emit_error(
                reader_source_loc(reader),
                "Unexpected char '%c' in `defined' in preprocessor condition",
                peek_char(reader));
            return false;
          }
        }

        char result;
        if (look_up_macro(&pp->macro_env, macro_name) == NULL) {
          result = '0';
        } else {
          result = '1';
        }
        *ARRAY_APPEND(&condition_chars, char) = result;
        *ARRAY_APPEND(&condition_chars, char) = ' ';
      } else {
        for (u32 i = 0; i < ident.len; i++) {
          *ARRAY_APPEND(&condition_chars, char) = ident.chars[i];
        }
      }
    } else if (c == '\'' || c == '"') {
      advance(reader);
      if (!append_string_or_char_literal(reader, c, &condition_chars)) {
        return false;
      }
    } else if (c == '\n') {
      // Leave the reader pointing at the newline, as that's what
      // handle_pp_directive expects.
      break;
    } else {
      advance(reader);
      *ARRAY_APPEND(&condition_chars, char) = c;
    }
  }
  *ARRAY_APPEND(&condition_chars, char) = '\0';
  char *condition_str = (char *)condition_chars.elements;

  char *expanded = macroexpand(pp, condition_str);
  array_free(&condition_chars);

  Array(SourceToken) tokens = EMPTY_ARRAY;
  Array(Adjustment) adjustments = EMPTY_ARRAY;
  *ARRAY_APPEND(&adjustments, Adjustment) = (Adjustment){
      .location = 0,
      .new_source_loc = start_source_loc,
      .type = NORMAL_ADJUSTMENT,
  };
  lex(&tokens, STRING(expanded), &adjustments);

  Pool ast_pool;
  pool_init(&ast_pool, 256);
  ASTExpr *expr;
  if (!parse_expr(&tokens, &ast_pool, &expr)) {
    pool_free(&ast_pool);
    return false;
  }

  bool okay = true;
  unsigned long integer_result = eval_pp_expr(pp, expr, &okay);
  pool_free(&ast_pool);

  *result = integer_result != 0;

  return okay;
}

static unsigned long eval_pp_expr(PP *pp, ASTExpr *expr, bool *okay)
{
  switch (expr->t) {
  case INT_LITERAL_EXPR: return expr->u.int_literal.value;
  case UNARY_PLUS_EXPR: return eval_pp_expr(pp, expr->u.unary_arg, okay);
  case UNARY_MINUS_EXPR: return -eval_pp_expr(pp, expr->u.unary_arg, okay);
  case BIT_NOT_EXPR: return ~eval_pp_expr(pp, expr->u.unary_arg, okay);
  case LOGICAL_NOT_EXPR: return !eval_pp_expr(pp, expr->u.unary_arg, okay);
  case MULTIPLY_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           * eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case DIVIDE_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           / eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case MODULO_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           % eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case ADD_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           + eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case MINUS_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           - eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case LEFT_SHIFT_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           << eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case RIGHT_SHIFT_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           >> eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case LESS_THAN_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           < eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case GREATER_THAN_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           > eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case LESS_THAN_OR_EQUAL_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           <= eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case GREATER_THAN_OR_EQUAL_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           >= eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case EQUAL_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           == eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case NOT_EQUAL_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           != eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case BIT_AND_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           & eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case BIT_XOR_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           ^ eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case BIT_OR_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           | eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case LOGICAL_AND_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           && eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case LOGICAL_OR_EXPR:
    return eval_pp_expr(pp, expr->u.binary_op.arg1, okay)
           || eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case COMMA_EXPR: return eval_pp_expr(pp, expr->u.binary_op.arg2, okay);
  case CONDITIONAL_EXPR:
    return eval_pp_expr(pp, expr->u.ternary_op.arg1, okay)
               ? eval_pp_expr(pp, expr->u.ternary_op.arg2, okay)
               : eval_pp_expr(pp, expr->u.ternary_op.arg3, okay);
  default:
    // @TODO: This isn't accurate as we don't attach SourceLoc to ASTExpr.
    // @TODO: Include the expr type.
    emit_error(
        reader_source_loc(&pp->reader),
        "Unsupported operation in preprocessor conditional");
    *okay = false;
    return 1;
  }
}

static bool handle_pp_directive(PP *pp)
{
  Reader *reader = &pp->reader;

  skip_whitespace_and_comments(pp, false);
  // Empty directive
  if (expect_char(reader, '\n')) {
    return true;
  }

  SourceLoc directive_start = reader_source_loc(reader);

  String directive = read_symbol(reader);
  if (!is_valid(directive)) {
    // @TODO: Sync to the next newline?
    emit_error(reader_source_loc(reader), "Expected preprocessor directive");
    return false;
  }

  // Process #if and friends even if we're currently ignoring tokens.
  if (strneq(directive.chars, "if", directive.len)) {
    // If we're in a false preprocessor conditional, don't bother trying to
    // handle the condition. Both because it's a waste of time, and because
    // it might use something we don't support.
    bool cond;
    if (ignoring_chars(pp)) {
      while (!at_end(reader) && peek_char(reader) != '\n') {
        advance(reader);
      }
      cond = false;
    } else {
      if (!eval_pp_condition(pp, &cond)) return false;
    }

    start_pp_if(pp, cond);
  } else if (strneq(directive.chars, "ifdef", directive.len)) {
    skip_whitespace_and_comments(pp, false);
    String macro_name = read_symbol(reader);
    if (!is_valid(macro_name)) {
      emit_error(reader_source_loc(reader), "Expected identifier after #ifdef");
      return false;
    }

    bool condition = look_up_macro(&pp->macro_env, macro_name) != NULL;
    start_pp_if(pp, condition);
  } else if (strneq(directive.chars, "ifndef", directive.len)) {
    skip_whitespace_and_comments(pp, false);
    String macro_name = read_symbol(reader);
    if (!is_valid(macro_name)) {
      emit_error(
          reader_source_loc(reader), "Expected identifier after #ifndef");
      return false;
    }

    bool condition = look_up_macro(&pp->macro_env, macro_name) == NULL;
    start_pp_if(pp, condition);
  } else if (strneq(directive.chars, "elif", directive.len)) {
    if (pp->pp_scope_stack.size == 0) {
      emit_error(directive_start, "#elif without #if");
      return false;
    }

    PPCondScope *top_scope = ARRAY_LAST(&pp->pp_scope_stack, PPCondScope);
    if (!top_scope->condition) {
      bool cond;
      if (!eval_pp_condition(pp, &cond)) return false;
      if (top_scope->position == THEN && cond) {
        ARRAY_LAST(&pp->pp_scope_stack, PPCondScope)->condition = true;
      }
    } else {
      top_scope->position = ELSE;
      top_scope->condition = false;
    }
  } else if (strneq(directive.chars, "else", directive.len)) {
    PPCondScope *scope = ARRAY_LAST(&pp->pp_scope_stack, PPCondScope);
    if (scope->position == ELSE) {
      emit_error(
          directive_start,
          "Duplicate #else clause for preprocessor conditional");
      return false;
    }

    scope->position = ELSE;
    scope->condition = !scope->condition;
    if (pp->pp_scope_stack.size >= 2) {
      PPCondScope *parent_scope = ARRAY_REF(
          &pp->pp_scope_stack, PPCondScope, pp->pp_scope_stack.size - 2);
      if (!parent_scope->condition) scope->condition = false;
    }
  } else if (strneq(directive.chars, "endif", directive.len)) {
    if (pp->pp_scope_stack.size == 0) {
      emit_error(directive_start, "Unmatched #endif");
      return false;
    }
    pp->pp_scope_stack.size--;
  } else if (!ignoring_chars(pp)) {
    if (strneq(directive.chars, "include", directive.len)) {
      skip_whitespace_and_comments(pp, false);
      SourceLoc include_path_source_loc = reader_source_loc(reader);

      char c = read_char(reader);
      if (c != '<' && c != '"') {
        // @TODO: Resync to newline?
        emit_error(
            reader_source_loc(reader), "Expected filename after #include");
        return false;
      }

      char terminator = c == '<' ? '>' : '"';
      u32 start_index = reader->position;
      while (!at_end(reader) && read_char(reader) != terminator)
        ;

      if (at_end(reader)) {
        emit_error(include_path_source_loc, "Unterminated include path");
        return false;
      }

      u32 end_index = reader->position - 1;
      u32 length = end_index - start_index;

      String include_path =
          (String){reader->buffer.chars + start_index, length};
      String includee_path = look_up_include_path(
          pp->include_dirs, reader_source_loc(reader).filename, include_path);

      if (!is_valid(includee_path)) {
        emit_error(
            include_path_source_loc, "File not found: '%.*s'", include_path.len,
            include_path.chars);
        return false;
      }

      // @LEAK: We leak includee_path because it gets attached to
      // SourceLoc's on tokens created by preprocess_file. Given how little
      // data this should take up in a given compilation this is probably
      // fine.

      bool success =
          preprocess_file(pp, includee_path, include_path_source_loc);

      if (!string_eq(include_path, includee_path)) free(includee_path.chars);

      if (!success) return false;

      skip_whitespace_and_comments(pp, false);
    } else if (strneq(directive.chars, "define", directive.len)) {
      skip_whitespace_and_comments(pp, false);

      String macro_name = read_symbol(reader);
      if (!is_valid(macro_name)) {
        emit_error(
            reader_source_loc(reader), "Expected identifier after #define");
        return false;
      }

      Array(String) arg_names;
      bool variadic = false;
      ARRAY_INIT(&arg_names, String, 0);
      if (expect_char(reader, '(')) {
        for (;;) {
          skip_whitespace_and_comments(pp, false);
          char c = peek_char(reader);
          if (c == '\n') {
            emit_error(
                reader_source_loc(reader),
                "Unexpected newline in macro definition argument list");
            return false;
          } else if (c == '.') {
            advance(reader);
            if (read_char(reader) != '.' || read_char(reader) != '.') {
              emit_error(
                  reader_source_loc(reader),
                  "Unexpected char in variadic macro argument list");
              return false;
            }
            variadic = true;

            skip_whitespace_and_comments(pp, false);
            if (read_char(reader) == ')') break;

            emit_error(
                reader_source_loc(reader),
                "Unexpected char after ellipsis in variadic macro argument "
                "list");
            return false;
          } else {
            String arg_name = read_symbol(reader);
            if (!is_valid(arg_name)) {
              emit_error(
                  reader_source_loc(reader),
                  "Unexpected character while processing "
                  "macro argument list");
              return false;
            }

            *ARRAY_APPEND(&arg_names, String) = arg_name;
            skip_whitespace_and_comments(pp, false);
            char next = read_char(reader);
            if (next == ')') {
              break;
            } else if (next != ',') {
              emit_error(
                  reader_source_loc(reader),
                  "Expected comma after macro argument name");
              return false;
            }
          }
        }
      }

      skip_whitespace_and_comments(pp, false);
      Array(char) macro_value_chars;
      ARRAY_INIT(&macro_value_chars, char, 10);
      while (peek_char(reader) != '\n')
        *ARRAY_APPEND(&macro_value_chars, char) = read_char(reader);

      char *macro_value =
          strndup((char *)macro_value_chars.elements, macro_value_chars.size);
      array_free(&macro_value_chars);

      Macro *macro = look_up_macro(&pp->macro_env, macro_name);
      if (macro != NULL) {
        // @TODO: Proper checks as per C99 6.10.3.2
        if (macro->arg_names.size != arg_names.size) {
          emit_error(
              reader_source_loc(reader),
              "Redefined macro '%.*s' has different number of arguments",
              macro_name.len, macro_name.chars);
          return false;
        }
      } else {
        macro = ARRAY_APPEND(&pp->macro_env, Macro);
      }

      macro->name = macro_name;
      macro->value = macro_value;
      macro->arg_names = arg_names;
      macro->variadic = variadic;
    } else if (strneq(directive.chars, "undef", directive.len)) {
      skip_whitespace_and_comments(pp, false);

      String macro_name = read_symbol(reader);
      if (!is_valid(macro_name)) {
        emit_error(
            reader_source_loc(reader), "Expected identifier after #undef");
        return false;
      }

      // @NOTE: #undef on an undefined macro is allowed (C99 6.10.3.5.2)
      for (u32 i = 0; i < pp->macro_env.size; i++) {
        Macro *m = ARRAY_REF(&pp->macro_env, Macro, i);
        if (m->name.len == macro_name.len
            && strneq(m->name.chars, macro_name.chars, macro_name.len)) {
          ARRAY_REMOVE(&pp->macro_env, Macro, i);
          break;
        }
      }

      skip_whitespace_and_comments(pp, false);
    } else if (strneq(directive.chars, "line", directive.len)) {
      UNIMPLEMENTED("#line preprocessor directive");
    } else if (strneq(directive.chars, "error", directive.len)) {
      advance(reader);

      u32 start = reader->position;
      while (peek_char(reader) != '\n') advance(reader);

      u32 length = reader->position - start;
      char *error = strndup(reader->buffer.chars + start, length);
      emit_error(directive_start, error);
      return false;
    } else if (strneq(directive.chars, "pragma", directive.len)) {
      UNIMPLEMENTED("#pragma preprocessor directive");
    } else {
      emit_error(
          reader_source_loc(reader), "Invalid preprocessor directive: %s",
          directive);
      return false;
    }
  }

  skip_whitespace_and_comments(pp, false);

  if (!ignoring_chars(pp) && !at_end(reader) && peek_char(reader) != '\n') {
    emit_error(
        reader_source_loc(reader),
        "Extraneous text after preprocessing directive");
    return false;
  }

  // Advance past the newline, otherwise it will get added to out_chars by
  // skip_whitespace_and_comments.
  advance(reader);
  add_adjustment(pp, NORMAL_ADJUSTMENT);

  return true;
}

static bool substitute_macro_params(PP *pp, Macro *macro)
{
  Reader *reader = &pp->reader;
  SourceLoc start_source_loc = reader_source_loc(&pp->reader);

  // First, find the span which contains all the arguments to this function-like
  // macro.
  // @TODO: Might make sense to merge this with the next pass.
  Array(char) macro_args_str = EMPTY_ARRAY;
  while (!at_end(reader)) {
    char c = read_char(reader);
    switch (c) {
    case '(': {
      u32 bracket_depth = 1;
      *ARRAY_APPEND(&macro_args_str, char) = c;

      while (bracket_depth != 0 && !at_end(reader)) {
        char c = read_char(reader);
        switch (c) {
        case '(': bracket_depth++; break;
        case ')': bracket_depth--; break;
        }

        // @TODO: Append all at once with ARRAY_APPEND_ELEMS instead.
        *ARRAY_APPEND(&macro_args_str, char) = c;
      }
      break;
    }
    case '"':
    case '\'':
      if (!append_string_or_char_literal(reader, c, &macro_args_str))
        return false;
      break;
    case ')': goto finished_span;
    default: *ARRAY_APPEND(&macro_args_str, char) = c;
    }
  }

  // If we're here, we reached the end of the reader without finding a
  // terminating ')'.
  emit_error(start_source_loc, "Unterminated function-like macro invocation");
  array_free(&macro_args_str);
  return false;

finished_span:;
  // Take the resulting string and divide it up into arguments.
  Reader args_reader;
  reader_init(
      &args_reader,
      (String){
          .chars = (char *)macro_args_str.elements, .len = macro_args_str.size},
      EMPTY_ARRAY, false, STRING("??"));

  Array(char *) arg_values = EMPTY_ARRAY;
  u32 curr_arg_start = 0;
  for (;;) {
    skip_whitespace_and_comments_from_reader(&args_reader, NULL, false);
    char c = read_char(&args_reader);
    switch (c) {
    case '(': {
      u32 bracket_depth = 1;

      while (bracket_depth != 0 && !at_end(&args_reader)) {
        c = read_char(&args_reader);
        switch (c) {
        case '(': bracket_depth++; break;
        case ')': bracket_depth--; break;
        }
      }
      break;
    }
    case '\'':
    case '"':
      if (!append_string_or_char_literal(&args_reader, c, NULL)) {
        array_free(&macro_args_str);
        return false;
      }
      break;
    case EOF:
    case ',': {
      u32 curr_arg_end = args_reader.position - 1;
      if (curr_arg_end != curr_arg_start) {
        String arg_value = {
            .chars = args_reader.buffer.chars + curr_arg_start,
            .len = curr_arg_end - curr_arg_start,
        };
        char *arg_value_str = strndup(arg_value.chars, arg_value.len);
        char *expanded = macroexpand(pp, arg_value_str);
        free(arg_value_str);
        *ARRAY_APPEND(&arg_values, char *) = expanded;
      }

      if (c == EOF) {
        goto got_all_args;
      }

      skip_whitespace_and_comments_from_reader(&args_reader, NULL, false);
      curr_arg_start = args_reader.position;
      break;
    }
    }
  }

got_all_args:
  array_free(&macro_args_str);
  bool ret = true;

  if ((!macro->variadic && arg_values.size != macro->arg_names.size)
      || (macro->variadic && arg_values.size < macro->arg_names.size)) {
    emit_error(
        reader_source_loc(reader),
        "Wrong number of parameters to function-like macro '%.*s'"
        " (expected %u%s, got %u)",
        macro->name.len, macro->name.chars, macro->arg_names.size,
        macro->variadic ? "+" : "", arg_values.size);
    ret = false;
    goto cleanup;
  }

  // Finally, assign each argument to its associated parameter name.
  Array(Macro) new_macro_params;
  ARRAY_INIT(
      &new_macro_params, Macro, arg_values.size + (macro->variadic ? 1 : 0));
  for (u32 i = 0; i < macro->arg_names.size; i++) {
    char *arg_value = *ARRAY_REF(&arg_values, char *, i);
    Macro *arg_macro = ARRAY_APPEND(&new_macro_params, Macro);
    arg_macro->arg_names = EMPTY_ARRAY;
    arg_macro->variadic = false;
    arg_macro->name = *ARRAY_REF(&macro->arg_names, String, i);
    arg_macro->value = arg_value;
  }
  // All extra args get assigned to __VA_ARGS__, with a comma in between each.
  if (arg_values.size > macro->arg_names.size) {
    Array(char) va_args_value = EMPTY_ARRAY;
    for (u32 i = macro->arg_names.size; i < arg_values.size; i++) {
      char *arg_value = *ARRAY_REF(&arg_values, char *, i);
      ARRAY_APPEND_ELEMS(&va_args_value, char, strlen(arg_value), arg_value);
      if (i != arg_values.size - 1) {
        *ARRAY_APPEND(&va_args_value, char) = ',';
      }
    }

    Macro *va_args_macro = ARRAY_APPEND(&new_macro_params, Macro);
    va_args_macro->arg_names = EMPTY_ARRAY;
    va_args_macro->variadic = false;
    va_args_macro->name = STRING("__VA_ARGS__");
    va_args_macro->value =
        strndup((char *)va_args_value.elements, va_args_value.size);
  }

cleanup:
  if (ret) pp->curr_macro_params = new_macro_params;
  return ret;
}

static bool preprocess_file(
    PP *pp, String input_filename, SourceLoc blame_source_loc)
{
  char *c_str = string_to_c_string(input_filename);
  String buffer = map_file_into_memory(c_str);
  free(c_str);
  if (!is_valid(buffer)) {
    if (!is_valid(blame_source_loc.filename)) {
      fprintf(
          stderr, "Failed to open input file: '%.*s'\n", input_filename.len,
          input_filename.chars);
    } else {
      emit_error(
          blame_source_loc, "File not found: '%.*s'\n", input_filename.len,
          input_filename.chars);
    }

    return false;
  }
  *ARRAY_APPEND(&pp->mapped_files, String) = buffer;

  Reader old_reader = pp->reader;

  reader_init(&pp->reader, buffer, EMPTY_ARRAY, true, input_filename);
  add_adjustment(pp, NORMAL_ADJUSTMENT);

  bool ret = preprocess_aux(pp);

  pp->reader = old_reader;
  return ret;
}

static bool preprocess_string(PP *pp, char *string)
{
  Reader old_reader = pp->reader;
  Array(Adjustment) old_adjustments = pp->out_adjustments;
  Array(PPCondScope) old_scope_stack = pp->pp_scope_stack;
  pp->out_adjustments = EMPTY_ARRAY;
  pp->pp_scope_stack = EMPTY_ARRAY;

  reader_init(
      &pp->reader, (String){string, strlen(string)}, EMPTY_ARRAY, false,
      STRING("??"));

  bool ret = preprocess_aux(pp);

  pp->reader = old_reader;
  array_free(&pp->out_adjustments);
  array_free(&pp->pp_scope_stack);
  pp->out_adjustments = old_adjustments;
  pp->pp_scope_stack = old_scope_stack;

  return ret;
}

// Convenience wrapper around preprocess_string that doesn't append to the
// existing buffer, but instead just returns the string.
static char *macroexpand(PP *pp, char *string)
{
  Array(char) old_out_chars = pp->out_chars;
  pp->out_chars = EMPTY_ARRAY;

  if (!preprocess_string(pp, string)) {
    array_free(&pp->out_chars);
    return NULL;
  }

  *ARRAY_APPEND(&pp->out_chars, char) = '\0';

  char *expanded = (char *)pp->out_chars.elements;
  pp->out_chars = old_out_chars;

  return expanded;
}

static void skip_to_next_directive(Reader *reader)
{
  // handle_pp_directive puts us at the beginning of the next line,
  // so we start out ready for another directive.
  while (!at_end(reader)) {
    bool at_start_of_line = reader->at_start_of_line;

    // @TODO: This doesn't work if there's a comment in between the start of the
    // line and the directive.
    switch (peek_char(reader)) {
    case ' ':
    case '\t':
      advance(reader);
      reader->at_start_of_line = at_start_of_line;
      break;
    case '#':
      if (reader->at_start_of_line) {
        return;
      }
      // fallthrough
    default: advance(reader); break;
    }
  }
}

static bool preprocess_aux(PP *pp)
{
  Reader *reader = &pp->reader;
  String *buffer = &pp->reader.buffer;

  ARRAY_ENSURE_ROOM(&pp->out_chars, char, buffer->len);

  while (!at_end(reader)) {
    u32 start_input_position = reader->position;
    u32 start_output_position = pp->out_chars.size;

    // If we just reached a false preprocessor conditional, skip everything
    // until the next preprocessor conditional.
    if (ignoring_chars(pp)) {
      skip_to_next_directive(reader);
      if (at_end(reader)) {
        // @TODO: Keep track of the SourceLoc of the opening directive.
        // Maybe we should store it on PPCondScope?
        SourceLoc s = {STRING("<unknown>"), 0};
        emit_error(s, "Unterminated preprocessor conditional");
        return false;
      }
    } else {
      skip_whitespace_and_comments(pp, true);
    }

    u32 end_input_position = reader->position;
    u32 end_output_position = pp->out_chars.size;

    if (end_input_position - start_input_position
        > end_output_position - start_output_position) {
      // We skipped some chars, but did we skip any newlines? If so
      // they've been placed in the output buffer, and we don't need a
      // token separator. Otherwise we need to append a space.
      if (start_output_position == end_output_position) {
        *ARRAY_APPEND(&pp->out_chars, char) = ' ';
      }

      add_adjustment(pp, NORMAL_ADJUSTMENT);
    }

    SourceLoc start_source_loc = reader_source_loc(reader);
    bool at_start_of_line = reader->at_start_of_line;

    char c = read_char(reader);
    switch (c) {
    // We need to handle string and character literals here so that we
    // don't expand macros inside them.
    case '\'':
    case '"': {
      if (!append_string_or_char_literal(reader, c, &pp->out_chars))
        return false;

      break;
    }
    case '#':
      if (expect_char(reader, '#')) {
        // Token pasting operator
        // @TODO: Handle empty replacements properly.
        // @TODO: Handle the case where '##' is formed out of pasting
        // other tokens together. In this case it doesn't invoke the
        // token pasting operator.
        if (pp->macro_depth == 0) {
          emit_error(
              start_source_loc,
              "The '##' operator is only allowed in a macro definition");
          return false;
        }

        while (pp->out_chars.size >= 1
               && *ARRAY_LAST(&pp->out_chars, char) == ' ') {
          pp->out_chars.size--;
        }

        skip_whitespace_and_comments(pp, false);
      } else if (at_start_of_line) {
        if (!handle_pp_directive(pp)) return false;
      } else if (pp->macro_depth == 0) {
        emit_error(
            start_source_loc,
            "Unexpected preprocessor directive (not at start of line)");
        return false;
      } else {
        // Stringification operator
        // @TODO: Handle whitespace between preprocessing tokens.
        // @TODO: Handle whitespace at the start or end of the
        //        replacement text.
        skip_whitespace_and_comments(pp, false);

        SourceLoc expected_arg_name_source_loc = reader_source_loc(reader);
        String arg_name = read_symbol(reader);

        if (!is_valid(arg_name)) {
          emit_error(
              expected_arg_name_source_loc,
              "Expected identifier after '#' operator");
          return false;
        }

        Macro *macro = look_up_macro(&pp->curr_macro_params, arg_name);
        if (macro == NULL) {
          emit_error(
              expected_arg_name_source_loc,
              "Argument to '#' does not name a macro parameter");
          return false;
        }

        u32 macro_value_len = strlen(macro->value);

        *ARRAY_APPEND(&pp->out_chars, char) = '"';
        for (u32 i = 0; i < macro_value_len; i++) {
          char c = macro->value[i];
          if (c == '"') {
            *ARRAY_APPEND(&pp->out_chars, char) = '\\';
          }

          *ARRAY_APPEND(&pp->out_chars, char) = c;
        }
        *ARRAY_APPEND(&pp->out_chars, char) = '"';
      }

      break;
    case EOF: break;
    default:
      if (initial_ident_char(c)) {
        u32 symbol_start = reader->position - 1;
        while (ident_char(peek_char(reader))) read_char(reader);

        String symbol = (String){
            buffer->chars + symbol_start,
            reader->position - symbol_start,
        };

        Macro *macro = look_up_macro(&pp->curr_macro_params, symbol);
        if (macro == NULL) macro = look_up_macro(&pp->macro_env, symbol);

        if (macro == NULL) {
          ARRAY_APPEND_ELEMS(&pp->out_chars, char, symbol.len, symbol.chars);
        } else if (macro->arg_names.size == 0 && !macro->variadic) {
          Array(Macro) old_params = pp->curr_macro_params;
          pp->curr_macro_params = EMPTY_ARRAY;
          add_adjustment_to(pp, BEGIN_MACRO_ADJUSTMENT, start_source_loc);

          if (!preprocess_string(pp, macro->value)) return false;
          pp->curr_macro_params = old_params;

          add_adjustment(pp, END_MACRO_ADJUSTMENT);
        } else {
          skip_whitespace_and_comments(pp, true);
          if (!expect_char(reader, '(')) {
            // This identifier names a function-like macro, but it
            // appears here without arguments. Therefore, we leave
            // it as is.
            ARRAY_APPEND_ELEMS(&pp->out_chars, char, symbol.len, symbol.chars);
          } else {
            Array(Macro) old_params = pp->curr_macro_params;
            add_adjustment_to(pp, BEGIN_MACRO_ADJUSTMENT, start_source_loc);
            if (!substitute_macro_params(pp, macro)) return false;

            // @TODO: This isn't quite correct - we need to do a separate pass
            // over the body first where we just replace macro params with their
            // values. Either that or have some special logic which pre-expands
            // only macro params before dividing up into arguments. Not doing
            // this yet because it's giving me a headache.
            //
            // I think the real solution here is to process the body of the
            // macro up-front, locating all the params. Then the replacement
            // string can omit them, and we can have a separate list of places
            // to insert arguments (potentially as a string).

            if (!preprocess_string(pp, macro->value)) return false;

            array_free(&pp->curr_macro_params);
            pp->curr_macro_params = old_params;

            // Add a space to separate tokens if necessary.
            char next_char = peek_char(reader);
            if (next_char != ' ' && next_char != '\n' && next_char != EOF) {
              *ARRAY_APPEND(&pp->out_chars, char) = ' ';
            }

            add_adjustment(pp, END_MACRO_ADJUSTMENT);
          }
        }
      } else {
        *ARRAY_APPEND(&pp->out_chars, char) = c;
      }

      break;
    }
  }

  return true;
}

void add_predefined_macro(Array(Macro) *macro_env, char *name, char *value)
{
  *ARRAY_APPEND(macro_env, Macro) = (Macro){
      .arg_names = EMPTY_ARRAY,
      .name = STRING(name),
      .value = value,
  };
}

bool preprocess(
    String input_filename, Array(String) *include_dirs,
    Array(char) *preprocessed, Array(Adjustment) *adjustments)
{
  PP pp = {
      .out_chars = EMPTY_ARRAY,
      .out_adjustments = EMPTY_ARRAY,
      .macro_depth = 0,
      .mapped_files = EMPTY_ARRAY,
      .include_dirs = include_dirs,
      .pp_scope_stack = EMPTY_ARRAY,
      .macro_env = EMPTY_ARRAY,
      .curr_macro_params = EMPTY_ARRAY,
  };
  add_predefined_macro(&pp.macro_env, "__STDC__", "1");
  // @TODO: Change this based on -std, once we support multiple standard
  // versions.
  add_predefined_macro(&pp.macro_env, "__STDC_VERSION__", "199901L");

  bool ret =
      preprocess_file(&pp, input_filename, (SourceLoc){INVALID_STRING, 0});

  for (u32 i = 0; i < pp.mapped_files.size; i++) {
    String *buffer = ARRAY_REF(&pp.mapped_files, String, i);
    unmap_file(*buffer);
  }
  array_free(&pp.mapped_files);
  array_free(&pp.pp_scope_stack);
  for (u32 i = 0; i < pp.macro_env.size; i++) {
    Macro *macro = ARRAY_REF(&pp.macro_env, Macro, i);
    array_free(&macro->arg_names);
  }
  array_free(&pp.macro_env);
  array_free(&pp.curr_macro_params);

  *preprocessed = pp.out_chars;
  *adjustments = pp.out_adjustments;

  return ret;
}
