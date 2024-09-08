#define _POSIX_C_SOURCE 200809L
#include "syntax/lex.h"

#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "array.h"
#include "assertions.h"
#include "diagnostics.h"
#include "macros.h"
#include "syntax/reader.h"
#include "types.h"

typedef struct Lexer
{
  Reader reader;
  Array(SourceToken) *tokens;
} Lexer;

static Token *append_token(
    Lexer *lexer, SourceRange source_range, TokenType type)
{
  SourceToken *source_token = ARRAY_APPEND(lexer->tokens, SourceToken);
  source_token->token.t = type;
  source_token->source_range = source_range;

  return (Token *)source_token;
}

static bool lex_aux(Lexer *lexer);
static bool read_numeric_literal(Reader *reader, Token *token);
static void classify_numeric_literal(
    Reader *reader, TokenType *token_type, int *out_radix);
static Token read_int_literal(Reader *reader, int radix);
static Token read_float_literal(Reader *reader, int radix);
static int char_to_digit(char c, int radix);

bool lex(
    Array(SourceToken) *tokens, String text, Array(Adjustment) *adjustments)
{
  ARRAY_INIT(tokens, SourceToken, 500);

  Lexer lexer;
  lexer.tokens = tokens;
  // @TODO: This feels like a bit of a hack.
  String filename = INVALID_STRING;
  if (adjustments->size != 0) {
    Adjustment *first = ARRAY_REF(adjustments, Adjustment, 0);
    filename = first->new_source_loc.filename;
  }
  reader_init(&lexer.reader, text, *adjustments, false, filename);

  // @TODO: It feels like there should be a nicer way of doing this such that
  // we don't need a special case here. Maybe reader_init should do the
  // requisite logic from advance to set source_loc properly, but not advance
  // forward a character?
  ASSERT(adjustments->size != 0);
  Adjustment *first = ARRAY_REF(adjustments, Adjustment, 0);
  ASSERT(first->location == 0);
  ASSERT(first->type == NORMAL_ADJUSTMENT);

  lexer.reader.next_adjustment++;

  bool ret = lex_aux(&lexer);

  // Concatentate adjacent string literals
  u32 dest = 0;
  u32 i = 0;
  while (i < tokens->size) {
    SourceToken *token = ARRAY_REF(tokens, SourceToken, i);
    u32 j = i + 1;
    if (token->token.t == TOK_STRING_LITERAL) {
      String str = token->token.u.string_literal;
      u32 str_size = str.len + 1;

      while (j < tokens->size
             && ARRAY_REF(tokens, SourceToken, j)->token.t
                    == TOK_STRING_LITERAL) {
        String next_str =
            ARRAY_REF(tokens, SourceToken, j)->token.u.string_literal;
        u32 new_size = str_size + next_str.len;
        str.chars = realloc(str.chars, new_size);
        memcpy(str.chars + str_size - 1, next_str.chars, next_str.len + 1);

        str_size = new_size;
        j++;
      }

      str.len = str_size - 1;
      token->token.u.string_literal = str;
    }

    if (i != dest) {
      *ARRAY_REF(tokens, SourceToken, dest) = *token;
    }

    dest++;
    i = j;
  }

  tokens->size = dest;

  return ret;
}

static bool is_numeric_suffix_char(char c)
{
  return c == 'u' || c == 'U' || c == 'l' || c == 'L' || c == 'f' || c == 'F';
}

static NumericSuffix read_numeric_suffix(Reader *reader)
{
  NumericSuffix suffix = NO_SUFFIX;
  SourceLoc start_loc = reader_source_loc(reader);

  while (!at_end(reader)) {
    SourceLoc loc = reader_source_loc(reader);
    char c = read_char(reader);
    switch (c) {
    case 'u':
    case 'U':
      if ((suffix & UNSIGNED_SUFFIX) != 0) {
        emit_error(
            (SourceRange){start_loc, loc},
            "Multiple 'u' suffixes on numeric literal");
      }

      suffix |= UNSIGNED_SUFFIX;
      break;
    case 'l':
    case 'L':
      if (((suffix & (LONG_SUFFIX | LONG_LONG_SUFFIX)) != 0)) {
        emit_error(
            (SourceRange){start_loc, loc},
            "Multiple 'l'/'ll' suffixes on numeric literal");
      }

      if (expect_char(reader, c)) {
        suffix |= LONG_LONG_SUFFIX;
      } else {
        suffix |= LONG_SUFFIX;
      }
      break;
    case 'f':
    case 'F':
      if (((suffix & FLOAT_SUFFIX) != 0)) {
        emit_error(
            (SourceRange){start_loc, loc},
            "Multiple 'f' suffixes on numeric literal");
      }

      suffix |= FLOAT_SUFFIX;
      break;

    default: back_up(reader); return suffix;
    }
  }

  return suffix;
}

static bool read_octal_number(Reader *reader, u64 *value)
{
  u64 x = 0;
  char c = peek_char(reader);
  while (c >= '0' && c <= '9') {
    if (c == '8' || c == '9') {
      // @TODO: Skip past all numeric characters to resync?
      emit_error(
          point_range(reader_source_loc(reader)),
          "Invalid digit '%c' in octal literal", c);
      return false;
    } else {
      x *= 8;
      x += c - '0';

      advance(reader);
      c = peek_char(reader);
    }
  }

  *value = x;
  return true;
}

// Expects the reader to be pointing at the first digit after the '0x'.
static bool read_hex_number(Reader *reader, u64 *value)
{
  SourceLoc start_loc = reader_source_loc(reader);
  start_loc.offset -= 2;

  u64 x = 0;
  bool at_least_one_digit = false;
  for (;;) {
    char c = peek_char(reader);
    if (c >= 'a' && c <= 'f') {
      x = x * 16 + c - 'a' + 10;
    } else if (c >= 'A' && c <= 'F') {
      x = x * 16 + c - 'A' + 10;
    } else if (c >= '0' && c <= '9') {
      x = x * 16 + c - '0';
    } else {
      break;
    }

    at_least_one_digit = true;
    advance(reader);
  }

  if (!at_least_one_digit) {
    emit_error(
        (SourceRange){start_loc, reader_source_loc(reader)},
        "Hexadecimal literal must have at least one digit");
    return false;
  }

  *value = x;

  return true;
}

i64 read_char_in_literal(Reader *reader, SourceLoc start_loc)
{
  u64 value;

  char c = read_char(reader);
  if (c == '\\') {
    SourceLoc loc = reader_source_loc(reader);
    switch (read_char(reader)) {
    case '\\': value = '\\'; break;
    case '\'': value = '\''; break;
    case '"': value = '"'; break;
    case 'a': value = '\a'; break;
    case 'b': value = '\b'; break;
    case 'f': value = '\f'; break;
    case 'n': value = '\n'; break;
    case 'r': value = '\r'; break;
    case 't': value = '\t'; break;
    case 'v': value = '\v'; break;
    case '0':
      if (!read_octal_number(reader, &value)) return -1;
      break;
    case 'x':
      if (!read_hex_number(reader, &value)) return -1;
      break;
    default:
      emit_error(
          (SourceRange){start_loc, loc}, "Invalid escape character '%c'", c);
      return -1;
    }
  } else {
    value = (unsigned char)c;
  }

  if (value > 0xFF) {
    emit_error(
        range_from(reader, start_loc),
        "Character constant larger than a character");
    return -1;
  }

  return value;
}

static bool lex_aux(Lexer *lexer)
{
  Reader *reader = &lexer->reader;

  while (!at_end(reader)) {
    SourceLoc start_loc = reader_source_loc(reader);

#define ADD_TOK(t) append_token(lexer, range_from(reader, start_loc), t)

    switch (read_char(reader)) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9': {
      back_up(reader);

      SourceToken *token = ARRAY_APPEND(lexer->tokens, SourceToken);
      if (!read_numeric_literal(reader, (Token *)token)) return false;
      token->source_range = range_from(reader, start_loc);
      break;
    }
    case '"': {
      Array(char) string_literal_chars;
      ARRAY_INIT(&string_literal_chars, char, 20);

      while (peek_char(reader) != '"') {
        i64 c = read_char_in_literal(reader, start_loc);
        if (c == -1) {
          array_free(&string_literal_chars);
          return false;
        }

        *ARRAY_APPEND(&string_literal_chars, char) = (char)c;
      }

      read_char(reader);

      *ARRAY_APPEND(&string_literal_chars, char) = '\0';

      Token *token = ADD_TOK(TOK_STRING_LITERAL);
      token->u.string_literal = (String){
          .chars = (char *)string_literal_chars.elements,
          .len = string_literal_chars.size - 1,
      };

      break;
    }
    case '\'': {
      i64 value = read_char_in_literal(reader, start_loc);
      if (value == -1) return false;

      if (read_char(reader) != '\'') {
        emit_error(
            range_from(reader, start_loc), "Unterminated character literal");
        return false;
      }

      Token *token = ADD_TOK(TOK_INT_LITERAL);
      token->u.int_literal.value = value;
      token->u.int_literal.suffix = NO_SUFFIX;
      break;
    }
    case '+':
      if (expect_char(reader, '+')) {
        ADD_TOK(TOK_INCREMENT);
      } else if (expect_char(reader, '=')) {
        ADD_TOK(TOK_PLUS_ASSIGN);
      } else {
        ADD_TOK(TOK_PLUS);
      }
      break;
    case '-':
      if (expect_char(reader, '-')) {
        ADD_TOK(TOK_DECREMENT);
      } else if (expect_char(reader, '=')) {
        ADD_TOK(TOK_MINUS_ASSIGN);
      } else if (expect_char(reader, '>')) {
        ADD_TOK(TOK_ARROW);
      } else {
        ADD_TOK(TOK_MINUS);
      }
      break;
    case '*':
      if (expect_char(reader, '=')) {
        ADD_TOK(TOK_MULTIPLY_ASSIGN);
      } else {
        ADD_TOK(TOK_ASTERISK);
      }
      break;
    case '/':
      if (expect_char(reader, '=')) {
        ADD_TOK(TOK_DIVIDE_ASSIGN);
      } else {
        ADD_TOK(TOK_DIVIDE);
      }
      break;
    case '%':
      if (expect_char(reader, '=')) {
        ADD_TOK(TOK_MODULO_ASSIGN);
      } else {
        ADD_TOK(TOK_MODULO);
      }
      break;
    case '&':
      if (expect_char(reader, '&')) {
        ADD_TOK(TOK_LOGICAL_AND);
      } else if (expect_char(reader, '=')) {
        ADD_TOK(TOK_BIT_AND_ASSIGN);
      } else {
        ADD_TOK(TOK_AMPERSAND);
      }
      break;
    case '|':
      if (expect_char(reader, '|')) {
        ADD_TOK(TOK_LOGICAL_OR);
      } else if (expect_char(reader, '=')) {
        ADD_TOK(TOK_BIT_OR_ASSIGN);
      } else {
        ADD_TOK(TOK_BIT_OR);
      }
      break;
    case '^':
      if (expect_char(reader, '=')) {
        ADD_TOK(TOK_BIT_XOR_ASSIGN);
      } else {
        ADD_TOK(TOK_BIT_XOR);
      }
      break;
    case '=':
      if (expect_char(reader, '=')) {
        ADD_TOK(TOK_EQUAL);
      } else {
        ADD_TOK(TOK_ASSIGN);
      }
      break;
    case '!':
      if (expect_char(reader, '=')) {
        ADD_TOK(TOK_NOT_EQUAL);
      } else {
        ADD_TOK(TOK_LOGICAL_NOT);
      }
      break;
    case '<':
      if (expect_char(reader, '=')) {
        ADD_TOK(TOK_LESS_THAN_OR_EQUAL);
      } else if (expect_char(reader, '<')) {
        if (expect_char(reader, '=')) {
          ADD_TOK(TOK_LEFT_SHIFT_ASSIGN);
        } else {
          ADD_TOK(TOK_LEFT_SHIFT);
        }
      } else {
        ADD_TOK(TOK_LESS_THAN);
      }
      break;
    case '>':
      if (expect_char(reader, '=')) {
        ADD_TOK(TOK_GREATER_THAN_OR_EQUAL);
      } else if (expect_char(reader, '>')) {
        if (expect_char(reader, '=')) {
          ADD_TOK(TOK_RIGHT_SHIFT_ASSIGN);
        } else {
          ADD_TOK(TOK_RIGHT_SHIFT);
        }
      } else {
        ADD_TOK(TOK_GREATER_THAN);
      }
      break;
    case '.':
      if (expect_char(reader, '.')) {
        if (expect_char(reader, '.')) {
          ADD_TOK(TOK_ELLIPSIS);
        } else {
          back_up(reader);
          ADD_TOK(TOK_DOT);
        }
      } else if (isdigit(peek_char(reader))) {
        back_up(reader);
        *ADD_TOK(TOK_FLOAT_LITERAL) = read_float_literal(reader, 10);
      } else {
        ADD_TOK(TOK_DOT);
      }
      break;
    case '~': ADD_TOK(TOK_BIT_NOT); break;
    case '?': ADD_TOK(TOK_QUESTION_MARK); break;
    case ':': ADD_TOK(TOK_COLON); break;
    case ';': ADD_TOK(TOK_SEMICOLON); break;
    case ',': ADD_TOK(TOK_COMMA); break;

    case '{': ADD_TOK(TOK_LCURLY); break;
    case '}': ADD_TOK(TOK_RCURLY); break;
    case '(': ADD_TOK(TOK_LROUND); break;
    case ')': ADD_TOK(TOK_RROUND); break;
    case '[': ADD_TOK(TOK_LSQUARE); break;
    case ']': ADD_TOK(TOK_RSQUARE); break;

    case ' ':
    case '\n': break;

    default: {
      back_up(reader);
      String symbol = read_symbol(reader);
      ASSERT(is_valid(symbol));
      // @TODO: These two should be handled in the preprocessor.
      if (string_eq(symbol, STRING("__LINE__"))) {
        Token *line_number = ADD_TOK(TOK_INT_LITERAL);
        line_number->u.int_literal.value = reader_current_line(reader);
        line_number->u.int_literal.suffix = NO_SUFFIX;
      } else if (string_eq(symbol, STRING("__FILE__"))) {
        Token *file_name = ADD_TOK(TOK_STRING_LITERAL);
        file_name->u.string_literal = reader->filename;
      } else {
        Token *token = ADD_TOK(TOK_SYMBOL);
        ASSERT(symbol.chars != NULL);
        // @LEAK
        // We should have a pool for this.
        token->u.symbol = string_dup(symbol);
      }

      break;
    }
    }
  }

  return true;
}

static bool read_numeric_literal(Reader *reader, Token *token)
{
  u32 start_pos = reader->position;
  u32 start_adjusted_pos = reader->adjusted_position;

  TokenType type;
  int radix;
  classify_numeric_literal(reader, &type, &radix);

  reader->position = start_pos;
  reader->adjusted_position = start_adjusted_pos;

  if (radix == 16) {
    // Skip past the "0x" we know is there
    advance(reader);
    advance(reader);
  }

  if (type == TOK_INT_LITERAL) {
    *token = read_int_literal(reader, radix);
  } else {
    ASSERT(type == TOK_FLOAT_LITERAL);
    *token = read_float_literal(reader, radix);
  }

  return true;
}

static void skip_digits(Reader *reader, int radix);

static void classify_numeric_literal(
    Reader *reader, TokenType *token_type, int *out_radix)
{
  int radix = 10;
  if (expect_char(reader, '0')) {
    if (!at_end(reader)) {
      if (expect_char(reader, 'x')) {
        radix = 16;
      } else if (isdigit(peek_char(reader))) {
        radix = 8;
      }
    }
  }

  *out_radix = radix;

  skip_digits(reader, radix);

  if (radix == 8) {
    *token_type = TOK_INT_LITERAL;
    return;
  }

  if (expect_char(reader, '.')) {
    *token_type = TOK_FLOAT_LITERAL;
    return;
  }

  if (radix == 10 && (expect_char(reader, 'e') || expect_char(reader, 'E'))) {
    *token_type = TOK_FLOAT_LITERAL;
    return;
  }

  if (radix == 16 && (expect_char(reader, 'p') || expect_char(reader, 'P'))) {
    *token_type = TOK_FLOAT_LITERAL;
    return;
  }

  *token_type = TOK_INT_LITERAL;
}

static void skip_digits(Reader *reader, int radix)
{
  for (;;) {
    char c = peek_char(reader);
    if (char_to_digit(c, radix) != -1) {
      advance(reader);
    } else {
      break;
    }
  }
}

static Token read_int_literal(Reader *reader, int radix)
{
  u64 value = 0;
  for (;;) {
    int digit = char_to_digit(peek_char(reader), radix);
    if (digit == -1) break;

    value = value * radix + digit;
    advance(reader);
  }

  NumericSuffix suffix = NO_SUFFIX;
  if (is_numeric_suffix_char(peek_char(reader))) {
    suffix = read_numeric_suffix(reader);
  }

  return (Token){
      .t = TOK_INT_LITERAL,
      .u.int_literal.value = value,
      .u.int_literal.suffix = suffix,
  };
}

double double_from_decimal_exponent(u64 significand, i32 exponent);

// @TODO: There are things we need to do differently for float literals for
// double literals. For now we just treat everything as double.
static Token read_float_literal(Reader *reader, int radix)
{
  u32 extra_exponent = 0;
  u64 significand = 0;
  int significand_digit_count = 0;
  int decimal_point_position = -1;

  // For hex literals, the significand is base 16 but the exponent is
  // base 2. So every digit past the decimal point shifts the exponent by 4,
  // not 2.
  u32 exponent_per_digit = radix == 10 ? 1 : 4;

  for (;;) {
    char c = peek_char(reader);
    if (c == '.') {
      decimal_point_position = significand_digit_count;
      advance(reader);
      continue;
    }

    int digit = char_to_digit(c, radix);
    if (digit == -1) break;

    significand_digit_count += 1;
    if (significand < 1UL << 52) {
      significand = significand * radix + digit;
    } else {
      // If the significand is too large, we can't represent it as an integer,
      // but we still want to get the magnitude correct. So we do this by just
      // dropping significant digits and increasing the exponent.
      extra_exponent += exponent_per_digit;
    }
    advance(reader);
  }

  i32 exponent = 0;
  char c = peek_char(reader);
  if ((radix == 10 && (c == 'e' || c == 'E'))
      || (radix == 16 && (c == 'p' || c == 'P'))) {
    advance(reader);
    bool negative = false;
    if (!expect_char(reader, '+')) negative = expect_char(reader, '-');
    for (;;) {
      char c = peek_char(reader);

      // Exponents are always written in base 10, even for hex float literals.
      // The base to which the exponent is raised is different, but the literal
      // expressing the exponent is always base 10.
      int digit = char_to_digit(c, 10);
      if (digit == -1) break;

      exponent = exponent * 10 + digit;
      advance(reader);
    }

    if (negative) exponent = -exponent;
  }

  NumericSuffix suffix = NO_SUFFIX;
  if (is_numeric_suffix_char(peek_char(reader))) {
    suffix = read_numeric_suffix(reader);
  }

  exponent += extra_exponent;
  if (decimal_point_position != -1) {
    u32 digits_past_decimal_point =
        significand_digit_count - decimal_point_position;
    exponent -= exponent_per_digit * digits_past_decimal_point;
  }

  // We store the result in a double regardless of whether it's a float or
  // double, as floats can be losslessly converted into doubles.
  double value;
  if (radix == 16) {
    // Hex float literals are easy: the significand is guaranteed to be exactly
    // representable as a double, and the exponent is already base 2.
    value = ldexp((double)significand, exponent);
  } else {
    // @TODO: There are some edge cases where parsing as a double and converting
    // to a float gives a different result from parsing to a float directly. We
    // ignore these for now.
    value = double_from_decimal_exponent(significand, exponent);
  }

  return (Token){
      .t = TOK_FLOAT_LITERAL,
      .u.float_literal.value = value,
      .u.float_literal.suffix = suffix,
  };
}

static int char_to_digit(char c, int radix)
{
  if (radix == 16 && c >= 'a' && c <= 'f') {
    return c - 'a' + 10;
  } else if (radix == 16 && c >= 'A' && c <= 'F') {
    return c - 'A' + 10;
  } else if (radix >= 10 && c >= '8' && c <= '9') {
    return c - '0';
  } else if (c >= '0' && c <= '7') {
    return c - '0';
  } else {
    return -1;
  }
}

double positive_powers_of_ten[] = {
    1e0,  1e1,  1e2,  1e3,  1e4,  1e5,  1e6,  1e7,  1e8,  1e9,  1e10, 1e11,
    1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22,
};

double negative_powers_of_ten[] = {
    1e-0,  1e-1,  1e-2,  1e-3,  1e-4,  1e-5,  1e-6,  1e-7,
    1e-8,  1e-9,  1e-10, 1e-11, 1e-12, 1e-13, 1e-14, 1e-15,
    1e-16, 1e-17, 1e-18, 1e-19, 1e-20, 1e-21, 1e-22,
};

double double_from_decimal_exponent(u64 significand, i32 exponent)
{
  if (abs(exponent) <= 22) {
    // This is the easy case.
    // Powers of ten from -22 to 22 can be exactly represented as doubles. The
    // significand can also be exactly represented, and double multiplication is
    // correctly rounded by definition, so we can just convert the significand
    // to a double and multiply and we're done.
    double x = (double)significand;
    double y =
        (exponent < 0 ? negative_powers_of_ten
                      : positive_powers_of_ten)[abs(exponent)];
    return x * y;
  }

  UNIMPLEMENTED("Float parsing with exponent %d (> 22)", exponent);
}

#define X(x) #x
char *token_type_names[] = {TOKEN_TYPES};
#undef X

void dump_numeric_suffix(NumericSuffix suffix)
{
  if ((suffix & UNSIGNED_SUFFIX) != 0) {
    putchar('U');
  }
  if ((suffix & LONG_SUFFIX) != 0) {
    putchar('L');
  }
  if ((suffix & LONG_LONG_SUFFIX) != 0) {
    fputs("LL", stdout);
  }
  if ((suffix & FLOAT_SUFFIX) != 0) {
    fputs("F", stdout);
  }
}

void dump_token(Token *token)
{
  fputs(token_type_names[token->t], stdout);
  switch (token->t) {
  case TOK_INT_LITERAL: {
    printf("(%" PRIu64, token->u.int_literal.value);
    dump_numeric_suffix(token->u.int_literal.suffix);
    putchar(')');
    break;
  }
  case TOK_FLOAT_LITERAL: {
    printf("(%e", token->u.float_literal.value);
    dump_numeric_suffix(token->u.float_literal.suffix);
    putchar(')');
    break;
  }
  case TOK_STRING_LITERAL:
    // @TODO: Escape the resulting string
    printf("(\"%s\")", token->u.string_literal.chars);
    break;
  case TOK_SYMBOL:
    printf("(%.*s)", token->u.symbol.len, token->u.symbol.chars);
    break;
  default: break;
  }
}
