#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "array.h"
#include "diagnostics.h"
#include "misc.h"
#include "reader.h"
#include "tokenise.h"
#include "util.h"

typedef struct Tokeniser
{
	Reader reader;
	Array(SourceToken) *tokens;
} Tokeniser;


static Token *append_token(
		Tokeniser *tokeniser, SourceLoc source_loc, TokenType type)
{
	SourceToken *source_token = ARRAY_APPEND(tokeniser->tokens, SourceToken);
	source_token->token.t = type;
	source_token->source_loc = source_loc;

	return (Token *)source_token;
}

static bool tokenise_aux(Tokeniser *tokeniser);

bool tokenise(Array(SourceToken) *tokens, Array(char) *text,
		Array(Adjustment) *adjustments)
{
	ARRAY_INIT(tokens, SourceToken, 500);

	Tokeniser tokeniser;
	tokeniser.tokens = tokens;
	reader_init(&tokeniser.reader,
			(String) { (char *)text->elements, text->size },
			*adjustments, false, NULL);

	// @TODO: It feels like there should be a nicer way of doing this such that
	// we don't need a special case here. Maybe reader_init should do the
	// requisite logic from advance to set source_loc properly, but not advance
	// forward a character?
	assert(adjustments->size != 0);
	Adjustment *first = ARRAY_REF(adjustments, Adjustment, 0);
	assert(first->location == 0);
	assert(first->type == NORMAL_ADJUSTMENT);

	tokeniser.reader.source_loc = first->new_source_loc;
	tokeniser.reader.next_adjustment++;

	bool ret = tokenise_aux(&tokeniser);

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


static void read_int_literal_suffix(Reader *reader)
{
	// @TODO: Assign the type based on the suffix and the table in 6.4.4.1.5
	bool read_length_suffix = false;
	bool read_unsigned_suffix = false;

	while (!at_end(reader)) {
		char c = read_char(reader);
		switch (c) {
		case 'u': case 'U':
			if (read_unsigned_suffix) {
				issue_error(&reader->source_loc,
						"Multiple 'u' suffixes on integer literal");
			}

			read_unsigned_suffix = true;
			break;
		case 'l': case 'L':
			if (read_length_suffix) {
				issue_error(&reader->source_loc,
						"Multiple 'l'/'ll' suffixes on integer literal");
			}

			read_length_suffix = true;
			if (peek_char(reader) == c) {
				advance(reader);
			}
			break;
		default:
			back_up(reader);
			return;
		}
	}
}

static bool read_octal_number(Reader *reader, u64 *value)
{
	u64 x = 0;
	char c = peek_char(reader);
	while (c >= '0' && c <= '9') {
		if (c == '8' || c == '9') {
			// @TODO: Skip past all numeric characters to resync?
			issue_error(&reader->source_loc,
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

static bool read_hex_number(Reader *reader, u64 *value)
{
	u64 x = 0;
	bool at_least_one_digit = false;
	for (;;) {
		char c = peek_char(reader);
		if (c >= 'a' && c <= 'f')
			x = x * 16 + c - 'a' + 10;
		else if (c >= 'A' && c <= 'F')
			x = x * 16 + c - 'A' + 10;
		else if (c >= '0' && c <= '9')
			x = x * 16 + c - '0';
		else
			break;

		at_least_one_digit = true;
		advance(reader);
	}

	if (!at_least_one_digit) {
		issue_error(&reader->source_loc,
				"Hexadecimal literal must have at least one digit");
		return false;
	}

	*value = x;

	return true;
}


i64 read_char_in_literal(Reader *reader, SourceLoc *start_source_loc) {
	u64 value;

	char c = read_char(reader);
	if (c == '\\') {
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
			if (!read_octal_number(reader, &value))
				return -1;
			break;
		case 'x':
			if (!read_hex_number(reader, &value))
				return -1;
			break;
		default:
			issue_error(start_source_loc,
					"Invalid escape character '%c'", c);
			return -1;
		}
	} else {
		value = c;
	}

	if (value > 0xFF) {
		issue_error(start_source_loc,
				"Character constant larger than a character");
		return -1;
	}

	return value;
}

static bool tokenise_aux(Tokeniser *tokeniser)
{
	Reader *reader = &tokeniser->reader;

	while (!at_end(reader)) {
		SourceLoc start_source_loc = reader->source_loc;

#define ADD_TOK(t) (append_token(tokeniser, start_source_loc, t))

		switch (read_char(reader)) {
		case '0': {
			u64 value;

			if (at_end(reader)) {
				value = 0;
			} else {
				char c = peek_char(reader);
				if (c == 'x') {
					advance(reader);
					if (!read_hex_number(reader, &value))
						return false;
				} else {
					if (!read_octal_number(reader, &value))
						return false;
				}

				read_int_literal_suffix(reader);
			}

			Token *token = ADD_TOK(TOK_INT_LITERAL);
			token->u.int_literal = value;
			break;
		}
		case '1': case '2': case '3': case '4': case '5': case '6':
		case '7': case '8': case '9': {
			back_up(reader);
			u64 value = 0;

			for (;;) {
				char c = peek_char(reader);
				if (!(c >= '0' && c <= '9'))
					break;

				value *= 10;
				value += c - '0';
				advance(reader);
			}

			read_int_literal_suffix(reader);

			Token *token = ADD_TOK(TOK_INT_LITERAL);
			token->u.int_literal = value;

			break;
		}
		case '"': {
			Array(char) string_literal_chars;
			ARRAY_INIT(&string_literal_chars, char, 20);

			while (peek_char(reader) != '"') {
				i64 c = read_char_in_literal(reader, &start_source_loc);
				if (c == -1) {
					array_free(&string_literal_chars);
					return false;
				}

				*ARRAY_APPEND(&string_literal_chars, char) = (char)c;
			}

			read_char(reader);

			*ARRAY_APPEND(&string_literal_chars, char) = '\0';

			Token *token = ADD_TOK(TOK_STRING_LITERAL);
			token->u.string_literal = (String) {
				.chars = (char *)string_literal_chars.elements,
				.len = string_literal_chars.size - 1,
			};

			break;
		}
		case '\'': {
			i64 value = read_char_in_literal(reader, &start_source_loc);
			if (value == -1)
				return false;

			if (read_char(reader) != '\'') {
				issue_error(&start_source_loc, "Unterminated character literal");
				return false;
			}

			Token *token = ADD_TOK(TOK_INT_LITERAL);
			token->u.int_literal = value;
			break;
		}
		case '+':
			switch (read_char(reader)) {
			case '+': ADD_TOK(TOK_INCREMENT); break;
			case '=': ADD_TOK(TOK_PLUS_ASSIGN); break;
			default: back_up(reader); ADD_TOK(TOK_PLUS); break;
			}

			break;
		case '-':
			switch (read_char(reader)) {
			case '-': ADD_TOK(TOK_DECREMENT); break;
			case '=': ADD_TOK(TOK_MINUS_ASSIGN); break;
			case '>': ADD_TOK(TOK_ARROW); break;
			default: back_up(reader); ADD_TOK(TOK_MINUS); break;
			}

			break;
		case '*':
			if (read_char(reader) == '=') ADD_TOK(TOK_MULTIPLY_ASSIGN);
			else { back_up(reader); ADD_TOK(TOK_ASTERISK); }

			break;
		case '/':
			if (read_char(reader) == '=') ADD_TOK(TOK_DIVIDE_ASSIGN);
			else { back_up(reader); ADD_TOK(TOK_DIVIDE); }

			break;
		case '%':
			if (read_char(reader) == '=') ADD_TOK(TOK_MODULO_ASSIGN);
			else { back_up(reader); ADD_TOK(TOK_MODULO); }

			break;
		case '&':
			switch (read_char(reader)) {
			case '&': ADD_TOK(TOK_LOGICAL_AND); break;
			case '=': ADD_TOK(TOK_BIT_AND_ASSIGN); break;
			default: back_up(reader); ADD_TOK(TOK_AMPERSAND); break;
			}

			break;
		case '|':
			switch (read_char(reader)) {
			case '|': ADD_TOK(TOK_LOGICAL_OR); break;
			case '=': ADD_TOK(TOK_BIT_OR_ASSIGN); break;
			default: back_up(reader); ADD_TOK(TOK_BIT_OR); break;
			}

			break;
		case '^':
			if (read_char(reader) == '=') ADD_TOK(TOK_BIT_XOR_ASSIGN);
			else { back_up(reader); ADD_TOK(TOK_BIT_XOR); }

			break;
		case '=':
			if (read_char(reader) == '=') ADD_TOK(TOK_EQUAL);
			else { back_up(reader); ADD_TOK(TOK_ASSIGN); }

			break;
		case '!':
			if (read_char(reader) == '=') ADD_TOK(TOK_NOT_EQUAL);
			else { back_up(reader); ADD_TOK(TOK_LOGICAL_NOT); }

			break;
		case '<':
			switch (read_char(reader)) {
			case '=': ADD_TOK(TOK_LESS_THAN_OR_EQUAL); break;
			case '<':
				if (read_char(reader) == '=') ADD_TOK(TOK_LEFT_SHIFT_ASSIGN);
				else { back_up(reader); ADD_TOK(TOK_LEFT_SHIFT); }
				break;
			default: back_up(reader); ADD_TOK(TOK_LESS_THAN); break;
			}

			break;
		case '>':
			switch (read_char(reader)) {
			case '=': ADD_TOK(TOK_GREATER_THAN_OR_EQUAL); break;
			case '>':
				if (read_char(reader) == '=') ADD_TOK(TOK_RIGHT_SHIFT_ASSIGN);
				else { back_up(reader); ADD_TOK(TOK_RIGHT_SHIFT); }
				break;
			default: back_up(reader); ADD_TOK(TOK_GREATER_THAN); break;
			}

			break;
		case '.':
			if (read_char(reader) == '.') {
				if (read_char(reader) == '.') ADD_TOK(TOK_ELLIPSIS);
				else { back_up(reader); back_up(reader); ADD_TOK(TOK_DOT); }
			} else { back_up(reader); ADD_TOK(TOK_DOT); }

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

		case ' ': case '\n':
			break;

		default: {
			back_up(reader);
			String symbol = read_symbol(reader);
			assert(is_valid(symbol));
			// @TODO: These two should be handled in the preprocessor.
			if (strneq(symbol.chars, "__LINE__", symbol.len)) {
				Token *line_number = ADD_TOK(TOK_INT_LITERAL);
				line_number->u.int_literal = reader->source_loc.line;
			} else if (strneq(symbol.chars, "__FILE__", symbol.len)) {
				Token *file_name = ADD_TOK(TOK_STRING_LITERAL);
				assert(reader->source_loc.filename != NULL);
				file_name->u.string_literal = (String) {
					.chars = reader->source_loc.filename,
					.len = strlen(reader->source_loc.filename),
				};
			} else {
				Token *token = ADD_TOK(TOK_SYMBOL);
				assert(symbol.chars != NULL);
				token->u.symbol = strndup(symbol.chars, symbol.len);
			}

			break;
		}
		}
	}

	return true;
}




#define X(x) #x
char *token_type_names[] = {
	TOKEN_TYPES
};
#undef X

void dump_token(Token *token)
{
	fputs(token_type_names[token->t], stdout);
	switch (token->t) {
	case TOK_INT_LITERAL:
		printf("(%" PRIu64 ")", token->u.int_literal);
		break;
	case TOK_STRING_LITERAL:
		// @TODO: Escape the resulting string
		printf("(\"%s\")", token->u.string_literal.chars);
		break;
	case TOK_SYMBOL:
		printf("(%s)", token->u.symbol);
		break;
	default:
		break;
	}
}
