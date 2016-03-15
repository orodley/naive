#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "misc.h"
#include "tokenise.h"
#include "array.h"

// @PORT
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>

typedef struct InputBuffer
{
	char *buffer;
	size_t length;
} InputBuffer;

#define INVALID_INPUT_BUFFER ((InputBuffer) { NULL, 0 })

// @PORT
InputBuffer map_file_into_memory(const char *filename)
{
	int fd = open(filename, O_RDONLY);
	if (fd == -1)
		return INVALID_INPUT_BUFFER;

	off_t file_size = lseek(fd, 0, SEEK_END);

	if (file_size == -1)
		return INVALID_INPUT_BUFFER;

	char *buffer = mmap(NULL, file_size, PROT_READ, MAP_PRIVATE, fd, 0);
	if (buffer == MAP_FAILED)
		return INVALID_INPUT_BUFFER;

	close(fd);

	return (InputBuffer) { buffer, file_size };
}


typedef struct Reader
{
	char *buffer;
	u32 position;
	u32 length;

	u32 line;
	u32 column;
} Reader;

static inline char peek_char(Reader *reader)
{
	return reader->buffer[reader->position];
}

static inline void advance(Reader *reader)
{
	if (peek_char(reader) == '\n') {
		reader->line++;
		reader->column = 1;
	} else {
		reader->column++;
	}

	reader->position++;
}

static inline char read_char(Reader *reader)
{
	char c = peek_char(reader);
	advance(reader);

	return c;
}

static inline void back_up(Reader *reader)
{
	// We should never need to back up over a newline. We assert that we don't
	// because if we did we'd need to keep track of the previous lines length
	// to preserve source information.
	assert(peek_char(reader) != '\n');

	reader->column--;
	reader->position--;
}

// @IMPROVE: Replace with a finite state machine
void tokenise(Array(SourceToken) *tokens, const char *input_filename)
{
	InputBuffer buffer = map_file_into_memory(input_filename);

	if (buffer.buffer == NULL)
		return;

	// @TUNE: 500 tokens is a quick estimate of a reasonable minimum. We should
	// do some more thorough measurement and determine a good value for this.
	ARRAY_INIT(tokens, SourceToken, 500);

	Reader reader = { buffer.buffer, 0, (u32)buffer.length, 1, 1 };
	Reader *r = &reader;

	SourceToken *source_token;
	bool read_token = true;
	while (reader.position < reader.length) {
		if (read_token) {
			source_token = ARRAY_APPEND(tokens, SourceToken);
			source_token->token.type = TOK_INVALID;
		}

		source_token->source_loc.filename = input_filename;
		source_token->source_loc.line = reader.line;
		source_token->source_loc.column = reader.column;

		Token *token = &source_token->token;

		// We read tokens more often than not, so set it here and override it
		// later in the few cases where it's necessary.
		read_token = true;

		switch (read_char(r)) {
		case '0': case '1': case '2': case '3': case '4': case '5': case '6':
		case '7': case '8': case '9': {
			i64 value = 0;

			for (;;) {
				char c = peek_char(r);
				if (!(c >= '0' && c <= '9'))
					break;

				value *= 10;
				value += c - '0';
				advance(r);
			}

			token->type = TOK_INT_LITERAL;
			token->val.int_literal = value;

			break;
		}
		case '"': {
			u32 start_index = reader.position;
			for (;;) {
				char c = read_char(r);

				if (c == '\\')
					advance(r);
				else if (c == '"')
					break;
			}

			u32 size = ((reader.position - 1) - start_index) + 1;

			token->type = TOK_STRING_LITERAL;
			char *str = malloc(size);
			strncpy(str, reader.buffer + start_index, size - 1);
			str[size - 1] = '\0';

			token->val.symbol_or_string_literal = str;

			break;
		}
		case '+':
			switch (read_char(r)) {
			case '+':
				token->type = TOK_INCREMENT;
				break;
			case '=':
				token->type = TOK_PLUS_ASSIGN;
				break;
			default:
				back_up(r);
				token->type = TOK_PLUS;
			}

			break;

		case '-':
			switch (read_char(r)) {
			case '-':
				token->type = TOK_DECREMENT;
				break;
			case '=':
				token->type = TOK_MINUS_ASSIGN;
				break;
			case '>':
				token->type = TOK_ARROW;
				break;
			default:
				back_up(r);
				token->type = TOK_MINUS;
			}

			break;

		case '*':
			if (read_char(r) == '=') {
				token->type = TOK_MULT_ASSIGN;
			} else {
				back_up(r);
				token->type = TOK_ASTERISK;
			}

			break;

		case '/':
			switch (read_char(r)) {
			case '=':
				token->type = TOK_DIVIDE_ASSIGN;
				break;
			case '/':
				while (read_char(r) != '\n')
					;

				read_token = false;
				break;
			case '*':
				for (;;) {
					char c = read_char(r);
					if (c == '*') {
						if (read_char(r) == '/')
							break;

						back_up(r);
					}
				}

				read_token = false;
				break;
			default:
				back_up(r);
				token->type = TOK_DIVIDE;
			}

			break;

		case '%':
			if (read_char(r) == '=') {
				token->type = TOK_MOD_ASSIGN;
			} else {
				back_up(r);
				token->type = TOK_MOD;
			}

			break;

		case '&':
			switch (read_char(r)) {
			case '&':
				token->type = TOK_LOGICAL_AND;
				break;
			case '=':
				token->type = TOK_BIT_AND_ASSIGN;
				break;
			default:
				back_up(r);
				token->type = TOK_AMPERSAND;
			}

			break;

		case '|':
			switch (read_char(r)) {
			case '|':
				token->type = TOK_LOGICAL_OR;
				break;
			case '=':
				token->type = TOK_BIT_OR_ASSIGN;
				break;
			default:
				back_up(r);
				token->type = TOK_BIT_OR;
			}

			break;

		case '^':
			if (read_char(r) == '=') {
				token->type = TOK_BIT_XOR_ASSIGN;
			} else {
				back_up(r);
				token->type = TOK_BIT_XOR;
			}

			break;

		case '=':
			if (read_char(r) == '=') {
				token->type = TOK_EQUAL;
			} else {
				back_up(r);
				token->type = TOK_ASSIGN;
			}

			break;

		case '!':
			if (read_char(r) == '=') {
				token->type = TOK_NOT_EQUAL;
			} else {
				back_up(r);
				token->type = TOK_NOT;
			}

			break;

		case '<':
			switch (read_char(r)) {
			case '=':
				token->type = TOK_LESS_THAN_OR_EQUAL;
				break;
			case '<':
				if (read_char(r) == '=') {
					token->type = TOK_LEFT_SHIFT_ASSIGN;
				} else {
					back_up(r);
					token->type = TOK_LEFT_SHIFT;
				}
				break;
			default:
				back_up(r);
				token->type = TOK_LESS_THAN;
			}

			break;

		case '>':
			switch (read_char(r)) {
			case '=':
				token->type = TOK_GREATER_THAN_OR_EQUAL;
				break;
			case '>':
				if (read_char(r) == '=') {
					token->type = TOK_RIGHT_SHIFT_ASSIGN;
				} else {
					back_up(r);
					token->type = TOK_RIGHT_SHIFT;
				}
				break;
			default:
				back_up(r);
				token->type = TOK_GREATER_THAN;
			}

			break;

		case '#':
			if (read_char(r) == '#') {
				token->type = TOK_DOUBLE_HASH;
			} else {
				back_up(r);
				token->type = TOK_HASH;
			}

			break;

		case '.':
			if (read_char(r) == '.') {
				if (read_char(r) == '.') {
					token->type = TOK_ELLIPSIS;
				} else {
					back_up(r);
					back_up(r);
					token->type = TOK_DOT;
				}
			} else {
				back_up(r);
				token->type = TOK_DOT;
			}

			break;

		case '~': token->type = TOK_BIT_NOT; break;
		case '?': token->type = TOK_QUESTION_MARK; break;
		case ':': token->type = TOK_COLON; break;
		case ';': token->type = TOK_SEMICOLON; break;
		case ',': token->type = TOK_COMMA; break;

		case '{': token->type = TOK_LCURLY; break;
		case '}': token->type = TOK_RCURLY; break;
		case '(': token->type = TOK_LROUND; break;
		case ')': token->type = TOK_RROUND; break;
		case '[': token->type = TOK_LSQUARE; break;
		case ']': token->type = TOK_RSQUARE; break;

		case ' ': case '\t': case '\n':
			read_token = false;
			break;

		default: {
			u32 start_index = reader.position - 1;
			for (;;) {
				char c = peek_char(r);
				if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z')
							|| (c >= 'A' && c <= 'Z') || (c == '_')))
					break;

				advance(r);
			}

			u32 size = reader.position - start_index + 1;

			token->type = TOK_SYMBOL;
			char *str = malloc(size);
			strncpy(str, reader.buffer + start_index, size - 1);
			str[size - 1] = '\0';

			token->val.symbol_or_string_literal = str;
			break;
		}
		}

		assert(!read_token || token->type != TOK_INVALID);
	}

	if (!read_token)
		array_delete_last(tokens);
}
