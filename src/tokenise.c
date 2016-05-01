#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "array.h"
#include "diagnostics.h"
#include "misc.h"
#include "tokenise.h"

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
static InputBuffer map_file_into_memory(const char *filename)
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

// @PORT
static void unmap_file(InputBuffer buffer)
{
	int ret = munmap(buffer.buffer, buffer.length);
	assert(ret == 0);
}


typedef struct Reader
{
	Array(SourceToken) *tokens;

	InputBuffer buffer;
	u32 position;

	SourceLoc source_loc;
	bool first_token_of_line;
} Reader;

static inline bool at_end(Reader *reader) {
	return reader->position >= reader->buffer.length;
}

static inline char peek_char(Reader *reader)
{
	return reader->buffer.buffer[reader->position];
}

static inline void next_char(Reader *reader)
{
	if (peek_char(reader) == '\n') {
		reader->source_loc.line++;
		reader->source_loc.column = 1;
	} else {
		reader->source_loc.column++;
	}

	reader->position++;
}

static inline void back_up(Reader *reader)
{
	// We should never need to back up over a newline. We assert that we don't
	// because if we did we'd need to keep track of the previous lines length
	// to preserve source information.
	assert(peek_char(reader) != '\n');

	reader->source_loc.column--;
	reader->position--;
}

static void advance(Reader *reader)
{
	for (;;) {
		next_char(reader);
		if (peek_char(reader) == '\\') {
			next_char(reader);
			if (peek_char(reader) == '\n') {
				continue;
			} else {
				back_up(reader);
				break;
			}
		} else {
			break;
		}
	}
}

static inline char read_char(Reader *reader)
{
	char c = peek_char(reader);
	advance(reader);

	return c;
}

static void skip_whitespace_and_comments(Reader *reader, bool skip_newline)
{
	while (!at_end(reader)) {
		switch (peek_char(reader)) {
		case '\n':
			reader->first_token_of_line = true;
			if (skip_newline)
				break;
			else
				return;
		case ' ': case '\t':
			break;
		case '/':
			advance(reader);
			switch (peek_char(reader)) {
			case '/':
				while (peek_char(reader) != '\n' && !at_end(reader)) {
					advance(reader);
				}
				break;
			case '*':
				while (!at_end(reader)) {
					if (read_char(reader) == '*') {
						if (read_char(reader) == '/')
							break;

						back_up(reader);
					}
				}

				if (at_end(reader))
					issue_error(&reader->source_loc, "Unterminated /* comment");
				break;
			default:
				back_up(reader);
				return;
			}
			break;
		default:
			return;
		}

		advance(reader);
	}
}


static Token *append_token(Reader *reader, TokenType type)
{
	SourceToken *source_token = ARRAY_APPEND(reader->tokens, SourceToken);
	source_token->token.type = TOK_INVALID;
	source_token->source_loc = reader->source_loc;

	Token *token = (Token *)source_token;
	token->type = type;
	return token;
}

static void handle_pp_directive(Reader *reader);

// @IMPROVE: Replace with a finite state machine
void tokenise(Array(SourceToken) *tokens, const char *input_filename)
{
	InputBuffer buffer = map_file_into_memory(input_filename);

	if (buffer.buffer == NULL)
		return;

	// @TUNE: 500 tokens is a quick estimate of a reasonable minimum. We should
	// do some more thorough measurement and determine a good value for this.
	ARRAY_INIT(tokens, SourceToken, 500);

	Reader reader = {
		tokens,
		buffer,
		0,
		(SourceLoc) { input_filename, 1, 1, },
		true
	};
	Reader *r = &reader;

	while (!at_end(r)) {
		skip_whitespace_and_comments(r, true);
		if (at_end(r))
			break;

		switch (read_char(r)) {
		case '0': case '1': case '2': case '3': case '4': case '5': case '6':
		case '7': case '8': case '9': {
			back_up(r);
			i64 value = 0;

			for (;;) {
				char c = peek_char(r);
				if (!(c >= '0' && c <= '9'))
					break;

				value *= 10;
				value += c - '0';
				advance(r);
			}

			Token *token = append_token(r, TOK_INT_LITERAL);
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

			Token *token = append_token(r, TOK_STRING_LITERAL);
			char *str = malloc(size);
			strncpy(str, reader.buffer.buffer + start_index, size - 1);
			str[size - 1] = '\0';

			token->val.symbol_or_string_literal = str;

			break;
		}
		case '+':
			switch (read_char(r)) {
			case '+':
				append_token(r, TOK_INCREMENT);
				break;
			case '=':
				append_token(r, TOK_PLUS_ASSIGN);
				break;
			default:
				back_up(r);
				append_token(r, TOK_PLUS);
			}

			break;

		case '-':
			switch (read_char(r)) {
			case '-':
				append_token(r, TOK_DECREMENT);
				break;
			case '=':
				append_token(r, TOK_MINUS_ASSIGN);
				break;
			case '>':
				append_token(r, TOK_ARROW);
				break;
			default:
				back_up(r);
				append_token(r, TOK_MINUS);
			}

			break;

		case '*':
			if (read_char(r) == '=') {
				append_token(r, TOK_MULT_ASSIGN);
			} else {
				back_up(r);
				append_token(r, TOK_ASTERISK);
			}

			break;

		case '/':
			if (read_char(r) == '=') {
				append_token(r, TOK_DIVIDE_ASSIGN);
			} else {
				back_up(r);
				append_token(r, TOK_DIVIDE);
			}

			break;

		case '%':
			if (read_char(r) == '=') {
				append_token(r, TOK_MODULO_ASSIGN);
			} else {
				back_up(r);
				append_token(r, TOK_MODULO);
			}

			break;

		case '&':
			switch (read_char(r)) {
			case '&':
				append_token(r, TOK_LOGICAL_AND);
				break;
			case '=':
				append_token(r, TOK_BIT_AND_ASSIGN);
				break;
			default:
				back_up(r);
				append_token(r, TOK_AMPERSAND);
			}

			break;

		case '|':
			switch (read_char(r)) {
			case '|':
				append_token(r, TOK_LOGICAL_OR);
				break;
			case '=':
				append_token(r, TOK_BIT_OR_ASSIGN);
				break;
			default:
				back_up(r);
				append_token(r, TOK_BIT_OR);
			}

			break;

		case '^':
			if (read_char(r) == '=') {
				append_token(r, TOK_BIT_XOR_ASSIGN);
			} else {
				back_up(r);
				append_token(r, TOK_BIT_XOR);
			}

			break;

		case '=':
			if (read_char(r) == '=') {
				append_token(r, TOK_EQUAL);
			} else {
				back_up(r);
				append_token(r, TOK_ASSIGN);
			}

			break;

		case '!':
			if (read_char(r) == '=') {
				append_token(r, TOK_NOT_EQUAL);
			} else {
				back_up(r);
				append_token(r, TOK_LOGICAL_NOT);
			}

			break;

		case '<':
			switch (read_char(r)) {
			case '=':
				append_token(r, TOK_LESS_THAN_OR_EQUAL);
				break;
			case '<':
				if (read_char(r) == '=') {
					append_token(r, TOK_LEFT_SHIFT_ASSIGN);
				} else {
					back_up(r);
					append_token(r, TOK_LEFT_SHIFT);
				}
				break;
			default:
				back_up(r);
				append_token(r, TOK_LESS_THAN);
			}

			break;

		case '>':
			switch (read_char(r)) {
			case '=':
				append_token(r, TOK_GREATER_THAN_OR_EQUAL);
				break;
			case '>':
				if (read_char(r) == '=') {
					append_token(r, TOK_RIGHT_SHIFT_ASSIGN);
				} else {
					back_up(r);
					append_token(r, TOK_RIGHT_SHIFT);
				}
				break;
			default:
				back_up(r);
				append_token(r, TOK_GREATER_THAN);
			}

			break;

		case '#':
			if (read_char(r) == '#') {
				append_token(r, TOK_DOUBLE_HASH);
			} else {
				back_up(r);

				if (!reader.first_token_of_line) {
					issue_error(&reader.source_loc,
							"Unexpected preprocessor directive");
				} else {
					handle_pp_directive(r);
				}
			}

			break;

		case '.':
			if (read_char(r) == '.') {
				if (read_char(r) == '.') {
					append_token(r, TOK_ELLIPSIS);
				} else {
					back_up(r);
					back_up(r);
					append_token(r, TOK_DOT);
				}
			} else {
				back_up(r);
				append_token(r, TOK_DOT);
			}

			break;

		case '~': append_token(r, TOK_BIT_NOT); break;
		case '?': append_token(r, TOK_QUESTION_MARK); break;
		case ':': append_token(r, TOK_COLON); break;
		case ';': append_token(r, TOK_SEMICOLON); break;
		case ',': append_token(r, TOK_COMMA); break;

		case '{': append_token(r, TOK_LCURLY); break;
		case '}': append_token(r, TOK_RCURLY); break;
		case '(': append_token(r, TOK_LROUND); break;
		case ')': append_token(r, TOK_RROUND); break;
		case '[': append_token(r, TOK_LSQUARE); break;
		case ']': append_token(r, TOK_RSQUARE); break;

		case '\n': case ' ': case '\t':
			// skip_whitespace_and_comments should have moved us past these
			UNREACHABLE;

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

			Token *token = append_token(r, TOK_SYMBOL);
			char *str = malloc(size);
			strncpy(str, reader.buffer.buffer + start_index, size - 1);
			str[size - 1] = '\0';

			token->val.symbol_or_string_literal = str;
			break;
		}
		}

		reader.first_token_of_line = false;
	}

	unmap_file(buffer);
}


static void handle_pp_directive(Reader *reader)
{
	IGNORE(reader);
	SourceToken directive;

	// @TODO: Preprocessing tokens are distinct from regular tokens, but there
	// is a lot of overlap. The overlap should be factored out, and then used
	// to read both types of tokens. For now we just pretend they're the same.
	//read_token(reader, &directive);

	if (directive.token.type != TOK_SYMBOL) {
		// @TODO: Maybe we should skip to the next newline or something to try
		// to re-sync when this happens?
		issue_error(&directive.source_loc, "Expected identifier");
		return;
	}

	char *name = directive.token.val.symbol_or_string_literal;
	if (strcmp(name, "include") == 0) {

	}
}


#define X(x) #x
const char *token_type_names[] = {
	TOKEN_TYPES
};
#undef X

void dump_token(Token *token)
{
	fputs(token_type_names[token->type], stdout);
	switch (token->type) {
	case TOK_INT_LITERAL:
		printf("(%" PRId64 ")", token->val.int_literal);
		break;
	case TOK_FLOAT_LITERAL:
		printf("(%lf)", token->val.float_literal);
		break;
	case TOK_STRING_LITERAL:
		// @TODO: Escape the resulting string
		printf("(\"%s\")", token->val.symbol_or_string_literal);
		break;
	case TOK_SYMBOL:
		printf("(%s)", token->val.symbol_or_string_literal);
		break;
	default:
		break;
	}
}
