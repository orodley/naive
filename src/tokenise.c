#include <stdio.h>
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

#define INVALID_INPUT_BUFFER ((InputBuffer) { NULL, -1 })

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

	return (InputBuffer) { buffer, file_size };
}


typedef struct Reader
{
	char *buffer;
	u32 position;
	u32 length;
} Reader;

static inline char read_char(Reader *reader)
{
	return reader->buffer[reader->position++];
}

static inline char peek_char(Reader *reader)
{
	return reader->buffer[reader->position];
}

static inline void advance(Reader *reader)
{
	reader->position++;
}

static inline void back_up(Reader *reader)
{
	reader->position--;
}

// TODO: Replace with a finite state machine
void tokenise(const char *input_filename)
{
	InputBuffer buffer = map_file_into_memory(input_filename);

	if (buffer.buffer == NULL)
		return;

	Reader reader = (Reader) { buffer.buffer, 0, (u32)buffer.length };
	Reader *r = &reader;

	while (reader.position < reader.length) {
		switch (read_char(r)) {
		case '0': case '1': case '2': case '3': case '4': case '5': case '6':
		case '7': case '8': case '9':
			for (;;) {
				char c = peek_char(r);
				if (!(c >= '0' && c <= '9'))
					break;

				advance(r);
			}

			puts("TOK_INT_LITERAL");
			break;

		case '"':
			for (;;) {
				char c = read_char(r);

				if (c == '\\')
					advance(r);
				else if (c == '"')
					break;
			}

			puts("TOK_STRING_LITERAL");
			break;

		case '+':
			switch (read_char(r)) {
			case '+':
				puts("TOK_INCREMENT");
				break;
			case '=':
				puts("TOK_PLUS_ASSIGN");
				break;
			default:
				back_up(r);
				puts("TOK_PLUS");
			}

			break;

		case '-':
			switch (read_char(r)) {
			case '-':
				puts("TOK_DECREMENT");
				break;
			case '=':
				puts("TOK_MINUS_ASSING");
				break;
			case '>':
				puts("TOK_ARROW");
				break;
			default:
				back_up(r);
				puts("TOK_MINUS");
			}

			break;

		case '*':
			if (read_char(r) == '=') {
				puts("TOK_MULT_ASSIGN");
			} else {
				back_up(r);
				puts("TOK_ASTERISK");
			}

			break;

		case '/':
			switch (read_char(r)) {
			case '=':
				puts("TOK_DIVIDE_ASSIGN");
				break;
			case '/':
				while (read_char(r) != '\n')
					;
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
				break;
			default:
				back_up(r);
				puts("TOK_DIVIDE");
			}

			break;

		case '%':
			if (read_char(r) == '=') {
				puts("TOK_MOD_ASSIGN");
			} else {
				back_up(r);
				puts("TOK_MOD");
			}

			break;

		case '&':
			switch (read_char(r)) {
			case '&':
				puts("TOK_LOGICAL_AND");
				break;
			case '=':
				puts("TOK_BIT_AND_ASSIGN");
				break;
			default:
				back_up(r);
				puts("TOK_AMPERSAND");
			}

			break;

		case '|':
			switch (read_char(r)) {
			case '|':
				puts("TOK_LOGICAL_OR");
				break;
			case '=':
				puts("TOK_BIT_OR_ASSIGN");
				break;
			default:
				back_up(r);
				puts("TOK_BIT_OR");
			}

			break;

		case '^':
			if (read_char(r) == '=') {
				puts("TOK_BIT_XOR_ASSIGN");
			} else {
				back_up(r);
				puts("TOK_BIT_XOR");
			}

			break;

		case '=':
			if (read_char(r) == '=') {
				puts("TOK_EQUAL");
			} else {
				back_up(r);
				puts("TOK_ASSIGN");
			}

			break;

		case '!':
			if (read_char(r) == '=') {
				puts("TOK_NOT_EQUAL");
			} else {
				back_up(r);
				puts("TOK_NOT");
			}

			break;

		case '<':
			switch (read_char(r)) {
			case '=':
				puts("TOK_LESS_THAN_OR_EQUAL");
				break;
			case '<':
				if (read_char(r) == '=') {
					puts("TOK_LEFT_SHIFT_ASSIGN");
				} else {
					back_up(r);
					puts("TOK_LEFT_SHIFT");
				}
				break;
			default:
				back_up(r);
				puts("TOK_LESS_THAN");
			}

			break;

		case '>':
			switch (read_char(r)) {
			case '=':
				puts("TOK_GREATER_THAN_OR_EQUAL");
				break;
			case '>':
				if (read_char(r) == '=') {
					puts("TOK_RIGHT_SHIFT_ASSIGN");
				} else {
					back_up(r);
					puts("TOK_RIGHT_SHIFT");
				}
				break;
			default:
				back_up(r);
				puts("TOK_GREATER_THAN");
			}

			break;

		case '#':
			if (read_char(r) == '#') {
				puts("TOK_DOUBLE_HASH");
			} else {
				back_up(r);
				puts("TOK_HASH");
			}

			break;

		case '.':
			if (read_char(r) == '.') {
				if (read_char(r) == '.') {
					puts("TOK_ELLIPSIS");
				} else {
					back_up(r);
					back_up(r);
					puts("TOK_DOT");
				}
			} else {
				back_up(r);
				puts("TOK_DOT");
			}

			break;

		case '~': puts("TOK_BIT_NOT"); break;
		case '?': puts("TOK_QUESTION_MARK"); break;
		case ':': puts("TOK_COLON"); break;
		case ';': puts("TOK_SEMICOLON"); break;
		case ',': puts("TOK_COMMA"); break;

		case '{': puts("TOK_LCURLY"); break;
		case '}': puts("TOK_RCURLY"); break;
		case '(': puts("TOK_LROUND"); break;
		case ')': puts("TOK_RROUND"); break;
		case '[': puts("TOK_LSQUARE"); break;
		case ']': puts("TOK_RSQUARE"); break;

		case ' ': case '\t': case '\n':
			  break;

		default:
			for (;;) {
				char c = peek_char(r);
				if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z')
							|| (c >= 'A' && c <= 'Z') || (c == '_')))
					break;

				advance(r);
			}

			puts("TOK_SYMBOL");
			break;
		}
	}
}
