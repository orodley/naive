#ifndef NAIVE_TOKENISE_H
#define NAIVE_TOKENISE_H

#include "misc.h"
#include "array.h"

typedef enum TokenType
{
	TOK_INVALID,

	TOK_INT_LITERAL,
	TOK_FLOAT_LITERAL,
	TOK_STRING_LITERAL,
	TOK_SYMBOL,

	TOK_PLUS,
	TOK_INCREMENT,
	TOK_PLUS_ASSIGN,

	TOK_MINUS,
	TOK_DECREMENT,
	TOK_MINUS_ASSIGN,
	TOK_ARROW,

	TOK_ASTERISK,
	TOK_MULT_ASSIGN,

	TOK_DIVIDE,
	TOK_DIVIDE_ASSIGN,

	TOK_MOD,
	TOK_MOD_ASSIGN,

	TOK_AMPERSAND,
	TOK_LOGICAL_AND,
	TOK_BIT_AND_ASSIGN,

	TOK_BIT_OR,
	TOK_LOGICAL_OR,
	TOK_BIT_OR_ASSIGN,

	TOK_BIT_XOR,
	TOK_BIT_XOR_ASSIGN,

	TOK_ASSIGN,
	TOK_EQUAL,

	TOK_NOT,
	TOK_NOT_EQUAL,

	TOK_LESS_THAN,
	TOK_LESS_THAN_OR_EQUAL,
	TOK_LEFT_SHIFT,
	TOK_LEFT_SHIFT_ASSIGN,

	TOK_GREATER_THAN,
	TOK_GREATER_THAN_OR_EQUAL,
	TOK_RIGHT_SHIFT,
	TOK_RIGHT_SHIFT_ASSIGN,

	TOK_HASH,
	TOK_DOUBLE_HASH,

	TOK_DOT,
	TOK_ELLIPSIS,

	TOK_BIT_NOT,
	TOK_QUESTION_MARK,
	TOK_COLON,
	TOK_SEMICOLON,
	TOK_COMMA,

	TOK_LCURLY,
	TOK_RCURLY,
	TOK_LROUND,
	TOK_RROUND,
	TOK_LSQUARE,
	TOK_RSQUARE,
} TokenType;

typedef struct Token
{
	TokenType type;

	union
	{
		i64 int_literal;
		double float_literal;
		char *symbol_or_string_literal;
	} val;
} Token;

typedef struct SourceLoc
{
	const char *filename;
	u32 line;
	u32 column;
} SourceLoc;

typedef struct SourceToken
{
	// @NOTE: This is deliberately at the start, so that pointers to
	// SourceTokens can be treated as pointers to Tokens.
	Token token;
	SourceLoc source_loc;
} SourceToken;

void tokenise(Array(SourceToken) *tokens, const char *input_filename);

#endif
