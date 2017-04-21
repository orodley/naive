#ifndef NAIVE_TOKENISE_H
#define NAIVE_TOKENISE_H

#include "misc.h"
#include "array.h"

#define TOKEN_TYPES \
	X(TOK_INVALID), \
\
	X(TOK_INT_LITERAL), \
	X(TOK_STRING_LITERAL), \
	X(TOK_SYMBOL), \
\
	X(TOK_PLUS), \
	X(TOK_INCREMENT), \
	X(TOK_PLUS_ASSIGN), \
\
	X(TOK_MINUS), \
	X(TOK_DECREMENT), \
	X(TOK_MINUS_ASSIGN), \
	X(TOK_ARROW), \
\
	X(TOK_ASTERISK), \
	X(TOK_MULTIPLY_ASSIGN), \
\
	X(TOK_DIVIDE), \
	X(TOK_DIVIDE_ASSIGN), \
\
	X(TOK_MODULO), \
	X(TOK_MODULO_ASSIGN), \
\
	X(TOK_AMPERSAND), \
	X(TOK_LOGICAL_AND), \
	X(TOK_BIT_AND_ASSIGN), \
\
	X(TOK_BIT_OR), \
	X(TOK_LOGICAL_OR), \
	X(TOK_BIT_OR_ASSIGN), \
\
	X(TOK_BIT_XOR), \
	X(TOK_BIT_XOR_ASSIGN), \
\
	X(TOK_ASSIGN), \
	X(TOK_EQUAL), \
\
	X(TOK_LOGICAL_NOT), \
	X(TOK_NOT_EQUAL), \
\
	X(TOK_LESS_THAN), \
	X(TOK_LESS_THAN_OR_EQUAL), \
	X(TOK_LEFT_SHIFT), \
	X(TOK_LEFT_SHIFT_ASSIGN), \
\
	X(TOK_GREATER_THAN), \
	X(TOK_GREATER_THAN_OR_EQUAL), \
	X(TOK_RIGHT_SHIFT), \
	X(TOK_RIGHT_SHIFT_ASSIGN), \
\
	X(TOK_DOUBLE_HASH), \
\
	X(TOK_DOT), \
	X(TOK_ELLIPSIS), \
\
	X(TOK_BIT_NOT), \
	X(TOK_QUESTION_MARK), \
	X(TOK_COLON), \
	X(TOK_SEMICOLON), \
	X(TOK_COMMA), \
\
	X(TOK_LCURLY), \
	X(TOK_RCURLY), \
	X(TOK_LROUND), \
	X(TOK_RROUND), \
	X(TOK_LSQUARE), \
	X(TOK_RSQUARE), \

#define X(x) x
typedef enum TokenType
{
	TOKEN_TYPES
} TokenType;
#undef X

typedef struct Token
{
	TokenType t;

	union
	{
		u64 int_literal;
		char *symbol;
		char *string_literal;
	} u;
} Token;

typedef struct SourceLoc
{
	char *filename;
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

extern char *token_type_names[];

bool tokenise(Array(SourceToken) *tokens, char *input_filename);
void dump_token(Token *token);

#endif
