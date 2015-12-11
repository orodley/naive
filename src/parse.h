#ifndef NAIVE_PARSE_H_
#define NAIVE_PARSE_H_

#include "array.h"
#include "misc.h"
#include "tokenise.h"

typedef struct ParseError
{
	Token *encountered_token;
	const char *expected;
} ParseError;

typedef struct ASTType
{
	const char *name;
} ASTType;

typedef struct ASTVar
{
	ASTType *type;
	char *name;
} ASTVar;

typedef struct ASTExpression
{
	enum
	{
		AST_INTEGER_LITERAL,
	} type;

	union
	{
		i64 integer_literal;
	} val;
} ASTExpression;

typedef struct ASTStatement
{
	enum
	{
		AST_COMPOUND_STATEMENT,
		AST_RETURN_STATEMENT,
	} type;

	union
	{
		Array statements;
		ASTExpression *return_value;
	} val;
} ASTStatement;

typedef struct ASTToplevel
{
	enum
	{
		AST_FUNCTION_DEF,
	} type;

	union
	{
		struct
		{
			ASTType *return_type;
			char *name;
			Array arguments;
			ASTStatement *body;
		} function_def;
	} val;
} ASTToplevel;

void dump_toplevel(ASTToplevel *ast);
ASTToplevel *parse_toplevel(Array *tokens);

#endif
