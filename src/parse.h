#ifndef NAIVE_PARSE_H_
#define NAIVE_PARSE_H_

#include "array.h"
#include "misc.h"
#include "tokenise.h"
#include "pool.h"

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

#define AST_EXPR_TYPES \
		X(AST_INT_LITERAL), \
\
		X(AST_STRUCT_DOT_FIELD), \
		X(AST_STRUCT_ARROW_FIELD), \
\
		X(AST_INDEX), \
		X(AST_POST_INCREMENT), \
		X(AST_POST_DECREMENT), \
\
		X(AST_PRE_INCREMENT), \
		X(AST_PRE_DECREMENT), \
		X(AST_ADDRESS_OF), \
		X(AST_DEREF), \
		X(AST_UNARY_PLUS), \
		X(AST_UNARY_MINUS), \
		X(AST_BIT_NOT), \
		X(AST_LOGICAL_NOT), \
\
		X(AST_CAST), \
		X(AST_SIZEOF_EXPR), \
		X(AST_SIZEOF_TYPE), \
\
		X(AST_MULTIPLY), \
		X(AST_DIVIDE), \
		X(AST_MODULO), \
		X(AST_ADD), \
		X(AST_MINUS), \
		X(AST_LEFT_SHIFT), \
		X(AST_RIGHT_SHIFT), \
\
		X(AST_LESS_THAN), \
		X(AST_GREATER_THAN), \
		X(AST_LESS_THAN_OR_EQUAL), \
		X(AST_GREATER_THAN_OR_EQUAL), \
		X(AST_EQUAL), \
		X(AST_NOT_EQUAL), \
\
		X(AST_BIT_AND), \
		X(AST_BIT_XOR), \
		X(AST_BIT_OR), \
\
		X(AST_LOGICAL_AND), \
		X(AST_LOGICAL_OR), \
\
		X(AST_CONDITIONAL), \
\
		X(AST_ASSIGN), \
		X(AST_MULT_ASSIGN), \
		X(AST_DIVIDE_ASSIGN), \
		X(AST_MOD_ASSIGN), \
		X(AST_PLUS_ASSIGN), \
		X(AST_MINUS_ASSIGN), \
		X(AST_LEFT_SHIFT_ASSIGN), \
		X(AST_RIGHT_SHIFT_ASSIGN), \
		X(AST_BIT_AND_ASSIGN), \
		X(AST_BIT_XOR_ASSIGN), \
		X(AST_BIT_OR_ASSIGN),

#define X(x) x
typedef enum ASTExprType
{
	AST_EXPR_TYPES
} ASTExprType;
#undef X


typedef struct ASTExpr
{
	ASTExprType type;

	union
	{
		i64 int_literal;
		struct ASTExpr *unary_arg;
		ASTType *type;
		struct
		{
			struct ASTExpr *arg1;
			struct ASTExpr *arg2;
		} binary_op;
		struct
		{
			struct ASTExpr *arg1;
			struct ASTExpr *arg2;
			struct ASTExpr *arg3;
		} ternary_op;
		struct
		{
			struct ASTExpr *struct_value;
			const char *field_name;
		} struct_field;
		struct
		{
			ASTType *cast_type;
			struct ASTExpr *arg;
		} cast;
	} val;
} ASTExpr;

typedef struct ASTStatement
{
	enum
	{
		AST_COMPOUND_STATEMENT,
		AST_RETURN_STATEMENT,
	} type;

	union
	{
		Array(ASTStatement *) statements;
		ASTExpr *return_value;
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
			Array(ASTVar *) arguments;
			ASTStatement *body;
		} function_def;
	} val;
} ASTToplevel;

void dump_toplevel(ASTToplevel *ast);
ASTToplevel *parse_toplevel(Array(SourceToken) *tokens, Pool *ast_pool);

#endif
