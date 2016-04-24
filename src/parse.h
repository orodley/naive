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

#define AST_EXPR_TYPES \
		X(AST_INT_LITERAL), \
\
		X(AST_IDENTIFIER), \
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
		X(AST_MODULO_ASSIGN), \
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
		const char *identifier;
		struct ASTExpr *unary_arg;
		struct ASTTypeName *type;
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
			struct ASTTypeName *cast_type;
			struct ASTExpr *arg;
		} cast;
	} val;
} ASTExpr;

#define AST_STATEMENT_TYPES \
		X(AST_EMPTY_STATEMENT), \
		X(AST_LABELED_STATEMENT), \
		X(AST_CASE_STATEMENT), \
		X(AST_COMPOUND_STATEMENT), \
		X(AST_EXPR_STATEMENT), \
		X(AST_IF_STATEMENT), \
		X(AST_SWITCH_STATEMENT), \
		X(AST_WHILE_STATEMENT), \
		X(AST_DO_WHILE_STATEMENT), \
		X(AST_FOR_STATEMENT), \
		X(AST_GOTO_STATEMENT), \
		X(AST_CONTINUE_STATEMENT), \
		X(AST_BREAK_STATEMENT), \
		X(AST_RETURN_STATEMENT)

#define X(x) x
typedef enum ASTStatementType
{
	AST_STATEMENT_TYPES
} ASTStatementType;
#undef X

typedef struct ASTStatement
{
	ASTStatementType type;

	union
	{
		struct
		{
			const char *label_name;
			struct ASTStatement *statement;
		} labeled_statement;
		struct
		{
			ASTExpr *expr;
			struct ASTStatement *statement;
		} expr_and_statement;
		struct ASTBlockItem *block_items;
		struct
		{
			ASTExpr *condition;
			struct ASTStatement *then_statement;
			struct ASTStatement *else_statement;
		} if_statement;
		// @TODO: For loops with declarations.
		struct
		{
			ASTExpr *init_expr;
			ASTExpr *condition;
			ASTExpr *update_expr;
			struct ASTStatement *body;
		} for_statement;
		const char *goto_label;
		ASTExpr *expr;
	} val;
} ASTStatement;

typedef struct ASTBlockItem
{
	struct ASTBlockItem *next;

	enum
	{
		BLOCK_ITEM_DECL,
		BLOCK_ITEM_STATEMENT,
	} type;

	union 
	{
		struct ASTDecl *decl;
		ASTStatement *statement;
	} val;
} ASTBlockItem;


typedef struct ASTDesignator
{
	struct ASTDesignator *next;

	enum
	{
		INDEX_DESIGNATOR,
		FIELD_DESIGNATOR,
	} type;

	union
	{
		ASTExpr *index_expr;
		const char *field_name;
	} val;
} ASTDesignator;

typedef struct ASTInitializerElement
{
	struct ASTInitializerElement *next;

	ASTDesignator *designators;
	struct ASTInitializer *initializer;
} ASTInitializerElement;

typedef struct ASTInitializer
{
	enum
	{
		EXPR_INITIALIZER,
		BRACE_INITIALIZER,
	} type;

	union
	{
		ASTExpr *expr;
		ASTInitializerElement *initializer_elements;
	} val;
} ASTInitializer;

typedef struct ASTInitDeclarator
{
	struct ASTInitDeclarator *next;

	struct ASTDeclarator *declarator;
	ASTInitializer *initializer;
} ASTInitDeclarator;

typedef struct ASTDecl
{
	struct ASTDecl *next;

	struct ASTDeclSpecifier *decl_specifiers;
	ASTInitDeclarator *init_declarators;
} ASTDecl;

typedef struct ASTTypeName
{
	struct ASTDeclSpecifier *decl_specifiers;
	struct ASTDirectDeclarator *declarator;
} ASTTypeName;

typedef struct ASTParameterDecl
{
	struct ASTParameterDecl *next;

	struct ASTDeclSpecifier *decl_specifiers;
	struct ASTDeclarator *declarator;
} ASTParameterDecl;

typedef struct ASTDirectDeclarator
{
	enum
	{
		DECLARATOR,
		IDENTIFIER_DECLARATOR,
		ARRAY_DECLARATOR,
		FUNCTION_DECLARATOR,
	} type;

	union
	{
		const char *name;
		struct ASTDeclarator *declarator;
		struct
		{
			struct ASTDirectDeclarator *element_declarator;
			ASTExpr *array_length;
		} array_declarator;
		struct
		{
			struct ASTDirectDeclarator *declarator;
			ASTParameterDecl *parameters;
		} function_declarator;
	} val;
} ASTDirectDeclarator;

typedef struct ASTDeclarator
{
	enum
	{
		POINTER_DECLARATOR,
		DIRECT_DECLARATOR,
	} type;

	union
	{
		struct
		{
			struct ASTDeclSpecifier *decl_specifiers;
			struct ASTDeclarator *pointee;
		} pointer_declarator;
		ASTDirectDeclarator *direct_declarator;
	} val;
} ASTDeclarator;

typedef enum ASTStorageClassSpecifier
{
	TYPEDEF_SPECIFIER,
	EXTERN_SPECIFIER,
	STATIC_SPECIFIER,
	AUTO_SPECIFIER,
	REGISTER_SPECIFIER,
} ASTStorageClassSpecifier;

typedef enum ASTTypeQualifier
{
	CONST_QUALIFIER,
	RESTRICT_QUALIFIER,
	VOLATILE_QUALIFIER,
} ASTTypeQualifier;

typedef enum ASTFunctionSpecifier
{
	INLINE_SPECIFIER,
} ASTFunctionSpecifier;

typedef struct ASTEnumerator
{
	struct ASTEnumerator *next;
	const char *name;
	ASTExpr *value;
} ASTEnumerator;

typedef struct ASTFieldDeclarator
{
	struct ASTFieldDeclarator *next;

	enum
	{
		BITFIELD_FIELD_DECLARATOR,
		NORMAL_FIELD_DECLARATOR,
	} type;

	union
	{
		struct
		{
			ASTDeclarator *declarator;
			ASTExpr *width;
		} bitfield;
		ASTDeclarator *declarator;
	} val;
} ASTFieldDeclarator;

typedef struct ASTFieldDecl
{
	struct ASTFieldDecl *next;

	struct ASTDeclSpecifier *decl_specifiers;
	ASTFieldDeclarator *field_declarators;
} ASTFieldDecl;

typedef struct ASTTypeSpecifier
{
	enum
	{
		NAMED_TYPE_SPECIFIER,
		STRUCT_TYPE_SPECIFIER,
		UNION_TYPE_SPECIFIER,
		ENUM_TYPE_SPECIFIER,
	} type;

	union
	{
		const char *name;
		struct
		{
			const char *name;
			ASTFieldDecl *fields;
		} struct_or_union_specifier;
		struct
		{
			const char *name;
			ASTEnumerator *enumerators;
		} enum_specifier;
	} val;
} ASTTypeSpecifier;

typedef struct ASTDeclSpecifier
{
	struct ASTDeclSpecifier *next;
	enum
	{
		STORAGE_CLASS_SPECIFIER,
		TYPE_SPECIFIER,
		TYPE_QUALIFIER,
		FUNCTION_SPECIFIER,
	} type;

	union
	{
		ASTStorageClassSpecifier storage_class_specifier;
		ASTTypeSpecifier *type_specifier;
		ASTTypeQualifier type_qualifier;
		ASTFunctionSpecifier function_specifier;
	} val;
} ASTDeclSpecifier;

typedef struct ASTFunctionDef
{
	ASTDeclSpecifier *specifiers;
	ASTDeclarator *declarator;
	ASTDecl *old_style_param_decls;
	ASTStatement *body;
} ASTFunctionDef;

typedef struct ASTToplevel
{
	struct ASTToplevel *next;
	enum
	{
		FUNCTION_DEF,
		DECL,
	} type;

	union
	{
		ASTFunctionDef *function_def;
		ASTDecl *decl;
	} val;
} ASTToplevel;

void dump_toplevel(ASTToplevel *ast);
ASTToplevel *parse_toplevel(Array(SourceToken) *tokens, Pool *ast_pool);

#endif
