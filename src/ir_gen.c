#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include "ir.h"
#include "parse.h"

#if 0
static void ir_gen_statement(Builder *builder, ASTStatement *statement);
static Value ir_gen_expression(Builder *builder, ASTExpr *expr);

static struct { char *name; IrType type; } primitive_types[] =
{
	{ "int", { .bit_width = 32 } },
};

static IrType look_up_type(ASTTypeName *type_name)
{
	for (u32 i = 0; i < STATIC_ARRAY_LENGTH(primitive_types); i++)
		if (streq(type_spec->name, primitive_types[i].name))
			return primitive_types[i].type;

	UNREACHABLE;
	IGNORE(type_name);
	return (IrType) { .bit_width = 32 };
}
#endif

void ir_gen_function(TransUnit *tu, Builder *builder, ASTFunctionDef *func)
{
#if 0
	Array(ASTVar *) *arguments = &ast->val.function_def.arguments;
	u32 arity = arguments->size;

	IrType *arg_types = malloc(sizeof(*arg_types) * arity);
	for (u32 i = 0; i < arity; i++) {
		ASTVar *var = *ARRAY_REF(arguments, ASTVar *, i);
		arg_types[i] = look_up_type(var->type);
	}

	IrType return_type = look_up_type(ast->val.function_def.return_type);

	IrFunction *f = trans_unit_add_function(tu, ast->val.function_def.name,
			return_type, arity, arg_types);
	builder->function = f;
	builder->current_block = &f->entry_block;

	ir_gen_statement(builder, ast->val.function_def.body);
#else
	IGNORE(tu);
	IGNORE(builder);
	IGNORE(func);
#endif
}

#if 0
static void ir_gen_statement(Builder *builder, ASTStatement *statement)
{
	switch (statement->type) {
	case AST_COMPOUND_STATEMENT: {
		ASTStatement **statements = statement->val.compound_statement.statements;
		for (u32 i = 0; i < statement->val.compound_statement.num_statements; i++) {
			ASTStatement *sub_statement = statements[i];

			ir_gen_statement(builder, sub_statement);
		}

		break;
	}
	case AST_RETURN_STATEMENT: {
		Value value = ir_gen_expression(builder, statement->val.expr);
		Block *ret_block = &builder->function->ret_block;
		build_branch(builder, ret_block, value);
		break;
	}
	default:
		UNIMPLEMENTED;
	}
	IGNORE(builder);
	IGNORE(statement);
}

static Value ir_gen_expression(Builder *builder, ASTExpr *expr)
{
	IGNORE(builder);

	switch (expr->type) {
	case AST_INT_LITERAL:
		return value_const(expr->val.int_literal);
	default:
		UNIMPLEMENTED;
	}
}
#endif
