#include "ir.h"
#include "parse.h"

static void ir_gen_statement(Builder *builder, ASTStatement *statement);
static Instr *ir_gen_expression(Builder *builder, ASTExpression *expr);

void ir_gen_function(TransUnit *tu, Builder *builder, ASTToplevel *ast)
{
	Function *f = trans_unit_add_function(
			tu, ast->val.function_def.name, ast->val.function_def.arguments.size);
	builder->function = f;
	builder->current_block = &f->entry_block;

	ir_gen_statement(builder, ast->val.function_def.body);
}

static void ir_gen_statement(Builder *builder, ASTStatement *statement)
{
	switch (statement->type) {
	case AST_COMPOUND_STATEMENT: {
		Array *statements = &statement->val.statements;
		for (u32 i = 0; i < statements->size; i++) {
			ASTStatement *sub_statement =
				*(ASTStatement **)array_ref(statements, i);

			ir_gen_statement(builder, sub_statement);
		}

		break;
	}

	case AST_RETURN_STATEMENT: {
		Instr *value = ir_gen_expression(builder, statement->val.return_value);
		Block *ret_block = &builder->function->ret_block;
		build_branch(builder, ret_block, value);
		break;
	}
	}
}

static Instr *ir_gen_expression(Builder *builder, ASTExpression *expr)
{
	UNUSED(builder);

	switch (expr->type) {
	case AST_INTEGER_LITERAL:
		return build_const(builder, expr->val.integer_literal);
	}
}
