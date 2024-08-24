#ifndef NAIVE_SYNTAX_PARSE_H_
#define NAIVE_SYNTAX_PARSE_H_

#include "array.h"
#include "pool.h"
#include "syntax/ast.h"
#include "syntax/lex.h"
#include "types.h"

bool parse_toplevel(
    Array(SourceToken) *tokens, Pool *ast_pool, ASTToplevel **toplevel);
bool parse_expr(Array(SourceToken) *tokens, Pool *ast_pool, ASTExpr **expr);

#endif
