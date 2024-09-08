#include "syntax/parse.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "array.h"
#include "assertions.h"
#include "diagnostics.h"
#include "macros.h"
#include "pool.h"
#include "syntax/ast.h"
#include "syntax/lex.h"
#include "types.h"

typedef struct TypeTableEntry
{
  String type_name;
} TypeTableEntry;

typedef struct TypeTable
{
  Array(TypeTableEntry) entries;
} TypeTable;

static String builtin_types[] = {
    LS("void"),     LS("char"),  LS("short"),    LS("int"),
    LS("long"),     LS("float"), LS("double"),   LS("signed"),
    LS("unsigned"), LS("_Bool"), LS("_Complex"),
};

static void type_table_add_entry(TypeTable *table, TypeTableEntry entry)
{
  *ARRAY_APPEND(&table->entries, TypeTableEntry) = entry;
}

static void type_table_init(TypeTable *type_table)
{
  ARRAY_INIT(
      &type_table->entries, TypeTableEntry, STATIC_ARRAY_LENGTH(builtin_types));
  for (u32 i = 0; i < STATIC_ARRAY_LENGTH(builtin_types); i++) {
    TypeTableEntry entry = {.type_name = builtin_types[i]};
    type_table_add_entry(type_table, entry);
  }
}

static void type_table_free(TypeTable *type_table)
{
  array_free(&type_table->entries);
}

static bool type_table_look_up_name(
    TypeTable *type_table, String name, TypeTableEntry *out)
{
  for (u32 i = 0; i < type_table->entries.size; i++) {
    TypeTableEntry *entry = ARRAY_REF(&type_table->entries, TypeTableEntry, i);
    if (string_eq(entry->type_name, name)) {
      *out = *entry;
      return true;
    }
  }

  return false;
}

typedef struct Parser
{
  Pool *pool;

  Array(SourceToken) *tokens;
  u32 position;

  TypeTable defined_types;
} Parser;

// @TODO: Move the functions in this file that are only used by generated code.
// They could either just be directly in the header produced by peg.py, or in
// a separate "support" file which is #included by the header.

static Token *read_token(Parser *parser)
{
  SourceToken *token = ARRAY_REF(parser->tokens, SourceToken, parser->position);
  parser->position++;

  return (Token *)token;
}

static void back_up(Parser *parser) { parser->position--; }

typedef struct ParserResult
{
  void *result;
  bool success;
} ParserResult;

static ParserResult success(void *result)
{
  return (ParserResult){.result = result, .success = true};
}

static ParserResult failure = {.result = NULL, .success = false};

static ParserResult revert(Parser *parser, u32 position)
{
  parser->position = position;
  return failure;
}

static Token *current_token(Parser *parser)
{
  return (Token *)ARRAY_REF(parser->tokens, SourceToken, parser->position);
}

static SourceLoc *token_context(Token *token)
{
  return &((SourceToken *)token)->source_range.start;
}

typedef struct WhichResult
{
  u32 which;
  void *result;
} WhichResult;

static void *middle(Parser *parser, void *a, void *b, void *c)
{
  IGNORE(parser);
  IGNORE(a);
  IGNORE(c);

  return b;
}

static void *first(Parser *parser, void *a, void *b)
{
  IGNORE(parser);
  IGNORE(b);

  return a;
}

static void *second(Parser *parser, void *a, void *b)
{
  IGNORE(parser);
  IGNORE(a);

  return b;
}

static String direct_declarator_name(ASTDirectDeclarator *declarator);

static String declarator_name(ASTDeclarator *declarator)
{
  switch (declarator->t) {
  case POINTER_DECLARATOR:
    return declarator_name(declarator->u.pointer_declarator.pointee);
  case DIRECT_DECLARATOR:
    return direct_declarator_name(declarator->u.direct_declarator);
  }

  UNREACHABLE;
}

static String direct_declarator_name(ASTDirectDeclarator *declarator)
{
  switch (declarator->t) {
  case IDENTIFIER_DECLARATOR: return declarator->u.name;
  case ARRAY_DECLARATOR:
    return direct_declarator_name(
        declarator->u.array_declarator.element_declarator);
  case FUNCTION_DECLARATOR:
    return direct_declarator_name(declarator->u.function_declarator.declarator);
  case DECLARATOR: return declarator_name(declarator->u.declarator);
  }

  UNREACHABLE;
}

ASTDecl *build_decl(
    Parser *parser, ASTDeclSpecifier *decl_specifier_list,
    ASTInitDeclarator *init_declarator_list, Token *semi)
{
  IGNORE(semi);

  ASTDecl *decl = pool_alloc(parser->pool, sizeof *decl);
  decl->decl_specifier_list = decl_specifier_list;
  decl->init_declarators = init_declarator_list;
  decl->next = NULL;

  while (decl_specifier_list != NULL) {
    if (decl_specifier_list->t == STORAGE_CLASS_SPECIFIER
        && decl_specifier_list->u.storage_class_specifier
               == TYPEDEF_SPECIFIER) {
      while (init_declarator_list != NULL) {
        TypeTableEntry entry = {
            .type_name = declarator_name(init_declarator_list->declarator)};
        type_table_add_entry(&parser->defined_types, entry);

        init_declarator_list = init_declarator_list->next;
      }

      break;
    }

    decl_specifier_list = decl_specifier_list->next;
  }

  return decl;
}

static ASTExpr *build_constant(Parser *parser, Token *token)
{
  ASTExpr *expr = pool_alloc(parser->pool, sizeof *expr);
  switch (token->t) {
  case TOK_INT_LITERAL:
    expr->t = INT_LITERAL_EXPR;
    expr->u.int_literal = token->u.int_literal;
    break;
  case TOK_FLOAT_LITERAL:
    expr->t = FLOAT_LITERAL_EXPR;
    expr->u.float_literal = token->u.float_literal;
    break;
  case TOK_STRING_LITERAL:
    expr->t = STRING_LITERAL_EXPR;
    expr->u.string_literal = token->u.string_literal;
    break;
  default: UNREACHABLE;
  }

  return expr;
}

static ASTExpr *build_postfix_expr(
    Parser *parser, ASTExpr *curr, WhichResult *which)
{
  ASTExpr *next = pool_alloc(parser->pool, sizeof *next);
  switch (which->which) {
  case 0:
    next->t = INDEX_EXPR;
    next->u.binary_op.arg1 = curr;
    next->u.binary_op.arg2 = which->result;
    return next;
  case 1:
    next->t = FUNCTION_CALL_EXPR;
    next->u.function_call.callee = curr;
    next->u.function_call.arg_list = which->result;
    return next;
  case 2:
    next->t = STRUCT_DOT_FIELD_EXPR;
    next->u.struct_field.struct_expr = curr;
    next->u.struct_field.field_name = ((Token *)which->result)->u.symbol;
    return next;
  case 3:
    next->t = STRUCT_ARROW_FIELD_EXPR;
    next->u.struct_field.struct_expr = curr;
    next->u.struct_field.field_name = ((Token *)which->result)->u.symbol;
    return next;
  case 4:
    next->t = POST_INCREMENT_EXPR;
    next->u.unary_arg = curr;
    return next;
  case 5:
    next->t = POST_DECREMENT_EXPR;
    next->u.unary_arg = curr;
    return next;
  default: UNREACHABLE;
  }

  return NULL;
}

static ASTExpr *build_unary_expr(Parser *parser, Token *token, ASTExpr *arg)
{
  ASTExpr *next = pool_alloc(parser->pool, sizeof *next);
  next->u.unary_arg = arg;
  switch (token->t) {
  case TOK_INCREMENT: next->t = PRE_INCREMENT_EXPR; break;
  case TOK_DECREMENT: next->t = PRE_DECREMENT_EXPR; break;
  case TOK_AMPERSAND: next->t = ADDRESS_OF_EXPR; break;
  case TOK_ASTERISK: next->t = DEREF_EXPR; break;
  case TOK_PLUS: next->t = UNARY_PLUS_EXPR; break;
  case TOK_MINUS: next->t = UNARY_MINUS_EXPR; break;
  case TOK_BIT_NOT: next->t = BIT_NOT_EXPR; break;
  case TOK_LOGICAL_NOT: next->t = LOGICAL_NOT_EXPR; break;
  default: UNREACHABLE;
  }

  return next;
}

typedef struct BinaryTail
{
  Token *operator;
  ASTExpr *tail_expr;
} BinaryTail;

#define CASE2(token, ast_type) \
  case TOK_##token:            \
    expr->t = ast_type##_EXPR; \
    break;
#define CASE1(operator) CASE2(operator, operator)

static ASTExpr *build_binary_head(
    Parser *parser, ASTExpr *curr, BinaryTail *tail)
{
  ASTExpr *expr = pool_alloc(parser->pool, sizeof *expr);
  expr->u.binary_op.arg1 = curr;
  expr->u.binary_op.arg2 = tail->tail_expr;

  switch (tail->operator->t) {
    CASE2(ASTERISK, MULTIPLY)
    CASE2(PLUS_ASSIGN, ADD_ASSIGN)
    CASE1(DIVIDE)
    CASE1(MODULO)
    CASE2(PLUS, ADD)
    CASE1(MINUS)
    CASE1(LEFT_SHIFT)
    CASE1(RIGHT_SHIFT)
    CASE1(LESS_THAN)
    CASE1(GREATER_THAN)
    CASE1(LESS_THAN_OR_EQUAL)
    CASE1(GREATER_THAN_OR_EQUAL)
    CASE1(EQUAL)
    CASE1(NOT_EQUAL)
    CASE2(AMPERSAND, BIT_AND)
    CASE1(BIT_XOR)
    CASE1(BIT_OR)
    CASE1(LOGICAL_AND)
    CASE1(LOGICAL_OR)
    CASE1(ASSIGN)
    CASE1(MULTIPLY_ASSIGN)
    CASE1(DIVIDE_ASSIGN)
    CASE1(MODULO_ASSIGN)
    CASE1(MINUS_ASSIGN)
    CASE1(LEFT_SHIFT_ASSIGN)
    CASE1(RIGHT_SHIFT_ASSIGN)
    CASE1(BIT_AND_ASSIGN)
    CASE1(BIT_XOR_ASSIGN)
    CASE1(BIT_OR_ASSIGN)
    CASE1(COMMA)

  default: UNREACHABLE;
  }

  return expr;
}

#undef CASE1
#undef CASE2

ASTBlockItem *build_block_item(Parser *parser, WhichResult *decl_or_statement)
{
  ASTBlockItem *result = pool_alloc(parser->pool, sizeof *result);
  switch (decl_or_statement->which) {
  case 0:
    result->t = BLOCK_ITEM_DECL;
    result->u.decl = decl_or_statement->result;
    break;
  case 1:
    result->t = BLOCK_ITEM_STATEMENT;
    result->u.statement = decl_or_statement->result;
    break;
  default: UNREACHABLE;
  }

  return result;
}

ASTStatement *build_expr_statement(
    Parser *parser, ASTExpr *opt_expr, Token *semicolon)
{
  IGNORE(semicolon);

  ASTStatement *statement = pool_alloc(parser->pool, sizeof *statement);
  if (opt_expr == NULL) {
    statement->t = EMPTY_STATEMENT;
    return statement;
  }

  statement->t = EXPR_STATEMENT;
  statement->u.expr = opt_expr;
  return statement;
}

static ASTToplevel *build_toplevel(
    Parser *parser, WhichResult *function_def_or_decl)
{
  ASTToplevel *toplevel = pool_alloc(parser->pool, sizeof *toplevel);
  switch (function_def_or_decl->which) {
  case 0:
    toplevel->t = FUNCTION_DEF;
    toplevel->u.function_def = function_def_or_decl->result;
    break;
  case 1:
    toplevel->t = DECL;
    toplevel->u.decl = function_def_or_decl->result;
    break;
  default: UNREACHABLE;
  }

  return toplevel;
}

static ASTDeclSpecifier *build_storage_class_specifier(
    Parser *parser, WhichResult *keyword)
{
  ASTDeclSpecifier *result = pool_alloc(parser->pool, sizeof *result);
  result->t = STORAGE_CLASS_SPECIFIER;

  ASTStorageClassSpecifier specifier;
  switch (keyword->which) {
  case 0: specifier = TYPEDEF_SPECIFIER; break;
  case 1: specifier = EXTERN_SPECIFIER; break;
  case 2: specifier = STATIC_SPECIFIER; break;
  case 3: specifier = AUTO_SPECIFIER; break;
  case 4: specifier = REGISTER_SPECIFIER; break;
  default: UNREACHABLE;
  }
  result->u.storage_class_specifier = specifier;

  return result;
}

static ASTDeclSpecifier *build_type_qualifier(
    Parser *parser, WhichResult *keyword)
{
  ASTDeclSpecifier *result = pool_alloc(parser->pool, sizeof *result);
  result->t = TYPE_QUALIFIER;

  ASTTypeQualifier qualifier;
  switch (keyword->which) {
  case 0: qualifier = CONST_QUALIFIER; break;
  case 1: qualifier = RESTRICT_QUALIFIER; break;
  case 2: qualifier = VOLATILE_QUALIFIER; break;
  default: UNREACHABLE;
  }
  result->u.type_qualifier = qualifier;

  return result;
}

static ParserResult named_type(Parser *parser)
{
  if (parser->position >= parser->tokens->size) return failure;

  Token *token = read_token(parser);
  if (token->t != TOK_SYMBOL) {
    back_up(parser);
    return failure;
  }

  String name = token->u.symbol;
  TypeTableEntry entry;
  if (!type_table_look_up_name(&parser->defined_types, name, &entry)) {
    back_up(parser);
    return failure;
  }

  return success(token);
}

ASTTypeSpecifier *build_struct_or_union_tagged_named_type(
    Parser *parser, WhichResult *keyword, Token *name)
{
  ASTTypeSpecifier *tagged_type = pool_alloc(parser->pool, sizeof *tagged_type);
  tagged_type->t =
      keyword->which == 0 ? STRUCT_TYPE_SPECIFIER : UNION_TYPE_SPECIFIER;
  tagged_type->u.struct_or_union_specifier.name = name->u.symbol;
  tagged_type->u.struct_or_union_specifier.field_list = NULL;
  tagged_type->u.struct_or_union_specifier.attribute = NULL;

  return tagged_type;
}

ASTTypeSpecifier *build_struct_or_union(
    Parser *parser, WhichResult *keyword, Token *opt_name, Token *lcurly,
    ASTFieldDecl *field_list, Token *rcurly, ASTAttribute *attribute)
{
  IGNORE(lcurly);
  IGNORE(rcurly);

  ASTTypeSpecifier *result = pool_alloc(parser->pool, sizeof *result);
  result->t =
      keyword->which == 0 ? STRUCT_TYPE_SPECIFIER : UNION_TYPE_SPECIFIER;
  if (opt_name == NULL) {
    result->u.struct_or_union_specifier.name = INVALID_STRING;
  } else {
    result->u.struct_or_union_specifier.name = opt_name->u.symbol;
  }
  result->u.struct_or_union_specifier.field_list = field_list;
  result->u.struct_or_union_specifier.attribute = attribute;

  return result;
}

ASTTypeSpecifier *build_enum(
    Parser *parser, Token *keyword_enum, Token *opt_name, Token *lcurly,
    ASTEnumerator *enumerator_list, Token *opt_comma, Token *rcurly)
{
  IGNORE(keyword_enum);
  IGNORE(lcurly);
  IGNORE(opt_comma);
  IGNORE(rcurly);

  ASTTypeSpecifier *result = pool_alloc(parser->pool, sizeof *result);
  result->t = ENUM_TYPE_SPECIFIER;
  if (opt_name == NULL) {
    result->u.enum_specifier.name = INVALID_STRING;
  } else {
    result->u.enum_specifier.name = opt_name->u.symbol;
  }
  result->u.enum_specifier.enumerator_list = enumerator_list;

  return result;
}

// @TODO: This feels unnecessary. Couldn't we just have the parser keep
// wrapping the next thing in the input? This is complicated a bit because
// 'pointer' is currently a separate parser to the thing after it.
typedef struct PointerResult
{
  ASTDeclarator *first;
  ASTDeclarator *last;
} PointerResult;

PointerResult *build_next_pointer(
    Parser *parser, PointerResult *pointers, ASTDeclarator *pointer)
{
  IGNORE(parser);

  pointers->last->u.pointer_declarator.pointee = pointer;
  pointers->last = pointer;

  return pointers;
}

ASTDeclarator *build_pointee_declarator(
    Parser *parser, PointerResult *opt_pointer, ASTDirectDeclarator *declarator)
{
  ASTDeclarator *result = pool_alloc(parser->pool, sizeof *result);
  result->t = DIRECT_DECLARATOR;
  result->u.direct_declarator = declarator;

  if (opt_pointer == NULL) return result;

  opt_pointer->last->u.pointer_declarator.pointee = result;

  return opt_pointer->first;
}

ASTDeclarator *build_terminal_pointer(
    Parser *parser, PointerResult *pointer_result)
{
  IGNORE(parser);

  pointer_result->last->u.pointer_declarator.pointee = NULL;
  return pointer_result->first;
}

ASTDirectDeclarator *optional_declarator(
    Parser *parser, ASTDirectDeclarator *declarator)
{
  // This can happen if this is an abstract declarator. In this case we
  // generate an ident declarator with a NULL name, so that we can handle
  // declarators and abstract declarators uniformly.
  if (declarator == NULL) {
    declarator = pool_alloc(parser->pool, sizeof *declarator);
    declarator->t = IDENTIFIER_DECLARATOR;
    declarator->u.name = INVALID_STRING;
  }

  return declarator;
}

static ParserResult identifier(Parser *parser);
static ParserResult direct_declarator(Parser *parser);
static ParserResult direct_abstract_declarator(Parser *parser);

#include "syntax/parse.inc"

// In general we prefer not to reject identifiers that are keywords during
// parsing, because it's easier to provide good error messages by doing so
// during ir_gen instead. However we need to reject "sizeof" so that "sizeof
// *foo" isn't interpreted as MULTIPLY_EXPR.
//
// Similarly, we have to reject type names when parsing identifiers. Otherwise
// "(int[3])foo" is parsed not as a cast expression, but rather as a bracketed
// array index expression followed by an extraneous token.
static ParserResult identifier(Parser *parser)
{
  Token *token = read_token(parser);
  if (token->t != TOK_SYMBOL) {
    back_up(parser);
    return failure;
  }

  String name = token->u.symbol;
  if (string_eq(name, STRING("sizeof"))) {
    back_up(parser);
    return failure;
  }
  TypeTableEntry entry;
  if (type_table_look_up_name(&parser->defined_types, name, &entry)) {
    back_up(parser);
    return failure;
  }

  ASTExpr *ident_expr = pool_alloc(parser->pool, sizeof *ident_expr);
  ident_expr->t = IDENTIFIER_EXPR;
  ident_expr->u.identifier = name;

  return success(ident_expr);
}

static ParserResult direct_declarator_tail(
    Parser *parser, ASTDirectDeclarator *head)
{
  u32 start = parser->position;

  switch (read_token(parser)->t) {
  case TOK_LSQUARE: {
    ASTDirectDeclarator *declarator = head;
    ASTDirectDeclarator **next = &declarator;

    back_up(parser);
    while (current_token(parser)->t == TOK_LSQUARE) {
      read_token(parser);

      ParserResult opt_expr = expr(parser);
      ASTExpr *length_expr = NULL;
      if (opt_expr.success) {
        length_expr = opt_expr.result;
      }
      if (read_token(parser)->t != TOK_RSQUARE) {
        parser->position = start;
        return failure;
      }

      ASTDirectDeclarator *array = pool_alloc(parser->pool, sizeof *array);
      array->t = ARRAY_DECLARATOR;
      array->u.array_declarator.element_declarator = head;
      array->u.array_declarator.array_length = length_expr;

      *next = array;
      next = &array->u.array_declarator.element_declarator;
    }

    return success(declarator);
  }
  case TOK_LROUND: {
    ParserResult params = parameter_list(parser);
    if (!params.success || read_token(parser)->t != TOK_RROUND) {
      parser->position = start;
      return failure;
    }

    ASTDirectDeclarator *func = pool_alloc(parser->pool, sizeof *func);
    func->t = FUNCTION_DECLARATOR;
    func->u.function_declarator.declarator = head;
    func->u.function_declarator.parameters = params.result;

    return success(func);
  }
  default: back_up(parser); return success(head);
  }
}

static ParserResult direct_declarator(Parser *parser)
{
  ParserResult head_result = direct_declarator_head(parser);
  if (!head_result.success) return head_result;

  return direct_declarator_tail(parser, head_result.result);
}

static ParserResult direct_abstract_declarator(Parser *parser)
{
  ParserResult head_result = direct_abstract_declarator_head(parser);
  if (!head_result.success) return head_result;

  return direct_declarator_tail(parser, head_result.result);
}

// The input array consists of SourceTokens, but we treat them as Tokens most
// of the time.
bool parse_toplevel(
    Array(SourceToken) *tokens, Pool *ast_pool, ASTToplevel **toplevel)
{
  Parser parser = {ast_pool, tokens, 0, {EMPTY_ARRAY}};
  type_table_init(&parser.defined_types);

  ParserResult result = translation_unit(&parser);
  if (parser.position != tokens->size) {
    if (_unexpected_token.t != TOK_INVALID) {
      emit_error(
          point_range(_longest_parse_pos), "Unexpected token %s",
          token_type_names[_unexpected_token.t]);
    } else {
      emit_error(UNKNOWN_RANGE, "Unknown error while parsing");
    }

    return false;
  }

  type_table_free(&parser.defined_types);

  *toplevel = result.result;
  return true;
}

bool parse_expr(Array(SourceToken) *tokens, Pool *ast_pool, ASTExpr **out_expr)
{
  Parser parser = {ast_pool, tokens, 0, {EMPTY_ARRAY}};

  ParserResult result = expr(&parser);
  if (parser.position != tokens->size) {
    if (_unexpected_token.t != TOK_INVALID) {
      emit_error(
          point_range(_longest_parse_pos), "Unexpected token %s",
          token_type_names[_unexpected_token.t]);
    } else {
      emit_error(UNKNOWN_RANGE, "Unknown error while parsing");
    }

    return false;
  }

  *out_expr = result.result;
  return true;
}
