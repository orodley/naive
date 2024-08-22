#include "syntax/parse.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "array.h"
#include "diagnostics.h"
#include "exit_code.h"
#include "macros.h"
#include "misc.h"
#include "pool.h"
#include "syntax/lex.h"
#include "util.h"

typedef struct TypeTableEntry
{
  char *type_name;
} TypeTableEntry;

typedef struct TypeTable
{
  Array(TypeTableEntry) entries;
} TypeTable;

static char *builtin_types[] = {
    "void",   "char",   "short",    "int",   "long",     "float",
    "double", "signed", "unsigned", "_Bool", "_Complex",
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
    TypeTable *type_table, char *name, TypeTableEntry *out)
{
  for (u32 i = 0; i < type_table->entries.size; i++) {
    TypeTableEntry *entry = ARRAY_REF(&type_table->entries, TypeTableEntry, i);
    if (streq(entry->type_name, name)) {
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
  return &((SourceToken *)token)->source_loc;
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

static char *direct_declarator_name(ASTDirectDeclarator *declarator);

static char *declarator_name(ASTDeclarator *declarator)
{
  switch (declarator->t) {
  case POINTER_DECLARATOR:
    return declarator_name(declarator->u.pointer_declarator.pointee);
  case DIRECT_DECLARATOR:
    return direct_declarator_name(declarator->u.direct_declarator);
  }

  UNREACHABLE;
}

static char *direct_declarator_name(ASTDirectDeclarator *declarator)
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

  char *name = token->u.symbol;
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
    result->u.struct_or_union_specifier.name = NULL;
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
    result->u.enum_specifier.name = NULL;
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
    declarator->u.name = NULL;
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

  char *name = token->u.symbol;
  if (streq(name, "sizeof")) {
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
          &_longest_parse_pos, "Unexpected token %s",
          token_type_names[_unexpected_token.t]);
    } else {
      SourceLoc s = {"<unknown>", 0, 0};
      emit_error(&s, "Unknown error while parsing");
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
          &_longest_parse_pos, "Unexpected token %s",
          token_type_names[_unexpected_token.t]);
    } else {
      SourceLoc s = {"<unknown>", 0, 0};
      emit_error(&s, "Unknown error while parsing");
    }

    return false;
  }

  *out_expr = result.result;
  return true;
}

static int indent_level = 0;

static void print_indent(void)
{
  for (int n = 0; n < indent_level; n++) fputs("    ", stdout);
}

static void pretty_printf(char *fmt, ...)
{
  va_list varargs;
  va_start(varargs, fmt);

  for (int i = 0; fmt[i] != '\0'; i++) {
    char c = fmt[i];
    switch (c) {
    case '%':
      i++;
      ASSERT(fmt[i] != '\0');

      switch (fmt[i]) {
      case 's':;
        // @NOTE: We assume we never need to do any formatting of
        // stuff printed by '%s', as usually this is just identifiers
        // and stuff, no control characters we'd indent based on.
        char *str = va_arg(varargs, char *);
        fputs(str, stdout);
        break;
      case '8':;
        uint64_t x = va_arg(varargs, uint64_t);
        printf("%" PRIu64, x);
        break;
      case 'f':;
        double d = va_arg(varargs, double);
        printf("%f", d);
        break;
      default: UNREACHABLE;
      }
      break;
    case '(':
      puts("(");
      indent_level++;
      print_indent();

      break;
    case ',':
      puts(",");
      print_indent();
      break;
    case ')':
      putchar('\n');
      indent_level--;
      print_indent();
      putchar(')');

      break;
    default: putchar(c); break;
    }
  }

  va_end(varargs);
}

static void dump_decl_specifier_list(ASTDeclSpecifier *decl_specifier_list);
static void dump_declarator(ASTDeclarator *declarator);
static void dump_expr(ASTExpr *expr);

static void dump_type_name(ASTTypeName *type_name)
{
  pretty_printf("TYPE_NAME(");
  dump_decl_specifier_list(type_name->decl_specifier_list);
  pretty_printf(",");
  if (type_name->declarator != NULL) dump_declarator(type_name->declarator);
  pretty_printf(")");
}

static void dump_args(ASTArgument *args)
{
  while (args != NULL) {
    dump_expr(args->expr);
    pretty_printf(",");

    args = args->next;
  }
}

#define X(x) #x
static char *expr_type_names[] = {AST_EXPR_TYPES};
#undef X

static void dump_initializer_element_list(ASTInitializerElement *element_list);

static void dump_numeric_suffix(NumericSuffix suffix)
{
  if ((suffix & UNSIGNED_SUFFIX) != 0) {
    pretty_printf("U");
  }
  if ((suffix & LONG_SUFFIX) != 0) {
    pretty_printf("L");
  }
  if ((suffix & LONG_LONG_SUFFIX) != 0) {
    pretty_printf("LL");
  }
  if ((suffix & FLOAT_SUFFIX) != 0) {
    pretty_printf("F");
  }
}

static void dump_expr(ASTExpr *expr)
{
  pretty_printf("%s(", expr_type_names[expr->t]);
  switch (expr->t) {
  case INT_LITERAL_EXPR: {
    pretty_printf("%8", expr->u.int_literal.value);
    dump_numeric_suffix(expr->u.int_literal.suffix);
    break;
  }
  case FLOAT_LITERAL_EXPR: {
    pretty_printf("%f", expr->u.float_literal.value);
    dump_numeric_suffix(expr->u.float_literal.suffix);
    break;
  }
  case STRING_LITERAL_EXPR:
    pretty_printf("%s", expr->u.string_literal.chars);
    break;
  case IDENTIFIER_EXPR: pretty_printf("%s", expr->u.identifier); break;
  case STRUCT_DOT_FIELD_EXPR:
  case STRUCT_ARROW_FIELD_EXPR:
    dump_expr(expr->u.struct_field.struct_expr);
    pretty_printf(",%s", expr->u.struct_field.field_name);
    break;
  case POST_INCREMENT_EXPR:
  case POST_DECREMENT_EXPR:
  case PRE_INCREMENT_EXPR:
  case PRE_DECREMENT_EXPR:
  case ADDRESS_OF_EXPR:
  case DEREF_EXPR:
  case UNARY_PLUS_EXPR:
  case UNARY_MINUS_EXPR:
  case BIT_NOT_EXPR:
  case LOGICAL_NOT_EXPR:
  case SIZEOF_EXPR_EXPR: dump_expr(expr->u.unary_arg); break;
  case FUNCTION_CALL_EXPR:
    dump_expr(expr->u.function_call.callee);
    pretty_printf(",ARGS(");
    dump_args(expr->u.function_call.arg_list);
    pretty_printf(")");
    break;
  case CAST_EXPR:
    dump_type_name(expr->u.cast.cast_type);
    pretty_printf(",");
    dump_expr(expr->u.cast.arg);
    break;
  case BUILTIN_VA_ARG_EXPR:
    dump_expr(expr->u.builtin_va_arg.va_list_expr);
    pretty_printf(",");
    dump_type_name(expr->u.builtin_va_arg.type_name);
    break;
  case SIZEOF_TYPE_EXPR: dump_type_name(expr->u.type); break;
  case INDEX_EXPR:
  case MULTIPLY_EXPR:
  case DIVIDE_EXPR:
  case MODULO_EXPR:
  case ADD_EXPR:
  case MINUS_EXPR:
  case LEFT_SHIFT_EXPR:
  case RIGHT_SHIFT_EXPR:
  case LESS_THAN_EXPR:
  case GREATER_THAN_EXPR:
  case LESS_THAN_OR_EQUAL_EXPR:
  case GREATER_THAN_OR_EQUAL_EXPR:
  case EQUAL_EXPR:
  case NOT_EQUAL_EXPR:
  case BIT_AND_EXPR:
  case BIT_XOR_EXPR:
  case BIT_OR_EXPR:
  case LOGICAL_AND_EXPR:
  case LOGICAL_OR_EXPR:
  case ASSIGN_EXPR:
  case MULTIPLY_ASSIGN_EXPR:
  case DIVIDE_ASSIGN_EXPR:
  case MODULO_ASSIGN_EXPR:
  case ADD_ASSIGN_EXPR:
  case MINUS_ASSIGN_EXPR:
  case LEFT_SHIFT_ASSIGN_EXPR:
  case RIGHT_SHIFT_ASSIGN_EXPR:
  case BIT_AND_ASSIGN_EXPR:
  case BIT_XOR_ASSIGN_EXPR:
  case BIT_OR_ASSIGN_EXPR:
  case COMMA_EXPR:
    dump_expr(expr->u.binary_op.arg1);
    pretty_printf(",");
    dump_expr(expr->u.binary_op.arg2);
    break;
  case CONDITIONAL_EXPR:
    dump_expr(expr->u.ternary_op.arg1);
    pretty_printf(",");
    dump_expr(expr->u.ternary_op.arg2);
    pretty_printf(",");
    dump_expr(expr->u.ternary_op.arg3);
    break;
  case COMPOUND_EXPR:
    dump_type_name(expr->u.compound.type_name);
    pretty_printf(",");
    dump_initializer_element_list(expr->u.compound.initializer_element_list);
    break;
  }

  pretty_printf(")");
}

#define X(x) #x
static char *statement_type_names[] = {AST_STATEMENT_TYPES};
#undef X

static void dump_decls(ASTDecl *decls);

static void dump_statement(ASTStatement *statement)
{
  pretty_printf("%s(", statement_type_names[statement->t]);
  switch (statement->t) {
  case EMPTY_STATEMENT:
  case CONTINUE_STATEMENT:
  case BREAK_STATEMENT: break;
  case LABELED_STATEMENT:
    pretty_printf("%s,", statement->u.labeled_statement.label_name);
    dump_statement(statement->u.labeled_statement.statement);
    break;
  case COMPOUND_STATEMENT: {
    ASTBlockItem *block_item = statement->u.block_item_list;
    while (block_item != NULL) {
      switch (block_item->t) {
      case BLOCK_ITEM_STATEMENT:
        pretty_printf("BLOCK_ITEM_STATEMENT(");
        dump_statement(block_item->u.statement);
        break;
      case BLOCK_ITEM_DECL:
        pretty_printf("BLOCK_ITEM_DECL(");
        dump_decls(block_item->u.decl);
        break;
      }
      pretty_printf(")");

      if (block_item->next != NULL) pretty_printf(",");
      block_item = block_item->next;
    }
    break;
  }
  case RETURN_STATEMENT:
    if (statement->u.expr == NULL) {
      break;
    }
    // fallthrough
  case EXPR_STATEMENT: dump_expr(statement->u.expr); break;
  case IF_STATEMENT:
    dump_expr(statement->u.if_statement.condition);
    pretty_printf(",");
    dump_statement(statement->u.if_statement.then_statement);
    if (statement->u.if_statement.else_statement != NULL) {
      pretty_printf(",");
      dump_statement(statement->u.if_statement.else_statement);
    }
    break;
  case CASE_STATEMENT:
  case SWITCH_STATEMENT:
  case WHILE_STATEMENT:
  case DO_WHILE_STATEMENT:
    dump_expr(statement->u.expr_and_statement.expr);
    pretty_printf(",");
    dump_statement(statement->u.expr_and_statement.statement);
    break;
  case FOR_STATEMENT:
    switch (statement->u.for_statement.init_type) {
    case FOR_INIT_EXPR:
      if (statement->u.for_statement.init.expr != NULL)
        dump_expr(statement->u.for_statement.init.expr);
      break;
    case FOR_INIT_DECL: dump_decls(statement->u.for_statement.init.decl); break;
    }
    pretty_printf(",");
    if (statement->u.for_statement.condition)
      dump_expr(statement->u.for_statement.condition);
    pretty_printf(",");
    if (statement->u.for_statement.update_expr != NULL)
      dump_expr(statement->u.for_statement.update_expr);
    break;
  case GOTO_STATEMENT: pretty_printf(statement->u.goto_label); break;
  }

  pretty_printf(")");
}

static void dump_field_declarator_list(
    ASTFieldDeclarator *field_declarator_list)
{
  while (field_declarator_list != NULL) {
    switch (field_declarator_list->t) {
    case NORMAL_FIELD_DECLARATOR:
      pretty_printf("NORMAL_FIELD_DECLARATOR(");
      dump_declarator(field_declarator_list->u.declarator);
      pretty_printf(")");
      break;
    case BITFIELD_FIELD_DECLARATOR:
      pretty_printf("BITFIELD_DECLARATOR(");
      dump_declarator(field_declarator_list->u.bitfield.declarator);
      pretty_printf(",");
      dump_expr(field_declarator_list->u.bitfield.width);
      pretty_printf(")");
      break;
    }

    if (field_declarator_list->next != NULL) pretty_printf(",");
    field_declarator_list = field_declarator_list->next;
  }
}

static void dump_struct_or_union_field_list(ASTFieldDecl *field_list)
{
  while (field_list != NULL) {
    pretty_printf("FIELD(");
    if (field_list->decl_specifier_list != NULL) {
      dump_decl_specifier_list(field_list->decl_specifier_list);
      pretty_printf(",");
    }

    pretty_printf("FIELD_DECLARATOR_LIST(");
    dump_field_declarator_list(field_list->field_declarator_list);
    pretty_printf("))");

    if (field_list->next != NULL) pretty_printf(",");
    field_list = field_list->next;
  }
}

static void dump_attribute(ASTAttribute *attribute)
{
  pretty_printf("ATTRIBUTE(");
  pretty_printf("%s", attribute->name);
  pretty_printf(")");
}

static void dump_type_specifier(ASTTypeSpecifier *type_specifier)
{
  switch (type_specifier->t) {
  case NAMED_TYPE_SPECIFIER:
    pretty_printf("NAMED_TYPE_SPECIFIER(%s", type_specifier->u.name);
    break;
  case UNION_TYPE_SPECIFIER:
  case STRUCT_TYPE_SPECIFIER: {
    pretty_printf(
        type_specifier->t == STRUCT_TYPE_SPECIFIER ? "STRUCT_TYPE_SPECIFIER("
                                                   : "UNION_TYPE_SPECIFIER(");

    char *name = type_specifier->u.struct_or_union_specifier.name;
    if (name != NULL) pretty_printf("%s,", name);

    pretty_printf("FIELD_LIST(");
    dump_struct_or_union_field_list(
        type_specifier->u.struct_or_union_specifier.field_list);
    pretty_printf(")");

    if (type_specifier->u.struct_or_union_specifier.attribute != NULL) {
      pretty_printf(",");
      dump_attribute(type_specifier->u.struct_or_union_specifier.attribute);
    }

    break;
  }
  case ENUM_TYPE_SPECIFIER: {
    pretty_printf("ENUM_TYPE_SPECIFIER(");
    char *name = type_specifier->u.enum_specifier.name;
    ASTEnumerator *enumerator_list =
        type_specifier->u.enum_specifier.enumerator_list;

    if (name != NULL) {
      pretty_printf("%s", name);
      if (enumerator_list != NULL) pretty_printf(",");
    }

    while (enumerator_list != NULL) {
      pretty_printf("ENUMERATOR(");
      if (enumerator_list->name != NULL) {
        pretty_printf("%s", enumerator_list->name);
      }
      if (enumerator_list->value != NULL) {
        if (enumerator_list->name != NULL) pretty_printf(",");
        dump_expr(enumerator_list->value);
      }
      pretty_printf(")");
      enumerator_list = enumerator_list->next;
    }
  }
  }

  pretty_printf(")");
}

static void dump_decl_specifier_list(ASTDeclSpecifier *decl_specifier_list)
{
  pretty_printf("DECL_SPECIFIER(");

#define CASE(x) \
  case x: pretty_printf(#x); break;

  while (decl_specifier_list != NULL) {
    switch (decl_specifier_list->t) {
    case STORAGE_CLASS_SPECIFIER:
      switch (decl_specifier_list->u.storage_class_specifier) {
        CASE(TYPEDEF_SPECIFIER)
        CASE(EXTERN_SPECIFIER)
        CASE(STATIC_SPECIFIER)
        CASE(AUTO_SPECIFIER)
        CASE(REGISTER_SPECIFIER)
      }
      break;
    case TYPE_QUALIFIER:
      switch (decl_specifier_list->u.type_qualifier) {
        CASE(CONST_QUALIFIER)
        CASE(RESTRICT_QUALIFIER)
        CASE(VOLATILE_QUALIFIER)
      }
      break;
#undef CASE
    case FUNCTION_SPECIFIER:
      ASSERT(decl_specifier_list->u.function_specifier == INLINE_SPECIFIER);
      pretty_printf("INLINE_SPECIFIER");
      break;
    case TYPE_SPECIFIER:
      dump_type_specifier(decl_specifier_list->u.type_specifier);
      break;
    }

    if (decl_specifier_list->next != NULL) pretty_printf(",");

    decl_specifier_list = decl_specifier_list->next;
  }

  pretty_printf(")");
}

static void dump_parameter_decls(ASTParameterDecl *param_decls)
{
  pretty_printf("PARAM_DECLS(");
  while (param_decls != NULL) {
    switch (param_decls->t) {
    case PARAMETER_DECL:
      pretty_printf("PARAM(");
      dump_decl_specifier_list(param_decls->decl_specifier_list);
      pretty_printf(",");
      dump_declarator(param_decls->declarator);
      pretty_printf(")");
      break;
    case ELLIPSIS_DECL: pretty_printf("ELLIPSIS"); break;
    }

    if (param_decls->next != NULL) pretty_printf(",");

    param_decls = param_decls->next;
  }

  pretty_printf(")");
}

static void dump_direct_declarator(ASTDirectDeclarator *declarator)
{
  switch (declarator->t) {
  case DECLARATOR:
    pretty_printf("DECLARATOR(");
    dump_declarator(declarator->u.declarator);
    break;
  case IDENTIFIER_DECLARATOR:
    pretty_printf("IDENTIFIER_DECLARATOR(");
    if (declarator->u.name != NULL) pretty_printf("%s", declarator->u.name);
    break;
  case FUNCTION_DECLARATOR:
    pretty_printf("FUNCTION_DECLARATOR(");
    dump_direct_declarator(declarator->u.function_declarator.declarator);
    pretty_printf(",");
    dump_parameter_decls(declarator->u.function_declarator.parameters);
    break;
  case ARRAY_DECLARATOR:
    pretty_printf("ARRAY_DECLARATOR(");
    dump_direct_declarator(declarator->u.array_declarator.element_declarator);

    if (declarator->u.array_declarator.array_length != NULL) {
      pretty_printf(",");
      dump_expr(declarator->u.array_declarator.array_length);
    }
    break;
  }

  pretty_printf(")");
}

static void dump_declarator(ASTDeclarator *declarator)
{
  switch (declarator->t) {
  case POINTER_DECLARATOR:
    pretty_printf("POINTER_DECLARATOR(");
    dump_decl_specifier_list(
        declarator->u.pointer_declarator.decl_specifier_list);
    pretty_printf(",");
    if (declarator->u.pointer_declarator.pointee != NULL)
      dump_declarator(declarator->u.pointer_declarator.pointee);
    break;
  case DIRECT_DECLARATOR:
    pretty_printf("DIRECT_DECLARATOR(");
    dump_direct_declarator(declarator->u.direct_declarator);
    break;
  }

  pretty_printf(")");
}

static void dump_designator_list(ASTDesignator *designator_list)
{
  while (designator_list != NULL) {
    switch (designator_list->t) {
    case INDEX_DESIGNATOR:
      pretty_printf("INDEX_DESIGNATOR(");
      dump_expr(designator_list->u.index_expr);
      break;
    case FIELD_DESIGNATOR:
      pretty_printf("FIELD_DESIGNATOR(%s", designator_list->u.field_name);
      break;
    }
    pretty_printf(")");

    if (designator_list->next != NULL) pretty_printf(",");

    designator_list = designator_list->next;
  }
}

static void dump_initializer(ASTInitializer *initializer);

static void dump_initializer_element_list(ASTInitializerElement *element_list)
{
  while (element_list != NULL) {
    pretty_printf("INITIALIZER_ELEMENT(");
    pretty_printf("DESIGNATOR_LIST(");
    dump_designator_list(element_list->designator_list);
    pretty_printf("),INITIALIZER(");
    dump_initializer(element_list->initializer);
    pretty_printf("))");

    if (element_list->next != NULL) pretty_printf(",");

    element_list = element_list->next;
  }
}

static void dump_initializer(ASTInitializer *initializer)
{
  switch (initializer->t) {
  case EXPR_INITIALIZER:
    pretty_printf("EXPR_INITIALIZER(");
    dump_expr(initializer->u.expr);
    break;
  case BRACE_INITIALIZER:
    pretty_printf("BRACE_INITIALIZER(");
    dump_initializer_element_list(initializer->u.initializer_element_list);
    break;
  }

  pretty_printf(")");
}

static void dump_init_declarators(ASTInitDeclarator *init_declarators)
{
  while (init_declarators != NULL) {
    pretty_printf("INIT_DECLARATOR(");
    dump_declarator(init_declarators->declarator);

    if (init_declarators->initializer != NULL) {
      pretty_printf(",");
      dump_initializer(init_declarators->initializer);
    }

    pretty_printf(")");
    if (init_declarators->next != NULL) pretty_printf(",");

    init_declarators = init_declarators->next;
  }
}

static void dump_decls(ASTDecl *decls)
{
  while (decls != NULL) {
    pretty_printf("DECL(");
    dump_decl_specifier_list(decls->decl_specifier_list);
    pretty_printf(",");
    dump_init_declarators(decls->init_declarators);
    pretty_printf(")");

    if (decls->next != NULL) pretty_printf(",");

    decls = decls->next;
  }
}

void dump_toplevel(ASTToplevel *ast)
{
  PRECONDITION(indent_level == 0);

  while (ast != NULL) {
    switch (ast->t) {
    case FUNCTION_DEF:
      pretty_printf("FUNCTION_DEF(");
      dump_decl_specifier_list(ast->u.function_def->decl_specifier_list);
      pretty_printf(",");
      dump_declarator(ast->u.function_def->declarator);
      pretty_printf(",");
      pretty_printf("OLD_STYLE_PARAM_DECL_LIST(");
      dump_decls(ast->u.function_def->old_style_param_decl_list);
      pretty_printf("),");
      dump_statement(ast->u.function_def->body);
      break;
    case DECL:
      pretty_printf("DECLS(");
      dump_decls(ast->u.decl);
      break;
    }

    pretty_printf(")\n");

    ast = ast->next;
  }

  POSTCONDITION(indent_level == 0);
}
