#include "syntax/ast.h"

#include <stdarg.h>
#include <stdio.h>

#include "assertions.h"

static void dump_decl_specifier_list(ASTDeclSpecifier *decl_specifier_list);
static void dump_declarator(ASTDeclarator *declarator);
static void dump_expr(ASTExpr *expr);

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
      case 'S':;
        String s = va_arg(varargs, String);
        for (u32 i = 0; i < s.len; i++) {
          putchar(s.chars[i]);
        }
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
  case STRING_LITERAL_EXPR: pretty_printf("%S", expr->u.string_literal); break;
  case IDENTIFIER_EXPR: pretty_printf("%S", expr->u.identifier); break;
  case STRUCT_DOT_FIELD_EXPR:
  case STRUCT_ARROW_FIELD_EXPR:
    dump_expr(expr->u.struct_field.struct_expr);
    pretty_printf(",%S", expr->u.struct_field.field_name);
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
  case GOTO_STATEMENT: pretty_printf("%S", statement->u.goto_label); break;
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
    pretty_printf("NAMED_TYPE_SPECIFIER(%S", type_specifier->u.name);
    break;
  case UNION_TYPE_SPECIFIER:
  case STRUCT_TYPE_SPECIFIER: {
    pretty_printf(
        type_specifier->t == STRUCT_TYPE_SPECIFIER ? "STRUCT_TYPE_SPECIFIER("
                                                   : "UNION_TYPE_SPECIFIER(");

    String name = type_specifier->u.struct_or_union_specifier.name;
    if (is_valid(name)) pretty_printf("%S,", name);

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
    String name = type_specifier->u.enum_specifier.name;
    ASTEnumerator *enumerator_list =
        type_specifier->u.enum_specifier.enumerator_list;

    if (is_valid(name)) {
      pretty_printf("%S", name);
      if (enumerator_list != NULL) pretty_printf(",");
    }

    while (enumerator_list != NULL) {
      pretty_printf("ENUMERATOR(");
      if (is_valid(enumerator_list->name)) {
        pretty_printf("%S", enumerator_list->name);
      }
      if (enumerator_list->value != NULL) {
        if (is_valid(enumerator_list->name)) pretty_printf(",");
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
    if (is_valid(declarator->u.name)) pretty_printf("%S", declarator->u.name);
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
      pretty_printf("FIELD_DESIGNATOR(%S", designator_list->u.field_name);
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
