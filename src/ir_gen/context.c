#include "ir_gen/context.h"

#include "array.h"
#include "assertions.h"

Binding *binding_for_name(Scope *scope, String name)
{
  for (u32 i = 0; i < scope->bindings.size; i++) {
    Binding *binding = ARRAY_REF(&scope->bindings, Binding, i);
    if (string_eq(binding->name, name)) return binding;
  }

  if (scope->parent_scope != NULL) {
    return binding_for_name(scope->parent_scope, name);
  } else {
    return NULL;
  }
}

void push_scope(IrGenContext *ctx, Scope *scope)
{
  scope->bindings = EMPTY_ARRAY;
  scope->parent_scope = ctx->scope;
  ctx->scope = scope;
}

void pop_scope(IrGenContext *ctx)
{
  PRECONDITION(ctx->scope != NULL);

  array_free(&ctx->scope->bindings);
  ctx->scope = ctx->scope->parent_scope;
}