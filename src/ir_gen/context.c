#include "ir_gen/context.h"

Binding *binding_for_name(Scope *scope, char *name)
{
  for (u32 i = 0; i < scope->bindings.size; i++) {
    Binding *binding = ARRAY_REF(&scope->bindings, Binding, i);
    if (streq(binding->name, name)) return binding;
  }

  if (scope->parent_scope != NULL) {
    return binding_for_name(scope->parent_scope, name);
  } else {
    return NULL;
  }
}