// FLAGS: -fsyntax-only -dump-ast

int foo(int *x)
{
  return *x;
}
