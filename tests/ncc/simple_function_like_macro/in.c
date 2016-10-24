// FLAGS: -fsyntax-only -dump-tokens

#define FOO(x) int x = 3;
FOO(foo)

#define BAR(x) int foo = x;
BAR(baz(1, 2))
