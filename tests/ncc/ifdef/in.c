// FLAGS: -fsyntax-only -fdump-tokens

#define FOO

#ifdef FOO
int foo
# ifndef FOO
XXX
# else
= 3;
# endif
#else
# ifndef BAR
YYY
# else
ZZZ
# endif
#endif

// []
