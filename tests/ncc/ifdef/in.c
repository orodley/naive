// FLAGS: -fsyntax-only -fdump-tokens

#define FOO

#ifdef FOO
int foo
# ifndef FOO
 @@@ awefawef ' " garbage that doesn't even tokenise
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
