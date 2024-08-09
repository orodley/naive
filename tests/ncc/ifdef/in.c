// FLAGS: -fsyntax-only -dump-tokens

#define FOO

#ifdef FOO
int foo
# ifndef FOO
 @@@ awefawef ' " garbage that doesn't even lex
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
