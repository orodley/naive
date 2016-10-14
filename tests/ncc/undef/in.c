// FLAGS: -fsyntax-only

#define FOO
#undef FOO

#ifdef FOO
#error FOO shouldn't be defined here!
#endif
