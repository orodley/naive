// FLAGS: -E

#define FOO
#if !defined(FOO)
foo
#else
bar
#endif
#if !defined(BAR)
baz
#else
quux
#endif
