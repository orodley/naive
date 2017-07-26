// FLAGS: -E

#define FOO

#if defined    FOO
A
#else
B
#endif

#if defined(FOO)
C
#else
D
#endif

#if defined    (    FOO   )     
E
#else
F
#endif

#if defined BAR
G
#else
H
#endif
