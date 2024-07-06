// FLAGS: -E

#if 0
A
#elif 0
B
#elif 1
C
#elif 1
D
#endif

#if 1
E
#elif 1
    F
#elif 0
    G
#endif
