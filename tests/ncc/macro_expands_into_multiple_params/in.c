// FLAGS: -E

#define BAZ(a, b, c) c, b, a
#define QUUX(...) BAZ(__VA_ARGS__)
QUUX('a', 'b', 'c')

#define INDIRECTION(x) BAZ(x)
#define ARGS 1, 2, 3
INDIRECTION(ARGS)