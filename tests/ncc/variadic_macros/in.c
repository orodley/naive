// FLAGS: -E

#define FOO(...) __VA_ARGS_
#define BAR(a, b, ...) __VA_ARGS_, b, a
#define BAZ(a, b, c) c, b, a
#define QUUX(...) BAZ(__VA_ARGS_)

FOO(a, b, c, d, e, f)
BAR(100, 200, 300, 400, 500)
QUUX('a', 'b', 'c')
