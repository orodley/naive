// FLAGS: -E

#define FOO(...) __VA_ARGS__
#define BAR(a, b, ...) __VA_ARGS__, a, b

FOO(a, b, c, d, e, f)
BAR(100, 200, 300, 400, 500)