// FLAGS: -E

#define FOO(x) BAR(x)
#define BAR(y) y
FOO(2)
