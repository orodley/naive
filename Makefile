NAME := ncc
CC ?= clang
CFLAGS += -std=c99 -Werror -Wall -Wextra -Wstrict-prototypes

OBJS := $(patsubst src/%.c, src/%.o, $(wildcard src/*.c))

.PHONY: all
all: $(NAME)

.PHONY: debug
debug: CFLAGS += -g
debug: $(NAME)

.PHONY: opt
opt: CFLAGS += -O3
opt: $(NAME)

$(NAME): $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $@

.PHONY: clean
clean:
	rm -f src/*.o
	rm -f $(NAME)
