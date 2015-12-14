NAME := ncc
CC ?= clang
CFLAGS += -g -std=c99 -Werror -Wall -Wextra -Wstrict-prototypes

OBJS := $(patsubst src/%.c, src/%.o, $(wildcard src/*.c))

.PHONY: all
all: $(NAME)

$(NAME): $(OBJS)
	$(CC) $(LDFLAGS) $^ -o $@

.PHONY: clean
clean:
	rm -f src/*.o
	rm -f $(NAME)
