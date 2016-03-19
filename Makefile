NAME := ncc
CC ?= clang

COMMON_CFLAGS := $(CFLAGS) -c -std=c99 -Wall -Wextra -Wstrict-prototypes -Wformat

ifneq (, $(shell which ccache))
	ifeq ($(CC), clang)
		COMMON_CFLAGS += -fcolor-diagnostics
	endif

	# CCACHE_CPP2 avoids warnings from compiling pre-preproccessed files
	CC := CCACHE_CPP2=yes ccache $(CC)
endif

DEBUG_CFLAGS := $(COMMON_CFLAGS) -Werror -g
RELEASE_CFLAGS := $(COMMON_CFLAGS) -O3

OBJS := $(patsubst src/%.c, src/%.o, $(shell find src -name '*.c'))
DEBUG_OBJS := $(addprefix debug/, $(OBJS))
RELEASE_OBJS := $(addprefix release/, $(OBJS))

HEADERS := $(shell find src -name '*.h')

.PHONY: debug
debug: debug/$(NAME)

debug/$(NAME): $(DEBUG_OBJS)
	@echo '(DEBUG) CC $@'
	@mkdir -p debug && $(CC) $^ -o $@

debug/%.o: %.c $(HEADERS)
	@echo '(DEBUG) CC $<'
	@mkdir -p `dirname $@` && $(CC) $(DEBUG_CFLAGS) $< -o $@

.PHONY: release
release: release/$(NAME)

release/$(NAME): $(RELEASE_OBJS)
	@echo '(RELEASE) CC $@'
	@mkdir -p release && $(CC) $^ -o $@

release/%.o: %.c $(HEADERS)
	@echo '(RELEASE) CC $<'
	@mkdir -p `dirname $@` && $(CC) $(RELEASE_CFLAGS) $< -o $@

.PHONY: clean
clean:
	rm -rf release debug
