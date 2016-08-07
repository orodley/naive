NAME := ncc
CC ?= clang

PEG ?= meta/peg.py
ENC ?= meta/enc.py

COMMON_CFLAGS := $(CFLAGS) -c -std=c99 -Wall -Wextra -Wstrict-prototypes -Wformat -Isrc

ifneq (, $(shell which ccache))
	ifeq ($(CC), clang)
		COMMON_CFLAGS += -fcolor-diagnostics -Wno-parentheses-equality
	endif

	# CCACHE_CPP2 avoids warnings from compiling pre-preproccessed files
	CC := CCACHE_CPP2=yes ccache $(CC)
endif

DEBUG_CFLAGS := $(COMMON_CFLAGS) -Werror -g -Idebug/src
RELEASE_CFLAGS := $(COMMON_CFLAGS) -O3 -Irelease/src

GEN_FILES := $(patsubst src/%.peg, src/%.inc, $(shell find src -name '*.peg'))
GEN_FILES += $(patsubst src/%.enc, src/%.inc, $(shell find src -name '*.enc'))
DEBUG_GEN_FILES := $(addprefix debug/, $(GEN_FILES))
RELEASE_GEN_FILES := $(addprefix release/, $(GEN_FILES))

OBJS := $(patsubst src/%.c, src/%.o, $(shell find src -name '*.c'))
DEBUG_OBJS := $(addprefix debug/, $(OBJS))
RELEASE_OBJS := $(addprefix release/, $(OBJS))

HEADERS := $(shell find src -name '*.h')

# Please don't make us waste time regenerating temp files, Mr. Make
.SECONDARY: $(DEBUG_GEN_FILES) $(RELEASE_GEN_FILES)

.PHONY: debug
debug: debug/$(NAME) tags

tags: $(DEBUG_OBJS) $(HEADERS)
	@echo '(DEBUG) ctags'
	@ctags -R --fields=+Sl --langmap=c:+.h

debug/$(NAME): $(DEBUG_OBJS)
	@echo '(DEBUG) CC $@'
	@mkdir -p debug && $(CC) $^ -o $@

debug/%.o: %.c $(HEADERS) $(DEBUG_GEN_FILES)
	@echo '(DEBUG) CC $<'
	@mkdir -p `dirname $@` && $(CC) $(DEBUG_CFLAGS) $< -o $@

debug/%.inc: %.peg $(PEG)
	@echo '(DEBUG) PEG $<'
	@mkdir -p `dirname $@` && $(PEG) $< $@

debug/%.inc: %.enc $(ENC)
	@echo '(DEBUG) ENC $<'
	@mkdir -p `dirname $@` && $(ENC) $< $@

.PHONY: release
release: release/$(NAME)

release/$(NAME): $(RELEASE_OBJS)
	@echo '(RELEASE) CC $@'
	@mkdir -p release && $(CC) $^ -o $@

release/%.o: %.c $(HEADERS) $(RELEASE_GEN_FILES)
	@echo '(RELEASE) CC $<'
	@mkdir -p `dirname $@` && $(CC) $(RELEASE_CFLAGS) $< -o $@

release/%.inc: %.peg $(PEG)
	@echo '(RELEASE) PEG $<'
	@mkdir -p `dirname $@` && $(PEG) $< $@

release/%.inc: %.enc $(ENC)
	@echo '(RELEASE) ENC $<'
	@mkdir -p `dirname $@` && $(ENC) $< $@

.PHONY: clean
clean:
	rm -rf release debug
