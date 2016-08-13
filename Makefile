NAME := ncc
CC ?= clang

PEG ?= meta/peg.py
ENC ?= meta/enc.py

CFLAGS := $(CFLAGS) -g -std=c99 -Isrc \
	-Werror -Wall -Wextra -Wstrict-prototypes -Wformat

ifneq (, $(shell which ccache))
	ifeq ($(CC), clang)
		CFLAGS += -fcolor-diagnostics -Wno-parentheses-equality
	endif

	# CCACHE_CPP2 prevents warnings from compiling pre-preproccessed files
	CC := CCACHE_CPP2=yes ccache $(CC)
endif

GEN_FILES := $(patsubst %.peg, %.inc, $(shell find src -name '*.peg'))
GEN_FILES += $(patsubst %.enc, %.inc, $(shell find src -name '*.enc'))

OBJS := $(patsubst %.c, %.o, $(shell find src -name '*.c'))
HEADERS := $(shell find src -name '*.h')

# Please don't make us waste time regenerating temp files, Mr. Make
.SECONDARY: $(GEN_FILES)

.PHONY: all
all: $(NAME) tags

tags: $(OBJS) $(HEADERS)
	@echo 'ctags'
	@ctags -R --fields=+Sl --langmap=c:+.h

$(NAME): $(OBJS)
	@echo 'CC $@'
	@$(CC) $^ -o $@

%.o: %.c $(HEADERS) $(GEN_FILES)
	@echo 'CC $<'
	@$(CC) -c $(CFLAGS) $< -o $@

%.inc: %.peg $(PEG)
	@echo 'PEG $<'
	@$(PEG) $< $@

%.inc: %.enc $(ENC)
	@echo 'ENC $<'
	@$(ENC) $< $@

.PHONY: clean
clean:
	rm -f $(NAME) $(OBJS) $(GEN_FILES)
