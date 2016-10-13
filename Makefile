CC ?= clang
AR ?= ar
NASM ?= nasm

PEG ?= meta/peg.py
ENC ?= meta/enc.py

CFLAGS := $(CFLAGS) -std=c99 -Isrc \
	-Werror -Wall -Wextra -Wstrict-prototypes -Wformat

ifneq (, $(shell which ccache))
	ifeq ($(CC), clang)
		CFLAGS += -fcolor-diagnostics -Wno-parentheses-equality
	endif

	# CCACHE_CPP2 prevents warnings from compiling pre-preproccessed files
	CC := CCACHE_CPP2=yes ccache $(CC)
endif

SRC_DIRS := src libc

GEN_FILES := $(patsubst %.peg, %.inc, $(shell find $(SRC_DIRS) -name '*.peg'))
GEN_FILES += $(patsubst %.enc, %.inc, $(shell find $(SRC_DIRS) -name '*.enc'))

HEADERS := $(shell find $(SRC_DIRS) -name '*.h')

objs_for_dir = $(patsubst %.c, %.o, $(shell find $(1) -name '*.c')) \
			   $(patsubst %.s, %.o, $(shell find $(1) -name '*.s')) \

# Please don't make us waste time regenerating temp files, Mr. Make
.SECONDARY: $(GEN_FILES)

.PHONY: all
all: ncc libc.a tags

.PHONY: asan
asan: CFLAGS += -fsanitize=address
asan: all

.PHONY: msan
msan: CFLAGS += -fsanitize=memory -fsanitize-memory-track-origins=2
msan: all

.PHONY: test
test: all
	@echo 'TEST'
	@./run_tests.py

tags: ncc
	@echo 'ctags'
	@ctags -R --fields=+Sl --langmap=c:+.h

ncc: $(call objs_for_dir,src)
	@echo 'CC $@'
	@$(CC) $(CFLAGS) $^ -o $@

libc.a: $(call objs_for_dir,libc)
	@echo 'AR $@'
	@$(AR) -cr $@ $^
	@[ -w /opt/naive ] \
		|| (echo 'Please create /opt/naive and give yourself write permissions' \
			&& exit 1)
	@cp $@ /opt/naive
	@mkdir -p /opt/naive/include
	@cp libc/include/* /opt/naive/include


libc/%.o: libc/%.c
	@echo 'CC $@'
	@$(CC) -c $(CFLAGS) -fno-asynchronous-unwind-tables -ffreestanding \
		-nostdinc -I libc -I libc/include $< -o $@

%.o: %.c $(HEADERS) $(GEN_FILES)
	@echo 'CC $<'
	@$(CC) -c $(CFLAGS) -g $< -o $@

%.o: %.s
	@echo 'NASM $<'
	@$(NASM) -f elf64 $< -o $@

%.inc: %.peg $(PEG)
	@echo 'PEG $<'
	@$(PEG) $< $@

%.inc: %.enc $(ENC)
	@echo 'ENC $<'
	@$(ENC) $< $@

.PHONY: clean
clean:
	rm -f ncc libc.a $(shell find $(SRC_DIRS) -name '*.o') $(GEN_FILES)
