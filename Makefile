CC ?= clang
AR ?= ar
NASM ?= nasm

PEG ?= meta/peg.py
ENC ?= meta/enc.py

COMMON_CFLAGS := $(CFLAGS) -std=c99 -Werror -Wall -Wextra -Wstrict-prototypes \
	-Wformat
NCC_CFLAGS := -Isrc
NAR_CFLAGS = $(NCC_CFLAGS)
LIBC_CFLAGS := -fno-asynchronous-unwind-tables -ffreestanding -fno-common \
	-Ilibc -Ilibc/include

ifneq (, $(shell which ccache))
	ifeq ($(CC), clang)
		COMMON_CFLAGS += -fcolor-diagnostics -Wno-parentheses-equality
	endif

	# CCACHE_CPP2 prevents warnings from compiling pre-preproccessed files
	CC := CCACHE_CPP2=yes ccache $(CC)
endif

SRC_DIRS := src libc freestanding

GEN_FILES := $(patsubst %.peg, %.inc, $(shell find $(SRC_DIRS) -name '*.peg'))
GEN_FILES += $(patsubst %.enc, %.inc, $(shell find $(SRC_DIRS) -name '*.enc'))

HEADERS := $(shell find $(SRC_DIRS) -name '*.h')

objs_for_dir = $(patsubst %.c, %.o, $(shell find $(1) -name '*.c')) \
			   $(patsubst %.s, %.o, $(shell find $(1) -name '*.s')) \

# Please don't make us waste time regenerating temp files, Mr. Make
.SECONDARY: $(GEN_FILES)

.PHONY: all
all: ncc nar libc.a tags

.PHONY: asan
asan: NCC_CFLAGS += -fsanitize=address
asan: all

.PHONY: msan
msan: NCC_CFLAGS += -fsanitize=memory -fsanitize-memory-track-origins=2
msan: all

.PHONY: test
test: all
	@./run_tests.py

tags: ncc
	@echo 'ctags'
	@ctags -R --fields=+Sl --langmap=c:+.h

ncc: src/bin/ncc.o src/array.o src/asm.o src/asm_gen.o src/bit_set.o \
		src/diagnostics.o src/elf.o src/file.o src/ir.o src/ir_gen.o \
		src/parse.o src/pool.o src/preprocess.o src/reader.o src/tokenise.o \
		src/util.o
	@echo 'CC $@'
	@$(CC) $(COMMON_CFLAGS) $(NCC_CFLAGS) $^ -o $@
	@echo 'Installing ncc in /opt/naive'
	@cp ncc /opt/naive

nar: src/bin/nar.o src/array.o src/file.o src/util.o
	@echo 'CC $@'
	@$(CC) $(COMMON_CFLAGS) $(NAR_CFLAGS) $^ -o $@
	@echo 'Installing nar in /opt/naive'
	@cp nar /opt/naive

libc.a: $(call objs_for_dir,libc) $(HEADERS)
	@echo 'AR $@'
	@$(AR) -cr $@ $(call objs_for_dir,libc)
	@[ -w /opt/naive ] \
		|| (echo 'Please create /opt/naive and give yourself write permissions' \
			&& exit 1)
	@echo 'Installing libc in /opt/naive'
	@cp $@ /opt/naive
	@rm -rf /opt/naive/include
	@cp -r libc/include /opt/naive
	@cp -r freestanding /opt/naive


libc/%.o: libc/%.c
	@echo 'CC $@'
	@$(CC) -c $(COMMON_CFLAGS) $(LIBC_CFLAGS) $< -o $@

src/%.o: src/%.c $(HEADERS) $(GEN_FILES)
	@echo 'CC $<'
	@$(CC) -c $(COMMON_CFLAGS) $(NCC_CFLAGS) -g $< -o $@

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
	rm -f ncc nar libc.a $(shell find $(SRC_DIRS) -name '*.o') $(GEN_FILES)
