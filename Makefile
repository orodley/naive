CC ?= clang
AR ?= ar
ASM ?= nasm

PEG ?= meta/peg.py
ENC ?= meta/enc.py

INSTALL_DIR ?= build/toolchain/

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

# Disable the builtin implicit rules. They can conflict with our rules. Usually
# this would mean that our rules overwrite them, but this only happens if the
# dependencies are the same. In some cases they aren't, e.g. when we depend on
# headers as well.
.SUFFIXES:

# Please don't make us waste time regenerating temp files, Mr. Make
.SECONDARY: $(GEN_FILES)

.PHONY: all
all: ncc nar nas libc.a tags

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

ncc: src/bin/ncc.o src/array.o src/asm.o src/asm_gen.o src/reg_alloc.o src/bit_set.o \
		src/diagnostics.o src/elf.o src/file.o src/ir.o src/ir_gen.o src/c_type.o \
		src/parse.o src/pool.o src/preprocess.o src/reader.o src/tokenise.o \
		src/util.o
	@echo 'CC $@'
	@$(CC) $(COMMON_CFLAGS) $(NCC_CFLAGS) $^ -o $@
	@mkdir -p "$(INSTALL_DIR)" 2>&1 > /dev/null \
		|| (echo "Please create $(INSTALL_DIR) and give yourself write permissions" \
			&& exit 1)
	@echo "Installing ncc in $(INSTALL_DIR)"
	@cp ncc "$(INSTALL_DIR)"
	@echo "Installing freestanding headers in $(INSTALL_DIR)"
	@cp -r freestanding "$(INSTALL_DIR)"

nar: src/bin/nar.o src/array.o src/file.o src/util.o
	@echo 'CC $@'
	@$(CC) $(COMMON_CFLAGS) $(NAR_CFLAGS) $^ -o $@
	@mkdir -p "$(INSTALL_DIR)" 2>&1 > /dev/null \
		|| (echo "Please create $(INSTALL_DIR) and give yourself write permissions" \
			&& exit 1)
	@echo "Installing nar in $(INSTALL_DIR)"
	@cp nar "$(INSTALL_DIR)"

nas: src/bin/nas.o src/reader.o src/util.o src/diagnostics.o src/asm.o \
		src/elf.o src/pool.o src/file.o src/array.o
	@echo 'CC $@'
	@$(CC) $(COMMON_CFLAGS) $(NAR_CFLAGS) $^ -o $@
	@mkdir -p "$(INSTALL_DIR)" 2>&1 > /dev/null \
		|| (echo "Please create $(INSTALL_DIR) and give yourself write permissions" \
			&& exit 1)
	@echo "Installing nas in $(INSTALL_DIR)"
	@cp nas "$(INSTALL_DIR)"

libc.a: $(call objs_for_dir,libc) $(HEADERS)
	@echo 'AR $@'
	@$(AR) -cr $@ $(call objs_for_dir,libc)
	@mkdir -p "$(INSTALL_DIR)" 2>&1 > /dev/null \
		|| (echo "Please create $(INSTALL_DIR) and give yourself write permissions" \
			&& exit 1)
	@echo "Installing libc in $(INSTALL_DIR)"
	@cp $@ "$(INSTALL_DIR)"
	@rm -rf "$(INSTALL_DIR)/include"
	@cp -r libc/include "$(INSTALL_DIR)"


libc/%.o: libc/%.c ncc
	@echo 'NCC $@'
	@./ncc -c $(COMMON_CFLAGS) $(LIBC_CFLAGS) $< -o $@

src/%.o: src/%.c $(HEADERS) $(GEN_FILES)
	@echo 'CC $<'
	@$(CC) -c $(COMMON_CFLAGS) $(NCC_CFLAGS) -g $< -o $@

%.o: %.s
	@echo 'ASM $<'
	@$(ASM) -f elf64 $< -o $@

%.inc: %.peg $(PEG)
	@echo 'PEG $<'
	@$(PEG) $< $@

%.inc: %.enc $(ENC)
	@echo 'ENC $<'
	@$(ENC) $< $@

.PHONY: clean
clean:
	rm -f ncc nar libc.a $(shell find $(SRC_DIRS) -name '*.o') $(GEN_FILES)
