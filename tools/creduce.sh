#!/bin/sh

# Miscellaneous stuff used for testing with creduce.

NAIVE_DIR=.

# Sometimes creduce produces stuff that makes ncc use tons of memory.
# Obviously this should be fixed, but for now we don't want it stopping us from
# using creduce.
ulimit -Sv 500000

#trap 'if [ $? -eq 139 ]; then exit 0; else exit 1; fi' CHLD
#[ -s in.c ] \
	#&& ! "$NAIVE_DIR"/ncc1 -fsyntax-only -dump-ast in.c

#grep -q '[[^:space:]]' in.c \
	#&& "$NAIVE_DIR"/ncc  -c -dump-register-assignments in.c > a.regs \
	#&& "$NAIVE_DIR"/ncc1 -c -dump-register-assignments in.c > b.regs \
	#&& ! diff -q a.regs b.regs
"$NAIVE_DIR"/ncc -c in.c 2>&1 | grep -q "asm_gen.c:1758"
#"$NAIVE_DIR"/ncc -c in.c 2>&1 | grep -q AddressSanitizer
#"$NAIVE_DIR"/ncc -c in.c && \
	#readelf -s in.o | grep -q '0x.*[0-9] is_sign_extending_instr'
