#!/bin/sh

cd "$(dirname "$0")"

VERSION=1.13

if [ ! -f gzip-$VERSION.tar.xz ]; then
	curl "https://mirror.endianness.com/gnu/gzip/gzip-$VERSION.tar.xz" \
					--output gzip-$VERSION.tar.xz
fi

if [ ! -d gzip-$VERSION.tar.xz ]; then
	tar -Jxf gzip-$VERSION.tar.xz
fi

cd gzip-$VERSION
if ! CC=../../../build/toolchain/ncc ./configure; then
	cat config.log
fi
