C compiler toolchain targeting Linux x64, written from scratch in C. Many
features are missing, but it is complete enough to self-host.

Includes:

* libc
* C frontend
* SSA form IR
* x64 backend
* x64 Assembler (for the in-memory format, and for textual assembly)
* ELF object file output
* Linker
* `ar` clone

Build-time dependencies:

* A C compiler
* nasm
* Python 3

To build, run `build.py`. There are various subcommands for running tests,
static checks, bootstrapping, etc. See `build.py --help` for more info.