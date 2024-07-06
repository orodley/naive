#define _POSIX_C_SOURCE 200809
#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "asm.h"
#include "diagnostics.h"
#include "elf.h"
#include "misc.h"
#include "pool.h"
#include "reader.h"
#include "util.h"

// @NOTE: This doesn't skip newlines, because newlines aren't allowed in the
// middle of instructions or directives.
static void skip_whitespace(Reader *reader)
{
  while (!at_end(reader)) {
    char c = peek_char(reader);
    if (c == ' ' || c == '\t') {
      advance(reader);
    } else {
      break;
    }
  }
}

static u64 read_integer(Reader *reader)
{
  u64 imm = 0;
  while (isdigit(peek_char(reader))) {
    imm *= 10;
    imm += read_char(reader) - '0';
  }

  return imm;
}

#define X(c, b, w, d, o) \
  {                      \
    c, { b, w, d, o }    \
  }
static struct
{
  RegClass class;
  char *names[4];
} registers[] = {REG_CLASSES};
#undef X

static bool string_eq_case_insensitive(String a, char *b)
{
  if (strlen(b) != a.len) return false;

  for (u32 i = 0; i < a.len; i++) {
    if (tolower(a.chars[i]) != tolower(b[i])) {
      return false;
    }
  }

  return true;
}

static AsmValue read_register_or_symbol(
    Reader *reader, Array(AsmSymbol *) *symbols)
{
  String ident = read_symbol(reader);
  assert(is_valid(ident));

  for (u32 i = 0; i < STATIC_ARRAY_LENGTH(registers); i++) {
    for (u32 j = 0; j < STATIC_ARRAY_LENGTH(registers[i].names); j++) {
      char *name = registers[i].names[j];
      if (string_eq_case_insensitive(ident, name)) {
        return asm_phys_reg(registers[i].class, 8 << j);
      }
    }
  }

  for (u32 i = 0; i < symbols->size; i++) {
    AsmSymbol *symbol = *ARRAY_REF(symbols, AsmSymbol *, i);
    char *name = symbol->name;
    if (strlen(name) == ident.len && strneq(ident.chars, name, ident.len)) {
      return asm_symbol(symbol);
    }
  }

  UNREACHABLE;
}

// @TODO: Make this be able to fail, and replace asserts with issue_errors.
static AsmValue read_operand(Reader *reader, Array(AsmSymbol *) *symbols)
{
  switch (peek_char(reader)) {
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9': return asm_imm(read_integer(reader));
  case '[': {
    advance(reader);
    AsmValue value = read_register_or_symbol(reader, symbols);
    assert(value.t == ASM_VALUE_REGISTER);

    Register reg = value.u.reg;
    assert(reg.t == PHYS_REG);

    skip_whitespace(reader);

    switch (read_char(reader)) {
    case '+': {
      skip_whitespace(reader);
      u64 offset = read_integer(reader);
      skip_whitespace(reader);
      assert(read_char(reader) == ']');

      return asm_deref(
          asm_offset_reg(reg.u.class, reg.width, asm_const_imm(offset)));
    }
    case ']': return asm_deref(asm_phys_reg(reg.u.class, reg.width));
    default: UNREACHABLE;
    }
  }
  default: {
    return read_register_or_symbol(reader, symbols);
  }
  }
}

#define X(x) #x
static char *asm_op_names[] = {ASM_OPS};
#undef X

int main(int argc, char *argv[])
{
  char *input_filename = NULL;
  char *output_filename = NULL;

  for (i32 i = 1; i < argc; i++) {
    char *arg = argv[i];

    if (arg[0] == '-') {
      if (streq(arg, "-f")) {
        if (i == argc - 1) {
          fputs("Error: No argument after '-f'\n", stderr);
          return 1;
        }

        char *format = argv[++i];
        if (!streq(format, "elf64")) {
          fprintf(stderr, "Error: Unsupported output format '%s'\n", format);
          return 1;
        }
      } else if (streq(arg, "-o")) {
        if (i == argc - 1) {
          fputs("Error: No filename after '-o'\n", stderr);
          return 1;
        }
        output_filename = argv[++i];
      }
    } else if (input_filename != NULL) {
      fputs("Error: multiple input filenames given\n", stderr);
      return 1;
    } else {
      input_filename = arg;
    }
  }

  if (input_filename == NULL) {
    fputs("Error: no input filename given\n", stderr);
    return 1;
  }
  if (output_filename == NULL) {
    fputs("Error: no output filename given\n", stderr);
    return 1;
  }

  String input = map_file_into_memory(input_filename);
  if (!is_valid(input)) {
    fprintf(stderr, "Failed to open input file: '%s'\n", input_filename);
    return 1;
  }

  Reader _reader;
  Reader *reader = &_reader;
  reader_init(reader, input, EMPTY_ARRAY, true, input_filename);

  AsmModule asm_module;
  init_asm_module(&asm_module, input_filename);
  bool in_text_section = false;
  Array(String) global_symbols = EMPTY_ARRAY;
  AsmSymbol *prev_symbol = NULL;

  while (!at_end(reader)) {
    switch (peek_char(reader)) {
    case ' ':
    case '\t':
    case '\n': advance(reader); break;
    case ';':
      while (read_char(reader) != '\n')
        ;
      break;
    default: {
      if (!initial_ident_char(peek_char(reader))) {
        issue_error(
            &reader->source_loc, "Unexpected character: '%c'\n",
            peek_char(reader));
        return 1;
      }

      SourceLoc ident_source_loc = reader->source_loc;
      String ident = read_symbol(reader);
      assert(is_valid(ident));
      skip_whitespace(reader);

      if (strneq(ident.chars, "bits", ident.len)) {
        if (read_char(reader) != '6' || read_char(reader) != '4') {
          issue_error(&reader->source_loc, "Only 64-bit mode is supported");
          return 1;
        }
      } else if (strneq(ident.chars, "section", ident.len)) {
        // @HACK: read_symbol doesn't handle '.', but the section names
        // we care about can start with '.'. Rather than rewriting or
        // extending read_symbol we just handle this case specially.
        bool starts_with_dot = false;
        if (peek_char(reader) == '.') {
          starts_with_dot = true;
          advance(reader);
        }

        String section_name = read_symbol(reader);
        if (!is_valid(section_name)) {
          issue_error(&reader->source_loc, "Bad section name");
          return 1;
        }

        if (starts_with_dot) section_name.chars--;

        if (strneq(section_name.chars, ".text", section_name.len)) {
          in_text_section = true;
        } else {
          UNIMPLEMENTED;
        }
      } else if (strneq(ident.chars, "global", ident.len)) {
        String symbol_name = read_symbol(reader);
        if (!is_valid(symbol_name)) {
          issue_error(&reader->source_loc, "Bad symbol name");
          return 1;
        }

        *ARRAY_APPEND(&global_symbols, String) = symbol_name;
      } else if (strneq(ident.chars, "extern", ident.len)) {
        String symbol_name = read_symbol(reader);
        if (!is_valid(symbol_name)) {
          issue_error(&reader->source_loc, "Bad symbol name");
          return 1;
        }

        AsmSymbol *symbol = pool_alloc(&asm_module.pool, sizeof *symbol);
        *symbol = (AsmSymbol){
            .name = strndup(symbol_name.chars, symbol_name.len),
            .section = TEXT_SECTION,
            .defined = false,
            .linkage = ASM_GLOBAL_LINKAGE,
            .symtab_index = asm_module.symbols.size + 1,
        };

        *ARRAY_APPEND(&asm_module.symbols, AsmSymbol *) = symbol;
      } else if (peek_char(reader) != ':') {
        if (!in_text_section) {
          issue_error(
              &ident_source_loc,
              "Instruction encountered before section directive");
          return 1;
        }

        AsmInstr *instr = ARRAY_APPEND(&asm_module.text.instrs, AsmInstr);
        instr->num_deps = 0;
        instr->label = NULL;
        if (prev_symbol != NULL) {
          instr->label = prev_symbol;
          prev_symbol = NULL;
        }

        bool found = false;
        for (AsmOp op = 0; op < STATIC_ARRAY_LENGTH(asm_op_names); op++) {
          char *name = asm_op_names[op];
          if (string_eq_case_insensitive(ident, name)) {
            found = true;
            instr->op = op;
            break;
          }
        }

        if (!found) {
          issue_error(
              &ident_source_loc, "Unknown instruction name '%s'\n",
              strndup(ident.chars, ident.len));
          return 1;
        }

        skip_whitespace(reader);

        u32 arg = 0;
        while (!at_end(reader) && peek_char(reader) != '\n') {
          if (arg == STATIC_ARRAY_LENGTH(instr->args)) {
            issue_error(&reader->source_loc, "Too many operands");
            return 1;
          }

          instr->args[arg] = read_operand(reader, &asm_module.symbols);

          skip_whitespace(reader);
          if (peek_char(reader) == ',') {
            advance(reader);
            skip_whitespace(reader);
          } else if (!at_end(reader) && peek_char(reader) != '\n') {
            issue_error(
                &reader->source_loc, "Expected comma or newline after operand");
            return 1;
          }

          arg++;
        }

        instr->arity = arg;
      } else {
        advance(reader);

        if (!in_text_section) {
          issue_error(
              &ident_source_loc, "Symbol encountered before section directive");
          return 1;
        }

        AsmLinkage linkage = ASM_LOCAL_LINKAGE;
        for (u32 i = 0; i < global_symbols.size; i++) {
          String *existing = ARRAY_REF(&global_symbols, String, i);
          if (ident.len == existing->len
              && strneq(existing->chars, ident.chars, ident.len)) {
            linkage = ASM_GLOBAL_LINKAGE;
            break;
          }
        }

        AsmSymbol *symbol = pool_alloc(&asm_module.pool, sizeof *symbol);
        *symbol = (AsmSymbol){
            .name = strndup(ident.chars, ident.len),
            .section = TEXT_SECTION,
            .defined = true,
            .linkage = linkage,
            .symtab_index = asm_module.symbols.size + 1,
        };

        *ARRAY_APPEND(&asm_module.symbols, AsmSymbol *) = symbol;
        prev_symbol = symbol;
      }
    }
    }
  }

  assemble(&asm_module);
  return !write_elf_object_file(output_filename, &asm_module);
}
