#define _POSIX_C_SOURCE 200809
#include <ctype.h>
#include <stdio.h>
#include <string.h>

#include "backend/asm.h"
#include "backend/elf.h"
#include "diagnostics.h"
#include "file.h"
#include "macros.h"
#include "pool.h"
#include "strings.h"
#include "syntax/reader.h"
#include "types.h"

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

#define X(c, b, w, d, o, do) \
  {                          \
    c, { b, w, d, o, do }    \
  }
static struct
{
  RegClass class;
  char *names[5];
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
  SourceLoc ident_start = reader_source_loc(reader);
  String ident = read_symbol(reader);
  if (!is_valid(ident)) {
    emit_fatal_error(
        range_from(reader, ident_start), "Invalid identifier %.*s\n", ident.len,
        ident.chars);
  }

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
    String name = symbol->name;
    if (string_eq(name, ident)) {
      return asm_symbol(symbol);
    }
  }

  UNREACHABLE;
}

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
    SourceLoc start_loc = reader_source_loc(reader);
    AsmValue value = read_register_or_symbol(reader, symbols);
    if (value.t != ASM_VALUE_REGISTER) {
      emit_fatal_error(
          range_from(reader, start_loc), "Expected register in memory operand");
    }

    Register reg = value.u.reg;
    ASSERT(reg.t == PHYS_REG);

    skip_whitespace(reader);

    switch (read_char(reader)) {
    case '+': {
      skip_whitespace(reader);
      u64 offset = read_integer(reader);
      skip_whitespace(reader);
      if (!expect_char(reader, ']')) {
        emit_fatal_error(
            point_range(reader_source_loc(reader)),
            "Expected ']' after offset");
      }

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
  String input_filename = INVALID_STRING;
  String output_filename = INVALID_STRING;

  for (i32 i = 1; i < argc; i++) {
    char *arg = argv[i];

    if (arg[0] == '-') {
      if (streq(arg, "-f")) {
        if (i == argc - 1) {
          fputs("Error: No argument after '-f'\n", stderr);
          return EXIT_CODE_BAD_CLI;
        }

        char *format = argv[++i];
        if (!streq(format, "elf64")) {
          fprintf(stderr, "Error: Unsupported output format '%s'\n", format);
          return EXIT_CODE_UNIMPLEMENTED;
        }
      } else if (streq(arg, "-o")) {
        if (i == argc - 1) {
          fputs("Error: No filename after '-o'\n", stderr);
          return EXIT_CODE_BAD_CLI;
        }
        i++;
        output_filename = STRING(argv[i]);
      }
    } else if (is_valid(input_filename)) {
      fputs("Error: multiple input filenames given\n", stderr);
      return EXIT_CODE_BAD_CLI;
    } else {
      input_filename = STRING(arg);
    }
  }

  if (!is_valid(input_filename)) {
    fputs("Error: no input filename given\n", stderr);
    return EXIT_CODE_BAD_CLI;
  }
  if (!is_valid(output_filename)) {
    fputs("Error: no output filename given\n", stderr);
    return EXIT_CODE_BAD_CLI;
  }

  // @LEAK
  String input = map_file_into_memory(string_to_c_string(input_filename));
  if (!is_valid(input)) {
    fprintf(
        stderr, "Failed to open input file: '%.*s'\n", input_filename.len,
        input_filename.chars);
    return EXIT_CODE_IO_ERROR;
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
        emit_fatal_error(
            point_range(reader_source_loc(reader)),
            "Unexpected character: '%c'", peek_char(reader));
        return EXIT_CODE_INVALID_SOURCE;
      }

      SourceLoc ident_loc = reader_source_loc(reader);
      String ident = read_symbol(reader);
      if (!is_valid(ident)) {
        emit_fatal_error(
            point_range(ident_loc), "Invalid identifier %.*s", ident.len,
            ident.chars);
      }
      SourceRange ident_range = range_from(reader, ident_loc);

      skip_whitespace(reader);

      if (strneq(ident.chars, "bits", ident.len)) {
        if (read_char(reader) != '6' || read_char(reader) != '4') {
          emit_error(
              range_from(reader, ident_loc), "Only 64-bit mode is supported");
          return EXIT_CODE_UNIMPLEMENTED;
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

        SourceLoc section_name_loc = reader_source_loc(reader);
        String section_name = read_symbol(reader);
        if (!is_valid(section_name)) {
          emit_error(point_range(section_name_loc), "Bad section name");
          return EXIT_CODE_INVALID_SOURCE;
        }

        if (starts_with_dot) section_name.chars--;

        if (strneq(section_name.chars, ".text", section_name.len)) {
          in_text_section = true;
        } else {
          UNIMPLEMENTED(
              "Non-text section '%.*s'", section_name.len, section_name.chars);
        }
      } else if (strneq(ident.chars, "global", ident.len)) {
        SourceLoc symbol_loc = reader_source_loc(reader);
        String symbol_name = read_symbol(reader);
        if (!is_valid(symbol_name)) {
          emit_error(point_range(symbol_loc), "Bad symbol name");
          return EXIT_CODE_INVALID_SOURCE;
        }

        *ARRAY_APPEND(&global_symbols, String) = symbol_name;
      } else if (strneq(ident.chars, "extern", ident.len)) {
        SourceLoc symbol_loc = reader_source_loc(reader);
        String symbol_name = read_symbol(reader);
        if (!is_valid(symbol_name)) {
          emit_error(point_range(symbol_loc), "Bad symbol name");
          return EXIT_CODE_INVALID_SOURCE;
        }

        AsmSymbol *symbol = pool_alloc(&asm_module.pool, sizeof *symbol);
        *symbol = (AsmSymbol){
            .name = symbol_name,
            .section = TEXT_SECTION,
            .defined = false,
            .linkage = ASM_GLOBAL_LINKAGE,
            .symtab_index = asm_module.symbols.size + 1,
        };

        *ARRAY_APPEND(&asm_module.symbols, AsmSymbol *) = symbol;
      } else if (peek_char(reader) != ':') {
        if (!in_text_section) {
          emit_fatal_error(
              ident_range, "Instruction encountered before section directive");
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
          emit_fatal_error(
              ident_range, "Unknown instruction name '%s'",
              strndup(ident.chars, ident.len));
        }

        skip_whitespace(reader);

        u32 arg = 0;
        while (!at_end(reader) && peek_char(reader) != '\n') {
          if (arg == STATIC_ARRAY_LENGTH(instr->args)) {
            emit_error(ident_range, "Too many operands to instruction");
            return EXIT_CODE_INVALID_SOURCE;
          }

          instr->args[arg] = read_operand(reader, &asm_module.symbols);

          skip_whitespace(reader);
          if (peek_char(reader) == ',') {
            advance(reader);
            skip_whitespace(reader);
          } else if (!at_end(reader) && peek_char(reader) != '\n') {
            emit_error(
                point_range(reader_source_loc(reader)),
                "Expected comma or newline after operand");
            return EXIT_CODE_INVALID_SOURCE;
          }

          arg++;
        }

        instr->arity = arg;
      } else {
        advance(reader);

        if (!in_text_section) {
          emit_error(
              ident_range, "Symbol encountered before section directive");
          return EXIT_CODE_INVALID_SOURCE;
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
            .name = ident,
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
  return write_elf_object_file(output_filename, &asm_module);
}
