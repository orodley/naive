#!/usr/bin/env python

# Quick and dirty PEG generator.

from collections import namedtuple
import pprint
import sys

pp = pprint.PrettyPrinter(indent=4)

def generate_parsers(input_filename, output_filename):
    with open(input_filename, 'r') as f:
        peg = f.read()

    tokens = tokenise(peg)
    #pp.pprint(tokens)

    reader = Reader(tokens)
    while reader.has_more():
        reader.read_toplevel()

    parsers = reader.dedupe()
    #pp.pprint(parsers)

    CWriter(parsers).write(input_filename, output_filename)

def tokenise(peg):
    i = 0
    tokens = []
    while i != len(peg):
        c = peg[i]
        if c in ('=', '(', ')', ','):
            tokens.append(c)
        elif ident_char(c):
            symbol = [c]
            i += 1
            while ident_char(peg[i]):
                symbol.append(peg[i])
                i += 1
            i -= 1

            tokens.append(''.join(symbol))
        elif c.isspace():
            pass

        i += 1

    return tokens

def ident_char(c):
    return c == '_' or c.isalpha()

Parser = namedtuple('Parser', ['name', 'operator', 'args'])

class Reader(object):
    def __init__(self, tokens):
        self.tokens = tokens

        self.index = 0
        self.temp_name_counter = 0
        self.parsers = []

    def has_more(self):
        return self.index != len(self.tokens)

    def at(self, offset):
        return self.tokens[self.index + offset]

    def temp_name(self):
        name = '_parser' + `self.temp_name_counter`
        self.temp_name_counter += 1

        return name

    def read_token(self):
        #print self.at(0)
        self.index += 1
        return self.at(-1)

    def read_toplevel(self):
        name = self.read_token()
        equals = self.read_token()

        assert all(ident_char(c) for c in name)
        assert equals == '='

        self.parsers.append(self.read_parser()._replace(name = name))

    def read_parser(self):
        operator = self.read_token()

        if operator.startswith("TOK_"):
            return Parser('_token_' + operator, 'token', [operator])
        elif operator == 'keyword':
            open_bracket = self.read_token()
            keyword = self.read_token()
            close_bracket = self.read_token()

            assert open_bracket == '('
            assert close_bracket == ')'

            return Parser('_keyword_' + keyword, 'keyword', [keyword])
        elif operator == 'oneof':
            open_bracket = self.read_token()
            assert open_bracket == '('

            token_list = []
            while True:
                token_list.append(self.read_token())

                seperator = self.read_token()
                assert seperator in (')', ',')
                if seperator == ')':
                    break

            return Parser('_oneof_' + '_'.join(token_list), 'oneof', token_list)
        elif operator in ('which', 'or', 'seq', 'fold', 'opt'):
            open_bracket = self.read_token()
            assert open_bracket == '('

            args = []
            while True:
                parser = self.read_parser()
                args.append(parser.name)
                self.parsers.append(parser)

                seperator = self.read_token()
                assert seperator in (')', ',')
                if seperator == ')':
                    break

            return Parser(self.temp_name(), operator, args)
        else:
            assert all(ident_char(c) for c in operator)
            return Parser(operator, 'named', [])

    def dedupe(self):
        deduped = []
        primitives = set()

        for parser in self.parsers:
            if parser.operator not in ('token', 'keyword', 'oneof'):
                deduped.append(parser)
            elif parser.name not in primitives:
                deduped.append(parser)
                primitives.add(parser.name)

        return deduped

class CWriter(object):
    def __init__(self, parsers):
        self.parsers = parsers
        self.definitions = []
        self.arrays = []

    def write(self, input_filename, output_filename):
        for parser in self.parsers:
            self.write_parser(parser)

        output = []

        output.append("""
// @NOTE: This is an automatically generated file! Do not edit it!
//        It was generated from '%s', edit that instead.

#include <stddef.h>

#include "misc.h"
#include "parse.h"

""" % input_filename)

        for definition in self.definitions:
            output.append(definition.split('\n')[0] + ';\n')
        output.append("\n")
        for array in self.arrays:
            output.append(array + '\n')
        output.append("\n")
        for definition in self.definitions:
            output.append(definition + '\n\n')

        with open(output_filename, 'w') as f:
            f.writelines(output)

    def emit_function(self, name, body):
        signature = 'static void *%s(Parser *parser)' % name
        self.definitions.append(signature +
                '\n{' +
                '\t' + '\n\t'.join(body.split('\n')) +
                '\n}')

    def emit_array(self, element_type, elements):
        name = "_temp_array" + `len(self.arrays)`
        self.arrays.append("%s %s[] = {%s};"
                % (element_type, name, ', '.join(elements)))
        return name

    def write_parser(self, parser):
        # @PERF: Could turn this into a packrat parser (or a limited packrat
        # parser as decribed in Redziejowski, 2007) by having a static cache
        # variable for each function.
        if parser.operator == 'token':
            self.emit_function(parser.name,
"""
Token *result = read_token(parser);
if (result->type != %s) {
\tback_up(parser);
\treturn NULL;
}

return result;""" % parser.args[0])
        elif parser.operator == 'keyword':
            self.emit_function(parser.name,
"""
return expect_keyword(parser, "%s") ? (void *)1 : NULL;
""" % parser.args[0])
        elif parser.operator == 'oneof':
            assert all(elem.startswith("TOK_") for elem in parser.args)
            array_name = self.emit_array("TokenType", parser.args)

            self.emit_function(parser.name,
"""
Token *token = read_token(parser);
for (u32 i = 0; i < STATIC_ARRAY_LENGTH(%s); i++) {
\tif (token->type == %s[i]) {
\t\treturn token;
\t}
}

back_up(parser);
return NULL;""" % (array_name, array_name))
        elif parser.operator in ('or', 'which'):
            prologue = \
"""
void *result; (void)result;
u32 start; (void)start;
"""
            if parser.operator == 'which':
                ret_body = lambda i: \
"""\tWhichResult *which_result = pool_alloc(parser->pool, sizeof(*which_result));
\twhich_result->which = %d;
\twhich_result->result = result;
\t
\treturn which_result;""" % i
            else:
                ret_body = lambda i: "\treturn result;"

            main = ''.join(
"""
start = parser->index;
result = %s(parser);
if (result != NULL) {
%s
}
parser->index = start;
""" % (parser_name, ret_body(i)) for i, parser_name in enumerate(parser.args))

            epilogue = "return NULL;"

            self.emit_function(parser.name, prologue + main + epilogue)
        elif parser.operator == 'seq':
            prologue = "\nu32 start; (void)start;"
            main = ''.join(
"""
start = parser->index;
void *_{0}_result = {0}(parser);
if (_{0}_result == NULL)
\treturn revert(parser, start);
""".format(p) for p in parser.args[1:])
            result_args = ''.join(', _%s_result' % p for p in parser.args[1:])
            epilogue = "\nreturn %s(parser%s);" % (parser.args[0], result_args)

            self.emit_function(parser.name, prologue + main + epilogue)
        elif parser.operator == 'fold':
            self.emit_function(parser.name,
"""
void *initial = %s(parser);
if (initial == NULL)
\treturn NULL;

void *curr = initial;
for (;;) {
\tvoid *next = %s(parser);
\tif (next == NULL)
\t\treturn curr;
\t
\tcurr = %s(parser, curr, next);
}""" % (parser.args[1], parser.args[2], parser.args[0]))
        elif parser.operator == 'opt':
            self.emit_function(parser.name,
"""
void *result = %s(parser);
OptResult *opt = pool_alloc(parser->pool, sizeof(*opt));
opt->result = result;

return opt;
""" % parser.args[0])
        elif parser.operator == 'named':
            pass
        else:
            print "Unrecognised parser type: " + parser.operator
            assert not "Unreachable"



if __name__ == '__main__':
    if len(sys.argv) not in (2, 3):
        print "Usage: %s <peg definition> [output file]" % sys.argv[0]
        sys.exit(1)

    # @PORT
    output_filename = "/dev/stdout" if len(sys.argv) == 2 else sys.argv[2]
    generate_parsers(sys.argv[1], output_filename)
