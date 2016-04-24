#!/usr/bin/env python

# Quick and dirty PEG generator.

import pprint
import sys

pp = pprint.PrettyPrinter(indent=4)

def generate_parsers(input_filename, output_filename):
    with open(input_filename, 'r') as f:
        peg = f.read()

    tokens = tokenise(peg)
    #pp.pprint(tokens)

    named_parsers = []
    reader = Reader(tokens)
    while reader.has_more():
        named_parsers.append(reader.read_toplevel())

    #pp.pprint(parsers)

    CWriter(named_parsers).write(input_filename, output_filename)

def tokenise(peg):
    i = 0
    tokens = []
    while i != len(peg):
        c = peg[i]
        if c in ('=', '(', ')', ','):
            tokens.append(c)
        elif ident_char(c):
            start = i
            i += 1
            while ident_char(peg[i]):
                i += 1

            tokens.append(peg[start:i])
            i -= 1
        elif c == '"':
            start = i
            i += 1
            while peg[i] != '"':
                i += 1

            tokens.append(peg[start + 1:i])
        elif c.isspace():
            pass

        i += 1

    return tokens

def ident_char(c):
    return c in '_.#->' or c.isalnum()

class Reader(object):
    def __init__(self, tokens):
        self.tokens = tokens

        self.index = 0

    def has_more(self):
        return self.index != len(self.tokens)

    def at(self, offset):
        return self.tokens[self.index + offset]

    def read_token(self):
        #print self.at(0)
        self.index += 1
        return self.at(-1)

    def read_toplevel(self):
        name = self.read_token()
        equals = self.read_token()

        assert all(ident_char(c) for c in name)
        assert equals == '='

        parser = self.read_parser()
        return [name, parser]

    def read_parser(self):
        operator = self.read_token()

        if self.at(0) == '(':
            self.read_token()
            args = []
            while True:
                parser = self.read_parser()
                args.append(parser)

                seperator = self.read_token()
                assert seperator in (')', ',')
                if seperator == ')':
                    break
            return [operator] + args
        else:
            return operator

class CWriter(object):
    def __init__(self, named_parsers):
        self.named_parsers = named_parsers
        self.definitions = []

    def write(self, input_filename, output_filename):
        for named_parser in self.named_parsers:
            self.generate_parser(named_parser[1], named_parser[0])

        output = []

        output.append("""
// @NOTE: This is an automatically generated file! Do not edit it!
//        It was generated from '%s', edit that instead.

#include <stddef.h>

#include "misc.h"
#include "parse.h"


static u32 _longest_parse_length;
static SourceLoc _longest_parse_pos;
static Token _unexpected_token;
""" % input_filename)

        for definition in self.definitions:
            output.append(definition.split('\n')[0] + ';\n')
        output.append("\n")
        for definition in self.definitions:
            output.append(definition + '\n\n')

        with open(output_filename, 'w') as f:
            f.writelines(output)

    def emit_function(self, body, name, signature=None):
        if signature is None:
            signature = 'static void *%s(Parser *parser)' % name
        self.definitions.append(signature +
                '\n{' +
                '\t' + '\n\t'.join(body.split('\n')) +
                '\n}')

        return name

    def generate_parser(self, parser, name=None):
        if name is None:
            name = ''.join('_' if c in "[], \"'" else c for c in `parser`) + \
                    `len(self.definitions)`
        if isinstance(parser, str):
            if parser.startswith('TOK_'):
                return self.emit_function(
"""
Token *result = read_token(parser);
if (result->type != %s) {
\tif (parser->index > _longest_parse_length) {
\t\t_longest_parse_length = parser->index;
\t\t_longest_parse_pos = *parser_context(parser);
\t\t_unexpected_token = *result;
\t}
\t
\tback_up(parser);
\treturn NULL;
}

return result;""" % parser, name)
            else:
                return parser

        # @PERF: Could turn this into a packrat parser (or a limited packrat
        # parser as decribed in Redziejowski, 2007) by having a static cache
        # variable for each function.
        operator = parser[0]
        args = parser[1:]

        if operator == 'keyword':
            return self.emit_function(
"""
if (expect_keyword(parser, "%s")) {
\treturn (void *)1;
} else {
\tif (parser->index > _longest_parse_length) {
\t\t_longest_parse_length = parser->index;
\t\t_longest_parse_pos = *parser_context(parser);
\t\tback_up(parser);
\t\t
\t\t_unexpected_token = *current_token(parser);
\t}
\t
\treturn NULL;
}
""" % args[0], name)
        elif operator == 'build':
            result_type = args[0]
            arity = int(args[1])
            signature = 'static %s *%s(Parser *parser%s)' % \
                (result_type, name, ''.join(', void *arg' + `i` for i in xrange(arity)))
            field_map = [args[i:i+2] for i in xrange(2, len(args), 2)]
            prologue = '\n%s *result = pool_alloc(parser->pool, sizeof *result);\n' % result_type
            prologue += ' '.join('(void)arg%d;' % i for i in xrange(arity)) + '\n'
            field_assigners = ''.join(
                field_assigner(field_name, assigner) for field_name, assigner in field_map)
            epilogue = 'return result;'

            return self.emit_function(
                    prologue + field_assigners + epilogue,
                    name,
                    signature)
        elif operator in ('or', 'which'):
            prologue = \
"""
void *result; (void)result;
u32 start; (void)start;
"""
            if operator == 'which':
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
""" % (self.generate_parser(arg), ret_body(i)) for i, arg in enumerate(args))

            epilogue = "return NULL;"

            return self.emit_function(prologue + main + epilogue, name)
        elif operator == 'seq':
            args = map(self.generate_parser, args)
            prologue = "\nu32 start; (void)start;"
            main = ''.join(
"""
start = parser->index;
void *_{0}_result{1} = {0}(parser);
if (_{0}_result{1} == NULL)
\treturn revert(parser, start);
""".format(p, i) for i, p in enumerate(args[1:]))
            result_args = ''.join(', _%s_result%d' % (p, i) for i, p in
                    enumerate(args[1:]))
            epilogue = "\nreturn %s(parser%s);" % (args[0], result_args)

            return self.emit_function(prologue + main + epilogue, name)
        elif operator == 'fold':
            args = map(self.generate_parser, args)
            return self.emit_function(
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
}""" % (args[1], args[2], args[0]), name)
        elif operator == 'list':
            element_type = args[0]
            element_parser = self.generate_parser(args[1])
            if len(args) == 3:
                seperator = \
"""
\tif (%s(parser) == NULL)
\t\treturn first;
""" % self.generate_parser(args[2])
            else:
                seperator = ""

            return self.emit_function(
"""
%s *first = %s(parser);
if (first == NULL)
\treturn NULL;

%s *curr = first;
for (;;) {%s
\t%s *next = %s(parser);
\tif (next == NULL)
\t\treturn first;
\tcurr->next = next;
\tcurr = next;
}
""" % (element_type, element_parser, element_type, seperator, element_type, element_parser), name)
        elif operator == 'opt':
            return self.emit_function(
"""
void *result = %s(parser);
OptResult *opt = pool_alloc(parser->pool, sizeof(*opt));
opt->result = result;

return opt;
""" % self.generate_parser(args[0]), name)
        else:
            assert not "Unreachable"

def field_assigner(field_name, assigner_expr):
    return 'result->%s = %s;\n' % (field_name, assigner_expr.replace('#', 'arg'))



if __name__ == '__main__':
    if len(sys.argv) not in (2, 3):
        print "Usage: %s <peg definition> [output file]" % sys.argv[0]
        sys.exit(1)

    # @PORT
    output_filename = "/dev/stdout" if len(sys.argv) == 2 else sys.argv[2]
    generate_parsers(sys.argv[1], output_filename)
