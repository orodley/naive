#!/usr/bin/env python

# Generates code for encoding assembly instructions (and potentially decoding
# in the future for a disassembler). The format that is parsed matches the
# format used in the Intel architecture manual, read it for further details.

import pprint
import re
import sys
from collections import namedtuple

pp = pprint.PrettyPrinter(indent=4)

Instr = namedtuple('Instr', ['opcode', 'encodings'])
Encoding = namedtuple('Encoding',
        ['args', 'arg_order', 'rex_prefix', 'opcode_size', 'opcode',
         'reg_and_rm', 'opcode_extension', 'immediate_size', 'reg_in_opcode'])

def generate_encoder(input_filename, output_filename):
    instrs = {}

    with open(input_filename, 'r') as f:
        for line in f.readlines():
            if line == '\n' or line.startswith('#'):
                continue

            instr, encoding = line.rstrip('\n').split('=')

            instr_components = instr.replace(',', ' ').split()
            assert 1 <= len(instr_components) <= 3
            instr_name = instr_components[0]
            args = instr_components[1:]

            if args == ['r64', 'r/m64']:
                arg_order = 'RM'
            elif args == ['r/m64', 'r64']:
                arg_order = 'MR'
            else:
                arg_order = 'INVALID'

            # @TODO: We treat 'i' immediates the same as 'c' immediates. It's
            # not entirely clear whether they are actually encoded the same, as
            # the manual says that 'i' operands follow the opcode, ModR/M and
            # SIB, and that 'c' operands follow the opcode. Look into this
            # further if there is an opcode with a 'c' immediate and ModR/M or
            # SIB bytes.
            match = re.match(
                    r' *(?:REX\.(?P<prefix>[WRXB]+) *\+ *)? *' +
                    r'(?P<opcode>([0-9a-fA-F]+|\[[0-9a-fA-F ]+\])) *' +
                    r'(?P<slash>/.)? *' +
                    r'(?P<reg_in_opcode>\+rd)? *' +
                    r'(?P<immediate>[ic][bdo])? *',
                    encoding)

            rex_prefix = -1
            if match.group('prefix'):
                rex_prefix = 0b01000000
                for c in match.group('prefix'):
                    rex_prefix |= {
                        'W': 1 << 3,
                        'R': 1 << 2,
                        'X': 1 << 1,
                        'B': 1 << 0
                    }[c]

            opcode_str = match.group('opcode').strip('[]').replace(' ', '')
            opcode = [int(a + b, 16) for a, b in zip(opcode_str[0::2], opcode_str[1::2])]
            opcode_size = len(opcode)

            slash = match.group('slash')
            if slash:
                slash = slash[1]

            if slash and ('0' <= slash <= '7'):
                opcode_extension = ord(slash) - ord('0')
            else:
                opcode_extension = -1

            reg_and_rm = slash == 'r'

            immediate = match.group('immediate')
            if immediate:
                immediate_size = {
                    'b': 1,
                    'd': 4,
                    'o': 8,
                }[immediate[1]]
            else:
                immediate_size = -1

            reg_in_opcode = bool(match.group('reg_in_opcode'))

            encoding = Encoding(args, arg_order, rex_prefix, opcode_size, opcode,
                    reg_and_rm, opcode_extension, immediate_size, reg_in_opcode)
            
            # @TODO: We should sort encodings by immediate size (ascending) so
            # that the smallest encoding gets selected automatically.
            if instr_name not in instrs:
                instrs[instr_name] = Instr(instr_name, [])
            instrs[instr_name].encodings.append(encoding)

    output = []
    output.append("""
// @NOTE: This is an automatically generated file! Do not edit it!
//        It was generated from '%s', edit that instead

typedef struct Bytes_
{
    u8 bytes[4];
} Bytes_;

static void assemble_instr(FILE *output_file, AsmModule *asm_module, AsmInstr *instr)
{
\tswitch (instr->op) {
""" % input_filename)

    #pp.pprint(instrs)

    for opcode in instrs:
        output.append("\tcase %s:\n" % opcode)
        for encoding in instrs[opcode].encodings:
            if len(encoding.args) == 0:
                indent = '\t\t'
            else:
                output.append("\t\tif (%s) {\n" % ' && '.join(
                    arg_condition(arg, i) for i, arg in enumerate(encoding.args)))
                indent = '\t\t\t'
            output.append(("%sencode_instr(output_file, asm_module, instr, %s);\n" +
                           "%sreturn;\n")
                    % (indent,
                        ', '.join(map(to_c_val,
                            [encoding.arg_order, encoding.rex_prefix,
                            encoding.opcode_size, encoding.opcode,
                            encoding.reg_and_rm, encoding.opcode_extension,
                            encoding.immediate_size, encoding.reg_in_opcode])),
                        indent))
            if len(encoding.args) != 0:
                output.append("\t\t}\n")

        output.append("\t\tbreak;\n")

    output.append("""
\tdefault: break;
\t}
\t
\tfputs("Unimplemented instruction:\\n", stderr);
\tdump_asm_instr(instr);
\t
\tUNIMPLEMENTED;
}
""")

    with open(output_filename, 'w') as f:
        f.writelines(output)

def arg_condition(arg, i):
    if arg == 'r/m64':
        return ('((instr->args[%d].type == ASM_ARG_REGISTER)'
                + ' || (instr->args[%d].type == ASM_ARG_OFFSET_REGISTER))') % (i, i)
    if arg == 'r64':
        return '(instr->args[%d].type == ASM_ARG_REGISTER) && !instr->args[%d].is_deref' % (i, i)
    if arg == 'imm8':
        return '(is_const_and_fits(instr->args[%d], 8))' % i
    if arg == 'imm32':
        return '(is_const_and_fits(instr->args[%d], 32))' % i
    if arg == 'imm64':
        return '(is_const_and_fits(instr->args[%d], 64))' % i
    if arg == 'sym':
        return ('(instr->args[%d].type == ASM_ARG_LABEL'
                + ' || instr->args[%d].type == ASM_ARG_GLOBAL)') % (i, i)

    print "Unknown arg type: '%s'" % arg
    assert False

def to_c_val(x):
    if isinstance(x, bool):
        return 'true' if x else 'false'
    # Must be in this order as bool is a subclass of int.
    if isinstance(x, int):
        return hex(x) if x >= 0 else str(x)
    if isinstance(x, str):
        return x
    if isinstance(x, list):
        return '((Bytes_){ .bytes = {' + ', '.join(map(to_c_val, x)) + '} }).bytes'

    assert False

if __name__ == '__main__':
    if len(sys.argv) not in (2, 3):
        print "Usage: %s <enc definition> [output file]" % sys.argv[0]
        sys.exit(1)

    # @PORT
    output_filename = "/dev/stdout" if len(sys.argv) == 2 else sys.argv[2]
    generate_encoder(sys.argv[1], output_filename)
