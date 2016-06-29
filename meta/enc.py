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
        ['args', 'arg_order', 'rex_prefix', 'opcode_num', 'reg_and_rm', 'opcode_extension',
         'immediate_size', 'reg_in_opcode'])

def generate_encoder(input_filename, output_filename):
    instrs = {}

    with open(input_filename, 'r') as f:
        for line in f.readlines():
            if line == '\n':
                continue

            instr, encoding = line.rstrip('\n').split('=')

            instr_components = instr.replace(',', ' ').split()
            assert 1 <= len(instr_components) <= 3
            opcode = instr_components[0]
            args = instr_components[1:]

            if args == ['r64', 'r/m64']:
                arg_order = 'RM'
            elif args == ['r/m64', 'r64']:
                arg_order = 'MR'
            else:
                arg_order = 'INVALID'

            match = re.match(
                    r' *(?:REX\.(?P<prefix>[WRXB]+) *\+ *)?' +
                    r'(?P<opcode>[0-9a-fA-F]+) *' +
                    r'(?P<slash>/.)? *' +
                    r'(?P<immediate>ib)? *' +
                    r'(?P<reg_in_opcode>\+rd)? *',
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

            opcode_num = int(match.group('opcode'), 16)

            slash = match.group('slash')
            if slash:
                slash = slash[1]

            if slash and ('0' <= slash <= '7'):
                opcode_extension = ord(slash) - ord('0')
            else:
                opcode_extension = -1

            reg_and_rm = slash == 'r'

            if match.group('immediate'):
                immediate_size = 8
            else:
                immediate_size = -1

            reg_in_opcode = bool(match.group('reg_in_opcode'))

            encoding = Encoding(args, arg_order, rex_prefix, opcode_num,
                    reg_and_rm, opcode_extension, immediate_size, reg_in_opcode)
            
            # @TODO: We should sort encodings by immediate size (ascending) so
            # that the smallest encoding gets selected automatically.
            if opcode not in instrs:
                instrs[opcode] = Instr(opcode, [])
            instrs[opcode].encodings.append(encoding)

    output = []
    output.append("""
// @NOTE: This is an automatically generated file! Do not edit it!
//        It was generated from '%s', edit that instead

static u32 assemble_instr(FILE *output_file, AsmInstr *instr)
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
                output.append("\t\tif (%s)" % ' && '.join(
                    arg_condition(arg, i) for i, arg in enumerate(encoding.args)))
                indent = '\n\t\t\t'
            output.append("%sreturn encode_instr(output_file, instr, %s);\n"
                    % (indent,
                        ', '.join(map(to_c_val,
                            [encoding.arg_order, encoding.rex_prefix,
                            encoding.opcode_num, encoding.reg_and_rm,
                            encoding.opcode_extension, encoding.immediate_size,
                            encoding.reg_in_opcode]))))

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
        return '((instr->args[%d].type == REGISTER) || (instr->args[%d].type == OFFSET_REGISTER))' % (i, i)
    if arg == 'r64':
        return '(instr->args[%d].type == REGISTER) && !instr->args[%d].is_deref' % (i, i)
    if arg == 'imm8':
        return '(is_const_and_fits(instr->args[%d], 8))' % i

    assert False

def to_c_val(x):
    if isinstance(x, bool):
        return 'true' if x else 'false'
    # Must be in this order as bool is a subclass of int.
    if isinstance(x, int):
        return hex(x) if x >= 0 else str(x)
    if isinstance(x, str):
        return x

    assert False

if __name__ == '__main__':
    if len(sys.argv) not in (2, 3):
        print "Usage: %s <enc definition> [output file]" % sys.argv[0]
        sys.exit(1)

    # @PORT
    output_filename = "/dev/stdout" if len(sys.argv) == 2 else sys.argv[2]
    generate_encoder(sys.argv[1], output_filename)
