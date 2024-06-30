#!/usr/bin/env python3

# Generates code for encoding assembly instructions (and potentially decoding
# in the future for a disassembler). The format that is parsed matches the
# format used in the Intel architecture manual, read it for further details.

import pprint
import re
import sys
from collections import namedtuple

pp = pprint.PrettyPrinter(indent=4)

Instr = namedtuple("Instr", ["opcode", "encodings"])
Encoding = namedtuple(
    "Encoding",
    [
        "args",
        "arg_order",
        "use_rex_w",
        "use_oso",
        "opcode_size",
        "opcode",
        "reg_and_rm",
        "opcode_extension",
        "immediate_size",
        "reg_in_opcode",
        "fixup_type",
    ],
)


def generate_encoder(input_filename, output_filename):
    instrs = {}

    with open(input_filename, "r") as f:
        for line in f.readlines():
            if line == "\n" or line.startswith("#"):
                continue

            instr, encoding = line.rstrip("\n").split("=")

            instr_components = instr.replace(",", " ").split()
            assert 1 <= len(instr_components) <= 4
            instr_name = instr_components[0]
            args = instr_components[1:]

            if len(args) >= 2:
                if args[1].startswith("r/m"):
                    arg_order = "RM"
                elif args[0].startswith("r/m"):
                    arg_order = "MR"
            else:
                arg_order = "INVALID"

            if "rel" in args:
                fixup_type = "FIXUP_RELATIVE"
            else:
                fixup_type = "FIXUP_ABSOLUTE"

            # @TODO: We treat 'i' immediates the same as 'c' immediates. It's
            # not entirely clear whether they are actually encoded the same, as
            # the manual says that 'i' operands follow the opcode, ModR/M and
            # SIB, and that 'c' operands follow the opcode. Look into this
            # further if there is an opcode with a 'c' immediate and ModR/M or
            # SIB bytes.
            match = re.match(
                r" *(?P<use_rex_w>REX\.W *\+ *)? *"
                + r"(?P<use_oso>OSO *\+ *)? *"
                + r"(?P<opcode>([0-9a-fA-F]+|\[[0-9a-fA-F ]+\])) *"
                + r"(?P<slash>/.)? *"
                + r"(?P<reg_in_opcode>\+r[bwdo])? *"
                + r"(?P<immediate>[ic][bwdo])? *",
                encoding,
            )

            use_rex_w = bool(match.group("use_rex_w"))
            use_oso = bool(match.group("use_oso"))

            opcode_str = match.group("opcode").strip("[]").replace(" ", "")
            opcode = [
                int(a + b, 16) for a, b in zip(opcode_str[0::2], opcode_str[1::2])
            ]
            opcode_size = len(opcode)

            slash = match.group("slash")
            if slash:
                slash = slash[1]

            if slash and ("0" <= slash <= "7"):
                opcode_extension = ord(slash) - ord("0")
            else:
                opcode_extension = -1

            reg_and_rm = slash == "r"

            immediate = match.group("immediate")
            if immediate:
                immediate_size = {
                    "b": 1,
                    "w": 2,
                    "d": 4,
                    "o": 8,
                }[immediate[1]]
            else:
                immediate_size = -1

            reg_in_opcode = bool(match.group("reg_in_opcode"))

            encoding = Encoding(
                args,
                arg_order,
                use_rex_w,
                use_oso,
                opcode_size,
                opcode,
                reg_and_rm,
                opcode_extension,
                immediate_size,
                reg_in_opcode,
                fixup_type,
            )

            # @TODO: We should sort encodings by immediate size (ascending) so
            # that the smallest encoding gets selected automatically.
            if instr_name not in instrs:
                instrs[instr_name] = Instr(instr_name, [])
            instrs[instr_name].encodings.append(encoding)

    output = []
    output.append(
        """
// @NOTE: This is an automatically generated file! Do not edit it!
//        It was generated from '%s', edit that instead

static void assemble_instr(Array(u8) *output, AsmModule *asm_module, AsmInstr *instr)
{
\tswitch (instr->op) {
"""
        % input_filename
    )

    # pp.pprint(instrs)

    for opcode in instrs:
        output.append("\tcase %s:\n" % opcode)
        for encoding in instrs[opcode].encodings:
            if len(encoding.args) == 0:
                indent = "\t\t"
            else:
                output.append(
                    "\t\tif ((instr->arity == %d) && %s) {\n"
                    % (len(encoding.args), arg_conditions(encoding.args))
                )
                indent = "\t\t\t"
            output.append(
                ("%sencode_instr(output, asm_module, instr, %s);\n" + "%sreturn;\n")
                % (
                    indent,
                    ", ".join(
                        map(
                            to_c_val,
                            [
                                encoding.arg_order,
                                encoding.use_rex_w,
                                encoding.use_oso,
                                encoding.opcode_size,
                                encoding.opcode,
                                encoding.reg_and_rm,
                                encoding.opcode_extension,
                                encoding.immediate_size,
                                encoding.reg_in_opcode,
                                encoding.fixup_type,
                            ],
                        )
                    ),
                    indent,
                )
            )
            if len(encoding.args) != 0:
                output.append("\t\t}\n")

        output.append("\t\tbreak;\n")

    output.append(
        """
\tdefault: break;
\t}
\t
\tfputs("Unimplemented instruction:\\n", stderr);
\tdump_asm_instr(instr);
\t
\tUNIMPLEMENTED;
}
"""
    )

    with open(output_filename, "w") as f:
        f.writelines(output)


def check_width(width):
    assert int(width) in [8, 16, 32, 64]


REGISTER_MAP = {
    "AL": ("REG_CLASS_A", 8),
    "AX": ("REG_CLASS_A", 16),
    "EAX": ("REG_CLASS_A", 32),
    "RAX": ("REG_CLASS_A", 64),
    "BL": ("REG_CLASS_B", 8),
    "BX": ("REG_CLASS_B", 16),
    "EBX": ("REG_CLASS_B", 32),
    "RBX": ("REG_CLASS_B", 64),
    "CL": ("REG_CLASS_C", 8),
    "CX": ("REG_CLASS_C", 16),
    "ECX": ("REG_CLASS_C", 32),
    "RCX": ("REG_CLASS_C", 64),
    "DL": ("REG_CLASS_D", 8),
    "DX": ("REG_CLASS_D", 16),
    "EDX": ("REG_CLASS_D", 32),
    "RDX": ("REG_CLASS_D", 64),
    "DIL": ("REG_CLASS_DI", 8),
    "DI": ("REG_CLASS_DI", 16),
    "EDI": ("REG_CLASS_DI", 32),
    "RDI": ("REG_CLASS_DI", 64),
    "SIL": ("REG_CLASS_SI", 8),
    "SI": ("REG_CLASS_SI", 16),
    "ESI": ("REG_CLASS_SI", 32),
    "RSI": ("REG_CLASS_SI", 64),
    "BPL": ("REG_CLASS_BP", 8),
    "BP": ("REG_CLASS_BP", 16),
    "EBP": ("REG_CLASS_BP", 32),
    "RBP": ("REG_CLASS_BP", 64),
    "SPL": ("REG_CLASS_SP", 8),
    "SP": ("REG_CLASS_SP", 16),
    "ESP": ("REG_CLASS_SP", 32),
    "RSP": ("REG_CLASS_SP", 64),
    "R8B": ("REG_CLASS_R8", 8),
    "R8W": ("REG_CLASS_R8", 16),
    "R8D": ("REG_CLASS_R8", 32),
    "R8": ("REG_CLASS_R8", 64),
    "R9B": ("REG_CLASS_R9", 8),
    "R9W": ("REG_CLASS_R9", 16),
    "R9D": ("REG_CLASS_R9", 32),
    "R9": ("REG_CLASS_R9", 64),
    "R10B": ("REG_CLASS_R10", 8),
    "R10W": ("REG_CLASS_R10", 16),
    "R10D": ("REG_CLASS_R10", 32),
    "R10": ("REG_CLASS_R10", 64),
    "R11B": ("REG_CLASS_R11", 8),
    "R11W": ("REG_CLASS_R11", 16),
    "R11D": ("REG_CLASS_R11", 32),
    "R11": ("REG_CLASS_R11", 64),
    "R12B": ("REG_CLASS_R12", 8),
    "R12W": ("REG_CLASS_R12", 16),
    "R12D": ("REG_CLASS_R12", 32),
    "R12": ("REG_CLASS_R12", 64),
    "R13B": ("REG_CLASS_R13", 8),
    "R13W": ("REG_CLASS_R13", 16),
    "R13D": ("REG_CLASS_R13", 32),
    "R13": ("REG_CLASS_R13", 64),
    "R14B": ("REG_CLASS_R14", 8),
    "R14W": ("REG_CLASS_R14", 16),
    "R14D": ("REG_CLASS_R14", 32),
    "R14": ("REG_CLASS_R14", 64),
    "R15B": ("REG_CLASS_R15", 8),
    "R15W": ("REG_CLASS_R15", 16),
    "R15D": ("REG_CLASS_R15", 32),
    "R15": ("REG_CLASS_R15", 64),
}


def arg_conditions(args):
    conditions = []
    ext_width = 0

    for i, arg in enumerate(args):
        arg_str = "instr->args[%d]" % i
        if arg.startswith("r/m"):
            width = arg[3:]
            check_width(width)
            ext_width = width

            conditions.append(
                (
                    "(({0}.t == ASM_VALUE_REGISTER"
                    + " && {0}.u.reg.width == {1})"
                    + " || ({0}.is_deref"
                    + " && {0}.u.reg.width == 64))"
                ).format(arg_str, width)
            )
        elif arg[0] == "r" and all(c.isdigit() for c in arg[1:]):
            width = arg[1:]
            check_width(width)
            ext_width = width

            conditions.append(
                (
                    "({0}.t == ASM_VALUE_REGISTER"
                    + " && {0}.u.reg.width == {1}"
                    + " && !{0}.is_deref)"
                ).format(arg_str, width)
            )
        elif arg.startswith("imm"):
            width = arg[3:]
            check_width(width)
            conditions.append(
                (
                    "(is_const_and_fits({0}, {1}, {2}, "
                    + "is_sign_extending_instr(instr)))"
                ).format(arg_str, ext_width, width)
            )
        elif arg == "rel":
            conditions.append("({0}.t == ASM_VALUE_CONST)".format(arg_str))
        elif arg in REGISTER_MAP:
            reg_class, width = REGISTER_MAP[arg]
            conditions.append(
                (
                    "({0}.t == ASM_VALUE_REGISTER"
                    + " && {0}.u.reg.u.class == {1}"
                    + " && {0}.u.reg.width == {2})"
                ).format(arg_str, reg_class, width)
            )
        else:
            print("Unknown arg type: '%s'" % arg)
            assert False

    return " && ".join(conditions)


def to_c_val(x):
    if isinstance(x, bool):
        return "true" if x else "false"
    # Must be in this order as bool is a subclass of int.
    if isinstance(x, int):
        return hex(x) if x >= 0 else str(x)
    if isinstance(x, str):
        return x
    if isinstance(x, list):
        return "(u8[]){ %s }" % ", ".join(map(to_c_val, x))

    assert False


if __name__ == "__main__":
    if len(sys.argv) not in (2, 3):
        print("Usage: %s <enc definition> [output file]" % sys.argv[0])
        sys.exit(1)

    # @PORT
    output_filename = "/dev/stdout" if len(sys.argv) == 2 else sys.argv[2]
    generate_encoder(sys.argv[1], output_filename)
