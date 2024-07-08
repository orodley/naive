#!/usr/bin/env python3

import argparse
import fnmatch
import multiprocessing
import os
import shutil
import signal
import subprocess
import sys
import time
from functools import reduce


def make_arg_parser():
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(dest="command")
    test_parser = subparsers.add_parser("test", aliases=["t"], help="Run tests")
    test_parser.add_argument(
        "--silent", "-s", help="Show only the summary", action="store_true"
    )
    test_parser.add_argument(
        "--positive",
        "-p",
        help="Show details even for successful tests",
        action="store_true",
    )
    test_parser.add_argument(
        "--create-testcase",
        "-c",
        help="Record the results for the given tests",
        action="store_true",
    )
    test_parser.add_argument(
        "tests",
        help="The set of tests to run (empty = all)",
        nargs="*",
    )

    build_parser = subparsers.add_parser(
        "build",
        aliases=["b"],
        help="Build the toolchain",
    )

    return parser


def main(args):
    if args.command in {"test", "t"}:
        ret = run_tests(args)
    elif args.command in {"build", "b"}:
        ret = build(args)
    sys.exit(ret)


def run_tests(args):
    if args.create_testcase:
        if len(args.tests) == 0:
            print("Must specify a list of tests if passing '-c'")
            return
        map(create_test_files, args.tests)
        return 0

    test_names = args.tests
    if len(test_names) == 0:
        for root, dirnames, filenames in os.walk("tests"):
            if len(dirnames) == 0:
                test_names.append(root)
    testcases = list(map(make_testcase, test_names))

    num_tests = len(testcases)
    os.chdir(os.path.dirname(os.path.abspath(__file__)))

    print("Running %d tests:" % num_tests)

    # Try to keep n compiler processes running at once, where n = the number of
    # cpu cores on this machine.
    # @NOTE: we don't bother running the binaries produced in parallel, because
    # none of the tests we have so far take a significant amount of time to
    # run. If they start taking a while, we'll want to change this.
    procs = []
    results = []
    for testcase in testcases:
        enqueue_proc(procs, testcase.cmdline, testcase, cwd=testcase.name)
    for returncode, stdout, stderr, (testcase,) in run_all_procs(procs):
        test_result = run_testcase(testcase, returncode, stdout, stderr)
        sys.stdout.write(result_char(test_result))
        sys.stdout.flush()
        results.append(test_result)

    print("\n")

    passes = sum(1 for result in results if result.passed())
    print("%d / %d tests passed" % (passes, num_tests))

    if not args.silent:
        results.sort(key=lambda r: r.name)
        for result in results:
            if args.positive:
                if result.passed():
                    print("test '%s' passed" % result.name)
            elif not result.passed():
                print("\ntest '%s' failed:\n%s" % (result.name, result.error))

    return 0 if passes == num_tests else 1


class Testcase(object):
    __slots__ = [
        "name",
        "cmdline",
        "binary",
        "expected_compile_stdout",
        "expected_compile_stderr",
        "expected_run_stdout",
        "expected_run_stderr",
        "run_stdin",
    ]


def make_testcase(test_dir):
    testcase = Testcase()
    testcase.name = test_dir

    sub_files = os.listdir(test_dir)

    testcase.expected_compile_stdout = b""
    testcase.expected_compile_stderr = b""
    testcase.expected_run_stdout = b""
    testcase.expected_run_stderr = b""
    testcase.run_stdin = b""

    test_filenames = []
    extra_flags = []
    for filename in sub_files:
        with open(os.path.join(test_dir, filename), "rb") as f:
            contents = f.read()

        if filename == "compile_stdout":
            testcase.expected_compile_stdout = contents
        elif filename == "compile_stderr":
            testcase.expected_compile_stderr = contents
        elif filename == "run_stdout":
            testcase.expected_run_stdout = contents
        elif filename == "run_stderr":
            testcase.expected_run_stderr = contents
        elif filename == "run_stdin":
            testcase.run_stdin = contents
        elif filename.endswith(".c"):
            test_filenames.append(filename)
            first_line = contents[: contents.index(b"\n")].decode()
            flags_str = "// FLAGS:"
            if first_line.startswith(flags_str):
                extra_flags = first_line[len(flags_str) :].strip().split(" ")

    assert test_filenames != []

    testcase.binary = os.path.abspath(os.path.join(test_dir, "a.out.tmp"))
    testcase.cmdline = (
        [os.path.abspath(get_cc()), "-o", testcase.binary]
        + extra_flags
        + test_filenames
    )
    return testcase


def create_test_files(test_dir):
    testcase = make_testcase(test_dir)
    cc_proc = subprocess.Popen(
        testcase.cmdline, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=test_dir
    )
    compile_stdout, compile_stderr = cc_proc.communicate()

    run_stdout = ""
    run_stderr = ""
    binary_path = os.path.abspath(testcase.binary)
    if testcase.cc_proc.returncode == 0 and os.path.exists(binary_path):
        program_proc = subprocess.Popen(
            binary_path,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            stdin=subprocess.PIPE,
            cwd=os.path.abspath(test_dir),
        )
        run_stdout, run_stderr = program_proc.communicate(testcase.run_stdin)
        os.remove(binary_path)

    def create_file(contents, filename):
        if contents != "":
            with open(os.path.join(test_dir, filename), "w") as f:
                f.write(contents)

    create_file(compile_stdout, "compile_stdout")
    create_file(compile_stderr, "compile_stderr")
    create_file(run_stdout, "run_stdout")
    create_file(run_stderr, "run_stderr")


class TestResult(object):
    __slots__ = ["name", "error"]

    def passed(self):
        return self.error == ""


def run_testcase(testcase, compile_returncode, compile_stdout, compile_stderr):
    test_result = TestResult()
    test_result.name = testcase.name
    test_result.error = ""

    compiled_successfully = compile_returncode == 0
    # Non-empty stderr = expected compile failure
    if testcase.expected_compile_stderr != b"":
        if testcase.expected_compile_stderr != compile_stderr:
            if compiled_successfully:
                os.remove(testcase.binary)
                test_result.error = "compilation succeeded when expected to fail"
            else:
                test_result.error = "expected compile stderr:\n%s\ngot:\n%s" % (
                    indent_bytes(testcase.expected_compile_stderr),
                    indent_bytes(compile_stderr),
                )
            return test_result
        else:
            return test_result

    if compile_stdout != testcase.expected_compile_stdout:
        test_result.error = "expected compile stdout:\n%s\ngot:\n%s" % (
            indent_bytes(testcase.expected_compile_stdout),
            indent_bytes(compile_stdout),
        )

    if not compiled_successfully:
        test_result.error = "compilation failed with stderr:\n" + indent_bytes(
            compile_stderr
        )
        return test_result

    binary_path = os.path.abspath(testcase.binary)
    if compiled_successfully and not os.path.exists(binary_path):
        return test_result

    test_dir = os.path.abspath(testcase.name)
    program_proc = subprocess.Popen(
        binary_path,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        stdin=subprocess.PIPE,
        cwd=test_dir,
    )
    run_stdout, run_stderr = program_proc.communicate(testcase.run_stdin)
    status_code = program_proc.returncode
    os.remove(binary_path)

    if status_code != 0:
        test_result.error = "non-zero return code %d" % status_code
    if run_stdout != testcase.expected_run_stdout:
        test_result.error = "expected runtime stdout of %r, got %r" % (
            testcase.expected_run_stdout,
            run_stdout,
        )
    if run_stderr != testcase.expected_run_stderr:
        test_result.error = "expected runtime stderr of %r, got %r" % (
            testcase.expected_run_stderr,
            run_stderr,
        )

    return test_result


def indent_bytes(bs):
    return indent(bs.decode())


def indent(string):
    return "\n".join("    " + line for line in string.split("\n"))


# @TODO: check if we're on a platform that doesn't support ANSI colors
green = "\033[92m"
red = "\033[91m"
reset_color = "\033[0m"


def result_char(test_result):
    if test_result.passed():
        return green + "." + reset_color
    else:
        return red + "F" + reset_color


def partition(l, pred):
    true = []
    false = []
    for x in l:
        if pred(x):
            true.append(x)
        else:
            false.append(x)

    return true, false


def build(args):
    cc = [get_cc()]
    ASM = "nasm"
    AR = "ar"
    if program_exists("ccache"):
        cc = ["ccache"] + cc
    COMMON_CFLAGS = [
        "-Isrc",
        "-Ibuild",
        "-std=c99",
        "-Werror",
        "-Wall",
        "-Wextra",
        "-Wstrict-prototypes",
        "-Wformat",
    ]
    LIBC_CFLAGS = [
        "-fno-asynchronous-unwind-tables",
        "-ffreestanding",
        "-fno-common",
        "-Ilibc",
        "-Ilibc/include",
    ]

    os.chdir(os.path.dirname(os.path.realpath(__file__)))
    os.makedirs("build/toolchain", exist_ok=True)
    os.makedirs("build/bin", exist_ok=True)
    os.makedirs("build/libc", exist_ok=True)

    # Run the metaprograms
    procs = []
    enqueue_proc(procs, ["meta/peg.py", "src/parse.peg", "build/parse.inc"], "PEG")
    enqueue_proc(procs, ["meta/enc.py", "src/x64.enc", "build/x64.inc"], "INC")
    if (ret := run_all_procs_printing_failures(procs)) != 0:
        return ret

    deps_for_bin = {
        "src/bin/ncc": [
            "src/bin/ncc.o",
            "src/array.o",
            "src/asm.o",
            "src/asm_gen.o",
            "src/bit_set.o",
            "src/diagnostics.o",
            "src/elf.o",
            "src/file.o",
            "src/ir.o",
            "src/ir_gen.o",
            "src/parse.o",
            "src/pool.o",
            "src/preprocess.o",
            "src/reader.o",
            "src/tokenise.o",
            "src/util.o",
        ],
        "src/bin/nas": [
            "src/bin/nas.o",
            "src/reader.o",
            "src/util.o",
            "src/diagnostics.o",
            "src/asm.o",
            "src/elf.o",
            "src/pool.o",
            "src/file.o",
            "src/array.o",
        ],
        "src/bin/nar": ["src/bin/nar.o", "src/array.o", "src/file.o", "src/util.o"],
    }

    procs = []
    for dep in reduce(set.union, map(set, deps_for_bin.values())):
        enqueue_proc(
            procs,
            cc
            + COMMON_CFLAGS
            + ["-c"]
            + ["-o", dep.replace("src/", "build/")]
            + [dep.replace(".o", ".c")],
        )
    if (ret := run_all_procs_printing_failures(procs)) != 0:
        return ret

    procs = []
    for bin, deps in deps_for_bin.items():
        enqueue_proc(
            procs,
            cc
            + COMMON_CFLAGS
            + ["-o", bin.replace("src/bin/", "build/toolchain/")]
            + [d.replace("src/", "build/") for d in deps],
        )
    if (ret := run_all_procs_printing_failures(procs)) != 0:
        return ret

    shutil.copytree("freestanding", "build/toolchain/freestanding", dirs_exist_ok=True)
    shutil.copytree("libc/include", "build/toolchain/include", dirs_exist_ok=True)

    procs = []
    libc_objs = []
    for libc_source_file in os.listdir("libc"):
        libc_source_file = f"libc/{libc_source_file}"
        if libc_source_file.endswith(".c"):
            obj = f"build/{libc_source_file.replace('.c', '.o')}"
            enqueue_proc(
                procs,
                ["build/toolchain/ncc"]
                + COMMON_CFLAGS
                + LIBC_CFLAGS
                + ["-c", "-o", obj, libc_source_file],
            )
        elif libc_source_file.endswith(".s"):
            obj = f"build/{libc_source_file.replace('.s', '.o')}"
            enqueue_proc(
                procs,
                [
                    ASM,
                    "-f",
                    "elf64",
                    "-o",
                    obj,
                    libc_source_file,
                ],
            )
        else:
            continue
        libc_objs.append(obj)

    if (ret := run_all_procs_printing_failures(procs)) != 0:
        return ret

    procs = []
    enqueue_proc(procs, [AR, "-cr", "build/toolchain/libc.a"] + libc_objs)
    if (ret := run_all_procs_printing_failures(procs)) != 0:
        return ret

    return 0


def enqueue_proc(procs, cmdline, *proc_info, **kwargs):
    procs.append((cmdline, proc_info, kwargs))


def run_all_procs(procs):
    parallel_procs = multiprocessing.cpu_count()
    running_procs = []
    while procs or running_procs:
        done, running_procs = partition(
            running_procs, lambda proc: proc[0].poll() is not None
        )

        while len(running_procs) < parallel_procs and len(procs) > 0:
            cmdline, proc_info, kwargs = procs.pop()
            running_procs.append(
                (
                    subprocess.Popen(
                        cmdline,
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE,
                        **kwargs,
                    ),
                    proc_info,
                )
            )

        if len(done) == 0:
            time.sleep(0.05)

        for subproc, info in done:
            stdout, stderr = subproc.communicate()
            yield subproc.returncode, stdout, stderr, info


def run_all_procs_printing_failures(procs):
    overall_ret_code = 0
    for returncode, stdout, stderr, _ in run_all_procs(procs):
        if returncode != 0:
            overall_ret_code = returncode
            print(stderr.decode(), file=sys.stderr)
    return overall_ret_code


def program_exists(name):
    return shutil.which(name) is not None


def get_cc():
    if (cc := os.getenv("CC")) is not None:
        return cc
    if program_exists("clang"):
        return "clang"
    if program_exists("gcc"):
        return "gcc"
    raise Exception("No C compiler could be found")


def sigint_handler(signal, frame):
    # If we get Ctrl-C in between finishing compiling a test and running it,
    # the binary is still there.
    for root, dirnames, filenames in os.walk("tests"):
        for filename in fnmatch.filter(filenames, "a.out.tmp"):
            os.remove(os.path.join(root, filename))

    sys.exit(0)


if __name__ == "__main__":
    signal.signal(signal.SIGINT, sigint_handler)
    main(make_arg_parser().parse_args())
