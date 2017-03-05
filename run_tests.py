#!/usr/bin/env python2

import fnmatch
import multiprocessing
import os
import subprocess
import signal
import sys
import time
import uuid

def main():
    print_details = True
    be_positive = False
    create_testcase = False
    tests = []
    for arg in sys.argv[1:]:
        if arg == '-s':
            print_details = False
        elif arg == '-p':
            be_positive = True
        elif arg == '-c':
            create_testcase = True
        else:
            tests.append(arg)
    if not print_details and be_positive:
        print "Cannot specify '-s' and '-p'"
        return
    if create_testcase:
        if len(tests) == 0:
            print "Must specify a list of tests if passing '-c'"
            return
        map(create_test_files, tests)
        return

    if len(tests) == 0:
        for root, dirnames, filenames in os.walk('tests'):
            if len(dirnames) == 0:
                tests.append(root)

    num_tests = len(tests)
    os.chdir(os.path.dirname(os.path.abspath(__file__)))

    print "Running %d tests:" % num_tests

    # Try to keep n compiler processes running at once, where n = the number of
    # cpu cores on this machine.
    # @NOTE: we don't bother running the binaries produced in parallel, because
    # none of the tests we have so far take a significant amount of time to
    # run. If they start taking a while, we'll want to change this.
    parallel_testcases = multiprocessing.cpu_count()
    running_testcases = []
    results = []
    while len(results) != num_tests:
        done, running_testcases = partition(running_testcases,
                lambda testcase: testcase.cc_proc.poll() is not None)

        while len(running_testcases) < parallel_testcases and len(tests) > 0:
            new_test = tests.pop()
            running_testcases.append(start_compiling(new_test))

        if len(done) == 0:
            time.sleep(0.05)

        for testcase in done:
            test_result = run_testcase(testcase)
            sys.stdout.write(result_char(test_result))
            sys.stdout.flush()
            results.append(test_result)

    print "\n"

    passes = sum(1 for result in results if result.passed())
    print "%d / %d tests passed" % (passes, num_tests)

    if print_details:
        results.sort(key=lambda r: r.name)
        for result in results:
            if be_positive:
                if result.passed():
                    print "test '%s' passed" % result.name
            elif not result.passed():
                print "\ntest '%s' failed:\n%s" % (result.name, result.error)

class Testcase(object):
    __slots__ = ['name', 'binary', 'cc_proc',
            'expected_compile_stdout', 'expected_compile_stderr',
            'expected_run_stdout', 'expected_run_stderr', 'run_stdin']

def start_compiling(test_dir):
    testcase = Testcase()
    testcase.name = test_dir

    sub_files = os.listdir(test_dir)

    testcase.expected_compile_stdout = ''
    testcase.expected_compile_stderr = ''
    testcase.expected_run_stdout = ''
    testcase.expected_run_stderr = ''
    testcase.run_stdin = ''

    test_filenames = []
    extra_flags = []
    for filename in sub_files:
        with open(os.path.join(test_dir, filename), 'r') as f:
            contents = f.read()

        if filename == 'compile_stdout':
            testcase.expected_compile_stdout = contents
        elif filename == 'compile_stderr':
            testcase.expected_compile_stderr = contents
        elif filename == 'run_stdout':
            testcase.expected_run_stdout = contents
        elif filename == 'run_stderr':
            testcase.expected_run_stderr = contents
        elif filename == 'run_stdin':
            testcase.run_stdin = contents
        elif filename.endswith('.c'):
            test_filenames.append(filename)
            first_line = contents[:contents.index('\n')]
            flags_str = '// FLAGS:'
            if first_line.startswith(flags_str):
                extra_flags = first_line[len(flags_str):].strip().split(' ')

    assert test_filenames != []

    testcase.binary = os.path.abspath(os.path.join(test_dir, "a.out.tmp"))
    testcase.cc_proc = subprocess.Popen(
            [os.path.abspath('./ncc'), '-o', testcase.binary] + extra_flags + test_filenames,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd=test_dir)
    return testcase

def create_test_files(test_dir):
    testcase = start_compiling(test_dir)
    compile_stdout, compile_stderr = testcase.cc_proc.communicate()

    run_stdout = ''
    run_stderr = ''
    binary_path = os.path.abspath(testcase.binary)
    if testcase.cc_proc.returncode == 0 and os.path.exists(binary_path):
        program_proc = subprocess.Popen(binary_path,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.PIPE,
            cwd=os.path.abspath(test_dir))
        run_stdout, run_stderr = program_proc.communicate(testcase.run_stdin)
        os.remove(binary_path)

    def create_file(contents, filename):
        if contents != '':
            with open(os.path.join(test_dir, filename), 'w') as f:
                f.write(contents)

    create_file(compile_stdout, 'compile_stdout')
    create_file(compile_stderr, 'compile_stderr')
    create_file(run_stdout, 'run_stdout')
    create_file(run_stderr, 'run_stderr')

class TestResult(object):
    __slots__ = ['name', 'error']

    def passed(self):
        return self.error == ''


def run_testcase(testcase):
    test_result = TestResult()
    test_result.name = testcase.name
    test_result.error = ''
    cc_proc = testcase.cc_proc

    # @TODO: This seems to hang forever if cc produces loads of output (like if
    # we have some debugging stuff in there.
    compile_stdout, compile_stderr = cc_proc.communicate()
    compiled_successfully = cc_proc.returncode == 0
    # Non-empty stderr = expected compile failure
    if testcase.expected_compile_stderr != '':
        if testcase.expected_compile_stderr != compile_stderr:
            if compiled_successfully:
                os.remove(testcase.binary)
                test_result.error = "compilation succeeded when expected to fail"
            else:
                test_result.error = "expected compile stderr:\n%s\ngot:\n%s" \
                    % (indent(testcase.expected_compile_stderr), indent(compile_stderr))
            return test_result
        else:
            return test_result
    if compile_stdout != testcase.expected_compile_stdout:
        test_result.error = "expected compile stdout:\n%s\ngot:\n%s" \
                % (indent(testcase.expected_compile_stdout), indent(compile_stdout))

    if not compiled_successfully:
        test_result.error = "compilation failed with stderr:\n" \
            + indent(compile_stderr)
        return test_result

    binary_path = os.path.abspath(testcase.binary)
    if compiled_successfully and not os.path.exists(binary_path):
        return test_result

    test_dir = os.path.abspath(testcase.name)
    program_proc = subprocess.Popen(binary_path,
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, stdin=subprocess.PIPE,
        cwd=test_dir)
    run_stdout, run_stderr = program_proc.communicate(testcase.run_stdin)
    status_code = program_proc.returncode
    os.remove(binary_path)

    if status_code != 0:
        test_result.error = "non-zero return code %d" % status_code
    if run_stdout != testcase.expected_run_stdout:
        test_result.error = "expected runtime stdout of %r, got %r" % \
            (testcase.expected_run_stdout, run_stdout)
    if run_stderr != testcase.expected_run_stderr:
        test_result.error = "expected runtime stderr of %r, got %r" % \
            (testcase.expected_run_stderr, run_stderr)

    return test_result

def indent(string):
    return '\n'.join("    " + line for line in string.split('\n'))

# @TODO: check if we're on a platform that doesn't support ANSI colors
green = '\033[92m'
red = '\033[91m'
reset_color = '\033[0m'

def result_char(test_result):
    if test_result.passed():
        return green + '.' + reset_color
    else:
        return red + 'F' + reset_color

def partition(l, pred):
    true = []
    false = []
    for x in l:
        if pred(x):
            true.append(x)
        else:
            false.append(x)

    return true, false

def sigint_handler(signal, frame):
    # If we get Ctrl-C in between finishing compiling a test and running it,
    # the binary is still there.
    for root, dirnames, filenames in os.walk('tests'):
        for filename in fnmatch.filter(filenames, 'a.out.tmp'):
            os.remove(os.path.join(root, filename))

    sys.exit(0)

if __name__ == "__main__":
    signal.signal(signal.SIGINT, sigint_handler)
    main()
