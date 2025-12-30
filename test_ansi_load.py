"""Test loading ANSI test files to debug errors."""

import sys
import io
sys.path.insert(0, '.')

import fclpy.lisptype as lisptype
import fclpy.lispfunc as lispfunc
import fclpy.lispreader as lispreader
from fclpy import lispenv
from fclpy.readtable import get_current_readtable

def evaluate_string(expr_str, verbose=False):
    """Helper to evaluate a Lisp expression from string."""
    string_io = io.StringIO(expr_str)
    stream = lispreader.LispStream(string_io)
    readtable = get_current_readtable()
    reader = lispreader.LispReader(readtable.get_macro_character, stream)
    expr = reader.read_1()
    if verbose:
        print(f"Parsed: {expr}")
    return lispfunc.eval(expr, lispenv.current_environment)

# Set up environment
lispenv.setup_standard_environment()
env = lispenv.current_environment

# Test the type constructor pattern that seems to be failing
# The error "Not a function: CHARACTER" suggests the code is trying to call
# CHARACTER as a function to create an instance of a type

print("=== Testing COERCE patterns ===")
test_cases = [
    "(coerce '(1 2 3) 'list)",
    "(coerce \"abc\" 'list)",
    "(coerce '(1 2 3) 'vector)",
    "(typep 3 'integer)",
    "(typep #\\a 'character)",
]
for test in test_cases:
    try:
        result = evaluate_string(test)
        print(f"  {test} => {result}")
    except Exception as e:
        print(f"  {test} => ERROR: {e}")

print("\n=== Testing backquote/unquote patterns ===")
# Test backquote expansion which uses UNQUOTE
test_cases = [
    "'(a b c)",
    "`(a b c)",
    "(let ((x 1)) `(a ,x c))",
]
for test in test_cases:
    try:
        result = evaluate_string(test)
        print(f"  {test} => {result}")
    except Exception as e:
        print(f"  {test} => ERROR: {e}")

print("\n=== Testing MAKE-STRING with problematic patterns ===")
test_cases = [
    "(make-string 5)",
    "(make-string 5 :initial-element #\\x)",
    "(make-string '(5))",  # This might be wrong - list instead of int
]
for test in test_cases:
    try:
        result = evaluate_string(test)
        print(f"  {test} => {result}")
    except Exception as e:
        print(f"  {test} => ERROR: {e}")
