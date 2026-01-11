#!/usr/bin/env python3
"""Test upgraded-complex-part-type function."""

import sys
sys.path.insert(0, 'fclpy')

from fclpy.lispenv import setup_standard_environment
from fclpy.reader import read_all
from fclpy.lispfunc.evaluation import eval

env = setup_standard_environment()

# Test the function exists
print("Testing upgraded-complex-part-type...")
code = "(upgraded-complex-part-type 'integer)"
forms = read_all(code)
result = eval(forms[0], env)
print(f"Result: {result}")
print(f"Result type: {type(result)}")

# Test if the macro expands properly
print("\nTesting def-ucpt-test macro...")
macro_code = """
(defmacro def-ucpt-test (name types)
  `(deftest ,name
     (loop for type in (remove-duplicates ,types)
           for upgraded-type = (upgraded-complex-part-type type)
           for result = (append (check-all-subtypep type upgraded-type))
           when result
           collect result)
     nil))
"""
macro_forms = read_all(macro_code)
result = eval(macro_forms[0], env)
print(f"Macro defined: {result}")

# Try to use the macro
use_code = """(def-ucpt-test test-1 '(integer float))"""
use_forms = read_all(use_code)
try:
    result = eval(use_forms[0], env)
    print(f"Macro expansion result: {result}")
except Exception as e:
    print(f"Error: {e}")
