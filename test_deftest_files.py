#!/usr/bin/env python3
"""Test DEFTEST by loading actual Lisp files like the ANSI test suite does."""

import sys
import os

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from fclpy.runtime import load_and_evaluate_file

print("=" * 70)
print("DEFTEST TEST WITH ACTUAL LISP FILES")
print("=" * 70)

# Change to the fclpy directory so relative paths work
os.chdir(os.path.dirname(os.path.abspath(__file__)))

print("\nMethod 1: Load mini_init.lsp (which loads rt.lsp, cl-test-package.lsp, test_deftest.lsp)")
print("-" * 70)

try:
    results = load_and_evaluate_file('test_files/mini_init.lsp', verbose=True)
    print(f"\nSuccess! Results: {results}")
except Exception as e:
    print(f"\nFailed with error: {e}")
    import traceback
    traceback.print_exc()

print("\n" + "=" * 70)
print("Method 2: Load files individually in sequence")
print("=" * 70)

# Reset state
import importlib
import fclpy.lisptype as lisptype
import fclpy.lispenv as lispenv

# Re-setup environment
lispenv.setup_standard_environment()
env = lispenv.current_environment

print("\n1. Loading rt.lsp...")
try:
    load_and_evaluate_file('../ansi-test/rt.lsp', environment=env, verbose=False)
    print("   rt.lsp loaded successfully")
    
    # Check DEFTEST
    rt_pkg = lisptype.find_package('REGRESSION-TEST')
    if rt_pkg:
        result = rt_pkg.find_symbol('DEFTEST')
        deftest_sym = result[0] if isinstance(result, tuple) else result
        if deftest_sym:
            func = env.find_func(deftest_sym)
            print(f"   DEFTEST symbol id: {hex(id(deftest_sym))}")
            print(f"   DEFTEST macro found: {func is not None}")
            if func:
                print(f"   __is_macro__: {getattr(func, '__is_macro__', False)}")
except Exception as e:
    print(f"   Error: {e}")

print("\n2. Loading cl-test-package.lsp...")
try:
    load_and_evaluate_file('../ansi-test/cl-test-package.lsp', environment=env, verbose=False)
    print("   cl-test-package.lsp loaded successfully")
    
    # Check CL-TEST package
    cl_test = lisptype.find_package('CL-TEST')
    if cl_test:
        print(f"   CL-TEST uses: {[p.name for p in cl_test.use_packages]}")
except Exception as e:
    print(f"   Error: {e}")

print("\n3. Loading test_deftest.lsp (this uses DEFTEST macro)...")
try:
    load_and_evaluate_file('test_files/test_deftest.lsp', environment=env, verbose=True)
    print("   test_deftest.lsp loaded successfully!")
except Exception as e:
    print(f"   Error: {e}")
    import traceback
    traceback.print_exc()

print("\n" + "=" * 70)
print("TEST COMPLETE")
print("=" * 70)
