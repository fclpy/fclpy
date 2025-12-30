#!/usr/bin/env python3
"""Debug package/symbol visibility"""

import sys
sys.path.insert(0, '.')
from fclpy import lispfunc, lisptype, state
from fclpy.reader import read, read_all
from fclpy.lisptype_extended import Environment

# Create global env
env = Environment()

def load_file(filename, env, verbose=False):
    """Load file and catch errors"""
    try:
        with open(filename, 'r') as f:
            content = f.read()
    except FileNotFoundError:
        print(f'File not found: {filename}')
        return False
    
    print(f'Loading: {filename}')
    try:
        exprs = read_all(content)
        expr_count = 0
        for expr in exprs:
            expr_count += 1
            try:
                result = lispfunc.eval(expr, env)
            except Exception as e:
                print(f'  Error at expr {expr_count}: {e}')
                if verbose:
                    print(f'  Expression: {expr}')
                # Continue
        print(f'  Loaded {expr_count} expressions')
        return True
    except Exception as e:
        print(f'  Read error: {e}')
        return False

# Load the base files
base = '../ansi-test/'
load_file(base + 'compile-and-load.lsp', env)
load_file(base + 'rt-package.lsp', env)
load_file(base + 'rt.lsp', env)
load_file(base + 'cl-test-package.lsp', env)

# Check what's exported from RT
print("\n=== Checking packages ===")
rt_pkg = lisptype.find_package('REGRESSION-TEST')
if rt_pkg:
    print(f"RT package found: {rt_pkg.name}")
    print(f"  External symbols (raw): {list(rt_pkg.external_symbols)[:5]}...")
    print(f"  Symbol 'DEFTEST' in external_symbols: {'DEFTEST' in rt_pkg.external_symbols}")
    print(f"  Symbol '#:DEFTEST' in external_symbols: {'#:DEFTEST' in rt_pkg.external_symbols}")
    # Look at actual symbols
    print(f"  All symbols dict keys: {list(rt_pkg.symbols.keys())[:5]}...")
    if 'DEFTEST' in rt_pkg.symbols:
        sym = rt_pkg.symbols['DEFTEST']
        print(f"  DEFTEST symbol: {sym}, name='{sym.name}', pkg={sym.package}")
else:
    print("RT package NOT found")

cl_test_pkg = lisptype.find_package('CL-TEST')
if cl_test_pkg:
    print(f"CL-TEST package found: {cl_test_pkg.name}")
    print(f"  Uses packages: {[p.name for p in cl_test_pkg.use_packages]}")
else:
    print("CL-TEST package NOT found")

# Test symbol lookup
print("\n=== Testing symbol lookup ===")
# In CL-TEST package
if cl_test_pkg:
    state.current_package = cl_test_pkg
    sym = lisptype.intern_symbol('DEFTEST', cl_test_pkg)
    print(f"DEFTEST in CL-TEST: {sym}")
    # Check if it's the same as the one in RT
    if rt_pkg:
        rt_sym = lisptype.intern_symbol('DEFTEST', rt_pkg)
        print(f"DEFTEST in RT: {rt_sym}")
        print(f"Same symbol? {sym is rt_sym}")

# Try to find DEFTEST via find_symbol
if cl_test_pkg:
    found = lisptype.find_symbol('DEFTEST', cl_test_pkg)
    print(f"find_symbol('DEFTEST', CL-TEST): {found}")
