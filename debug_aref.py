#!/usr/bin/env python
"""Debug AREF error - find which file and expression causes it."""

import sys
import os
import io
sys.path.insert(0, '.')

import fclpy.lispfunc as lispfunc
import fclpy.lispreader as lispreader
from fclpy import lispenv
from fclpy.readtable import get_current_readtable

# Initialize
lispenv.setup_standard_environment()
env = lispenv.current_environment

# We need to load files in order to find where AREF error occurs
# gclload1.lsp loads: compile-and-load.lsp, rt-package.lsp, rt.lsp, cl-test-package.lsp, etc.

def load_file(filename, env, verbose=False):
    """Load and evaluate a file, reporting errors."""
    print(f"Loading: {filename}")
    
    with open(filename, 'r') as f:
        content = f.read()
    
    string_io = io.StringIO(content)
    stream = lispreader.LispStream(string_io)
    readtable = get_current_readtable()
    reader = lispreader.LispReader(readtable.get_macro_character, stream)
    
    expr_count = 0
    while True:
        try:
            expr = reader.read_1()
            if expr is None:
                break
            expr_count += 1
            try:
                result = lispfunc.eval(expr, env)
            except Exception as e:
                error_str = str(e)
                if 'AREF' in error_str and 'Not implemented' not in error_str:
                    print(f'  FOUND AREF ERROR at expr {expr_count}!')
                    print(f'  Expression: {repr(expr)[:500]}')
                    import traceback
                    traceback.print_exc()
                    return False
                if verbose:
                    print(f'  Error at expr {expr_count}: {e}')
        except Exception as e:
            print(f'  Read error at expr {expr_count + 1}: {e}')
            break
    
    print(f'  Loaded {expr_count} expressions')
    return True

# Load files in order
base = '../ansi-test/'
files_to_load = [
    'compile-and-load.lsp',
    'rt-package.lsp',
    'rt.lsp',
    'cl-test-package.lsp',
    'auxiliary/ansi-aux-macros.lsp',
    'universe.lsp',
    'auxiliary/random-aux.lsp',
    'auxiliary/ansi-aux.lsp',
    'cl-symbol-names.lsp',
    'notes.lsp',
]

for f in files_to_load:
    path = base + f
    if os.path.exists(path):
        if not load_file(path, env, verbose=False):
            print(f"Stopped at {f}")
            break
    else:
        print(f"File not found: {path}")
