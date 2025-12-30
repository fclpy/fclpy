#!/usr/bin/env python3
"""
Debug script to trace ANSI test loading issues.
This script adds detailed logging to understand what happens during gclload1/2 loading.
"""

import sys
import os
import io

# Add fclpy to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'fclpy'))

from fclpy import runtime, lispenv
import fclpy.lisptype as lisptype
import fclpy.state as state
import fclpy.lispreader as lispreader
from fclpy.readtable import get_current_readtable


def check_package_state(label):
    """Print diagnostic info about current package state."""
    print(f"\n=== {label} ===")
    print(f"state.current_package: {state.current_package}")
    
    # Check what packages exist
    print(f"Known packages: {list(state.packages.keys())}")
    
    # Check CL-TEST package specifically
    cl_test = lisptype.find_package('CL-TEST')
    if cl_test:
        print(f"CL-TEST package found: {cl_test}")
        print(f"  use_packages: {cl_test.use_packages}")
        
        # Check for compile-and-load* symbol
        sym, status = cl_test.find_symbol('COMPILE-AND-LOAD*')
        print(f"  COMPILE-AND-LOAD* in CL-TEST: {sym}, status={status}")
        
        # Check in interned symbols
        if 'COMPILE-AND-LOAD*' in cl_test.symbols:
            print(f"  -> directly interned")
        
        # Check external symbols
        if 'COMPILE-AND-LOAD*' in cl_test.external_symbols:
            print(f"  -> is external")
    else:
        print("CL-TEST package NOT found")
    
    # Check CL-USER package
    cl_user = lisptype.find_package('COMMON-LISP-USER')
    if cl_user:
        sym, status = cl_user.find_symbol('COMPILE-AND-LOAD*')
        print(f"COMPILE-AND-LOAD* in CL-USER: {sym}, status={status}")
        
        # Check if it's bound to a function
        if sym:
            env = state.current_environment
            if env:
                try:
                    func = env.find_function(sym)
                    print(f"  Function binding: {func}")
                except:
                    print(f"  No function binding found")
    
    print(f"===\n")


def main():
    print("=== ANSI Test Debug Script ===\n")
    
    # Setup environment
    lispenv.setup_standard_environment()
    environment = lispenv.current_environment
    
    check_package_state("Initial state")
    
    # Load gclload1.lsp
    print("\n>>> Loading gclload1.lsp...")
    gclload1_path = os.path.abspath("../ansi-test/gclload1.lsp")
    result = runtime.load_and_evaluate_file(gclload1_path, environment, verbose=True)
    print(f"gclload1.lsp result: {result}")
    
    check_package_state("After gclload1.lsp")
    
    # Check for key functions that should be defined
    print("\n>>> Checking for key functions...")
    key_functions = ['NAME', 'PEND', 'PROPS', 'FORM', 'MAKE-ENTRY', 'DEFTEST', 'ADD-ENTRY']
    for fname in key_functions:
        # Check in environment using CL-USER symbol
        sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol(fname)
        func = environment.find_func(sym)
        print(f"  {fname} in CL-USER env: {func}")
        
        # Check in RT package
        rt_pkg = lisptype.find_package('REGRESSION-TEST')
        if rt_pkg:
            rt_sym, status = rt_pkg.find_symbol(fname)
            if rt_sym:
                rt_func = environment.find_func(rt_sym)
                print(f"  {fname} in RT ({status}): {rt_func}")
        
        # Check in CL-TEST package (what the tests use)
        cl_test_pkg = lisptype.find_package('CL-TEST')
        if cl_test_pkg:
            ct_sym, ct_status = cl_test_pkg.find_symbol(fname)
            if ct_sym:
                ct_func = environment.find_func(ct_sym)
                print(f"  {fname} in CL-TEST ({ct_status}): {ct_func}")
            else:
                print(f"  {fname} NOT FOUND in CL-TEST")
    
    # Now try loading gclload2 properly using runtime
    print("\n>>> Testing IN-PACKAGE :CL-TEST...")
    
    # Read and evaluate (in-package :cl-test)
    from fclpy import lispfunc
    
    # Parse expression manually
    string_io = io.StringIO("(in-package :cl-test)")
    stream = lispreader.LispStream(string_io)
    readtable = get_current_readtable()
    reader = lispreader.LispReader(readtable.get_macro_character, stream)
    in_pkg_expr = reader.read_1()
    print(f"Expression: {in_pkg_expr}")
    result = lispfunc.eval(in_pkg_expr, environment)
    print(f"Result: {result}")
    
    check_package_state("After IN-PACKAGE :CL-TEST")
    
    # Now test if compile-and-load* is accessible
    print("\n>>> Testing COMPILE-AND-LOAD* accessibility...")
    
    # Parse compile-and-load* symbol
    string_io = io.StringIO("compile-and-load*")
    stream = lispreader.LispStream(string_io)
    reader = lispreader.LispReader(readtable.get_macro_character, stream)
    test_expr = reader.read_1()
    print(f"Symbol expression: {test_expr}")
    try:
        # Try to find the function
        sym = test_expr
        if isinstance(sym, lisptype.LispSymbol):
            func = environment.find_function(sym)
            print(f"Function found: {func}")
    except Exception as e:
        print(f"Error finding function: {e}")
    
    # Try loading symbols/load.lsp directly
    print("\n>>> Attempting to load symbols/load.lsp...")
    symbols_load_path = os.path.abspath("../ansi-test/symbols/load.lsp")
    
    with open(symbols_load_path, 'r') as f:
        content = f.read()
    print(f"First 500 chars:\n{content[:500]}")
    
    # Parse just the first expression
    string_io = io.StringIO(content)
    stream = lispreader.LispStream(string_io)
    readtable = get_current_readtable()
    reader = lispreader.LispReader(readtable.get_macro_character, stream)
    
    expr = reader.read_1()
    print(f"\nFirst expression: {expr}")
    
    # Check if this is a COMPILE-AND-LOAD* call
    if isinstance(expr, lisptype.lispCons):
        car = expr.car
        print(f"Car of expression: {car} (type: {type(car)})")
        
        if isinstance(car, lisptype.LispSymbol):
            print(f"Symbol name: {car.name}")
            print(f"Symbol package: {car.package}")
            
            # Try to resolve the function
            try:
                func = environment.find_function(car)
                print(f"Function resolved to: {func}")
            except Exception as e:
                print(f"Failed to resolve function: {e}")
    
    try:
        print("\n>>> Evaluating first expression...")
        result = lispfunc.eval(expr, environment)
        print(f"Result: {result}")
    except Exception as e:
        print(f"Error: {e}")
        import traceback
        traceback.print_exc()

    # Continue evaluating remaining expressions
    print("\n>>> Evaluating remaining expressions from symbols/load.lsp...")
    expr_count = 1
    while True:
        try:
            expr = reader.read_1()
            if expr is None:
                break
            expr_count += 1
            print(f"Expression {expr_count}: {expr}")
            # Check if it's an IN-PACKAGE form
            if isinstance(expr, lisptype.lispCons):
                car = expr.car
                if isinstance(car, lisptype.LispSymbol) and car.name == 'IN-PACKAGE':
                    # Get the argument
                    arg = expr.cdr.car if expr.cdr else None
                    print(f"  IN-PACKAGE arg: {arg}, type: {type(arg)}, package: {getattr(arg, 'package', 'N/A')}")
            result = lispfunc.eval(expr, environment)
            print(f"  => {result}")
        except EOFError:
            break
        except Exception as e:
            print(f"  Error: {e}")
            import traceback
            traceback.print_exc()
            break
    
    print(f"\nLoaded {expr_count} expressions from symbols/load.lsp")
    check_package_state("After symbols/load.lsp")


if __name__ == "__main__":
    main()
