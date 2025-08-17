#!/usr/bin/env python3
"""
Comprehensive Test Suite for FCLPY Lisp Interpreter

This consolidated test suite replaces multiple individual test files and provides
comprehensive testing of all major components:
- Function name mapping
- Environment setup  
- Basic evaluation
- Variable bindings
- Special forms
- Metacircular readiness
- Arithmetic operations
"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'fclpy'))

from fclpy.lisptype import LispSymbol, lispCons, Environment, NIL
from fclpy import lispenv, lispfunc

def setup_test_environment():
    """Set up the test environment."""
    lispenv.setup_standard_environment()
    return lispenv.current_environment

class TestResults:
    def __init__(self):
        self.passed = 0
        self.failed = 0
        self.errors = []
    
    def test(self, name, condition, error_msg=None):
        """Record a test result."""
        try:
            if condition:
                print(f"PASS {name}")
                self.passed += 1
            else:
                print(f"FAIL {name}: {error_msg or 'Assertion failed'}")
                self.failed += 1
                if error_msg:
                    self.errors.append(f"{name}: {error_msg}")
        except Exception as e:
            print(f"ERROR {name}: {e}")
            self.failed += 1
            self.errors.append(f"{name}: {e}")
    
    def summary(self):
        """Print test summary."""
        total = self.passed + self.failed
        print(f"\nTest Results: {self.passed}/{total} passed")
        if self.failed > 0:
            print(f"Failures:")
            for error in self.errors:
                print(f"  - {error}")
        return self.failed == 0

def test_function_mapping(results):
    """Test that Lisp function names properly map to Python functions."""
    print("\n=== Function Name Mapping Tests ===")
    
    env = setup_test_environment()
    
    # Test critical mappings that were problematic
    test_cases = [
        ('NOT', 'not_fn'),
        ('<=', '_s_lt__s_eq_'),
        ('>=', '_s_gt__s_eq_'), 
        ('<', '_s_lt_'),
        ('>', '_s_gt_'),
        ('=', '_s_eq_'),
        ('+', '_s_plus_'),
        ('-', '_s_minus_'),
        ('*', '_s_star_'),
        ('/', '_s_slash_'),
        ('CAR', 'car'),
        ('CDR', 'cdr'),
        ('CONS', 'cons'),
    ]
    
    for lisp_name, expected_python_name in test_cases:
        symbol = LispSymbol(lisp_name)
        func = env.find_func(symbol)
        if func:
            results.test(f"Function mapping {lisp_name}", 
                        func.__name__ == expected_python_name,
                        f"Expected {expected_python_name}, got {func.__name__}")
        else:
            results.test(f"Function mapping {lisp_name}", False, "Function not found")

def test_arithmetic_functions(results):
    """Test arithmetic functions work with proper Lisp semantics."""
    print("\n=== Arithmetic Function Tests ===")
    
    env = setup_test_environment()
    
    # Test addition (variadic)
    plus_func = env.find_func(LispSymbol('+'))
    if plus_func:
        result = plus_func(1, 2, 3, 4)
        results.test("Addition (+ 1 2 3 4)", result == 10, f"Expected 10, got {result}")
    
    # Test subtraction (negation and subtraction)
    minus_func = env.find_func(LispSymbol('-'))
    if minus_func:
        result1 = minus_func(5)  # Negation
        results.test("Negation (- 5)", result1 == -5, f"Expected -5, got {result1}")
        
        result2 = minus_func(10, 3, 2)  # Subtraction
        results.test("Subtraction (- 10 3 2)", result2 == 5, f"Expected 5, got {result2}")
    
    # Test comparison
    from fclpy.lisptype import T, NIL
    le_func = env.find_func(LispSymbol('<='))
    if le_func:
        result1 = le_func(3, 5)
        results.test("Comparison (<= 3 5)", result1 == T, f"Expected T, got {result1}")
        
        result2 = le_func(5, 3)
        results.test("Comparison (<= 5 3)", result2 == NIL, f"Expected NIL, got {result2}")

def test_basic_evaluation(results):
    """Test basic evaluation functionality."""
    print("\n=== Basic Evaluation Tests ===")
    
    # Test self-evaluating forms
    results.test("Self-eval number", lispfunc.eval(42) == 42)
    results.test("Self-eval string", lispfunc.eval("hello") == "hello") 
    results.test("Self-eval NIL", lispfunc.eval(NIL) == NIL)
    
    # Test simple function call
    expr = lispCons(LispSymbol('+'), lispCons(2, lispCons(3, NIL)))
    result = lispfunc.eval(expr)
    results.test("Simple function call (+ 2 3)", result == 5, f"Expected 5, got {result}")

def test_special_forms(results):
    """Test special forms."""
    print("\n=== Special Forms Tests ===")
    
    # Test QUOTE
    sym = LispSymbol("X")
    quoted = lispCons(LispSymbol("QUOTE"), lispCons(sym, NIL))
    result = lispfunc.eval(quoted)
    results.test("QUOTE special form", result == sym)
    
    # Test IF
    if_expr = lispCons(LispSymbol("IF"), 
                      lispCons(True, 
                              lispCons(1, lispCons(2, NIL))))
    result = lispfunc.eval(if_expr)
    results.test("IF special form (true case)", result == 1)

def test_metacircular_readiness(results):
    """Test that all functions required for metacircular evaluation are available."""
    print("\n=== Metacircular Readiness Tests ===")
    
    env = setup_test_environment()
    
    required_functions = [
        # Core operations
        'CAR', 'CDR', 'CONS', 'LIST', 'APPEND',
        # Predicates  
        'ATOM', 'NULL', 'EQ', 'NOT',
        # Arithmetic
        '+', '-', '*', '/', '=', '<', '>', '<=', '>=',
        # Type predicates
        'SYMBOLP', 'NUMBERP', 'STRINGP',
        # Special forms (handled by eval)
        'IF', 'LAMBDA', 'DEFUN', 'SETQ',
        # Evaluation
        'EVAL', 'APPLY'
    ]
    
    missing = []
    for func_name in required_functions:
        symbol = LispSymbol(func_name)
        func = env.find_func(symbol)
        if func:
            results.test(f"Required function {func_name}", True)
        else:
            missing.append(func_name)
            results.test(f"Required function {func_name}", False, "Missing")
    
    results.test("All metacircular functions available", 
                len(missing) == 0, 
                f"Missing: {', '.join(missing)}")

def test_environment_operations(results):
    """Test environment variable operations."""
    print("\n=== Environment Tests ===")
    
    # Create test environment
    env = Environment()
    
    # Test variable binding
    var = LispSymbol("X")
    env.add_variable(var, 42)
    
    result = env.find_variable(var)
    results.test("Variable binding", result == 42, f"Expected 42, got {result}")
    
    # Test variable update
    env.set_variable(var, 100)
    result = env.find_variable(var)
    results.test("Variable update", result == 100, f"Expected 100, got {result}")

def main():
    """Run all tests."""
    print("FCLPY Comprehensive Test Suite")
    print("=" * 50)
    
    # Set up the test environment
    print("Setting up test environment...")
    env = setup_test_environment()
    print("Environment setup complete.")
    
    results = TestResults()
    
    # Run all test suites
    test_function_mapping(results)
    test_arithmetic_functions(results)
    test_basic_evaluation(results)
    test_special_forms(results)
    test_environment_operations(results)
    test_metacircular_readiness(results)
    
    # Print final results
    print("\n" + "=" * 50)
    success = results.summary()
    
    if success:
        print("\nALL TESTS PASSED!")
        print("FCLPY is ready for metacircular evaluation!")
        print("\nNext steps:")
        print("1. Load metacircular_evaluator.lisp")
        print("2. Parse and evaluate the Lisp code")
        print("3. Test the Lisp-based evaluator")
    else:
        print("\nSOME TESTS FAILED. Please check the errors above.")
    
    return success

if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
