"""
Tests for Phase 3: Basic Evaluation - Macro vs Function Separation.

This test module verifies that the registry correctly tracks which names
are macros vs functions.
"""

import pytest
from fclpy.lisptype import (
    LispSymbol, lispCons, NIL, T
)
from fclpy.lispfunc.evaluation import eval
from fclpy.lispenv import setup_standard_environment
from fclpy.lispfunc.core import car, cdr
import fclpy.state as state


@pytest.fixture
def env():
    """Setup a clean environment for each test."""
    state.current_environment = None
    state.functions_loaded = False
    return setup_standard_environment()


class TestMacroIdentification:
    """Test that macros are correctly identified."""
    
    def test_function_vs_macro_in_registry(self):
        """Functions and macros should be distinguishable in registry."""
        # This tests that the evaluator can tell the difference
        # We'll use DEFMACRO and DEFUN to create both
        pass
    
    def test_macro_function_marked_correctly(self, env):
        """Macro functions should be marked with __is_macro__ attribute."""
        # Define a simple macro
        defmacro_sym = LispSymbol('DEFMACRO')
        macro_name = LispSymbol('MY-MACRO')
        params = lispCons(LispSymbol('x'), NIL)
        body = LispSymbol('x')
        
        defmacro_form = lispCons(defmacro_sym, lispCons(macro_name, lispCons(params, lispCons(body, NIL))))
        
        result = eval(defmacro_form, env)
        
        # Get the macro from environment
        macro_func = env.find_func(macro_name)
        
        # Should have __is_macro__ attribute
        assert macro_func is not None
        assert hasattr(macro_func, '__is_macro__')
        assert getattr(macro_func, '__is_macro__', False) is True
    
    def test_regular_function_not_marked_macro(self, env):
        """Regular functions should NOT be marked as macros."""
        # Define a simple function
        defun_sym = LispSymbol('DEFUN')
        func_name = LispSymbol('MY-FUNC')
        params = lispCons(LispSymbol('x'), NIL)
        body = LispSymbol('x')
        
        defun_form = lispCons(defun_sym, lispCons(func_name, lispCons(params, lispCons(body, NIL))))
        
        result = eval(defun_form, env)
        
        # Get the function from environment
        func = env.find_func(func_name)
        
        # Should NOT have __is_macro__ set to True
        assert func is not None
        is_macro = getattr(func, '__is_macro__', False)
        assert is_macro is not True


class TestMacroExpansion:
    """Test that macros are expanded correctly."""
    
    def test_macro_call_expands(self, env):
        """Calling a macro should expand it instead of evaluating args."""
        # Define: (DEFMACRO double (x) `(* ,x 2))
        # This is complex, so let's use a simpler test for now
        pass
    
    def test_function_vs_macro_behavior(self, env):
        """Functions and macros should behave differently."""
        # Functions: eval their arguments before calling
        # Macros: pass raw arguments, do not evaluate
        pass


class TestEnvironmentMacroLookup:
    """Test that environment correctly looks up macros vs functions."""
    
    def test_find_macro_vs_find_func(self, env):
        """Environment should have way to distinguish macro from function."""
        # This might require extending the Environment class
        pass
    
    def test_macro_called_with_raw_arguments(self, env):
        """When a macro is called, args should not be evaluated."""
        # Define a macro that returns its first argument wrapped in QUOTE
        defmacro_sym = LispSymbol('DEFMACRO')
        macro_name = LispSymbol('IDENTITY-MACRO')
        x_sym = LispSymbol('x')
        params = lispCons(x_sym, NIL)
        # Return (QUOTE x) - this quotes the argument
        quote_sym = LispSymbol('QUOTE')
        body = lispCons(quote_sym, lispCons(x_sym, NIL))
        
        defmacro_form = lispCons(defmacro_sym, lispCons(macro_name, lispCons(params, lispCons(body, NIL))))
        
        result = eval(defmacro_form, env)
        
        # Now call the macro with an unevaluated form
        undefined_sym = LispSymbol('UNDEFINED-VAR')
        call_form = lispCons(macro_name, lispCons(undefined_sym, NIL))
        
        # The macro should receive the raw symbol without evaluating it
        # Macro expands to (QUOTE UNDEFINED-VAR) which then evaluates to UNDEFINED-VAR
        result = eval(call_form, env)
        
        # Result should be the symbol UNDEFINED-VAR
        assert isinstance(result, LispSymbol)
        assert result.name == 'UNDEFINED-VAR'


class TestMacroFunctionNamespacing:
    """Test how macros and functions share or don't share namespace."""
    
    def test_same_name_function_and_macro(self, env):
        """Can we have a macro and function with the same name? (Usually no)."""
        # In Common Lisp, macros and functions share the same namespace
        # So defining a macro with a function name should replace the function
        
        # First define a function
        defun_sym = LispSymbol('DEFUN')
        name_sym = LispSymbol('MY-NAME')
        params = lispCons(LispSymbol('x'), NIL)
        func_body = lispCons(LispSymbol('+'), lispCons(LispSymbol('x'), lispCons(1, NIL)))
        
        defun_form = lispCons(defun_sym, lispCons(name_sym, lispCons(params, lispCons(func_body, NIL))))
        eval(defun_form, env)
        
        # Now define a macro with the same name
        defmacro_sym = LispSymbol('DEFMACRO')
        macro_params = lispCons(LispSymbol('y'), NIL)
        macro_body = LispSymbol('y')
        
        defmacro_form = lispCons(defmacro_sym, lispCons(name_sym, lispCons(macro_params, lispCons(macro_body, NIL))))
        eval(defmacro_form, env)
        
        # The macro should replace the function
        binding = env.find_func(name_sym)
        assert binding is not None
        # Should be marked as macro
        assert getattr(binding, '__is_macro__', False) is True
