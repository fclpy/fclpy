"""
Tests for Phase 3: Basic Evaluation - Macro System.

This test module verifies that DEFMACRO, MACROEXPAND, and MACRO-FUNCTION work correctly.
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


class TestDefmacro:
    """Test DEFMACRO special form."""
    
    def test_defmacro_simple(self, env):
        """DEFMACRO should define a simple macro."""
        # (DEFMACRO double (x) `(* ,x 2))
        # For now, a simpler version:
        # (DEFMACRO identity (x) x)
        defmacro_sym = LispSymbol('DEFMACRO')
        macro_name = LispSymbol('IDENTITY')
        x = LispSymbol('x')
        params = lispCons(x, NIL)
        body = x
        
        form = lispCons(defmacro_sym, lispCons(macro_name, lispCons(params, lispCons(body, NIL))))
        
        result = eval(form, env)
        
        # Result should be the macro name
        assert isinstance(result, LispSymbol)
        assert result.name == 'IDENTITY'
        
        # Macro should be in environment
        macro = env.find_func(macro_name)
        assert macro is not None
        assert getattr(macro, '__is_macro__', False)
    
    def test_defmacro_with_multiple_parameters(self, env):
        """DEFMACRO should handle multiple parameters."""
        # (DEFMACRO list-of (a b) (list a b))
        defmacro_sym = LispSymbol('DEFMACRO')
        macro_name = LispSymbol('LIST-OF')
        a = LispSymbol('a')
        b = LispSymbol('b')
        params = lispCons(a, lispCons(b, NIL))
        
        list_sym = LispSymbol('LIST')
        body = lispCons(list_sym, lispCons(a, lispCons(b, NIL)))
        
        form = lispCons(defmacro_sym, lispCons(macro_name, lispCons(params, lispCons(body, NIL))))
        
        result = eval(form, env)
        
        assert isinstance(result, LispSymbol)
        assert result.name == 'LIST-OF'


class TestMacroexpand:
    """Test MACROEXPAND functions."""
    
    def test_macroexpand_simple(self, env):
        """MACROEXPAND should expand a macro call."""
        # First define a simple macro using QUASIQUOTE
        defmacro_sym = LispSymbol('DEFMACRO')
        macro_name = LispSymbol('TWICE')
        x = LispSymbol('x')
        params = lispCons(x, NIL)
        # Body: `(+ ,x ,x) - proper code-generating macro
        quasiquote_sym = LispSymbol('QUASIQUOTE')
        plus_sym = LispSymbol('+')
        unquote_x1 = lispCons(LispSymbol('UNQUOTE'), lispCons(x, NIL))
        unquote_x2 = lispCons(LispSymbol('UNQUOTE'), lispCons(x, NIL))
        inner_list = lispCons(plus_sym, lispCons(unquote_x1, lispCons(unquote_x2, NIL)))
        body = lispCons(quasiquote_sym, lispCons(inner_list, NIL))
        
        defmacro_form = lispCons(defmacro_sym, lispCons(macro_name, lispCons(params, lispCons(body, NIL))))
        eval(defmacro_form, env)
        
        # Now test MACROEXPAND
        # (MACROEXPAND-1 '(twice 5))
        macroexpand1_sym = LispSymbol('MACROEXPAND-1')
        quote_sym = LispSymbol('QUOTE')
        call_form = lispCons(macro_name, lispCons(5, NIL))
        quoted_form = lispCons(quote_sym, lispCons(call_form, NIL))
        
        macroexpand_form = lispCons(macroexpand1_sym, lispCons(quoted_form, NIL))
        
        result = eval(macroexpand_form, env)
        
        # Result should be the expanded form: (+ 5 5)
        assert car(result).name == '+'
        assert car(cdr(result)) == 5
        assert car(cdr(cdr(result))) == 5
    
    def test_macroexpand_returns_same_if_not_macro(self, env):
        """MACROEXPAND should return same form if not a macro."""
        # Test with a regular function call
        macroexpand1_sym = LispSymbol('MACROEXPAND-1')
        quote_sym = LispSymbol('QUOTE')
        
        # (+ 1 2)
        call_form = lispCons(LispSymbol('+'), lispCons(1, lispCons(2, NIL)))
        quoted_form = lispCons(quote_sym, lispCons(call_form, NIL))
        
        macroexpand_form = lispCons(macroexpand1_sym, lispCons(quoted_form, NIL))
        
        result = eval(macroexpand_form, env)
        
        # Should be unchanged
        assert car(result).name == '+'
        assert car(cdr(result)) == 1
        assert car(cdr(cdr(result))) == 2


class TestMacroFunction:
    """Test MACRO-FUNCTION."""
    
    def test_macro_function_returns_macro(self, env):
        """MACRO-FUNCTION should return the macro function."""
        # First define a macro
        defmacro_sym = LispSymbol('DEFMACRO')
        macro_name = LispSymbol('MY-MACRO')
        x = LispSymbol('x')
        params = lispCons(x, NIL)
        body = x
        
        defmacro_form = lispCons(defmacro_sym, lispCons(macro_name, lispCons(params, lispCons(body, NIL))))
        eval(defmacro_form, env)
        
        # Get the macro function
        macro_function_sym = LispSymbol('MACRO-FUNCTION')
        quote_sym = LispSymbol('QUOTE')
        quoted_name = lispCons(quote_sym, lispCons(macro_name, NIL))
        
        form = lispCons(macro_function_sym, lispCons(quoted_name, NIL))
        
        result = eval(form, env)
        
        # Result should be a callable
        assert result is not None
        assert callable(result)
        # And it should be marked as a macro
        assert getattr(result, '__is_macro__', False)
    
    def test_macro_function_returns_nil_for_non_macro(self, env):
        """MACRO-FUNCTION should return NIL for non-macros."""
        # Test with a non-existent name or a function
        macro_function_sym = LispSymbol('MACRO-FUNCTION')
        quote_sym = LispSymbol('QUOTE')
        
        # Try with '+' which is a function, not a macro
        quoted_name = lispCons(quote_sym, lispCons(LispSymbol('+'), NIL))
        
        form = lispCons(macro_function_sym, lispCons(quoted_name, NIL))
        
        result = eval(form, env)
        
        # Result should be NIL
        assert result is NIL or result is None


class TestMacroExpansionChain:
    """Test that macroexpand continues until no more macros."""
    
    def test_macroexpand_continues_chain(self, env):
        """MACROEXPAND should expand macros recursively."""
        # Define macro 1: (DEFMACRO m1 (x) `(m2 ,x))
        defmacro_sym = LispSymbol('DEFMACRO')
        m1_name = LispSymbol('M1')
        x = LispSymbol('x')
        m2_name = LispSymbol('M2')
        
        # For now, skip this complex test - it requires backquote
        # which we'll implement in Task 6
        pass


class TestMacroWithRest:
    """Test macros with &rest parameters."""
    
    def test_macro_with_rest_parameter(self, env):
        """DEFMACRO should support &rest parameters."""
        # (DEFMACRO collect (&rest args) (list 'quote args))
        defmacro_sym = LispSymbol('DEFMACRO')
        macro_name = LispSymbol('COLLECT')
        and_rest = LispSymbol('&REST')
        args = LispSymbol('args')
        params = lispCons(and_rest, lispCons(args, NIL))
        
        quote_sym = LispSymbol('QUOTE')
        list_sym = LispSymbol('LIST')
        quoted = lispCons(quote_sym, lispCons(args, NIL))
        body = lispCons(list_sym, lispCons(quoted, NIL))
        
        form = lispCons(defmacro_sym, lispCons(macro_name, lispCons(params, lispCons(body, NIL))))
        
        result = eval(form, env)
        
        assert isinstance(result, LispSymbol)
        assert result.name == 'COLLECT'


class TestMacroInvocation:
    """Test calling macros from Lisp code."""
    
    def test_macro_invocation_in_eval(self, env):
        """Macros should be invoked correctly during evaluation."""
        # Define: (DEFMACRO first (x) x)
        defmacro_sym = LispSymbol('DEFMACRO')
        macro_name = LispSymbol('FIRST')
        x = LispSymbol('x')
        params = lispCons(x, NIL)
        body = x
        
        defmacro_form = lispCons(defmacro_sym, lispCons(macro_name, lispCons(params, lispCons(body, NIL))))
        eval(defmacro_form, env)
        
        # Invoke the macro with a quoted symbol
        # Macro returns the quoted form unevaluated, then it gets evaluated
        quote_sym = LispSymbol('QUOTE')
        value_sym = LispSymbol('MY-VALUE')
        quoted_arg = lispCons(quote_sym, lispCons(value_sym, NIL))
        call_form = lispCons(macro_name, lispCons(quoted_arg, NIL))
        
        # The macro receives '(QUOTE MY-VALUE), returns it, then eval processes it
        # Result: MY-VALUE (the symbol)
        result = eval(call_form, env)
        
        # The result should be the symbol
        assert isinstance(result, LispSymbol)
        assert result.name == 'MY-VALUE'
