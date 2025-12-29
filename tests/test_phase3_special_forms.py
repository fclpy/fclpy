"""
Tests for Phase 3: Basic Evaluation - Special Forms.

This test module verifies that special forms (QUOTE, IF, PROGN, etc.)
work correctly in the evaluator.
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


class TestQuoteSpecialForm:
    """Test QUOTE special form."""
    
    def test_quote_returns_argument_unchanged(self, env):
        """QUOTE should return its argument unevaluated."""
        # (QUOTE x) should return x unchanged
        quote_sym = LispSymbol('QUOTE')
        x_sym = LispSymbol('x')
        form = lispCons(quote_sym, lispCons(x_sym, NIL))
        
        result = eval(form, env)
        
        # Result should be the symbol x, not its value
        assert isinstance(result, LispSymbol)
        assert result.name == 'x'
    
    def test_quote_list_returns_list_unchanged(self, env):
        """QUOTE should return a list unchanged."""
        # (QUOTE (a b c)) should return (a b c)
        quote_sym = LispSymbol('QUOTE')
        a = LispSymbol('a')
        b = LispSymbol('b')
        c = LispSymbol('c')
        inner_list = lispCons(a, lispCons(b, lispCons(c, NIL)))
        form = lispCons(quote_sym, lispCons(inner_list, NIL))
        
        result = eval(form, env)
        
        # Result should be the list (a b c)
        assert result == inner_list
        assert car(result).name == 'a'
        assert car(cdr(result)).name == 'b'
        assert car(cdr(cdr(result))).name == 'c'


class TestIfSpecialForm:
    """Test IF special form."""
    
    def test_if_with_true_test(self, env):
        """IF with true test should evaluate then-form."""
        # (IF T 5 10) should return 5
        if_sym = LispSymbol('IF')
        then_form = 5
        else_form = 10
        form = lispCons(if_sym, lispCons(T, lispCons(then_form, lispCons(else_form, NIL))))
        
        result = eval(form, env)
        
        assert result == 5
    
    def test_if_with_false_test(self, env):
        """IF with false test should evaluate else-form."""
        # (IF NIL 5 10) should return 10
        if_sym = LispSymbol('IF')
        then_form = 5
        else_form = 10
        form = lispCons(if_sym, lispCons(NIL, lispCons(then_form, lispCons(else_form, NIL))))
        
        result = eval(form, env)
        
        assert result == 10
    
    def test_if_without_else_form(self, env):
        """IF without else-form should return NIL when test is false."""
        # (IF NIL 5) should return NIL
        if_sym = LispSymbol('IF')
        then_form = 5
        form = lispCons(if_sym, lispCons(NIL, lispCons(then_form, NIL)))
        
        result = eval(form, env)
        
        assert result is NIL or result is None
    
    def test_if_evaluates_test_expression(self, env):
        """IF should evaluate the test expression."""
        # Set up: x = T, (IF x 5 10) should return 5
        x_sym = LispSymbol('x')
        if_sym = LispSymbol('IF')
        then_form = 5
        else_form = 10
        form = lispCons(if_sym, lispCons(x_sym, lispCons(then_form, lispCons(else_form, NIL))))
        
        env.add_variable(x_sym, T)
        result = eval(form, env)
        
        assert result == 5


class TestPrognSpecialForm:
    """Test PROGN special form."""
    
    def test_progn_returns_last_value(self, env):
        """PROGN should return the value of the last form."""
        # (PROGN 1 2 3) should return 3
        progn_sym = LispSymbol('PROGN')
        form = lispCons(progn_sym, lispCons(1, lispCons(2, lispCons(3, NIL))))
        
        result = eval(form, env)
        
        assert result == 3
    
    def test_progn_evaluates_all_forms(self, env):
        """PROGN should evaluate all forms for side effects."""
        # Create a simple test to verify all forms are evaluated
        progn_sym = LispSymbol('PROGN')
        
        # We'll use assignment for side effects
        x_sym = LispSymbol('x')
        setq_sym = LispSymbol('SETQ')
        
        # (PROGN (SETQ x 1) (SETQ x 2) x)
        form1 = lispCons(setq_sym, lispCons(x_sym, lispCons(1, NIL)))
        form2 = lispCons(setq_sym, lispCons(x_sym, lispCons(2, NIL)))
        form3 = x_sym
        
        form = lispCons(progn_sym, lispCons(form1, lispCons(form2, lispCons(form3, NIL))))
        
        result = eval(form, env)
        
        # Should evaluate to 2 (last SETQ returns its value)
        assert result == 2
        # x should be bound to 2
        assert env.find_variable(x_sym) == 2


class TestProgn2SpecialForm:
    """Test PROGN with multiple forms."""
    
    def test_progn_with_single_form(self, env):
        """PROGN with single form should return its value."""
        progn_sym = LispSymbol('PROGN')
        form = lispCons(progn_sym, lispCons(42, NIL))
        
        result = eval(form, env)
        
        assert result == 42
    
    def test_progn_empty(self, env):
        """PROGN with no forms should return NIL."""
        progn_sym = LispSymbol('PROGN')
        form = lispCons(progn_sym, NIL)
        
        result = eval(form, env)
        
        assert result is None


class TestSpecialFormDispatcher:
    """Test that the special form dispatcher correctly routes to handlers."""
    
    def test_quote_via_dispatcher(self, env):
        """Quote should be recognized as a special form."""
        quote_sym = LispSymbol('QUOTE')
        x_sym = LispSymbol('x')
        form = lispCons(quote_sym, lispCons(x_sym, NIL))
        
        result = eval(form, env)
        
        assert result.name == 'x'
    
    def test_if_via_dispatcher(self, env):
        """IF should be recognized as a special form."""
        if_sym = LispSymbol('IF')
        form = lispCons(if_sym, lispCons(T, lispCons(100, lispCons(200, NIL))))
        
        result = eval(form, env)
        
        assert result == 100
    
    def test_progn_via_dispatcher(self, env):
        """PROGN should be recognized as a special form."""
        progn_sym = LispSymbol('PROGN')
        form = lispCons(progn_sym, lispCons(1, lispCons(2, lispCons(3, NIL))))
        
        result = eval(form, env)
        
        assert result == 3


class TestSpecialFormsConsistency:
    """Test that special forms behave consistently."""
    
    def test_nested_special_forms(self, env):
        """Nested special forms should evaluate correctly."""
        # (IF T (PROGN 1 2 3) 999) should return 3
        if_sym = LispSymbol('IF')
        progn_sym = LispSymbol('PROGN')
        
        progn_body = lispCons(progn_sym, lispCons(1, lispCons(2, lispCons(3, NIL))))
        form = lispCons(if_sym, lispCons(T, lispCons(progn_body, lispCons(999, NIL))))
        
        result = eval(form, env)
        
        assert result == 3
    
    def test_quote_inside_progn(self, env):
        """QUOTE inside PROGN should work correctly."""
        # (PROGN (QUOTE x) (QUOTE y) (QUOTE z)) should return z
        progn_sym = LispSymbol('PROGN')
        quote_sym = LispSymbol('QUOTE')
        
        x_sym = LispSymbol('x')
        y_sym = LispSymbol('y')
        z_sym = LispSymbol('z')
        
        form1 = lispCons(quote_sym, lispCons(x_sym, NIL))
        form2 = lispCons(quote_sym, lispCons(y_sym, NIL))
        form3 = lispCons(quote_sym, lispCons(z_sym, NIL))
        
        form = lispCons(progn_sym, lispCons(form1, lispCons(form2, lispCons(form3, NIL))))
        
        result = eval(form, env)
        
        assert result.name == 'z'
