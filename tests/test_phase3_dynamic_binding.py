"""
Tests for Phase 3: Basic Evaluation - Dynamic Binding.

This test module verifies that dynamic binding (LET, LET*, special variables)
works correctly in the evaluator.
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
    env_obj = setup_standard_environment()
    yield env_obj
    # Cleanup after test to avoid leaking state to other tests
    state.current_environment = None
    state.functions_loaded = False


class TestLetSpecialForm:
    """Test LET special form for local variable binding."""
    
    def test_let_binds_single_variable(self, env):
        """LET should bind a single variable in its body."""
        # (LET ((x 10)) x) should return 10
        let_sym = LispSymbol('LET')
        x_sym = LispSymbol('x')
        binding = lispCons(x_sym, lispCons(10, NIL))
        bindings = lispCons(binding, NIL)
        body = x_sym
        
        form = lispCons(let_sym, lispCons(bindings, lispCons(body, NIL)))
        result = eval(form, env)
        
        assert result == 10
    
    def test_let_binds_multiple_variables(self, env):
        """LET should bind multiple variables."""
        # (LET ((x 1) (y 2)) (+ x y)) should return 3
        let_sym = LispSymbol('LET')
        x_sym = LispSymbol('x')
        y_sym = LispSymbol('y')
        
        binding1 = lispCons(x_sym, lispCons(1, NIL))
        binding2 = lispCons(y_sym, lispCons(2, NIL))
        bindings = lispCons(binding1, lispCons(binding2, NIL))
        
        plus_sym = LispSymbol('+')
        body = lispCons(plus_sym, lispCons(x_sym, lispCons(y_sym, NIL)))
        
        form = lispCons(let_sym, lispCons(bindings, lispCons(body, NIL)))
        result = eval(form, env)
        
        assert result == 3
    
    def test_let_isolates_variables(self, env):
        """LET bindings should not affect outer scope."""
        # (LET ((x 99)) x) followed by checking x is unbound
        let_sym = LispSymbol('LET')
        x_sym = LispSymbol('x')
        binding = lispCons(x_sym, lispCons(99, NIL))
        bindings = lispCons(binding, NIL)
        body = x_sym
        
        form = lispCons(let_sym, lispCons(bindings, lispCons(body, NIL)))
        result = eval(form, env)
        
        assert result == 99
        # x should not be bound in outer scope
        with pytest.raises(Exception):  # Should raise unbound variable error
            eval(x_sym, env)
    
    def test_let_with_no_bindings(self, env):
        """LET with no bindings should evaluate body normally."""
        # (LET () 42) should return 42
        let_sym = LispSymbol('LET')
        bindings = NIL
        body = 42
        
        form = lispCons(let_sym, lispCons(bindings, lispCons(body, NIL)))
        result = eval(form, env)
        
        assert result == 42
    
    def test_let_body_with_multiple_forms(self, env):
        """LET body can have multiple forms, last is returned."""
        # (LET ((x 1)) (+ x 1) (+ x 10)) should return 11
        let_sym = LispSymbol('LET')
        x_sym = LispSymbol('x')
        binding = lispCons(x_sym, lispCons(1, NIL))
        bindings = lispCons(binding, NIL)
        
        plus_sym = LispSymbol('+')
        form1 = lispCons(plus_sym, lispCons(x_sym, lispCons(1, NIL)))
        form2 = lispCons(plus_sym, lispCons(x_sym, lispCons(10, NIL)))
        
        form = lispCons(let_sym, lispCons(bindings, lispCons(form1, lispCons(form2, NIL))))
        result = eval(form, env)
        
        assert result == 11
    
    def test_let_bindings_evaluated_in_outer_scope(self, env):
        """LET binding values should be evaluated in outer scope, not new scope."""
        # (LET ((x 1) (y x)) y) - y should evaluate x in outer scope
        let_sym = LispSymbol('LET')
        x_sym = LispSymbol('x')
        y_sym = LispSymbol('y')
        
        # Bind x to 5 in outer scope
        env.add_variable(x_sym, 5)
        
        binding1 = lispCons(x_sym, lispCons(1, NIL))
        binding2 = lispCons(y_sym, lispCons(x_sym, NIL))
        bindings = lispCons(binding1, lispCons(binding2, NIL))
        body = y_sym
        
        form = lispCons(let_sym, lispCons(bindings, lispCons(body, NIL)))
        result = eval(form, env)
        
        # y should be bound to 5 (outer x), not 1 (inner x)
        assert result == 5


class TestLetStarSpecialForm:
    """Test LET* special form for sequential binding."""
    
    def test_letstar_bindings_sequential(self, env):
        """LET* bindings should be evaluated sequentially, each seeing previous ones."""
        # (LET* ((x 1) (y (+ x 1))) y) should return 2
        letstar_sym = LispSymbol('LET*')
        x_sym = LispSymbol('x')
        y_sym = LispSymbol('y')
        
        binding1 = lispCons(x_sym, lispCons(1, NIL))
        plus_sym = LispSymbol('+')
        y_value = lispCons(plus_sym, lispCons(x_sym, lispCons(1, NIL)))
        binding2 = lispCons(y_sym, lispCons(y_value, NIL))
        
        bindings = lispCons(binding1, lispCons(binding2, NIL))
        body = y_sym
        
        form = lispCons(letstar_sym, lispCons(bindings, lispCons(body, NIL)))
        result = eval(form, env)
        
        assert result == 2
    
    def test_letstar_vs_let_difference(self, env):
        """LET* differs from LET in binding order."""
        # With LET: (LET ((x 1) (y x)) y) - y sees outer x, not 1
        # With LET*: (LET* ((x 1) (y x)) y) - y sees inner x, which is 1
        letstar_sym = LispSymbol('LET*')
        x_sym = LispSymbol('x')
        y_sym = LispSymbol('y')
        
        binding1 = lispCons(x_sym, lispCons(1, NIL))
        binding2 = lispCons(y_sym, lispCons(x_sym, NIL))
        bindings = lispCons(binding1, lispCons(binding2, NIL))
        body = y_sym
        
        form = lispCons(letstar_sym, lispCons(bindings, lispCons(body, NIL)))
        result = eval(form, env)
        
        # In LET*, y should see x=1 (the binding just made)
        assert result == 1


class TestSpecialVariables:
    """Test dynamic binding for special variables (with asterisks)."""
    
    def test_special_variable_dynamic_binding(self, env):
        """Variables with asterisks should support dynamic binding."""
        # *x* = 10 globally
        # In LET: (*x* 20) should dynamically bind *x* to 20 inside
        special_x = LispSymbol('*x*')
        let_sym = LispSymbol('LET')
        
        # Set outer value
        env.add_variable(special_x, 10)
        
        binding = lispCons(special_x, lispCons(20, NIL))
        bindings = lispCons(binding, NIL)
        body = special_x
        
        form = lispCons(let_sym, lispCons(bindings, lispCons(body, NIL)))
        result = eval(form, env)
        
        assert result == 20
        # After LET, should restore to 10
        assert env.find_variable(special_x) == 10
    
    def test_special_variable_restoration(self, env):
        """Dynamic binding should restore special variable after scope exits."""
        special_x = LispSymbol('*x*')
        let_sym = LispSymbol('LET')
        
        # Set outer value
        env.add_variable(special_x, 10)
        
        binding = lispCons(special_x, lispCons(20, NIL))
        bindings = lispCons(binding, NIL)
        body = special_x
        
        form = lispCons(let_sym, lispCons(bindings, lispCons(body, NIL)))
        result = eval(form, env)
        
        assert result == 20
        # Outer scope should still have original value
        assert env.find_variable(special_x) == 10


class TestNestedLet:
    """Test nested LET forms."""
    
    def test_nested_let_variable_shadowing(self, env):
        """Inner LET should shadow outer LET bindings."""
        # (LET ((x 1)) (LET ((x 2)) x)) should return 2
        let_sym = LispSymbol('LET')
        x_sym = LispSymbol('x')
        
        inner_binding = lispCons(x_sym, lispCons(2, NIL))
        inner_bindings = lispCons(inner_binding, NIL)
        inner_body = x_sym
        inner_let = lispCons(let_sym, lispCons(inner_bindings, lispCons(inner_body, NIL)))
        
        outer_binding = lispCons(x_sym, lispCons(1, NIL))
        outer_bindings = lispCons(outer_binding, NIL)
        
        form = lispCons(let_sym, lispCons(outer_bindings, lispCons(inner_let, NIL)))
        result = eval(form, env)
        
        assert result == 2
    
    def test_nested_let_access_outer(self, env):
        """Inner LET can access outer variables if not shadowed."""
        # (LET ((x 1) (y 2)) (LET ((z (+ x y))) z)) should return 3
        let_sym = LispSymbol('LET')
        x_sym = LispSymbol('x')
        y_sym = LispSymbol('y')
        z_sym = LispSymbol('z')
        plus_sym = LispSymbol('+')
        
        # Inner LET
        z_value = lispCons(plus_sym, lispCons(x_sym, lispCons(y_sym, NIL)))
        z_binding = lispCons(z_sym, lispCons(z_value, NIL))
        inner_bindings = lispCons(z_binding, NIL)
        inner_body = z_sym
        inner_let = lispCons(let_sym, lispCons(inner_bindings, lispCons(inner_body, NIL)))
        
        # Outer LET
        x_binding = lispCons(x_sym, lispCons(1, NIL))
        y_binding = lispCons(y_sym, lispCons(2, NIL))
        outer_bindings = lispCons(x_binding, lispCons(y_binding, NIL))
        
        form = lispCons(let_sym, lispCons(outer_bindings, lispCons(inner_let, NIL)))
        result = eval(form, env)
        
        assert result == 3
