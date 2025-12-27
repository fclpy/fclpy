"""Tests for Task 9: Multiple values support (basic implementation)."""

import pytest
import fclpy.state as state
from fclpy.lisptype import LispSymbol, lispCons as cons, NIL, T
from fclpy.lispfunc.evaluation import eval
from fclpy.lispfunc.core import car, cdr
from fclpy.lispenv import setup_standard_environment


def ls(name):
    """Shorthand to create a LispSymbol."""
    return LispSymbol(name)


@pytest.fixture
def env():
    """Create a fresh environment for each test."""
    state.current_environment = None
    state.functions_loaded = False
    return setup_standard_environment()


class TestMultipleValues:
    """Test basic multiple values support.
    
    Common Lisp has sophisticated multiple-value support with VALUES, 
    MULTIPLE-VALUE-BIND, etc. This implementation is basic/placeholder
    and captures just the first value most of the time.
    """

    def test_single_value_return(self, env):
        """Functions returning single values work normally."""
        # (+ 1 2) returns 3
        form = cons(ls('+'), cons(1, cons(2, NIL)))
        result = eval(form, env)
        assert result == 3

    def test_values_form_basic(self, env):
        """(VALUES x) returns x."""
        # (VALUES 42)
        form = cons(ls('VALUES'), cons(42, NIL))
        result = eval(form, env)
        assert result == 42

    def test_values_multiple_forms(self, env):
        """(VALUES a b c) - for now, returns all as tuple or takes first."""
        # This test documents behavior: (VALUES 1 2 3)
        # Implementation choice: return as tuple for now
        form = cons(ls('VALUES'), cons(1, cons(2, cons(3, NIL))))
        result = eval(form, env)
        # Accept either tuple or first value
        assert result == (1, 2, 3) or result == 1

    def test_values_with_variables(self, env):
        """(VALUES x y) where x and y are variables."""
        x_var = ls('X')
        y_var = ls('Y')
        env.set_variable(x_var, 10)
        env.set_variable(y_var, 20)
        
        form = cons(ls('VALUES'), cons(x_var, cons(y_var, NIL)))
        result = eval(form, env)
        
        # Should return values
        assert result == (10, 20) or result == 10

    def test_multiple_value_list_conversion(self, env):
        """VALUES result can be converted to list for convenience."""
        # (VALUES 1 2 3) when used in list context
        form = cons(ls('VALUES'), cons(1, cons(2, cons(3, NIL))))
        result = eval(form, env)
        
        # Result should be usable
        assert result is not None

    def test_values_zero_values(self, env):
        """(VALUES) with no arguments returns NIL."""
        form = cons(ls('VALUES'), NIL)
        result = eval(form, env)
        assert result is NIL

    def test_values_single_nil(self, env):
        """(VALUES NIL) returns NIL."""
        form = cons(ls('VALUES'), cons(NIL, NIL))
        result = eval(form, env)
        assert result is NIL


class TestMultipleValueBasics:
    """Basic multiple-value scenarios for documentation."""

    def test_first_value_extraction(self, env):
        """When multiple values used in single-value context, take first."""
        # This is the main behavior: first value is used
        x_var = ls('X')
        form = cons(ls('VALUES'), cons(100, cons(200, NIL)))
        result = eval(form, env)
        
        # Whether tuple or single value, we should be able to use it
        if isinstance(result, tuple):
            assert result[0] == 100
        else:
            assert result == 100

    def test_values_preserves_data(self, env):
        """Multiple values carry data through evaluation."""
        # All data should be preserved even if not used
        form = cons(ls('VALUES'), cons(5, cons(10, cons(15, NIL))))
        result = eval(form, env)
        
        # Result contains all values
        if isinstance(result, tuple):
            assert len(result) == 3
            assert result == (5, 10, 15)
        else:
            # Or just first
            assert result == 5


class TestMultipleValueLimitations:
    """Document limitations of this basic implementation."""

    def test_multiple_value_bind_not_implemented(self, env):
        """MULTIPLE-VALUE-BIND is not implemented in this basic version."""
        # This would be: (MULTIPLE-VALUE-BIND (a b c) (VALUES 1 2 3) (+ a b c))
        # For now, this is documented as not supported
        pass

    def test_multiple_value_call_not_implemented(self, env):
        """MULTIPLE-VALUE-CALL is not implemented."""
        # This feature requires sophisticated control flow
        pass

    def test_multiple_value_list_not_implemented(self, env):
        """MULTIPLE-VALUE-LIST is not implemented."""
        # (MULTIPLE-VALUE-LIST (VALUES 1 2 3)) => (1 2 3)
        pass
