"""Tests for Phase 4, Task 1: Support multiple return values."""

import pytest
import fclpy.state as state
from fclpy.lisptype import LispSymbol, lispCons as cons, NIL, T, MultipleValues
from fclpy.lispfunc.evaluation import eval, values, values_list, multiple_value_list, nth_value
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


class TestMultipleValuesClass:
    """Test the MultipleValues class itself."""
    
    def test_create_empty_multiple_values(self):
        """Create MultipleValues with no values."""
        mv = MultipleValues()
        assert len(mv) == 0
        assert mv.values == ()
        assert mv.get_primary() is NIL
        assert mv.to_list() is NIL
    
    def test_create_single_value(self):
        """Create MultipleValues with single value."""
        mv = MultipleValues(42)
        assert len(mv) == 1
        assert mv[0] == 42
        assert mv.get_primary() == 42
    
    def test_create_multiple_values(self):
        """Create MultipleValues with multiple values."""
        mv = MultipleValues(1, 2, 3)
        assert len(mv) == 3
        assert mv[0] == 1
        assert mv[1] == 2
        assert mv[2] == 3
        assert mv.get_primary() == 1
        assert mv.get_all() == (1, 2, 3)
    
    def test_multiple_values_from_list(self):
        """Create MultipleValues from list."""
        lst = cons(1, cons(2, cons(3, NIL)))
        mv = MultipleValues.from_list(lst)
        assert len(mv) == 3
        assert mv[0] == 1
        assert mv[1] == 2
        assert mv[2] == 3
    
    def test_multiple_values_to_list(self):
        """Convert MultipleValues to list."""
        mv = MultipleValues(10, 20, 30)
        lst = mv.to_list()
        # Check it's the Lisp cons type
        assert isinstance(lst, cons)
        assert car(lst) == 10
        assert car(cdr(lst)) == 20
        assert car(cdr(cdr(lst))) == 30
    
    def test_multiple_values_repr(self):
        """Test MultipleValues repr."""
        mv = MultipleValues(1, 2, 3)
        repr_str = repr(mv)
        assert "VALUES" in repr_str
        assert "1" in repr_str


class TestValuesFunction:
    """Test the VALUES function."""
    
    def test_values_no_args(self, env):
        """(VALUES) returns zero values (NIL as the primary value)."""
        form = cons(ls('VALUES'), NIL)
        result = eval(form, env)
        assert isinstance(result, MultipleValues)
        assert result.get_all() == ()
    
    def test_values_single_arg(self, env):
        """(VALUES x) returns x."""
        form = cons(ls('VALUES'), cons(42, NIL))
        result = eval(form, env)
        assert result == 42
    
    def test_values_multiple_args(self, env):
        """(VALUES a b c) returns MultipleValues(a, b, c)."""
        form = cons(ls('VALUES'), cons(1, cons(2, cons(3, NIL))))
        result = eval(form, env)
        assert isinstance(result, MultipleValues)
        assert result[0] == 1
        assert result[1] == 2
        assert result[2] == 3
    
    def test_values_with_vars(self, env):
        """(VALUES x y) where x and y are variables."""
        x_var = ls('X')
        y_var = ls('Y')
        env.set_variable(x_var, 100)
        env.set_variable(y_var, 200)
        
        form = cons(ls('VALUES'), cons(x_var, cons(y_var, NIL)))
        result = eval(form, env)
        assert isinstance(result, MultipleValues)
        assert result[0] == 100
        assert result[1] == 200
    
    def test_values_with_nil(self, env):
        """(VALUES NIL) returns NIL."""
        form = cons(ls('VALUES'), cons(NIL, NIL))
        result = eval(form, env)
        assert result is NIL


class TestValuesListFunction:
    """Test the VALUES-LIST function."""
    
    def test_values_list_empty(self, env):
        """(VALUES-LIST NIL) returns ZERO values, exactly like (VALUES).

        CLHS: (values-list list) is equivalent to (apply #'values list), so an
        empty list yields no values at all. This used to assert `result is NIL`
        -- one value -- which contradicted this file's own
        `test_values_no_args`, where zero values are represented as an empty
        MultipleValues.
        """
        form = cons(ls('VALUES-LIST'), cons(NIL, NIL))
        result = eval(form, env)
        # Same representation test_values_no_args uses for (VALUES).
        assert isinstance(result, MultipleValues)
        assert result.get_all() == ()

    def test_values_list_single(self, env):
        """(VALUES-LIST '(42)) returns single value or MultipleValues."""
        # Use QUOTE to construct the list
        lst_form = cons(ls('QUOTE'), cons(cons(42, NIL), NIL))
        form = cons(ls('VALUES-LIST'), cons(lst_form, NIL))
        result = eval(form, env)
        # Should be MultipleValues with one value or just 42
        if isinstance(result, MultipleValues):
            assert result[0] == 42
            assert len(result) == 1
        else:
            assert result == 42
    
    def test_values_list_multiple(self, env):
        """(VALUES-LIST '(1 2 3)) returns multiple values."""
        # Use QUOTE to construct the list
        lst_form = cons(ls('QUOTE'), cons(cons(1, cons(2, cons(3, NIL))), NIL))
        form = cons(ls('VALUES-LIST'), cons(lst_form, NIL))
        result = eval(form, env)
        assert isinstance(result, MultipleValues)
        assert result[0] == 1
        assert result[1] == 2
        assert result[2] == 3


class TestMultipleValueListFunction:
    """Test the MULTIPLE-VALUE-LIST function."""
    
    def test_multiple_value_list_single(self, env):
        """(MULTIPLE-VALUE-LIST 42) returns (42)."""
        form = cons(ls('MULTIPLE-VALUE-LIST'), cons(42, NIL))
        result = eval(form, env)
        assert isinstance(result, cons)
        assert car(result) == 42
        assert cdr(result) is NIL
    
    def test_multiple_value_list_from_values(self, env):
        """(MULTIPLE-VALUE-LIST (VALUES 1 2 3)) returns (1 2 3)."""
        values_form = cons(ls('VALUES'), cons(1, cons(2, cons(3, NIL))))
        form = cons(ls('MULTIPLE-VALUE-LIST'), cons(values_form, NIL))
        result = eval(form, env)
        
        # Result should be a proper list
        assert isinstance(result, cons)
        assert car(result) == 1
        assert car(cdr(result)) == 2
        assert car(cdr(cdr(result))) == 3
        assert cdr(cdr(cdr(result))) is NIL
    
    def test_multiple_value_list_nil(self, env):
        """(MULTIPLE-VALUE-LIST NIL) returns (NIL)."""
        form = cons(ls('MULTIPLE-VALUE-LIST'), cons(NIL, NIL))
        result = eval(form, env)
        assert isinstance(result, cons)
        assert car(result) is NIL
        assert cdr(result) is NIL


class TestNthValueFunction:
    """Test the NTH-VALUE function."""
    
    def test_nth_value_zero(self, env):
        """(NTH-VALUE 0 (VALUES a b c)) returns a."""
        values_form = cons(ls('VALUES'), cons(10, cons(20, cons(30, NIL))))
        form = cons(ls('NTH-VALUE'), cons(0, cons(values_form, NIL)))
        result = eval(form, env)
        assert result == 10
    
    def test_nth_value_one(self, env):
        """(NTH-VALUE 1 (VALUES a b c)) returns b."""
        values_form = cons(ls('VALUES'), cons(10, cons(20, cons(30, NIL))))
        form = cons(ls('NTH-VALUE'), cons(1, cons(values_form, NIL)))
        result = eval(form, env)
        assert result == 20
    
    def test_nth_value_two(self, env):
        """(NTH-VALUE 2 (VALUES a b c)) returns c."""
        values_form = cons(ls('VALUES'), cons(10, cons(20, cons(30, NIL))))
        form = cons(ls('NTH-VALUE'), cons(2, cons(values_form, NIL)))
        result = eval(form, env)
        assert result == 30
    
    def test_nth_value_out_of_range(self, env):
        """(NTH-VALUE 5 (VALUES a b c)) returns NIL."""
        values_form = cons(ls('VALUES'), cons(10, cons(20, cons(30, NIL))))
        form = cons(ls('NTH-VALUE'), cons(5, cons(values_form, NIL)))
        result = eval(form, env)
        assert result is NIL
    
    def test_nth_value_negative(self, env):
        """(NTH-VALUE -1 (VALUES a b c)) returns NIL."""
        values_form = cons(ls('VALUES'), cons(10, cons(20, cons(30, NIL))))
        form = cons(ls('NTH-VALUE'), cons(-1, cons(values_form, NIL)))
        result = eval(form, env)
        assert result is NIL
    
    def test_nth_value_single(self, env):
        """(NTH-VALUE 0 42) returns 42."""
        form = cons(ls('NTH-VALUE'), cons(0, cons(42, NIL)))
        result = eval(form, env)
        assert result == 42
    
    def test_nth_value_single_out_of_range(self, env):
        """(NTH-VALUE 1 42) returns NIL."""
        form = cons(ls('NTH-VALUE'), cons(1, cons(42, NIL)))
        result = eval(form, env)
        assert result is NIL


class TestReturnFromMultipleValues:
    """Test that RETURN-FROM works with multiple values."""
    
    def test_return_from_multiple_values(self, env):
        """(BLOCK x (RETURN-FROM x (VALUES 1 2 3))) returns multiple values."""
        block_name = ls('X')
        return_form = cons(
            ls('RETURN-FROM'),
            cons(block_name, cons(
                cons(ls('VALUES'), cons(1, cons(2, cons(3, NIL)))),
                NIL
            ))
        )
        block_form = cons(ls('BLOCK'), cons(block_name, cons(return_form, NIL)))
        result = eval(block_form, env)
        
        # Should get back the multiple values
        assert isinstance(result, MultipleValues)
        assert result[0] == 1
        assert result[1] == 2
        assert result[2] == 3


class TestFunctionReturnsMultipleValues:
    """Test user-defined functions returning multiple values."""
    
    def test_function_returns_multiple_values(self, env):
        """User function can return multiple values via VALUES."""
        # Define: (DEFUN SPLIT (X) (VALUES X (+ X 1)))
        defun_form = cons(
            ls('DEFUN'),
            cons(
                ls('SPLIT'),
                cons(
                    cons(ls('X'), NIL),  # parameter list
                    cons(
                        cons(
                            ls('VALUES'),
                            cons(ls('X'), cons(
                                cons(ls('+'), cons(ls('X'), cons(1, NIL))),
                                NIL
                            ))
                        ),
                        NIL
                    )
                )
            )
        )
        
        eval(defun_form, env)
        
        # Call: (SPLIT 5)
        call_form = cons(ls('SPLIT'), cons(5, NIL))
        result = eval(call_form, env)
        
        # Should return MultipleValues
        assert isinstance(result, MultipleValues)
        assert result[0] == 5
        assert result[1] == 6


class TestMultipleValuesInProgn:
    """Test multiple values propagate through PROGN."""
    
    def test_progn_returns_last_value(self, env):
        """(PROGN ... (VALUES 1 2 3)) returns the multiple values."""
        progn_form = cons(
            ls('PROGN'),
            cons(
                1,  # First form
                cons(
                    cons(ls('VALUES'), cons(10, cons(20, cons(30, NIL)))),
                    NIL
                )
            )
        )
        result = eval(progn_form, env)
        
        # PROGN returns last form's result
        assert isinstance(result, MultipleValues)
        assert result[0] == 10
        assert result[1] == 20
        assert result[2] == 30


class TestMultipleValuesInLet:
    """Test multiple values in LET bindings."""
    
    def test_let_with_multiple_values(self, env):
        """(LET ((X (VALUES 1 2))) X) - first value used for binding."""
        let_form = cons(
            ls('LET'),
            cons(
                cons(
                    cons(ls('X'), cons(
                        cons(ls('VALUES'), cons(1, cons(2, NIL))),
                        NIL
                    )),
                    NIL
                ),
                cons(ls('X'), NIL)
            )
        )
        result = eval(let_form, env)

        # CLHS 3.1.2.1 / 5.1.1: a LET init form is a single-value context, so X
        # is bound to the primary value and nothing else survives -- the form
        # answers exactly 1.
        #
        # This asserted `result == 1 or isinstance(result, MultipleValues)`,
        # i.e. it accepted both of two mutually exclusive outcomes (a collapsed
        # value *or* an un-collapsed MultipleValues) and so could not fail if
        # the collapse regressed. Only one of them is the ANSI answer, and the
        # test has to say which.
        assert not isinstance(result, MultipleValues), (
            "a LET init form is a single-value context: X must be bound to the "
            "primary value, not to a MultipleValues object")
        assert result == 1


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
