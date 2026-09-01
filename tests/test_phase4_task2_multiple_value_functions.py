"""Tests for Phase 4, Task 2: Multiple value functions (MULTIPLE-VALUE-CALL, MULTIPLE-VALUE-BIND)."""

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


class TestMultipleValueCall:
    """Test the MULTIPLE-VALUE-CALL special form."""
    
    def test_multiple_value_call_single_value(self, env):
        """(MULTIPLE-VALUE-CALL #'+ 10) should call (+ 10)."""
        # Create a form: (MULTIPLE-VALUE-CALL (QUOTE +) 10)
        form = cons(
            ls('MULTIPLE-VALUE-CALL'),
            cons(
                cons(ls('QUOTE'), cons(ls('+'), NIL)),
                cons(10, NIL)
            )
        )
        result = eval(form, env)
        assert result == 10
    
    def test_multiple_value_call_multiple_single_values(self, env):
        """(MULTIPLE-VALUE-CALL #'+ 10 20 30) should call (+ 10 20 30)."""
        form = cons(
            ls('MULTIPLE-VALUE-CALL'),
            cons(
                cons(ls('QUOTE'), cons(ls('+'), NIL)),
                cons(10, cons(20, cons(30, NIL)))
            )
        )
        result = eval(form, env)
        assert result == 60
    
    def test_multiple_value_call_with_multiple_values(self, env):
        """(MULTIPLE-VALUE-CALL #'+ (VALUES 1 2) (VALUES 3 4)) 
        should call (+ 1 2 3 4)."""
        # Create form: (MULTIPLE-VALUE-CALL (QUOTE +) (VALUES 1 2) (VALUES 3 4))
        form = cons(
            ls('MULTIPLE-VALUE-CALL'),
            cons(
                cons(ls('QUOTE'), cons(ls('+'), NIL)),
                cons(
                    cons(ls('VALUES'), cons(1, cons(2, NIL))),
                    cons(
                        cons(ls('VALUES'), cons(3, cons(4, NIL))),
                        NIL
                    )
                )
            )
        )
        result = eval(form, env)
        # Should be 1 + 2 + 3 + 4 = 10
        assert result == 10
    
    def test_multiple_value_call_mixed_values(self, env):
        """(MULTIPLE-VALUE-CALL #'+ (VALUES 1 2) 3) should call (+ 1 2 3)."""
        form = cons(
            ls('MULTIPLE-VALUE-CALL'),
            cons(
                cons(ls('QUOTE'), cons(ls('+'), NIL)),
                cons(
                    cons(ls('VALUES'), cons(1, cons(2, NIL))),
                    cons(3, NIL)
                )
            )
        )
        result = eval(form, env)
        assert result == 6
    
    def test_multiple_value_call_list(self, env):
        """(MULTIPLE-VALUE-CALL #'LIST (VALUES 1 2 3)) should call (LIST 1 2 3)."""
        form = cons(
            ls('MULTIPLE-VALUE-CALL'),
            cons(
                cons(ls('QUOTE'), cons(ls('LIST'), NIL)),
                cons(
                    cons(ls('VALUES'), cons(1, cons(2, cons(3, NIL)))),
                    NIL
                )
            )
        )
        result = eval(form, env)
        # Should return a list (1 2 3)
        assert isinstance(result, cons)
        assert car(result) == 1
        assert car(cdr(result)) == 2
        assert car(cdr(cdr(result))) == 3
    
    def test_multiple_value_call_user_function(self, env):
        """Test MULTIPLE-VALUE-CALL with user-defined function."""
        # Define a simple function that adds all its arguments
        defun_form = cons(
            ls('DEFUN'),
            cons(
                ls('SUM-THREE'),
                cons(
                    cons(ls('A'), cons(ls('B'), cons(ls('C'), NIL))),  # three parameters
                    cons(
                        cons(ls('+'), cons(ls('A'), cons(ls('B'), cons(ls('C'), NIL)))),
                        NIL
                    )
                )
            )
        )
        eval(defun_form, env)
        
        # Now call: (MULTIPLE-VALUE-CALL #'SUM-THREE (VALUES 10 20 30))
        form = cons(
            ls('MULTIPLE-VALUE-CALL'),
            cons(
                cons(ls('QUOTE'), cons(ls('SUM-THREE'), NIL)),
                cons(
                    cons(ls('VALUES'), cons(10, cons(20, cons(30, NIL)))),
                    NIL
                )
            )
        )
        result = eval(form, env)
        # Should get back 10 + 20 + 30 = 60
        assert result == 60
        """(MULTIPLE-VALUE-CALL #'LIST NIL) should call (LIST NIL)."""
        form = cons(
            ls('MULTIPLE-VALUE-CALL'),
            cons(
                cons(ls('QUOTE'), cons(ls('LIST'), NIL)),
                cons(NIL, NIL)
            )
        )
        result = eval(form, env)
        # Should return a list (NIL)
        assert isinstance(result, cons)
        assert car(result) is NIL


class TestMultipleValueBind:
    """Test the MULTIPLE-VALUE-BIND special form."""
    
    def test_multiple_value_bind_single_var(self, env):
        """(MULTIPLE-VALUE-BIND (X) (VALUES 42) X) should return 42."""
        form = cons(
            ls('MULTIPLE-VALUE-BIND'),
            cons(
                cons(ls('X'), NIL),  # variable list: (X)
                cons(
                    cons(ls('VALUES'), cons(42, NIL)),  # value form: (VALUES 42)
                    cons(ls('X'), NIL)  # body: X
                )
            )
        )
        result = eval(form, env)
        assert result == 42
    
    def test_multiple_value_bind_multiple_vars(self, env):
        """(MULTIPLE-VALUE-BIND (X Y Z) (VALUES 1 2 3) (LIST X Y Z)) 
        should return (1 2 3)."""
        form = cons(
            ls('MULTIPLE-VALUE-BIND'),
            cons(
                cons(ls('X'), cons(ls('Y'), cons(ls('Z'), NIL))),  # (X Y Z)
                cons(
                    cons(ls('VALUES'), cons(1, cons(2, cons(3, NIL)))),  # (VALUES 1 2 3)
                    cons(
                        cons(ls('LIST'), cons(ls('X'), cons(ls('Y'), cons(ls('Z'), NIL)))),
                        NIL
                    )
                )
            )
        )
        result = eval(form, env)
        # Should return list (1 2 3)
        assert isinstance(result, cons)
        assert car(result) == 1
        assert car(cdr(result)) == 2
        assert car(cdr(cdr(result))) == 3
    
    def test_multiple_value_bind_more_vars_than_values(self, env):
        """(MULTIPLE-VALUE-BIND (X Y Z) (VALUES 1 2) (LIST X Y Z))
        should bind X=1, Y=2, Z=NIL."""
        form = cons(
            ls('MULTIPLE-VALUE-BIND'),
            cons(
                cons(ls('X'), cons(ls('Y'), cons(ls('Z'), NIL))),
                cons(
                    cons(ls('VALUES'), cons(1, cons(2, NIL))),
                    cons(
                        cons(ls('LIST'), cons(ls('X'), cons(ls('Y'), cons(ls('Z'), NIL)))),
                        NIL
                    )
                )
            )
        )
        result = eval(form, env)
        # Should return (1 2 NIL)
        assert isinstance(result, cons)
        assert car(result) == 1
        assert car(cdr(result)) == 2
        assert car(cdr(cdr(result))) is NIL
    
    def test_multiple_value_bind_fewer_vars_than_values(self, env):
        """(MULTIPLE-VALUE-BIND (X Y) (VALUES 1 2 3 4) (LIST X Y))
        should bind X=1, Y=2 (ignoring 3 and 4)."""
        form = cons(
            ls('MULTIPLE-VALUE-BIND'),
            cons(
                cons(ls('X'), cons(ls('Y'), NIL)),
                cons(
                    cons(ls('VALUES'), cons(1, cons(2, cons(3, cons(4, NIL))))),
                    cons(
                        cons(ls('LIST'), cons(ls('X'), cons(ls('Y'), NIL))),
                        NIL
                    )
                )
            )
        )
        result = eval(form, env)
        # Should return (1 2)
        assert isinstance(result, cons)
        assert car(result) == 1
        assert car(cdr(result)) == 2
    
    def test_multiple_value_bind_single_value(self, env):
        """(MULTIPLE-VALUE-BIND (X Y) 42 (LIST X Y)) 
        should bind X=42, Y=NIL."""
        form = cons(
            ls('MULTIPLE-VALUE-BIND'),
            cons(
                cons(ls('X'), cons(ls('Y'), NIL)),
                cons(
                    42,  # Single value, not wrapped in VALUES
                    cons(
                        cons(ls('LIST'), cons(ls('X'), cons(ls('Y'), NIL))),
                        NIL
                    )
                )
            )
        )
        result = eval(form, env)
        # Should return (42 NIL)
        assert isinstance(result, cons)
        assert car(result) == 42
        assert car(cdr(result)) is NIL
    
    def test_multiple_value_bind_multiple_body_forms(self, env):
        """(MULTIPLE-VALUE-BIND (X Y) (VALUES 10 20) 
            (SETQ X (+ X 5))  <- side effect
            (+ X Y))  <- last form returned
        should return 35."""
        form = cons(
            ls('MULTIPLE-VALUE-BIND'),
            cons(
                cons(ls('X'), cons(ls('Y'), NIL)),
                cons(
                    cons(ls('VALUES'), cons(10, cons(20, NIL))),
                    cons(
                        cons(ls('SETQ'), cons(ls('X'), cons(
                            cons(ls('+'), cons(ls('X'), cons(5, NIL))),
                            NIL
                        ))),
                        cons(
                            cons(ls('+'), cons(ls('X'), cons(ls('Y'), NIL))),
                            NIL
                        )
                    )
                )
            )
        )
        result = eval(form, env)
        # X should be 10 + 5 = 15, Y = 20, so result = 15 + 20 = 35
        assert result == 35
    
    def test_multiple_value_bind_nested(self, env):
        """(MULTIPLE-VALUE-BIND (X) (VALUES 10)
            (MULTIPLE-VALUE-BIND (Y) (VALUES (+ X 5))
              (+ X Y)))
        should return 25."""
        inner_form = cons(
            ls('MULTIPLE-VALUE-BIND'),
            cons(
                cons(ls('Y'), NIL),
                cons(
                    cons(ls('VALUES'), cons(
                        cons(ls('+'), cons(ls('X'), cons(5, NIL))),
                        NIL
                    )),
                    cons(
                        cons(ls('+'), cons(ls('X'), cons(ls('Y'), NIL))),
                        NIL
                    )
                )
            )
        )
        
        outer_form = cons(
            ls('MULTIPLE-VALUE-BIND'),
            cons(
                cons(ls('X'), NIL),
                cons(
                    cons(ls('VALUES'), cons(10, NIL)),
                    cons(inner_form, NIL)
                )
            )
        )
        
        result = eval(outer_form, env)
        # X=10, Y=10+5=15, X+Y = 10+15 = 25
        assert result == 25
    
    def test_multiple_value_bind_with_function_call(self, env):
        """(MULTIPLE-VALUE-BIND (X Y) (FUNC-RETURNING-TWO-VALUES) X)
        should work with user functions."""
        # First define a function that returns multiple values
        defun_form = cons(
            ls('DEFUN'),
            cons(
                ls('TWO-VALUES'),
                cons(
                    cons(ls('A'), NIL),  # (A)
                    cons(
                        cons(ls('VALUES'), cons(ls('A'), cons(
                            cons(ls('+'), cons(ls('A'), cons(1, NIL))),
                            NIL
                        ))),
                        NIL
                    )
                )
            )
        )
        eval(defun_form, env)
        
        # Now use it in MULTIPLE-VALUE-BIND
        form = cons(
            ls('MULTIPLE-VALUE-BIND'),
            cons(
                cons(ls('X'), cons(ls('Y'), NIL)),
                cons(
                    cons(ls('TWO-VALUES'), cons(100, NIL)),
                    cons(
                        cons(ls('LIST'), cons(ls('X'), cons(ls('Y'), NIL))),
                        NIL
                    )
                )
            )
        )
        result = eval(form, env)
        # TWO-VALUES(100) returns (VALUES 100 101)
        # So X=100, Y=101, result should be (100 101)
        assert isinstance(result, cons)
        assert car(result) == 100
        assert car(cdr(result)) == 101
    
    def test_multiple_value_bind_nil_value(self, env):
        """(MULTIPLE-VALUE-BIND (X) NIL X) should return NIL."""
        form = cons(
            ls('MULTIPLE-VALUE-BIND'),
            cons(
                cons(ls('X'), NIL),
                cons(
                    NIL,
                    cons(ls('X'), NIL)
                )
            )
        )
        result = eval(form, env)
        assert result is NIL
    
    def test_multiple_value_bind_no_vars(self, env):
        """(MULTIPLE-VALUE-BIND () (VALUES 1 2 3) 42) should return 42."""
        form = cons(
            ls('MULTIPLE-VALUE-BIND'),
            cons(
                NIL,  # empty variable list
                cons(
                    cons(ls('VALUES'), cons(1, cons(2, cons(3, NIL)))),
                    cons(42, NIL)
                )
            )
        )
        result = eval(form, env)
        assert result == 42


class TestMultipleValueIntegration:
    """Integration tests for multiple value functions."""
    
    def test_multiple_value_call_with_bind(self, env):
        """(MULTIPLE-VALUE-BIND (X Y) (VALUES 1 2)
            (MULTIPLE-VALUE-CALL #'+ X Y))
        should call (+ 1 2) and return 3."""
        form = cons(
            ls('MULTIPLE-VALUE-BIND'),
            cons(
                cons(ls('X'), cons(ls('Y'), NIL)),
                cons(
                    cons(ls('VALUES'), cons(1, cons(2, NIL))),
                    cons(
                        cons(
                            ls('MULTIPLE-VALUE-CALL'),
                            cons(
                                cons(ls('QUOTE'), cons(ls('+'), NIL)),
                                cons(ls('X'), cons(ls('Y'), NIL))
                            )
                        ),
                        NIL
                    )
                )
            )
        )
        result = eval(form, env)
        assert result == 3
    
    def test_values_list_with_bind(self, env):
        """(MULTIPLE-VALUE-BIND (X Y Z) (VALUES-LIST '(10 20 30)) 
            (+ X Y Z))
        should work correctly."""
        form = cons(
            ls('MULTIPLE-VALUE-BIND'),
            cons(
                cons(ls('X'), cons(ls('Y'), cons(ls('Z'), NIL))),
                cons(
                    cons(
                        ls('VALUES-LIST'),
                        cons(
                            cons(ls('QUOTE'), cons(
                                cons(10, cons(20, cons(30, NIL))),
                                NIL
                            )),
                            NIL
                        )
                    ),
                    cons(
                        cons(ls('+'), cons(ls('X'), cons(ls('Y'), cons(ls('Z'), NIL)))),
                        NIL
                    )
                )
            )
        )
        result = eval(form, env)
        assert result == 60


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
