"""Tests for Phase 4, Task 4: Signaling functions (SIGNAL, ERROR, CERROR, WARN).

This test suite verifies:
- SIGNAL function: raises conditions that may be handled
- ERROR function: raises conditions that halt execution
- CERROR function: raises errors with built-in continue restart
- WARN function: signals non-fatal warnings
"""

import pytest
import fclpy.state as state
from fclpy.lisptype import (
    LispSymbol, lispCons as cons, NIL, T, Condition, SimpleCondition, Warning, Error
)
from fclpy.lispfunc.evaluation import eval, ConditionException
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


class TestErrorFunction:
    """Test ERROR function for raising fatal conditions."""
    
    def test_error_no_args(self, env):
        """(ERROR) raises a non-recoverable ConditionException."""
        # Create form: (ERROR)
        form = cons(ls('ERROR'), NIL)
        
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert exc_info.value.recoverable is False
        assert isinstance(exc_info.value.condition, Condition)
    
    def test_error_is_non_recoverable(self, env):
        """ERROR creates a non-recoverable exception."""
        form = cons(ls('ERROR'), NIL)
        
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert exc_info.value.recoverable is False


class TestSignalFunction:
    """Test SIGNAL function for raising recoverable conditions."""
    
    def test_signal_with_error_object(self, env):
        """SIGNAL raises a ConditionException with recoverable=True."""
        # Create form: (SIGNAL (ERROR))
        form = cons(
            ls('SIGNAL'),
            cons(
                cons(ls('ERROR'), NIL),
                NIL
            )
        )
        
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert exc_info.value.recoverable is True
        assert isinstance(exc_info.value.condition, Condition)
    
    def test_signal_is_recoverable(self, env):
        """SIGNAL raises exception marked as recoverable."""
        form = cons(
            ls('SIGNAL'),
            cons(
                cons(ls('ERROR'), NIL),
                NIL
            )
        )
        
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert exc_info.value.recoverable is True


class TestCerrorFunction:
    """Test CERROR function for errors with continue restart."""
    
    def test_cerror_raises_recoverable_exception(self, env):
        """CERROR raises an exception marked as recoverable."""
        # Create form: (CERROR "Continue anyway" (ERROR))
        form = cons(
            ls('CERROR'),
            cons(
                "Continue anyway",
                cons(
                    cons(ls('ERROR'), NIL),
                    NIL
                )
            )
        )
        
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert isinstance(exc_info.value.condition, Condition)
        assert exc_info.value.recoverable is True
    
    def test_cerror_stores_continue_format(self, env):
        """CERROR stores the continue format string."""
        continue_msg = "Press space to continue"
        form = cons(
            ls('CERROR'),
            cons(
                continue_msg,
                cons(
                    cons(ls('ERROR'), NIL),
                    NIL
                )
            )
        )
        
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert hasattr(exc_info.value, 'continue_format')
        assert exc_info.value.continue_format == continue_msg


class TestWarnFunction:
    """Test WARN function for non-fatal warnings."""
    
    def test_warn_returns_nil(self, env):
        """WARN returns NIL (doesn't interrupt execution)."""
        # Create form: (WARN "This is a warning")
        form = cons(
            ls('WARN'),
            cons("This is a warning", NIL)
        )
        
        result = eval(form, env)
        assert result is NIL
    
    def test_warn_with_error_object(self, env):
        """WARN accepts an error/condition object and returns NIL."""
        # Create form: (WARN (ERROR))
        form = cons(
            ls('WARN'),
            cons(
                cons(ls('ERROR'), NIL),
                NIL
            )
        )
        
        result = eval(form, env)
        assert result is NIL
    
    def test_warn_does_not_raise_exception(self, env):
        """WARN does not raise an exception (it's non-fatal)."""
        # Create form: (PROGN (WARN "Warning 1") (WARN "Warning 2") (+ 1 2))
        form = cons(
            ls('PROGN'),
            cons(
                cons(ls('WARN'), cons("Warning 1", NIL)),
                cons(
                    cons(ls('WARN'), cons("Warning 2", NIL)),
                    cons(
                        cons(ls('+'), cons(1, cons(2, NIL))),
                        NIL
                    )
                )
            )
        )
        
        result = eval(form, env)
        assert result == 3


class TestSignalingIntegration:
    """Integration tests combining multiple signaling operations."""
    
    def test_signal_in_function_propagates(self, env):
        """A SIGNAL in a function propagates to caller."""
        # Define a function that signals: (DEFUN SIGNAL-ERROR () (SIGNAL (ERROR)))
        defun_form = cons(
            ls('DEFUN'),
            cons(
                ls('SIGNAL-ERROR'),
                cons(
                    NIL,  # parameter list
                    cons(
                        cons(
                            ls('SIGNAL'),
                            cons(
                                cons(ls('ERROR'), NIL),
                                NIL
                            )
                        ),
                        NIL
                    )
                )
            )
        )
        eval(defun_form, env)
        
        # Call the function: (SIGNAL-ERROR)
        form = cons(ls('SIGNAL-ERROR'), NIL)
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert exc_info.value.recoverable is True
    
    def test_nested_error_calls(self, env):
        """Nested function calls that raise errors propagate correctly."""
        # Define INNER: (DEFUN INNER () (ERROR))
        inner_form = cons(
            ls('DEFUN'),
            cons(
                ls('INNER'),
                cons(
                    NIL,  # no parameters
                    cons(
                        cons(ls('ERROR'), NIL),
                        NIL
                    )
                )
            )
        )
        eval(inner_form, env)
        
        # Define OUTER: (DEFUN OUTER () (INNER))
        outer_form = cons(
            ls('DEFUN'),
            cons(
                ls('OUTER'),
                cons(
                    NIL,  # no parameters
                    cons(
                        cons(ls('INNER'), NIL),
                        NIL
                    )
                )
            )
        )
        eval(outer_form, env)
        
        # Call OUTER: (OUTER)
        form = cons(ls('OUTER'), NIL)
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert isinstance(exc_info.value.condition, Condition)
        assert exc_info.value.recoverable is False


class TestConditionExceptionProperties:
    """Test properties of ConditionException."""
    
    def test_condition_exception_has_condition_attribute(self, env):
        """ConditionException stores the condition object."""
        form = cons(ls('ERROR'), NIL)
        
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert hasattr(exc_info.value, 'condition')
        assert isinstance(exc_info.value.condition, Condition)
    
    def test_condition_exception_has_recoverable_attribute(self, env):
        """ConditionException tracks recoverability."""
        # Non-recoverable (ERROR)
        form1 = cons(ls('ERROR'), NIL)
        with pytest.raises(ConditionException) as exc_info:
            eval(form1, env)
        assert exc_info.value.recoverable is False
        
        # Recoverable (SIGNAL)
        form2 = cons(
            ls('SIGNAL'),
            cons(
                cons(ls('ERROR'), NIL),
                NIL
            )
        )
        with pytest.raises(ConditionException) as exc_info:
            eval(form2, env)
        assert exc_info.value.recoverable is True
        
        # Recoverable (CERROR)
        form3 = cons(
            ls('CERROR'),
            cons(
                "continue",
                cons(
                    cons(ls('ERROR'), NIL),
                    NIL
                )
            )
        )
        with pytest.raises(ConditionException) as exc_info:
            eval(form3, env)
        assert exc_info.value.recoverable is True


class TestSignalingEdgeCases:
    """Test edge cases and error handling in signaling."""
    
    def test_warn_multiple_times(self, env):
        """Can issue multiple warnings without interference."""
        # (PROGN (WARN "First") (WARN "Second") (WARN "Third") 42)
        form = cons(
            ls('PROGN'),
            cons(
                cons(ls('WARN'), cons("First", NIL)),
                cons(
                    cons(ls('WARN'), cons("Second", NIL)),
                    cons(
                        cons(ls('WARN'), cons("Third", NIL)),
                        cons(42, NIL)
                    )
                )
            )
        )
        result = eval(form, env)
        assert result == 42
    
    def test_signal_and_error_both_raise(self, env):
        """Both SIGNAL and ERROR raise exceptions (though recoverable differs)."""
        # SIGNAL
        form1 = cons(
            ls('SIGNAL'),
            cons(
                cons(ls('ERROR'), NIL),
                NIL
            )
        )
        with pytest.raises(ConditionException):
            eval(form1, env)
        
        # ERROR
        form2 = cons(ls('ERROR'), NIL)
        with pytest.raises(ConditionException):
            eval(form2, env)
        
        # Verify recoverability difference
        with pytest.raises(ConditionException) as exc1:
            eval(form1, env)
        signal_exc = exc1.value
        
        with pytest.raises(ConditionException) as exc2:
            eval(form2, env)
        error_exc = exc2.value
        
        assert signal_exc.recoverable is True
        assert error_exc.recoverable is False


class TestSignalingWithEnvironment:
    """Test signaling with different environment configurations."""
    
    def test_signal_in_nested_environment(self, env):
        """SIGNAL works correctly in nested environments."""
        # Define: (DEFUN LOCAL-SIGNAL (X) (IF (> X 0) (ERROR) X))
        defun_form = cons(
            ls('DEFUN'),
            cons(
                ls('LOCAL-SIGNAL'),
                cons(
                    cons(ls('X'), NIL),  # parameter list
                    cons(
                        cons(
                            ls('IF'),
                            cons(
                                cons(ls('>'), cons(ls('X'), cons(0, NIL))),
                                cons(
                                    cons(ls('ERROR'), NIL),
                                    cons(ls('X'), NIL)
                                )
                            )
                        ),
                        NIL
                    )
                )
            )
        )
        eval(defun_form, env)
        
        # Call: (LOCAL-SIGNAL 5)
        form = cons(ls('LOCAL-SIGNAL'), cons(5, NIL))
        with pytest.raises(ConditionException):
            eval(form, env)
    
    def test_signal_in_block_context(self, env):
        """SIGNAL inside a BLOCK propagates correctly."""
        # (BLOCK TEST (ERROR))
        form = cons(
            ls('BLOCK'),
            cons(
                ls('TEST'),
                cons(
                    cons(ls('ERROR'), NIL),
                    NIL
                )
            )
        )
        
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert isinstance(exc_info.value.condition, Condition)


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

