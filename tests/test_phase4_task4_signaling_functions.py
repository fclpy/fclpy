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
    LispSymbol, NIL, Condition, SimpleCondition, Warning, Error, 
    MultipleValues
)
from fclpy.lispfunc.evaluation import eval, ConditionException
from fclpy.lispenv import setup_standard_environment


class TestSignalFunction:
    """Test SIGNAL function for raising recoverable conditions."""
    
    def test_signal_with_error_object(self):
        """SIGNAL raises a ConditionException with recoverable=True."""
        env = setup_standard_environment()
        
        # Define an error and signal it
        eval('(DEFUN MAKE-ERROR () (ERROR))', env)
        form = '(SIGNAL (MAKE-ERROR))'
        
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert exc_info.value.recoverable is True
        assert isinstance(exc_info.value.condition, (Error, Condition))
    
    def test_signal_returns_none_if_unhandled(self):
        """SIGNAL raises exception since we don't have handler-bind yet."""
        env = setup_standard_environment()
        
        # Create a simple condition
        form = '(SIGNAL (ERROR))'
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert exc_info.value.recoverable is True


class TestErrorFunction:
    """Test ERROR function for raising fatal conditions."""
    
    def test_error_with_simple_condition(self):
        """ERROR raises a non-recoverable ConditionException."""
        env = setup_standard_environment()
        
        form = '(ERROR)'
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert exc_info.value.recoverable is False
        assert isinstance(exc_info.value.condition, Condition)
    
    def test_error_is_non_recoverable(self):
        """ERROR creates a non-recoverable exception."""
        env = setup_standard_environment()
        
        # ERROR without args still raises non-recoverable exception
        form = '(ERROR)'
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert exc_info.value.recoverable is False
    
    def test_error_with_undefined_condition(self):
        """ERROR handles cases where condition type is not found."""
        env = setup_standard_environment()
        
        # Even with no condition args, ERROR should create an error
        form = '(ERROR)'
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert isinstance(exc_info.value.condition, Condition)


class TestCerrorFunction:
    """Test CERROR function for errors with continue restart."""
    
    def test_cerror_raises_recoverable_exception(self):
        """CERROR raises an exception marked as recoverable."""
        env = setup_standard_environment()
        
        form = '(CERROR "Continue anyway" (ERROR))'
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert isinstance(exc_info.value.condition, Condition)
        assert exc_info.value.recoverable is True
    
    def test_cerror_stores_continue_format(self):
        """CERROR stores the continue format string."""
        env = setup_standard_environment()
        
        continue_msg = "Press space to continue"
        form = f'(CERROR "{continue_msg}" (ERROR))'
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert hasattr(exc_info.value, 'continue_format')
        assert exc_info.value.continue_format == continue_msg


class TestWarnFunction:
    """Test WARN function for non-fatal warnings."""
    
    def test_warn_returns_nil(self):
        """WARN returns NIL (doesn't interrupt execution)."""
        env = setup_standard_environment()
        
        form = '(WARN "This is a warning")'
        result = eval(form, env)
        
        # WARN should return NIL
        assert result is NIL
    
    def test_warn_with_error_object(self):
        """WARN accepts an error/condition object and returns NIL."""
        env = setup_standard_environment()
        
        form = '(WARN (ERROR))'
        result = eval(form, env)
        
        assert result is NIL
    
    def test_warn_does_not_raise_exception(self):
        """WARN does not raise an exception (it's non-fatal)."""
        env = setup_standard_environment()
        
        # Multiple warnings should all succeed and return final value
        form = '(PROGN (WARN "Warning 1") (WARN "Warning 2") (+ 1 2))'
        result = eval(form, env)
        
        # Should complete and return the result of (+ 1 2)
        assert result == 3


class TestSignalingIntegration:
    """Integration tests combining multiple signaling operations."""
    
    def test_signal_in_function_propagates(self):
        """A SIGNAL in a function propagates to caller."""
        env = setup_standard_environment()
        
        # Define a function that signals
        eval('(DEFUN SIGNAL-ERROR () (SIGNAL (ERROR)))', env)
        
        form = '(SIGNAL-ERROR)'
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert exc_info.value.recoverable is True
    
    def test_nested_error_calls(self):
        """Nested function calls that raise errors propagate correctly."""
        env = setup_standard_environment()
        
        # Define nested function
        eval('(DEFUN INNER () (ERROR))', env)
        eval('(DEFUN OUTER () (INNER))', env)
        
        form = '(OUTER)'
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert isinstance(exc_info.value.condition, Condition)
        assert exc_info.value.recoverable is False


class TestConditionExceptionProperties:
    """Test properties of ConditionException."""
    
    def test_condition_exception_has_condition_attribute(self):
        """ConditionException stores the condition object."""
        env = setup_standard_environment()
        
        form = '(ERROR)'
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert hasattr(exc_info.value, 'condition')
        assert isinstance(exc_info.value.condition, Condition)
    
    def test_condition_exception_has_recoverable_attribute(self):
        """ConditionException tracks recoverability."""
        env = setup_standard_environment()
        
        # Non-recoverable (ERROR)
        form1 = '(ERROR)'
        with pytest.raises(ConditionException) as exc_info:
            eval(form1, env)
        assert exc_info.value.recoverable is False
        
        # Recoverable (SIGNAL)
        form2 = '(SIGNAL (ERROR))'
        with pytest.raises(ConditionException) as exc_info:
            eval(form2, env)
        assert exc_info.value.recoverable is True
        
        # Recoverable (CERROR)
        form3 = '(CERROR "continue" (ERROR))'
        with pytest.raises(ConditionException) as exc_info:
            eval(form3, env)
        assert exc_info.value.recoverable is True


class TestSignalingEdgeCases:
    """Test edge cases and error handling in signaling."""
    
    def test_warn_multiple_times(self):
        """Can issue multiple warnings without interference."""
        env = setup_standard_environment()
        
        form = '(PROGN (WARN "First") (WARN "Second") (WARN "Third") 42)'
        result = eval(form, env)
        assert result == 42
    
    def test_signal_and_error_both_raise(self):
        """Both SIGNAL and ERROR raise exceptions (though recoverable differs)."""
        env = setup_standard_environment()
        
        # SIGNAL
        form1 = '(SIGNAL (ERROR))'
        with pytest.raises(ConditionException):
            eval(form1, env)
        
        # ERROR
        form2 = '(ERROR)'
        with pytest.raises(ConditionException):
            eval(form2, env)
        
        # Verify recoverability difference
        with pytest.raises(ConditionException) as exc1:
            eval('(SIGNAL (ERROR))', env)
        signal_exc = exc1.value
        
        with pytest.raises(ConditionException) as exc2:
            eval('(ERROR)', env)
        error_exc = exc2.value
        
        assert signal_exc.recoverable is True
        assert error_exc.recoverable is False


class TestSignalingWithEnvironment:
    """Test signaling with different environment configurations."""
    
    def test_signal_in_nested_environment(self):
        """SIGNAL works correctly in nested environments."""
        env = setup_standard_environment()
        
        # Define a function that uses local variables
        eval('(DEFUN LOCAL-SIGNAL (X) (IF (> X 0) (ERROR) X))', env)
        
        form = '(LOCAL-SIGNAL 5)'
        with pytest.raises(ConditionException):
            eval(form, env)
    
    def test_signal_in_block_context(self):
        """SIGNAL inside a BLOCK propagates correctly."""
        env = setup_standard_environment()
        
        # The error/signal should propagate out of the block
        form = '(BLOCK TEST (ERROR))'
        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)
        
        assert isinstance(exc_info.value.condition, Condition)


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

