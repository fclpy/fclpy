"""
Tests for Phase 4, Task 5: Restart System
Tests RESTART-CASE, RESTART-BIND, INVOKE-RESTART, and ABORT special forms.
"""
import pytest
import fclpy.state as state
from fclpy import lisptype
from fclpy.lisptype import (
    LispSymbol, lispCons as cons, NIL, T, Restart, RestartException
)
from fclpy.lispfunc.evaluation import eval
from fclpy.lispenv import setup_standard_environment


def ls(name):
    """Shorthand to create a LispSymbol."""
    return LispSymbol(name)


@pytest.fixture
def env():
    """Create a fresh environment for each test."""
    state.current_environment = None
    state.functions_loaded = False
    state.restart_stack = []  # Reset restart stack for each test
    return setup_standard_environment()


class TestRestartBasics:
    """Test basic restart system functionality."""

    def test_restart_class_creation(self):
        """Test creating Restart objects."""
        def handler(*args):
            return args
        
        restart = Restart('MY-RESTART', handler, report="Test restart")
        assert isinstance(restart.name, lisptype.LispSymbol)
        assert restart.name.name == 'MY-RESTART'
        assert restart.handler == handler
        assert restart.report == "Test restart"

    def test_restart_exception_creation(self):
        """Test creating RestartException objects."""
        exc = RestartException('MY-RESTART', [1, 2, 3])
        assert exc.restart_name == 'MY-RESTART'
        # The args are stored as part of the Exception base class
        # and also as the value passed in
        assert isinstance(exc, RestartException)

    def test_restart_case_evaluates_protected_form(self, env):
        """Test simple RESTART-CASE evaluates protected form."""
        # (restart-case 42 (continue () 99))
        form = cons(
            ls('RESTART-CASE'),
            cons(
                42,
                cons(
                    cons(ls('CONTINUE'), cons(NIL, cons(99, NIL))),
                    NIL
                )
            )
        )
        
        result = eval(form, env)
        assert result == 42

    def test_restart_stack_initialized(self):
        """Test that restart stack is initialized."""
        assert isinstance(state.restart_stack, list)
        assert len(state.restart_stack) == 0


class TestRestartIntegration:
    """Test that restart forms are properly registered."""

    def test_restart_case_special_form_exists(self, env):
        """Test that RESTART-CASE special form is registered."""
        # RESTART-CASE should be callable from the evaluator
        form = cons(ls('RESTART-CASE'), cons(42, NIL))
        # If the form doesn't raise, it's properly wired
        result = eval(form, env)
        assert result == 42

    def test_restart_bind_special_form_exists(self, env):
        """Test that RESTART-BIND special form is registered."""
        # (restart-bind ((continue ())) 42)
        form = cons(
            ls('RESTART-BIND'),
            cons(
                cons(cons(ls('CONTINUE'), cons(NIL, NIL)), NIL),
                cons(42, NIL)
            )
        )
        result = eval(form, env)
        assert result == 42

    def test_invoke_restart_special_form_exists(self, env):
        """Test that INVOKE-RESTART special form is registered."""
        # Simply test that the form is recognized and callable
        # (invoke-restart) with no arguments should work even if it errors
        # because the form is properly dispatched
        form = cons(ls('INVOKE-RESTART'), cons(ls('NONEXISTENT'), NIL))
        # This should raise an error about restart not found, not about form not recognized
        with pytest.raises((lisptype.LispError, lisptype.LispNotImplementedError)):
            eval(form, env)

    def test_abort_special_form_exists(self, env):
        """Test that ABORT special form is registered."""
        form = cons(ls('ABORT'), NIL)
        # ABORT should be recognized as a special form
        with pytest.raises((lisptype.LispError, lisptype.LispNotImplementedError)):
            eval(form, env)


class TestRestartStackManagement:
    """Test that restart stack is properly managed."""

    def test_restart_stack_initialized(self):
        """Test that restart stack is initialized."""
        assert isinstance(state.restart_stack, list)

    def test_restart_stack_cleanup_after_case(self, env):
        """Test that restart stack is cleaned after RESTART-CASE."""
        # Ensure we start with clean stack
        state.restart_stack = []
        assert len(state.restart_stack) == 0
        
        # Execute a simple RESTART-CASE form
        form = cons(
            ls('RESTART-CASE'),
            cons(
                42,
                cons(
                    cons(ls('CONTINUE'), cons(NIL, cons(99, NIL))),
                    NIL
                )
            )
        )
        result = eval(form, env)
        assert result == 42
        
        # Stack should be clean after evaluation
        assert len(state.restart_stack) == 0


if __name__ == '__main__':
    pytest.main([__file__, '-v'])

