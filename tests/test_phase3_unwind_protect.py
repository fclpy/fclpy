"""Tests for Task 8: UNWIND-PROTECT cleanup support."""

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


class TestUnwindProtect:
    """Test UNWIND-PROTECT cleanup support."""

    def test_unwind_protect_normal_completion(self, env):
        """UNWIND-PROTECT should run cleanup on normal completion."""
        # Setup: define variable for tracking
        cleanup_var = ls('CLEANUP-RAN')
        env.set_variable(cleanup_var, NIL)
        
        # Form: (UNWIND-PROTECT (+ 1 2) (SETQ CLEANUP-RAN T))
        protected_form = cons(ls('+'), cons(1, cons(2, NIL)))
        cleanup_form = cons(ls('SETQ'), cons(cleanup_var, cons(T, NIL)))
        unwind_form = cons(ls('UNWIND-PROTECT'), cons(protected_form, cons(cleanup_form, NIL)))
        
        result = eval(unwind_form, env)
        
        # Result should be 3 (the result of protected form)
        assert result == 3
        # Cleanup var should be T
        assert env.find_variable(cleanup_var) is T

    def test_unwind_protect_multiple_cleanups(self, env):
        """UNWIND-PROTECT can have multiple cleanup forms (PROGN them)."""
        c1_var = ls('C1')
        c2_var = ls('C2')
        env.set_variable(c1_var, NIL)
        env.set_variable(c2_var, NIL)
        
        cleanup1 = cons(ls('SETQ'), cons(c1_var, cons(T, NIL)))
        cleanup2 = cons(ls('SETQ'), cons(c2_var, cons(T, NIL)))
        
        protected_form = 42
        unwind_form = cons(ls('UNWIND-PROTECT'), cons(protected_form, cons(cleanup1, cons(cleanup2, NIL))))
        
        result = eval(unwind_form, env)
        
        # Result should be 42
        assert result == 42
        # Both cleanup vars should be T
        assert env.find_variable(c1_var) is T
        assert env.find_variable(c2_var) is T

    def test_unwind_protect_with_return_from(self, env):
        """UNWIND-PROTECT cleanup should run even when RETURN-FROM exits the block."""
        cleanup_var = ls('CLEANUP-RAN')
        block_name = ls('MYBLOCK')
        env.set_variable(cleanup_var, NIL)
        
        cleanup_form = cons(ls('SETQ'), cons(cleanup_var, cons(T, NIL)))
        return_form = cons(ls('RETURN-FROM'), cons(block_name, cons(42, NIL)))
        unwind_form = cons(ls('UNWIND-PROTECT'), cons(return_form, cons(cleanup_form, NIL)))
        block_form = cons(ls('BLOCK'), cons(block_name, cons(unwind_form, NIL)))
        
        result = eval(block_form, env)
        
        # Result should be 42 (the returned value)
        assert result == 42
        # Cleanup var should be T
        assert env.find_variable(cleanup_var) is T

    def test_unwind_protect_with_throw(self, env):
        """UNWIND-PROTECT cleanup should run even when THROW exits."""
        cleanup_var = ls('CLEANUP-RAN')
        tag = ls('MYTAG')
        env.set_variable(cleanup_var, NIL)
        
        # Create quoted tag for CATCH and THROW
        quote_sym = ls('QUOTE')
        quoted_tag = cons(quote_sym, cons(tag, NIL))
        
        cleanup_form = cons(ls('SETQ'), cons(cleanup_var, cons(T, NIL)))
        throw_form = cons(ls('THROW'), cons(quoted_tag, cons(99, NIL)))
        unwind_form = cons(ls('UNWIND-PROTECT'), cons(throw_form, cons(cleanup_form, NIL)))
        catch_form = cons(ls('CATCH'), cons(quoted_tag, cons(unwind_form, NIL)))
        
        result = eval(catch_form, env)
        
        # Result should be 99 (the thrown value)
        assert result == 99
        # Cleanup var should be T
        assert env.find_variable(cleanup_var) is T

    def test_unwind_protect_nested(self, env):
        """UNWIND-PROTECT can be nested - all should run."""
        c1_var = ls('C1')
        c2_var = ls('C2')
        env.set_variable(c1_var, NIL)
        env.set_variable(c2_var, NIL)
        
        cleanup1 = cons(ls('SETQ'), cons(c1_var, cons(T, NIL)))
        cleanup2 = cons(ls('SETQ'), cons(c2_var, cons(T, NIL)))
        
        inner_unwind = cons(ls('UNWIND-PROTECT'), 
                           cons(42, cons(cleanup2, NIL)))
        outer_unwind = cons(ls('UNWIND-PROTECT'), 
                           cons(inner_unwind, cons(cleanup1, NIL)))
        
        result = eval(outer_unwind, env)
        
        assert result == 42
        assert env.find_variable(c1_var) is T
        assert env.find_variable(c2_var) is T

    def test_unwind_protect_exception_preserves_cleanup(self, env):
        """Uncaught exception should still run cleanup before re-raising."""
        # This is a complex case - Python exception handling
        # For now, just document this behavior
        # In a full implementation, UNWIND-PROTECT would catch any exception,
        # run cleanup, and re-raise
        pass


class TestBasicCleanup:
    """Test basic cleanup scenarios."""

    def test_cleanup_runs_in_finally_block(self, env):
        """UNWIND-PROTECT is like Python try/finally."""
        # Simple case: ensure cleanup always runs
        cleanup_var = ls('CLEANUP')
        env.set_variable(cleanup_var, NIL)
        
        cleanup = cons(ls('SETQ'), cons(cleanup_var, cons(1, NIL)))
        protected = cons(ls('UNWIND-PROTECT'), cons(100, cons(cleanup, NIL)))
        
        result = eval(protected, env)
        assert result == 100
        assert env.find_variable(cleanup_var) == 1
