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
    """Test SIGNAL, which offers a condition to the handlers without unwinding."""

    def test_signal_returns_nil_when_unhandled(self, env):
        """(SIGNAL datum) returns NIL if no handler transfers control.

        CLHS SIGNAL: signaling a condition no handler handles simply returns
        NIL -- SIGNAL does not unwind and does not enter the debugger for a
        non-serious condition. This previously raised unconditionally, so a
        SIGNAL nobody handled aborted the rest of the enclosing form.
        """
        form = cons(ls('SIGNAL'), cons("a simple condition", NIL))

        assert eval(form, env) == NIL

    def test_signal_of_string_datum_builds_a_simple_condition(self, env):
        """SIGNAL's default condition type is SIMPLE-CONDITION, not an ERROR.

        This is what keeps (HANDLER-BIND ((ERROR ...)) (SIGNAL "...")) from
        wrongly invoking the ERROR handler: a SIMPLE-CONDITION is not an ERROR
        subtype. Previously SIGNAL signaled whatever its argument evaluated to,
        which ConditionException then wrapped in a generic ERROR.
        """
        from fclpy.lispfunc.evaluation_conditions import build_condition
        import fclpy.lisptype as lisptype

        condition = build_condition("a simple condition", [], lisptype.SimpleCondition)
        assert isinstance(condition, lisptype.SimpleCondition)
        assert not isinstance(condition, lisptype.Error)
        assert condition.get_slot('format-control') == "a simple condition"

    def test_condition_signaled_while_evaluating_datum_propagates_unchanged(self, env):
        """A condition signaled by SIGNAL's *argument* is not SIGNAL's condition.

        (SIGNAL (ERROR)) evaluates (ERROR) first, which signals a
        non-recoverable error; that must propagate as-is. SIGNAL used to catch
        it and re-raise it as its own recoverable condition, silently changing
        the severity of an unrelated error.
        """
        form = cons(
            ls('SIGNAL'),
            cons(
                cons(ls('ERROR'), NIL),
                NIL
            )
        )

        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)

        assert exc_info.value.recoverable is False
        assert isinstance(exc_info.value.condition, Condition)


class TestCerrorFunction:
    """Test CERROR function for errors with a built-in CONTINUE restart.

    `ConditionException.recoverable`/`.continue_format` (asserted by the two
    tests this replaces) were flags set at the raise site and never read by
    anything -- CERROR had no actual restart behind them, so nothing could
    ever "continue" from one. Recoverability is now a real CONTINUE restart
    (CLHS 9.1, `evaluation_conditions._signal_cerror_object`), so these test
    that instead: invoking it (directly, or via a handler calling CONTINUE)
    makes CERROR return NIL and resume, exactly what the dead flags claimed
    to represent but never delivered.
    """

    def test_cerror_raises_when_uncaught(self, env):
        """CERROR signals an error, same as ERROR, when nothing invokes its
        CONTINUE restart."""
        form = cons(
            ls('CERROR'),
            cons(
                "Continue anyway",
                cons("the error", NIL)
            )
        )

        with pytest.raises(ConditionException) as exc_info:
            eval(form, env)

        assert isinstance(exc_info.value.condition, Condition)

    def test_cerror_continue_restart_resumes_execution(self, env):
        """A handler that invokes CERROR's CONTINUE restart makes CERROR
        return NIL instead of unwinding, and the enclosing form resumes."""
        from fclpy.lispfunc import eval_string

        result = eval_string(
            "(handler-bind"
            "  ((error #'(lambda (c) (invoke-restart (find-restart 'continue c)))))"
            "  (progn (cerror \"Continue anyway\" \"the error\") 'resumed))",
            env)
        assert getattr(result, 'name', result) == 'RESUMED'

    def test_cerror_continue_restart_report_uses_continue_format(self, env):
        """CERROR's CONTINUE restart reports itself via the given
        continue-format-control (CLHS 9.1)."""
        from fclpy.lispfunc import eval_string

        result = eval_string(
            "(with-output-to-string (s)"
            "  (handler-bind"
            "    ((error #'(lambda (c)"
            "                (let ((*print-escape* nil))"
            "                  (format s \"~A\" (find-restart 'continue c)))"
            "                (invoke-restart (find-restart 'continue c)))))"
            "    (cerror \"Press space to continue\" \"the error\")))",
            env)
        assert "Press space to continue" in str(result)


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
    
    def test_warn_with_condition_type_designator(self, env):
        """WARN accepts a condition-type designator (a symbol naming a
        condition type) and returns NIL, per the ANSI condition-designator
        rules shared with ERROR/SIGNAL/CERROR.

        Note: (WARN (ERROR)) is *not* a way to pass "an error object" to
        WARN - evaluating the (ERROR) argument form signals unconditionally
        before WARN is ever entered, per ANSI argument evaluation order.
        Swallowing that signal (the previous version of this test) was the
        silent-failure pattern flagged in plan.md's standing rules, not
        correct WARN behavior.
        """
        # Create form: (WARN 'SIMPLE-WARNING)
        form = cons(
            ls('WARN'),
            cons(
                cons(ls('QUOTE'), cons(ls('SIMPLE-WARNING'), NIL)),
                NIL
            )
        )

        result = eval(form, env)
        assert result is NIL

    def test_warn_argument_evaluation_signals_normally(self, env):
        """Evaluating WARN's datum argument is ordinary argument evaluation:
        if the argument form itself signals (e.g. it calls ERROR), that
        signal propagates before WARN ever runs - it must not be swallowed.
        """
        # Create form: (WARN (ERROR))
        form = cons(
            ls('WARN'),
            cons(
                cons(ls('ERROR'), NIL),
                NIL
            )
        )

        with pytest.raises(ConditionException):
            eval(form, env)
    
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
        """An error signaled inside a function propagates to the caller.

        The body here is (SIGNAL (ERROR)): the inner (ERROR) is what signals,
        so what reaches the caller is that non-recoverable error, unchanged.
        """
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

        assert exc_info.value.recoverable is False
    
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
        """ERROR's ConditionException is not recoverable; CERROR's carries a
        real CONTINUE restart instead of the `recoverable` flag (see
        TestCerrorFunction above for that restart actually being invoked)."""
        # Non-recoverable (ERROR)
        form1 = cons(ls('ERROR'), NIL)
        with pytest.raises(ConditionException) as exc_info:
            eval(form1, env)
        assert exc_info.value.recoverable is False

        # CERROR, uncaught, still raises -- its CONTINUE restart was never
        # invoked -- but the condition it carries is the one that restart
        # was associated with.
        form2 = cons(
            ls('CERROR'),
            cons(
                "continue",
                cons("the error", NIL)
            )
        )
        with pytest.raises(ConditionException) as exc_info:
            eval(form2, env)
        assert isinstance(exc_info.value.condition, Condition)


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
    
    def test_error_unwinds_but_signal_does_not(self, env):
        """ERROR never returns; SIGNAL returns NIL when unhandled.

        This is the difference CLHS draws between the two, and the one the
        previous implementation erased by making both raise: SIGNAL only
        transfers control if a *handler* chooses to.
        """
        # (SIGNAL "a simple condition") -- nothing handles it, so it returns NIL
        signal_form = cons(ls('SIGNAL'), cons("a simple condition", NIL))
        assert eval(signal_form, env) == NIL

        # (ERROR) -- unhandled, so it unwinds
        error_form = cons(ls('ERROR'), NIL)
        with pytest.raises(ConditionException) as exc_info:
            eval(error_form, env)
        assert exc_info.value.recoverable is False


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

