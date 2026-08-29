"""Regression tests for the ERROR/SIGNAL condition-designator fixes.

Context: the ANSI suite's conditions/error.lsp used to abort the entire
22036-test run (see plan.md, M0 "Current status") because (error fmt) with
fmt bound to a FORMATTER-produced function signaled a raw Python function
object as "the condition" -- unmatchable by any handler, so it escaped every
enclosing HANDLER-CASE/HANDLER-BIND, including RT's own top-level (error (c)
...) handler in ansi-test/rt.lsp, and killed the whole test process. These
tests cover the underlying mechanisms fixed along the way, at the level
tests/test_phase4_task3_condition_hierarchy.py already establishes (direct
construction, no full ansi-test harness needed) -- ansi-test/conditions/
error.lsp (error.1-error.12) is the authoritative end-to-end regression test
and now passes in full; see run_do_test.py to reproduce it directly.
"""

import pytest
import fclpy.state as state
import fclpy.lisptype as lisptype
from fclpy.lisptype import (
    Condition, SimpleCondition, SimpleError, SimpleWarning, Error, Warning,
)
from fclpy.lispfunc.evaluation_core import ConditionException
from fclpy.lispfunc.evaluation_conditions import _condition_matches
from fclpy.lispfunc import setup_environment, eval_string


class TestConditionExceptionInvariant:
    """ConditionException must never smuggle a non-Condition object through
    as ".condition" -- that object becomes unmatchable by any handler type,
    including T, since _condition_matches can't isinstance() a bare value.
    """

    def test_wraps_arbitrary_object_into_a_condition(self):
        def not_a_condition():
            pass

        exc = ConditionException(not_a_condition, recoverable=False)
        assert isinstance(exc.condition, Condition)

    def test_wraps_string_into_a_condition(self):
        exc = ConditionException("just a string", recoverable=False)
        assert isinstance(exc.condition, Condition)

    def test_passes_through_a_real_condition_unchanged(self):
        cond = SimpleError(format_control="boom")
        exc = ConditionException(cond, recoverable=False)
        assert exc.condition is cond


class TestSimpleConditionMixin:
    """CLHS Figure 9-1: simple-error is (error simple-condition) and
    simple-warning is (warning simple-condition) -- true multiple
    inheritance, not just error/warning. Without the SimpleCondition parent,
    (typep <simple-error> 'simple-condition) is NIL and any code (including
    the ANSI suite's own FROB-SIMPLE-CONDITION helper) keyed on
    SIMPLE-CONDITION silently fails to match.
    """

    def test_simple_error_is_simple_condition(self):
        err = SimpleError(format_control="boom")
        assert isinstance(err, SimpleCondition)
        assert isinstance(err, Error)

    def test_simple_warning_is_simple_condition(self):
        warn = SimpleWarning(format_control="boom")
        assert isinstance(warn, SimpleCondition)
        assert isinstance(warn, Warning)

    def test_simple_error_message_survives_lispstring_format_control(self):
        # A LispString has no str base (plan.md Finding I); str(condition)
        # must still return a plain str, not the LispString object itself.
        ls = lisptype.LispString("boom")
        err = SimpleError(format_control=ls)
        assert isinstance(str(err), str)

    def test_callable_format_control_does_not_crash_message(self):
        # FORMATTER-style format controls are functions, not strings; the
        # message slot must stay a plain string (empty is fine) rather than
        # storing the callable and breaking __str__.
        err = SimpleError(format_control=lambda stream, *args: None)
        assert isinstance(str(err), str)


class TestConditionMatchesUsesRealLattice:
    """_condition_matches used to fall back to 'assume it's an ERROR' for
    every condition type name it didn't have a hardcoded table entry for --
    which incorrectly matched SIMPLE-CONDITION/SIMPLE-WARNING (real ANSI
    types that are *not* ERROR subtypes) against an (ERROR (C) ...) clause.
    """

    def test_simple_condition_does_not_match_error(self):
        cond = SimpleCondition(message="just a condition")
        assert _condition_matches('ERROR', cond) is False

    def test_simple_warning_does_not_match_error(self):
        warn = SimpleWarning(format_control="boo")
        assert _condition_matches('ERROR', warn) is False

    def test_simple_error_matches_error_and_simple_condition(self):
        err = SimpleError(format_control="boom")
        assert _condition_matches('ERROR', err) is True
        assert _condition_matches('SIMPLE-CONDITION', err) is True
        assert _condition_matches('SIMPLE-ERROR', err) is True

    def test_everything_matches_t_and_condition(self):
        for obj in (SimpleCondition(), SimpleWarning(), SimpleError()):
            assert _condition_matches('T', obj) is True
            assert _condition_matches('CONDITION', obj) is True


class TestMakeConditionBuildsRealInstances:
    """MAKE-CONDITION used to be a stub that returned its type-designator
    argument unchanged -- the bare symbol SIMPLE-ERROR, not a condition.
    """

    @pytest.fixture
    def env(self):
        state.current_environment = None
        state.functions_loaded = False
        return setup_environment()

    def test_make_condition_returns_a_condition_instance(self, env):
        result = eval_string("(make-condition 'simple-error :format-control \"boom\")", env)
        assert isinstance(result, Condition)
        assert isinstance(result, SimpleError)

    def test_make_condition_unknown_type_signals(self, env):
        # A raw LispTypeError raised from a registered function gets wrapped
        # into a ConditionException by the evaluator's function-call
        # dispatch; the important invariant is that *something* Condition-
        # shaped and catchable comes out, not a bare symbol echoed back.
        with pytest.raises(ConditionException) as exc_info:
            eval_string("(make-condition 'not-a-real-condition-type)", env)
        assert isinstance(exc_info.value.condition, Condition)

    def test_make_condition_unknown_type_is_catchable_as_error(self, env):
        result = eval_string(
            "(handler-case (make-condition 'not-a-real-condition-type)"
            " (error (c) :caught))",
            env)
        assert str(result) == ':CAUGHT'


class TestTypepRecognizesConditions:
    """TYPEP previously had no branch at all for Condition instances, so
    (typep <simple-error-instance> 'simple-error) fell through to the
    CLOS-class lookup at the bottom and returned NIL.
    """

    @pytest.fixture
    def env(self):
        state.current_environment = None
        state.functions_loaded = False
        return setup_environment()

    def test_typep_simple_error(self, env):
        assert eval_string(
            "(typep (make-condition 'simple-error) 'simple-error)", env) is lisptype.T

    def test_typep_simple_error_as_simple_condition(self, env):
        assert eval_string(
            "(typep (make-condition 'simple-error) 'simple-condition)", env) is lisptype.T

    def test_typep_simple_warning_not_error(self, env):
        assert eval_string(
            "(typep (make-condition 'simple-warning) 'error)", env) is lisptype.NIL


class TestErrorWithFormatterDoesNotCrash:
    """The end-to-end regression: (error fmt) where fmt is a FORMATTER
    result used to raise a raw Python function as the signaled condition,
    which no HANDLER-CASE clause could ever match, escaping uncaught.
    """

    @pytest.fixture
    def env(self):
        state.current_environment = None
        state.functions_loaded = False
        return setup_environment()

    def test_error_with_formatter_datum_is_a_matchable_simple_error(self, env):
        result = eval_string(
            "(let ((fmt (formatter \"Error\")))"
            " (handler-case (error fmt) (simple-error (c) c)))",
            env)
        assert isinstance(result, SimpleError)

    def test_format_with_function_control_string_and_nil_destination(self, env):
        result = eval_string(
            "(let ((fmt (formatter \"hi\"))) (format nil fmt))", env)
        # FORMAT to a NIL destination returns a string. It used to return a
        # bare Python `str` (standing rule 2: a Python object appearing as a
        # Lisp value is a bug); the string-output stream model now returns
        # the LispString CLHS 21.2 requires, so assert on Lisp string-ness
        # and content rather than on the Python type.
        assert isinstance(result, lisptype.LispString) and str(result) == "hi"


class TestLoopAsSynonym:
    """CLHS 6.1.2.1: "either the keyword FOR or the keyword AS may be used
    to begin a for-as-clause" -- AS was previously unrecognized, so e.g.
    (loop as x in '(1 2 3) ...) fell into the zero-iteration-clause "simple
    LOOP" branch and looped forever over inert AS/X/IN body forms until the
    10-minute LOOP_TIMEOUT_ERROR hard cap fired.
    """

    @pytest.fixture
    def env(self):
        state.current_environment = None
        state.functions_loaded = False
        return setup_environment()

    def test_as_alone(self, env):
        assert eval_string("(loop as x in '(1 2 3) sum x)", env) == 6

    def test_as_repeated(self, env):
        result = eval_string(
            "(loop as x in '(a b c) as y in '(1 2 3) collect (list x y))", env)
        assert str(result) == "((A 1) (B 2) (C 3))"

    def test_as_mixed_with_for(self, env):
        result = eval_string(
            "(loop as x in '(a b c) for y in '(1 2 3) collect (list x y))", env)
        assert str(result) == "((A 1) (B 2) (C 3))"


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
