"""
Tests for Phase 4, Task 5: Restart System
Tests RESTART-CASE, RESTART-BIND, INVOKE-RESTART, and ABORT special forms.

Rewritten alongside the real restart-mechanism implementation (plan.md C9):
the previous version of this file pinned the *old* stub shapes --
`Restart(name, handler, report=...)` with a `.handler`/`.report` attribute,
and a `RestartException` carrying a bare name string -- which is exactly the
architecture CLAUDE.md's control-transfer-exception note flags as broken
(`RestartException` was in none of the evaluator's control-transfer
pass-through tuples, and a plain dict kept restarts by name instead of as
real objects). Per CLAUDE.md's own rule, "when they disagree, ansi-test is
right, and the unit test is wrong" -- these tests pinned the bug, not ANSI
behavior, so they are rewritten against the real mechanism instead of kept
for compatibility. `ansi-test/conditions/restart-case.lsp` and
`restart-bind.lsp` are the end-to-end authority; these pin the individual
Python-level pieces (the `Restart` object model, stack discipline) the same
way test_handler_stack_signaling.py does for HANDLER-BIND.
"""
import pytest
import fclpy.state as state
from fclpy import lisptype
from fclpy.lisptype import Restart
from fclpy.lispfunc import setup_environment, eval_string


def sym_name(value):
    """The name of a returned Lisp symbol, for comparing against 'GOOD'/'BAD'."""
    return getattr(value, 'name', str(value)).upper()


@pytest.fixture
def env():
    state.current_environment = None
    state.functions_loaded = False
    state.restart_stack = []
    return setup_environment()


class TestRestartObjectModel:
    """The `Restart` object model itself (lisptype_extended.Restart)."""

    def test_restart_class_creation(self):
        def handler(*args):
            return args

        restart = Restart('MY-RESTART', handler)
        assert isinstance(restart.name, lisptype.LispSymbol)
        assert restart.name.name == 'MY-RESTART'
        assert restart.function is handler

    def test_anonymous_restart_has_no_name_match(self):
        restart = Restart(lisptype.NIL, lambda: None)
        assert restart.name is lisptype.NIL
        assert restart.name_matches(lisptype.LispSymbol('FOO')) is False

    def test_restart_name_matching_is_by_name_not_identity(self):
        restart = Restart('FOO', lambda: None)
        assert restart.name_matches(lisptype.LispSymbol('FOO')) is True
        assert restart.name_matches(lisptype.LispSymbol('BAR')) is False

    def test_applies_to_with_no_condition_ignores_association(self):
        restart = Restart('FOO', lambda: None)
        restart.associated_conditions.append(object())
        assert restart.applies_to(None) is True

    def test_applies_to_filters_by_association_when_condition_given(self):
        restart = Restart('FOO', lambda: None)
        marker = object()
        restart.associated_conditions.append(marker)
        assert restart.applies_to(marker) is True
        assert restart.applies_to(object()) is False


class TestRestartCaseEndToEnd:
    """Smoke tests for the special forms, via real Lisp source -- the
    detailed semantics are ansi-test's job (conditions/restart-case.lsp,
    restart-bind.lsp); these just pin that the mechanism is wired up."""

    def test_restart_case_evaluates_protected_form_when_untouched(self, env):
        assert eval_string("(restart-case 42 (foo () 99))", env) == 42

    def test_restart_case_runs_clause_on_invoke(self, env):
        result = eval_string(
            "(restart-case (progn (invoke-restart 'foo) 'bad) (foo () 'good))", env)
        assert sym_name(result) == 'GOOD'

    def test_restart_bind_evaluates_body_when_untouched(self, env):
        assert eval_string("(restart-bind () 42)", env) == 42

    def test_restart_bind_invokes_function_directly_without_unwinding(self, env):
        result = eval_string(
            "(block done"
            "  (restart-bind ((foo #'(lambda () (return-from done 'good))))"
            "    (invoke-restart 'foo) 'bad))", env)
        assert sym_name(result) == 'GOOD'

    def test_invoke_restart_signals_control_error_for_unknown_name(self, env):
        with pytest.raises(Exception):
            eval_string("(invoke-restart 'nonexistent)", env)

    def test_abort_signals_control_error_with_no_restart(self, env):
        with pytest.raises(Exception):
            eval_string("(abort)", env)

    def test_restart_stack_is_clean_after_restart_case(self, env):
        state.restart_stack = []
        eval_string("(restart-case 42 (foo () 99))", env)
        assert len(state.restart_stack) == 0

    def test_restart_stack_is_clean_after_transfer(self, env):
        state.restart_stack = []
        eval_string(
            "(restart-case (progn (invoke-restart 'foo) 'bad) (foo () 'good))", env)
        assert len(state.restart_stack) == 0

    def test_nested_restart_case_same_name_resolves_innermost_first(self, env):
        # CLHS 9.2's own worked example: the inner clause's body re-invokes
        # the same restart name, which must resolve to the *outer* restart
        # (the inner one is disestablished before its own clause body runs).
        result = eval_string(
            "(restart-case"
            "  (restart-case (invoke-restart 'foo 1)"
            "    (foo (x) (invoke-restart 'foo (1+ x))))"
            "  (foo (y) (+ 4 y)))", env)
        assert result == 6


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
