"""Tests for LoopWatchdog -- the slow-loop reporter used by LOOP/DO/DO*.

Context: the warning used to be the whole mechanism. It fires once and nothing
is printed again, so "slow but finished", "still spinning", and "aborted at the
hard cap" were indistinguishable in run_all_tests.err -- the hard cap's
LispError surfaces in the .log as an ordinary test failure, never on stderr.
A warning you cannot resolve is not a usable signal for "is this run stuck?",
which is the only question it exists to answer.

These tests drive the watchdog directly with a lowered threshold rather than
waiting out the real 120s/600s ones.
"""

import pytest
import fclpy.lisptype as lisptype
import fclpy.lispfunc.evaluation_loops_conditionals as loops
from fclpy.lispfunc.evaluation_loops_conditionals import LoopWatchdog


# A threshold small enough that any real elapsed time exceeds it, but still
# positive: a value <= 0 *disables* the check, which is the documented meaning of
# LOOP_TIMEOUT_WARNING = 0.
TINY = 1e-9


@pytest.fixture
def warn_immediately(monkeypatch):
    """Make any tick past the first warn, so tests need no real elapsed time."""
    monkeypatch.setattr(loops, 'LOOP_TIMEOUT_WARNING', TINY)


def make(kind='LOOP', hard_cap=0):
    return LoopWatchdog(kind, lambda: ['detail: x'], hard_cap=hard_cap)


class TestQuietWhenFast:
    def test_a_loop_that_never_warns_prints_nothing(self, capsys, monkeypatch):
        monkeypatch.setattr(loops, 'LOOP_TIMEOUT_WARNING', 3600)
        watchdog = make()
        with watchdog:
            for _ in range(50):
                watchdog.tick()
        assert capsys.readouterr().err == ''
        assert watchdog.warned is False


class TestResolutionIsAlwaysReported:
    """The counterpart that was missing: a warned loop must always say how it
    ended, on every exit path."""

    def test_normal_completion_reports_resolved(self, capsys, warn_immediately):
        watchdog = make()
        with watchdog:
            watchdog.tick()
        err = capsys.readouterr().err
        assert 'LOOP WARNING' in err
        assert 'LOOP RESOLVED' in err
        assert 'detail: x' in err

    def test_non_local_exit_reports_how_it_exited(self, capsys, warn_immediately):
        """Most Lisp loops end via RETURN-FROM/THROW/GO, i.e. an exception
        through the watchdog's scope -- that must still produce an outcome line
        rather than looking like a loop that is still running."""
        from fclpy.lispfunc.evaluation_core import ThrowException

        watchdog = make()
        with pytest.raises(ThrowException):
            with watchdog:
                watchdog.tick()
                raise ThrowException(lisptype.LispSymbol('DONE'), lisptype.NIL)
        err = capsys.readouterr().err
        assert 'LOOP WARNING' in err
        assert 'EXITED via ThrowException' in err

    def test_iteration_count_and_elapsed_are_reported(self, capsys, warn_immediately):
        watchdog = make()
        with watchdog:
            for _ in range(7):
                watchdog.tick()
        assert '(7 iterations)' in capsys.readouterr().err


class TestHardCap:
    def test_hard_cap_announces_on_stderr_before_raising(self, capsys, warn_immediately):
        """The LispError alone lands in the .log as a test failure, so .err
        would otherwise show a warning with no outcome."""
        watchdog = make(hard_cap=TINY)
        with pytest.raises(lisptype.LispError) as exc_info:
            with watchdog:
                watchdog.tick()
        err = capsys.readouterr().err
        assert 'LOOP ABORTED' in err
        assert 'exceeded' in str(exc_info.value)

    def test_no_hard_cap_means_no_abort(self, capsys, warn_immediately):
        """DO/DO* pass no hard cap -- their own drivers bound them -- so ticking
        must never raise there."""
        watchdog = make('DO', hard_cap=0)
        with watchdog:
            for _ in range(20):
                watchdog.tick()
        assert 'ABORTED' not in capsys.readouterr().err


class TestTimestamps:
    def test_every_line_carries_a_wall_clock_stamp(self, capsys, warn_immediately):
        """A bare elapsed-seconds figure cannot be placed against the rest of a
        20-minute suite run."""
        import re

        watchdog = make()
        with watchdog:
            watchdog.tick()
        stamped = [line for line in capsys.readouterr().err.splitlines()
                   if 'LOOP WARNING' in line or 'LOOP RESOLVED' in line]
        assert len(stamped) == 2
        for line in stamped:
            assert re.search(r'\[\d{2}:\d{2}:\d{2}\]', line), line


class TestDescribeIsLazy:
    def test_describe_is_not_called_when_no_warning_fires(self, monkeypatch):
        """Diagnostic detail can be expensive to render (whole form bodies), so
        it must only be built if a warning actually fires."""
        monkeypatch.setattr(loops, 'LOOP_TIMEOUT_WARNING', 3600)
        calls = []

        watchdog = LoopWatchdog('LOOP', lambda: calls.append(1) or ['x'])
        with watchdog:
            for _ in range(30):
                watchdog.tick()
        assert calls == []
