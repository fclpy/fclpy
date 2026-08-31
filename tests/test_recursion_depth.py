"""Lisp recursion depth against the *default* CPython frame limit.

Why these run at the default limit
----------------------------------
`scripts/run_ansi.py` raises the recursion limit to 60000 (`run_with_deep_stack`);
`run_all_tests.py` -- the scoreboard -- does not, and per recursion-plan.md's
"constraint this plan works under" the fix is to reduce frames consumed, never
to raise the limit. pytest runs at the default 1000, so these tests measure the
configuration that actually gates a full suite run.

What each shape corresponds to in ansi-test
-------------------------------------------
The suite's own helpers (which are test code and cannot be edited) are what
exhaust the stack, so the shapes here are copied from them:

- `check-cons-copy` (auxiliary/cons-aux.lsp:56) recurses on car **and** cdr;
  the cdr call is the last operand of its AND, so it is a *tail* call and its
  depth is the **list length** -- 700 for COPY-TREE.2, which walks `*universe*`.
- `make-scaffold-copy` (auxiliary/cons-aux.lsp:20) recurses in **argument**
  position (`:car (make-scaffold-copy (car x))`), so no tail-call transform can
  flatten it. Depth is again the list length: 100 for most of the
  `DO-RANDOM-*` tests, **334** for NINTERSECTION.10/.11.
"""

import pytest

from fclpy import lispenv
from fclpy.lispfunc import eval_string


@pytest.fixture(scope='module', autouse=True)
def _env():
    lispenv.setup_standard_environment()


def _ev(source):
    return eval_string(source)


class TestTailRecursionIsALoop:
    """Self tail calls must cost O(1) host stack (recursion-plan.md Step 4)."""

    def test_self_tail_call_is_not_depth_limited(self):
        """A tail-recursive countdown must not be bounded by the frame limit.

        Before Step 4 this capped at ~186 levels; the trampoline in
        `make_ordinary_function.call` makes it a loop.
        """
        _ev('(defun %tail-countdown (n) (if (zerop n) :done '
            '(%tail-countdown (1- n))))')
        assert str(_ev('(%tail-countdown 10000)')) == ':DONE'

    def test_tail_call_through_cond_and_and(self):
        """The ansi-test shape: the self call is the last operand of an AND
        inside the selected COND clause -- tail position through two forms."""
        _ev('(defun %walk (x) (cond ((consp x) (and t (%walk (cdr x)))) '
            '(t :end)))')
        assert str(_ev('(%walk (make-list 5000 :initial-element 1))')) == ':END'

    def test_check_cons_copy_shape_at_universe_length(self):
        """COPY-TREE.2's actual workload: `check-cons-copy` over a 700-element
        list. This is the exact case that aborted the 2026-08-31 full run."""
        _ev('''(progn
                 (defun %notnot (x) (not (not x)))
                 (defun %eqt (x y) (%notnot (eq x y)))
                 (defun %ccc (x y)
                   (cond ((consp x)
                          (and (consp y)
                               (not (%eqt x y))
                               (%ccc (car x) (car y))
                               (%ccc (cdr x) (cdr y))))
                         ((%eqt x y) t)
                         (t nil)))
                 t)''')
        assert str(_ev('(let* ((x (make-list 700 :initial-element 1)) '
                       '       (y (copy-tree x))) (%ccc x y))')) == 'T'


class TestNonTailRecursionDepth:
    """Recursion in *argument* position still consumes host frames.

    No tail-call transform applies to these; only reducing frames per Lisp
    level does (recursion-plan.md 2A).
    """

    def _define_scaffold_shape(self):
        # Mirrors make-scaffold-copy: both recursive calls are arguments.
        _ev('(defun %mk (x) (if (consp x) '
            '(list (%mk (car x)) (%mk (cdr x))) x))')

    def test_shallow_non_tail_recursion_still_works(self):
        """Guards against a regression in the non-tail case: Step 4 must not
        buy tail depth at the price of this. (Measured 149 levels before and
        after; an intermediate `_call_once` helper had dropped it to 124.)"""
        self._define_scaffold_shape()
        result = _ev('(%mk (make-list 100 :initial-element 1))')
        assert result is not None

    def test_non_tail_recursion_at_334_levels(self):
        """NINTERSECTION.10/.11's depth, the deepest the suite demands.

        Passes since recursion-plan.md Step 6 put argument evaluation on an
        explicit continuation stack: 5 host frames per Lisp level -> 2, and the
        max non-tail depth 149 -> 372 at the default limit.
        """
        self._define_scaffold_shape()
        result = _ev('(%mk (make-list 334 :initial-element 1))')
        assert result is not None
