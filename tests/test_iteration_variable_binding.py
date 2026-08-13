"""An iteration form must bind its variable, not assign to an outer one.

**These tests document a known, diagnosed, unfixed defect.** The binding ones
are `xfail`; the ones asserting the loops still compute correctly pass today and
are here so that whoever fixes the binding can see immediately if they broke the
loop itself.

Every one of DO, DO*, DOLIST, DOTIMES, LOOP, DO-SYMBOLS, DO-EXTERNAL-SYMBOLS
and DO-ALL-SYMBOLS establishes its variable with `Environment.set_variable`, which
walks the environment chain and mutates the first binding of that name it finds.
Because `Environment.__init__` gives a child environment its parent's
`variable_bindings` list, that walk always reaches an enclosing binding, so

    (let ((x 99)) (dolist (x '(1 2 3))) x)  =>  NIL, not 99

This is not only a wrong value. `(do-all-symbols (s) ...)` overwrites any
enclosing `s`, and rt.lsp's own failure reporter takes its output stream in a
parameter named `s` -- so `printer/print-symbols.lsp`'s PRINT.SYMBOL.RANDOM.3,
which iterates every symbol with `do-all-symbols`, leaves RT printing its next
failure report *to a symbol*. That aborts a `run_ansi.py printer` run outright.
It is how this defect was found. `printer/print-strings.lsp` aborts the same
run for the same reason one file earlier: its line 149 is `for s = (coerce ...)`,
so LOOP's driver clobbers RT's `s` too. **Between them, a whole-directory
`run_ansi.py printer` run cannot complete until this is fixed.**

**Why it is not simply fixed here.** Switching the establishing call to
`add_variable` (which prepends a binding to the loop's own environment, so the
per-iteration `set_variable` stops reaching the enclosing one) fixes all eight
leaks and was measured: `run_ansi.py iteration` went 410 -> 408, gaining
DOLIST.14 and DOTIMES.16 but losing DO.14, DO*.14, DOTIMES.18 and DOTIMES.18A.
Those four all declare the iteration variable special inside the loop body:

    (let ((i 0) (y nil))
      (declare (special i))
      (flet ((%f () i))
        (dotimes (i 4) (declare (special i)) (push (%f) y)))
      y)                                       ; => (3 2 1 0)

A variable declared special must be bound *dynamically* -- in the symbol's value
cell, where the `flet` closure's free reference finds it -- not lexically. That
rule lives in `eval_let` and, copy-pasted, in `eval_letstar`; the iteration forms
do not have it. So the correct fix is to extract that one decision into a shared
binder and use it from LET, LET*, and all eight iteration forms, which is M2's
environment model. plan.md's note on M2 warns specifically against fixing
specials one binding form at a time, so a third copy of that logic is the wrong
move and the change was reverted rather than shipped with a per-file regression.
"""

import io

import pytest

from fclpy import lispenv
from fclpy.lispfunc.evaluation_core import eval as lisp_eval
from fclpy.lispreader import LispReader, LispStream
from fclpy.readtable import get_current_readtable


@pytest.fixture(autouse=True)
def env():
    lispenv.setup_standard_environment()


def ev(source):
    import fclpy.state as state
    stream = LispStream(io.StringIO(source))
    readtable = get_current_readtable()
    form = LispReader(readtable.get_macro_character, stream).read_1()
    return lisp_eval(form, state.current_environment)


def evs(source):
    return str(ev(source))


_KNOWN_LEAK = pytest.mark.xfail(strict=True, reason=(
    "Iteration forms assign to an enclosing binding of the same name instead of "
    "establishing their own. Fixing it requires the special-vs-lexical binding "
    "decision that currently lives only in eval_let/eval_letstar -- M2. See this "
    "module's docstring for the measurement and the four tests that constrain it."))


class TestTheVariableIsBoundNotAssigned:
    """An enclosing variable of the same name must survive the loop."""

    @_KNOWN_LEAK
    @pytest.mark.parametrize('form', [
        "(dolist (x (list 1 2 3)))",
        "(dotimes (x 3))",
        "(do ((x 0 (1+ x))) ((> x 2)))",
        "(do* ((x 0 (1+ x))) ((> x 2)))",
        '(do-symbols (x (find-package "KEYWORD")))',
        '(do-external-symbols (x (find-package "KEYWORD")))',
        "(do-all-symbols (x))",
        # LOOP's drivers leak the same way -- `eval_loop` also establishes its
        # variable with `set_variable`. This is what makes
        # `printer/print-strings.lsp` abort a run: its line 149 is
        # `for s = (coerce ...)`, so RT's report stream `s` becomes a string.
        "(loop for x in (list 1 2 3))",
        "(loop for x = 5 repeat 2)",
        "(loop for x below 3)",
    ])
    def test_enclosing_binding_is_untouched(self, form):
        assert evs(f"(let ((x 99)) {form} x)") == '99'

    @_KNOWN_LEAK
    def test_an_inner_loop_does_not_clobber_an_outer_one(self):
        """The shadowing has to work between two loops, not just LET and a loop."""
        assert evs("(let ((x 1)) (dolist (y (list 1 2)) "
                   "(dolist (x (list 8 9)))) x)") == '1'

    @_KNOWN_LEAK
    def test_do_all_symbols_does_not_clobber_a_variable_named_s(self):
        """The exact shape that aborts a `run_ansi.py printer` run.

        rt.lsp's failure reporter is `(defun do-entry (entry &optional (s
        *standard-output*)) ... (format s ...))`, so a leaked `s` from
        `do-all-symbols` becomes RT's output stream and RT tries to print to a
        symbol.
        """
        assert ev('(let ((s (make-string-output-stream))) '
                  '(do-all-symbols (s)) '
                  '(princ "ok" s) (get-output-stream-string s))') == 'ok'

    @pytest.mark.xfail(strict=True, reason=(
        "The same leak, seen from the printer's side: once `do-all-symbols` has "
        "clobbered `s`, writing to it is a type error rather than output."))
    def test_writing_to_a_clobbered_stream_variable_is_the_observed_failure(self):
        """Pins the *symptom* the printer reports, so the two stay connected."""
        assert ev('(let ((s (make-string-output-stream))) '
                  '(do-all-symbols (s)) '
                  '(format s "x") (get-output-stream-string s))') == 'x'


class TestTheLoopStillWorks:
    """The binding fix must not change what the forms compute."""

    @pytest.mark.parametrize('form,expected', [
        ("(let ((acc nil)) (dolist (x (list 1 2 3)) (push x acc)) acc)", '(3 2 1)'),
        ("(dolist (x (list 1 2) :done) x)", ':DONE'),
        ("(let ((n 0)) (dotimes (i 4) (incf n i)) n)", '6'),
        ("(dotimes (i 3 i))", '3'),
        ("(do ((i 0 (1+ i)) (acc nil)) ((= i 3) acc) (push i acc))", '(2 1 0)'),
        # DO* steps sequentially, so j sees the *new* i.
        ("(do* ((i 0 (1+ i)) (j (* i 10) (* i 10))) ((= i 3) j))", '30'),
        # DO binds in parallel, so j's init sees the outer i, not the new one.
        ("(let ((i 7)) (do ((i 0 (1+ i)) (j i)) ((= i 2) j)))", '7'),
    ])
    def test_result(self, form, expected):
        assert evs(form) == expected

    def test_the_body_sees_the_loop_variable(self):
        """Shadowing must not hide the loop's own value from the body."""
        assert evs("(let ((x 99)) (let ((acc nil)) "
                   "(dolist (x (list 1 2)) (push x acc)) acc))") == '(2 1)'
