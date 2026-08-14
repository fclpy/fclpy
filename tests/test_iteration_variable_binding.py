"""An iteration form must bind its variable, not assign to an outer one.

**These tests were `xfail` and are now the regression net for the fix.** Every
one of DO, DO*, DOLIST, DOTIMES, LOOP, DO-SYMBOLS, DO-EXTERNAL-SYMBOLS and
DO-ALL-SYMBOLS used to establish its variable with `Environment.set_variable`,
which walks the environment chain and mutates the first binding of that name it
finds. Because `Environment.__init__` gives a child environment its parent's
`variable_bindings` list, that walk always reached an enclosing binding, so

    (let ((x 99)) (dolist (x '(1 2 3))) x)  =>  NIL, not 99

That was not only a wrong value. `(do-all-symbols (s) ...)` overwrote any
enclosing `s`, and rt.lsp's own failure reporter takes its output stream in a
parameter named `s` -- so `printer/print-symbols.lsp`'s PRINT.SYMBOL.RANDOM.3,
which iterates every symbol with `do-all-symbols`, left RT printing its next
failure report *to a symbol*, aborting a `run_ansi.py printer` run outright. It
is how this defect was found. `printer/print-strings.lsp` aborted the same run
one file earlier: its line 149 is `for s = (coerce ...)`, so LOOP's driver
clobbered RT's `s` too.

**Why the one-word fix was wrong, and what replaced it.** Switching the
establishing call to `add_variable` fixes all eight leaks, and was measured at
`iteration` 410 -> 408: it gained DOLIST.14 and DOTIMES.16 but lost DO.14,
DO*.14, DOTIMES.18 and DOTIMES.18A, all four of which declare the iteration
variable special inside the loop body. A variable declared special must be bound
*dynamically* -- in the symbol's value cell, where a closure's free reference
finds it -- not lexically. That rule lived in `eval_let` and, copy-pasted and
subtly wrong, in `eval_letstar`; the iteration forms did not have it.

So all ten binding forms now share one binder, `fclpy.lispfunc.binding`'s
`BindingFrame` -- M2's environment-model slice. `TestSpecialVersusLexical` below
pins the distinction the shared binder has to get right, and it is a real
distinction rather than a quirk: DOTIMES.17 and DOTIMES.18 differ only in
whether the loop body declares the variable special, and they expect different
answers.
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


class TestTheVariableIsBoundNotAssigned:
    """An enclosing variable of the same name must survive the loop."""

    @pytest.mark.parametrize('form', [
        "(dolist (x (list 1 2 3)))",
        "(dotimes (x 3))",
        "(do ((x 0 (1+ x))) ((> x 2)))",
        "(do* ((x 0 (1+ x))) ((> x 2)))",
        '(do-symbols (x (find-package "KEYWORD")))',
        '(do-external-symbols (x (find-package "KEYWORD")))',
        "(do-all-symbols (x))",
        # LOOP's drivers leaked the same way -- `eval_loop` also established its
        # variable with `set_variable`. This is what made
        # `printer/print-strings.lsp` abort a run: its line 149 is
        # `for s = (coerce ...)`, so RT's report stream `s` became a string.
        "(loop for x in (list 1 2 3))",
        "(loop for x = 5 repeat 2)",
        "(loop for x below 3)",
        "(loop for x on (list 1 2 3))",
        "(loop for x across (vector 1 2 3))",
        "(loop for x from 1 to 3)",
        "(loop repeat 2 collect 1 into x)",
    ])
    def test_enclosing_binding_is_untouched(self, form):
        assert evs(f"(let ((x 99)) {form} x)") == '99'

    def test_an_inner_loop_does_not_clobber_an_outer_one(self):
        """The shadowing has to work between two loops, not just LET and a loop."""
        assert evs("(let ((x 1)) (dolist (y (list 1 2)) "
                   "(dolist (x (list 8 9)))) x)") == '1'

    def test_do_all_symbols_does_not_clobber_a_variable_named_s(self):
        """The exact shape that aborted a `run_ansi.py printer` run.

        rt.lsp's failure reporter is `(defun do-entry (entry &optional (s
        *standard-output*)) ... (format s ...))`, so a leaked `s` from
        `do-all-symbols` became RT's output stream and RT tried to print to a
        symbol.
        """
        assert ev('(let ((s (make-string-output-stream))) '
                  '(do-all-symbols (s)) '
                  '(princ "ok" s) (get-output-stream-string s))') == 'ok'

    def test_writing_to_a_clobbered_stream_variable_is_the_observed_failure(self):
        """Pins the *symptom* the printer reported, so the two stay connected."""
        assert ev('(let ((s (make-string-output-stream))) '
                  '(do-all-symbols (s)) '
                  '(format s "x") (get-output-stream-string s))') == 'x'

    def test_a_loop_driver_does_not_clobber_a_stream_parameter(self):
        """`printer/print-strings.lsp:149`'s shape, the other half of the abort."""
        assert ev('(let ((s (make-string-output-stream))) '
                  '(loop for s = 1 repeat 2) '
                  '(princ "ok" s) (get-output-stream-string s))') == 'ok'


class TestSpecialVersusLexical:
    """The decision the shared binder exists to make in one place.

    A variable *declared* special by the binding form is bound dynamically, in
    the symbol's value cell, so a closure's free reference sees each iteration.
    A variable that is not is bound lexically, and an enclosing dynamic binding
    of the same name stays visible to such a closure. DOTIMES.17 vs DOTIMES.18
    is exactly this pair.
    """

    def test_no_declaration_means_the_loop_binds_lexically(self):
        """DOTIMES.17: `%f` keeps seeing the enclosing *special* i."""
        assert evs("(let ((i 0) (y nil)) (declare (special i)) "
                   "(flet ((%f () i)) (dotimes (i 4) (push (%f) y))) y)") == '(0 0 0 0)'

    def test_a_declaration_means_the_loop_binds_dynamically(self):
        """DOTIMES.18: the loop's own declaration rebinds the value cell."""
        assert evs("(let ((i 0) (y nil)) (declare (special i)) "
                   "(flet ((%f () i)) (dotimes (i 4) (declare (special i)) "
                   "(push (%f) y))) y)") == '(3 2 1 0)'

    def test_do_binds_dynamically_for_a_free_reference_elsewhere(self):
        """DO.14: `%f` reaches the DO's `i` through a LOCALLY special declaration."""
        assert evs("(let ((x 0)) (flet ((%f () (locally (declare (special i)) "
                   "(incf x i)))) (do ((i 0 (1+ i))) ((>= i 10) x) "
                   "(declare (special i)) (%f))))") == '45'

    def test_do_star_binds_dynamically_too(self):
        """DO*.14, the sequential-binding twin of the above."""
        assert evs("(let ((x 0)) (flet ((%f () (locally (declare (special i)) "
                   "(incf x i)))) (do* ((i 0 (1+ i))) ((>= i 10) x) "
                   "(declare (special i)) (%f))))") == '45'

    def test_a_free_declaration_redirects_a_result_form(self):
        """DOLIST.17: the body's free `special x` governs the result form too."""
        assert evs("(let ((x :good)) (declare (special x)) "
                   "(let ((x :bad)) (dolist (e nil x) (declare (special x)))))") == ':GOOD'

    @pytest.mark.parametrize('form', [
        # DOLIST.16
        "(dolist (e (return-from done x)) (declare (special x)))",
        # DO.16 -- init forms evaluated in the enclosing environment
        "(do ((i (return-from done x) 0)) (t nil) (declare (special x)))",
        # DO*.16 -- init forms evaluated in the *loop's* environment, which is
        # why installing the free declaration is deferred until after them.
        "(do* ((i (return-from done x) 0)) (t nil) (declare (special x)))",
    ])
    def test_an_init_form_is_outside_the_bodys_declarations(self, form):
        """Init forms are not in the scope of the body's declarations, so a free
        `special` declaration must not redirect them."""
        assert evs(f"(block done (let ((x :bad)) (declare (special x)) "
                   f"(let ((x :good)) {form})))") == ':GOOD'

    @pytest.mark.parametrize('form', [
        # DO.17 / DO*.17 -- step forms *are* in that scope.
        "(do ((i 0 (return-from done x))) (nil nil) (declare (special x)))",
        "(do* ((i 0 (return-from done x))) (nil nil) (declare (special x)))",
    ])
    def test_a_step_form_is_inside_the_bodys_declarations(self, form):
        assert evs(f"(block done (let ((x :good)) (declare (special x)) "
                   f"(let ((x :bad)) {form})))") == ':GOOD'

    def test_a_dynamic_binding_is_undone_when_the_loop_exits(self):
        """The frame unwinds, so the enclosing dynamic value comes back."""
        assert evs("(let ((i 7)) (declare (special i)) "
                   "(dotimes (i 3) (declare (special i))) i)") == '7'

    def test_a_dynamic_binding_is_undone_on_a_non_local_exit(self):
        """Unwinding has to happen on the exceptional path as well."""
        assert evs("(let ((i 7)) (declare (special i)) "
                   "(block out (dotimes (i 3) (declare (special i)) "
                   "(return-from out nil))) i)") == '7'


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
        # DOLIST.14: the result form assigns the loop's own binding, not the
        # enclosing one -- the two halves of this fix in a single expression.
        ("(let ((e 1)) (dolist (e (list 1 2 3) (setf e 2))) e)", '1'),
        # A SETQ in the body must reach the loop's binding as well.
        ("(let ((x 9)) (dolist (x (list 1 2)) (setq x 5)) x)", '9'),
        # LOOP's INTO destination is local to the loop (CLHS 6.1.3).
        ("(loop for i below 3 collect i into acc finally (return acc))", '(0 1 2)'),
    ])
    def test_result(self, form, expected):
        assert evs(form) == expected

    def test_the_body_sees_the_loop_variable(self):
        """Shadowing must not hide the loop's own value from the body."""
        assert evs("(let ((x 99)) (let ((acc nil)) "
                   "(dolist (x (list 1 2)) (push x acc)) acc))") == '(2 1)'

    def test_successive_iterations_share_one_binding(self):
        """DO.15: the variable is the same binding each time round, so closures
        made in different iterations all see the final value."""
        assert evs("(let ((fns nil)) (do ((i 0 (1+ i))) ((= i 3)) "
                   "(push (lambda () i) fns)) "
                   "(mapcar #'funcall fns))") == '(3 3 3)'


class TestLetStar:
    """LET*'s copy of the decision was not merely duplicated, it was wrong.

    For a special variable it called `global_env.add_variable`, which puts a
    *lexical* binding in the global environment and never removes it: the
    binding outlived the LET* and was invisible to SYMBOL-VALUE. It now takes
    the same dynamic path LET does, through the shared frame.
    """

    def test_a_special_binding_is_dynamic_and_scoped(self):
        assert evs("(progn (defvar *bv* 1) (let* ((*bv* 2)) "
                   "(symbol-value '*bv*)))") == '2'
        # ...and is gone again afterwards, rather than left in the global
        # environment as a lexical binding of 2.
        assert evs("(progn (defvar *bv* 1) (let* ((*bv* 2)) *bv*) "
                   "(boundp '*bv*))") == 'NIL'

    def test_a_special_binding_is_undone_on_a_non_local_exit(self):
        assert evs("(progn (defvar *bv3* 1) "
                   "(block out (let* ((*bv3* 2)) (return-from out nil))) "
                   "(boundp '*bv3*))") == 'NIL'

    def test_a_bare_symbol_binds_to_nil(self):
        """CLHS 3.1.2.1.1. LET* skipped bare symbols, leaving them unbound."""
        assert evs("(let* (x) x)") == 'NIL'
        assert evs("(let* ((a 1) b (c 3)) (list a b c))") == '(1 NIL 3)'


class TestThePackageMirror:
    """`*PACKAGE*`'s value is mirrored in `state.current_package`, and the
    restore used to be guarded by `if old_package is not None`.

    That conflates "nothing was saved" with "None *is* the saved value" -- and
    None is the mirror's normal state until something binds `*PACKAGE*`, since
    a plain reference falls back to a default. So the *first*
    `(let ((*package* p)) ...)` in a session never restored, leaving the current
    package set to `p` for everything after it and silently interning every
    later symbol into the wrong package. Found by a smoke test whose every
    subsequent form came back with keywords where it had written symbols.
    """

    @pytest.mark.parametrize('form', [
        '(let ((*package* (find-package "KEYWORD"))) (package-name *package*))',
        '(let* ((*package* (find-package "KEYWORD"))) (package-name *package*))',
    ])
    def test_the_binding_takes_effect_and_is_undone(self, form):
        assert evs(form) == 'KEYWORD'
        assert evs('(package-name *package*)') == 'COMMON-LISP-USER'

    def test_the_mirror_is_restored_on_a_non_local_exit(self):
        evs('(block out (let ((*package* (find-package "KEYWORD"))) '
            '(return-from out nil)))')
        assert evs('(package-name *package*)') == 'COMMON-LISP-USER'

    def test_a_symbol_read_after_the_binding_goes_to_the_right_package(self):
        """The symptom, not just the variable: interning must recover too."""
        evs('(let ((*package* (find-package "KEYWORD"))) 1)')
        assert evs("(symbol-package 'some-fresh-symbol)") != evs('(find-package "KEYWORD")')


class TestTheGlobalValueCellDefect:
    """A special variable has two homes and they never reconcile. **Not this
    change's defect** -- it is `eval_defvar`'s, and it is why the assertions
    above have to go through BOUNDP rather than a plain reference.

    `DEFVAR`/`DEFPARAMETER` call `global_env.add_variable`, which creates a
    *lexical* binding in the global environment, and never write the symbol's
    value cell. `SETQ` then maintains that lexical binding. But `SYMBOL-VALUE`,
    `BOUNDP`, `SET`, `PROGV` and every dynamic binding -- including the one the
    shared `BindingFrame` establishes -- use the value cell, and
    `eval`'s symbol path checks the lexical chain *first*. So a plain reference
    to a defvar'd variable reads the global lexical binding and cannot see the
    dynamic binding that shadows it.

    Diagnosed while consolidating the binder; recorded in plan.md section 5 as
    M2's, and left for its own measured change because the fix has to move
    `SETQ` and the `eval` lookup order with it.
    """

    _TWO_HOMES = pytest.mark.xfail(strict=True, reason=(
        "DEFVAR creates a lexical binding in the global environment instead of "
        "writing the symbol's value cell, and eval checks the lexical chain "
        "first -- so a plain reference cannot see a dynamic binding. M2; see "
        "this class's docstring and plan.md section 5."))

    @_TWO_HOMES
    def test_defvar_makes_the_variable_boundp(self):
        assert evs("(progn (defvar *th1* 1) (boundp '*th1*))") == 'T'

    @_TWO_HOMES
    def test_defvar_is_visible_to_symbol_value(self):
        assert evs("(progn (defvar *th2* 1) (symbol-value '*th2*))") == '1'

    @_TWO_HOMES
    def test_a_plain_reference_sees_a_dynamic_binding(self):
        assert evs("(progn (defvar *th3* 1) (let ((*th3* 2)) *th3*))") == '2'

    @_TWO_HOMES
    def test_setq_of_a_global_special_reaches_the_value_cell(self):
        assert evs("(progn (defvar *th4* 1) (setq *th4* 3) "
                   "(symbol-value '*th4*))") == '3'
