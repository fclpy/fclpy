"""Method combination is one mechanism, and the effective method is a form.

`GenericFunction` used to have no method combination at all: `DEFGENERIC`
dropped its `:method-combination` option silently, and `call_generic_function`
hard-coded standard combination's four qualifier buckets -- so a method
qualified `progn` (CLHS 7.6.6.4's built-in combinations) landed in none of them
and was discarded with no diagnostic. `DEFINE-METHOD-COMBINATION` existed twice,
neither copy defining anything.

Two properties are pinned here rather than left to `run_ansi.py objects`:

1. **The three operators are special operators.** `CALL-METHOD` and
   `MAKE-METHOD` were registered as `cl_function`s, which evaluates their
   operands -- and their operands are a method object and an unevaluated body
   form. That registration is what made `next-method-list` unreachable.

2. **The effective method is built as a form, not folded in Python.** The AND
   and OR combinations must short-circuit, so `(and (call-method m1)
   (call-method m2))` has to be a real form the evaluator walks. Collecting
   every method's value and reducing them in Python gives the same answer for
   PROGN and LIST and the *wrong* one here, which is easy to reintroduce as a
   simplification.
"""

import pytest

from fclpy import classes, lispenv
from fclpy.lispfunc import eval_string, registry


@pytest.fixture(autouse=True)
def env():
    lispenv.setup_standard_environment()
    import fclpy.state as state
    return state.current_environment


def ev(source):
    """Evaluate every top-level form in `source`, answering the last value
    as the *Lisp printer* renders it.

    Python's `str()` is not the printer -- it renders the list `(A)` as
    `(A . NIL)` -- so asserting on it would be asserting on a representation
    no Lisp program can observe."""
    import fclpy.state as state
    from fclpy.lispfunc.io_write import prin1_to_string
    return str(prin1_to_string(eval_string(source, state.current_environment)))


class TestOperatorsAreSpecialForms:
    @pytest.mark.parametrize('name', ['CALL-METHOD', 'MAKE-METHOD'])
    def test_registered_once_as_a_special_operator(self, name):
        # Importing the modules that own them; the registries are populated at
        # import time and this file may be the first to ask.
        import fclpy.lispfunc.misc_clos  # noqa: F401
        import fclpy.lispfunc.evaluation_special_registrations  # noqa: F401

        assert name in registry.special_registry, (
            f"{name} is not registered as a special operator")
        assert name not in registry.function_registry, (
            f"{name} is *also* registered as a function. A function's operands "
            f"are evaluated before it runs, which is wrong for both: a "
            f"method object and an unevaluated method body respectively -- "
            f"and whichever registration the environment bootstrap installs "
            f"last silently wins (standing rule 3).")

    def test_define_method_combination_is_a_real_macro(self):
        """CLHS 7.6.6.2 heads DEFINE-METHOD-COMBINATION 'Macro', not
        'Special Operator' -- unlike CALL-METHOD/MAKE-METHOD above, its
        unevaluated-argument requirement is exactly what a macro already
        gives for free (M4: standard_macros.py's `_reuse_definer`), so it
        belongs in `function_registry` marked as a macro, not in
        `special_registry`. The vestigial `cl_special` stub
        (`evaluation_special_registrations.py`) is never reached by
        `eval()` any more -- the same harmless leftover WHEN/UNLESS/PROG
        keep beside their own macro registrations -- so its presence in
        `special_registry` is not asserted here."""
        import fclpy.lispfunc.standard_macros  # noqa: F401

        entry = registry.function_registry.get('DEFINE-METHOD-COMBINATION')
        assert entry is not None, (
            "DEFINE-METHOD-COMBINATION is not registered as a function/macro")
        assert getattr(entry.func, '__is_macro__', False), (
            "DEFINE-METHOD-COMBINATION is registered in function_registry "
            "but not marked as a macro -- its arguments (a lambda-list, "
            "method-group specs, an unevaluated body) would be evaluated "
            "as ordinary call arguments")


class TestBuiltInCombinations:
    def test_progn_runs_every_primary_and_answers_the_last(self):
        assert ev("""
            (defgeneric gf-progn (x)
              (:method-combination progn)
              (:method progn ((x integer)) 'a)
              (:method progn ((x t)) 'b))
            (gf-progn 1)
        """) == 'B'

    def test_and_short_circuits(self):
        """The property that requires an effective-method *form*."""
        assert ev("""
            (defparameter *mc-ran* nil)
            (defgeneric gf-and (x)
              (:method-combination and)
              (:method and ((x integer)) (push 'i *mc-ran*) nil)
              (:method and ((x t)) (push 't *mc-ran*) t))
            (gf-and 1)
            (reverse *mc-ran*)
        """) == '(I)'

    def test_list_is_not_identity_with_one_argument(self):
        """CLHS 7.6.6.4: LIST and APPEND are the two built-ins that apply
        their operator even to a single method."""
        assert ev("""
            (defgeneric gf-list (x)
              (:method-combination list)
              (:method list ((x t)) 'a))
            (gf-list 1)
        """) == '(A)'

    def test_most_specific_last_reverses_the_primaries(self):
        assert ev("""
            (defgeneric gf-msl (x)
              (:method-combination progn :most-specific-last)
              (:method progn ((x integer)) 'a)
              (:method progn ((x t)) 'b))
            (gf-msl 1)
        """) == 'A'

    def test_around_wraps_the_combination_and_reaches_it(self):
        assert ev("""
            (defgeneric gf-around (x)
              (:method-combination progn)
              (:method progn ((x integer)) 'a)
              (:method :around ((x integer)) (list 'wrapped (call-next-method)))
              (:method progn ((x t)) 'b))
            (gf-around 1)
        """) == '(WRAPPED B)'

    def test_an_unrecognized_qualifier_is_an_error_not_a_dropped_method(self):
        """Standing rule 4: the defect this replaced discarded the method."""
        with pytest.raises(Exception):
            ev("""
                (defgeneric gf-bogus (x)
                  (:method-combination progn)
                  (:method bogus ((x t)) 'a))
                (gf-bogus 1)
            """)


class TestDefineMethodCombination:
    def test_short_form_defines_a_usable_combination(self):
        assert ev("""
            (define-method-combination times :operator *)
            (defgeneric gf-short (x) (:method-combination times))
            (defmethod gf-short times ((x integer)) 2)
            (defmethod gf-short times ((x rational)) 3)
            (gf-short 1)
        """) == '6'

    def test_long_form_body_computes_the_effective_method(self):
        assert ev("""
            (define-method-combination mc-long () ((all *))
              `(list ,@(mapcar #'(lambda (m) `(call-method ,m)) all)))
            (defgeneric gf-long (x) (:method-combination mc-long))
            (defmethod gf-long ((x integer)) 'a)
            (defmethod gf-long ((x t)) 'b)
            (gf-long 1)
        """) == '(A B)'

    def test_no_applicable_method_is_an_error_for_any_combination(self):
        """Decided before the combination is consulted (CLHS 7.6.6) -- a
        long-form body mapping over an empty method group otherwise happily
        returns the operator applied to nothing."""
        with pytest.raises(Exception):
            ev("""
                (define-method-combination mc-long () ((all *))
                  `(list ,@(mapcar #'(lambda (m) `(call-method ,m)) all)))
                (defgeneric gf-empty (x) (:method-combination mc-long))
                (defmethod gf-empty ((x integer)) 'a)
                (gf-empty 'not-an-integer)
            """)

    def test_an_undefined_combination_name_is_an_error(self):
        with pytest.raises(Exception):
            ev("(defgeneric gf-undef (x) (:method-combination no-such-combination))")


class TestStandardCombinationStillHolds:
    def test_next_method_p_inside_an_around_reflects_the_real_chain(self):
        """It used to answer T unconditionally, because the :around frame
        carried a separate `core` closure rather than a next-method list."""
        assert ev("""
            (defgeneric gf-nmp (x)
              (:method ((x integer)) 'primary)
              (:method :after ((x integer)) nil))
            (defmethod gf-nmp :around ((x integer)) (list (next-method-p) (call-next-method)))
            (gf-nmp 1)
        """) == '(T PRIMARY)'

    def test_before_primary_after_order(self):
        assert ev("""
            (defparameter *mc-order* nil)
            (defgeneric gf-order (x)
              (:method ((x integer)) (push 'primary *mc-order*) 'r)
              (:method :before ((x integer)) (push 'before *mc-order*))
              (:method :after ((x integer)) (push 'after *mc-order*)))
            (gf-order 1)
            (reverse *mc-order*)
        """) == '(BEFORE PRIMARY AFTER)'


class TestOneMethodInvocationPath:
    def test_call_method_is_the_only_way_a_method_is_invoked(self):
        """Every combination bottoms out in `classes.call_method`, which is
        what keeps CALL-NEXT-METHOD consistent between them. If a second
        invocation path appears, the next-method context it forgets to push
        is invisible until some unrelated test fails."""
        source = (classes.__file__)
        with open(source, encoding='utf-8') as handle:
            text = handle.read()
        # `method.function(*args)` should appear exactly once: inside call_method.
        assert text.count('method.function(*args)') == 1, (
            "a second site invokes a method's function directly instead of "
            "going through classes.call_method")
