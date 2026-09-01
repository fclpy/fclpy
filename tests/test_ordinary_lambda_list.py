"""LAMBDA, DEFUN, FLET and LABELS share one ordinary-lambda-list binder.

These tests exist because they used to have three, and the three agreed on
almost nothing (plan.md Changelog, 2026-08-22). Every assertion here is
written so that it would pass on *one* of the old binders and fail on at
least one other -- that is the regression this file guards: not "does
`&optional` work", but "do all four operators answer the same".

Each case is therefore parameterised over the four defining forms rather than
written once against LAMBDA. A binder that reappears for FLET only, which is
exactly what happened before, fails here immediately.
"""

import pytest

from fclpy import lispenv, lisptype
from fclpy.lispfunc import eval_string


@pytest.fixture(autouse=True)
def env():
    lispenv.setup_standard_environment()


def _wrap(operator, lambda_list, body, call_args):
    """`(<operator> ...)` defining `%F` with `lambda_list`, then calling it.

    The point of this helper is that the *same* lambda list and the *same*
    call go through each operator, so a difference in the answer is a
    difference between binders and nothing else.
    """
    if operator == 'LAMBDA':
        return f"((lambda {lambda_list} {body}) {call_args})"
    if operator == 'DEFUN':
        return f"(progn (defun %f {lambda_list} {body}) (%f {call_args}))"
    return f"({operator.lower()} ((%f {lambda_list} {body})) (%f {call_args}))"


OPERATORS = ['LAMBDA', 'DEFUN', 'FLET', 'LABELS']


def check_all(lambda_list, body, call_args, expected):
    """Assert every operator answers `expected` for the same lambda list."""
    for operator in OPERATORS:
        source = _wrap(operator, lambda_list, body, call_args)
        assert str(eval_string(source)) == expected, f"{operator}: {source}"


def signals_all(lambda_list, body, call_args):
    """Assert every operator signals a PROGRAM-ERROR for the same call.

    Asserted through HANDLER-CASE rather than `pytest.raises`, because the
    thing under test is the *condition type* the form signals, and a Python
    exception class is not that -- catching one would pass even if the
    condition were a plain ERROR, which is the distinction CLHS 3.5.1 draws.
    """
    for operator in OPERATORS:
        source = _wrap(operator, lambda_list, body, call_args)
        guarded = f"(handler-case {source} (program-error () :caught))"
        assert str(eval_string(guarded)) == ':CAUGHT', f"{operator}: {source}"


class TestSuppliedP:
    """`&optional`/`&key` supplied-p variables. FLET/LABELS parsed these and
    then threw them away, so reading one signalled `Unbound variable`."""

    def test_key_supplied_p_when_omitted(self):
        check_all('(&key a (b 0 b-p))', '(list a b (not (not b-p)))', '',
                  '(NIL 0 NIL)')

    def test_key_supplied_p_when_given(self):
        check_all('(&key a (b 0 b-p))', '(list a b (not (not b-p)))', ':b 2',
                  '(NIL 2 T)')

    def test_key_supplied_p_is_true_even_for_a_nil_value(self):
        # CLHS 3.4.1.4: supplied-p reports whether the argument was *passed*,
        # which a NIL value cannot be distinguished from otherwise.
        check_all('(&key (a 1 a-p))', '(list a (not (not a-p)))', ':a nil',
                  '(NIL T)')

    def test_optional_supplied_p(self):
        check_all('(x &optional (y 1 y-p))', '(list x y (not (not y-p)))',
                  "'a", '(A 1 NIL)')


class TestRestAndKeyTogether:
    """CLHS 3.4.1: `&rest` receives *all* remaining arguments, the ones the
    `&key` parameters also consume included. LAMBDA's binder stopped `&rest`
    at the first keyword-shaped value, so it received nothing."""

    def test_rest_includes_the_keyword_arguments(self):
        check_all('(x &rest r &key foo)', '(list x r foo)', "'a :foo 'h",
                  '(A (:FOO H) H)')

    def test_bare_key_still_opens_the_keyword_region(self):
        # `&key` naming no parameters is not the same as no `&key` at all.
        check_all('(&rest x &key)', 'x', ':allow-other-keys nil',
                  '(:ALLOW-OTHER-KEYS NIL)')


class TestKeywordArgumentRules:
    def test_leftmost_pair_wins_for_a_repeated_keyword(self):
        # CLHS 3.4.1.4. The binder that installed defaults first and then
        # overwrote them took the rightmost.
        check_all('(&key a b)', '(list a b)', ':a 1 :b 2 :a 3', '(1 2)')

    def test_non_keyword_keyword_name(self):
        # ((keyword-name var) init): the parameter answers to the symbol
        # written, not to a keyword of the same name.
        check_all('(&key ((foo bar) nil))', 'bar', "'foo 10", '10')

    def test_a_plain_key_parameter_answers_only_to_its_keyword(self):
        # `&key b` declares :B, so the *symbol* B is an unrecognized keyword
        # argument -- matching on the name alone bound B here.
        signals_all('(&key b)', 'b', "'b 100")

    def test_allow_other_keys_may_itself_be_a_parameter(self):
        # CLHS 3.4.1.4.1 makes :ALLOW-OTHER-KEYS always permissible; that does
        # not remove it from the argument list when the lambda list names it.
        check_all('(&key a ((:allow-other-keys aok)))', '(list aok a)',
                  ':a 1 :allow-other-keys t :c 20', '(T 1)')

    def test_unrecognized_keyword_is_a_program_error(self):
        signals_all('(&key a)', 'a', ':b 1')

    def test_odd_keyword_list_is_a_program_error(self):
        signals_all('(&key a)', 'a', ':a')

    def test_non_symbol_keyword_name_is_a_program_error(self):
        signals_all('(&key a)', 'a', "'(foo)")


class TestAux:
    """`&aux` was two literal `pass` branches in the FLET/LABELS binder."""

    def test_aux_init_forms_see_earlier_parameters_in_order(self):
        check_all('(x y &aux (a (1+ x)) (b (+ x y a)) (c (list x y a b)))',
                  'c', '5 9', '(5 9 6 20)')

    def test_aux_init_forms_see_rest_and_key(self):
        check_all('(x &rest r &key foo &aux (c (list x r foo)))', 'c',
                  "1 :foo 'a", '(1 (:FOO A) A)')


class TestArity:
    """CLHS 3.5.1.2/3.5.1.3. Every binder used to pad with NIL and discard."""

    def test_too_few_arguments(self):
        signals_all('(a)', 'a', '')

    def test_too_many_arguments(self):
        signals_all('(a)', 'a', '1 2')

    def test_optional_raises_the_maximum(self):
        check_all('(a &optional b)', '(list a b)', '1 2', '(1 2)')

    def test_rest_removes_the_maximum(self):
        check_all('(a &rest r)', '(list a r)', '1 2 3', '(1 (2 3))')


class TestSpecialDeclarations:
    """A parameter declared SPECIAL binds the symbol's value cell for the
    call's dynamic extent (CLHS 11.1.2.1.2). No binder honoured this."""

    def test_required_parameter_declared_special(self):
        assert str(eval_string(
            "(let ((x 'bad)) (declare (special x))"
            "  (flet ((%f () x))"
            "    ((lambda (x) (declare (special x)) (%f)) 'good)))")) == 'GOOD'

    def test_aux_parameter_declared_special(self):
        assert str(eval_string(
            "(let ((y :bad)) (declare (special y))"
            "  (flet ((%f () y))"
            "    ((lambda (x &aux (y :good)) (declare (special y)) (%f)) nil)))"
        )) == ':GOOD'

    def test_dynamic_binding_is_not_shadowed_by_an_enclosing_lexical_one(self):
        # The dynamic binding adds nothing to the environment, so the ordinary
        # "chain, then value cell" lookup found the outer LEXICAL Y first.
        assert str(eval_string(
            "(let ((y :bad1))"
            "  (let ((y :bad2)) (declare (special y))"
            "    (flet ((%f () y))"
            "      ((lambda (x &aux (y :good)) (declare (special y)) (%f)) nil))))"
        )) == ':GOOD'

    def test_a_free_declaration_does_not_cover_init_forms(self):
        # CLHS 3.3.4: a *free* declaration's scope excludes initialization
        # forms, so this &aux init form reads the enclosing LEXICAL X.
        assert str(eval_string(
            "(let ((x :bad)) (declare (special x))"
            "  (let ((x :good))"
            "    (flet ((%f (&aux (y x)) (declare (special x)) y))"
            "      (%f))))")) == ':GOOD'

    def test_assignment_through_a_special_declaration(self):
        # `(incf x)` under a declaration expands to the place
        # (%SPECIAL-REF x), which had a reader and no writer -- so this used
        # to answer "Undefined function: (SETF %SPECIAL-REF)" as a *value*.
        assert str(eval_string(
            "(flet ((%f () (locally (declare (special *x*)) (incf *x*))))"
            "  ((lambda (*x*) (declare (special *x*)) (%f) *x*) 10))")) == '11'

    def test_the_dynamic_binding_is_undone_on_exit(self):
        assert str(eval_string(
            "(progn ((lambda (x) (declare (special x)) x) 'inner)"
            "       (boundp 'x))")) == 'NIL'


class TestImplicitBlock:
    """CLHS 3.1.2.1.2.2: the implicit block encloses the *body*, not the
    lambda list -- so a RETURN-FROM in an init form leaves the function."""

    def test_return_from_in_an_aux_init_form_escapes_the_function(self):
        assert str(eval_string(
            "(block %f (flet ((%f (&aux (x (return-from %f 10))) 20)) (%f)))"
        )) == '10'

    def test_a_local_function_has_an_implicit_block_of_its_own_name(self):
        assert str(eval_string(
            "(flet ((%f () (return-from %f 15) 35)) (%f))")) == '15'
        assert str(eval_string(
            "(labels ((%f () (return-from %f 15) 35)) (%f))")) == '15'

    def test_lambda_establishes_no_implicit_block(self):
        # A bare (return-from nil ...) inside a LAMBDA must keep propagating.
        assert str(eval_string(
            "(block nil (funcall (lambda () (return-from nil 7))) 9)")) == '7'


class TestDocumentationString:
    def test_a_lone_string_is_the_body_not_documentation(self):
        # CLHS 3.4.11: documentation must be followed by at least one form.
        assert str(eval_string('(progn (defun %f () "x") (%f))')) == 'x'

    def test_a_string_followed_by_a_form_is_documentation(self):
        assert str(eval_string('(progn (defun %f () "doc" 5) (%f))')) == '5'


class TestLocalFunctionNames:
    def test_flet_accepts_a_setf_function_name(self):
        # `isinstance(name, LispSymbol)` skipped every other shape, so this
        # defined nothing at all rather than signalling.
        assert eval_string(
            "(flet (((setf %f) (x y) (setf (car y) x)))"
            "  (functionp #'(setf %f)))") is not lisptype.NIL
