"""CLHS 3.4.1.4 / 3.5.1.5: keyword arguments to a builtin.

The mechanism under test is `evaluation_core.LambdaListShape` -- reading a
builtin's ANSI lambda list off its Python signature -- and the reason it is a
mechanism rather than a per-operator matter is that the standard's checks are
undecidable without it. Python's `inspect.signature` was being read as "every
defaulted parameter is a `&key` name", which cannot tell

    (union nil nil :bad t)      ; :BAD is an unrecognized keyword -> PROGRAM-ERROR
    (intern "a" :cl-test)       ; :CL-TEST is an &optional value -> fine

apart, so `split_keyword_args` had to *guess*, and guessed by letting an
unrecognized keyword become a positional argument -- a silently wrong answer
where ANSI wants a signal. Spelling a builtin's `&key` parameters
**keyword-only** states them exactly, and `&optional` stays
positional-or-keyword-with-a-default.

Every assertion here is a shape ansi-test checks across dozens of files
(`*.ERROR.3`, `*.ALLOW-OTHER-KEYS.*`, `*.KEYWORDS.*`), which is why they are
pinned once here rather than per operator.
"""

import io

import pytest

import fclpy.lisptype as lisptype
from fclpy import lispenv, state
from fclpy.lispfunc.evaluation_core import (
    eval as lisp_eval, get_func_signature_info, split_keyword_args,
)
from fclpy.lispreader import LispReader, LispStream
from fclpy.readtable import get_current_readtable


@pytest.fixture(autouse=True)
def env():
    lispenv.setup_standard_environment()


def ev(source):
    stream = LispStream(io.StringIO(source))
    form = LispReader(get_current_readtable().get_macro_character, stream).read_1()
    return lisp_eval(form, state.current_environment)


def elements(value):
    from fclpy.lispfunc.sequence_protocol import seq_elements
    return seq_elements(value)


class TestTheLambdaListIsReadFromTheSignature:
    def test_keyword_only_parameters_are_key(self):
        def f(a, b, *, test=None, key=None):
            pass
        shape = get_func_signature_info(f)
        assert shape.num_required == 2
        assert shape.num_optional == 0
        assert shape.declared_keys == frozenset({'test', 'key'})

    def test_a_defaulted_positional_parameter_is_optional(self):
        def f(a, b=None):
            pass
        shape = get_func_signature_info(f)
        assert shape.num_required == 1
        assert shape.num_optional == 1
        assert shape.declared_keys == frozenset()

    def test_the_keyword_region_starts_after_the_optionals(self):
        # Both at once is a real ANSI shape (READ-FROM-STRING), and it is the
        # case a single "required positionals then keywords" boundary cannot
        # express.
        def f(a, b=None, *, start=None):
            pass
        pos, kwargs = split_keyword_args(
            f, [1, 2, lisptype.intern_keyword('START'), 7])
        assert pos == [1, 2]
        assert kwargs == {'start': 7}


class TestUnrecognizedKeywords:
    @pytest.mark.parametrize('source', [
        '(union nil nil :bad t)',
        '(member nil nil :bad t)',
        '(subst (quote a) (quote b) nil :foo nil)',
        '(sublis (quote ((a . 1))) (quote (a)) :bad t)',
    ])
    def test_an_unrecognized_keyword_is_a_program_error(self, source):
        with pytest.raises(Exception) as excinfo:
            ev(source)
        assert 'BAD' in str(excinfo.value) or 'FOO' in str(excinfo.value)

    def test_an_odd_number_of_keyword_arguments_is_a_program_error(self):
        # CLHS 3.5.1.6. `(subst-if 'a #'null nil :test)` is the shape that used
        # to slip through as a fourth positional argument, because :TEST is not
        # one of SUBST-IF's keywords and nothing else in the call was evidence.
        for source in ('(union nil nil :key)',
                       '(subst-if (quote a) (function null) nil :test)',
                       '(member nil nil nil)'):
            with pytest.raises(Exception):
                ev(source)

    def test_a_non_symbol_in_a_keyword_position_is_a_program_error(self):
        with pytest.raises(Exception):
            ev('(union nil nil 1 2)')


class TestAllowOtherKeys:
    def test_the_leftmost_occurrence_governs(self):
        # CLHS 3.4.1.4.1: leftmost wins, wherever it appears -- so the trailing
        # `:allow-other-keys nil` does not undo the leading true one, and the
        # stray pair after it is accepted.
        assert elements(ev("(member 'b '(a b c) "
                           ":allow-other-keys 17 :allow-other-keys nil '#:x t)")) \
            == elements(ev("'(b c)"))

    def test_the_name_need_only_be_a_symbol(self):
        # 3.4.1.4.1.1: with &allow-other-keys the extra names are symbols, not
        # necessarily keywords -- an interned one here, an uninterned one above.
        result = ev("(adjoin 'a '(b c) "
                    ":allow-other-keys t :allow-other-keys nil 'bad t)")
        assert [str(x) for x in elements(result)] == ['A', 'B', 'C']

    def test_allow_other_keys_is_consumed_not_forwarded(self):
        # MAKE-LIST declares only :initial-element, yet this is legal.
        assert len(elements(ev("(make-list 5 :allow-other-keys t "
                               ":allow-other-keys nil 'bad t)"))) == 5

    def test_a_repeated_recognized_keyword_uses_the_leftmost_pair(self):
        assert len(elements(ev("(make-list 5 :initial-element 'a "
                               ":initial-element 'b)"))) == 5
        assert all(str(x) == 'A' for x in
                   elements(ev("(make-list 5 :initial-element 'a "
                               ":initial-element 'b)")))


class TestOptionalValuesStillReachTheirSlot:
    def test_a_keyword_shaped_optional_value_is_not_a_keyword_argument(self):
        # INTERN's `package` is &optional, so :CL-USER here is its *value* --
        # the designator of the package to intern into, not a stray keyword
        # argument. This is the half of the ambiguity that must keep working.
        symbol = ev('(intern "zzz" :cl-user)')
        assert symbol.package is not None
        assert symbol.package.name == 'COMMON-LISP-USER'

    def test_subseq_end_is_positional(self):
        assert elements(ev("(subseq '(1 2 3 4) 1 3)")) == [2, 3]


class TestTheMapFamilyRequiresAList:
    @pytest.mark.parametrize('source', [
        '(mapcar (function append))',
        '(maplist (function append))',
        '(mapc (function append))',
        '(mapcan (function append))',
        '(mapcon (function append))',
        '(mapl (function append))',
    ])
    def test_at_least_one_list_is_required(self, source):
        # `(function &rest lists+)`: with `*lists` Python has no arity to
        # check, and answering NIL for no lists answered the call instead of
        # rejecting it.
        with pytest.raises(Exception):
            ev(source)
