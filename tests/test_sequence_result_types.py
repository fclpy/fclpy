"""The sequence protocol: element access and result construction (CLHS 17.1).

These tests pin the property that made this a *mechanism* rather than a set of
individual bugs: a sequence function's result must be a sequence of the right
**Lisp type**, and its elements must be read from whatever representation the
argument actually has. Before the shared protocol, results came back as Python
`list`s -- which is this implementation's *vector* -- so `(union '(1 2) '(2 3))`
and `(sort (list 3 1 2) #'<)` both answered a vector that printed convincingly
as a list, and FIND/COUNT over a `LispString` or a `LispArray` raised a
Python `TypeError` that surfaced as the value of the form.

They are grouped by the property rather than by operator on purpose: the same
assertion is what fails for all ~40 operators when the protocol is wrong.
"""

import pytest

import fclpy.lisptype as lisptype
from fclpy.lispfunc import sequences as seq
from fclpy.lispfunc.sequence_protocol import (
    build_sequence, parse_sequence_type, rebuild_like, seq_elements,
)
from fclpy.lispfunc.arrays import LispArray, make_array


def lisp_list(*items):
    result = lisptype.NIL
    for item in reversed(items):
        result = lisptype.lispCons(item, result)
    return result


def elements(value):
    """The elements of a result, for comparing contents independent of type."""
    return seq_elements(value)


class TestElementAccess:
    """Every Lisp sequence representation is readable, and nothing else is."""

    @pytest.mark.parametrize('sequence, expected', [
        (lisp_list(1, 2, 3), [1, 2, 3]),
        (lisptype.NIL, []),
        (None, []),
        ([1, 2, 3], [1, 2, 3]),
        (lisptype.LispString('abc'), ['a', 'b', 'c']),
        ('abc', ['a', 'b', 'c']),
    ])
    def test_reads_every_representation(self, sequence, expected):
        assert seq_elements(sequence) == expected

    def test_reads_an_adjustable_vector_up_to_its_fill_pointer(self):
        vector = make_array(5, initial_element=0, fill_pointer=2)
        assert seq_elements(vector) == [0, 0]

    def test_a_dotted_tail_is_not_dropped(self):
        dotted = lisptype.lispCons(1, lisptype.lispCons(2, 3))
        assert seq_elements(dotted) == [1, 2, 3]

    def test_a_non_sequence_is_a_lisp_type_error(self):
        # Not a Python TypeError: that used to reach the test suite as the
        # value of the form (plan.md standing rule 2).
        with pytest.raises(lisptype.LispTypeError):
            seq_elements(42)


class TestResultTypeDesignator:
    """CLHS 15.1.2.2 sequence type specifiers, including compound ones."""

    @pytest.mark.parametrize('designator, kind', [
        ('LIST', 'LIST'),
        ('VECTOR', 'VECTOR'),
        ('STRING', 'STRING'),
        ('SIMPLE-STRING', 'STRING'),
        ('BIT-VECTOR', 'BIT-VECTOR'),
        (None, 'NIL'),
    ])
    def test_simple_designators(self, designator, kind):
        assert parse_sequence_type(designator)[0] == kind

    def test_a_character_element_type_names_a_string(self):
        # (vector character) is a string, and the *element type* is what says
        # so -- not the head symbol.
        assert parse_sequence_type(['VECTOR', 'CHARACTER'])[0] == 'STRING'

    def test_a_length_constraint_is_parsed_and_enforced(self):
        assert parse_sequence_type(['VECTOR', 'T', 3])[1] == 3
        with pytest.raises(lisptype.LispTypeError):
            build_sequence(['VECTOR', 'T', 3], [1, 2])

    def test_an_unknown_designator_is_rejected_loudly(self):
        with pytest.raises(lisptype.LispTypeError):
            build_sequence('INTEGER', [1, 2])


class TestSameTypeResults:
    """CLHS 17.1: no `result-type` argument means "the argument's type"."""

    def test_sort_preserves_each_sequence_type(self):
        assert isinstance(seq.sort(lisp_list(3, 1, 2), lambda a, b: a < b),
                          lisptype.lispCons)
        assert isinstance(seq.sort([3, 1, 2], lambda a, b: a < b), list)
        assert str(seq.sort(lisptype.LispString('cba'),
                            lambda a, b: a < b)) == 'abc'

    def test_reverse_preserves_each_sequence_type(self):
        assert isinstance(seq.reverse(lisp_list(1, 2)), lisptype.lispCons)
        assert str(seq.reverse(lisptype.LispString('abc'))) == 'cba'

    def test_the_empty_list_result_is_nil_not_an_empty_vector(self):
        assert seq.remove(1, lisp_list(1)) is lisptype.NIL
        assert rebuild_like(lisptype.NIL, []) is lisptype.NIL

    def test_subseq_and_copy_seq_preserve_type(self):
        assert isinstance(seq.subseq(lisp_list(1, 2, 3), 1), lisptype.lispCons)
        assert str(seq.copy_seq(lisptype.LispString('ab'))) == 'ab'


class TestListResults:
    """Operators specified to return a *list* must not return a vector."""

    @pytest.mark.parametrize('call, expected', [
        (lambda: seq.union(lisp_list(1, 2), lisp_list(2, 3)), [1, 2, 3]),
        (lambda: seq.intersection(lisp_list(1, 2), lisp_list(2, 3)), [2]),
        (lambda: seq.set_difference(lisp_list(1, 2), lisp_list(2, 3)), [1]),
        (lambda: seq.mapcar(lambda x: x, lisp_list(1, 2)), [1, 2]),
        (lambda: seq.revappend(lisp_list(1, 2), lisp_list(3)), [2, 1, 3]),
        (lambda: seq.nbutlast(lisp_list(1, 2, 3)), [1, 2]),
    ])
    def test_result_is_a_lisp_list(self, call, expected):
        result = call()
        assert result is lisptype.NIL or isinstance(result, lisptype.lispCons)
        assert elements(result) == expected

    def test_member_returns_the_tail_itself(self):
        # Not a copy, and not a vector: callers walk or mutate this tail.
        original = lisp_list(1, 2, 3)
        assert seq.member(2, original) is original.cdr

    def test_append_shares_the_last_argument(self):
        tail = lisp_list(3, 4)
        assert seq.append(lisp_list(1, 2), tail).cdr.cdr is tail


class TestResultTypeArgument:
    """MAP/CONCATENATE/MERGE/MAKE-SEQUENCE build what they are asked for."""

    def test_concatenate_iterates_its_arguments(self):
        result = seq.concatenate('LIST', lisptype.LispString('ab'), [1, 2])
        assert elements(result) == ['a', 'b', 1, 2]
        assert isinstance(result, lisptype.lispCons)

    def test_map_honours_a_string_result_type(self):
        result = seq.map_fn('STRING', lambda c: c.upper(),
                            lisptype.LispString('abc'))
        assert str(result) == 'ABC'

    def test_map_with_a_nil_result_type_returns_nil(self):
        seen = []
        assert seq.map_fn(None, seen.append, [1, 2]) is lisptype.NIL
        assert seen == [1, 2]

    def test_merge_result_type_and_stability(self):
        result = seq.merge('LIST', lisp_list(1, 3), lisp_list(2, 4),
                           lambda a, b: a < b)
        assert isinstance(result, lisptype.lispCons)
        assert elements(result) == [1, 2, 3, 4]

    def test_make_sequence_builds_the_named_type(self):
        assert isinstance(seq.make_sequence('LIST', 2), lisptype.lispCons)
        assert isinstance(seq.make_sequence('VECTOR', 2), list)
        assert len(str(seq.make_sequence('STRING', 3))) == 3


class TestScanningHonoursTheProtocol:
    """FIND/POSITION/COUNT read any sequence and share one scan."""

    def test_find_and_count_work_over_a_vector_literal(self):
        vector = make_array(3, initial_element=1, adjustable=lisptype.T)
        assert seq.find(1, vector) == 1
        assert seq.count(1, vector) == 3

    def test_find_and_count_work_over_a_lisp_string(self):
        text = lisptype.LispString('aab')
        assert seq.count('a', text) == 2
        assert seq.position('b', text) == 2

    def test_from_end_is_honoured(self):
        assert seq.position(1, lisp_list(1, 2, 1), from_end=True) == 2

    def test_bounds_accept_nil_for_end(self):
        assert seq.count(1, lisp_list(1, 1), end=lisptype.NIL) == 2

    def test_not_found_is_nil(self):
        assert seq.find(9, lisp_list(1)) is lisptype.NIL
        assert seq.position(9, lisp_list(1)) is lisptype.NIL

    def test_the_default_test_is_eql_not_python_equality(self):
        # `1 == 1.0` in Python, but `(eql 1 1.0)` is false.
        assert seq.find(1.0, lisp_list(1)) is lisptype.NIL

    def test_mismatch_and_search_key_both_elements(self):
        # CLHS 17.2.1: for the two-sequence operators the key applies to
        # elements of *both* sequences, unlike FIND/POSITION.
        assert seq.mismatch([1, 2], [3, 2], key=lambda x: x % 2) is lisptype.NIL
        # Keys are 0 for the pattern and 1,1,0 for the sequence, so the only
        # match is the last position -- with the key applied to one side only
        # this answered 0.
        assert seq.search([2], [1, 3, 2], key=lambda x: x % 2) == 2


class TestHigherOrderOperators:
    """REDUCE/EVERY/SOME/MAP-INTO read through the protocol too."""

    def test_reduce_over_a_vector(self):
        assert seq.reduce_fn(lambda a, b: a + b, [1, 2, 3]) == 6

    def test_reduce_from_end_folds_right(self):
        assert seq.reduce_fn(lambda a, b: [a, b], [1, 2, 3],
                             from_end=lisptype.T) == [1, [2, 3]]

    def test_every_over_a_vector(self):
        vector = make_array(2, initial_element=1, adjustable=lisptype.T)
        assert seq.every(lambda x: lisptype.T, vector) is lisptype.T

    def test_some_returns_the_predicate_value(self):
        assert seq.some(lambda x: x if x > 1 else lisptype.NIL,
                        lisp_list(1, 2)) == 2

    def test_map_into_writes_through_a_list(self):
        destination = lisp_list(0, 0)
        assert seq.map_into(destination, lambda x: x + 1, lisp_list(1, 2)) is destination
        assert elements(destination) == [2, 3]

    def test_fill_accepts_a_lisp_list(self):
        destination = lisp_list(1, 2)
        assert seq.fill(destination, 9) is destination
        assert elements(destination) == [9, 9]

    def test_remove_duplicates_uses_the_test_and_keys_both_sides(self):
        # Set membership was the old comparison, so :test and :key were
        # ignored; and like MISMATCH, both arguments here are elements, so
        # the key applies to both.
        result = seq.remove_duplicates(lisp_list(1, 2, 3, 4),
                                       key=lambda x: x % 2)
        assert elements(result) == [3, 4]

    def test_remove_duplicates_keeps_the_later_element_by_default(self):
        assert elements(seq.remove_duplicates(lisp_list(1, 2, 1))) == [2, 1]
        assert elements(seq.remove_duplicates(lisp_list(1, 2, 1),
                                              from_end=lisptype.T)) == [1, 2]

    def test_maplist_maps_over_tails_not_elements(self):
        result = seq.maplist(lambda tail: seq_elements(tail), lisp_list(1, 2))
        assert elements(result) == [[1, 2], [2]]
