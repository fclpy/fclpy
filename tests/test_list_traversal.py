"""CLHS 14.2 list traversal: what a list operator does with an improper list.

These are grouped by the *property* rather than by operator, because one
mechanism was missing for all of them: there was no primitive that walked a
Lisp list, so each of ~30 operators walked it its own way and none of them
checked what it was walking. Two consequences, and the second is the expensive
one:

* A non-list argument produced NIL instead of a TYPE-ERROR -- `(member 'a 1)`
  answered NIL because the `isinstance` loop simply never entered.
* **A dotted list's terminator was read as one more element.** That is a wrong
  *value*, not a missing error: `(append '(a . b) '(z))` answered `(A B Z)`,
  `(pairlis '(a . b) '(c . d))` paired B with D, and
  `(list-length '(a b c d . e))` answered 4.

`sequence_protocol.list_cells` is that primitive, and the `dotted` policy is
the axis these tests are organised around: operators that require a *proper*
list signal, and the ones CLHS defines *on* a dotted list stop at the
terminator without consuming it.
"""

import pytest

import fclpy.lisptype as lisptype
from fclpy.lispfunc.sequence_protocol import (
    list_cells, list_elements, list_tail, seq_elements,
)


def lisp_list(*items, tail=lisptype.NIL):
    result = tail
    for item in reversed(items):
        result = lisptype.lispCons(item, result)
    return result


DOTTED = lisp_list(1, 2, tail=3)
NOT_A_LIST = [1, 2, 42, lisptype.LispString('ab'), lisptype.Character('w')]


class TestTheWalker:
    def test_a_proper_list_yields_one_cell_per_cons(self):
        cells = list(list_cells(lisp_list(1, 2, 3)))
        assert [cell.car for cell in cells] == [1, 2, 3]

    def test_nil_is_the_empty_list(self):
        assert list(list_cells(lisptype.NIL)) == []
        assert list(list_cells(None)) == []

    @pytest.mark.parametrize('value', NOT_A_LIST)
    def test_a_non_list_signals_eagerly(self, value):
        # Eagerly, not on the first next(): an operator that gives up before
        # consuming its walker -- MEMBER on the empty list, MAPLIST once
        # another argument runs out -- would otherwise never reach the check,
        # and ansi-test's check-type-error demands one for every non-list.
        with pytest.raises(lisptype.LispTypeError):
            list_cells(value, 'TEST')

    def test_a_dotted_tail_signals_by_default(self):
        with pytest.raises(lisptype.LispTypeError):
            list_elements(DOTTED, 'TEST')

    def test_the_signal_happens_at_the_step_not_up_front(self):
        # Laziness is a semantic requirement: `(member 'a '(a . b))` finds A
        # and returns before the terminator is ever reached, while
        # `(member 'x '(a . b))` has to walk past it and signals.
        walker = list_cells(DOTTED, 'TEST')
        assert next(walker).car == 1
        assert next(walker).car == 2
        with pytest.raises(lisptype.LispTypeError):
            next(walker)

    def test_dotted_allow_stops_at_the_terminator(self):
        assert list_elements(DOTTED, 'TEST', dotted='allow') == [1, 2]
        assert list_tail(DOTTED, 'TEST') == 3

    def test_a_proper_list_has_a_nil_tail(self):
        assert list_tail(lisp_list(1, 2), 'TEST') is lisptype.NIL


class TestProperListRequired:
    """CLHS 14.2's LIST arguments and 17.1's "proper sequence"."""

    @pytest.mark.parametrize('call', [
        lambda x: seq_elements(x, 'TEST'),
        lambda x: __import__('fclpy.lispfunc.sequences_compose', fromlist=['x'])
                  .append(x, lisp_list(9)),
        lambda x: __import__('fclpy.lispfunc.sequences_compose', fromlist=['x'])
                  .list_length(x),
        lambda x: __import__('fclpy.lispfunc.sequences_higher', fromlist=['x'])
                  .union(x, lisp_list(9)),
        lambda x: __import__('fclpy.lispfunc.sequences_higher', fromlist=['x'])
                  .pairlis(x, lisp_list(9, 9)),
        lambda x: __import__('fclpy.lispfunc.sequences_search', fromlist=['x'])
                  .assoc(9, x),
    ])
    def test_a_dotted_argument_is_a_type_error(self, call):
        with pytest.raises(lisptype.LispTypeError):
            call(DOTTED)


class TestDefinedOnDottedLists:
    """LAST/BUTLAST/NTHCDR count *conses*, so a terminator is not an element."""

    def setup_method(self):
        from fclpy.lispfunc import sequences_compose
        self.seq = sequences_compose

    def test_last_returns_the_tail_beyond_the_last_n_conses(self):
        dotted = lisp_list('a', 'b', tail='c')
        # The result is that tail itself, not a copy of it.
        assert self.seq.last(dotted) is dotted.cdr
        # n = 0 is the terminator itself, not NIL.
        assert self.seq.last(lisp_list('a', tail='b'), 0) == 'b'
        one = lisp_list('a', tail='b')
        assert self.seq.last(one, 1) is one

    def test_butlast_counts_conses_not_elements(self):
        assert list_elements(self.seq.butlast(lisp_list('a', 'b', 'c', tail='d'), 1)) \
            == ['a', 'b']

    def test_nbutlast_is_destructive(self):
        # nbutlast.1/.4 require the result to be EQ to the argument.
        original = lisp_list(1, 2, 3, 4, 5)
        assert self.seq.nbutlast(original, 2) is original
        assert list_elements(original) == [1, 2, 3]

    def test_nthcdr_may_enter_a_dotted_list_but_not_step_past_it(self):
        dotted = lisp_list('a', tail='b')
        assert self.seq.nthcdr(1, dotted) == 'b'
        with pytest.raises(lisptype.LispTypeError):
            self.seq.nthcdr(2, dotted)

    @pytest.mark.parametrize('n', [-1, 10.0, True, 'x'])
    def test_a_count_argument_must_be_an_unsigned_byte(self, n):
        with pytest.raises(lisptype.LispTypeError):
            self.seq.last(lisp_list(1, 2, 3), n)

    def test_list_length_answers_nil_for_a_circular_list(self):
        # The one operator CLHS requires to terminate on a circular list; every
        # other one is explicitly undefined on it, which is why the identity
        # set lives here and not in the shared walker.
        circular = lisp_list(1, 2, 3)
        circular.cdr.cdr.cdr = circular
        assert self.seq.list_length(circular) is lisptype.NIL

    def test_list_length_rejects_a_dotted_list(self):
        with pytest.raises(lisptype.LispTypeError):
            self.seq.list_length(DOTTED)


class TestNconcSplicesDestructively:
    def test_the_spine_is_rplacd_onto_the_next_argument(self):
        from fclpy.lispfunc.sequences_compose import nconc
        first, second = lisp_list('a', 'b', 'c'), lisp_list('d', 'e')
        assert nconc(first, second) is first
        assert first.cdr.cdr.cdr is second

    def test_a_dotted_non_final_argument_loses_its_terminator(self):
        # nconc.7: the terminator is overwritten by the splice, which is why
        # NCONC accepts the very shape APPEND rejects.
        from fclpy.lispfunc.sequences_compose import nconc
        result = nconc(lisp_list('a', tail='b'), lisp_list('c', tail='d'), 'foo')
        assert list_elements(result, 'TEST', dotted='allow') == ['a', 'c']
        assert list_tail(result, 'TEST') == 'foo'

    def test_the_last_argument_is_never_traversed(self):
        # So it need not be a list at all -- and `(mapcan (constantly 1) '(a))`
        # is 1 rather than (1), because MAPCAN *is* NCONC (mapcan.11).
        from fclpy.lispfunc.sequences_compose import nconc
        assert nconc(42) == 42
