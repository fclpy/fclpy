import io
import pytest

import fclpy.lisptype as lisptype
from fclpy.readtable import get_current_readtable
from fclpy.lispreader import LispReader, LispStream
from fclpy.runtime import FclpyREPL


def test_parse_backquote_wraps_quasiquote():
    rt = get_current_readtable()
    s = io.StringIO('`(A B C)')
    reader = LispReader(rt.get_macro_character, LispStream(s))
    expr = reader.read_1()

    # The reader should return a form whose operator is QUASIQUOTE
    # e.g. (QUASIQUOTE (A B C))
    assert repr(expr).upper().startswith('(QUASIQUOTE')


def test_eval_print_quasiquote_captures_output(capsys):
    r = FclpyREPL(quiet=True)
    # parse the combined form and evaluate it
    expr = r.parse_with_reader('(print `(A B C))')
    # Evaluate — print will emit to stdout
    r.evaluate_expression(expr)

    captured = capsys.readouterr()
    # The printed representation should include the list (A B C)
    assert '(A B C)' in captured.out


class TestDottedTemplates:
    """A backquote template's dotted tail survives expansion.

    `eval_quasiquote` walked the template with `while consp(cur)` and built the
    result onto NIL, so the terminator was **dropped**: `` `(a . d) `` answered
    `(A)`. ansi-test builds most of its association lists with exactly the
    idiom that breaks, `` `((,x . d) (,y . e)) ``, so every such alist arrived
    with its values missing (`assoc.11`, `rassoc.11`).
    """

    @staticmethod
    def ev(source):
        r = FclpyREPL(quiet=True)
        return r.evaluate_expression(r.parse_with_reader(source))

    def test_a_literal_dotted_tail_is_preserved(self):
        assert repr(self.ev('`(a . d)')) == '(A . D)'

    def test_an_unquoted_element_before_a_dotted_tail(self):
        assert repr(self.ev('(let ((x 1)) `(,x . d))')) == '(1 . D)'
        assert repr(self.ev('(let ((x 1)) `(z ,x . d))')) == '(Z 1 . D)'

    def test_the_tail_itself_may_be_unquoted(self):
        # `` `(a . ,x) `` reads as the *proper* list `(A UNQUOTE X)`, because
        # `. (unquote x)` is just `unquote x` -- so the expander has to
        # recognise an UNQUOTE *symbol* in a car position, not only a cons
        # whose car is UNQUOTE.
        assert repr(self.ev('(let ((x 1)) `(,x . ,x))')) == '(1 . 1)'

    def test_splicing_still_composes_with_a_dotted_tail(self):
        assert repr(self.ev("`(,@'(a b) . c)")) == '(A B . C)'

    def test_a_dotted_alist_template_keeps_its_values(self):
        result = self.ev("(let ((x (list 'a 'b))) `((,x . d) (,x . e)))")
        assert repr(result) == '(((A B) . D) ((A B) . E))'
