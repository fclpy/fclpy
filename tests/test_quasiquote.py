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
