import io as pyio
import pytest
import fclpy.lispreader as lispreader
from fclpy.readtable import get_current_readtable
import fclpy.lispfunc.io as lisp_io
import fclpy.lisptype as lisptype


def make_reader_from_string(s: str):
    string_io = pyio.StringIO(s)
    stream = lispreader.LispStream(string_io)
    reader = lispreader.LispReader(get_current_readtable().get_macro_character, stream)
    return reader


def test_print_and_read_roundtrip():
    r = make_reader_from_string('(A B C)')
    form = r.read_1()
    s = lisp_io.prin1_to_string(form)
    assert s == '(A B C)'


def test_keyword_roundtrip():
    r = make_reader_from_string(':FOO')
    form = r.read_1()
    s = lisp_io.prin1_to_string(form)
    assert s == ':FOO'


@pytest.mark.xfail(reason="Symbols are not yet interned to same identity; expected to xfail until intern implemented")
def test_symbol_identity_on_read():
    r1 = make_reader_from_string('BAR')
    v1 = r1.read_1()
    r2 = make_reader_from_string('BAR')
    v2 = r2.read_1()
    assert v1 is v2
