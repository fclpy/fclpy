import fclpy.lisptype as lisptype
from fclpy.runtime import FclpyREPL


def test_parse_simple_keyword_and_evaluate():
    repl = FclpyREPL(quiet=True)
    expr = repl.parse_simple_expression(':FOO')
    assert isinstance(expr, lisptype.lispKeyword)
    res = repl.evaluate_expression(expr)
    assert res is expr


def test_print_keyword_in_repl():
    repl = FclpyREPL(quiet=True)
    expr = repl.parse_with_reader('(PRINT :FOO)')
    res = repl.evaluate_expression(expr)
    # PRINT returns the object printed
    assert isinstance(res, lisptype.lispKeyword)
    assert res.name == 'FOO'
