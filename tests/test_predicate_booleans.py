import fclpy.lisptype as lisptype
from fclpy.lispfunc.utilities import functionp
from fclpy.lispfunc.comparison import null, eq, constantp
from fclpy.lispfunc.evaluation import boundp
from fclpy.lispfunc.io import pathnamep, pathname_match_p, streamp
from fclpy.lispfunc.core import array_has_fill_pointer_p, symbolp
from fclpy.lispfunc.comparison import typep


def test_predicates_return_lisp_booleans():
    # functionp should return Lisp T
    assert functionp(lambda x: x) == lisptype.T

    # null should return Lisp T for NIL
    assert null(lisptype.NIL) == lisptype.T

    # constantp should return Lisp T for literal constants
    assert constantp(1) == lisptype.T

    # boundp currently returns T by default in evaluator
    sym = lisptype.LispSymbol('X')
    assert boundp(sym) == lisptype.T

    # eq should return T for identical values and NIL otherwise
    assert eq(1, 1) == lisptype.T
    assert eq(1, 2) == lisptype.NIL

    # pathnamep should recognize strings as pathname
    assert pathnamep("/tmp/file") == lisptype.T

    # pathname_match_p should compare names using lisp_bool
    assert pathname_match_p("a","a") == lisptype.T

    # streamp: our simple heuristic will return T for StringIO-like objects
    import io as _io
    s = _io.StringIO()
    assert streamp(s) == lisptype.T

    # array_has_fill_pointer_p was converted to return Lisp boolean
    class A: pass
    a = A()
    assert array_has_fill_pointer_p(a) == lisptype.NIL

    # symbolp should identify LispSymbol
    sym = lisptype.LispSymbol('Y')
    assert symbolp(sym) == lisptype.T

    # typep basic checks
    assert typep(1, 'INTEGER') == lisptype.T
    assert typep('c', 'CHARACTER') == lisptype.T
