import pytest
import fclpy.lisptype as lisptype
from fclpy.lispfunc.utilities import functionp
from fclpy.lispfunc.comparison import null, eq, constantp
from fclpy.lispfunc.evaluation import boundp
from fclpy.lispfunc.io import pathnamep, pathname_match_p, streamp
from fclpy.lispfunc.arrays import array_has_fill_pointer_p
from fclpy.lispfunc.core import symbolp
from fclpy.lispfunc.comparison import typep


def test_predicates_return_lisp_booleans():
    # functionp should return Lisp T
    assert functionp(lambda x: x) == lisptype.T

    # null should return Lisp T for NIL
    assert null(lisptype.NIL) == lisptype.T

    # constantp should return Lisp T for literal constants
    assert constantp(1) == lisptype.T

    # boundp should return NIL for newly-created symbols (unbound)
    sym = lisptype.LispSymbol('X')
    assert boundp(sym) == lisptype.NIL

    # eq should return T for identical values and NIL otherwise
    assert eq(1, 1) == lisptype.T
    assert eq(1, 2) == lisptype.NIL

    # pathnamep should recognize Pathname objects (not strings)
    from fclpy.lispfunc.pathnames import Pathname
    assert pathnamep(Pathname("/tmp/file")) == lisptype.T
    assert pathnamep("/tmp/file") == lisptype.NIL  # strings are NOT pathnames

    # pathname_match_p should compare names using lisp_bool
    assert pathname_match_p("a","a") == lisptype.T

    # streamp: our simple heuristic will return T for StringIO-like objects
    import io as _io
    s = _io.StringIO()
    assert streamp(s) == lisptype.T

    # ARRAY-HAS-FILL-POINTER-P answers a Lisp boolean for an array, and
    # signals for anything else: its argument must be an array (CLHS 15.2.16),
    # so answering NIL for a non-array conflated "no fill pointer" with "not
    # an array at all".
    assert array_has_fill_pointer_p([1, 2]) == lisptype.NIL
    class A: pass
    with pytest.raises(lisptype.LispTypeError):
        array_has_fill_pointer_p(A())

    # symbolp should identify LispSymbol
    sym = lisptype.LispSymbol('Y')
    assert symbolp(sym) == lisptype.T

    # typep basic checks
    assert typep(1, 'INTEGER') == lisptype.T
    assert typep('c', 'CHARACTER') == lisptype.T
