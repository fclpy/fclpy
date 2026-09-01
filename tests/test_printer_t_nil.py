import fclpy.lisptype as lisptype
from fclpy.lispfunc import io


def test_write_to_string_prints_T_and_NIL():
    # Construct Lisp list (T NIL)
    cons = lisptype.lispCons(lisptype.T, lisptype.lispCons(lisptype.NIL))
    s = io.write_to_string(cons)
    assert s == "(T NIL)"
