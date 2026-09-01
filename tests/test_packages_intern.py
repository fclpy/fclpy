import fclpy.lisptype as lisptype
from fclpy.lispfunc import utilities as utils


def test_intern_idempotent_same_package():
    pkg = lisptype.make_package('TEST-PKG-1')
    s1 = utils.intern('FOO', pkg)
    s2 = utils.intern('FOO', pkg)
    assert s1 is s2


def test_intern_different_packages_not_same():
    p1 = lisptype.make_package('TEST-PKG-A')
    p2 = lisptype.make_package('TEST-PKG-B')
    a1 = utils.intern('BAR', p1)
    b1 = utils.intern('BAR', p2)
    assert a1 is not b1
