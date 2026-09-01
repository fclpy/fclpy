import fclpy.readtable as readtable_mod
from fclpy.lispfunc.io import (
    set_macro_character,
    get_macro_character,
    set_dispatch_macro_character,
    get_dispatch_macro_character,
    copy_readtable,
)
from fclpy.readtable import Readtable


def test_set_and_copy_readtable():
    # Dummy reader functions
    def dummy(c, s):
        return None

    def sub_dummy(c, s):
        return None

    # Ensure we have a current readtable
    rt0 = readtable_mod.get_current_readtable()

    # Install a macro character and verify it is registered
    set_macro_character('~', dummy, True)
    mc = get_macro_character('~')
    assert mc is not None
    assert callable(mc[0])

    # Copy the readtable and verify the result is a Readtable instance
    new_rt = copy_readtable(rt0)
    assert isinstance(new_rt, Readtable)

    # Install a dispatch macro character and verify retrieval
    set_dispatch_macro_character('#', 'a', sub_dummy)
    sub = get_dispatch_macro_character('#', 'a')
    assert callable(sub)
