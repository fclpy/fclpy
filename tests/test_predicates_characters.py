import fclpy.lispenv as lispenv
import fclpy.state as state
import fclpy.lisptype as lisptype
from fclpy.lisptype import LispSymbol


def setup_env():
    state.current_environment = None
    state.functions_loaded = False
    return lispenv.setup_standard_environment()

def call(env, name, *args):
    fn = env.find_func(LispSymbol(name))
    assert fn is not None, f"Function {name} not found"
    return fn(*args)

def test_char_case_insensitive_predicates_return_lisp_booleans():
    env = setup_env()
    # CHAR-EQUAL
    assert call(env, 'CHAR-EQUAL', 'a', 'A') is lisptype.T
    assert call(env, 'CHAR-EQUAL', 'a', 'B') is lisptype.NIL
    # CHAR-LESSP
    assert call(env, 'CHAR-LESSP', 'A', 'B') is lisptype.T
    assert call(env, 'CHAR-LESSP', 'B', 'A') is lisptype.NIL
    # CHAR-GREATERP
    assert call(env, 'CHAR-GREATERP', 'B', 'A') is lisptype.T
    assert call(env, 'CHAR-GREATERP', 'A', 'B') is lisptype.NIL
    # CHAR-NOT-EQUAL
    assert call(env, 'CHAR-NOT-EQUAL', 'A', 'B') is lisptype.T
    assert call(env, 'CHAR-NOT-EQUAL', 'A', 'A') is lisptype.NIL
    # CHAR-NOT-GREATERP
    assert call(env, 'CHAR-NOT-GREATERP', 'A', 'B') is lisptype.T
    assert call(env, 'CHAR-NOT-GREATERP', 'B', 'A') is lisptype.NIL
    # CHAR-NOT-LESSP
    assert call(env, 'CHAR-NOT-LESSP', 'B', 'A') is lisptype.T
    assert call(env, 'CHAR-NOT-LESSP', 'A', 'B') is lisptype.NIL
