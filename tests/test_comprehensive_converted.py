import pytest

import fclpy.lispenv as lispenv
import fclpy.lispfunc as lispfunc
import fclpy.state as state
from fclpy.lisptype import LispSymbol, lispCons, Environment, NIL, T


@pytest.fixture
def env():
    # Reset shared state and build a fresh standard environment
    state.current_environment = None
    state.functions_loaded = False
    return lispenv.setup_standard_environment()


def test_function_mapping(env):
    """Test that Lisp function names properly map to Python functions."""
    test_cases = [
        ('NOT', 'not_fn'),
        ('<=', '_s_lt__s_eq_'),
        ('>=', '_s_gt__s_eq_'),
        ('<', '_s_lt_'),
        ('>', '_s_gt_'),
        ('=', '_s_eq_'),
        ('+', '_s_plus_'),
        ('-', '_s_minus_'),
        ('*', '_s_star_'),
        ('/', '_s_slash_'),
        ('CAR', 'car'),
        ('CDR', 'cdr'),
        ('CONS', 'cons'),
    ]

    for lisp_name, expected_python_name in test_cases:
        symbol = LispSymbol(lisp_name)
        func = env.find_func(symbol)
        assert func is not None, f"Function {lisp_name} not found"
        assert func.__name__ == expected_python_name, f"Expected {expected_python_name}, got {func.__name__}"


def test_arithmetic_functions(env):
    """Arithmetic semantics for + and - and comparisons."""
    plus_func = env.find_func(LispSymbol('+'))
    assert plus_func is not None and plus_func(1, 2, 3, 4) == 10

    minus_func = env.find_func(LispSymbol('-'))
    assert minus_func is not None
    assert minus_func(5) == -5
    assert minus_func(10, 3, 2) == 5

    le_func = env.find_func(LispSymbol('<='))
    assert le_func is not None
    assert le_func(3, 5) == T
    assert le_func(5, 3) == NIL


def test_basic_evaluation(env):
    """Basic eval behaviour and self-evaluating forms."""
    assert lispfunc.eval(42) == 42
    assert lispfunc.eval("hello") == "hello"
    assert lispfunc.eval(NIL) == NIL

    expr = lispCons(LispSymbol('+'), lispCons(2, lispCons(3, NIL)))
    result = lispfunc.eval(expr)
    assert result == 5


def test_special_forms(env):
    """Basic special form behaviours (quote, if)."""
    sym = LispSymbol("X")
    quoted = lispCons(LispSymbol("QUOTE"), lispCons(sym, NIL))
    assert lispfunc.eval(quoted) == sym

    if_expr = lispCons(LispSymbol("IF"), lispCons(True, lispCons(1, lispCons(2, NIL))))
    assert lispfunc.eval(if_expr) == 1


def test_metacircular_readiness(env):
    """Ensure required functions for metacircular evaluator are available."""
    required_functions = [
        'CAR', 'CDR', 'CONS', 'LIST', 'APPEND',
        'ATOM', 'NULL', 'EQ', 'NOT',
        '+', '-', '*', '/', '=', '<', '>', '<=', '>=',
        'SYMBOLP', 'NUMBERP', 'STRINGP',
        'IF', 'LAMBDA', 'DEFUN', 'SETQ',
        'EVAL', 'APPLY'
    ]

    missing = [name for name in required_functions if env.find_func(LispSymbol(name)) is None]
    assert not missing, f"Missing required functions for metacircular readiness: {', '.join(missing)}"


def test_environment_operations():
    """Test the Environment variable binding API."""
    env = Environment()
    var = LispSymbol("X")
    env.add_variable(var, 42)
    assert env.find_variable(var) == 42

    env.set_variable(var, 100)
    assert env.find_variable(var) == 100
