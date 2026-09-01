import fclpy.lisptype as lisptype
from fclpy.lispfunc.evaluation import eval as lisp_eval


def test_keyword_evaluates_to_itself():
    # Create a keyword instance and evaluate it in an empty environment
    kw = lisptype.lispKeyword('FOO')
    env = lisptype.Environment()
    result = lisp_eval(kw, env)
    assert result is kw
