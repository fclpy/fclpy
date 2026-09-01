#!/usr/bin/env python
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from fclpy.lispfunc import setup_environment
from fclpy import runtime
from fclpy.lispfunc.io_read import read_from_string
from fclpy.lispfunc.evaluation_special_forms import eval_macroexpand_1
from fclpy.lispfunc.evaluation_core import eval as lisp_eval


def main():
    env = setup_environment()
    ansi_test_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', 'ansi-test'))
    # Load structure-00.lsp which defines the macro
    runtime.load_and_evaluate_file(os.path.join(ansi_test_dir, 'structures', 'structure-00.lsp'), env, verbose=False)

    forms = [
        '(defstruct-with-tests struct-test-03 a b c d)',
        '(defstruct-with-tests (struct-test-04) a b c)',
        "(defstruct-with-tests (struct-test-05 :constructor) a05 b05 c05)",
    ]

    for s in forms:
        # Parse the form and the MACROEXPAND-1 wrapper
        form = read_from_string(s)
        macro_form = read_from_string(f'(MACROEXPAND-1 {s})')
        expanded = eval_macroexpand_1(macro_form, env)
        print('--- FORM ---')
        print(s)
        print('--- EXPANDED (repr) ---')
        print(repr(expanded))
        print('--- EXPANDED (str) ---')
        print(expanded)


if __name__ == '__main__':
    main()
