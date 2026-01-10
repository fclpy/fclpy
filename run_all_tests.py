#!/usr/bin/env python3
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from fclpy import runtime
from fclpy.lispfunc import setup_environment, eval_string

env = setup_environment()
base = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
doit_lsp = os.path.join(base, 'ansi-test', 'doit.lsp')


if os.path.exists(doit_lsp):
    print('Loading doit.lsp...')
    res = runtime.load_and_evaluate_file(doit_lsp, env, verbose=False)
    print('Result: %s' % str(res))
else:
    print('doit.lsp not found; continuing')
