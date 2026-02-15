#!/usr/bin/env python3
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from fclpy import runtime
from fclpy.lispfunc import setup_environment, eval_string

env = setup_environment()
base = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
init_lsp = os.path.join(base, 'ansi-test', 'init.lsp')
rt = os.path.join(base, 'ansi-test', 'rt.lsp')
rt_test = os.path.join(base, 'ansi-test', 'rt-test.lsp')

if os.path.exists(init_lsp):
	print('Loading init.lsp...')
	runtime.load_and_evaluate_file(init_lsp, env, verbose=False)
else:
	print('init.lsp not found; continuing')

# test_lisp = "(in-package :cl-test) (do-test 'char=.1)"
# test_lisp = "(in-package :cl-test) (do-test 'symbol-&allow-other-keys)"

# Test all eval error tests
# test_lisp = "(in-package :cl-test) (progn (do-test 'eval.error.1) (do-test 'eval.error.2) (do-test 'eval.error.3) (do-test 'eval.error.4))"
test_lisp = "(in-package :cl-test) (do-test 'defmacro.3)"

print("Running %s" % test_lisp)
res = eval_string(test_lisp, env)
print('Result:', res)
