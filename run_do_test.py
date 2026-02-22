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

# Allow specifying a test on the command line. If the first argument
# starts with '(' we treat it as a full Lisp form to evaluate. Otherwise
# we treat it as a test name and wrap it in the usual (in-package ...
# (do-test 'NAME)) form to run tests from the cl-test package.
if len(sys.argv) > 1:
	arg = sys.argv[1]
	if arg.strip().startswith('('):
		test_lisp = arg
	else:
		test_lisp = "(in-package :cl-test) (do-test '%s)" % arg
else:
	# Default test when no arg provided
	test_lisp = "(in-package :cl-test) (do-test 'macroexpand-1.error.1)"

print("Running %s" % test_lisp)
res = eval_string(test_lisp, env)
print('Result:', res)
