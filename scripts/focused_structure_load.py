#!/usr/bin/env python
import os, sys, io
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from fclpy import runtime
from fclpy.lispfunc import setup_environment

ansi_test_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', 'ansi-test'))
file1 = os.path.join(ansi_test_dir, 'structures', 'structure-00.lsp')
file2 = os.path.join(ansi_test_dir, 'structures', 'structures-02.lsp')
file_rt = os.path.join(ansi_test_dir, 'rt.lsp')

env = setup_environment()
print('Loading', file_rt)
runtime.load_and_evaluate_file(file_rt, env, verbose=True)
print('\nDone rt.lsp\n')

print('Loading', file1)
runtime.load_and_evaluate_file(file1, env, verbose=True)
print('\nDone file1\n')
print('Loading', file2)
runtime.load_and_evaluate_file(file2, env, verbose=True)
print('\nDone file2\n')

