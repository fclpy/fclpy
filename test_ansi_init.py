#!/usr/bin/env python3
"""Test loading the actual ANSI init.lsp to find where DEFTEST fails."""

import sys
import os

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from fclpy.runtime import load_and_evaluate_file
import fclpy.lispenv as lispenv

print("=" * 70)
print("LOADING ACTUAL ANSI init.lsp")
print("=" * 70)

# Change to the ansi-test directory so relative paths work
ansi_test_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'ansi-test')
ansi_test_dir = os.path.abspath(ansi_test_dir)
os.chdir(ansi_test_dir)
print(f"Working directory: {os.getcwd()}")

lispenv.setup_standard_environment()
env = lispenv.current_environment

print("\nLoading init.lsp...")
try:
    results = load_and_evaluate_file('init.lsp', environment=env, verbose=True)
    print(f"\nSuccess!")
except Exception as e:
    print(f"\nFailed with error: {e}")
    import traceback
    traceback.print_exc()
