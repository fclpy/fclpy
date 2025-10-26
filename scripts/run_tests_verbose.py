#!/usr/bin/env python3
"""Run pytest with explicit verbose options and print failing test ids.

Usage: python scripts/run_tests_verbose.py
"""
import sys
import pytest


def main(argv=None):
    argv = argv if argv is not None else []
    # Ensure we include verbosity and show locals for failures
    pytest_args = ["-ra", "--showlocals"] + list(argv)
    # Run pytest and capture result
    return_code = pytest.main(pytest_args)

    # If tests failed, run one more time to capture failing node ids
    if return_code != 0:
        # Run pytest in --last-failed mode to list failures (if plugin present)
        # Otherwise run with -q to show failure locations
        print("\nDetailed pytest run (to show failing tests):")
        pytest.main(["-q"] + list(argv))

    return return_code


if __name__ == '__main__':
    sys.exit(main())
