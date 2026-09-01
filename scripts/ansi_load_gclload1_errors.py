#!/usr/bin/env python
"""Capture ANSI gclload1.lsp loading errors only.

This avoids the long gclload2.lsp load and is intended for debugging regressions
in the RT/package/bootstrap layer.
"""

import io
import os
import sys
from contextlib import redirect_stderr, redirect_stdout
from datetime import datetime

# Add fclpy to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def main() -> None:
    ansi_test_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', 'ansi-test'))
    os.environ['LISP_CWD'] = ansi_test_dir

    # Enable per-error traceback only when requested.
    # Set to '1' when you need a Python traceback for the first failing expression.
    os.environ.setdefault('FCLPY_LOAD_TRACEBACK', '0')

    output_file = os.path.join(os.path.dirname(__file__), '..', 'ansi_gclload1_errors.txt')

    from fclpy import runtime
    from fclpy.lispfunc import setup_environment

    env = setup_environment()
    gclload1 = os.path.join(ansi_test_dir, 'gclload1.lsp')

    captured = io.StringIO()
    with redirect_stdout(captured), redirect_stderr(captured):
        runtime.load_and_evaluate_file(gclload1, env, verbose=False)

    out = captured.getvalue()
    errors = [
        line.strip()
        for line in out.splitlines()
        if line.lstrip().startswith('Error evaluating expression')
        or line.lstrip().startswith('Error loading file')
        or line.lstrip().startswith("Error: File")
    ]

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write('ANSI gclload1.lsp Loading Errors\n')
        f.write(f'Generated: {datetime.now().isoformat()}\n')
        f.write('=' * 60 + '\n\n')
        f.write(f'ANSI Test Directory: {ansi_test_dir}\n')
        f.write(f'gclload1: {gclload1}\n\n')
        f.write(f'Summary: {len(errors)} total errors\n\n')
        for i, err in enumerate(errors, 1):
            f.write(f'{i}. {err}\n')

    print(f'Wrote: {output_file} ({len(errors)} errors)')


if __name__ == '__main__':
    main()
