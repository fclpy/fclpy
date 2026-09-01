#!/usr/bin/env python
"""Capture ANSI test loading errors to a file for systematic analysis."""

import os
import sys
import io
from datetime import datetime
from contextlib import redirect_stderr, redirect_stdout

# Add fclpy to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

def main():
    # Set LISP_CWD for proper path resolution
    ansi_test_dir = os.path.abspath(os.path.join(
        os.path.dirname(__file__), '..', '..', 'ansi-test'
    ))
    os.environ['LISP_CWD'] = ansi_test_dir
    # Keep default runs lightweight; opt into tracebacks when needed.
    # The parser below only collects structured "Error evaluating expression" lines.
    os.environ.setdefault('FCLPY_LOAD_TRACEBACK', '0')
    
    output_file = os.path.join(os.path.dirname(__file__), '..', 'ansi_load_errors.txt')
    
    # Import fclpy after setting up environment
    from fclpy import runtime
    from fclpy.lispfunc import setup_environment
    
    env = setup_environment()
    
    # Files to load
    gclload1 = os.path.join(ansi_test_dir, 'gclload1.lsp')
    gclload2 = os.path.join(ansi_test_dir, 'gclload2.lsp')
    rt_lsp = os.path.join(ansi_test_dir, 'rt.lsp')
    
    print(f"ANSI Test Directory: {ansi_test_dir}")
    print(f"Output file: {output_file}")
    print()
    
    # Capture stdout/stderr during loading
    captured_output = io.StringIO()
    
    print("=" * 60)
    print("Loading gclload1.lsp (RT package infrastructure)...")
    print("=" * 60)

    # Ensure the regression-test runtime file (which defines DEFTET/DEFTEST helper macros)
    # is loaded first in case compile-and-load semantics differ under this loader.
    print('Loading rt.lsp (regression-test harness) explicitly...')
    with redirect_stdout(captured_output), redirect_stderr(captured_output):
        try:
            runtime.load_and_evaluate_file(rt_lsp, env, verbose=False)
        except Exception as e:
            print(f"rt.lsp exception: {e}")

    # Now load the main gclload1 file as usual
    with redirect_stdout(captured_output), redirect_stderr(captured_output):
        try:
            runtime.load_and_evaluate_file(gclload1, env, verbose=False)
        except Exception as e:
            print(f"FATAL gclload1.lsp: {e}")
    
    gclload1_output = captured_output.getvalue()
    captured_output.truncate(0)
    captured_output.seek(0)
    
    print(f"  gclload1.lsp: {gclload1_output.count('Error')} errors")
    
    print()
    print("=" * 60)
    print("Loading gclload2.lsp (test definitions)...")
    print("=" * 60)
    
    with redirect_stdout(captured_output), redirect_stderr(captured_output):
        try:
            runtime.load_and_evaluate_file(gclload2, env, verbose=False)
        except Exception as e:
            print(f"gclload2.lsp exception: {e}")
    
    gclload2_output = captured_output.getvalue()
    
    print(f"  gclload2.lsp: {gclload2_output.count('Error')} errors")
    
    # Parse all errors
    all_output = gclload1_output + "\n" + gclload2_output

    def _is_structured_error_line(line: str) -> bool:
        s = line.lstrip()
        return (
            s.startswith('Error evaluating expression')
            or s.startswith('Error loading file')
            or s.startswith('Error: File')
        )

    errors = [line.strip() for line in all_output.split('\n') if _is_structured_error_line(line)]
    
    # Categorize errors
    error_types = {
        "Not a function": [],
        "Unbound variable": [],
        "Not implemented": [],
        "Assertion failed": [],
        "LOOP": [],
        "argument": [],
        "File not found": [],
        "EOF": [],
        "Other": []
    }
    
    for err in errors:
        categorized = False
        for cat in error_types:
            if cat != "Other" and cat.lower() in err.lower():
                error_types[cat].append(err)
                categorized = True
                break
        if not categorized:
            error_types["Other"].append(err)
    
    # Write results
    print()
    print("=" * 60)
    print("Writing results...")
    print("=" * 60)
    
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(f"ANSI Test Loading Errors\n")
        f.write(f"Generated: {datetime.now().isoformat()}\n")
        f.write(f"=" * 60 + "\n\n")
        
        f.write(f"Summary: {len(errors)} total errors\n\n")
        
        f.write("Error Categories:\n")
        for cat, errs in sorted(error_types.items(), key=lambda x: -len(x[1])):
            if errs:
                f.write(f"  {cat}: {len(errs)}\n")
        f.write("\n")
        
        # Unique errors (deduplicated)
        unique_errors = {}
        for err in errors:
            # Extract error type/message
            if ":" in err:
                parts = err.split(":", 2)
                if len(parts) >= 2:
                    key = parts[-1].strip()[:80]  # Last part, truncated
                    unique_errors.setdefault(key, []).append(err)
        
        f.write("=" * 60 + "\n")
        f.write(f"UNIQUE ERROR TYPES ({len(unique_errors)}):\n")
        f.write("=" * 60 + "\n\n")
        
        for key in sorted(unique_errors.keys()):
            count = len(unique_errors[key])
            f.write(f"[{count}x] {key}\n")
        
        f.write("\n")
        f.write("=" * 60 + "\n")
        f.write("ALL ERRORS (FULL):\n")
        f.write("=" * 60 + "\n\n")
        
        for i, err in enumerate(errors, 1):
            f.write(f"{i}. {err}\n")
    
    print(f"Results written to: {output_file}")
    print(f"  Total: {len(errors)} errors")
    print(f"  Unique: {len(unique_errors)} error types")
    print()
    print("Error categories:")
    for cat, errs in sorted(error_types.items(), key=lambda x: -len(x[1])):
        if errs:
            print(f"  {cat}: {len(errs)}")

if __name__ == '__main__':
    main()
