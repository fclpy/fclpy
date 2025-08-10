#!/usr/bin/env python3
"""
FCLPY Command Line Interface
A simple CLI wrapper for the FCLPY Lisp interpreter that mimics clisp functionality.
"""

import sys
import os
import argparse
from pathlib import Path

# Add fclpy to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'fclpy'))

from fclpy import runtime
from fclpy import lispenv

def run_comprehensive_tests():
    """Import and run the comprehensive test suite."""
    try:
        import test_comprehensive
        test_comprehensive.main()
        return True
    except ImportError:
        print("Error: test_comprehensive.py not found")
        return False
    except Exception as e:
        print(f"Error running tests: {e}")
        return False

def main():
    """Main entry point for the FCLPY CLI."""
    parser = argparse.ArgumentParser(
        prog='fclpy',
        description='FCLPY - A Common Lisp Interpreter',
        epilog="""Examples:
  fclpy                    # Start interactive REPL
  fclpy script.lisp        # Run a Lisp script
  fclpy -i script.lisp     # Run script then enter REPL
  fclpy -q                 # Quiet mode (no banner)
  fclpy -v                 # Verbose mode
  fclpy --test             # Run built-in tests""",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    
    parser.add_argument('files', nargs='*', help='Lisp files to evaluate')
    parser.add_argument('-i', '--interactive', action='store_true',
                       help='Enter REPL after processing files')
    parser.add_argument('-q', '--quiet', action='store_true',
                       help='Suppress startup messages')
    parser.add_argument('-v', '--verbose', action='store_true',
                       help='Verbose output')
    parser.add_argument('--test', action='store_true',
                       help='Run comprehensive tests')
    parser.add_argument('--version', action='version', version='FCLPY 1.0')
    
    # Additional clisp-like options (placeholders for future implementation)
    parser.add_argument('-E', '--encoding', default='utf-8',
                       help='File encoding (default: utf-8)')
    parser.add_argument('-norc', action='store_true',
                       help='Do not load init file (placeholder)')
    parser.add_argument('-ansi', action='store_true',
                       help='ANSI Common Lisp mode (placeholder)')
    
    args = parser.parse_args()
    
    # Handle test mode
    if args.test:
        if not args.quiet:
            print("Running FCLPY comprehensive tests...")
        success = run_comprehensive_tests()
        sys.exit(0 if success else 1)
    
    # Set up environment
    lispenv.setup_standard_environment()
    environment = lispenv.current_environment
    
    # Process files if provided
    if args.files:
        for filename in args.files:
            if not runtime.load_and_evaluate_file(filename, environment, args.verbose):
                sys.exit(1)
    
    # Enter REPL if requested or if no files provided
    if args.interactive or not args.files:
        runtime.repl(quiet=args.quiet, verbose=args.verbose)

if __name__ == "__main__":
    main()
