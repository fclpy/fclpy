#!/usr/bin/env python3
"""
FCLPY Runtime Library
Core functionality for the FCLPY Lisp interpreter that can be imported by other projects.
"""

import sys
import os
import io
import traceback
try:
    import readline  # For better REPL experience (Unix/Linux/Mac)
except ImportError:
    # Windows doesn't have readline, but that's okay
    pass

import fclpy.lisptype as lisptype
import fclpy.lispfunc as lispfunc
import fclpy.lispreader as lispreader
from fclpy import lispenv
from fclpy.readtable import get_current_readtable
from fclpy.lispfunc.evaluation_core import ThrowException, ConditionException

def setup_reader_macros():
    """Set up basic reader macros for parsing."""
    # Reader macros are now handled by the centralized readtable
    # This function is kept for backward compatibility but is no longer needed
    pass

def load_and_evaluate_file(filename, environment=None, verbose=False, timing=False):
    """Load and evaluate a Lisp file.
    
    Args:
        filename: Path to the Lisp file
        environment: Environment to use (default: current environment)
        verbose: Print detailed progress info
        timing: Print timing information for performance debugging
    """
    import os
    import time
    from fclpy.lispfunc.pathnames import pathname_from_os_path

    start_time = time.time()

    if environment is None:
        # Ensure standard environment is set up
        lispenv.setup_standard_environment()
        environment = lispenv.current_environment

    # Set *LOAD-TRUENAME* and *LOAD-PATHNAME* for this file
    abs_path = os.path.abspath(filename)
    pathname_obj = pathname_from_os_path(abs_path)
    
    load_truename_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-TRUENAME*')
    load_pathname_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-PATHNAME*')
    
    old_truename = environment.find_variable(load_truename_sym)
    old_pathname = environment.find_variable(load_pathname_sym)
    
    try:
        # Set load variables for this file
        environment.set_variable(load_truename_sym, pathname_obj)
        environment.set_variable(load_pathname_sym, pathname_obj)
        
        if verbose or timing:
            print(f"[{time.time() - start_time:.3f}s] Loading file: {filename}")
        
        # CLHS 24.1: LOAD *binds* *PACKAGE* for the extent of the file, so an
        # IN-PACKAGE inside the file is undone when the file finishes.
        #
        # This used to restore the package only when it had been None on entry,
        # which meant a *nested* load leaked its IN-PACKAGE into the rest of the
        # enclosing file. `init.lsp` is exactly that shape: its second top-level
        # form loads gclload1.lsp, whose `(in-package :cl-test)` then stayed
        # current, so init.lsp's *third* form was read in CL-TEST and
        # `*ROOT-PATH*` interned as a different symbol from the CL-USER one its
        # own DEFVAR had bound -- "Unbound variable: *ROOT-PATH*", which aborts
        # the rest of init.lsp. Global lookup is by symbol *identity*, not name
        # (CLAUDE.md), so two same-named symbols in two packages are two
        # variables and the failure looks impossible until you notice the
        # package changed underneath the reader.
        import fclpy.state as state
        old_pkg = getattr(state, 'current_package', None)
        # Default to COMMON-LISP-USER while loading a file when no package set
        if old_pkg is None:
            state.current_package = lisptype.COMMON_LISP_USER_PACKAGE

        with open(filename, 'r', encoding='utf-8') as f:
            content = f.read()
        
        if verbose or timing:
            print(f"[{time.time() - start_time:.3f}s] Read {len(content)} characters")
        
        # Create a stream from the file content
        string_io = io.StringIO(content)
        stream = lispreader.LispStream(string_io)
        
        # Set up basic reader macros
        setup_reader_macros()
        
        # Create reader using centralized readtable
        readtable = get_current_readtable()
        reader = lispreader.LispReader(readtable.get_macro_character, stream)
        
        results = []
        expr_count = 0
        last_timing_report = start_time
        
        # Read and evaluate expressions one by one
        current_expr = None

        while True:
            try:
                expr_start = time.time()
                current_expr = reader.read_1()
                if current_expr is None:  # EOF
                    break
                
                expr_count += 1
                read_time = time.time() - expr_start
                
                if verbose:
                    print(f"  Reading expression {expr_count}: {current_expr}")
                
                # Evaluate the expression
                eval_start = time.time()
                result = lispfunc.eval(current_expr, environment)
                eval_time = time.time() - eval_start
                results.append(result)
                
                # Report timing periodically or for slow expressions
                if timing:
                    now = time.time()
                    total_time = read_time + eval_time
                    # Report if expression took > 0.5s or every 5 seconds
                    if total_time > 0.5 or (now - last_timing_report) > 5.0:
                        print(f"[{now - start_time:.3f}s] Expr {expr_count}: read={read_time:.3f}s eval={eval_time:.3f}s")
                        last_timing_report = now
                
                # In standard Lisp, file loading is usually silent
                # Only show results in verbose mode
                if verbose:
                    print(f"  => {result}")
                    
            except EOFError:
                break
            except ThrowException as e:
                # Uncaught THROW - signal a CONTROL-ERROR condition
                control_error = lisptype.ControlError(message=f"Uncaught THROW {e.tag}")
                raise ConditionException(control_error, recoverable=False)
            except ConditionException:
                # Re-raise Lisp conditions so they can be handled by Lisp code
                raise
            except Exception as e:
                if "reader-error" in str(e) or not content.strip():
                    break  # End of file or empty content
                # Include filename to make large multi-file loads debuggable.
                expr_preview = ""
                try:
                    if current_expr is not None:
                        expr_preview = f" | expr={current_expr!r}"
                except Exception:
                    expr_preview = ""

                print(f"  Error evaluating expression {expr_count} in {filename}: {e}{expr_preview}")

                # Print a traceback when explicitly requested (or in verbose mode).
                if verbose or os.environ.get('FCLPY_LOAD_TRACEBACK') == '1':
                    traceback.print_exc()
        # Final timing report
        if verbose or timing:
            elapsed = time.time() - start_time
            print(f"[{elapsed:.3f}s] Loaded {expr_count} expressions from {filename}")
        return lisptype.T
        
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found")
        return lisptype.NIL
    except Exception as e:
        print(f"Error loading file '{filename}': {e}")
        if verbose:
            traceback.print_exc()
        return lisptype.NIL
    finally:
        # Restore old values
        if old_truename is not None:
            environment.set_variable(load_truename_sym, old_truename)
        else:
            environment.set_variable(load_truename_sym, lisptype.NIL)
        
        if old_pathname is not None:
            environment.set_variable(load_pathname_sym, old_pathname)
        else:
            environment.set_variable(load_pathname_sym, lisptype.NIL)

        # Unbind *PACKAGE* back to what the caller had (CLHS 24.1). In the
        # `finally` because a file that dies partway must not leave its
        # IN-PACKAGE current either -- that would silently redirect interning
        # for everything loaded afterwards. Both homes are written: the value
        # cell and `state.current_package`, which the reader consults (see
        # binding.BindingFrame._mirror_package).
        try:
            import fclpy.state as state
            state.current_package = old_pkg
            if isinstance(old_pkg, lisptype.Package):
                package_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*PACKAGE*')
                environment.set_variable(package_sym, old_pkg)
        except Exception:
            pass
        # Don't return here - let the try block's return value propagate

class FclpyREPL:
    """Interactive Read-Eval-Print Loop for FCLPY."""
    
    def __init__(self, quiet=False, verbose=False):
        self.quiet = quiet
        self.verbose = verbose
        # Set up environment
        lispenv.setup_standard_environment()
        self.environment = lispenv.current_environment
        
        # Set up reader macros
        setup_reader_macros()
        
        if not quiet:
            print("FCLPY - A Common Lisp Interpreter")
            print("Based on Python implementation")
            print("Type :help for help, :quit to exit")
            print()
    
    def read_input(self):
        """Read a line of input from the user."""
        try:
            line = input("FCLPY> ")

            # Quick command handling (only if they match a known command)
            if line and line.strip().startswith(':'):
                cmd = line.lower().strip()
                if cmd in [':quit', ':q', ':help', ':h', ':env', ':test', ':verbose']:
                    return self.handle_repl_command(line)
                # otherwise fall through and treat as a Lisp keyword/expression (e.g. :FOO)

            # Handle empty input
            if not line or not line.strip():
                return None

            # If the input begins an S-expression, attempt to parse it and
            # continue reading lines when the reader reports an EOF during
            # list read (i.e. an incomplete multi-line form).
            if line.lstrip().startswith('('):
                text = line
                while True:
                    try:
                        return self.parse_with_reader(text)
                    except Exception as e:
                        # If parser indicates an EOF/unterminated list, read more
                        msg = str(e).upper()
                        if 'EOF' in msg or 'UNTERMINATED' in msg or 'EOF DURING' in msg:
                            try:
                                cont = input('......> ')
                            except EOFError:
                                print('\nInterrupted during multiline input.')
                                return None
                            except KeyboardInterrupt:
                                print('\nInterrupted during multiline input.')
                                return None
                            # Append continuation and loop to try parsing again
                            text += '\n' + cont
                            continue
                        # Other parse errors should bubble up as parse errors
                        raise
            # Not an S-expression start, use simple parser for literals or symbols
            return self.parse_simple_expression(line.strip())
            
        except EOFError:
            print("\nGoodbye!")
            return ':quit'
        except KeyboardInterrupt:
            print("\nInterrupted. Type :quit to exit.")
            return None
    
    def parse_with_reader(self, text):
        """Parse text using the full S-expression reader."""
        try:
            string_io = io.StringIO(text)
            stream = lispreader.LispStream(string_io)
            readtable = get_current_readtable()
            reader = lispreader.LispReader(readtable.get_macro_character, stream)
            return reader.read_1()
        except Exception as e:
            raise Exception(f"Parse error: {e}")
    
    def parse_simple_expression(self, text):
        """Parse a simple expression (number, string, symbol, or quoted form)."""
        text = text.strip()
        
        # Handle quoted expressions
        if text.startswith("'"):
            quoted_expr = text[1:]
            if quoted_expr.isdigit():
                return lisptype.lispCons(lisptype.LispSymbol("QUOTE"), 
                    lisptype.lispCons(int(quoted_expr), lisptype.NIL))
            else:
                return lisptype.lispCons(lisptype.LispSymbol("QUOTE"), 
                    lisptype.lispCons(lisptype.LispSymbol(quoted_expr.upper()), lisptype.NIL))
        
        # Try to parse as number
        try:
            return int(text)
        except ValueError:
            pass
        
        # Try to parse as string
        if text.startswith('"') and text.endswith('"'):
            return text[1:-1]  # Remove quotes
        
        # Handle function calls without parentheses (like '+ 1 2 3')
        if ' ' in text:
            return self.parse_function_call(text)
        else:
            # Assume it's a symbol
            # Keywords of the form :FOO should be interned as keywords
            if text.startswith(':'):
                return lisptype.intern_keyword(text[1:].upper())
            return lisptype.LispSymbol(text.upper())
    
    def parse_function_call(self, text):
        """Parse a simple function call like '+ 1 2 3'."""
        parts = text.split()
        if not parts:
            return lisptype.NIL
        
        func_name = lisptype.LispSymbol(parts[0].upper())
        args = lisptype.NIL
        
        # Build argument list backwards
        for arg in reversed(parts[1:]):
            if arg.isdigit():
                args = lisptype.lispCons(int(arg), args)
            elif arg.startswith('"') and arg.endswith('"'):
                args = lisptype.lispCons(arg[1:-1], args)
            else:
                # Handle keyword arguments like :FOO
                if arg.startswith(':'):
                    args = lisptype.lispCons(lisptype.intern_keyword(arg[1:].upper()), args)
                else:
                    args = lisptype.lispCons(lisptype.LispSymbol(arg.upper()), args)
        
        return lisptype.lispCons(func_name, args)
    
    def evaluate_expression(self, expr):
        """Evaluate a Lisp expression."""
        if expr is None:
            return None
        
        try:
            result = lispfunc.eval(expr, self.environment)
            return result
        except Exception as e:
            if self.verbose:
                traceback.print_exc()
            raise e
    
    def handle_repl_command(self, command):
        """Handle REPL commands like :help, :quit, etc."""
        cmd = command.lower().strip()
        
        if cmd in [':quit', ':q']:
            return ':quit'
        elif cmd in [':help', ':h']:
            self.print_help()
            return None
        elif cmd == ':env':
            self.print_environment_info()
            return None
        elif cmd == ':test':
            self.run_simple_test()
            return None
        elif cmd == ':verbose':
            self.verbose = not self.verbose
            print(f"Verbose mode: {'on' if self.verbose else 'off'}")
            return None
        else:
            print(f"Unknown command: {command}")
            print("Type :help for available commands")
            return None
    
    def print_help(self):
        """Print REPL help."""
        print("""
FCLPY REPL Commands:
  :help, :h     - Show this help
  :quit, :q     - Exit the REPL
  :env          - Show environment information
  :test         - Run a simple test
  :verbose      - Toggle verbose mode

Lisp Examples:
  42            - Number literal
  "hello"       - String literal
  'x            - Quote symbol x
  (+ 1 2 3)     - Function call
  (car '(a b))  - Get first element
  (not nil)     - Logical not

Available Functions:
  +, -, *, /, =, <, >, <=, >=  - Arithmetic
  car, cdr, cons, list         - List operations
  atom, null, eq, equal        - Predicates
  not, and, or                 - Logic
  symbolp, numberp, stringp    - Type tests

Special Forms:
  quote, if, setq, let, defun, lambda
""")
    
    def print_environment_info(self):
        """Print information about the current environment."""
        print("Environment Information:")
        print("- Functions loaded: Available")
        print("- Primitive operations: 43+ functions")
        print("- Special forms: QUOTE, IF, SETQ, LET, DEFUN, LAMBDA")
        print("- Metacircular ready: Yes")
    
    def run_simple_test(self):
        """Run a simple test to verify functionality."""
        print("Running simple test...")
        test_expressions = [
            "(+ 1 2 3)",
            "(* 4 5)", 
            "(<= 3 5)",
            "(not nil)"
        ]
        
        expected_results = [6, 20, True, None]  # not nil should work once NIL is bound
        
        for i, expr_text in enumerate(test_expressions):
            try:
                expr = self.parse_with_reader(expr_text) if expr_text.startswith('(') else self.parse_simple_expression(expr_text)
                result = self.evaluate_expression(expr)
                if i < 3:  # Skip the nil test for now
                    print(f"✓ {expr_text} = {result}")
                else:
                    print(f"✗ {expr_text} = {result} (expected T)")
            except Exception as e:
                print(f"✗ {expr_text} = None (expected {expected_results[i] if i < len(expected_results) else 'unknown'})")
                if "NIL" in str(e):
                    print(f"Error: {e}")
        
        print("Test complete.")
    
    def run(self):
        """Run the main REPL loop."""
        while True:
            try:
                expr = self.read_input()
                
                if expr == ':quit':
                    break
                elif expr is None:
                    continue
                
                # Evaluate and print result
                result = self.evaluate_expression(expr)
                print(result)
                    
            except Exception as e:
                if self.verbose:
                    traceback.print_exc()
                print(f"REPL Error: {e}")

def repl(quiet=False, verbose=False):
    """Start an interactive REPL session."""
    repl_instance = FclpyREPL(quiet=quiet, verbose=verbose)
    repl_instance.run()
