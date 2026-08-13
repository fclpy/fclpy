import fclpy.lisptype
import fclpy.state as state
from fclpy.cl_symbol_names import CL_SYMBOL_NAMES


# Initialize the central environment object in state if not present
if state.current_environment is None:
    state.current_environment = fclpy.lisptype.Environment()

# Backward-compatible module-level name used across the codebase
current_environment = state.current_environment


def setup_standard_environment():
    """Initialize or return the standard Lisp environment.

    All function and special form registrations come from the decorator
    registry in fclpy.lispfunc.
    """
    # If functions were already loaded, return the environment.
    # BUT: if current_environment is None, always re-initialize (test reset)
    # OR: if functions_loaded is False (test explicitly reset), re-initialize
    if state.functions_loaded and state.current_environment is not None:
        # Extra safety check: ensure key functions are actually bound
        # This catches cases where state is corrupted across test boundaries
        try:
            star_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*')
            if state.current_environment.find_func(star_sym) is not None:
                return state.current_environment
            # If multiplication operator is missing, environment is stale - reinitialize
        except Exception:
            pass
    
    if state.current_environment is None:
        state.current_environment = fclpy.lisptype.Environment()

    # Canonical CL package membership (plan.md M1 step 1 / Finding A): the 978
    # symbols CLHS requires to be external in COMMON-LISP are interned and
    # exported unconditionally, independent of whether the registry below
    # happens to provide a binding for them. The registry supplies *bindings*
    # only; it must never decide *membership*.
    for _cl_name in CL_SYMBOL_NAMES:
        _cl_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(_cl_name)
        fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(_cl_sym)

    import fclpy.lispfunc as lispfunc  # local import to avoid circulars
    try:
        from fclpy.lispfunc import registry as _registry
    except Exception:
        _registry = None

    if _registry:
        # Functions - intern into COMMON-LISP (exported) only when the name
        # is one of the 978 canonical ANSI symbols; everything else the
        # registry auto-discovered (implementation helpers, dead/duplicate
        # code paths - Finding A/L) goes into FCLPY-INTERNAL instead, so it
        # never pollutes the CL namespace real Common Lisp libraries expect
        # to be clean. Binding lookup (find_func/add_function) is name-keyed
        # (RC-1), so this does not change which functions are callable.
        for lisp_name, meta in _registry.function_registry.items():
            # meta is now a RegistryEntry, not a dict
            # Prefer the stored callable; fall back to getattr for legacy entries
            fn = getattr(meta, 'func', None)
            if fn is None:
                py_name = meta.py_name if hasattr(meta, 'py_name') else (meta.get('py_name') if isinstance(meta, dict) else None)
                if py_name:
                    fn = getattr(lispfunc, py_name, None)
            if fn:
                if lisp_name in CL_SYMBOL_NAMES:
                    sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(lisp_name)
                    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(sym)
                else:
                    sym = fclpy.lisptype.FCLPY_INTERNAL_PACKAGE.intern_symbol(lisp_name)
                if state.current_environment.find_func(sym) is None:
                    state.current_environment.add_function(sym, fn)
        # Specials - same canonical-membership routing as functions above.
        for lisp_name, meta in _registry.special_registry.items():
            # meta is now a RegistryEntry, not a dict
            # Prefer the stored callable; fall back to getattr for legacy entries
            fn = getattr(meta, 'func', None)
            if fn is None:
                py_name = meta.py_name if hasattr(meta, 'py_name') else (meta.get('py_name') if isinstance(meta, dict) else None)
                if py_name:
                    fn = getattr(lispfunc, py_name, None)
            if lisp_name in CL_SYMBOL_NAMES:
                sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(lisp_name)
                fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(sym)
            else:
                sym = fclpy.lisptype.FCLPY_INTERNAL_PACKAGE.intern_symbol(lisp_name)
            if state.current_environment.find_func(sym) is None:
                state.current_environment.add_function(sym, fn or (lambda *a: f"SPECIAL:{lisp_name}"))

    # Ensure core Lisp symbols have variable bindings in the environment
    # so that symbols like T and NIL evaluate to their Lisp values rather
    # than resolving to function bindings when no variable binding exists.
    try:
        for name, val in (('T', fclpy.lisptype.T), ('NIL', fclpy.lisptype.NIL)):
            sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(name)
            fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(sym)
            if state.current_environment.find_variable(sym) is None:
                state.current_environment.add_variable(sym, val)
    except Exception:
        # Defensive: if lisptype is not fully available yet, ignore
        pass
    
    # Initialize special variables for file loading (ANSI CL requirements)
    import os
    # *LOAD-TRUENAME* - absolute truename of file being loaded (NIL if not loading)
    load_truename_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-TRUENAME*')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(load_truename_sym)
    if state.current_environment.find_variable(load_truename_sym) is None:
        state.current_environment.add_variable(load_truename_sym, fclpy.lisptype.NIL)
        
    # *LOAD-PATHNAME* - pathname of file being loaded (NIL if not loading)
    load_pathname_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-PATHNAME*')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(load_pathname_sym)
    if state.current_environment.find_variable(load_pathname_sym) is None:
        state.current_environment.add_variable(load_pathname_sym, fclpy.lisptype.NIL)
        
    # *COMPILE-FILE-TRUENAME* - pathname of file being compiled (NIL if not compiling)
    compile_file_truename_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*COMPILE-FILE-TRUENAME*')
    if state.current_environment.find_variable(compile_file_truename_sym) is None:
        state.current_environment.add_variable(compile_file_truename_sym, fclpy.lisptype.NIL)
        
    # *COMPILE-FILE-PATHNAME* - pathname of file being compiled (NIL if not compiling)
    compile_file_pathname_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*COMPILE-FILE-PATHNAME*')
    if state.current_environment.find_variable(compile_file_pathname_sym) is None:
        state.current_environment.add_variable(compile_file_pathname_sym, fclpy.lisptype.NIL)
        
    # *DEFAULT-PATHNAME-DEFAULTS* - default pathname for pathname functions
    # Initialize to current directory as a Pathname object
    from fclpy.lispfunc.pathnames import Pathname
    default_pathname_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*DEFAULT-PATHNAME-DEFAULTS*')
    if state.current_environment.find_variable(default_pathname_sym) is None:
        cwd_pathname = Pathname(os.getcwd())
        state.current_environment.add_variable(default_pathname_sym, cwd_pathname)
        
    # *FEATURES* - list of feature keywords for #+/- conditional read
    # Standard features include: :FCLPY (our implementation), :COMMON-LISP
    features_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*FEATURES*')
    if state.current_environment.find_variable(features_sym) is None:
        # Create a basic features list with our implementation identifier
        fclpy_feature = fclpy.lisptype.intern_keyword('FCLPY')
        cl_feature = fclpy.lisptype.intern_keyword('COMMON-LISP')
        ansi_cl_feature = fclpy.lisptype.intern_keyword('ANSI-CL')
        features_list = fclpy.lisptype.lispCons(fclpy_feature, 
                        fclpy.lisptype.lispCons(cl_feature,
                        fclpy.lisptype.lispCons(ansi_cl_feature, fclpy.lisptype.NIL)))
        state.current_environment.add_variable(features_sym, features_list)
        
    # Standard I/O stream variables
    import sys
    from fclpy.lispfunc.streams import Stream
        
    # *STANDARD-INPUT* - The stream from which input is read by default
    standard_input_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*STANDARD-INPUT*')
    if state.current_environment.find_variable(standard_input_sym) is None:
        stdin_stream = Stream('*STANDARD-INPUT*', sys.stdin, 'input')
        state.current_environment.add_variable(standard_input_sym, stdin_stream)
        
    # *STANDARD-OUTPUT* - The stream to which output is sent by default
    standard_output_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*STANDARD-OUTPUT*')
    if state.current_environment.find_variable(standard_output_sym) is None:
        stdout_stream = Stream('*STANDARD-OUTPUT*', sys.stdout, 'output')
        state.current_environment.add_variable(standard_output_sym, stdout_stream)
        
    # *ERROR-OUTPUT* - The stream for error output
    error_output_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*ERROR-OUTPUT*')
    if state.current_environment.find_variable(error_output_sym) is None:
        stderr_stream = Stream('*ERROR-OUTPUT*', sys.stderr, 'output')
        state.current_environment.add_variable(error_output_sym, stderr_stream)
        
    # *TRACE-OUTPUT* - The stream for trace output
    trace_output_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*TRACE-OUTPUT*')
    if state.current_environment.find_variable(trace_output_sym) is None:
        state.current_environment.add_variable(trace_output_sym, stdout_stream)
        
    # *DEBUG-IO* - The stream for interactive debugging
    debug_io_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*DEBUG-IO*')
    if state.current_environment.find_variable(debug_io_sym) is None:
        state.current_environment.add_variable(debug_io_sym, stdout_stream)
        
    # *QUERY-IO* - The stream for user queries
    query_io_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*QUERY-IO*')
    if state.current_environment.find_variable(query_io_sym) is None:
        state.current_environment.add_variable(query_io_sym, stdout_stream)
        
    # *TERMINAL-IO* - The stream connected to the user's terminal
    terminal_io_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*TERMINAL-IO*')
    if state.current_environment.find_variable(terminal_io_sym) is None:
        state.current_environment.add_variable(terminal_io_sym, stdout_stream)
        
    # The printer control variables (CLHS Figure 22-1), with their ANSI
    # initial values. These must be real variables with real values: the
    # printer reads them from this environment, and until they were bound here
    # a reference to `*print-base*` fell through the evaluator's variable
    # lookup into the *function* registry and evaluated to a Python function
    # object. `printer.PRINTER_VARIABLES` is the single table of defaults so
    # the bootstrap and the printer cannot disagree about them.
    from fclpy.printer import PRINTER_VARIABLES
    for print_var_name, print_var_default in PRINTER_VARIABLES.items():
        print_var_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(print_var_name)
        fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(print_var_sym)
        if state.current_environment.find_variable(print_var_sym) is None:
            if print_var_default is True:
                initial = fclpy.lisptype.T
            elif print_var_default is False or print_var_default is None:
                initial = fclpy.lisptype.NIL
            elif print_var_name == '*PRINT-CASE*':
                initial = fclpy.lisptype.intern_keyword(print_var_default)
            else:
                initial = print_var_default
            state.current_environment.add_variable(print_var_sym, initial)

    # Reader/loader control variables
    # *LOAD-VERBOSE* - whether LOAD should print messages
    load_verbose_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-VERBOSE*')
    if state.current_environment.find_variable(load_verbose_sym) is None:
        state.current_environment.add_variable(load_verbose_sym, fclpy.lisptype.NIL)
        
    # *LOAD-PRINT* - whether LOAD should print values
    load_print_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-PRINT*')
    if state.current_environment.find_variable(load_print_sym) is None:
        state.current_environment.add_variable(load_print_sym, fclpy.lisptype.NIL)
        
    # *COMPILE-VERBOSE* - whether COMPILE-FILE should print messages
    compile_verbose_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*COMPILE-VERBOSE*')
    if state.current_environment.find_variable(compile_verbose_sym) is None:
        state.current_environment.add_variable(compile_verbose_sym, fclpy.lisptype.NIL)
        
    # *COMPILE-PRINT* - whether COMPILE-FILE should print values
    compile_print_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*COMPILE-PRINT*')
    if state.current_environment.find_variable(compile_print_sym) is None:
        state.current_environment.add_variable(compile_print_sym, fclpy.lisptype.NIL)
        
    # *READTABLE* - the current readtable used for reading
    from fclpy.readtable import get_current_readtable
    readtable_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*READTABLE*')
    if state.current_environment.find_variable(readtable_sym) is None:
        state.current_environment.add_variable(readtable_sym, get_current_readtable())
        
    # *PACKAGE* - current package (if not already set from state)
    package_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*PACKAGE*')
    if state.current_environment.find_variable(package_sym) is None:
        current_pkg = getattr(state, 'current_package', None) or fclpy.lisptype.COMMON_LISP_USER_PACKAGE
        state.current_environment.add_variable(package_sym, current_pkg)
        
    # *GENSYM-COUNTER* - counter used by GENSYM
    gensym_counter_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*GENSYM-COUNTER*')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(gensym_counter_sym)
    if state.current_environment.find_variable(gensym_counter_sym) is None:
        state.current_environment.add_variable(gensym_counter_sym, 0)
        # Also set symbol value for direct access
        gensym_counter_sym.value = 0
        
    # *PRINT-PPRINT-DISPATCH* - the current pretty print dispatch table
    pprint_dispatch_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*PRINT-PPRINT-DISPATCH*')
    if state.current_environment.find_variable(pprint_dispatch_sym) is None:
        # Create a simple pprint dispatch table object
        class PprintDispatchTable:
            def __repr__(self):
                return "#<PPRINT-DISPATCH-TABLE>"
        state.current_environment.add_variable(pprint_dispatch_sym, PprintDispatchTable())
        
    # === Numeric Constants (ANSI CL required) ===
    # These constants must be in COMMON_LISP_PACKAGE and exported so all
    # packages that use CL can access them (including test code).
    import math
        
    # Integer limits
    most_positive_fixnum_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('MOST-POSITIVE-FIXNUM')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(most_positive_fixnum_sym)
    if state.current_environment.find_variable(most_positive_fixnum_sym) is None:
        state.current_environment.add_variable(most_positive_fixnum_sym, 2**63 - 1)
        
    most_negative_fixnum_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('MOST-NEGATIVE-FIXNUM')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(most_negative_fixnum_sym)
    if state.current_environment.find_variable(most_negative_fixnum_sym) is None:
        state.current_environment.add_variable(most_negative_fixnum_sym, -(2**63))
        
    # PI constant
    pi_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('PI')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(pi_sym)
    if state.current_environment.find_variable(pi_sym) is None:
        state.current_environment.add_variable(pi_sym, math.pi)
        
    # Float limits - single-float (Python float = IEEE double, but we expose as single for simplicity)
    most_positive_single_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('MOST-POSITIVE-SINGLE-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(most_positive_single_float_sym)
    if state.current_environment.find_variable(most_positive_single_float_sym) is None:
        state.current_environment.add_variable(most_positive_single_float_sym, 3.4028235e+38)
        
    least_positive_single_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-POSITIVE-SINGLE-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_positive_single_float_sym)
    if state.current_environment.find_variable(least_positive_single_float_sym) is None:
        state.current_environment.add_variable(least_positive_single_float_sym, 1.4e-45)
        
    least_positive_normalized_single_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_positive_normalized_single_float_sym)
    if state.current_environment.find_variable(least_positive_normalized_single_float_sym) is None:
        state.current_environment.add_variable(least_positive_normalized_single_float_sym, 1.17549435e-38)
        
    most_negative_single_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('MOST-NEGATIVE-SINGLE-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(most_negative_single_float_sym)
    if state.current_environment.find_variable(most_negative_single_float_sym) is None:
        state.current_environment.add_variable(most_negative_single_float_sym, -3.4028235e+38)
        
    least_negative_single_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-NEGATIVE-SINGLE-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_negative_single_float_sym)
    if state.current_environment.find_variable(least_negative_single_float_sym) is None:
        state.current_environment.add_variable(least_negative_single_float_sym, -1.4e-45)
        
    least_negative_normalized_single_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_negative_normalized_single_float_sym)
    if state.current_environment.find_variable(least_negative_normalized_single_float_sym) is None:
        state.current_environment.add_variable(least_negative_normalized_single_float_sym, -1.17549435e-38)
        
    # Float limits - double-float (Python float is IEEE double)
    most_positive_double_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('MOST-POSITIVE-DOUBLE-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(most_positive_double_float_sym)
    if state.current_environment.find_variable(most_positive_double_float_sym) is None:
        state.current_environment.add_variable(most_positive_double_float_sym, 1.7976931348623157e+308)
        
    least_positive_double_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-POSITIVE-DOUBLE-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_positive_double_float_sym)
    if state.current_environment.find_variable(least_positive_double_float_sym) is None:
        state.current_environment.add_variable(least_positive_double_float_sym, 5e-324)
        
    least_positive_normalized_double_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_positive_normalized_double_float_sym)
    if state.current_environment.find_variable(least_positive_normalized_double_float_sym) is None:
        state.current_environment.add_variable(least_positive_normalized_double_float_sym, 2.2250738585072014e-308)
        
    most_negative_double_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('MOST-NEGATIVE-DOUBLE-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(most_negative_double_float_sym)
    if state.current_environment.find_variable(most_negative_double_float_sym) is None:
        state.current_environment.add_variable(most_negative_double_float_sym, -1.7976931348623157e+308)
        
    least_negative_double_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-NEGATIVE-DOUBLE-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_negative_double_float_sym)
    if state.current_environment.find_variable(least_negative_double_float_sym) is None:
        state.current_environment.add_variable(least_negative_double_float_sym, -5e-324)
        
    least_negative_normalized_double_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_negative_normalized_double_float_sym)
    if state.current_environment.find_variable(least_negative_normalized_double_float_sym) is None:
        state.current_environment.add_variable(least_negative_normalized_double_float_sym, -2.2250738585072014e-308)
        
    # Short-float (same as single in our implementation)
    most_positive_short_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('MOST-POSITIVE-SHORT-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(most_positive_short_float_sym)
    if state.current_environment.find_variable(most_positive_short_float_sym) is None:
        state.current_environment.add_variable(most_positive_short_float_sym, 3.4028235e+38)
        
    least_positive_short_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-POSITIVE-SHORT-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_positive_short_float_sym)
    if state.current_environment.find_variable(least_positive_short_float_sym) is None:
        state.current_environment.add_variable(least_positive_short_float_sym, 1.4e-45)
        
    least_positive_normalized_short_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_positive_normalized_short_float_sym)
    if state.current_environment.find_variable(least_positive_normalized_short_float_sym) is None:
        state.current_environment.add_variable(least_positive_normalized_short_float_sym, 1.17549435e-38)
        
    most_negative_short_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('MOST-NEGATIVE-SHORT-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(most_negative_short_float_sym)
    if state.current_environment.find_variable(most_negative_short_float_sym) is None:
        state.current_environment.add_variable(most_negative_short_float_sym, -3.4028235e+38)
        
    least_negative_short_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-NEGATIVE-SHORT-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_negative_short_float_sym)
    if state.current_environment.find_variable(least_negative_short_float_sym) is None:
        state.current_environment.add_variable(least_negative_short_float_sym, -1.4e-45)
        
    least_negative_normalized_short_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_negative_normalized_short_float_sym)
    if state.current_environment.find_variable(least_negative_normalized_short_float_sym) is None:
        state.current_environment.add_variable(least_negative_normalized_short_float_sym, -1.17549435e-38)
        
    # Long-float (same as double in our implementation)
    most_positive_long_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('MOST-POSITIVE-LONG-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(most_positive_long_float_sym)
    if state.current_environment.find_variable(most_positive_long_float_sym) is None:
        state.current_environment.add_variable(most_positive_long_float_sym, 1.7976931348623157e+308)
        
    least_positive_long_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-POSITIVE-LONG-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_positive_long_float_sym)
    if state.current_environment.find_variable(least_positive_long_float_sym) is None:
        state.current_environment.add_variable(least_positive_long_float_sym, 5e-324)
        
    least_positive_normalized_long_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-POSITIVE-NORMALIZED-LONG-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_positive_normalized_long_float_sym)
    if state.current_environment.find_variable(least_positive_normalized_long_float_sym) is None:
        state.current_environment.add_variable(least_positive_normalized_long_float_sym, 2.2250738585072014e-308)
        
    most_negative_long_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('MOST-NEGATIVE-LONG-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(most_negative_long_float_sym)
    if state.current_environment.find_variable(most_negative_long_float_sym) is None:
        state.current_environment.add_variable(most_negative_long_float_sym, -1.7976931348623157e+308)
        
    least_negative_long_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-NEGATIVE-LONG-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_negative_long_float_sym)
    if state.current_environment.find_variable(least_negative_long_float_sym) is None:
        state.current_environment.add_variable(least_negative_long_float_sym, -5e-324)
        
    least_negative_normalized_long_float_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(least_negative_normalized_long_float_sym)
    if state.current_environment.find_variable(least_negative_normalized_long_float_sym) is None:
        state.current_environment.add_variable(least_negative_normalized_long_float_sym, -2.2250738585072014e-308)
        
    # Float epsilon values
    single_float_epsilon_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('SINGLE-FLOAT-EPSILON')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(single_float_epsilon_sym)
    if state.current_environment.find_variable(single_float_epsilon_sym) is None:
        state.current_environment.add_variable(single_float_epsilon_sym, 1.1920929e-7)
        
    single_float_negative_epsilon_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('SINGLE-FLOAT-NEGATIVE-EPSILON')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(single_float_negative_epsilon_sym)
    if state.current_environment.find_variable(single_float_negative_epsilon_sym) is None:
        state.current_environment.add_variable(single_float_negative_epsilon_sym, 5.9604645e-8)
        
    double_float_epsilon_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('DOUBLE-FLOAT-EPSILON')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(double_float_epsilon_sym)
    if state.current_environment.find_variable(double_float_epsilon_sym) is None:
        state.current_environment.add_variable(double_float_epsilon_sym, 2.220446049250313e-16)
        
    double_float_negative_epsilon_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('DOUBLE-FLOAT-NEGATIVE-EPSILON')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(double_float_negative_epsilon_sym)
    if state.current_environment.find_variable(double_float_negative_epsilon_sym) is None:
        state.current_environment.add_variable(double_float_negative_epsilon_sym, 1.1102230246251565e-16)
        
    short_float_epsilon_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('SHORT-FLOAT-EPSILON')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(short_float_epsilon_sym)
    if state.current_environment.find_variable(short_float_epsilon_sym) is None:
        state.current_environment.add_variable(short_float_epsilon_sym, 1.1920929e-7)
        
    short_float_negative_epsilon_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('SHORT-FLOAT-NEGATIVE-EPSILON')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(short_float_negative_epsilon_sym)
    if state.current_environment.find_variable(short_float_negative_epsilon_sym) is None:
        state.current_environment.add_variable(short_float_negative_epsilon_sym, 5.9604645e-8)
        
    long_float_epsilon_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LONG-FLOAT-EPSILON')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(long_float_epsilon_sym)
    if state.current_environment.find_variable(long_float_epsilon_sym) is None:
        state.current_environment.add_variable(long_float_epsilon_sym, 2.220446049250313e-16)
        
    long_float_negative_epsilon_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LONG-FLOAT-NEGATIVE-EPSILON')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(long_float_negative_epsilon_sym)
    if state.current_environment.find_variable(long_float_negative_epsilon_sym) is None:
        state.current_environment.add_variable(long_float_negative_epsilon_sym, 1.1102230246251565e-16)
        
    # === Array and Character Limits (ANSI CL constants) ===
    # CHAR-CODE-LIMIT - upper exclusive bound for character codes
    char_code_limit_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('CHAR-CODE-LIMIT')
    if state.current_environment.find_variable(char_code_limit_sym) is None:
        state.current_environment.add_variable(char_code_limit_sym, 1114112)  # Unicode max + 1
        
    # ARRAY-DIMENSION-LIMIT - exclusive upper bound for array dimension
    array_dimension_limit_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('ARRAY-DIMENSION-LIMIT')
    if state.current_environment.find_variable(array_dimension_limit_sym) is None:
        state.current_environment.add_variable(array_dimension_limit_sym, 2**31)
        
    # ARRAY-RANK-LIMIT - exclusive upper bound for array rank
    array_rank_limit_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('ARRAY-RANK-LIMIT')
    if state.current_environment.find_variable(array_rank_limit_sym) is None:
        state.current_environment.add_variable(array_rank_limit_sym, 64)
        
    # ARRAY-TOTAL-SIZE-LIMIT - exclusive upper bound for total elements
    array_total_size_limit_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('ARRAY-TOTAL-SIZE-LIMIT')
    if state.current_environment.find_variable(array_total_size_limit_sym) is None:
        state.current_environment.add_variable(array_total_size_limit_sym, 2**31)
        
    # CALL-ARGUMENTS-LIMIT - exclusive upper bound for function arguments
    call_arguments_limit_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('CALL-ARGUMENTS-LIMIT')
    if state.current_environment.find_variable(call_arguments_limit_sym) is None:
        state.current_environment.add_variable(call_arguments_limit_sym, 2**20)
        
    # LAMBDA-PARAMETERS-LIMIT - exclusive upper bound for lambda parameters
    lambda_parameters_limit_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LAMBDA-PARAMETERS-LIMIT')
    if state.current_environment.find_variable(lambda_parameters_limit_sym) is None:
        state.current_environment.add_variable(lambda_parameters_limit_sym, 2**20)
        
    # MULTIPLE-VALUES-LIMIT - exclusive upper bound for number of multiple values
    multiple_values_limit_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('MULTIPLE-VALUES-LIMIT')
    if state.current_environment.find_variable(multiple_values_limit_sym) is None:
        state.current_environment.add_variable(multiple_values_limit_sym, 2**20)
        
    # LAMBDA-LIST-KEYWORDS - list of lambda list keyword symbols
    lambda_list_keywords_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('LAMBDA-LIST-KEYWORDS')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(lambda_list_keywords_sym)
    if state.current_environment.find_variable(lambda_list_keywords_sym) is None:
        # Create a list of lambda list keywords
        keywords = ['&ALLOW-OTHER-KEYS', '&AUX', '&BODY', '&ENVIRONMENT', '&KEY',
                   '&OPTIONAL', '&REST', '&WHOLE']
        keyword_syms = [fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(k) for k in keywords]
        # Export all lambda list keyword symbols (ANSI CL requires them to be external)
        for k in keyword_syms:
            fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(k)
        # Build list from end
        keywords_list = fclpy.lisptype.NIL
        for k in reversed(keyword_syms):
            keywords_list = fclpy.lisptype.lispCons(k, keywords_list)
        state.current_environment.add_variable(lambda_list_keywords_sym, keywords_list)
        
    # === Type Specifier Symbols (ANSI CL type system) ===
    # These symbols should evaluate to themselves - they are used as type specifiers
    # Note: We EXCLUDE type names that are also function names (LIST, CONS, etc.)
    # Those work as types when quoted: (typep x 'list), not (typep x list)
    type_symbols = [
        # Basic types (excluding LIST, CONS which are functions)
        'NULL', 'ATOM',
        # Numeric types  
        'NUMBER', 'REAL', 'RATIONAL', 'INTEGER', 'FIXNUM', 'BIGNUM',
        'RATIO', 'FLOAT', 'SHORT-FLOAT', 'SINGLE-FLOAT', 'DOUBLE-FLOAT', 'LONG-FLOAT',
        'COMPLEX', 'BIT',
        # Character types
        'CHARACTER', 'BASE-CHAR', 'STANDARD-CHAR', 'EXTENDED-CHAR',
        # Sequence types (excluding STRING, VECTOR which may be functions)
        'SEQUENCE', 'SIMPLE-STRING', 'BASE-STRING', 'SIMPLE-BASE-STRING',
        'SIMPLE-VECTOR', 'BIT-VECTOR', 'SIMPLE-BIT-VECTOR',
        'ARRAY', 'SIMPLE-ARRAY',
        # Function types
        'COMPILED-FUNCTION',
        # Other built-in types (excluding PATHNAME which is a function)
        'HASH-TABLE', 'PACKAGE', 'LOGICAL-PATHNAME',
        'STREAM', 'FILE-STREAM', 'STRING-STREAM', 'BROADCAST-STREAM',
        'CONCATENATED-STREAM', 'ECHO-STREAM', 'SYNONYM-STREAM', 'TWO-WAY-STREAM',
        'RANDOM-STATE', 'READTABLE', 'RESTART',
        # Structure/class types
        'STRUCTURE-OBJECT', 'STANDARD-OBJECT', 'CLASS', 'STRUCTURE-CLASS',
        'STANDARD-CLASS', 'BUILT-IN-CLASS', 'METHOD', 'STANDARD-METHOD',
        'METHOD-COMBINATION', 'GENERIC-FUNCTION', 'STANDARD-GENERIC-FUNCTION',
        # Condition types
        'CONDITION', 'SIMPLE-WARNING', 'STYLE-WARNING',
        'SERIOUS-CONDITION', 'SIMPLE-ERROR', 'CELL-ERROR',
        'TYPE-ERROR', 'SIMPLE-TYPE-ERROR', 'PARSE-ERROR', 'PROGRAM-ERROR',
        'CONTROL-ERROR', 'PACKAGE-ERROR', 'STREAM-ERROR', 'END-OF-FILE',
        'FILE-ERROR', 'PRINT-NOT-READABLE', 'READER-ERROR',
        'ARITHMETIC-ERROR', 'DIVISION-BY-ZERO', 'FLOATING-POINT-OVERFLOW',
        'FLOATING-POINT-UNDERFLOW', 'FLOATING-POINT-INEXACT',
        'FLOATING-POINT-INVALID-OPERATION', 'STORAGE-CONDITION',
        'UNBOUND-SLOT', 'UNBOUND-VARIABLE',
        # Boolean
        'BOOLEAN',
    ]
        
    for type_name in type_symbols:
        sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(type_name)
        fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(sym)
        # Type symbols evaluate to themselves
        if state.current_environment.find_variable(sym) is None:
            state.current_environment.add_variable(sym, sym)
            

    # Update module-level variable for backward compatibility with code that uses lispenv.current_environment
    global current_environment
    current_environment = state.current_environment
    
    state.functions_loaded = True
    return state.current_environment
