import fclpy.lisptype
import fclpy.state as state


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
    if state.functions_loaded:
        return state.current_environment
    if state.current_environment is None:
        state.current_environment = fclpy.lisptype.Environment()

    import fclpy.lispfunc as lispfunc  # local import to avoid circulars
    try:
        from fclpy.lispfunc import registry as _registry
    except Exception:
        _registry = None

    if _registry:
        # Functions
        for lisp_name, meta in _registry.function_registry.items():
            # meta is now a RegistryEntry, not a dict
            py_name = meta.py_name if hasattr(meta, 'py_name') else (meta.get('py_name') if isinstance(meta, dict) else None)
            if not py_name:
                continue
            fn = getattr(lispfunc, py_name, None)
            if fn:
                # Intern the symbol into the COMMON-LISP-USER package so
                # environment bindings and reader-produced symbols share identity.
                sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol(lisp_name)
                if state.current_environment.find_func(sym) is None:
                    state.current_environment.add_function(sym, fn)
        # Specials
        for lisp_name, meta in _registry.special_registry.items():
            # meta is now a RegistryEntry, not a dict
            py_name = meta.py_name if hasattr(meta, 'py_name') else (meta.get('py_name') if isinstance(meta, dict) else None)
            fn = getattr(lispfunc, py_name, None) if py_name else None
            sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol(lisp_name)
            if state.current_environment.find_func(sym) is None:
                state.current_environment.add_function(sym, fn or (lambda *a: f"SPECIAL:{lisp_name}"))

    # Ensure core Lisp symbols have variable bindings in the environment
    # so that symbols like T and NIL evaluate to their Lisp values rather
    # than resolving to function bindings when no variable binding exists.
    try:
        for name, val in (('T', fclpy.lisptype.T), ('NIL', fclpy.lisptype.NIL)):
            sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol(name)
            if state.current_environment.find_variable(sym) is None:
                state.current_environment.add_variable(sym, val)
    except Exception:
        # Defensive: if lisptype is not fully available yet, ignore
        pass
    
    # Initialize special variables for file loading (ANSI CL requirements)
    try:
        import os
        # *LOAD-TRUENAME* - absolute truename of file being loaded (NIL if not loading)
        load_truename_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*LOAD-TRUENAME*')
        if state.current_environment.find_variable(load_truename_sym) is None:
            state.current_environment.add_variable(load_truename_sym, fclpy.lisptype.NIL)
        
        # *LOAD-PATHNAME* - pathname of file being loaded (NIL if not loading)
        load_pathname_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*LOAD-PATHNAME*')
        if state.current_environment.find_variable(load_pathname_sym) is None:
            state.current_environment.add_variable(load_pathname_sym, fclpy.lisptype.NIL)
        
        # *COMPILE-FILE-TRUENAME* - pathname of file being compiled (NIL if not compiling)
        compile_file_truename_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*COMPILE-FILE-TRUENAME*')
        if state.current_environment.find_variable(compile_file_truename_sym) is None:
            state.current_environment.add_variable(compile_file_truename_sym, fclpy.lisptype.NIL)
        
        # *COMPILE-FILE-PATHNAME* - pathname of file being compiled (NIL if not compiling)
        compile_file_pathname_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*COMPILE-FILE-PATHNAME*')
        if state.current_environment.find_variable(compile_file_pathname_sym) is None:
            state.current_environment.add_variable(compile_file_pathname_sym, fclpy.lisptype.NIL)
        
        # *DEFAULT-PATHNAME-DEFAULTS* - default pathname for pathname functions
        # Initialize to current directory as a Pathname object
        from fclpy.lispfunc.pathnames import Pathname
        default_pathname_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*DEFAULT-PATHNAME-DEFAULTS*')
        if state.current_environment.find_variable(default_pathname_sym) is None:
            cwd_pathname = Pathname(os.getcwd())
            state.current_environment.add_variable(default_pathname_sym, cwd_pathname)
        
        # *FEATURES* - list of feature keywords for #+/- conditional read
        # Standard features include: :FCLPY (our implementation), :COMMON-LISP
        features_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*FEATURES*')
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
        standard_input_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*STANDARD-INPUT*')
        if state.current_environment.find_variable(standard_input_sym) is None:
            stdin_stream = Stream('*STANDARD-INPUT*', sys.stdin, 'input')
            state.current_environment.add_variable(standard_input_sym, stdin_stream)
        
        # *STANDARD-OUTPUT* - The stream to which output is sent by default
        standard_output_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*STANDARD-OUTPUT*')
        if state.current_environment.find_variable(standard_output_sym) is None:
            stdout_stream = Stream('*STANDARD-OUTPUT*', sys.stdout, 'output')
            state.current_environment.add_variable(standard_output_sym, stdout_stream)
        
        # *ERROR-OUTPUT* - The stream for error output
        error_output_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*ERROR-OUTPUT*')
        if state.current_environment.find_variable(error_output_sym) is None:
            stderr_stream = Stream('*ERROR-OUTPUT*', sys.stderr, 'output')
            state.current_environment.add_variable(error_output_sym, stderr_stream)
        
        # *TRACE-OUTPUT* - The stream for trace output
        trace_output_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*TRACE-OUTPUT*')
        if state.current_environment.find_variable(trace_output_sym) is None:
            state.current_environment.add_variable(trace_output_sym, stdout_stream)
        
        # *DEBUG-IO* - The stream for interactive debugging
        debug_io_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*DEBUG-IO*')
        if state.current_environment.find_variable(debug_io_sym) is None:
            state.current_environment.add_variable(debug_io_sym, stdout_stream)
        
        # *QUERY-IO* - The stream for user queries
        query_io_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*QUERY-IO*')
        if state.current_environment.find_variable(query_io_sym) is None:
            state.current_environment.add_variable(query_io_sym, stdout_stream)
        
        # *TERMINAL-IO* - The stream connected to the user's terminal
        terminal_io_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*TERMINAL-IO*')
        if state.current_environment.find_variable(terminal_io_sym) is None:
            state.current_environment.add_variable(terminal_io_sym, stdout_stream)
        
        # Printer/reader control variables
        # *LOAD-VERBOSE* - whether LOAD should print messages
        load_verbose_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*LOAD-VERBOSE*')
        if state.current_environment.find_variable(load_verbose_sym) is None:
            state.current_environment.add_variable(load_verbose_sym, fclpy.lisptype.NIL)
        
        # *LOAD-PRINT* - whether LOAD should print values
        load_print_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*LOAD-PRINT*')
        if state.current_environment.find_variable(load_print_sym) is None:
            state.current_environment.add_variable(load_print_sym, fclpy.lisptype.NIL)
        
        # *COMPILE-VERBOSE* - whether COMPILE-FILE should print messages
        compile_verbose_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*COMPILE-VERBOSE*')
        if state.current_environment.find_variable(compile_verbose_sym) is None:
            state.current_environment.add_variable(compile_verbose_sym, fclpy.lisptype.NIL)
        
        # *COMPILE-PRINT* - whether COMPILE-FILE should print values
        compile_print_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*COMPILE-PRINT*')
        if state.current_environment.find_variable(compile_print_sym) is None:
            state.current_environment.add_variable(compile_print_sym, fclpy.lisptype.NIL)
        
        # *READTABLE* - the current readtable used for reading
        from fclpy.readtable import get_current_readtable
        readtable_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*READTABLE*')
        if state.current_environment.find_variable(readtable_sym) is None:
            state.current_environment.add_variable(readtable_sym, get_current_readtable())
        
        # *PACKAGE* - current package (if not already set from state)
        package_sym = fclpy.lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*PACKAGE*')
        if state.current_environment.find_variable(package_sym) is None:
            current_pkg = getattr(state, 'current_package', None) or fclpy.lisptype.COMMON_LISP_USER_PACKAGE
            state.current_environment.add_variable(package_sym, current_pkg)
            
    except Exception:
        # Defensive: if initialization fails, continue
        pass
    
    state.functions_loaded = True
    return state.current_environment
