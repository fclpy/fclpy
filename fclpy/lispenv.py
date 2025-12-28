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
    except Exception:
        # Defensive: if initialization fails, continue
        pass
    
    state.functions_loaded = True
    return state.current_environment
