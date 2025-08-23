import fclpy.lisptype
import fclpy.state as state


# Initialize the central environment object in state if not present
if state.current_environment is None:
    state.current_environment = fclpy.lisptype.Environment()

# Backward-compatible module-level name used across the codebase
current_environment = state.current_environment


def setup_standard_environment():
    """Initialize or return the standard Lisp environment.

    All function and special form registrations now come from the decorator
    registry in fclpy.lispfunc. The legacy function_mappings dict has been
    retired (left empty) but the hook remains for backward compatibility.
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
            py_name = meta.get('py_name') if isinstance(meta, dict) else None
            if not py_name:
                continue
            fn = getattr(lispfunc, py_name, None)
            if fn:
                sym = fclpy.lisptype.LispSymbol(lisp_name)
                if state.current_environment.find_func(sym) is None:
                    state.current_environment.add_function(sym, fn)
        # Specials
        for lisp_name, meta in _registry.special_registry.items():
            py_name = meta.get('py_name') if isinstance(meta, dict) else None
            fn = getattr(lispfunc, py_name, None) if py_name else None
            sym = fclpy.lisptype.LispSymbol(lisp_name)
            if state.current_environment.find_func(sym) is None:
                state.current_environment.add_function(sym, fn or (lambda *a: f"SPECIAL:{lisp_name}"))

    # Empty legacy mapping (interface placeholder retained for compatibility)
    function_mappings = {}
    if function_mappings:
        for lisp_name, python_func in function_mappings.items():
            sym = fclpy.lisptype.LispSymbol(lisp_name)
            state.current_environment.add_function(sym, python_func)

    state.functions_loaded = True
    return state.current_environment
