"""Function binding, definition, and macro operations."""

import inspect
import fclpy.lisptype as lisptype
import fclpy.state as state
from fclpy.lispfunc import registry as _registry


# --- Function binding and definition ---
@_registry.cl_function('FBOUNDP')
def fboundp(symbol):
    """Test if symbol has function binding.
    
    Returns T if the symbol has a global function definition, NIL otherwise.
    """
    if not isinstance(symbol, lisptype.LispSymbol):
        # Try to convert to symbol
        if isinstance(symbol, str):
            symbol = lisptype.LispSymbol(symbol.upper())
        else:
            return lisptype.NIL
    
    # Check in current environment's function bindings
    env = state.current_environment
    if env is not None:
        func = env.find_func(symbol)
        if func is not None:
            return lisptype.T
    
    # Check in function registry
    try:
        py_name = _registry.get_function_py_name(symbol.name)
        if py_name:
            return lisptype.T
    except Exception:
        pass
    
    return lisptype.NIL


@_registry.cl_function('FMAKUNBOUND')
def fmakunbound(symbol):
    """Remove function binding for symbol.

    This walks the current global environment's function bindings and
    removes any entry whose symbol name matches the provided symbol.
    Returns T if a binding was removed, otherwise NIL.
    """
    if not isinstance(symbol, lisptype.LispSymbol):
        symbol = lisptype.LispSymbol(str(symbol))
    env = state.current_environment
    if env is None:
        return lisptype.NIL
    prev = None
    node = env.function_bindings
    removed = False
    while node is not None:
        if node.symbol.name == symbol.name:
            if prev is None:
                env.function_bindings = node.next
            else:
                prev.next = node.next
            removed = True
            break
        prev = node
        node = node.next
    return lisptype.T if removed else lisptype.NIL


@_registry.cl_function('FDEFINITION')
def fdefinition(symbol):
    """Return the function object bound to a symbol.

    Looks up the symbol in the current environment's function bindings.
    Signals error if the symbol is not fbound.
    """
    if not isinstance(symbol, lisptype.LispSymbol):
        symbol = lisptype.LispSymbol(str(symbol))
    env = state.current_environment
    if env is None:
        raise lisptype.LispNotImplementedError("FDEFINITION: no environment")
    func = env.find_func(symbol)
    if func is None:
        raise lisptype.LispNotImplementedError("FDEFINITION: undefined function")
    return func


@_registry.cl_function('SYMBOL-FUNCTION')
def symbol_function(symbol):
    """Return the function bound to a symbol.

    Resolution order:
    1. If the current environment has an fdefinition, return it.
    2. Otherwise fall back to a .function attribute if present.
    3. Else NIL (represented by Python None).
    """
    try:
        return fdefinition(symbol)
    except Exception:
        return getattr(symbol, 'function', None)


@_registry.cl_function('FUNCTIONP')
def functionp(object):
    """Test if object is a function."""
    return lisptype.lisp_bool(callable(object))


@_registry.cl_function('COMPILED-FUNCTION-P')
def compiled_function_p(object):
    """Test if object is compiled function."""
    return lisptype.lisp_bool(callable(object) and hasattr(object, '__code__'))


@_registry.cl_function('COMPILE')
def compile_fn(name, definition=None):
    """Compile function (stub)."""
    return name


# --- Macro operations ---
@_registry.cl_function('MACRO-FUNCTION')
def macro_function(symbol, environment=None):
    """Get macro function for a symbol."""
    if environment is None:
        from fclpy.lispenv import current_environment as _cur_env
        environment = _cur_env
    func = environment.find_func(symbol)
    if callable(func) and getattr(func, '__is_macro__', False):
        return func
    return None


@_registry.cl_function('COMPILER-MACRO-FUNCTION')
def compiler_macro_function(name, environment=None):
    """Get compiler macro function."""
    return None


@_registry.cl_function('MACROLET')
def macrolet(definitions, *body):
    """Local macros (stub)."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('SYMBOL-MACROLET')
def symbol_macrolet(definitions, *body):
    """Symbol macros (stub)."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('DEFINE-COMPILER-MACRO')
def define_compiler_macro(name, lambda_list, *body):
    """Define compiler macro (stub)."""
    return name


# --- Function introspection ---
@_registry.cl_function('FUNCTION-KEYWORDS')
def function_keywords(function):
    """Return two values: FUNCTION-KEYWORDS and &ALLOW-OTHER-KEYS status.

    For Python callables inspects the signature and collects keyword
    parameters (those with defaults) and &ALLOW-OTHER-KEYS status.
    Returns (keywords, allow_other_keys_p).
    """
    if not callable(function):
        return [], lisptype.NIL
    try:
        sig = inspect.signature(function)
        keywords = []
        allow_other = lisptype.NIL
        for p in sig.parameters.values():
            if getattr(p, 'default', None) not in (None, getattr(inspect, '_empty', None)):
                keywords.append(p.name.upper())
            name_kind = str(getattr(p, 'kind', ''))
            if 'VAR_KEYWORD' in name_kind:
                allow_other = lisptype.T
        return keywords, allow_other
    except Exception:
        return [], lisptype.NIL


@_registry.cl_function('FUNCTION-LAMBDA-EXPRESSION')
def function_lambda_expression(function):
    """Return three values: lambda-list, source, closure-p.

    1. A simplified lambda list (list of parameter names as symbols)
    2. The body form source (string of source if available, else NIL)
    3. A closure-p flag (T if the function appears to close over free vars)
    """
    if not callable(function):
        return [], lisptype.NIL, lisptype.NIL
    params = []
    closure_p = lisptype.NIL
    try:
        sig = inspect.signature(function)
        for p in sig.parameters.values():
            name_kind = str(getattr(p, 'kind', ''))
            if 'POSITIONAL' in name_kind or 'KEYWORD_ONLY' in name_kind:
                params.append(lisptype.LispSymbol(p.name.upper()))
            elif 'VAR_POSITIONAL' in name_kind:
                params.append(lisptype.LispSymbol('&REST'))
            elif 'VAR_KEYWORD' in name_kind:
                params.append(lisptype.LispSymbol('&KEY'))
        if getattr(function, '__closure__', None):
            closure_p = lisptype.T if function.__closure__ else lisptype.NIL
        try:
            src = inspect.getsource(function)
        except Exception:
            src = None
        return params, (src or lisptype.NIL), closure_p
    except Exception:
        return [], lisptype.NIL, lisptype.NIL


# --- Special operators ---
@_registry.cl_function('SPECIAL-OPERATOR-P')
def special_operator_p(symbol):
    """Test if symbol is a special operator."""
    if isinstance(symbol, lisptype.LispSymbol):
        special_ops = {'QUOTE', 'IF', 'LAMBDA', 'SETQ', 'LET', 'DEFUN', 'DEFVAR',
                      'PROGN', 'COND', 'AND', 'OR', 'WHEN', 'UNLESS', 'PROGV'}
        return lisptype.lisp_bool(symbol.name in special_ops)
    return lisptype.lisp_bool(False)


# --- Special forms ---
@_registry.cl_special('EVAL-WHEN')
def eval_when(situations, *forms):
    """Evaluate when situations apply."""
    result = None
    for form in forms:
        result = form
    return result


def locally(*body):
    """Execute body in local scope."""
    raise lisptype.LispNotImplementedError("LOCALLY")


@_registry.cl_special('PROGV')
def progv(symbols, values, *body):
    """Special form PROGV (stub)."""
    raise lisptype.LispNotImplementedError("PROGV")


# --- Declaration-like functions ---
@_registry.cl_function('DYNAMIC-EXTENT')
def dynamic_extent(*args):
    """Dynamic extent declaration (stub)."""
    return None


@_registry.cl_function('FTYPE')
def ftype(*args):
    """Function type declaration (stub)."""
    return None


@_registry.cl_function('NOTINLINE')
def notinline(*args):
    """NOTINLINE declaration (stub)."""
    return None


@_registry.cl_function('INLINE')
def inline(*args):
    """INLINE declaration (stub)."""
    return None


@_registry.cl_function('LAMBDA-LIST-KEYWORDS')
def lambda_list_keywords():
    """Return list of lambda list keywords."""
    return [lisptype.LispSymbol(x) for x in ['&OPTIONAL','&REST','&KEY','&WHOLE','&ALLOW-OTHER-KEYS','&AUX']]


@_registry.cl_function('LAMBDA-PARAMETERS-LIMIT')
def lambda_parameters_limit():
    """Return limit on lambda parameters."""
    return 64


__all__ = [
    'fboundp',
    'fmakunbound',
    'fdefinition',
    'symbol_function',
    'functionp',
    'compiled_function_p',
    'compile_fn',
    'macro_function',
    'compiler_macro_function',
    'macrolet',
    'symbol_macrolet',
    'define_compiler_macro',
    'function_keywords',
    'function_lambda_expression',
    'special_operator_p',
    'eval_when',
    'locally',
    'progv',
    'dynamic_extent',
    'ftype',
    'notinline',
    'inline',
    'lambda_list_keywords',
    'lambda_parameters_limit',
]
