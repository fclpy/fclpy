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


## `MACROLET` is a special form handled by the evaluator; do not
## register it as a regular function here. The evaluator creates local
## macro bindings and evaluates body forms with those macros in scope.
def macrolet(definitions, *body):
    """Local macros (stub kept for reference)."""
    result = None
    for form in body:
        result = form
    return result


## `SYMBOL-MACROLET` is a special form handled by the evaluator; do not
## register it as a regular function here. The evaluator should provide
## the actual runtime semantics (or raise NotImplemented until implemented).
def symbol_macrolet(definitions, *body):
    """Symbol macros (stub kept for reference)."""
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


@_registry.cl_function('COERCE')
def coerce(object, result_type):
    """Coerce object to the specified type.
    
    Supports:
    - (COERCE sequence 'LIST) - converts sequence to list
    - (COERCE list 'VECTOR) - converts list to vector
    - (COERCE sequence 'STRING) - converts sequence of characters to string  
    - (COERCE character 'CHARACTER) - identity for characters
    - (COERCE number 'FLOAT) - converts integer to float
    - (COERCE number 'SINGLE-FLOAT) - converts to single-precision float
    - (COERCE number 'DOUBLE-FLOAT) - converts to double-precision float
    - (COERCE x 'T) - identity coercion
    - (COERCE object 'FUNCTION) - coerce to function
    """
    # Get the type name as a string for comparison
    type_name = result_type
    if isinstance(result_type, lisptype.LispSymbol):
        type_name = result_type.name
    elif hasattr(result_type, '__name__'):
        type_name = result_type.__name__
    
    # Normalize type name to uppercase string
    if isinstance(type_name, str):
        type_name = type_name.upper()
    
    # T - identity coercion (always works)
    if type_name == 'T':
        return object
    
    # LIST - convert sequence to list
    if type_name == 'LIST':
        if isinstance(object, list):
            return object
        elif isinstance(object, str):
            return list(object)
        elif isinstance(object, tuple):
            return list(object)
        elif hasattr(object, '__iter__'):
            return list(object)
        else:
            raise lisptype.LispTypeError(f"COERCE: cannot convert {type(object).__name__} to LIST",
                                        expected_type="LIST",
                                        actual_value=object)
    
    # VECTOR or SIMPLE-VECTOR - convert sequence to vector (list in Python)
    if type_name in ('VECTOR', 'SIMPLE-VECTOR'):
        if isinstance(object, (list, tuple)):
            return list(object)
        elif isinstance(object, str):
            return list(object)
        elif hasattr(object, '__iter__'):
            return list(object)
        else:
            raise lisptype.LispTypeError(f"COERCE: cannot convert {type(object).__name__} to VECTOR",
                                        expected_type="VECTOR",
                                        actual_value=object)
    
    # STRING, SIMPLE-STRING, BASE-STRING, SIMPLE-BASE-STRING - convert sequence of characters to string
    if type_name in ('STRING', 'SIMPLE-STRING', 'BASE-STRING', 'SIMPLE-BASE-STRING'):
        if isinstance(object, str):
            return object
        elif isinstance(object, (list, tuple)):
            # Sequence of characters - need to extract char from Character objects
            chars = []
            for c in object:
                if isinstance(c, lisptype.Character):
                    chars.append(c.char)
                elif isinstance(c, str) and len(c) == 1:
                    chars.append(c)
                else:
                    chars.append(str(c))
            return ''.join(chars)
        elif isinstance(object, lisptype.lispCons):
            # Lisp list of characters
            chars = []
            current = object
            while isinstance(current, lisptype.lispCons):
                c = current.car
                if isinstance(c, lisptype.Character):
                    chars.append(c.char)
                elif isinstance(c, str) and len(c) == 1:
                    chars.append(c)
                else:
                    chars.append(str(c))
                current = current.cdr
            return ''.join(chars)
        elif isinstance(object, lisptype.LispSymbol):
            return object.name
        elif hasattr(object, '__iter__'):
            return ''.join(str(c) for c in object)
        else:
            raise lisptype.LispTypeError(f"COERCE: cannot convert {type(object).__name__} to STRING",
                                        expected_type="STRING",
                                        actual_value=object)
    
    # CHARACTER - must already be a character
    if type_name == 'CHARACTER':
        if isinstance(object, str) and len(object) == 1:
            return object
        else:
            raise lisptype.LispTypeError(f"COERCE: cannot convert to CHARACTER",
                                        expected_type="CHARACTER",
                                        actual_value=object)
    
    # FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT - convert number to float
    if type_name in ('FLOAT', 'SINGLE-FLOAT', 'SHORT-FLOAT', 'DOUBLE-FLOAT', 'LONG-FLOAT'):
        if isinstance(object, (int, float)):
            return float(object)
        else:
            raise lisptype.LispTypeError(f"COERCE: cannot convert {type(object).__name__} to FLOAT",
                                        expected_type="FLOAT",
                                        actual_value=object)
    
    # COMPLEX - convert to complex number
    if type_name == 'COMPLEX':
        if isinstance(object, complex):
            return object
        elif isinstance(object, (int, float)):
            return complex(object, 0)
        else:
            raise lisptype.LispTypeError(f"COERCE: cannot convert to COMPLEX",
                                        expected_type="COMPLEX",
                                        actual_value=object)
    
    # FUNCTION - coerce to function (e.g., from symbol)
    if type_name == 'FUNCTION':
        if callable(object):
            return object
        elif isinstance(object, lisptype.LispSymbol):
            # Try to get function definition
            env = state.current_environment
            if env is not None:
                func = env.find_func(object)
                if func is not None:
                    return func
            raise lisptype.LispTypeError(f"COERCE: undefined function {object.name}",
                                        expected_type="FUNCTION",
                                        actual_value=object)
        else:
            raise lisptype.LispTypeError(f"COERCE: cannot convert to FUNCTION",
                                        expected_type="FUNCTION",
                                        actual_value=object)
    
    # If we get here, the type is not supported
    raise lisptype.LispTypeError(f"COERCE: unsupported result type {result_type}",
                                expected_type="LIST, VECTOR, STRING, CHARACTER, FLOAT, COMPLEX, FUNCTION, or T",
                                actual_value=result_type)


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
    'coerce',
]
