"""Function binding, definition, and macro operations."""

import inspect
from fractions import Fraction
import fclpy.lisptype as lisptype
import fclpy.state as state
from fclpy.lispfunc import registry as _registry
from .core import car, cdr, _consp_internal
from .sequence_protocol import build_sequence, seq_elements


def _function_spec_to_key(spec):
    """Resolve a function-name designator to the symbol used to key it in
    the environment/registry: a plain symbol as-is, or a (SETF name) list
    to the same synthetic "(SETF NAME)" symbol DEFUN uses to store setf
    functions. Returns None if spec isn't a recognizable function name.
    """
    if isinstance(spec, lisptype.LispSymbol):
        return spec
    if _consp_internal(spec):
        head = car(spec)
        rest = cdr(spec)
        if (isinstance(head, lisptype.LispSymbol) and head.name == 'SETF'
                and _consp_internal(rest) and isinstance(car(rest), lisptype.LispSymbol)):
            return lisptype.LispSymbol(f"(SETF {car(rest).name})")
    return None


# --- Function binding and definition ---
@_registry.cl_function('FBOUNDP')
def fboundp(symbol):
    """Test if symbol has function binding.

    Returns T if the symbol has a global function definition, NIL otherwise.
    Accepts either a plain symbol or a (SETF name) function-name designator.
    """
    key = _function_spec_to_key(symbol)
    if key is None:
        if isinstance(symbol, str):
            key = lisptype.LispSymbol(symbol.upper())
        else:
            return lisptype.NIL
    symbol = key


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
def fmakunbound(name):
    """Remove `name`'s global function definition and return `name`
    (CLHS FMAKUNBOUND).

    Three things it did not do:

    * it removed the binding from `Environment.function_bindings` only, leaving
      the `_function_map` cache `find_func` reads first, so FBOUNDP went on
      answering T -- see `Environment.unbind_function`, which is now the one
      place a function binding is removed;
    * it looked only in `state.current_environment`, while DEFUN defines in the
      *global* environment at the root of the chain, so an fmakunbound
      evaluated inside any binding form found nothing to remove;
    * it returned T/NIL rather than its argument, which is what CLHS specifies
      and what `fmakunbound.1`--`.4` check with `(eqt (fmakunbound g) g)`.

    A name that is neither a symbol nor `(SETF symbol)` is a TYPE-ERROR, not a
    quiet NIL: the whole of `fmakunbound.error.*` is about that distinction.
    """
    key = _function_spec_to_key(name)
    if key is None:
        raise lisptype.LispTypeError(
            f"FMAKUNBOUND: {name!r} is not a function name",
            expected_type='(OR SYMBOL (CONS (EQL SETF) (CONS SYMBOL NULL)))',
            actual_value=name)

    env = state.current_environment
    while env is not None:
        env.unbind_function(key)
        env = getattr(env, 'parent', None)

    # The symbol's own function cell is a third place a definition can live
    # (SYMBOL-FUNCTION / SET-SYMBOL-FUNCTION write it), so it has to be
    # cleared too or `(setf (symbol-function g) #'car)` would survive --
    # `fmakunbound.1` is exactly that case.
    key.function = None
    return name


@_registry.cl_function('FDEFINITION')
def fdefinition(symbol):
    """Return the function object bound to a symbol.

    Looks up the symbol in the current environment's function bindings.
    Accepts either a plain symbol or a (SETF name) function-name
    designator. Signals error if the symbol is not fbound.
    """
    symbol = _function_spec_to_key(symbol) or lisptype.LispSymbol(str(symbol))
    env = state.current_environment
    if env is None:
        raise lisptype.LispNotImplementedError("FDEFINITION: no environment")
    func = env.find_func(symbol)
    if func is None:
        raise lisptype.LispNotImplementedError("FDEFINITION: undefined function")
    return func


@_registry.cl_function('SYMBOL-FUNCTION')
def symbol_function(*args):
    """Return the function bound to a symbol.

    Accepts variable arguments to canonicalize error handling. If called
    with the wrong number of arguments, signal a Lisp PROGRAM-ERROR.

    Resolution order:
    1. If the current environment has an fdefinition, return it.
    2. Otherwise fall back to a .function attribute if present.
    3. Else NIL (represented by Python None).
    """
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"SYMBOL-FUNCTION: wrong number of arguments (got {len(args)}, expected 1)"
        )
    symbol = args[0]
    try:
        return fdefinition(symbol)
    except Exception:
        try:
            fn = getattr(symbol, 'function', None)
        except Exception:
            fn = None
        if fn is not None:
            return fn
        from .evaluation_core import ConditionException
        cond = lisptype.UndefinedFunction(name=symbol)
        raise ConditionException(cond, recoverable=False)


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
    """Compile a function definition.
    
    Common Lisp COMPILE returns three values:
    1. The compiled function (or NIL if unable to compile)
    2. Warnings-p (T if warnings were generated)
    3. Failure-p (T if compilation failed)
    
    Usage:
    - (compile name) - compile the function named by symbol 'name'
    - (compile nil definition) - compile the lambda form 'definition'
    """
    from fclpy.lispfunc.evaluation_core import eval as eval_lisp
    import fclpy.lispenv as lispenv
    
    result_fn = None
    warnings_p = lisptype.NIL
    failure_p = lisptype.NIL
    
    try:
        # Get current environment
        env = lispenv.current_environment
        if env is None:
            env = lisptype.Environment()
            lispenv.current_environment = env
        
        if definition is not None:
            # Compile the provided definition (usually a lambda form)
            # Evaluate it to get a function object
            result_fn = eval_lisp(definition, env)
        elif isinstance(name, lisptype.LispSymbol):
            # Look up the function by name and compile it
            func = env.find_func(name)
            if func is None:
                # Try to find it as a defined function
                result_fn = eval_lisp(name, env)
            else:
                result_fn = func
        else:
            # name is NIL but no definition provided - error
            failure_p = lisptype.T
            return lisptype.MultipleValues(lisptype.NIL, warnings_p, failure_p)
    except Exception as e:
        # Compilation failed
        failure_p = lisptype.T
        import sys
        print(f"Warning: COMPILE failed: {e}", file=sys.stderr)
        warnings_p = lisptype.T
    
    # Return three values as MultipleValues
    return lisptype.MultipleValues(result_fn if result_fn is not None else lisptype.NIL, warnings_p, failure_p)


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


## `DEFINE-COMPILER-MACRO` is a special form handled by the evaluator;
## do not register it as a regular function here.
def define_compiler_macro(name, lambda_list, *body):
    """Define compiler macro (stub kept for reference)."""
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
def special_operator_p(*args):
    """Test if symbol is a special operator.

    Accepts varargs and signals a LispProgramError on wrong arity.
    """
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"SPECIAL-OPERATOR-P: wrong number of arguments (got {len(args)}, expected 1)"
        )
    symbol = args[0]
    if isinstance(symbol, lisptype.LispSymbol):
        # The canonical ANSI list of 25 special operators (CLHS 3.1.2.1.2.1).
        # Note DEFUN/DEFVAR/COND/AND/OR/WHEN/UNLESS/LAMBDA are ordinary
        # macros, not special operators, even though this interpreter
        # happens to give some of them fast-path handling in eval().
        special_ops = {
            'BLOCK', 'CATCH', 'EVAL-WHEN', 'FLET', 'FUNCTION', 'GO', 'IF',
            'LABELS', 'LET', 'LET*', 'LOAD-TIME-VALUE', 'LOCALLY', 'MACROLET',
            'MULTIPLE-VALUE-CALL', 'MULTIPLE-VALUE-PROG1', 'PROGN', 'PROGV',
            'QUOTE', 'RETURN-FROM', 'SETQ', 'SYMBOL-MACROLET', 'TAGBODY',
            'THE', 'THROW', 'UNWIND-PROTECT',
        }
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
def eval_progv(form, env):
    """Evaluate PROGV special form.

    Syntax: (PROGV symbols-form values-form body-form*)

    Evaluates symbols-form then values-form, then temporarily rebinds each
    named symbol's global value cell (the same cell SYMBOL-VALUE / BOUNDP /
    SET use) to the corresponding value, evaluates the body forms, and
    restores every touched symbol's prior value-cell state afterward, even
    if the body exits non-locally. Symbols beyond the number of supplied
    values are made explicitly unbound for the dynamic extent (per ANSI),
    even if they already had a dynamic value established elsewhere (e.g. by
    an enclosing (LET (...) (DECLARE (SPECIAL ...))) )).
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("PROGV requires symbols-form, values-form, and a body")

    symbols_form = car(args)
    values_form = car(cdr(args))
    body = cdr(cdr(args))

    symbol_list = []
    current = eval(symbols_form, env)
    while _consp_internal(current):
        symbol_list.append(car(current))
        current = cdr(current)

    value_list = []
    current = eval(values_form, env)
    while _consp_internal(current):
        value_list.append(car(current))
        current = cdr(current)

    saved = []
    for i, sym in enumerate(symbol_list):
        had_value = getattr(sym, 'value', None) is not None
        saved.append((sym, had_value, getattr(sym, 'value', None)))
        sym.value = value_list[i] if i < len(value_list) else None

    try:
        result = lisptype.NIL
        b = body
        while _consp_internal(b):
            result = eval(car(b), env)
            b = cdr(b)
        return result
    finally:
        for sym, had_value, old_value in saved:
            sym.value = old_value if had_value else None


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


# The sequence type specifiers COERCE shares with MAP/CONCATENATE/MERGE.
_SEQUENCE_TYPE_NAMES = frozenset((
    'LIST', 'CONS', 'VECTOR', 'SIMPLE-VECTOR', 'ARRAY', 'SIMPLE-ARRAY',
    'STRING', 'SIMPLE-STRING', 'BASE-STRING', 'SIMPLE-BASE-STRING',
    'BIT-VECTOR', 'SIMPLE-BIT-VECTOR', 'SEQUENCE',
))


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

    # Compound COMPLEX, e.g. `(complex short-float)`: a list, so `type_name`
    # above never matches it by name and it fell all the way through to the
    # "unsupported result type" error. The component type isn't tracked
    # separately here -- there is one Python `complex` behind every CL
    # complex subtype, the same simplification `comparison.py` documents for
    # the four CL float subtypes sharing one Python `float` -- so building it
    # is the plain COMPLEX branch below, just also reached from a list head.
    if _consp_internal(result_type):
        head = car(result_type)
        head_name = head.name.upper() if hasattr(head, 'name') else str(head).upper()
        if head_name == 'COMPLEX':
            if isinstance(object, complex):
                return object
            elif isinstance(object, (int, float, Fraction)):
                return complex(object, 0)
            else:
                raise lisptype.LispTypeError(f"COERCE: cannot convert to COMPLEX",
                                            expected_type=result_type,
                                            actual_value=object)

    # T - identity coercion (always works)
    if type_name == 'T':
        return object
    
    # LIST / VECTOR / STRING and their subtypes are *sequence* type
    # specifiers, so they are built by the one sequence protocol rather than
    # by three more branches here. The branches this replaced were a fourth
    # copy of that construction and disagreed with the others: COERCE's LIST
    # branch returned a Python list, which is a **vector**, so
    # `(coerce "abc" 'list)` answered `#("a" "b" "c")` and `(listp ...)` of it
    # was NIL (plan.md standing rule 3, Finding M).
    if type_name in _SEQUENCE_TYPE_NAMES:
        return build_sequence(result_type, seq_elements(object, 'COERCE'), 'COERCE')

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
    
    # CLHS `coerce`: "if OBJECT is already of the given type, ... coerce
    # simply returns it." That is a general rule, not a per-type branch --
    # it is what makes `(coerce 2000 'integer)` and `(coerce 1/2 'rational)`
    # work without INTEGER/RATIONAL/RATIO/REAL/NUMBER each needing their own
    # copy of "return the object", so it belongs as the fallback here
    # instead of one more `if type_name == ...` above.
    from .comparison import typep
    if typep(object, result_type) == lisptype.T:
        return object

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
    'eval_progv',
    'dynamic_extent',
    'ftype',
    'notinline',
    'inline',
    'lambda_list_keywords',
    'lambda_parameters_limit',
    'coerce',
]
