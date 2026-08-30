"""Function binding, definition, and macro operations."""

import inspect
from fractions import Fraction
import fclpy.lisptype as lisptype
import fclpy.state as state
from fclpy.lispfunc import registry as _registry
from .core import car, cdr, cadr, cons, _consp_internal
from .sequence_protocol import build_sequence, seq_elements, list_elements as _list_elements


def _function_spec_to_key(spec):
    """Resolve a function-name designator to the symbol used to key it in
    the environment/registry: a plain symbol as-is, or a (SETF name) list
    to the same synthetic "(SETF NAME)" symbol DEFUN uses to store setf
    functions. Returns None if spec isn't a recognizable function name.
    
    Valid forms are:
    - A symbol (any symbol)
    - A proper list (SETF symbol) with exactly two elements
    
    Invalid forms (return None):
    - Non-symbol, non-list values
    - Improper lists like (SETF symbol . extra)
    - Lists with wrong head or structure like (setf) or (SETF symbol extra)
    """
    if isinstance(spec, lisptype.LispSymbol):
        return spec
    if _consp_internal(spec):
        head = car(spec)
        rest = cdr(spec)
        if isinstance(head, lisptype.LispSymbol) and head.name == 'SETF':
            # rest must be (symbol) - a cons cell with a symbol as head and NIL as tail
            if _consp_internal(rest):
                name_sym = car(rest)
                name_rest = cdr(rest)
                # Check that name is a symbol and rest is NIL (proper list with 2 elements)
                if isinstance(name_sym, lisptype.LispSymbol) and name_rest is lisptype.NIL:
                    return lisptype.LispSymbol(f"(SETF {name_sym.name})")
    return None


# --- Function binding and definition ---
@_registry.cl_function('FBOUNDP')
def fboundp(symbol):
    """Test if symbol has function binding.

    Returns T if the symbol has a global function definition, NIL otherwise.
    Accepts either a plain symbol or a (SETF name) function-name designator.
    Signals TYPE-ERROR if the argument is not a valid function name.
    """
    key = _function_spec_to_key(symbol)
    if key is None:
        # Invalid function name - must be a symbol or (SETF symbol)
        raise lisptype.LispTypeError(
            f"FBOUNDP: {symbol!r} is not a function name",
            expected_type='(OR SYMBOL (CONS (EQL SETF) (CONS SYMBOL NULL)))',
            actual_value=symbol)

    # Check in current environment's function bindings
    env = state.current_environment
    while env is not None:
        func = env.find_func(key)
        if func is not None:
            return lisptype.T
        env = getattr(env, 'parent', None)

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
    designator. Signals TYPE-ERROR if the argument is not a valid function name,
    and UNDEFINED-FUNCTION if the symbol is not fbound.
    """
    key = _function_spec_to_key(symbol)
    if key is None:
        # Invalid function name - must be a symbol or (SETF symbol)
        raise lisptype.LispTypeError(
            f"FDEFINITION: {symbol!r} is not a function name",
            expected_type='(OR SYMBOL (CONS (EQL SETF) (CONS SYMBOL NULL)))',
            actual_value=symbol)

    env = state.current_environment
    while env is not None:
        func = env.find_func(key)
        if func is not None:
            return func
        env = getattr(env, 'parent', None)

    # Symbol is not fbound - signal UNDEFINED-FUNCTION
    from .evaluation_core import ConditionException
    cond = lisptype.UndefinedFunction(name=symbol)
    raise ConditionException(cond, recoverable=False)


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
            # Compile the provided definition (usually a lambda form).
            # CLHS 3.2.2.1: the compiler expands compiler-macro calls in
            # the definition before producing the function -- the macro
            # function runs *now*, at compile time, and the compiled code
            # evaluates its expansion (define-compiler-macro.1/.2: the
            # expansion is embedded and *x* stays NIL at runtime).
            definition = _expand_compiler_macro_calls(definition)
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
    """COMPILER-MACRO-FUNCTION (CLHS 3.2.2.1): the compiler macro function
    for `name` -- a symbol or a ``(setf symbol)`` function name -- or NIL.

    The Python ``None`` this used to return was both the wrong *answer*
    (NIL is the specified not-present answer) and a Python object surfacing
    as a Lisp value (standing rule 2)."""
    entry = _compiler_macro_registry.get(_ccm_key(name))
    if entry is not None:
        return entry
    return lisptype.NIL


@_registry.cl_function('(SETF COMPILER-MACRO-FUNCTION)')
def set_compiler_macro_function(fn, name):
    """(SETF COMPILER-MACRO-FUNCTION) (CLHS 5.1.3)."""
    key = _ccm_key(name)
    if fn is lisptype.NIL or fn is None:
        _compiler_macro_registry.pop(key, None)
    else:
        _compiler_macro_registry[key] = fn
    return fn


# The compiler macro function registry, keyed by canonical name string --
# 'SYMBOL' or '(SETF SYMBOL)'. One home, shared by DEFINE-COMPILER-MACRO,
# COMPILER-MACRO-FUNCTION and COMPILE's expansion pass.
_compiler_macro_registry = {}


def _ccm_key(name):
    """Canonical registry key for a compiler-macro function name."""
    if _consp_internal(name):
        # A (setf symbol) function name.
        parts = []
        cur = name
        while _consp_internal(cur):
            part = car(cur)
            parts.append(part.name.upper() if isinstance(part, lisptype.LispSymbol)
                         else str(part).upper())
            cur = cdr(cur)
        return '(' + ' '.join(parts) + ')'
    if isinstance(name, lisptype.LispSymbol):
        return name.name.upper()
    return str(name).upper()


@_registry.cl_function('%DEFINE-COMPILER-MACRO')
def define_compiler_macro(name, lambda_list, *body):
    """The runtime half of DEFINE-COMPILER-MACRO (CLHS 3.2.2.1): build the
    macro function from the macro lambda list (&whole/&environment included,
    via the one macro binder) and install it as `name`'s compiler macro
    function. `name` is a symbol or a ``(setf symbol)`` form."""
    import fclpy.state as state
    from .evaluation_special_forms import _create_macro_function

    if _consp_internal(name):
        parts = []
        cur = name
        while _consp_internal(cur):
            parts.append(car(cur))
            cur = cdr(cur)
        debug_name = parts[-1] if parts else lisptype.LispSymbol('SETF')
    else:
        debug_name = name

    fn = _create_macro_function(debug_name, lambda_list, _cons_from_list(body), env=state.current_environment)
    _compiler_macro_registry[_ccm_key(name)] = fn
    # A leading docstring is this compiler macro's documentation
    # (CLHS 3.2.2.1), stored separately from the function documentation a
    # DEFUN on the same name writes (define-compiler-macro.5/.6).
    if body and isinstance(body[0], (str, lisptype.LispString)) \
            and isinstance(name, lisptype.LispSymbol):
        name.plist['COMPILER-MACRO-DOCUMENTATION'] = str(body[0])
    return name


def _cons_from_list(items):
    """A Lisp list from a Python sequence -- the one shape this module's
    form walkers and definers build."""
    result = lisptype.NIL
    for item in reversed(items):
        result = lisptype.lispCons(item, result)
    return result


def _expand_compiler_macro_calls(form, notinline=()):
    """Walk `form` replacing calls whose operator (or ``(setf op)`` place
    function) has a compiler macro function with that macro's expansion --
    what COMPILE must do before evaluating a lambda definition (CLHS
    3.2.2.1's file-compiler contract, applied to fclpy's compile-as-eval).

    A compiler macro declines by returning its own &whole form; the walk
    keeps the original call then. ``(declare (notinline f))`` suppresses
    expansion for `f` (define-compiler-macro.7).
    """
    if not _consp_internal(form):
        return form

    head = car(form)
    if isinstance(head, lisptype.LispSymbol):
        head_name = head.name.upper()
        if head_name == 'QUOTE':
            return form
        if head_name == 'DECLARE':
            return form

        new_notinline = set(notinline)
        # Declarations in the body of a lambda/let-ish form extend the
        # suppressed set for the remainder of this walk.
        if head_name in ('LAMBDA',):
            body_items = []
            cur = cdr(form)
            while _consp_internal(cur):
                item = car(cur)
                if _consp_internal(item) and isinstance(car(item), lisptype.LispSymbol) \
                        and car(item).name.upper() == 'DECLARE':
                    for spec in _list_elements(cdr(item)):
                        if _consp_internal(spec) and not _consp_internal(car(spec)) \
                                and isinstance(car(spec), lisptype.LispSymbol) \
                                and car(spec).name.upper() == 'NOTINLINE':
                            for fname in _list_elements(cdr(spec)):
                                new_notinline.add(_ccm_key(fname))
                body_items.append(item)
                cur = cdr(cur)
            return lisptype.lispCons(head, _cons_from_list(
                [_expand_compiler_macro_calls(item, new_notinline) for item in body_items]))

        if head_name not in new_notinline and head_name not in _SPECIAL_OPERATORS_NO_CCM:
            entry = _compiler_macro_registry.get(head_name)
            if entry is not None:
                expansion = _call_ccm(entry, form, [cdr(form)])
                if expansion is not None and not _same_form_p(expansion, form):
                    return _expand_compiler_macro_calls(expansion, notinline)

        if head_name == 'SETF' and not _consp_internal(head):
            # A compiler macro on a (setf f) function name applies to the
            # SETF form `(setf (f args...) newval)` (define-compiler-
            # macro.4). Only the single-place form is handled here; the
            # multi-place shape has no test coverage and is left alone.
            place = car(cdr(form))
            value_form = car(cdr(cdr(form)))
            tail_ok = _consp_internal(cdr(cdr(form))) and cdr(cdr(cdr(form))) is lisptype.NIL
            if (_consp_internal(place) and isinstance(car(place), lisptype.LispSymbol)
                    and tail_ok):
                fn_name = cons(lisptype.LispSymbol('SETF'),
                               _cons_from_list([car(place)]))
                key = _ccm_key(fn_name)
                entry = _compiler_macro_registry.get(key)
                if entry is not None and key not in {n.strip('()') for n in notinline}:
                    # The compiler macro's lambda list binds (newval args...)
                    # -- the newvalue form first, then the place's arguments
                    # -- and its &whole is the whole SETF form.
                    raw_args = cons(value_form, cdr(place))
                    expansion = _call_ccm(entry, form, [raw_args])
                    if expansion is not None and not _same_form_p(expansion, form):
                        return _expand_compiler_macro_calls(expansion, notinline)

    if _consp_internal(head):
        # A (setf f) place call inside a SETF: (SETF (F args...) newval).
        head_name = head.car.name.upper() if isinstance(head.car, lisptype.LispSymbol) else ''
        if head_name == 'SETF':
            place = cadr(form)
            if _consp_internal(place) and isinstance(car(place), lisptype.LispSymbol):
                fn_name = cons(lisptype.LispSymbol('SETF'),
                               _cons_from_list([car(place)]))
                key = _ccm_key(fn_name)
                entry = _compiler_macro_registry.get(key)
                if entry is not None and key.strip('()') not in {n.strip('()') for n in notinline}:
                    # The compiler macro for a (setf f) function name is
                    # called with the whole SETF form; its lambda list binds
                    # (newval args...).
                    expansion = _call_ccm(entry, form, [cdr(form)])
                    if expansion is not None and not _same_form_p(expansion, form):
                        return _expand_compiler_macro_calls(expansion, notinline)

    new_head = _expand_compiler_macro_calls(head, notinline) if _consp_internal(head) else head
    new_tail = _expand_compiler_macro_calls(cdr(form), notinline)
    return lisptype.lispCons(new_head, new_tail)


# Operators whose call sites never take a compiler macro (they are special
# forms; CLHS 3.2.2.1's "not shadowed" clause covers the rest).
_SPECIAL_OPERATORS_NO_CCM = {
    'QUOTE', 'IF', 'SETQ', 'SETF', 'PSETF', 'PROGN', 'LET', 'LET*', 'COND',
    'WHEN', 'UNLESS', 'AND', 'OR', 'BLOCK', 'RETURN-FROM', 'CATCH', 'THROW',
    'TAGBODY', 'GO', 'UNWIND-PROTECT', 'FUNCTION', 'MACROLET', 'FLET',
    'LABELS', 'DECLAIM', 'DEFMACRO', 'DEFUN', 'DEFVAR', 'DEFPARAMETER',
    'PROGV', 'LOCALLY', 'EVAL-WHEN', 'THE', 'LOAD-TIME-VALUE',
}


def _call_ccm(fn, whole_form, raw_args_lists):
    """Call a compiler macro function the way the evaluator calls macro
    functions: the &whole form first when the binder wants it, the call
    arguments, then the environment when the binder wants it. The caller
    passes the *unevaluated* argument lists; `raw_args_lists` here is a
    list whose single element is the arg cons."""
    args = []
    cur = raw_args_lists[0] if raw_args_lists else lisptype.NIL
    while _consp_internal(cur):
        args.append(car(cur))
        cur = cdr(cur)
    call_args = []
    if getattr(fn, '__expects_whole__', False):
        call_args.append(whole_form)
    call_args.extend(args)
    if getattr(fn, '__expects_environment__', False):
        import fclpy.state as state
        call_args.append(state.current_environment)
    try:
        return fn(*call_args)
    except TypeError:
        return fn(*args)


def _same_form_p(a, b):
    """EQ for atoms/symbols, structural identity for conses -- enough to
    detect a compiler macro declining by returning its own &whole form."""
    if a is b:
        return True
    if _consp_internal(a) and _consp_internal(b):
        return _same_form_p(car(a), car(b)) and _same_form_p(cdr(a), cdr(b))
    return False


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


## `DEFINE-COMPILER-MACRO` the *macro* is registered above, beside
## COMPILER-MACRO-FUNCTION; its expander (in evaluation_special_forms.py)
## quotes the definition into a call to %DEFINE-COMPILER-MACRO, the runtime
## half registered above. The stub that stood here returned the name and
## registered nothing, so every compiler-macro test failed on a missing
## facility rather than a wrong answer.


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


# The sequence type specifiers COERCE supports for result-type.
# CLHS 4.4.68 specifies that COERCE result-type can be a sequence type or class.
# For sequences, this includes LIST (but not CONS - CONS is a type, not a
# sequence result-type), VECTOR, ARRAY, STRING, and their subtypes.
_SEQUENCE_TYPE_NAMES = frozenset((
    'LIST', 'VECTOR', 'SIMPLE-VECTOR', 'ARRAY', 'SIMPLE-ARRAY',
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
    - (COERCE object class-object) - identity if object is of that class
    """
    from .comparison import typep
    from fclpy import classes

    # Get the type name as a string for comparison
    type_name = result_type
    if isinstance(result_type, lisptype.LispSymbol):
        type_name = result_type.name
    elif isinstance(result_type, classes.LispClass):
        # Handle class objects: check if object is already an instance of this class
        # Fall through to the final typep check at the end
        type_name = None
    elif hasattr(result_type, '__name__'):
        type_name = result_type.__name__

    # Normalize type name to uppercase string
    if isinstance(type_name, str):
        type_name = type_name.upper()

    # Compound type specifiers like (COMPLEX ...), (VECTOR ...), (ARRAY ...) etc.
    # These are lists, so type_name above is None and we need to extract the head.
    if _consp_internal(result_type):
        head = car(result_type)
        head_name = head.name.upper() if hasattr(head, 'name') else str(head).upper()

        # Compound COMPLEX, e.g. `(complex short-float)`: a list, so `type_name`
        # above never matches it by name and it fell all the way through to the
        # "unsupported result type" error. The component type isn't tracked
        # separately here -- there is one Python `complex` behind every CL
        # complex subtype, the same simplification `comparison.py` documents for
        # the four CL float subtypes sharing one Python `float` -- so building it
        # is the plain COMPLEX branch below, just also reached from a list head.
        if head_name == 'COMPLEX':
            if isinstance(object, complex):
                return object
            elif isinstance(object, (float, int, Fraction)):
                # Convert to complex: integer/float/ratio become complex with imaginary part 0
                return complex(object, 0)
            else:
                raise lisptype.LispTypeError(f"COERCE: cannot convert to COMPLEX",
                                            expected_type=result_type,
                                            actual_value=object)

        # Sequence type specifiers like (VECTOR *), (VECTOR * 2), etc.
        # Check if object is already of this type first (identity coercion),
        # before trying to build a sequence.
        if head_name in _SEQUENCE_TYPE_NAMES:
            # First check if object is already of this type
            if typep(object, result_type) == lisptype.T:
                return object
            # Otherwise, try to build the sequence
            return build_sequence(result_type, seq_elements(object, 'COERCE'), 'COERCE')

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
    # First check if object is already of this type (identity coercion).
    if type_name is not None and type_name in _SEQUENCE_TYPE_NAMES:
        if typep(object, result_type) == lisptype.T:
            return object
        return build_sequence(result_type, seq_elements(object, 'COERCE'), 'COERCE')

    # CHARACTER - accept characters and single-character strings/symbols
    if type_name == 'CHARACTER':
        # Character object (from lisptype_basic.Character)
        if isinstance(object, lisptype.Character):
            return object
        # Single-character string (Python str)
        elif isinstance(object, str) and len(object) == 1:
            # Python strings should be returned as-is (they are characters)
            return object
        # LispString (single character)
        elif isinstance(object, lisptype.LispString) and len(object) == 1:
            # Extract the single character from the LispString and return as Character
            from fclpy.lisptype_basic import Character
            return Character(str(object)[0])
        # Symbol with single character name
        elif isinstance(object, lisptype.LispSymbol) and len(object.name) == 1:
            # Return the first character as a Character object
            from fclpy.lisptype_basic import Character
            return Character(object.name[0])
        else:
            raise lisptype.LispTypeError(f"COERCE: cannot convert to CHARACTER",
                                        expected_type="CHARACTER",
                                        actual_value=object)

    # FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT - convert number to float
    if type_name in ('FLOAT', 'SINGLE-FLOAT', 'SHORT-FLOAT', 'DOUBLE-FLOAT', 'LONG-FLOAT'):
        if isinstance(object, (int, float, Fraction)):
            return float(object)
        else:
            raise lisptype.LispTypeError(f"COERCE: cannot convert {type(object).__name__} to FLOAT",
                                        expected_type="FLOAT",
                                        actual_value=object)

    # COMPLEX - convert to complex number
    if type_name == 'COMPLEX':
        if isinstance(object, complex):
            return object
        elif isinstance(object, (float, int, Fraction)):
            # Convert to complex: integer/float/ratio become complex with imaginary part 0
            return complex(object, 0)
        else:
            raise lisptype.LispTypeError(f"COERCE: cannot convert to COMPLEX",
                                        expected_type="COMPLEX",
                                        actual_value=object)

    # FUNCTION - coerce to function (e.g., from symbol or lambda form)
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
        elif _consp_internal(object):
            # Lambda form - evaluate it to get a function
            from .evaluation_core import eval as lisp_eval
            try:
                func = lisp_eval(object, state.current_environment)
                if callable(func):
                    return func
            except Exception:
                pass
            raise lisptype.LispTypeError(f"COERCE: cannot convert to FUNCTION",
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
    # This also handles class objects: if the object is an instance of the class,
    # return it unchanged.
    if typep(object, result_type) == lisptype.T:
        return object

    # Special handling for class objects: if result_type is a class object,
    # try to convert the object using the sequence protocol if it's convertible
    if isinstance(result_type, classes.LispClass):
        # Check if the class name corresponds to a sequence type
        if hasattr(result_type, 'name'):
            name = result_type.name
            if isinstance(name, lisptype.LispSymbol):
                class_name = name.name.upper()
            else:
                class_name = str(name).upper()
            if class_name in _SEQUENCE_TYPE_NAMES:
                # Try to build a sequence of this class type
                try:
                    return build_sequence(class_name, seq_elements(object, 'COERCE'), 'COERCE')
                except Exception:
                    pass

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
