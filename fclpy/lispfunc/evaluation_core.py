"""Core Lisp evaluation system - eval, apply, and dispatch.

This module contains the main eval() function that dispatches to special forms
and the apply() function for function application.
"""

import logging

import fclpy.state as state
import fclpy.lisptype as lisptype
import fclpy.lispreader as lispreader
from .core import car, cdr, cons, _consp_internal, _atom_internal
import fclpy.lispenv as lispenv  # environment setup utilities
from fclpy.lisptype import resolve_environment, LispEnvironmentError
import inspect
from functools import lru_cache
import sys
from fclpy import classes

# Register special operator handlers into the builtin registry
from . import registry as _registry
from . import arrays as _arrays
import fclpy.lispfunc as lispfunc

logger = logging.getLogger(__name__)


def is_arity_mismatch_message(error_str):
    """True if a Python `TypeError` string names a call-arity problem.

    Calling a Python callable with the wrong number of arguments is how a
    Lisp function invoked with the wrong number of arguments looks from
    here; CLHS says that signals PROGRAM-ERROR, not a raw Python exception
    reaching a Lisp value (plan.md finding X1). One predicate shared by
    every call site that converts a `TypeError` into a condition, so the
    definition of "looks like an arity error" cannot drift between them.
    """
    low = error_str.lower()
    return (('missing' in low and 'argument' in low)
            or ('takes' in low and 'argument' in low)
            or ('positional argument' in low))


# Cache for function signature information to avoid repeated inspect.signature calls
@lru_cache(maxsize=1024)
def _get_func_signature_info(func_id: int, func):
    """Get cached signature information for a function.
    
    Returns a tuple of (use_kwargs, kwarg_param_names_frozenset, num_required_positionals).
    If kwarg_param_names contains '*', it means the function accepts **kwargs
    and will accept any keyword argument.
    """
    try:
        sig = inspect.signature(func)
        params = list(sig.parameters.values())
        
        # Check if function accepts varargs (*args)
        has_var_positional = any(p.kind == inspect.Parameter.VAR_POSITIONAL for p in params)
        
        # Check if function accepts **kwargs
        has_var_keyword = any(p.kind == inspect.Parameter.VAR_KEYWORD for p in params)
        
        # Count required positional parameters (no default, not *args, not **kwargs)
        num_required_positionals = 0
        for p in params:
            if (p.kind in (inspect.Parameter.POSITIONAL_ONLY, inspect.Parameter.POSITIONAL_OR_KEYWORD)
                and p.default is inspect.Parameter.empty):
                num_required_positionals += 1
        
        # Collect the actual keyword parameter names for this function
        kwarg_param_names = set()
        for p in params:
            if (p.kind in (inspect.Parameter.KEYWORD_ONLY, inspect.Parameter.POSITIONAL_OR_KEYWORD)
                and p.default is not inspect.Parameter.empty):
                kwarg_param_names.add(p.name.lower())
        
        # If function accepts **kwargs, mark with '*' to accept any keyword
        if has_var_keyword:
            kwarg_param_names.add('*')
        
        use_kwargs = bool(kwarg_param_names) and not has_var_positional
        return (use_kwargs, frozenset(kwarg_param_names), num_required_positionals)
    except (ValueError, TypeError):
        return (False, frozenset(), 0)



def _eval_args(args, env):
    """Evaluate a place's argument forms, left to right, into a Python list.

    A place's subforms are evaluated once, in order (CLHS 5.1.1.1); the copies
    this replaced each evaluated exactly the first *two* of them, which is why
    an array place could only ever take one subscript.
    """
    values = []
    current = args
    while _consp_internal(current):
        values.append(eval(car(current), env))
        current = cdr(current)
    return values


def get_func_signature_info(func):
    """Get signature info for a function, using cached helper."""
    return _get_func_signature_info(id(func), func)


def split_keyword_args(func, values):
    """Split already-evaluated call arguments into (positional_args, kwargs).

    CLHS 3.4.1.4: a keyword/value pair is recognized once every required
    positional parameter is filled, and applies identically whether the
    call arrived as a direct function-call form, FUNCALL, or APPLY -- this
    is the one place that decision is made, so all three call sites agree.
    A user-defined LAMBDA/DEFUN closure takes `*call_args` with no
    `**kwargs` and parses its own `&key` parameters from the positional
    stream, so `use_kwargs` is false for it and this is a no-op passthrough
    -- only built-in `**kwargs`-accepting callables are affected.
    """
    use_kwargs, kwarg_param_names, num_required_positionals = get_func_signature_info(func)
    pos_args = []
    kwargs = {}
    i = 0
    n = len(values)
    while i < n:
        value = values[i]
        if (use_kwargs and isinstance(value, lisptype.lispKeyword)
                and len(pos_args) >= num_required_positionals):
            py_key = value.name.lower().replace('-', '_')
            if py_key in kwarg_param_names or '*' in kwarg_param_names:
                if i + 1 >= n:
                    # CLHS 3.5.1.6: an odd number of keyword arguments is a
                    # PROGRAM-ERROR.
                    raise lisptype.LispProgramError(
                        f"odd number of keyword arguments: {value.name} "
                        f"has no value")
                # CLHS 3.4.1.4.1: when a keyword appears more than once,
                # the *leftmost* pair is the one used.
                if py_key not in kwargs:
                    kwargs[py_key] = values[i + 1]
                i += 2
                continue
        pos_args.append(value)
        i += 1
    return pos_args, kwargs


class ReturnFromException(Exception):
    """Exception raised by RETURN-FROM to exit a BLOCK."""
    def __init__(self, tag, value):
        self.tag = tag
        self.value = value
        super().__init__(f"RETURN-FROM {tag.name if hasattr(tag, 'name') else tag}")


class ThrowException(Exception):
    """Exception raised by THROW when tag not caught."""
    def __init__(self, tag, value):
        self.tag = tag
        self.value = value
        super().__init__(f"Uncaught THROW {tag.name if hasattr(tag, 'name') else tag}")


class GoException(Exception):
    """Exception raised by GO to jump to a tag in TAGBODY."""
    def __init__(self, tag):
        self.tag = tag
        super().__init__(f"GO {tag.name if hasattr(tag, 'name') else tag}")


class HandlerCaseTag:
    """A unique CATCH tag identifying one HANDLER-CASE (or IGNORE-ERRORS)
    frame, used as the tag of a HandlerCaseTransfer.

    `__eq__` is identity and never returns NotImplemented, so the tag
    comparison in `eval_catch` can never be answered by a Lisp object's own
    `__eq__` on the other side: an intervening (CATCH 'FOO ...) sees this tag,
    finds it does not match, and re-raises, exactly as it should.
    """
    __slots__ = ()

    def __eq__(self, other):
        return self is other

    def __hash__(self):
        return id(self)

    def __repr__(self):
        return f"#<handler-case-tag {id(self):#x}>"


class HandlerCaseTransfer(ThrowException):
    """Carries control from a HANDLER-CASE handler back to its own frame.

    CLHS defines HANDLER-CASE as HANDLER-BIND plus a transfer of control out to
    the HANDLER-CASE form: the handler runs at the signal point like any other
    handler, transfers control here, and only then -- after unwinding, with the
    handlers disestablished -- is the clause body evaluated. See
    lispfunc/evaluation_conditions.py for the establishing side.

    It subclasses ThrowException because it *is* a throw to a dynamically
    established tag, and because that makes every place in the evaluator that
    already lets a THROW pass through untouched (APPLY, FUNCALL, the special
    forms' control-transfer re-raise clauses) do the right thing for it
    automatically. A new, unrelated exception class would have had to be added
    to each of those tuples by hand, and any site that was missed would
    silently convert a handler transfer into an error -- the same defect class
    as plan.md Finding K.
    """

    def __init__(self, tag, clause, condition):
        super().__init__(tag, condition)
        self.clause = clause
        self.condition = condition


class ConditionException(Exception):
    """Exception raised when a Lisp condition is signaled.

    This exception wraps a condition object and is used to communicate
    error/warning conditions through the Python exception system.
    """
    def __init__(self, condition, recoverable=False):
        """Initialize a condition exception.

        Args:
            condition: A Condition object from lisptype
            recoverable: If True, the condition has a continue restart
        """
        if not isinstance(condition, lisptype.Condition):
            # Every ERROR/SIGNAL/CERROR/WARN call site is supposed to build a
            # real condition object before raising, but a caller that fails
            # to do so (e.g. a datum whose type wasn't recognized) would
            # otherwise smuggle an arbitrary Python object through as "the
            # condition" -- _condition_matches can never match it against
            # any handler type, not even (ERROR (C) ...) or T, so it escapes
            # every enclosing HANDLER-CASE/HANDLER-BIND uncaught (see plan.md
            # Finding E: "a Python object surfacing as a Lisp value is
            # always a bug"). Wrap it in a generic ERROR condition instead,
            # so it is always at least catchable as an ERROR.
            condition = lisptype.Error(message=str(condition))
        self.condition = condition
        self.recoverable = recoverable
        super().__init__(str(condition))


def expand_deftype(entry, args):
    """Expand a DEFTYPE'd type specifier to the type it denotes (CLHS 4.2.3).

    `entry` is what the DEFTYPE special form stored in the global environment's
    `user_types`; `args` are the compound specifier's arguments (empty for an
    atomic reference to the type name). Called by `fclpy.typespec`, which is the
    one reader of that table.

    Only the expander's *primary* value is the type -- `deftype.17` defines an
    expander whose body is `(values 'integer t)` and requires the type to be
    INTEGER -- so a MultipleValues result is reduced here rather than being
    handed on as a type specifier.
    """
    expander = entry.get('expander')
    if expander is None:
        raise lisptype.LispError('DEFTYPE %r has no expander' % (entry.get('name'),))
    result = expander(*args)
    if isinstance(result, lisptype.MultipleValues):
        return result.values[0] if result.values else lisptype.NIL
    return result


def parse_lambda_list(lambda_list):
    """Parse a Common Lisp lambda list into structured form.

    Returns a dict with keys:
    - required: list of required parameter symbols
    - optional: list of optional parameter specs (symbol or [symbol, default])
    - rest: single rest parameter symbol or None
    - keyword: list of keyword parameter specs (symbol or [symbol, default])
    - aux: list of auxiliary parameter specs (symbol or [symbol, init])
    - environment: single environment parameter symbol or None
    
    Supported format:
    (req1 req2 &optional opt1 (opt2 default2) &rest rest-var 
     &key key1 (key2 default2) &aux (aux1 init1) &environment env)
    """
    required = []
    optional = []
    rest = None
    keyword = []
    aux = []
    environment = None
    
    # Parse the lambda list
    current_section = 'required'
    current = lambda_list
    whole = None

    while _consp_internal(current):
        param = car(current)

        # Check for section markers
        if isinstance(param, lisptype.LispSymbol):
            marker = param.name.upper()
            if marker == '&OPTIONAL':
                current_section = 'optional'
                current = cdr(current)
                continue
            elif marker == '&REST' or marker == '&BODY':
                # &BODY is a Common Lisp synonym for &REST
                current_section = 'rest'
                current = cdr(current)
                continue
            elif marker == '&KEY':
                current_section = 'keyword'
                current = cdr(current)
                continue
            elif marker == '&AUX':
                current_section = 'aux'
                current = cdr(current)
                continue
            elif marker == '&ALLOW-OTHER-KEYS':
                # Skip this marker - it's informational
                current = cdr(current)
                continue
            elif marker == '&WHOLE':
                # &WHOLE takes a single following symbol which is bound to the
                # entire macro form; consume that symbol and record it.
                next_sym = car(cdr(current)) if _consp_internal(cdr(current)) else None
                if isinstance(next_sym, lisptype.LispSymbol):
                    whole = next_sym
                # Advance past &WHOLE and its parameter
                current = cdr(cdr(current))
                continue
            elif marker == '&ENVIRONMENT':
                # &ENVIRONMENT takes a single following symbol which is bound to
                # the lexical environment; consume that symbol and record it.
                next_sym = car(cdr(current)) if _consp_internal(cdr(current)) else None
                if isinstance(next_sym, lisptype.LispSymbol):
                    environment = next_sym
                # Advance past &ENVIRONMENT and its parameter
                current = cdr(cdr(current))
                continue

        # Add parameter to appropriate section
        if current_section == 'required':
                if isinstance(param, lisptype.LispSymbol):
                    required.append(param)
                elif _consp_internal(param):
                    # Allow destructuring parameter specs (lists) to be included
                    # as required parameters so callers like (arg1 (&whole w arg2))
                    # are preserved for later destructuring binding.
                    required.append(param)
        elif current_section == 'optional':
            if isinstance(param, lisptype.LispSymbol):
                optional.append(param)
            elif _consp_internal(param):
                # Optional with default: (name default)
                optional.append(param)
        elif current_section == 'rest':
            # &REST may be followed by a simple symbol or a destructuring
            # specification like (name . tail). Preserve the spec as-is
            # so callers can handle destructuring.
            if isinstance(param, lisptype.LispSymbol) or _consp_internal(param):
                rest = param
                current_section = 'after_rest'  # After &REST, expect &KEY or &AUX
        elif current_section == 'keyword':
            if isinstance(param, lisptype.LispSymbol):
                keyword.append(param)
            elif _consp_internal(param):
                # Keyword with default: (name default)
                keyword.append(param)
        elif current_section == 'aux':
            if isinstance(param, lisptype.LispSymbol):
                aux.append(param)
            elif _consp_internal(param):
                # Aux with init: (name init)
                aux.append(param)

        current = cdr(current)

    # A destructuring lambda list may end in a dotted pair whose tail is a
    # single variable (CLHS 3.4.4), e.g. `(a b . rest)`, instead of an
    # explicit &REST/&BODY keyword. A proper list ends this loop with NIL
    # (however NIL is represented); anything else left in `current` is that
    # dotted-tail variable.
    if rest is None and isinstance(current, lisptype.LispSymbol) and current.name.upper() != 'NIL':
        rest = current

    # Include whole and environment in returned structure so macro handling can bind them
    return {
        'required': required,
        'optional': optional,
        'rest': rest,
        'keyword': keyword,
        'aux': aux,
        'whole': whole,
        'environment': environment
    }


def bind_destructuring_pattern(pattern, value, env):
    """Bind a Common Lisp *destructuring lambda list* pattern against a value.

    CLHS 3.4.4/3.4.5: a destructuring pattern is a symbol (bound directly to
    the whole value), NIL (nothing bound), or a list which may itself contain
    any of the lambda-list-keyword sections (&WHOLE/&OPTIONAL/&REST/&BODY/
    &KEY/&AUX) or end in a dotted tail -- and any variable name occupying any
    of those sections may itself be a nested pattern of the same shape. One
    recursive walk handles every level by reusing parse_lambda_list, so a
    nested pattern is parsed exactly like a top-level one instead of a second,
    partial copy that only understands the shapes its author had in mind.

    Shared by DEFMACRO/MACROLET's parameter binding and DESTRUCTURING-BIND,
    which destructure the same grammar (CLHS 3.4.4) into two different
    environments -- callers pass whichever `env` the bindings belong in.
    """
    if isinstance(pattern, lisptype.LispSymbol) and pattern.name.upper() != 'NIL':
        env.add_variable(pattern, value if value is not None else lisptype.NIL)
        return
    if not _consp_internal(pattern):
        return

    parsed = parse_lambda_list(pattern)

    whole = parsed.get('whole')
    if whole is not None:
        bind_destructuring_pattern(whole, value, env)

    cur = value
    for p in parsed.get('required', []):
        if _consp_internal(cur):
            v = car(cur)
            cur = cdr(cur)
        else:
            v = lisptype.NIL
        bind_destructuring_pattern(p, v, env)

    for opt in parsed.get('optional', []):
        if isinstance(opt, lisptype.LispSymbol):
            name, default_form, supplied_p = opt, None, None
        elif _consp_internal(opt):
            name = car(opt)
            rest_spec = cdr(opt)
            default_form = car(rest_spec) if _consp_internal(rest_spec) else None
            rest_spec2 = cdr(rest_spec) if _consp_internal(rest_spec) else None
            supplied_p = car(rest_spec2) if _consp_internal(rest_spec2) else None
        else:
            continue

        if _consp_internal(cur):
            v = car(cur)
            cur = cdr(cur)
            bind_destructuring_pattern(name, v, env)
            if supplied_p is not None:
                env.add_variable(supplied_p, lisptype.T)
        else:
            default_value = eval(default_form, env) if default_form is not None else lisptype.NIL
            bind_destructuring_pattern(name, default_value, env)
            if supplied_p is not None:
                env.add_variable(supplied_p, lisptype.NIL)

    rest_param = parsed.get('rest')
    if rest_param is not None:
        bind_destructuring_pattern(rest_param, cur, env)

    for kw in parsed.get('keyword', []):
        if _consp_internal(kw):
            key_name_spec = car(kw)
            rest_spec = cdr(kw)
            default_form = car(rest_spec) if _consp_internal(rest_spec) else None
            rest_spec2 = cdr(rest_spec) if _consp_internal(rest_spec) else None
            supplied_p = car(rest_spec2) if _consp_internal(rest_spec2) else None
        else:
            key_name_spec = kw
            default_form = None
            supplied_p = None

        if isinstance(key_name_spec, lisptype.LispSymbol):
            kw_name = key_name_spec.name.upper()
            var_pattern = key_name_spec
        elif _consp_internal(key_name_spec):
            kw_sym = car(key_name_spec)
            tail = cdr(key_name_spec)
            var_pattern = car(tail) if _consp_internal(tail) else None
            kw_name = kw_sym.name.upper() if isinstance(kw_sym, (lisptype.LispSymbol, lisptype.lispKeyword)) else None
        else:
            continue

        found = False
        tmpk = cur
        while _consp_internal(tmpk):
            k = car(tmpk)
            rest_k = cdr(tmpk)
            v = car(rest_k) if _consp_internal(rest_k) else lisptype.NIL
            if isinstance(k, lisptype.lispKeyword) and k.name.upper() == kw_name:
                bind_destructuring_pattern(var_pattern, v, env)
                if supplied_p is not None:
                    env.add_variable(supplied_p, lisptype.T)
                found = True
                break
            tmpk = cdr(rest_k) if _consp_internal(rest_k) else lisptype.NIL
        if not found:
            default_value = eval(default_form, env) if default_form is not None else lisptype.NIL
            bind_destructuring_pattern(var_pattern, default_value, env)
            if supplied_p is not None:
                env.add_variable(supplied_p, lisptype.NIL)

    for aux in parsed.get('aux', []):
        if isinstance(aux, lisptype.LispSymbol):
            env.add_variable(aux, lisptype.NIL)
        elif _consp_internal(aux):
            aux_name = car(aux)
            rest_spec = cdr(aux)
            init_form = car(rest_spec) if _consp_internal(rest_spec) else None
            init_value = eval(init_form, env) if init_form is not None else lisptype.NIL
            bind_destructuring_pattern(aux_name, init_value, env)


@_registry.cl_function('EVAL')
def eval_function(*args):
    """Common Lisp EVAL function - evaluates a form in the null lexical environment.

    EVAL takes exactly one argument: the form to evaluate.
    Per ANSI Common Lisp spec, EVAL has signature: (eval form) => result*

    Signals a PROGRAM-ERROR condition if the wrong number of arguments is provided.
    """
    # Validate argument count - EVAL takes exactly one argument
    if len(args) != 1:
        if len(args) == 0:
            cond = lisptype.ProgramError(message="EVAL requires exactly one argument: the form to evaluate")
        else:
            cond = lisptype.ProgramError(message=f"EVAL takes exactly one argument, got {len(args)}")
        raise ConditionException(cond, recoverable=False)

    form = args[0]
    # Call the internal eval with null lexical environment
    return eval(form, None)


def eval(form, env=None):
    """Internal evaluation function - evaluates a Lisp form in the given environment.

    This is the internal workhorse function. User code should call the EVAL function above.
    """
    # Import special form handlers lazily to avoid circular imports
    from .evaluation_special_forms import (
        eval_if, eval_setq, eval_defun, eval_defmacro, eval_macroexpand_1,
        eval_macro_function, eval_lambda, eval_declare, eval_declaim,
        eval_defvar, eval_defparameter, eval_defconstant, eval_defstruct, eval_pop, eval_push, eval_pushnew,
        eval_incf, eval_decf, eval_defclass, eval_defgeneric, eval_defmethod, eval_define_method_combination,
        eval_call_method, eval_make_method,
        eval_destructuring_bind, eval_psetq, eval_rotatef
    )
    from .evaluation_control_flow import (
        eval_block, eval_return_from, eval_catch, eval_throw,
        eval_unwind_protect, eval_tagbody, eval_go
    )
    from .evaluation_loops_conditionals import (
        eval_when, eval_unless, eval_cond, eval_case, eval_ccase, eval_and, eval_or,
        eval_progn, eval_locally, eval_prog1, eval_prog2, eval_prog, eval_prog_star, eval_time, eval_let, eval_letstar, eval_quasiquote,
        eval_loop, eval_eval_when, eval_do, eval_do_star, eval_dolist, eval_dotimes,
        eval_do_symbols, eval_do_external_symbols, eval_do_all_symbols,
        eval_flet, eval_labels,
        eval_ecase, eval_typecase, eval_etypecase, eval_ctypecase
    )
    from .utilities_functions import eval_progv
    from .evaluation_conditions import (
        eval_signal, eval_error, eval_cerror, eval_warn,
        eval_restart_case, eval_restart_bind, eval_invoke_restart, eval_abort,
        eval_multiple_value_call, eval_multiple_value_bind, eval_multiple_value_setq,
        eval_multiple_value_prog1,
        eval_handler_bind, eval_handler_case, eval_ignore_errors,
        eval_define_condition
    )
    
    env = resolve_environment(env)
    
    # Self-evaluating forms
    # Normalize Python-level sentinels into Lisp equivalents to avoid type
    # mismatches (e.g., Python True vs Lisp symbol T) leaking into Lisp code.
    if form is None:
        return lisptype.NIL
    if isinstance(form, bool):
        return lisptype.T if form else lisptype.NIL
    if isinstance(form, (int, float, str)):
        return form
    # Keywords are self-evaluating in Common Lisp semantics
    if isinstance(form, lisptype.lispKeyword):
        return form
    
    # Symbols - look up in environment
    if isinstance(form, lisptype.LispSymbol):
        # Check for symbol-macros first (SYMBOL-MACROLET)
        symbol_macro_expansion = env.get_symbol_macro(form)
        if symbol_macro_expansion is not None:
            # Expand the symbol-macro and evaluate the result
            return eval(symbol_macro_expansion, env)
        
        # Check variable bindings first - use has_variable to handle None values
        if env.has_variable(form):
            return env.find_variable(form)
        # Fall back to the symbol's own global/dynamic value cell -- the same
        # cell SET/SYMBOL-VALUE/BOUNDP/PROGV read and write -- for special
        # variables that have no lexical shadow in the current environment
        # chain.
        if getattr(form, 'value', None) is not None:
            return form.value
        # If not found as variable, check function bindings
        value = env.find_func(form)
        if value is not None:
            return value
        # As a fallback, consult the function registry and auto-install
        # bindings into the environment if a registered function exists but
        # hasn't been wired into this Environment instance yet.
        try:
            py_name = _registry.get_function_py_name(form.name)
            if py_name:
                # Ensure the standard environment and lispfunc exports are loaded
                try:
                    lispenv.setup_standard_environment()
                except Exception:
                    logger.error("Error during standard environment setup for function registry lookup for symbol: %s", form.name, exc_info=True)
                fn = getattr(lispfunc, py_name, None)
                if fn is None:
                    # Try importing common lispfunc submodules to resolve circular imports
                    try:
                        import importlib
                        for sub in ('core', 'math', 'sequences', 'vectors', 'streams', 'pathnames', 'hashtables', 'evaluation', 'comparison', 'characters', 'io', 'io_read', 'io_write', 'utilities', 'classes', 'misc_macros'):
                            try:
                                mod = importlib.import_module(f'fclpy.lispfunc.{sub}')
                                fn = getattr(mod, py_name, None)
                                if fn:
                                    # expose on package module for future lookups
                                    try:
                                        setattr(lispfunc, py_name, fn)
                                    except Exception:
                                        logger.error("Error setting attribute on lispfunc module for symbol: %s", form.name, exc_info=True)
                                    break
                            except Exception:
                                logger.warning("Import error during function registry lookup for symbol: %s in submodule: %s", form.name, sub, exc_info=True)
                                continue
                    except Exception:
                        logger.error("Error during function registry lookup for symbol: %s", form.name, exc_info=True)
                if fn:
                    # Bind into environment for faster future lookups
                    env.add_function(form, fn)
                    return fn
        except Exception:
            # Defensive: if registry lookup fails, ignore and raise below
            logger.error("Error during function registry lookup for symbol: %s", form.name, exc_info=True)
        # Create an UnboundVariable condition and raise as a ConditionException
        try:
            name_slot = form if isinstance(form, lisptype.LispSymbol) else getattr(form, 'name', str(form))
        except Exception:
            name_slot = getattr(form, 'name', str(form))
        cond = lisptype.UnboundVariable(name=name_slot, message=f"Unbound variable: {getattr(form, 'name', str(form))} with value {getattr(form, 'value', None)}")
        raise ConditionException(cond, recoverable=False)
    
    # Lists - function calls or special forms
    if _consp_internal(form):
        operator = car(form)
        args = cdr(form)
        
        # Check for special forms
        if isinstance(operator, lisptype.LispSymbol):
            if operator.name == 'QUOTE':
                return car(args)
            elif operator.name == 'IF':
                return eval_if(form, env)
            elif operator.name == 'FUNCTION':
                # (FUNCTION name) - look up the function
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("FUNCTION requires exactly one argument")
                name = car(args)
                # Handle lambda expressions
                if isinstance(name, lisptype.lispCons):
                    op = car(name)
                    if isinstance(op, lisptype.LispSymbol) and op.name == 'LAMBDA':
                        return eval_lambda(name, env)
                # Handle symbol names
                if isinstance(name, lisptype.LispSymbol):
                    # First look in environment
                    func = env.find_func(name)
                    if func is not None:
                        return func
                    # Then check the registry
                    py_name = _registry.get_function_py_name(name.name)
                    if py_name:
                        try:
                            lispenv.setup_standard_environment()
                        except Exception:
                            pass
                        import importlib
                        import fclpy.lispfunc as lispfunc_module
                        func = getattr(lispfunc_module, py_name, None)
                        if func is None:
                            for sub in ('core', 'math', 'sequences', 'vectors', 'streams', 'pathnames', 'hashtables', 'evaluation', 'comparison', 'characters', 'io', 'io_read', 'io_write', 'utilities', 'classes', 'misc_macros'):
                                try:
                                    mod = importlib.import_module(f'fclpy.lispfunc.{sub}')
                                    func = getattr(mod, py_name, None)
                                    if func is not None:
                                        try:
                                            setattr(lispfunc_module, py_name, func)
                                        except Exception:
                                            pass
                                        break
                                except Exception:
                                    continue
                        if func is not None:
                            return func
                        # Signal an UNDEFINED-FUNCTION condition
                        cond = lisptype.UndefinedFunction(name=name.name if isinstance(name, lisptype.LispSymbol) else str(name))
                        raise ConditionException(cond, recoverable=False)
                return name
            elif operator.name == 'SETQ':
                return eval_setq(form, env)
            elif operator.name == 'SETF':
                # SETF is a generalized assignment macro/special form
                # (SETF place value) or (SETF place1 value1 place2 value2 ...)
                # For simple variable places, behave like SETQ
                # For complex places (CAR, CDR, AREF, etc.), call appropriate setter
                args = cdr(form)
                result = lisptype.NIL
                
                while _consp_internal(args) and _consp_internal(cdr(args)):
                    place = car(args)
                    value_form = car(cdr(args))
                    
                    if isinstance(place, lisptype.LispSymbol):
                        # Simple variable assignment - like SETQ
                        result = eval(value_form, env)
                        env.set_variable(place, result)
                    elif _consp_internal(place):
                        # Complex place like (CAR x), (CDR x), (AREF arr i), (SLOT-VALUE obj slot), etc.
                        place_op = car(place)
                        if isinstance(place_op, lisptype.LispSymbol):
                            op_name = place_op.name
                            place_args = cdr(place)
                            result = eval(value_form, env)
                            
                            if op_name == 'CAR':
                                target = eval(car(place_args), env)
                                if _consp_internal(target):
                                    target.car = result
                                else:
                                    raise lisptype.LispError("SETF CAR: target is not a cons")
                            elif op_name == 'CDR':
                                target = eval(car(place_args), env)
                                if _consp_internal(target):
                                    target.cdr = result
                                else:
                                    raise lisptype.LispError("SETF CDR: target is not a cons")
                            elif op_name == 'CADR':
                                target = eval(car(place_args), env)
                                if _consp_internal(target) and _consp_internal(cdr(target)):
                                    cdr(target).car = result
                                else:
                                    raise lisptype.LispError("SETF CADR: invalid structure")
                            elif _arrays.is_array_place(op_name):
                                # (SETF (AREF arr i j) val), (SETF (FILL-POINTER v) n), ...
                                _arrays.array_place_write(
                                    op_name, _eval_args(place_args, env), result)
                            elif op_name in ('CHAR', 'SCHAR'):
                                # (SETF (CHAR str i) val) - now works with LispString
                                seq = eval(car(place_args), env)
                                idx = eval(car(cdr(place_args)), env)
                                try:
                                    seq[idx] = result
                                except (TypeError, IndexError) as e:
                                    raise lisptype.LispError(f"SETF {op_name}: {e}")
                            elif op_name == 'ELT':
                                # (SETF (ELT seq i) val) - works on lists and vectors
                                seq = eval(car(place_args), env)
                                idx = eval(car(cdr(place_args)), env)
                                if _consp_internal(seq):
                                    # List - walk to nth element
                                    current = seq
                                    for _ in range(idx):
                                        if not _consp_internal(current):
                                            raise lisptype.LispError("SETF ELT: index out of bounds")
                                        current = cdr(current)
                                    if _consp_internal(current):
                                        current.car = result
                                    else:
                                        raise lisptype.LispError("SETF ELT: index out of bounds")
                                else:
                                    # Vector/array/LispString - use indexing
                                    try:
                                        seq[idx] = result
                                    except (TypeError, IndexError) as e:
                                        raise lisptype.LispError(f"SETF ELT: {e}")
                            elif op_name == 'GETHASH':
                                key = eval(car(place_args), env)
                                table = eval(car(cdr(place_args)), env)
                                try:
                                    table[key] = result
                                except (TypeError, KeyError) as e:
                                    raise lisptype.LispError(f"SETF GETHASH: {e}")
                            elif op_name == 'SLOT-VALUE':
                                obj = eval(car(place_args), env)
                                slot_name = eval(car(cdr(place_args)), env)
                                if hasattr(obj, 'set_slot'):
                                    obj.set_slot(slot_name, result)
                                elif hasattr(obj, '__dict__'):
                                    slot_key = slot_name.name if isinstance(slot_name, lisptype.LispSymbol) else str(slot_name)
                                    obj.__dict__[slot_key] = result
                                else:
                                    raise lisptype.LispError("SETF SLOT-VALUE: cannot set slot")
                            elif op_name == 'NTH':
                                n = eval(car(place_args), env)
                                lst = eval(car(cdr(place_args)), env)
                                current = lst
                                for _ in range(n):
                                    if not _consp_internal(current):
                                        raise lisptype.LispError("SETF NTH: index out of bounds")
                                    current = cdr(current)
                                if _consp_internal(current):
                                    current.car = result
                                else:
                                    raise lisptype.LispError("SETF NTH: index out of bounds")
                            elif op_name == 'SYMBOL-VALUE':
                                sym = eval(car(place_args), env)
                                if isinstance(sym, lisptype.LispSymbol):
                                    # SYMBOL-VALUE's getter reads sym.value directly
                                    # (the same cell BOUNDP/SET/MAKUNBOUND/PROGV use) --
                                    # its setter must write the same cell, not a
                                    # lexical environment binding.
                                    sym.value = result
                                else:
                                    raise lisptype.LispError("SETF SYMBOL-VALUE: requires a symbol")
                            elif op_name == 'SYMBOL-FUNCTION':
                                sym = eval(car(place_args), env)
                                if isinstance(sym, lisptype.LispSymbol):
                                    env.add_function(sym, result)
                                else:
                                    raise lisptype.LispError("SETF SYMBOL-FUNCTION: requires a symbol")
                            elif op_name == 'FDEFINITION':
                                from .utilities_functions import _function_spec_to_key
                                sym = _function_spec_to_key(eval(car(place_args), env))
                                if sym is not None:
                                    env.add_function(sym, result)
                                else:
                                    raise lisptype.LispError("SETF FDEFINITION: requires a symbol")
                            elif op_name == 'FIND-CLASS':
                                # (SETF (FIND-CLASS name) class) registers a class with a new name
                                place_name = eval(car(place_args), env)  # e.g., n3
                                if isinstance(place_name, lisptype.LispSymbol):
                                    # result is the class object to assign
                                    # We need to register it under the new name
                                    if isinstance(result, classes.LispClass):
                                        # Update the class's name to the target name
                                        original_name = result.name
                                        result.name = place_name
                                        # Register under the new name
                                        classes.register_class(result)
                                        # Also register under original name if different (aliases)
                                        if original_name != place_name:
                                            result.name = original_name
                                            classes.register_class(result)
                                            result.name = place_name  # Restore target name
                                    else:
                                        raise lisptype.LispError("SETF FIND-CLASS: value must be a class")
                                else:
                                    raise lisptype.LispError("SETF FIND-CLASS: place name must be a symbol")
                            elif op_name == 'MACRO-FUNCTION':
                                # (SETF (MACRO-FUNCTION sym) val) should install a macro
                                # Install into the global (root) environment so later
                                # EVAL/MACROEXPAND can find the macro when expanding.
                                sym = eval(car(place_args), env)
                                if isinstance(sym, lisptype.LispSymbol):
                                    global_env = env
                                    while global_env.parent is not None:
                                        global_env = global_env.parent
                                    global_env.add_function(sym, result)
                                    # Also add to current env for immediate visibility
                                    if env is not global_env:
                                        env.add_function(sym, result)
                                else:
                                    raise lisptype.LispError("SETF MACRO-FUNCTION: requires a symbol")
                            elif op_name == 'SYMBOL-PLIST':
                                sym = eval(car(place_args), env)
                                if isinstance(sym, lisptype.LispSymbol):
                                    sym.plist = result
                                else:
                                    raise lisptype.LispError("SETF SYMBOL-PLIST: requires a symbol")
                            elif op_name == 'GET':
                                sym = eval(car(place_args), env)
                                indicator = eval(car(cdr(place_args)), env)
                                # (GET symbol indicator [default]) -- the
                                # optional default form must still be
                                # evaluated (for its side effects), even
                                # though SETF GET never consults its value.
                                default_args = cdr(cdr(place_args))
                                if _consp_internal(default_args):
                                    eval(car(default_args), env)
                                if isinstance(sym, lisptype.LispSymbol):
                                    if not hasattr(sym, 'plist') or sym.plist is None:
                                        sym.plist = lisptype.NIL
                                    plist = sym.plist
                                    found = False
                                    current = plist
                                    while _consp_internal(current) and _consp_internal(cdr(current)):
                                        if car(current) == indicator:
                                            cdr(current).car = result
                                            found = True
                                            break
                                        current = cdr(cdr(current))
                                    if not found:
                                        sym.plist = lisptype.lispCons(indicator, lisptype.lispCons(result, sym.plist))
                                else:
                                    raise lisptype.LispError("SETF GET: requires a symbol")
                            elif op_name == 'SUBSEQ':
                                # (SETF (SUBSEQ seq start [end]) new-seq)
                                # For now, just return result (stub - modifying subsequences is complex)
                                pass  # Silently accept but don't actually modify
                            elif op_name in ('FIRST', 'SECOND', 'THIRD', 'FOURTH', 'FIFTH',
                                            'SIXTH', 'SEVENTH', 'EIGHTH', 'NINTH', 'TENTH'):
                                # (SETF (FIRST list) val) etc.
                                lst = eval(car(place_args), env)
                                n = {'FIRST': 0, 'SECOND': 1, 'THIRD': 2, 'FOURTH': 3, 'FIFTH': 4,
                                     'SIXTH': 5, 'SEVENTH': 6, 'EIGHTH': 7, 'NINTH': 8, 'TENTH': 9}[op_name]
                                current = lst
                                for _ in range(n):
                                    if not _consp_internal(current):
                                        raise lisptype.LispError(f"SETF {op_name}: list too short")
                                    current = cdr(current)
                                if _consp_internal(current):
                                    current.car = result
                                else:
                                    raise lisptype.LispError(f"SETF {op_name}: list too short")
                            elif op_name in ('CAAR', 'CDAR', 'CDDR', 'CAAAR', 'CAADR', 'CADAR', 'CADDR',
                                            'CDAAR', 'CDADR', 'CDDAR', 'CDDDR'):
                                # Compound CAR/CDR accessors
                                target = eval(car(place_args), env)
                                # Navigate to the target cons, then set
                                for c in op_name[1:-1]:  # Skip first C and last R
                                    if not _consp_internal(target):
                                        raise lisptype.LispError(f"SETF {op_name}: invalid structure")
                                    target = target.car if c == 'A' else target.cdr
                                if not _consp_internal(target):
                                    raise lisptype.LispError(f"SETF {op_name}: invalid structure")
                                if op_name[-2] == 'A':
                                    target.car = result
                                else:
                                    target.cdr = result
                            elif op_name == 'GETF':
                                # (SETF (GETF plist indicator [default]) val)
                                # CLHS 5.1.2.6. Previously a bare `pass` --
                                # the assignment was silently discarded
                                # (plan.md standing rule 4). `_place_accessor`
                                # (shared with PUSH/PUSHNEW/INCF/ROTATEF) is
                                # the one home of this place's read/write
                                # pair, including rewriting the plist's own
                                # place when the indicator is new.
                                from .evaluation_special_forms import _place_accessor
                                _, getf_setter = _place_accessor(place, env)
                                getf_setter(result)
                            elif op_name == 'LDB':
                                # (SETF (LDB bytespec int) val) - byte manipulation, skip
                                pass
                            elif op_name == 'MASK-FIELD':
                                # (SETF (MASK-FIELD bytespec int) val)
                                pass
                            elif op_name == 'FILL-POINTER':
                                # (SETF (FILL-POINTER vector) val)
                                vec = eval(car(place_args), env)
                                if hasattr(vec, 'fill_pointer'):
                                    vec.fill_pointer = result
                            elif op_name == 'ROW-MAJOR-AREF':
                                # (SETF (ROW-MAJOR-AREF arr idx) val)
                                arr = eval(car(place_args), env)
                                idx = eval(car(cdr(place_args)), env)
                                try:
                                    arr[idx] = result
                                except (TypeError, IndexError) as e:
                                    raise lisptype.LispError(f"SETF ROW-MAJOR-AREF: {e}")
                            elif op_name == 'BIT':
                                # (SETF (BIT bit-array &rest subscripts) val)
                                arr = eval(car(place_args), env)
                                idx = eval(car(cdr(place_args)), env)
                                try:
                                    arr[idx] = result
                                except (TypeError, IndexError) as e:
                                    raise lisptype.LispError(f"SETF BIT: {e}")
                            elif op_name == 'SBIT':
                                # (SETF (SBIT bit-array &rest subscripts) val)
                                arr = eval(car(place_args), env)
                                idx = eval(car(cdr(place_args)), env)
                                try:
                                    arr[idx] = result
                                except (TypeError, IndexError) as e:
                                    raise lisptype.LispError(f"SETF SBIT: {e}")
                            else:
                                # Check for DEFSETF-defined expander
                                global_env = env
                                while global_env.parent is not None:
                                    global_env = global_env.parent
                                if hasattr(global_env, 'setf_expanders') and op_name in global_env.setf_expanders:
                                    expander = global_env.setf_expanders[op_name]
                                    if expander['type'] == 'short':
                                        update_fn_name = expander['update_fn']
                                        update_fn = env.find_func(update_fn_name)
                                        if update_fn is None:
                                            py_name = _registry.get_function_py_name(update_fn_name.name)
                                            if py_name:
                                                update_fn = getattr(lispfunc, py_name, None)
                                        if update_fn:
                                            eval_place_args = []
                                            pa = place_args
                                            while _consp_internal(pa):
                                                eval_place_args.append(eval(car(pa), env))
                                                pa = cdr(pa)
                                            eval_place_args.append(result)
                                            update_fn(*eval_place_args)
                                        else:
                                            raise lisptype.LispError(f"SETF: update function {update_fn_name} not found")
                                    else:
                                        # Long form - just accept for now
                                        pass
                                else:
                                    # Try generic struct accessor: look for SET-<accessor-name>
                                    setter_name = f"SET-{op_name}"
                                    setter_sym = lisptype.LispSymbol(setter_name)
                                    setter_func = env.find_func(setter_sym)
                                    if setter_func is None:
                                        # Try looking up in registry
                                        py_name = _registry.get_function_py_name(setter_name)
                                        if py_name:
                                            import fclpy.lispfunc as lispfunc
                                            setter_func = getattr(lispfunc, py_name, None)
                                    
                                    if setter_func:
                                        # Call the setter with the target object and all evaluated place args
                                        target_obj = eval(car(place_args), env)
                                        eval_place_args = [target_obj]
                                        pa = cdr(place_args)
                                        while _consp_internal(pa):
                                            eval_place_args.append(eval(car(pa), env))
                                            pa = cdr(pa)
                                        eval_place_args.append(result)
                                        setter_func(*eval_place_args)
                                    else:
                                        # (SETF (accessor arg*) value) where `accessor` has its
                                        # own registered (SETF accessor) function (e.g. via
                                        # (DEFUN (SETF accessor) ...) or (SETF (FDEFINITION
                                        # '(SETF accessor)) ...)). Per ANSI, call it with the
                                        # new value first, then the access-form's arguments.
                                        setf_fn_sym = lisptype.LispSymbol(f"(SETF {op_name})")
                                        setf_fn = env.find_func(setf_fn_sym)
                                        if setf_fn is not None:
                                            eval_place_args = [result]
                                            pa = place_args
                                            while _consp_internal(pa):
                                                eval_place_args.append(eval(car(pa), env))
                                                pa = cdr(pa)
                                            result = setf_fn(*eval_place_args)
                                        # else: unknown place type - silently accept (many exist)
                        else:
                            raise lisptype.LispNotImplementedError(f"SETF: place operator must be a symbol, got {place_op}")
                    else:
                        raise lisptype.LispNotImplementedError(f"SETF: place must be a symbol or form, got {place}")
                    
                    args = cdr(cdr(args))
                
                return result
            elif operator.name == 'PSETF':
                # PSETF is like SETF but evaluates ALL values FIRST before any assignment
                # (PSETF place1 value1 place2 value2 ...) - all values are computed first
                args = cdr(form)
                assignments = []  # List of (place, evaluated_value) pairs
                
                # First pass: collect all places and evaluate values
                while _consp_internal(args) and _consp_internal(cdr(args)):
                    place = car(args)
                    value_form = car(cdr(args))
                    value_result = eval(value_form, env)  # Evaluate all values first
                    assignments.append((place, value_result))
                    args = cdr(cdr(args))
                
                # Second pass: perform all assignments with pre-evaluated values
                result = lisptype.NIL
                for place, value_result in assignments:
                    result = value_result  # Track last assigned value
                    if isinstance(place, lisptype.LispSymbol):
                        # Simple variable assignment
                        env.set_variable(place, value_result)
                    elif _consp_internal(place):
                        # Complex place like (CAR x), (FDEFINITION sym), etc.
                        place_op = car(place)
                        if isinstance(place_op, lisptype.LispSymbol):
                            op_name = place_op.name
                            place_args = cdr(place)
                            
                            if op_name == 'CAR':
                                target = eval(car(place_args), env)
                                if _consp_internal(target):
                                    target.car = value_result
                                else:
                                    raise lisptype.LispError("PSETF CAR: target is not a cons")
                            elif op_name == 'CDR':
                                target = eval(car(place_args), env)
                                if _consp_internal(target):
                                    target.cdr = value_result
                                else:
                                    raise lisptype.LispError("PSETF CDR: target is not a cons")
                            elif _arrays.is_array_place(op_name):
                                _arrays.array_place_write(
                                    op_name, _eval_args(place_args, env), value_result)
                            elif op_name == 'SYMBOL-FUNCTION':
                                sym = eval(car(place_args), env)
                                if isinstance(sym, lisptype.LispSymbol):
                                    env.add_function(sym, value_result)
                                else:
                                    raise lisptype.LispError("PSETF SYMBOL-FUNCTION: requires a symbol")
                            elif op_name == 'FDEFINITION':
                                from .utilities_functions import _function_spec_to_key
                                sym = _function_spec_to_key(eval(car(place_args), env))
                                if sym is not None:
                                    env.add_function(sym, value_result)
                                else:
                                    raise lisptype.LispError("PSETF FDEFINITION: requires a symbol")
                            elif op_name == 'FIND-CLASS':
                                # (PSETF (FIND-CLASS name) class) registers a class with a new name
                                place_name = eval(car(place_args), env)  # e.g., n3
                                if isinstance(place_name, lisptype.LispSymbol):
                                    # value_result is the class object to assign
                                    # We need to register it under the new name
                                    if isinstance(value_result, classes.LispClass):
                                        # Update the class's name to the target name
                                        original_name = value_result.name
                                        value_result.name = place_name
                                        # Register under the new name
                                        classes.register_class(value_result)
                                        # Also register under original name if different (aliases)
                                        if original_name != place_name:
                                            value_result.name = original_name
                                            classes.register_class(value_result)
                                            value_result.name = place_name  # Restore target name
                                    else:
                                        raise lisptype.LispError("PSETF FIND-CLASS: value must be a class")
                                else:
                                    raise lisptype.LispError("PSETF FIND-CLASS: place name must be a symbol")
                            elif op_name == 'NTH':
                                n = eval(car(place_args), env)
                                lst = eval(car(cdr(place_args)), env)
                                current = lst
                                for _ in range(n):
                                    if not _consp_internal(current):
                                        raise lisptype.LispError("PSETF NTH: index out of bounds")
                                    current = cdr(current)
                                if _consp_internal(current):
                                    current.car = value_result
                                else:
                                    raise lisptype.LispError("PSETF NTH: index out of bounds")
                            elif op_name == 'FILL-POINTER':
                                vec = eval(car(place_args), env)
                                if hasattr(vec, 'fill_pointer'):
                                    vec.fill_pointer = value_result
                            elif op_name == 'MACRO-FUNCTION':
                                # (PSETF (MACRO-FUNCTION sym) val) should install a macro
                                sym = eval(car(place_args), env)
                                if isinstance(sym, lisptype.LispSymbol):
                                    global_env = env
                                    while global_env.parent is not None:
                                        global_env = global_env.parent
                                    global_env.add_function(sym, value_result)
                                    # Also add to current env for immediate visibility
                                    if env is not global_env:
                                        env.add_function(sym, value_result)
                                else:
                                    raise lisptype.LispError("PSETF MACRO-FUNCTION: requires a symbol")
                            else:
                                # For other complex places, try generic handling
                                pass
                        else:
                            raise lisptype.LispNotImplementedError(f"PSETF: place operator must be a symbol")
                    else:
                        raise lisptype.LispNotImplementedError(f"PSETF: place must be a symbol or form")
                
                return result
            elif operator.name == 'PROGN':
                return eval_progn(form, env)
            elif operator.name == 'LOCALLY':
                return eval_locally(form, env)
            elif operator.name == 'LET':
                return eval_let(form, env)
            elif operator.name == 'LET*':
                return eval_letstar(form, env)
            elif operator.name == 'FLET':
                return eval_flet(form, env)
            elif operator.name == 'LABELS':
                return eval_labels(form, env)
            elif operator.name == 'WHEN':
                return eval_when(form, env)
            elif operator.name == 'UNLESS':
                return eval_unless(form, env)
            elif operator.name == 'EVAL-WHEN':
                return eval_eval_when(form, env)
            elif operator.name == 'COND':
                return eval_cond(form, env)
            elif operator.name == 'CASE':
                return eval_case(form, env)
            elif operator.name == 'CCASE':
                return eval_ccase(form, env)
            elif operator.name == 'ECASE':
                return eval_ecase(form, env)
            elif operator.name == 'TYPECASE':
                return eval_typecase(form, env)
            elif operator.name == 'ETYPECASE':
                return eval_etypecase(form, env)
            elif operator.name == 'CTYPECASE':
                return eval_ctypecase(form, env)
            elif operator.name == 'PROGV':
                return eval_progv(form, env)
            elif operator.name == '%SPECIAL-REF':
                # Internal helper generated by LOCALLY's (DECLARE (SPECIAL x))
                # handling: read x's dynamic value cell if one has been
                # established (e.g. by PROGV), else fall back to a plain
                # lexical lookup of x in the calling environment.
                sym = car(cdr(form))
                if getattr(sym, 'value', None) is not None:
                    return sym.value
                if env.has_variable(sym):
                    return env.find_variable(sym)
                cond = lisptype.UnboundVariable(name=sym, message=f"Unbound variable: {sym.name}")
                raise ConditionException(cond, recoverable=False)
            elif operator.name == 'MULTIPLE-VALUE-SETQ':
                return eval_multiple_value_setq(form, env)
            elif operator.name == 'MULTIPLE-VALUE-PROG1':
                return eval_multiple_value_prog1(form, env)
            elif operator.name == 'PSETQ':
                return eval_psetq(form, env)
            elif operator.name == 'ROTATEF':
                return eval_rotatef(form, env)
            elif operator.name == 'MULTIPLE-VALUE-LIST':
                # Must see the raw (possibly multiple-valued) result of its
                # argument -- unlike ordinary function-call arguments, this
                # is NOT a single-value context, so it bypasses the generic
                # argument-evaluation loop below (which coerces to primary
                # value).
                from .evaluation_stubs import multiple_value_list as _mvl
                mvl_args = cdr(form)
                if not _consp_internal(mvl_args):
                    raise lisptype.LispNotImplementedError("MULTIPLE-VALUE-LIST requires one form")
                return _mvl(eval(car(mvl_args), env))
            elif operator.name == 'NTH-VALUE':
                # Same rationale as MULTIPLE-VALUE-LIST above: its value-form
                # argument must not be coerced to a single value.
                from .evaluation_stubs import nth_value as _nth_value
                nv_args = cdr(form)
                if not _consp_internal(nv_args) or not _consp_internal(cdr(nv_args)):
                    raise lisptype.LispNotImplementedError("NTH-VALUE requires n and a form")
                n = eval(car(nv_args), env)
                return _nth_value(n, eval(car(cdr(nv_args)), env))
            elif operator.name == 'AND':
                return eval_and(form, env)
            elif operator.name == 'OR':
                return eval_or(form, env)
            elif operator.name == 'PROG1':
                return eval_prog1(form, env)
            elif operator.name == 'PROG2':
                return eval_prog2(form, env)
            elif operator.name == 'PROG':
                return eval_prog(form, env)
            elif operator.name == 'PROG*':
                return eval_prog_star(form, env)
            elif operator.name == 'TIME':
                return eval_time(form, env)
            elif operator.name == 'DEFVAR':
                return eval_defvar(form, env)
            elif operator.name == 'DEFPARAMETER':
                return eval_defparameter(form, env)
            elif operator.name == 'DEFCONSTANT':
                return eval_defconstant(form, env)
            elif operator.name == 'DEFSTRUCT':
                return eval_defstruct(form, env)
            elif operator.name == 'DESTRUCTURING-BIND':
                return eval_destructuring_bind(form, env)
            elif operator.name == 'DEFCLASS':
                return eval_defclass(form, env)
            elif operator.name == 'DEFGENERIC':
                return eval_defgeneric(form, env)
            elif operator.name == 'DEFMETHOD':
                return eval_defmethod(form, env)
            elif operator.name == 'DEFINE-METHOD-COMBINATION':
                return eval_define_method_combination(form, env)
            elif operator.name == 'CALL-METHOD':
                return eval_call_method(form, env)
            elif operator.name == 'MAKE-METHOD':
                return eval_make_method(form, env)
            elif operator.name == 'LOOP':
                return eval_loop(form, env)
            elif operator.name == 'POP':
                return eval_pop(form, env)
            elif operator.name == 'PUSH':
                return eval_push(form, env)
            elif operator.name == 'PUSHNEW':
                return eval_pushnew(form, env)
            elif operator.name == 'DEFUN':
                return eval_defun(form, env)
            elif operator.name == 'LAMBDA':
                return eval_lambda(form, env)
            elif operator.name == 'QUASIQUOTE':
                return eval_quasiquote(form, env)
            elif operator.name == 'THE':
                from .evaluation_special_forms import eval_the
                return eval_the(form, env)
            elif operator.name == 'DEFMACRO':
                return eval_defmacro(form, env)
            elif operator.name == 'DECLARE':
                return eval_declare(form, env)
            elif operator.name == 'DECLAIM':
                return eval_declaim(form, env)
            elif operator.name == 'PROCLAIM':
                from .evaluation_special_forms import eval_proclaim
                return eval_proclaim(form, env)
            elif operator.name == 'MACROEXPAND-1':
                return eval_macroexpand_1(form, env)
            elif operator.name == 'MACRO-FUNCTION':
                return eval_macro_function(form, env)
            elif operator.name == 'BLOCK':
                return eval_block(form, env)
            elif operator.name == 'RETURN-FROM':
                return eval_return_from(form, env)
            elif operator.name == 'CATCH':
                return eval_catch(form, env)
            elif operator.name == 'THROW':
                return eval_throw(form, env)
            elif operator.name == 'UNWIND-PROTECT':
                return eval_unwind_protect(form, env)
            elif operator.name == 'MULTIPLE-VALUE-CALL':
                return eval_multiple_value_call(form, env)
            elif operator.name == 'MULTIPLE-VALUE-BIND':
                return eval_multiple_value_bind(form, env)
            elif operator.name == 'SIGNAL':
                return eval_signal(form, env)
            elif operator.name == 'ERROR':
                return eval_error(form, env)
            elif operator.name == 'CERROR':
                return eval_cerror(form, env)
            elif operator.name == 'WARN':
                return eval_warn(form, env)
            elif operator.name == 'RESTART-CASE':
                return eval_restart_case(form, env)
            elif operator.name == 'RESTART-BIND':
                return eval_restart_bind(form, env)
            elif operator.name == 'INVOKE-RESTART':
                return eval_invoke_restart(form, env)
            elif operator.name == 'ABORT':
                return eval_abort(form, env)
            elif operator.name == 'HANDLER-BIND':
                return eval_handler_bind(form, env)
            elif operator.name == 'HANDLER-CASE':
                return eval_handler_case(form, env)
            elif operator.name == 'IGNORE-ERRORS':
                return eval_ignore_errors(form, env)
            elif operator.name == 'TAGBODY':
                return eval_tagbody(form, env)
            elif operator.name == 'GO':
                return eval_go(form, env)
            elif operator.name == 'INCF':
                return eval_incf(form, env)
            elif operator.name == 'DECF':
                return eval_decf(form, env)
            elif operator.name == 'DO':
                return eval_do(form, env)
            elif operator.name == 'DO*':
                return eval_do_star(form, env)
            elif operator.name == 'DOLIST':
                return eval_dolist(form, env)
            elif operator.name == 'DOTIMES':
                return eval_dotimes(form, env)
            elif operator.name == 'DO-SYMBOLS':
                return eval_do_symbols(form, env)
            elif operator.name == 'DO-EXTERNAL-SYMBOLS':
                return eval_do_external_symbols(form, env)
            elif operator.name == 'DO-ALL-SYMBOLS':
                return eval_do_all_symbols(form, env)
            elif operator.name == 'IN-PACKAGE':
                # IN-PACKAGE is a macro in Common Lisp - it doesn't evaluate its argument
                # (in-package #:cl-test) -> call in_package with symbol CL-TEST
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("IN-PACKAGE requires a package designator")
                name_arg = car(args)
                # Don't evaluate - pass the symbol/keyword directly
                from .utilities_symbols import in_package
                return in_package(name_arg)
            

            elif operator.name == 'SYMBOL-MACROLET':
                # (SYMBOL-MACROLET ((sym1 expansion1) (sym2 expansion2) ...) body-form...)
                # Create symbol-macro bindings in a new environment and evaluate body
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("SYMBOL-MACROLET requires bindings")
                
                bindings_form = car(args)
                body_forms = cdr(args)
                
                # Create a new child environment for the symbol-macrolet scope
                new_env = lisptype.Environment(parent=env)
                
                # Process bindings: ((sym1 expansion1) (sym2 expansion2) ...)
                if _consp_internal(bindings_form):
                    binding_list = bindings_form
                    while _consp_internal(binding_list):
                        binding = car(binding_list)
                        if _consp_internal(binding):
                            sym = car(binding)
                            expansion = car(cdr(binding)) if _consp_internal(cdr(binding)) else lisptype.NIL
                            if isinstance(sym, lisptype.LispSymbol):
                                # Store symbol-macro as a special binding
                                # We'll mark it with a wrapper so lookup knows it's a symbol-macro
                                new_env.add_symbol_macro(sym, expansion)
                        binding_list = cdr(binding_list)
                
                # Evaluate body forms in the new environment with symbol-macros active
                result = lisptype.NIL
                body = body_forms
                while _consp_internal(body):
                    form_in_body = car(body)
                    result = eval(form_in_body, new_env)
                    body = cdr(body)
                
                return result
            elif operator.name == 'MACROLET':
                # (MACROLET ((name lambda-list . body) ...) body-form...)
                # Create local macro bindings in a new environment and evaluate body
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("MACROLET requires bindings")
                
                bindings_form = car(args)
                body_forms = cdr(args)
                
                # Create a new child environment for the macrolet scope
                new_env = lisptype.Environment(parent=env)
                
                # Process macro bindings: ((name lambda-list . body) ...)
                if _consp_internal(bindings_form):
                    binding_list = bindings_form
                    while _consp_internal(binding_list):
                        binding = car(binding_list)
                        if _consp_internal(binding):
                            macro_name = car(binding)
                            rest = cdr(binding)
                            if _consp_internal(rest):
                                lambda_list = car(rest)
                                macro_body = cdr(rest)
                                
                                if isinstance(macro_name, lisptype.LispSymbol):
                                    # Create a macro function from the lambda-list and body
                                    # Similar to DEFMACRO but local to this environment
                                    from .evaluation_special_forms import _create_macro_function
                                    macro_func = _create_macro_function(macro_name, lambda_list, macro_body, new_env)
                                    new_env.add_function(macro_name, macro_func)
                        binding_list = cdr(binding_list)
                
                # Evaluate body forms in the new environment with local macros active
                result = lisptype.NIL
                body = body_forms
                while _consp_internal(body):
                    form_in_body = car(body)
                    result = eval(form_in_body, new_env)
                    body = cdr(body)
                
                return result
            elif operator.name == 'DEFSETF':
                # DEFSETF has two forms:
                # Short form: (DEFSETF access-fn update-fn [documentation])
                # Long form:  (DEFSETF access-fn lambda-list (store-var...) [decl] [doc] form...)
                # Arguments should NOT be evaluated - they are symbol names and code templates
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("DEFSETF requires arguments")
                
                access_fn = car(args)
                rest = cdr(args)
                
                if not isinstance(access_fn, lisptype.LispSymbol):
                    raise lisptype.LispError("DEFSETF: access-fn must be a symbol")
                
                if rest is None or rest == lisptype.NIL:
                    raise lisptype.LispError("DEFSETF requires at least two arguments")
                
                second_arg = car(rest)
                third_and_beyond = cdr(rest)
                
                # Determine if short or long form based on second argument type
                is_short_form = isinstance(second_arg, lisptype.LispSymbol)
                
                # Get or create the global setf-expanders storage
                global_env = env
                while global_env.parent is not None:
                    global_env = global_env.parent
                
                if not hasattr(global_env, 'setf_expanders'):
                    global_env.setf_expanders = {}
                
                if is_short_form:
                    # Short form: (DEFSETF access-fn update-fn [documentation])
                    update_fn = second_arg
                    doc_string = None
                    if _consp_internal(third_and_beyond):
                        doc_form = car(third_and_beyond)
                        if isinstance(doc_form, str):
                            doc_string = doc_form
                    
                    # Store the setf expander info for later use by SETF macro
                    global_env.setf_expanders[access_fn.name] = {
                        'type': 'short',
                        'update_fn': update_fn,
                        'documentation': doc_string
                    }
                else:
                    # Long form: (DEFSETF access-fn lambda-list (store-var...) [decl] [doc] form...)
                    lambda_list = second_arg
                    
                    if not _consp_internal(third_and_beyond):
                        raise lisptype.LispError("DEFSETF long form requires store variables")
                    
                    store_vars = car(third_and_beyond)
                    body = cdr(third_and_beyond)
                    
                    # Parse optional declarations and docstring from body
                    declarations = []
                    doc_string = None
                    actual_body = body
                    
                    while _consp_internal(actual_body):
                        form_item = car(actual_body)
                        if _consp_internal(form_item):
                            op = car(form_item)
                            if isinstance(op, lisptype.LispSymbol) and op.name == 'DECLARE':
                                declarations.append(form_item)
                                actual_body = cdr(actual_body)
                                continue
                        if isinstance(form_item, str) and doc_string is None:
                            doc_string = form_item
                            actual_body = cdr(actual_body)
                            continue
                        break
                    
                    # Store the setf expander info for long form
                    global_env.setf_expanders[access_fn.name] = {
                        'type': 'long',
                        'lambda_list': lambda_list,
                        'store_vars': store_vars,
                        'declarations': declarations,
                        'documentation': doc_string,
                        'body': actual_body,
                        'env': env  # Capture lexical environment
                    }
                
                # Return the access-fn symbol as specified by ANSI CL
                return access_fn
            elif operator.name == 'DEFINE-SETF-EXPANDER':
                # (DEFINE-SETF-EXPANDER access-fn lambda-list [[declaration* | documentation]] form*)
                # This is a macro - arguments should NOT be evaluated
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("DEFINE-SETF-EXPANDER requires arguments")
                
                access_fn = car(args)
                rest = cdr(args)
                
                if not isinstance(access_fn, lisptype.LispSymbol):
                    raise lisptype.LispError("DEFINE-SETF-EXPANDER: access-fn must be a symbol")
                
                if rest is None or rest == lisptype.NIL:
                    raise lisptype.LispError("DEFINE-SETF-EXPANDER requires a lambda-list")
                
                lambda_list = car(rest)
                body = cdr(rest)
                
                # Get or create the global setf-expanders storage
                global_env = env
                while global_env.parent is not None:
                    global_env = global_env.parent
                
                if not hasattr(global_env, 'setf_expanders'):
                    global_env.setf_expanders = {}
                
                # Parse optional declarations and docstring from body
                declarations = []
                doc_string = None
                actual_body = body
                
                while _consp_internal(actual_body):
                    form_item = car(actual_body)
                    if _consp_internal(form_item):
                        op = car(form_item)
                        if isinstance(op, lisptype.LispSymbol) and op.name == 'DECLARE':
                            declarations.append(form_item)
                            actual_body = cdr(actual_body)
                            continue
                    if isinstance(form_item, str) and doc_string is None:
                        doc_string = form_item
                        actual_body = cdr(actual_body)
                        continue
                    break
                
                # Store the setf expander info - similar to long-form DEFSETF
                # but uses &ENVIRONMENT in the lambda list for macro environment
                global_env.setf_expanders[access_fn.name] = {
                    'type': 'expander',
                    'lambda_list': lambda_list,
                    'declarations': declarations,
                    'documentation': doc_string,
                    'body': actual_body,
                    'env': env  # Capture lexical environment
                }
                
                # Return the access-fn symbol as specified by ANSI CL
                return access_fn
            elif operator.name == 'DEFINE-COMPILER-MACRO':
                # (DEFINE-COMPILER-MACRO name lambda-list &body body)
                # Arguments should NOT be evaluated - just return the name
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("DEFINE-COMPILER-MACRO requires a name")
                
                name = car(args)
                # Store compiler macro info if needed (stub for now)
                # Return the name symbol as specified by ANSI CL
                return name
            elif operator.name == 'DEFINE-CONDITION':
                return eval_define_condition(form, env)
            elif operator.name == 'DEFTYPE':
                # (DEFTYPE name lambda-list &body body)
                # Arguments should NOT be evaluated - the name is a symbol
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("DEFTYPE requires a name")
                
                name = car(args)
                if not isinstance(name, lisptype.LispSymbol):
                    raise lisptype.LispError("DEFTYPE: name must be a symbol")
                
                rest = cdr(args)
                lambda_list = car(rest) if _consp_internal(rest) else lisptype.NIL
                body = cdr(rest) if _consp_internal(rest) else lisptype.NIL

                # Get or create the global types storage
                global_env = env
                while global_env.parent is not None:
                    global_env = global_env.parent

                if not hasattr(global_env, 'user_types'):
                    global_env.user_types = {}

                # Store a real *expander*, not the raw source.
                #
                # This dict used to hold `lambda_list`/`body`/`env` and nothing
                # anywhere read it, so a DEFTYPE'd name was invisible to both
                # TYPEP and SUBTYPEP -- `(deftype foo () '(integer 0 10))`
                # succeeded and then `(typep 5 'foo)` was NIL. The expander is
                # built by the one macro-lambda-list binder (CLHS 4.2.3: a
                # deftype lambda list is a macro lambda list, except that an
                # omitted &OPTIONAL/&KEY parameter defaults to `*` rather than
                # NIL, which is what `unsupplied_default` supplies). Reusing it
                # is also what gives DEFTYPE &WHOLE, &REST, &KEY,
                # destructuring, the docstring and the implicit BLOCK that
                # `(return-from <type-name> ...)` needs -- all of which
                # ansi-test's deftype.9-.19 exercise.
                from .evaluation_special_forms import _create_macro_function
                wild = lisptype.COMMON_LISP_PACKAGE.intern('*')
                expander = _create_macro_function(
                    name, lambda_list, body, env, unsupplied_default=wild)

                global_env.user_types[name.name] = {
                    'name': name,
                    'lambda_list': lambda_list,
                    'body': body,
                    'env': env,          # Capture lexical environment
                    'expander': expander,
                }

                # Return the name symbol
                return name
            elif operator.name == 'DEFPACKAGE':
                # DEFPACKAGE's option clauses are literal data (CLHS 7.2), not
                # forms to evaluate -- so this stays a special case in the
                # dispatcher rather than a cl_function, exactly as CLAUDE.md's
                # registry note requires for operators like this one.
                from .misc_packages import (
                    _designator_to_string, shadow as _pkg_shadow,
                    shadowing_import as _pkg_shadowing_import,
                )
                from .utilities_symbols import import_symbol as _pkg_import
                from .evaluation_conditions import signal_error_object as _signal_error

                def _signal_package_error(package, message):
                    # PACKAGE-ERROR lives in the *real* condition hierarchy
                    # (lisptype_extended.Error), not the "legacy"
                    # lisptype.LispError one HANDLER-CASE/IGNORE-ERRORS also
                    # catch directly -- signaling it any other way (a bare
                    # `raise`) skips signal_condition() and every handler, and
                    # the condition object then unwinds as a plain, unmatched
                    # Python exception instead of being caught.
                    _signal_error(lisptype.PackageError(package=package, message=message))

                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispProgramError(
                        "DEFPACKAGE: wrong number of arguments (got 0, expected at least 1)")

                name_arg = car(args)
                opt_forms = cdr(args)

                def _clause_items(r):
                    items = []
                    while _consp_internal(r):
                        items.append(car(r))
                        r = cdr(r)
                    return items

                def _key_name(key):
                    if isinstance(key, (lisptype.lispKeyword, lisptype.LispSymbol)):
                        n = key.name.upper()
                        return n[1:] if n.startswith(':') else n
                    return str(key).upper()

                pkg_name = _designator_to_string(name_arg)

                nicknames = []
                use_names = []
                export_names = []
                intern_names = []
                shadow_names = []
                shadowing_import_clauses = []  # [(pkg_name, [names])]
                import_from_clauses = []       # [(pkg_name, [names])]
                size_seen = False
                doc_seen = False

                cur = opt_forms
                while _consp_internal(cur):
                    clause = car(cur)
                    if _consp_internal(clause):
                        key_name = _key_name(car(clause))
                        rest_items = [_designator_to_string(i) if key_name not in
                                      ('SHADOWING-IMPORT-FROM', 'IMPORT-FROM', 'SIZE', 'DOCUMENTATION')
                                      else i
                                      for i in _clause_items(cdr(clause))]

                        if key_name == 'USE':
                            use_names.extend(rest_items)
                        elif key_name == 'NICKNAMES':
                            nicknames.extend(rest_items)
                        elif key_name == 'INTERN':
                            intern_names.extend(rest_items)
                        elif key_name == 'EXPORT':
                            export_names.extend(rest_items)
                        elif key_name == 'SHADOW':
                            shadow_names.extend(rest_items)
                        elif key_name == 'SHADOWING-IMPORT-FROM':
                            raw = _clause_items(cdr(clause))
                            if not raw:
                                raise lisptype.LispProgramError(
                                    "DEFPACKAGE: :SHADOWING-IMPORT-FROM requires a package name")
                            shadowing_import_clauses.append(
                                (_designator_to_string(raw[0]),
                                 [_designator_to_string(i) for i in raw[1:]]))
                        elif key_name == 'IMPORT-FROM':
                            raw = _clause_items(cdr(clause))
                            if not raw:
                                raise lisptype.LispProgramError(
                                    "DEFPACKAGE: :IMPORT-FROM requires a package name")
                            import_from_clauses.append(
                                (_designator_to_string(raw[0]),
                                 [_designator_to_string(i) for i in raw[1:]]))
                        elif key_name == 'SIZE':
                            if size_seen:
                                raise lisptype.LispProgramError(
                                    "DEFPACKAGE: :SIZE may only be given once")
                            size_seen = True
                        elif key_name == 'DOCUMENTATION':
                            if doc_seen:
                                raise lisptype.LispProgramError(
                                    "DEFPACKAGE: :DOCUMENTATION may only be given once")
                            doc_seen = True
                        # An unrecognized option keyword is left alone rather
                        # than made an error -- CLHS reserves this space for
                        # implementation extensions and no ANSI test exercises
                        # rejecting one.
                    cur = cdr(cur)

                # CLHS 7.2p2: these four options' names must be pairwise
                # disjoint, and :EXPORT/:INTERN must be disjoint -- checked
                # before any mutation so a malformed DEFPACKAGE has no partial
                # effect.
                shadow_set = set(shadow_names)
                shadowing_import_set = {n for _, names in shadowing_import_clauses for n in names}
                import_set = {n for _, names in import_from_clauses for n in names}
                intern_set = set(intern_names)
                export_set = set(export_names)

                def _require_disjoint(name_a, set_a, name_b, set_b):
                    overlap = set_a & set_b
                    if overlap:
                        raise lisptype.LispProgramError(
                            f"DEFPACKAGE: {name_a} and {name_b} must be disjoint "
                            f"(shared {sorted(overlap)!r})")

                _require_disjoint('SHADOW', shadow_set, 'SHADOWING-IMPORT-FROM', shadowing_import_set)
                _require_disjoint('SHADOW', shadow_set, 'IMPORT-FROM', import_set)
                _require_disjoint('SHADOW', shadow_set, 'INTERN', intern_set)
                _require_disjoint('SHADOWING-IMPORT-FROM', shadowing_import_set, 'IMPORT-FROM', import_set)
                _require_disjoint('SHADOWING-IMPORT-FROM', shadowing_import_set, 'INTERN', intern_set)
                _require_disjoint('IMPORT-FROM', import_set, 'INTERN', intern_set)
                _require_disjoint('EXPORT', export_set, 'INTERN', intern_set)

                # A nickname (or the name itself) that already denotes a
                # different existing package is a PACKAGE-ERROR (CLHS
                # MAKE-PACKAGE/DEFPACKAGE); genuinely *continuable* handling
                # needs the restart machinery M8 owns, so this signals the
                # condition honestly rather than silently accepting the clash.
                existing = lisptype.find_package(pkg_name)
                for nn in nicknames:
                    clash = lisptype.find_package(nn)
                    if clash is not None and clash is not existing:
                        _signal_package_error(nn, f"A package named {nn!r} already exists")

                pkg = existing if existing is not None else lisptype.make_package(pkg_name)
                if nicknames:
                    # Merge rather than overwrite: DEFPACKAGE allows multiple
                    # :NICKNAMES clauses, and each contributes its names.
                    merged = list(pkg.nick_names) if existing is not None else []
                    for nn in nicknames:
                        if nn not in merged:
                            merged.append(nn)
                    pkg.nick_names = merged

                use_packages = []
                for use_pkg_name in use_names:
                    use_pkg = lisptype.find_package(use_pkg_name)
                    if use_pkg is None:
                        use_pkg = lisptype.make_package(use_pkg_name)
                    if use_pkg not in use_packages:
                        use_packages.append(use_pkg)
                pkg.use_packages = use_packages

                for sym_name in intern_names:
                    pkg.intern(sym_name, external=False)
                for sym_name in export_names:
                    pkg.intern(sym_name, external=True)
                if shadow_names:
                    _pkg_shadow([lisptype.LispString(n) for n in shadow_names], pkg)

                for src_name, names in shadowing_import_clauses:
                    src_pkg = lisptype.find_package(src_name)
                    if src_pkg is None:
                        _signal_package_error(src_name, f"No package named {src_name!r}")
                    syms = []
                    for n in names:
                        sym, status = src_pkg.find_symbol(n)
                        if sym is None:
                            _signal_package_error(
                                src_name, f"{n!r} is not accessible in package {src_name!r}")
                        syms.append(sym)
                    if syms:
                        _pkg_shadowing_import(syms, pkg)

                for src_name, names in import_from_clauses:
                    src_pkg = lisptype.find_package(src_name)
                    if src_pkg is None:
                        _signal_package_error(src_name, f"No package named {src_name!r}")
                    syms = []
                    for n in names:
                        sym, status = src_pkg.find_symbol(n)
                        if sym is None:
                            _signal_package_error(
                                src_name, f"{n!r} is not accessible in package {src_name!r}")
                        syms.append(sym)
                    if syms:
                        _pkg_import(syms, pkg)

                return pkg
        
        # Macro handling: if operator names a macro function, expand first
        if isinstance(operator, lisptype.LispSymbol):
            func_binding = env.find_func(operator)
            # Unwrap MultipleValues in single-value context
            if isinstance(func_binding, lisptype.MultipleValues):
                func_binding = func_binding.get_primary()
            if callable(func_binding) and getattr(func_binding, '__is_macro__', False):
                # Gather raw args (without evaluating)
                raw_args = []
                current = args
                while _consp_internal(current):
                    raw_args.append(car(current))
                    current = cdr(current)
                try:
                    expects_whole = getattr(func_binding, '__expects_whole__', False)
                    expects_env = getattr(func_binding, '__expects_environment__', False)
                    
                    # Build call arguments based on macro function expectations
                    call_args = []
                    if expects_whole:
                        call_args.append(form)
                    call_args.extend(raw_args)
                    
                    # If macro expects expansion-time environment, append it as trailing arg
                    if expects_env:
                        call_args.append(env)
                    
                    expanded = func_binding(*call_args)
                except TypeError:
                    # Defensive fallback: call without whole/env if signature mismatch
                    if getattr(func_binding, '__expects_whole__', False):
                        expanded = func_binding(form, *raw_args)
                    else:
                        expanded = func_binding(*raw_args)

                # Evaluate the expansion in the current environment
                return eval(expanded, env)

        # Regular function call
        # In Common Lisp, function position uses the FUNCTION namespace, not variable namespace
        if isinstance(operator, lisptype.LispSymbol):
            # Look up in function namespace directly
            func = env.find_func(operator)
            if func is None:
                # Try registry fallback
                try:
                    py_name = _registry.get_function_py_name(operator.name)
                    if py_name:
                        # Ensure environment is populated
                        try:
                            lispenv.setup_standard_environment()
                        except Exception:
                            pass
                        
                        # Try environment lookup again
                        func = env.find_func(operator)
                        
                        # If still not found and we're in a child environment, 
                        # check the parent/global environment
                        if func is None:
                            global_env = env
                            while global_env.parent is not None:
                                global_env = global_env.parent
                            if global_env is not env:
                                func = global_env.find_func(operator)
                        
                        # If STILL not found, try to get it from lispfunc directly
                        if func is None:
                            fn = getattr(lispfunc, py_name, None)
                            if fn is None:
                                # Try importing submodules
                                try:
                                    import importlib
                                    for sub in ('core', 'math', 'sequences', 'vectors', 'streams', 'pathnames', 'hashtables', 'evaluation', 'comparison', 'characters', 'io', 'io_read', 'io_write', 'utilities', 'classes', 'misc_macros'):
                                        try:
                                            mod = importlib.import_module(f'fclpy.lispfunc.{sub}')
                                            fn = getattr(mod, py_name, None)
                                            if fn:
                                                try:
                                                    setattr(lispfunc, py_name, fn)
                                                except Exception:
                                                    pass
                                                break
                                        except Exception:
                                            continue
                                except Exception:
                                    pass
                            if fn:
                                # Add to both current and global environment
                                env.add_function(operator, fn)
                                func = fn
                except Exception:
                    pass
        else:
            # For non-symbol operators (e.g., lambda forms), evaluate to get function
            func = eval(operator, env)
        
        # If the function result is MultipleValues, extract the primary value
        # In Common Lisp, multiple values in single-value context reduce to the primary value
        if isinstance(func, lisptype.MultipleValues):
            func = func.get_primary()
        
        # Verify we have a callable function before proceeding
        if func is None or not callable(func):
            # When func is None, it means the symbol has no function binding
            if isinstance(operator, lisptype.LispSymbol):
                # Signal an UNDEFINED-FUNCTION condition so Lisp handlers can match it
                # Per ANSI spec, cell-error-name should return the actual symbol, not just its name string
                cond = lisptype.UndefinedFunction(name=operator, message=f"Undefined function {operator.name if hasattr(operator, 'name') else str(operator)} in package {getattr(operator, 'package', None)}")
                raise ConditionException(cond, recoverable=False)
            raise lisptype.LispError(f"Not a function: {operator}")
        
        # Evaluate arguments left to right. Ordinary function-call arguments
        # are single-value contexts: a MultipleValues result reduces to its
        # primary value (NIL if it returned zero values), per ANSI.
        eval_args = []
        current = args
        while _consp_internal(current):
            arg_val = eval(car(current), env)
            if isinstance(arg_val, lisptype.MultipleValues):
                _mv = arg_val.get_all()
                arg_val = _mv[0] if _mv else lisptype.NIL
            eval_args.append(arg_val)
            current = cdr(current)

        # Split evaluated arguments into positionals and &key pairs -- the
        # one shared decision (split_keyword_args), also used by APPLY and
        # FUNCALL so an indirect call recognizes keywords the same way a
        # direct one does.
        eval_args, kwargs = split_keyword_args(func, eval_args)

        # Call function with exception handling
        try:
            if kwargs:
                result = func(*eval_args, **kwargs)
            else:
                result = func(*eval_args)
        except ConditionException:
            # Re-raise Lisp conditions without wrapping them
            raise
        except (ReturnFromException, ThrowException, GoException):
            # Control-flow exceptions used for non-local exits should
            # propagate unchanged so enclosing forms like BLOCK, CATCH,
            # and TAGBODY can handle them.
            raise
        except lisptype.LispProgramError as e:
            # Convert Lisp program errors to PROGRAM-ERROR condition
            condition = lisptype.ProgramError(message=str(e))
            raise ConditionException(condition, recoverable=False)
        except lisptype.LispTypeError as e:
            # Convert Lisp type errors to TYPE-ERROR condition
            condition = lisptype.TypeError(
                datum=getattr(e, 'actual_value', None), 
                expected_type=getattr(e, 'expected_type', None), 
                message=str(e)
            )
            raise ConditionException(condition, recoverable=False)
        except lisptype.LispNotImplementedError as e:
            # Convert not implemented errors to appropriate condition
            condition = lisptype.Error(message=f"Not implemented: {str(e)}")
            raise ConditionException(condition, recoverable=False)
        except lisptype.LispError as e:
            # Convert other Lisp errors to Error condition
            condition = lisptype.Error(message=str(e))
            raise ConditionException(condition, recoverable=False)
        except TypeError as e:
            # Check if this is an argument count error (function signature problem)
            error_str = str(e)
            if is_arity_mismatch_message(error_str):
                # Argument count mismatch - PROGRAM-ERROR per ANSI CL spec
                condition = lisptype.ProgramError(message=error_str)
                raise ConditionException(condition, recoverable=False)
            else:
                # Other TypeErrors (e.g., type mismatches in function calls)
                condition = lisptype.TypeError(
                    datum=None,
                    expected_type='callable',
                    message=error_str
                )
                raise ConditionException(condition, recoverable=False)
        except AttributeError as e:
            # Handle attribute errors
            condition = lisptype.Error(message=f"Attribute error: {str(e)}")
            raise ConditionException(condition, recoverable=False)
        except ZeroDivisionError as e:
            # Handle division by zero
            condition = lisptype.DivisionByZero(message=str(e))
            raise ConditionException(condition, recoverable=False)
        except ArithmeticError as e:
            # Handle other arithmetic errors
            condition = lisptype.ArithmeticError(message=str(e))
            raise ConditionException(condition, recoverable=False)
        except Exception as e:
            # Catch-all for any other Python exceptions
            condition = lisptype.Error(message=f"Python error in function call: {type(e).__name__}: {str(e)}")
            raise ConditionException(condition, recoverable=False)

        # Normalize common Python return values into Lisp objects.
        if result is None:
            return lisptype.NIL
        if isinstance(result, bool):
            return lisptype.T if result else lisptype.NIL
        return result
    
    return form


def coerce_to_function(function, caller_name='FUNCALL'):
    """Resolve a CLHS "function designator" to a Python callable.

    A function designator is either a function object or a symbol naming a
    function in the function namespace. This is the one place that
    resolution happens; APPLY/FUNCALL use it directly, and every site
    elsewhere that accepts a :test/:test-not/:key/predicate designator
    (FIND, SORT, REDUCE, the set operations, ...) should coerce through
    this instead of assuming its argument is already callable -- see
    plan.md finding X2.
    """
    # If function is MultipleValues, extract the primary value (single-value context)
    if isinstance(function, lisptype.MultipleValues):
        function = function.get_primary()

    # If function is a symbol, look up its function binding
    if isinstance(function, lisptype.LispSymbol):
        env = state.current_environment
        func = env.find_func(function) if env is not None else None
        if func is None:
            raise ConditionException(
                lisptype.UndefinedFunction(name=function.name),
                recoverable=False
            )
        function = func

    # If function is nil or otherwise not callable, signal a PROGRAM-ERROR
    if function is None or function == lisptype.NIL or not callable(function):
        condition = lisptype.ProgramError(message=f"{caller_name} requires a function designator, got: {function}")
        raise ConditionException(condition, recoverable=False)

    return function


@_registry.cl_function('APPLY')
def apply(function, *args):
    """Apply function to arguments."""
    function = coerce_to_function(function, 'APPLY')

    try:
        if args and hasattr(args[-1], '__iter__'):
            # Last argument is a list of arguments
            all_args = list(args[:-1]) + list(args[-1])
        else:
            all_args = list(args)
        # CLHS 3.4.1.4: recognize &key pairs in the flattened argument list
        # the same way a direct call would -- see split_keyword_args.
        pos_args, kwargs = split_keyword_args(function, all_args)
        return function(*pos_args, **kwargs)
    except ConditionException:
        # Re-raise Lisp conditions without wrapping them
        raise
    except (ReturnFromException, ThrowException, GoException):
        # Allow non-local control-flow exceptions to propagate to enclosing
        # Lisp control forms (BLOCK/CATCH/TAGBODY) instead of being wrapped.
        raise
    except lisptype.LispProgramError as e:
        # Convert Lisp program errors to PROGRAM-ERROR condition
        condition = lisptype.ProgramError(message=str(e))
        raise ConditionException(condition, recoverable=False)
    except lisptype.LispTypeError as e:
        # Convert Python-level LispTypeError into Lisp TYPE-ERROR condition
        condition = lisptype.TypeError(datum=getattr(e, 'actual_value', None), expected_type=getattr(e, 'expected_type', None), message=str(e))
        raise ConditionException(condition, recoverable=False)
    except lisptype.LispError as e:
        # Convert other Lisp errors to Error condition
        condition = lisptype.Error(message=str(e))
        raise ConditionException(condition, recoverable=False)
    except TypeError as e:
        # Check if this is an argument count error (function signature problem)
        error_str = str(e)
        if ('missing' in error_str.lower() and 'argument' in error_str.lower()) or \
           ('takes' in error_str.lower() and 'argument' in error_str.lower()):
            # Argument count mismatch - PROGRAM-ERROR per ANSI CL spec
            condition = lisptype.ProgramError(message=error_str)
            raise ConditionException(condition, recoverable=False)
        else:
            # Other TypeErrors (e.g., type mismatches in function calls)
            condition = lisptype.TypeError(
                datum=None,
                expected_type='callable',
                message=error_str
            )
            raise ConditionException(condition, recoverable=False)
    except Exception as e:
        # Catch-all for any other Python exceptions
        condition = lisptype.Error(message=f"Python error in APPLY: {type(e).__name__}: {str(e)}")
        raise ConditionException(condition, recoverable=False)


@_registry.cl_function('FUNCALL')
def funcall(function, *args):
    """Call function with arguments."""
    function = coerce_to_function(function, 'FUNCALL')

    try:
        # CLHS 3.4.1.4: recognize &key pairs among the call arguments the
        # same way a direct call would -- see split_keyword_args.
        pos_args, kwargs = split_keyword_args(function, list(args))
        return function(*pos_args, **kwargs)
    except ConditionException:
        # Re-raise Lisp conditions without wrapping them
        raise
    except (ReturnFromException, ThrowException, GoException):
        # Allow non-local control-flow exceptions to propagate to enclosing
        # Lisp control forms (BLOCK/CATCH/TAGBODY) instead of being wrapped.
        raise
    except lisptype.LispProgramError as e:
        # Convert Lisp program errors to PROGRAM-ERROR condition
        condition = lisptype.ProgramError(message=str(e))
        raise ConditionException(condition, recoverable=False)
    except lisptype.LispTypeError as e:
        condition = lisptype.TypeError(datum=getattr(e, 'actual_value', None), expected_type=getattr(e, 'expected_type', None), message=str(e))
        raise ConditionException(condition, recoverable=False)
    except lisptype.LispError as e:
        # Convert other Lisp errors to Error condition
        condition = lisptype.Error(message=str(e))
        raise ConditionException(condition, recoverable=False)
    except TypeError as e:
        # Check if this is an argument count error (function signature problem)
        error_str = str(e)
        if ('missing' in error_str.lower() and 'argument' in error_str.lower()) or \
           ('takes' in error_str.lower() and 'argument' in error_str.lower()):
            # Argument count mismatch - PROGRAM-ERROR per ANSI CL spec
            condition = lisptype.ProgramError(message=error_str)
            raise ConditionException(condition, recoverable=False)
        else:
            # Other TypeErrors (e.g., type mismatches in function calls)
            condition = lisptype.TypeError(
                datum=None,
                expected_type='callable',
                message=error_str
            )
            raise ConditionException(condition, recoverable=False)
    except Exception as e:
        # Catch-all for any other Python exceptions
        condition = lisptype.Error(message=f"Python error in FUNCALL: {type(e).__name__}: {str(e)}")
        raise ConditionException(condition, recoverable=False)


# Aliases for functions that may have different names in lispenv.py
def apply_fn(function, *args):
    """Apply function (alias for apply)."""
    return apply(function, *args)


def eval_fn(form, env=None):
    """Eval function (alias)."""
    return eval(form, env)


__all__ = [
    # Exception classes
    'ReturnFromException',
    'ThrowException',
    'GoException',
    'ConditionException',
    # Core functions
    'parse_lambda_list',
    'eval',
    'apply',
    'funcall',
    'apply_fn',
    'eval_fn',
]
