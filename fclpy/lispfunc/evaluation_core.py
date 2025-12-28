"""Core Lisp evaluation system - eval, apply, and dispatch.

This module contains the main eval() function that dispatches to special forms
and the apply() function for function application.
"""

import fclpy.state as state
import fclpy.lisptype as lisptype
import fclpy.lispreader as lispreader
from .core import car, cdr, cons, _consp_internal, _atom_internal
import fclpy.lispenv as lispenv  # environment setup utilities
from fclpy.lisptype import resolve_environment, LispEnvironmentError

# Register special operator handlers into the builtin registry
from . import registry as _registry
import fclpy.lispfunc as lispfunc


# Exception classes for non-local exits
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
        self.condition = condition
        self.recoverable = recoverable
        super().__init__(str(condition))


def parse_lambda_list(lambda_list):
    """Parse a Common Lisp lambda list into structured form.
    
    Returns a dict with keys:
    - required: list of required parameter symbols
    - optional: list of optional parameter specs (symbol or [symbol, default])
    - rest: single rest parameter symbol or None
    - keyword: list of keyword parameter specs (symbol or [symbol, default])
    - aux: list of auxiliary parameter specs (symbol or [symbol, init])
    
    Supported format:
    (req1 req2 &optional opt1 (opt2 default2) &rest rest-var 
     &key key1 (key2 default2) &aux (aux1 init1))
    """
    required = []
    optional = []
    rest = None
    keyword = []
    aux = []
    
    # Parse the lambda list
    current_section = 'required'
    current = lambda_list
    
    while _consp_internal(current):
        param = car(current)
        
        # Check for section markers
        if isinstance(param, lisptype.LispSymbol):
            marker = param.name.upper()
            if marker == '&OPTIONAL':
                current_section = 'optional'
                current = cdr(current)
                continue
            elif marker == '&REST':
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
        
        # Add parameter to appropriate section
        if current_section == 'required':
            if isinstance(param, lisptype.LispSymbol):
                required.append(param)
        elif current_section == 'optional':
            if isinstance(param, lisptype.LispSymbol):
                optional.append(param)
            elif _consp_internal(param):
                # Optional with default: (name default)
                optional.append(param)
        elif current_section == 'rest':
            if isinstance(param, lisptype.LispSymbol):
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
    
    return {
        'required': required,
        'optional': optional,
        'rest': rest,
        'keyword': keyword,
        'aux': aux
    }


@_registry.cl_function('EVAL')
def eval(form, env=None):
    """Evaluate a Lisp form.

    An explicit env takes precedence; otherwise the global state.current_environment
    is used. If neither is available a LispEnvironmentError is raised (surfacing a
    clearer message instead of None dereference).
    """
    # Import special form handlers lazily to avoid circular imports
    from .evaluation_special_forms import (
        eval_if, eval_setq, eval_defun, eval_defmacro, eval_macroexpand_1,
        eval_macro_function, eval_lambda, eval_declare, eval_declaim,
        eval_defvar, eval_defparameter
    )
    from .evaluation_control_flow import (
        eval_block, eval_return_from, eval_catch, eval_throw,
        eval_unwind_protect, eval_tagbody, eval_go
    )
    from .evaluation_loops_conditionals import (
        eval_when, eval_unless, eval_cond, eval_and, eval_or,
        eval_progn, eval_prog1, eval_prog2, eval_let, eval_letstar, eval_quasiquote
    )
    from .evaluation_conditions import (
        eval_signal, eval_error, eval_cerror, eval_warn,
        eval_restart_case, eval_restart_bind, eval_invoke_restart, eval_abort,
        eval_multiple_value_call, eval_multiple_value_bind
    )
    
    env = resolve_environment(env)
    
    # Self-evaluating forms
    if form is None or isinstance(form, (int, float, str, bool)):
        return form
    # Keywords are self-evaluating in Common Lisp semantics
    if isinstance(form, lisptype.lispKeyword):
        return form
    
    # Symbols - look up in environment
    if isinstance(form, lisptype.LispSymbol):
        # Check variable bindings first
        value = env.find_variable(form)
        if value is not None:
            return value
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
                fn = getattr(lispfunc, py_name, None)
                if fn:
                    # Bind into environment for faster future lookups
                    env.add_function(form, fn)
                    return fn
        except Exception:
            # Defensive: if registry lookup fails, ignore and raise below
            pass
        # If not found in either, raise error
        raise lisptype.LispNotImplementedError(f"Unbound variable: {form.name}")
    
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
            elif operator.name == 'SETQ':
                return eval_setq(form, env)
            elif operator.name == 'PROGN':
                return eval_progn(form, env)
            elif operator.name == 'LET':
                return eval_let(form, env)
            elif operator.name == 'LET*':
                return eval_letstar(form, env)
            elif operator.name == 'WHEN':
                return eval_when(form, env)
            elif operator.name == 'UNLESS':
                return eval_unless(form, env)
            elif operator.name == 'COND':
                return eval_cond(form, env)
            elif operator.name == 'AND':
                return eval_and(form, env)
            elif operator.name == 'OR':
                return eval_or(form, env)
            elif operator.name == 'PROG1':
                return eval_prog1(form, env)
            elif operator.name == 'PROG2':
                return eval_prog2(form, env)
            elif operator.name == 'DEFVAR':
                return eval_defvar(form, env)
            elif operator.name == 'DEFPARAMETER':
                return eval_defparameter(form, env)
            elif operator.name == 'DEFUN':
                return eval_defun(form, env)
            elif operator.name == 'LAMBDA':
                return eval_lambda(form, env)
            elif operator.name == 'QUASIQUOTE':
                return eval_quasiquote(form, env)
            elif operator.name == 'DEFMACRO':
                return eval_defmacro(form, env)
            elif operator.name == 'DECLARE':
                return eval_declare(form, env)
            elif operator.name == 'DECLAIM':
                return eval_declaim(form, env)
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
            elif operator.name == 'TAGBODY':
                return eval_tagbody(form, env)
            elif operator.name == 'GO':
                return eval_go(form, env)
        
        # Macro handling: if operator names a macro function, expand first
        if isinstance(operator, lisptype.LispSymbol):
            func_binding = env.find_func(operator)
            if callable(func_binding) and getattr(func_binding, '__is_macro__', False):
                # Gather raw args (without evaluating)
                raw_args = []
                current = args
                while _consp_internal(current):
                    raw_args.append(car(current))
                    current = cdr(current)
                expanded = func_binding(*raw_args)
                # If macro returns a tuple/list of forms, wrap as progn
                return eval(expanded, env)

        # Regular function call
        func = eval(operator, env)
        if not callable(func):
            raise lisptype.LispNotImplementedError(f"Not a function: {operator}")
        eval_args = []
        current = args
        while _consp_internal(current):
            eval_args.append(eval(car(current), env))
            current = cdr(current)
        return func(*eval_args)
    
    return form


@_registry.cl_function('APPLY')
def apply(function, *args):
    """Apply function to arguments."""
    if args and hasattr(args[-1], '__iter__'):
        # Last argument is a list of arguments
        all_args = list(args[:-1]) + list(args[-1])
        return function(*all_args)
    else:
        return function(*args)


@_registry.cl_function('FUNCALL')
def funcall(function, *args):
    """Call function with arguments."""
    return function(*args)


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
