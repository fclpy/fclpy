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

# Register special operator handlers into the builtin registry
from . import registry as _registry
import fclpy.lispfunc as lispfunc

logger = logging.getLogger(__name__)

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


def get_func_signature_info(func):
    """Get signature info for a function, using cached helper."""
    return _get_func_signature_info(id(func), func)


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
        eval_defvar, eval_defparameter, eval_defconstant, eval_defstruct, eval_pop,
        eval_incf, eval_decf, eval_defclass, eval_defgeneric, eval_defmethod, eval_define_method_combination,
        eval_destructuring_bind
    )
    from .evaluation_control_flow import (
        eval_block, eval_return_from, eval_catch, eval_throw,
        eval_unwind_protect, eval_tagbody, eval_go
    )
    from .evaluation_loops_conditionals import (
        eval_when, eval_unless, eval_cond, eval_case, eval_and, eval_or,
        eval_progn, eval_locally, eval_prog1, eval_prog2, eval_time, eval_let, eval_letstar, eval_quasiquote,
        eval_loop, eval_eval_when, eval_do, eval_do_star, eval_dolist, eval_dotimes,
        eval_do_symbols, eval_do_external_symbols, eval_do_all_symbols,
        eval_flet, eval_labels
    )
    from .evaluation_conditions import (
        eval_signal, eval_error, eval_cerror, eval_warn,
        eval_restart_case, eval_restart_bind, eval_invoke_restart, eval_abort,
        eval_multiple_value_call, eval_multiple_value_bind,
        eval_handler_bind, eval_handler_case, eval_ignore_errors
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
                            elif op_name in ('AREF', 'SVREF'):
                                # (SETF (AREF arr i) val), etc.
                                arr = eval(car(place_args), env)
                                idx = eval(car(cdr(place_args)), env)
                                try:
                                    arr[idx] = result
                                except (TypeError, IndexError) as e:
                                    raise lisptype.LispError(f"SETF {op_name}: {e}")
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
                                    global_env = env
                                    while global_env.parent is not None:
                                        global_env = global_env.parent
                                    global_env.set_variable(sym, result)
                                else:
                                    raise lisptype.LispError("SETF SYMBOL-VALUE: requires a symbol")
                            elif op_name == 'SYMBOL-FUNCTION':
                                sym = eval(car(place_args), env)
                                if isinstance(sym, lisptype.LispSymbol):
                                    env.add_function(sym, result)
                                else:
                                    raise lisptype.LispError("SETF SYMBOL-FUNCTION: requires a symbol")
                            elif op_name == 'SYMBOL-PLIST':
                                sym = eval(car(place_args), env)
                                if isinstance(sym, lisptype.LispSymbol):
                                    sym.plist = result
                                else:
                                    raise lisptype.LispError("SETF SYMBOL-PLIST: requires a symbol")
                            elif op_name == 'GET':
                                sym = eval(car(place_args), env)
                                indicator = eval(car(cdr(place_args)), env)
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
                                # (SETF (GETF plist indicator) val) - complex, just accept
                                pass
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
                                        # Unknown place type - just silently accept (many place types exist)
                                        pass
                        else:
                            raise lisptype.LispNotImplementedError(f"SETF: place operator must be a symbol, got {place_op}")
                    else:
                        raise lisptype.LispNotImplementedError(f"SETF: place must be a symbol or form, got {place}")
                    
                    args = cdr(cdr(args))
                
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
            elif operator.name == 'AND':
                return eval_and(form, env)
            elif operator.name == 'OR':
                return eval_or(form, env)
            elif operator.name == 'PROG1':
                return eval_prog1(form, env)
            elif operator.name == 'PROG2':
                return eval_prog2(form, env)
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
            elif operator.name == 'LOOP':
                return eval_loop(form, env)
            elif operator.name == 'POP':
                return eval_pop(form, env)
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
                # (DEFINE-CONDITION name parent-types slot-specs &rest options)
                # Arguments should NOT be evaluated - the name is a symbol
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("DEFINE-CONDITION requires a name")
                
                name = car(args)
                if not isinstance(name, lisptype.LispSymbol):
                    raise lisptype.LispError("DEFINE-CONDITION: name must be a symbol")
                
                # Get parent types and slot specs (unevaluated)
                rest = cdr(args)
                parent_types = car(rest) if _consp_internal(rest) else lisptype.NIL
                rest2 = cdr(rest) if _consp_internal(rest) else lisptype.NIL
                slot_specs = car(rest2) if _consp_internal(rest2) else lisptype.NIL
                
                # Get or create the global conditions storage
                global_env = env
                while global_env.parent is not None:
                    global_env = global_env.parent
                
                if not hasattr(global_env, 'conditions'):
                    global_env.conditions = {}
                
                # Store condition definition
                global_env.conditions[name.name] = {
                    'name': name,
                    'parent_types': parent_types,
                    'slot_specs': slot_specs
                }
                
                # Return the name symbol
                return name
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
                
                # Store type definition
                global_env.user_types[name.name] = {
                    'name': name,
                    'lambda_list': lambda_list,
                    'body': body,
                    'env': env  # Capture lexical environment
                }
                
                # Return the name symbol
                return name
            elif operator.name == 'DEFPACKAGE':
                # DEFPACKAGE is a macro in Common Lisp - option clauses must not be evaluated.
                # Example: (DEFPACKAGE 'FOO (:USE 'CL) (:INTERN 'A 'B) (:EXPORT 'A))
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("DEFPACKAGE requires a package name")

                name_arg = car(args)
                opt_forms = cdr(args)

                def _unquote(x):
                    if isinstance(x, lisptype.lispCons):
                        op = car(x)
                        if isinstance(op, lisptype.LispSymbol) and op.name == 'QUOTE':
                            qargs = cdr(x)
                            if qargs is not None and qargs != lisptype.NIL:
                                return car(qargs)
                    return x

                def _designator_to_name(x):
                    x = _unquote(x)
                    if isinstance(x, lisptype.lispKeyword):
                        return x.name if hasattr(x, 'name') else str(x)
                    if isinstance(x, lisptype.LispSymbol):
                        return x.name if hasattr(x, 'name') else str(x)
                    if isinstance(x, str):
                        return x
                    return str(x)

                pkg_name = _designator_to_name(name_arg)
                pkg = lisptype.make_package(pkg_name)

                cur = opt_forms
                while _consp_internal(cur):
                    clause = car(cur)
                    if _consp_internal(clause):
                        key = car(clause)
                        rest = cdr(clause)

                        if isinstance(key, lisptype.lispKeyword):
                            key_name = key.name.upper()
                        elif isinstance(key, lisptype.LispSymbol):
                            key_name = key.name.upper()
                            if key_name.startswith(':'):
                                key_name = key_name[1:]
                        else:
                            key_name = str(key).upper()

                        if key_name == 'USE':
                            use_list = []
                            r = rest
                            while _consp_internal(r):
                                use_list.append(_designator_to_name(car(r)))
                                r = cdr(r)
                            pkg.use_packages = []
                            for use_pkg_name in use_list:
                                use_pkg = lisptype.find_package(use_pkg_name)
                                if use_pkg is None:
                                    use_pkg = lisptype.make_package(use_pkg_name)
                                if use_pkg not in pkg.use_packages:
                                    pkg.use_packages.append(use_pkg)

                        elif key_name == 'NICKNAMES':
                            nicknames = []
                            r = rest
                            while _consp_internal(r):
                                nicknames.append(_designator_to_name(car(r)))
                                r = cdr(r)
                            pkg.nick_names = nicknames

                        elif key_name == 'INTERN':
                            r = rest
                            while _consp_internal(r):
                                sym_name = _designator_to_name(car(r))
                                pkg.intern(sym_name, external=False)
                                r = cdr(r)

                        elif key_name == 'EXPORT':
                            r = rest
                            while _consp_internal(r):
                                sym_name = _designator_to_name(car(r))
                                pkg.intern(sym_name, external=True)
                                r = cdr(r)

                    cur = cdr(cur)

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
                    # If the macro expects the whole form (via &WHOLE), pass it
                    if getattr(func_binding, '__expects_whole__', False):
                        expanded = func_binding(form, *raw_args)
                    else:
                        expanded = func_binding(*raw_args)
                except TypeError:
                    # Defensive fallback: call without whole if signature mismatch
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
        
        # Get cached signature info for keyword argument handling
        use_kwargs, kwarg_param_names, num_required_positionals = get_func_signature_info(func)
        
        # Evaluate arguments
        eval_args = []
        kwargs = {}
        current = args
        
        while _consp_internal(current):
            arg_val = eval(car(current), env)
            
            # Only treat a keyword as a Python kwarg if:
            # 1. The function accepts kwargs
            # 2. We've already filled all required positional parameters
            # 3. The keyword name matches an actual parameter name, OR function has **kwargs
            if (use_kwargs and isinstance(arg_val, lisptype.lispKeyword) 
                and len(eval_args) >= num_required_positionals):
                # Convert keyword name to Python kwarg name format
                py_key = arg_val.name.lower().replace('-', '_')
                
                # Treat as kwarg if this matches a function parameter OR function accepts **kwargs ('*')
                if py_key in kwarg_param_names or '*' in kwarg_param_names:
                    # Get the next argument as the value
                    current = cdr(current)
                    if _consp_internal(current):
                        key_val = eval(car(current), env)
                        kwargs[py_key] = key_val
                    else:
                        # Keyword at end with no value - pass as positional
                        eval_args.append(arg_val)
                else:
                    # Keyword doesn't match a param, pass as positional value
                    eval_args.append(arg_val)
            else:
                eval_args.append(arg_val)
            
            current = cdr(current)
        
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


@_registry.cl_function('APPLY')
def apply(function, *args):
    """Apply function to arguments."""
    # If function is MultipleValues, extract the primary value (single-value context)
    if isinstance(function, lisptype.MultipleValues):
        function = function.get_primary()
    
    try:
        if args and hasattr(args[-1], '__iter__'):
            # Last argument is a list of arguments
            all_args = list(args[:-1]) + list(args[-1])
            return function(*all_args)
        else:
            return function(*args)
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
    # If function is MultipleValues, extract the primary value (single-value context)
    if isinstance(function, lisptype.MultipleValues):
        function = function.get_primary()
    # If function is nil or otherwise not callable, signal a PROGRAM-ERROR
    if function is None or function == lisptype.NIL or not callable(function):
        condition = lisptype.ProgramError(message=f"FUNCALL requires a function designator, got: {function}")
        raise ConditionException(condition, recoverable=False)
    
    try:
        return function(*args)
    except ConditionException:
        # Re-raise Lisp conditions without wrapping them
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
