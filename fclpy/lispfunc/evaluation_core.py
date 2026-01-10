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
import inspect
from functools import lru_cache

# Register special operator handlers into the builtin registry
from . import registry as _registry
import fclpy.lispfunc as lispfunc


# Cache for function signature information to avoid repeated inspect.signature calls
@lru_cache(maxsize=1024)
def _get_func_signature_info(func_id: int, func):
    """Get cached signature information for a function.
    
    Returns a tuple of (use_kwargs, kwarg_param_names_frozenset).
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
        return (use_kwargs, frozenset(kwarg_param_names))
    except (ValueError, TypeError):
        return (False, frozenset())


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
            elif marker == '&WHOLE':
                # &WHOLE takes a single following symbol which is bound to the
                # entire macro form; consume that symbol and record it.
                next_sym = car(cdr(current)) if _consp_internal(cdr(current)) else None
                if isinstance(next_sym, lisptype.LispSymbol):
                    whole = next_sym
                # Advance past &WHOLE and its parameter
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

    # Include whole in returned structure so macro handling can bind it
    return {
        'required': required,
        'optional': optional,
        'rest': rest,
        'keyword': keyword,
        'aux': aux,
        'whole': whole
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
        eval_defvar, eval_defparameter, eval_defconstant, eval_defstruct, eval_pop,
        eval_incf, eval_decf, eval_defclass, eval_defgeneric, eval_defmethod, eval_define_method_combination,
        eval_destructuring_bind
    )
    from .evaluation_control_flow import (
        eval_block, eval_return_from, eval_catch, eval_throw,
        eval_unwind_protect, eval_tagbody, eval_go
    )
    from .evaluation_loops_conditionals import (
        eval_when, eval_unless, eval_cond, eval_and, eval_or,
        eval_progn, eval_locally, eval_prog1, eval_prog2, eval_let, eval_letstar, eval_quasiquote,
        eval_loop, eval_eval_when, eval_do, eval_do_star, eval_dolist, eval_dotimes,
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
    if form is None or isinstance(form, (int, float, str, bool)):
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
                fn = getattr(lispfunc, py_name, None)
                if fn:
                    # Bind into environment for faster future lookups
                    env.add_function(form, fn)
                    return fn
        except Exception:
            # Defensive: if registry lookup fails, ignore and raise below
            pass
        # If not found in either, raise error
        # Debug: if this is the problematic OPT symbol, print surrounding env info
        try:
            if form.name.upper() == 'OPT':
                try:
                    print(f"[DEBUG] Unbound variable lookup for OPT in env: {env}")
                    # Attempt to dump known variable names in this environment
                    vars_list = list(getattr(env, '_variable_map', {}).keys())
                    print(f"[DEBUG] env._variable_map keys: {vars_list}")
                except Exception:
                    pass
        except Exception:
            pass
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
                        import fclpy.lispfunc as lispfunc_module
                        func = getattr(lispfunc_module, py_name, None)
                        if func is not None:
                            return func
                    raise lisptype.LispNotImplementedError(f"Undefined function: {name.name}")
                return name
            elif operator.name == 'SETQ':
                return eval_setq(form, env)
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
                        return x.name
                    if isinstance(x, lisptype.LispSymbol):
                        return x.name
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
            if callable(func_binding) and getattr(func_binding, '__is_macro__', False):
                # Gather raw args (without evaluating)
                raw_args = []
                current = args
                while _consp_internal(current):
                    raw_args.append(car(current))
                    current = cdr(current)
                # Debugging: log raw args for DEFSTRUCT-WITH-TESTS to inspect binding
                try:
                    if operator.name == 'DEFSTRUCT-WITH-TESTS':
                        try:
                            print("[DEBUG] MACRO RAW ARGS for DEFSTRUCT-WITH-TESTS:")
                            for i, a in enumerate(raw_args):
                                print(f"  arg[{i}]: type={type(a)} repr={a}")
                        except Exception:
                            pass

                    # If the macro expects the whole form (via &WHOLE), pass it
                    if getattr(func_binding, '__expects_whole__', False):
                        expanded = func_binding(form, *raw_args)
                    else:
                        expanded = func_binding(*raw_args)

                    # Additional debug: show the expansion for DEFSTRUCT-WITH-TESTS
                    try:
                        if operator.name == 'DEFSTRUCT-WITH-TESTS':
                            print("[DEBUG] MACRO EXPANDED for DEFSTRUCT-WITH-TESTS:")
                            print(repr(expanded))
                    except Exception:
                        pass
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
                        fn = getattr(lispfunc, py_name, None)
                        if fn:
                            env.add_function(operator, fn)
                            func = fn
                except Exception:
                    pass
        else:
            # For non-symbol operators (e.g., lambda forms), evaluate to get function
            func = eval(operator, env)
            
        if not callable(func):
            raise lisptype.LispNotImplementedError(f"Not a function: {operator}")
        
        # Get cached signature info for keyword argument handling
        use_kwargs, kwarg_param_names = get_func_signature_info(func)
        
        # Evaluate arguments
        eval_args = []
        kwargs = {}
        current = args
        
        while _consp_internal(current):
            arg_val = eval(car(current), env)
            
            # Only treat a keyword as a Python kwarg if:
            # 1. The function accepts kwargs
            # 2. The keyword name matches an actual parameter name, OR function has **kwargs
            if use_kwargs and isinstance(arg_val, lisptype.lispKeyword):
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
        
        # Call function
        if kwargs:
            return func(*eval_args, **kwargs)
        else:
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
