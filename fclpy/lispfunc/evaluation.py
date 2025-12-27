"""Lisp evaluation system - eval, special forms, and control structures."""

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
            # TODO: Implement DEFVAR / LET special forms directly; for now raise clearer error
            elif operator.name == 'DEFVAR':
                raise lisptype.LispNotImplementedError('DEFVAR special form not yet implemented in evaluator')
            elif operator.name == 'DEFUN':
                return eval_defun(form, env)
            elif operator.name == 'LAMBDA':
                return eval_lambda(form, env)
            elif operator.name == 'QUASIQUOTE':
                return eval_quasiquote(form, env)
            elif operator.name == 'DEFMACRO':
                return eval_defmacro(form, env)
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


@_registry.cl_special('IF')
def eval_if(form, env):
    """Evaluate IF special form."""
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("IF requires at least 2 arguments")
    
    test_form = car(args)
    then_form = car(cdr(args))
    else_form = car(cdr(cdr(args))) if _consp_internal(cdr(cdr(args))) else None
    
    test_result = eval(test_form, env)
    if test_result is not None and test_result != lisptype.NIL:
        return eval(then_form, env)
    elif else_form is not None:
        return eval(else_form, env)
    else:
        return None


@_registry.cl_special('SETQ')
def eval_setq(form, env):
    """Evaluate SETQ special form."""
    args = cdr(form)
    result = None

    while _consp_internal(args) and _consp_internal(cdr(args)):
        var = car(args)
        value_form = car(cdr(args))

        if not isinstance(var, lisptype.LispSymbol):
            raise lisptype.LispNotImplementedError("SETQ: variable must be a symbol")

        result = eval(value_form, env)
        env.set_variable(var, result)

        args = cdr(cdr(args))

    return result


@_registry.cl_special('DEFUN')
def eval_defun(form, env):
    """Evaluate DEFUN special form."""
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("DEFUN requires at least 2 arguments")
    
    func_name = car(args)
    param_list = car(cdr(args))
    body = cdr(cdr(args))
    
    if not isinstance(func_name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFUN: function name must be a symbol")
    
    # Create function closure
    def user_function(*call_args):
        # Create new environment for function execution
        func_env = lisptype.Environment(env)
        
        # Bind parameters
        params = param_list
        for i, arg in enumerate(call_args):
            if _consp_internal(params):
                param = car(params)
                if isinstance(param, lisptype.LispSymbol):
                    func_env.add_variable(param, arg)
                params = cdr(params)
        
        # Execute body
        result = None
        current_body = body
        while _consp_internal(current_body):
            result = eval(car(current_body), func_env)
            current_body = cdr(current_body)
        
        return result
    
    # Add function to environment
    env.add_function(func_name, user_function)
    return func_name


def eval_defmacro(form, env):
    """Evaluate DEFMACRO special form: register a macro in the environment.

    This creates a Python callable that performs parameter substitution in the
    macro body, returning the substituted form as the expansion (code, not evaluated).
    """
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("DEFMACRO requires a name, lambda-list and body")

    macro_name = car(args)
    lambda_list = car(cdr(args))
    body = cdr(cdr(args))

    if not isinstance(macro_name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFMACRO: macro name must be a symbol")

    # Build parameter name list
    params = []
    cur = lambda_list
    while _consp_internal(cur):
        p = car(cur)
        if isinstance(p, lisptype.LispSymbol):
            params.append(p.name)
        cur = cdr(cur)

    # Helper to substitute params in a form
    def substitute(form, mapping):
        # Symbols: replace if in mapping
        if isinstance(form, lisptype.LispSymbol):
            if form.name in mapping:
                return mapping[form.name]
            return form
        # Cons cell: recurse
        if _consp_internal(form):
            new_car = substitute(car(form), mapping)
            new_cdr = substitute(cdr(form), mapping) if _consp_internal(cdr(form)) or isinstance(cdr(form), lisptype.LispSymbol) else cdr(form)
            return lisptype.lispCons(new_car, new_cdr)
        # Other atoms: return as-is
        return form

    # Create the macro callable
    def macro_callable(*call_args):
        # Map parameter names to raw argument forms
        mapping = {}
        for i, name in enumerate(params):
            if i < len(call_args):
                mapping[name] = call_args[i]
            else:
                mapping[name] = None

        # If no body, return NIL
        if not _consp_internal(body):
            return lisptype.NIL

        # Build substituted body forms
        substituted_forms = []
        cur_body = body
        while _consp_internal(cur_body):
            substituted_forms.append(substitute(car(cur_body), mapping))
            cur_body = cdr(cur_body)

        # Return the last form (simplified PROGN semantics - normally all forms would be wrapped in PROGN)
        if len(substituted_forms) == 1:
            return substituted_forms[0]
        return substituted_forms[-1]

    # Mark as macro and register in environment
    setattr(macro_callable, '__is_macro__', True)
    env.add_function(macro_name, macro_callable)
    return macro_name


def eval_macroexpand_1(form, env):
    """Evaluate MACROEXPAND-1 special form.
    
    (MACROEXPAND-1 form) - expand a macro call one level.
    If form is a macro call, expands the macro and returns the expansion.
    Otherwise returns form unchanged.
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("MACROEXPAND-1 requires 1 argument")
    
    form_to_expand_raw = car(args)
    
    # If the form is (QUOTE x), evaluate it to get x
    # Otherwise, use the form as-is
    if _consp_internal(form_to_expand_raw) and isinstance(car(form_to_expand_raw), lisptype.LispSymbol) and car(form_to_expand_raw).name == 'QUOTE':
        form_to_expand = eval(form_to_expand_raw, env)
    else:
        form_to_expand = form_to_expand_raw
    
    # Only cons cells can be macro calls
    if not _consp_internal(form_to_expand):
        return form_to_expand
    
    operator = car(form_to_expand)
    if not isinstance(operator, lisptype.LispSymbol):
        return form_to_expand
    
    # Try to find the operator function
    macro_func = env.find_func(operator)
    if not macro_func or not callable(macro_func):
        return form_to_expand
    
    # Check if it's actually a macro
    if not getattr(macro_func, '__is_macro__', False):
        return form_to_expand
    
    # Call the macro with unevaluated arguments
    args_list = []
    current = cdr(form_to_expand)
    while _consp_internal(current):
        args_list.append(car(current))
        current = cdr(current)
    
    # If there's a non-nil tail, that's an error, but for now just ignore it
    try:
        return macro_func(*args_list)
    except Exception:
        # If macro expansion fails, return form unchanged
        return form_to_expand


def eval_macro_function(form, env):
    """Evaluate MACRO-FUNCTION special form.
    
    (MACRO-FUNCTION symbol) - return the macro function for a symbol, or NIL if not a macro.
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("MACRO-FUNCTION requires 1 argument")
    
    symbol_form = car(args)
    
    # The symbol form might be quoted, so we need to evaluate it to get the symbol
    # Or it might already be a symbol
    if isinstance(symbol_form, lisptype.LispSymbol):
        symbol = symbol_form
    else:
        # Try evaluating it
        symbol = eval(symbol_form, env)
    
    if not isinstance(symbol, lisptype.LispSymbol):
        return lisptype.NIL
    
    # Try to find the function
    func = env.find_func(symbol)
    if not func or not callable(func):
        return lisptype.NIL
    
    # Check if it's a macro
    if getattr(func, '__is_macro__', False):
        return func
    
    return lisptype.NIL


def eval_block(form, env):
    """Evaluate BLOCK special form: (BLOCK name body-form*)
    
    Establishes a block with the given name. Evaluates body forms in sequence.
    Can be exited early with RETURN-FROM.
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("BLOCK requires at least a name")
    
    block_name = car(args)
    body_forms = cdr(args)
    
    if not isinstance(block_name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError(f"BLOCK name must be a symbol, got {block_name}")
    
    try:
        # Evaluate body forms in sequence
        result = lisptype.NIL
        current = body_forms
        while _consp_internal(current):
            result = eval(car(current), env)
            current = cdr(current)
        return result
    except ReturnFromException as e:
        # Check if this exception is for our block
        if e.tag == block_name or (isinstance(e.tag, lisptype.LispSymbol) and 
                                    isinstance(block_name, lisptype.LispSymbol) and
                                    e.tag.name == block_name.name):
            return e.value
        else:
            # Not for us, re-raise for outer block
            raise


def eval_return_from(form, env):
    """Evaluate RETURN-FROM special form: (RETURN-FROM name value?)
    
    Exits the named BLOCK, returning the specified value (or NIL).
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("RETURN-FROM requires at least a name")
    
    block_name = car(args)
    value_forms = cdr(args)
    
    if not isinstance(block_name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError(f"RETURN-FROM name must be a symbol, got {block_name}")
    
    # Evaluate the value form (default to NIL)
    if _consp_internal(value_forms):
        value = eval(car(value_forms), env)
    else:
        value = lisptype.NIL
    
    # Raise exception to exit the block
    raise ReturnFromException(block_name, value)


def eval_catch(form, env):
    """Evaluate CATCH special form: (CATCH tag body-form*)
    
    Establishes a catch point. Evaluates body forms. If THROW is called
    with matching tag, catches it and returns the thrown value.
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("CATCH requires a tag and optional body forms")
    
    tag_form = car(args)
    body_forms = cdr(args)
    
    # Evaluate the tag form
    tag = eval(tag_form, env)
    
    try:
        # Evaluate body forms in sequence
        result = lisptype.NIL
        current = body_forms
        while _consp_internal(current):
            result = eval(car(current), env)
            current = cdr(current)
        return result
    except ThrowException as e:
        # Check if tag matches
        if e.tag == tag or (isinstance(e.tag, lisptype.LispSymbol) and 
                           isinstance(tag, lisptype.LispSymbol) and
                           e.tag.name == tag.name):
            # Caught! Return the thrown value
            return e.value
        else:
            # Not for us, re-raise for outer catch
            raise


def eval_throw(form, env):
    """Evaluate THROW special form: (THROW tag value)
    
    Throws to the nearest matching CATCH.
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("THROW requires a tag and a value")
    
    tag_form = car(args)
    rest = cdr(args)
    
    if not _consp_internal(rest):
        raise lisptype.LispNotImplementedError("THROW requires a value")
    
    value_form = car(rest)
    
    # Evaluate both tag and value
    tag = eval(tag_form, env)
    value = eval(value_form, env)
    
    # Raise exception
    raise ThrowException(tag, value)


def eval_lambda(form, env):
    """Evaluate LAMBDA special form."""
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("LAMBDA requires at least 1 argument")
    
    param_list = car(args)
    body = cdr(args)
    
    # Create function closure
    def lambda_function(*call_args):
        # Create new environment for function execution
        func_env = lisptype.Environment(env)
        
        # Bind parameters
        params = param_list
        for i, arg in enumerate(call_args):
            if _consp_internal(params):
                param = car(params)
                if isinstance(param, lisptype.LispSymbol):
                    func_env.add_variable(param, arg)
                params = cdr(params)
        
        # Execute body
        result = None
        current_body = body
        while _consp_internal(current_body):
            result = eval(car(current_body), func_env)
            current_body = cdr(current_body)
        
        return result
    
    return lambda_function


def eval_when(form, env):
    """Evaluate WHEN special form."""
    args = cdr(form)
    if not _consp_internal(args):
        return None
    
    test_form = car(args)
    body = cdr(args)
    
    test_result = eval(test_form, env)
    if test_result is not None and test_result != lisptype.NIL:
        result = None
        current_body = body
        while _consp_internal(current_body):
            result = eval(car(current_body), env)
            current_body = cdr(current_body)
        return result
    else:
        return None


def eval_unless(form, env):
    """Evaluate UNLESS special form."""
    args = cdr(form)
    if not _consp_internal(args):
        return None
    
    test_form = car(args)
    body = cdr(args)
    
    test_result = eval(test_form, env)
    if test_result is None or test_result == lisptype.NIL:
        result = None
        current_body = body
        while _consp_internal(current_body):
            result = eval(car(current_body), env)
            current_body = cdr(current_body)
        return result
    else:
        return None


def eval_cond(form, env):
    """Evaluate COND special form."""
    clauses = cdr(form)
    
    while _consp_internal(clauses):
        clause = car(clauses)
        if _consp_internal(clause):
            test = car(clause)
            
            # Special case for T
            if (isinstance(test, lisptype.LispSymbol) and test.name == 'T') or eval(test, env):
                # Execute forms in clause
                result = test if not _consp_internal(cdr(clause)) else None
                forms = cdr(clause)
                while _consp_internal(forms):
                    result = eval(car(forms), env)
                    forms = cdr(forms)
                return result
        
        clauses = cdr(clauses)
    
    return None


def eval_and(form, env):
    """Evaluate AND special form."""
    args = cdr(form)
    result = True  # AND with no arguments is T
    
    while _consp_internal(args):
        result = eval(car(args), env)
        if result is None or result == lisptype.NIL:
            return None
        args = cdr(args)
    
    return result


def eval_or(form, env):
    """Evaluate OR special form."""
    args = cdr(form)
    
    while _consp_internal(args):
        result = eval(car(args), env)
        if result is not None and result != lisptype.NIL:
            return result
        args = cdr(args)
    
    return None


def eval_progn(form, env):
    """Evaluate PROGN special form."""
    args = cdr(form)
    result = None
    
    while _consp_internal(args):
        result = eval(car(args), env)
        args = cdr(args)
    
    return result


def eval_let(form, env):
    """Evaluate LET special form with parallel binding semantics.
    
    (LET ((var1 init1) (var2 init2) ...) body...)
    
    In LET, all init forms are evaluated in the current environment BEFORE
    any bindings are created in the new scope. This is "parallel" binding.
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("LET requires at least a binding list")
    
    bindings_form = car(args)
    body = cdr(args)
    
    # Create new environment for LET scope
    let_env = lisptype.Environment(env)
    
    # Process bindings - evaluate all init forms in OUTER environment first
    bindings_list = []
    current = bindings_form
    while _consp_internal(current):
        binding = car(current)
        if _consp_internal(binding):
            var = car(binding)
            init_form = car(cdr(binding))
            # Evaluate init in OUTER environment
            value = eval(init_form, env)
            bindings_list.append((var, value))
        current = cdr(current)
    
    # Now bind all variables in new environment
    for var, value in bindings_list:
        if isinstance(var, lisptype.LispSymbol):
            let_env.add_variable(var, value)
    
    # Evaluate body in new environment
    result = None
    current = body
    while _consp_internal(current):
        result = eval(car(current), let_env)
        current = cdr(current)
    
    return result


def eval_letstar(form, env):
    """Evaluate LET* special form with sequential binding semantics.
    
    (LET* ((var1 init1) (var2 init2) ...) body...)
    
    In LET*, each init form is evaluated in the environment AFTER previous
    bindings have been established. This is "sequential" binding.
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("LET* requires at least a binding list")
    
    bindings_form = car(args)
    body = cdr(args)
    
    # Create new environment for LET* scope
    letstar_env = lisptype.Environment(env)
    
    # Process bindings sequentially - each can see previous ones
    current = bindings_form
    while _consp_internal(current):
        binding = car(current)
        if _consp_internal(binding):
            var = car(binding)
            init_form = car(cdr(binding))
            # Evaluate init in CURRENT environment (with previous bindings)
            value = eval(init_form, letstar_env)
            if isinstance(var, lisptype.LispSymbol):
                letstar_env.add_variable(var, value)
        current = cdr(current)
    
    # Evaluate body in environment with all bindings
    result = None
    current = body
    while _consp_internal(current):
        result = eval(car(current), letstar_env)
        current = cdr(current)
    
    return result


def eval_quasiquote(form, env):
    """Evaluate a QUASIQUOTE form by processing UNQUOTE and UNQUOTE-SPLICING.

    This is a simplified quasiquote evaluator that handles common patterns:
    - (QUASIQUOTE x) where x is a list will return a new list where elements
      of the form (UNQUOTE e) are replaced with the evaluated value of e,
      and elements of the form (UNQUOTE-SPLICING e) are spliced into the list.
    - Nested quasiquotes are not fully supported beyond a single level.
    """
    expr = car(cdr(form))

    def _quasi(obj):
        # If an explicit (UNQUOTE e) form, evaluate and return its value
        if _consp_internal(obj) and isinstance(car(obj), lisptype.LispSymbol) and car(obj).name == 'UNQUOTE':
            return eval(car(cdr(obj)), env)

        # If atom, return as-is
        if not _consp_internal(obj):
            return obj

        # Otherwise obj is a cons/list: build a resulting list applying unquote rules
        parts = []
        cur = obj
        while _consp_internal(cur):
            item = car(cur)
            # Handle (UNQUOTE-SPLICING e)
            if _consp_internal(item) and isinstance(car(item), lisptype.LispSymbol):
                name = car(item).name
                if name == 'UNQUOTE-SPLICING':
                    val = eval(car(cdr(item)), env)
                    # If val is NIL, splice nothing (empty)
                    if val is lisptype.NIL or val is None:
                        pass  # Don't append anything
                    # If val is a lispCons, iterate its elements
                    elif isinstance(val, lisptype.lispCons):
                        for v in val:
                            parts.append(v)
                    elif isinstance(val, (list, tuple)):
                        for v in val:
                            parts.append(v)
                    else:
                        parts.append(val)
                    cur = cdr(cur)
                    continue
                elif name == 'UNQUOTE':
                    val = eval(car(cdr(item)), env)
                    parts.append(val)
                    cur = cdr(cur)
                    continue

            # Otherwise, recursively quasiquote the item
            if _consp_internal(item):
                parts.append(_quasi(item))
            else:
                parts.append(item)

            cur = cdr(cur)

        # Convert parts to a lispCons chain
        res = lisptype.NIL
        for p in reversed(parts):
            res = lisptype.lispCons(p, res)
        return res

    return _quasi(expr)


def eval_prog1(form, env):
    """Evaluate PROG1 special form."""
    args = cdr(form)
    if not _consp_internal(args):
        return None
    
    result = eval(car(args), env)
    args = cdr(args)
    
    # Evaluate remaining forms for side effects
    while _consp_internal(args):
        eval(car(args), env)
        args = cdr(args)
    
    return result


def eval_prog2(form, env):
    """Evaluate PROG2 special form."""
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        return None
    
    # Evaluate first form for side effects
    eval(car(args), env)
    
    # Return value of second form
    result = eval(car(cdr(args)), env)
    args = cdr(cdr(args))
    
    # Evaluate remaining forms for side effects
    while _consp_internal(args):
        eval(car(args), env)
        args = cdr(args)
    
    return result


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


@_registry.cl_function('LAMBDA')
def lambda_fn(lambda_list, *body):
    """Create lambda function."""
    def lambda_func(*args):
        # Simple lambda implementation
        # In full implementation would handle lambda list parsing
        if body:
            return eval(body[-1])
        return None
    return lambda_func


# Control flow and special forms
def flet(bindings, *body):
    """Local function binding."""
    raise lisptype.LispNotImplementedError("FLET")

def labels(bindings, *body):
    """Recursive local function binding."""
    raise lisptype.LispNotImplementedError("LABELS")

def handler_bind(bindings, *body):
    """Bind condition handlers."""
    raise lisptype.LispNotImplementedError("HANDLER-BIND")

def handler_case(form, *clauses):
    """Handle conditions with case."""
    raise lisptype.LispNotImplementedError("HANDLER-CASE")

@_registry.cl_function('IGNORE-ERRORS')
def ignore_errors(*body):
    """Ignore errors in body."""
    try:
        result = None
        for form in body:
            result = eval(form)
        return result
    except:
        return None


from fclpy.lispfunc import registry as _registry  # ensure decorator available if not already

@_registry.cl_function('UNLESS')
def unless(test, *forms):
    """Execute forms if test is false (simple evaluator stub)."""
    if not test:
        return forms[-1] if forms else None
    return None


@_registry.cl_function('PROG1')
def prog1(first_form, *forms):
    """Return first argument after (stub) evaluating remaining forms."""
    return first_form


@_registry.cl_function('PROG2')
def prog2(first_form, second_form, *forms):
    """Return second argument after (stub) evaluating remaining forms."""
    return second_form


def progn(*forms):
    """Evaluate forms sequentially, return last form's value."""
    result = None
    for form in forms:
        result = eval(form)
    return result


def with_open_file(args, *body):
    """Execute body with open file."""
    raise lisptype.LispNotImplementedError("WITH-OPEN-FILE")

def loop_finish():
    """Finish loop execution."""
    raise lisptype.LispNotImplementedError("LOOP-FINISH")

def inline_decl(*args):
    """Inline declaration."""
    raise lisptype.LispNotImplementedError("INLINE")

def ignore(*args):
    """Ignore declaration."""
    return None

def ignorable(*args):
    """Ignorable declaration."""
    return None

@_registry.cl_function('DEFINE-MODIFY-MACRO')
def define_modify_macro(name, lambda_list, function, **kwargs):
    """Define modify macro."""
    raise lisptype.LispNotImplementedError("DEFINE-MODIFY-MACRO")


@_registry.cl_function('SET')
def set(symbol, value):
    """Set the value of a symbol (dynamic variable)."""
    # For now, just return the value - proper symbol table management later
    return value


@_registry.cl_function('BOUNDP')
def boundp(symbol):
    """Test if symbol has a value binding."""
    # For now, assume most symbols are bound - proper implementation later
    return lisptype.T


@_registry.cl_function('MAKUNBOUND')
def makunbound(symbol):
    """Make symbol unbound."""
    # For now, just return the symbol - proper implementation later
    return symbol


@_registry.cl_function('VALUES')
def values(*args):
    """Return multiple values."""
    # For now, return first value or None - proper multiple-values later
    return args[0] if args else None


@_registry.cl_function('VALUES-LIST')
def values_list(lst):
    """Return multiple values from a list."""
    # For now, return first element or None - proper implementation later
    from .core import _consp_internal, car
    return car(lst) if _consp_internal(lst) else None


# Aliases for functions that may have different names in lispenv.py
def apply_fn(function, *args):
    """Apply function (alias for apply)."""
    return apply(function, *args)


# Control flow and type operations
def typecase(keyform, *clauses):
    """Type-based case statement."""
    return None  # Simplified


def etypecase(keyform, *clauses):
    """Exhaustive typecase."""
    return None  # Simplified


def ctypecase(keyform, *clauses):
    """Correctable typecase."""
    return None  # Simplified


@_registry.cl_function('CCASE')
def ccase(keyform, *clauses):
    """Correctable case."""
    return None  # Simplified


@_registry.cl_function('ECASE')
def ecase(keyform, *clauses):
    """Exhaustive case."""
    return None  # Simplified


def eval_fn(form, env=None):
    """Eval function (alias)."""
    return eval(form, env)


def compile_fn(name, definition=None):
    """Compile function."""
    return None  # Simplified


@_registry.cl_function('THE')
def the(type_spec, form):
    """Type declaration."""
    return form


@_registry.cl_function('LOCALLY')
def locally(*body):
    """Local declarations."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('DESTRUCTURING-BIND')
def destructuring_bind(lambda_list, expression, *body):
    """Destructuring bind."""
    result = None
    for form in body:
        result = form
    return result


# Assignment and modification operations
@_registry.cl_function('DECF')
def decf(place, delta=1):
    """Decrement place (stub returns numeric result)."""
    return place - delta  # Simplified


@_registry.cl_function('PSETF')
def psetf(*pairs):
    """Parallel setf (stub)."""
    return None  # Simplified


@_registry.cl_function('SETF')
def setf(*pairs):
    """Set place (stub)."""
    return None  # Simplified


@_registry.cl_function('SHIFTF')
def shiftf(*places):
    """Shift places (stub)."""
    return None  # Simplified


@_registry.cl_function('ROTATEF')
def rotatef(*places):
    """Rotate places (stub)."""
    return None  # Simplified


@_registry.cl_function('PSETQ')
def psetq(*pairs):
    """Parallel setq (stub)."""
    return None  # Simplified


@_registry.cl_function('BLOCK')
def block(name, *body):
    """Execute block with optional return-from."""
    # For now, just evaluate body forms in sequence - proper implementation later
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('RETURN-FROM')
def return_from(name, value=None):
    """Return from named block."""
    # For now, just return the value - proper implementation later
    return value


@_registry.cl_function('CATCH')
def catch(tag, *body):
    """Catch thrown values."""
    # For now, just evaluate body - proper implementation later
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('THROW')
def throw(tag, value=None):
    """Throw value to catch."""
    # For now, just return the value - proper implementation later
    return value


@_registry.cl_function('TAGBODY')
def tagbody(*forms):
    """Execute forms with tags for GO."""
    # For now, just evaluate non-tag forms - proper implementation later
    result = None
    for form in forms:
        if not isinstance(form, (str, int)):  # Skip tags
            result = form
    return result


@_registry.cl_function('GO')
def go(tag):
    """Go to tag in tagbody."""
    # For now, just return None - proper implementation later
    return None


@_registry.cl_function('UNWIND-PROTECT')
def unwind_protect(protected_form, *cleanup_forms):
    """Execute protected form with cleanup."""
    # For now, just execute protected form - proper implementation later
    return protected_form


@_registry.cl_function('AND')
def and_fn(*args):
    """Logical AND of arguments."""
    result = True
    for arg in args:
        result = arg
        if not arg:
            return None
    return result


@_registry.cl_function('OR')
def or_fn(*args):
    """Logical OR of arguments."""
    for arg in args:
        if arg:
            return arg
    return None


@_registry.cl_function('PROG')
def prog(*body):
    """Execute prog block."""
    # For now, just evaluate forms - proper implementation later
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WHEN')
def when_fn(test, *body):
    """Execute body if test is true."""
    if test:
        result = None
        for form in body:
            result = form
        return result
    return None


@_registry.cl_function('UNLESS')
def unless_fn(test, *body):
    """Execute body if test is false."""
    if not test:
        result = None
        for form in body:
            result = form
        return result
    return None


@_registry.cl_function('CASE')
def case_fn(keyform, *clauses):
    """Case statement."""
    # For now, return None - proper implementation later
    return None


@_registry.cl_function('COND')
def cond_fn(*clauses):
    """Conditional statement."""
    # For now, return None - proper implementation later
    return None


@_registry.cl_function('DO')
def do_fn(*args):
    """Do loop."""
    # For now, return None - proper implementation later
    return None


@_registry.cl_function('DOLIST')
def dolist(*args):
    """Dolist loop."""
    # For now, return None - proper implementation later
    return None


@_registry.cl_function('DOTIMES')
def dotimes(*args):
    """Dotimes loop."""
    # For now, return None - proper implementation later
    return None


@_registry.cl_function('LOOP')
def loop_fn(*args):
    """Simple LOOP implementation that accepts common clauses.

    Supports forms such as:
      (LOOP FOR i FROM 0 TO 9 BY 1 DO (PRINT i))
      (LOOP WHILE <test> DO <forms>)
      (LOOP UNTIL <test> DO <forms>)
      (LOOP REPEAT <n> DO <forms>)

    This is not a complete implementation of ANSI LOOP, but covers common cases.
    loop_fn is invoked as a function/macro with raw unevaluated args; here we
    accept either raw cons arguments or Python sequences and interpret them.
    """
    # Normalize args into a Python list of forms
    forms = []
    # If the macro was passed a single cons wrapping the clause list, unwrap it
    if len(args) == 1 and _consp_internal(args[0]):
        cur = args[0]
        while _consp_internal(cur):
            clause = car(cur)
            # clause is expected to be a cons whose car is the clause keyword
            if _consp_internal(clause):
                # append the clause head symbol
                forms.append(car(clause))
                # append each element of the clause tail as a single form
                tail = cdr(clause)
                while _consp_internal(tail):
                    forms.append(car(tail))
                    tail = cdr(tail)
            else:
                forms.append(clause)
            cur = cdr(cur)
    else:
        for a in args:
            forms.append(a)

    # Simple parser: look for keywords FOR, FROM, TO, BY, DO, WHILE, UNTIL, REPEAT
    i = 0
    results = None

    def eval_body_list(body_forms, env):
        res = None
        for f in body_forms:
            res = eval(f, env)
        return res

    # If forms start with a single body, just evaluate it repeatedly? handle trivial case
    # Parse a single clause at a time
    env_for_loop = None
    while i < len(forms):
        token = forms[i]
        # Treat token names (symbols) by uppercase name
        name = token.name if isinstance(token, lisptype.LispSymbol) else None

        if name == 'FOR':
            # Expect: FOR <var> FROM <start> TO <end> [BY <step>] DO <body...>
            var = forms[i+1]
            if not isinstance(var, lisptype.LispSymbol):
                raise lisptype.LispNotImplementedError('LOOP FOR requires a symbol')
            # default values
            start = 0
            end = None
            step = 1
            j = i+2
            while j < len(forms):
                f = forms[j]
                fname = f.name if isinstance(f, lisptype.LispSymbol) else None
                if fname == 'FROM':
                    start = eval(forms[j+1])
                    j += 2
                elif fname == 'TO':
                    end = eval(forms[j+1])
                    j += 2
                elif fname == 'BY':
                    step = eval(forms[j+1])
                    j += 2
                elif fname == 'DO':
                    # body consumes rest until next top-level clause; take remaining as body
                    body = []
                    k = j+1
                    while k < len(forms):
                        body.append(forms[k])
                        k += 1
                    # run loop
                    # create local environment for loop variables
                    loop_env = lisptype.Environment(lispenv.current_environment)
                    loop_env.add_variable(var, start)
                    val = None
                    cur = start
                    # inclusive loop when end is provided
                    if end is None:
                        # no end -> single evaluation
                        val = eval_body_list(body, loop_env)
                    else:
                        # iterate
                        if step == 0:
                            raise lisptype.LispNotImplementedError('LOOP BY step cannot be 0')
                        compare = (lambda a, b: a <= b) if step > 0 else (lambda a, b: a >= b)
                        while compare(cur, end):
                            loop_env.set_variable(var, cur)
                            val = eval_body_list(body, loop_env)
                            cur = cur + step
                    results = val
                    return results
                else:
                    # Unexpected token, break
                    break
            i = j

        elif name == 'WHILE':
            # (LOOP WHILE <test> DO <body...>)
            test = forms[i+1]
            # find DO
            j = i+2
            while j < len(forms) and not (isinstance(forms[j], lisptype.LispSymbol) and forms[j].name == 'DO'):
                j += 1
            body = []
            k = j+1
            while k < len(forms) and not (isinstance(forms[k], lisptype.LispSymbol) and forms[k].name in ('WHILE','UNTIL','REPEAT','FOR')):
                body.append(forms[k]); k += 1

            # execute
            res = None
            while eval(test):
                res = eval_body_list(body, lispenv.current_environment)
            return res

        elif name == 'UNTIL':
            test = forms[i+1]
            # find DO
            j = i+2
            while j < len(forms) and not (isinstance(forms[j], lisptype.LispSymbol) and forms[j].name == 'DO'):
                j += 1
            body = []
            k = j+1
            while k < len(forms) and not (isinstance(forms[k], lisptype.LispSymbol) and forms[k].name in ('WHILE','UNTIL','REPEAT','FOR')):
                body.append(forms[k]); k += 1

            res = None
            while True:
                res = eval_body_list(body, lispenv.current_environment)
                if eval(test):
                    break
            return res

        elif name == 'REPEAT':
            # (LOOP REPEAT <n> DO <body...>)
            count = eval(forms[i+1])
            # find DO
            j = i+2
            while j < len(forms) and not (isinstance(forms[j], lisptype.LispSymbol) and forms[j].name == 'DO'):
                j += 1
            body = []
            k = j+1
            while k < len(forms):
                body.append(forms[k]); k += 1

            res = None
            for _ in range(count):
                res = eval_body_list(body, lispenv.current_environment)
            return res

        else:
            # Unrecognized - try to eval as single body
            return eval(token)

    return results

# Mark LOOP implementation as a macro so evaluator will call it with raw args
setattr(loop_fn, '__is_macro__', True)


def load_fn(filename, **kwargs):
    """Load file."""
    # For now, return None - proper implementation later
    return None


@_registry.cl_special('FUNCTION')
def function_fn(name):
    """FUNCTION special form (stub returning name)."""
    return name


@_registry.cl_special('QUOTE')
def quote_fn(expression):
    return expression

@_registry.cl_special('DEFMACRO')
def special_defmacro(*args):
    """DEFMACRO special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DEFMACRO (evaluated in evaluator)')
    return expression

# Register remaining special forms as stubs; real semantics handled in eval dispatcher.
@_registry.cl_special('IF')
def special_if(*args):
    """IF special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('IF (evaluated in evaluator)')

@_registry.cl_special('COND')
def special_cond(*args):
    """COND special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('COND (evaluated in evaluator)')

@_registry.cl_special('DEFUN')
def special_defun(*args):
    """DEFUN special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DEFUN (evaluated in evaluator)')

@_registry.cl_special('SETQ')
def special_setq(*args):
    """SETQ special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('SETQ (evaluated in evaluator)')

@_registry.cl_special('DEFVAR')
def special_defvar(*args):
    """DEFVAR special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DEFVAR (evaluated in evaluator)')

@_registry.cl_special('LET')
def special_let(*args):
    """LET special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('LET (evaluated in evaluator)')

@_registry.cl_special('WHEN')
def special_when(*args):
    """WHEN special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('WHEN (evaluated in evaluator)')

@_registry.cl_special('FLET')
def special_flet(*args):
    """FLET special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('FLET (evaluated in evaluator)')

@_registry.cl_special('LABELS')
def special_labels(*args):
    """LABELS special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('LABELS (evaluated in evaluator)')

@_registry.cl_special('HANDLER-BIND')
def special_handler_bind(*args):
    """HANDLER-BIND special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('HANDLER-BIND (evaluated in evaluator)')

@_registry.cl_special('HANDLER-CASE')
def special_handler_case(*args):
    """HANDLER-CASE special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('HANDLER-CASE (evaluated in evaluator)')

@_registry.cl_special('WITH-OPEN-FILE')
def special_with_open_file(*args):
    """WITH-OPEN-FILE special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('WITH-OPEN-FILE (evaluated in evaluator)')

@_registry.cl_special('LOOP-FINISH')
def special_loop_finish(*args):
    """LOOP-FINISH special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('LOOP-FINISH (evaluated in evaluator)')

@_registry.cl_special('INLINE')
def special_inline(*args):
    """INLINE declaration (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('INLINE (evaluated in evaluator)')

@_registry.cl_special('IGNORE')
def special_ignore(*args):
    """IGNORE declaration (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('IGNORE (evaluated in evaluator)')

@_registry.cl_special('IGNORABLE')
def special_ignorable(*args):
    """IGNORABLE declaration (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('IGNORABLE (evaluated in evaluator)')
