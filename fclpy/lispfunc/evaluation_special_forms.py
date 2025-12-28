"""Special forms: QUOTE, IF, DEFUN, DEFMACRO, LAMBDA, declarations.

This module contains handlers for special forms that don't fall into
control flow, loops/conditionals, or condition handling categories.
"""

import fclpy.lisptype as lisptype
from .core import car, cdr, _consp_internal
from . import registry as _registry


def eval_if(form, env):
    """Evaluate IF special form."""
    # Import eval lazily to avoid circular imports
    from .evaluation_core import eval
    
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


def eval_setq(form, env):
    """Evaluate SETQ special form."""
    from .evaluation_core import eval
    
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


def eval_defun(form, env):
    """Evaluate DEFUN special form."""
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("DEFUN requires at least 2 arguments")
    
    func_name = car(args)
    param_list = car(cdr(args))
    body = cdr(cdr(args))
    
    if not isinstance(func_name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFUN: function name must be a symbol")
    
    # Extract docstring if present (first form in body can be a string)
    docstring = None
    actual_body = body
    if _consp_internal(body):
        first_form = car(body)
        if isinstance(first_form, str):
            docstring = first_form
            actual_body = cdr(body)
    
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
        current_body = actual_body
        while _consp_internal(current_body):
            result = eval(car(current_body), func_env)
            current_body = cdr(current_body)
        
        return result
    
    # Add function to environment
    env.add_function(func_name, user_function)
    
    # Store docstring on the function symbol's property list
    if docstring:
        if not hasattr(func_name, 'plist'):
            func_name.plist = {}
        func_name.plist['DOCUMENTATION'] = docstring
    
    return func_name


def eval_defmacro(form, env):
    """Evaluate DEFMACRO special form: register a macro in the environment.

    This creates a Python callable that evaluates the macro body in an
    environment where the parameters are bound to the arguments. This allows
    QUASIQUOTE/UNQUOTE to work correctly in macro templates.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("DEFMACRO requires a name, lambda-list and body")

    macro_name = car(args)
    lambda_list = car(cdr(args))
    body = cdr(cdr(args))

    if not isinstance(macro_name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFMACRO: macro name must be a symbol")

    # Extract docstring if present (first form in body can be a string)
    docstring = None
    actual_body = body
    if _consp_internal(body):
        first_form = car(body)
        if isinstance(first_form, str):
            docstring = first_form
            actual_body = cdr(body)

    # Build parameter symbols list
    params = []
    cur = lambda_list
    while _consp_internal(cur):
        p = car(cur)
        if isinstance(p, lisptype.LispSymbol):
            params.append(p)
        cur = cdr(cur)

    # Create the macro callable
    def macro_callable(*call_args):
        # Create a new environment extending the definition environment
        macro_env = lisptype.Environment(parent=env)
        
        # Bind parameter symbols to raw argument forms
        for i, param in enumerate(params):
            if i < len(call_args):
                macro_env.add_variable(param, call_args[i])
            else:
                macro_env.add_variable(param, lisptype.NIL)

        # If no body, return NIL
        if not _consp_internal(actual_body):
            return lisptype.NIL

        # Evaluate body forms in macro environment, return last result
        result = lisptype.NIL
        cur_body = actual_body
        while _consp_internal(cur_body):
            result = eval(car(cur_body), macro_env)
            cur_body = cdr(cur_body)
        
        return result

    # Mark as macro and register in environment
    setattr(macro_callable, '__is_macro__', True)
    env.add_function(macro_name, macro_callable)
    
    # Store docstring on the macro symbol's property list
    if docstring:
        if not hasattr(macro_name, 'plist'):
            macro_name.plist = {}
        macro_name.plist['DOCUMENTATION'] = docstring
    
    return macro_name


def eval_macroexpand_1(form, env):
    """Evaluate MACROEXPAND-1 special form.
    
    (MACROEXPAND-1 form) - expand a macro call one level.
    If form is a macro call, expands the macro and returns the expansion.
    Otherwise returns form unchanged.
    """
    from .evaluation_core import eval
    
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
    from .evaluation_core import eval
    
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


def eval_lambda(form, env):
    """Evaluate LAMBDA special form."""
    from .evaluation_core import eval
    
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


def eval_declare(form, env):
    """Evaluate DECLARE special form.
    
    DECLARE is used inside function/macro/block bodies to provide declarations.
    This function stores declarations on the containing symbol's property list.
    
    Format: (DECLARE (declare-spec1) (declare-spec2) ...)
    Common declare-specs: (OPTIMIZE ...) (SPECIAL var ...) (TYPE type var ...) (IGNORE var ...)
    """
    args = cdr(form)
    
    # Collect all declare-specs
    result = None
    while _consp_internal(args):
        spec = car(args)
        # Each spec is a list like (OPTIMIZE ...) or (SPECIAL x y z)
        if _consp_internal(spec):
            spec_type = car(spec)
            if isinstance(spec_type, lisptype.LispSymbol):
                spec_name = spec_type.name.upper()
                
                # Store declaration in a reserved property list key
                if not hasattr(env, '_declarations'):
                    env._declarations = {}
                if spec_name not in env._declarations:
                    env._declarations[spec_name] = []
                env._declarations[spec_name].append(spec)
        
        args = cdr(args)
    
    # DECLARE returns NIL
    return lisptype.NIL


def eval_declaim(form, env):
    """Evaluate DECLAIM special form (global declarations).
    
    DECLAIM provides global declarations that affect the entire program.
    Stores declarations globally in the environment.
    
    Format: (DECLAIM (declare-spec1) (declare-spec2) ...)
    """
    args = cdr(form)
    
    # Get the global environment (root environment)
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    # Collect all declare-specs globally
    result = None
    while _consp_internal(args):
        spec = car(args)
        # Each spec is a list like (OPTIMIZE ...) or (SPECIAL x y z)
        if _consp_internal(spec):
            spec_type = car(spec)
            if isinstance(spec_type, lisptype.LispSymbol):
                spec_name = spec_type.name.upper()
                
                # Handle different declaration types
                if spec_name == 'OPTIMIZE':
                    # Store optimization settings globally
                    _store_optimization_declaration(global_env, spec)
                elif spec_name == 'SPECIAL':
                    # Store special variable declarations globally
                    _store_special_declaration(global_env, spec)
                else:
                    # Store other declarations
                    if not hasattr(global_env, '_global_declarations'):
                        global_env._global_declarations = {}
                    if spec_name not in global_env._global_declarations:
                        global_env._global_declarations[spec_name] = []
                    global_env._global_declarations[spec_name].append(spec)
        
        args = cdr(args)
    
    # DECLAIM returns NIL
    return lisptype.NIL


def _store_optimization_declaration(env, spec):
    """Helper to store OPTIMIZE declaration on environment."""
    # OPTIMIZE spec format: (OPTIMIZE (quality level) ...)
    # Qualities: SPEED, SAFETY, DEBUG, COMPILATION-SPEED, SPACE
    # Levels: 0 (minimum) to 3 (maximum)
    
    if not hasattr(env, '_optimization_policy'):
        env._optimization_policy = {
            'speed': 1,
            'safety': 1,
            'debug': 1,
            'compilation-speed': 1,
            'space': 1
        }
    
    # Parse (OPTIMIZE (quality level) ...)
    specs = cdr(spec)  # Skip 'OPTIMIZE' keyword
    while _consp_internal(specs):
        item = car(specs)
        if _consp_internal(item):
            quality = car(item)
            level = car(cdr(item))
            
            if isinstance(quality, lisptype.LispSymbol) and isinstance(level, int):
                q_name = quality.name.lower().replace('-', '_')
                env._optimization_policy[q_name] = max(0, min(3, level))
        
        specs = cdr(specs)


def _store_special_declaration(env, spec):
    """Helper to store SPECIAL declaration on environment."""
    # SPECIAL spec format: (SPECIAL var1 var2 ...)
    vars_to_declare = cdr(spec)  # Skip 'SPECIAL' keyword
    
    if not hasattr(env, '_special_variables'):
        env._special_variables = {}
    
    while _consp_internal(vars_to_declare):
        var = car(vars_to_declare)
        if isinstance(var, lisptype.LispSymbol):
            env._special_variables[var.name] = True
        vars_to_declare = cdr(vars_to_declare)


def eval_defvar(form, env):
    """Evaluate DEFVAR special form.
    
    (DEFVAR name)           - declares special variable, binds to NIL if unbound
    (DEFVAR name value)     - declares and initializes if unbound
    (DEFVAR name value doc) - with documentation string
    
    DEFVAR only sets the initial value if the variable is not already bound.
    This is in contrast to DEFPARAMETER which always sets the value.
    
    Returns the symbol name.
    """
    from .evaluation_core import eval as lisp_eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFVAR requires at least a variable name")
    
    name = car(args)
    if not isinstance(name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFVAR: first argument must be a symbol")
    
    # Check if variable is already bound
    current_value = env.find_variable(name)
    is_already_bound = current_value is not None
    
    # Get the value form if present
    rest_args = cdr(args)
    has_value_form = _consp_internal(rest_args)
    
    if has_value_form and not is_already_bound:
        # Has initial value and not already bound - evaluate and bind
        value_form = car(rest_args)
        value = lisp_eval(value_form, env)
        env.add_variable(name, value)
    elif not is_already_bound:
        # No value form and not bound - bind to NIL
        env.add_variable(name, lisptype.NIL)
    # If already bound, do nothing to the value
    
    # Handle documentation string if present (third argument)
    if has_value_form:
        doc_args = cdr(rest_args)
        if _consp_internal(doc_args):
            docstring = car(doc_args)
            if isinstance(docstring, str):
                # Store documentation on symbol's property list
                if not hasattr(name, 'plist'):
                    name.plist = {}
                name.plist['DOCUMENTATION'] = docstring
                name.plist['VARIABLE-DOCUMENTATION'] = docstring
    
    # Mark as special variable
    if not hasattr(env, '_special_variables'):
        env._special_variables = {}
    env._special_variables[name.name] = True
    
    return name


def eval_defparameter(form, env):
    """Evaluate DEFPARAMETER special form.
    
    (DEFPARAMETER name value)     - declares and always sets value
    (DEFPARAMETER name value doc) - with documentation string
    
    Unlike DEFVAR, DEFPARAMETER always sets the value, even if already bound.
    
    Returns the symbol name.
    """
    from .evaluation_core import eval as lisp_eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFPARAMETER requires a variable name")
    
    name = car(args)
    if not isinstance(name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFPARAMETER: first argument must be a symbol")
    
    # Get the value form (required for DEFPARAMETER)
    rest_args = cdr(args)
    if not _consp_internal(rest_args):
        raise lisptype.LispNotImplementedError("DEFPARAMETER requires an initial value")
    
    value_form = car(rest_args)
    value = lisp_eval(value_form, env)
    env.add_variable(name, value)
    
    # Handle documentation string if present (third argument)
    doc_args = cdr(rest_args)
    if _consp_internal(doc_args):
        docstring = car(doc_args)
        if isinstance(docstring, str):
            # Store documentation on symbol's property list
            if not hasattr(name, 'plist'):
                name.plist = {}
            name.plist['DOCUMENTATION'] = docstring
            name.plist['VARIABLE-DOCUMENTATION'] = docstring
    
    # Mark as special variable
    if not hasattr(env, '_special_variables'):
        env._special_variables = {}
    env._special_variables[name.name] = True
    
    return name


__all__ = [
    'eval_if',
    'eval_setq',
    'eval_defun',
    'eval_defmacro',
    'eval_macroexpand_1',
    'eval_macro_function',
    'eval_lambda',
    'eval_declare',
    'eval_declaim',
    'eval_defvar',
    'eval_defparameter',
    '_store_optimization_declaration',
    '_store_special_declaration',
]
