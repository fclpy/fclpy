"""Loops and conditionals: WHEN, COND, AND, OR, PROGN, LET, quasiquote."""

import fclpy.lisptype as lisptype
from .core import car, cdr, _consp_internal, cons
from . import registry as _registry
import time
import sys

# Timeout for loop warning (in seconds) - set to 0 to disable
LOOP_TIMEOUT_WARNING = 120  # 2 minutes


def eval_when(form, env):
    """Evaluate WHEN special form."""
    from .evaluation_core import eval
    
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
    from .evaluation_core import eval
    
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


def eval_eval_when(form, env):
    """Evaluate EVAL-WHEN special form.
    
    (EVAL-WHEN (situation*) form*)
    
    Situations can be:
    - :COMPILE-TOPLEVEL (or COMPILE) - evaluate at compile time
    - :LOAD-TOPLEVEL (or LOAD) - evaluate at load time  
    - :EXECUTE (or EVAL) - evaluate at execution time
    
    For an interpreter, we evaluate if :EXECUTE or :LOAD-TOPLEVEL is present.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    situations = car(args)
    body = cdr(args)
    
    # Check if any relevant situation applies
    # For an interpreter at runtime, :EXECUTE and :LOAD-TOPLEVEL apply
    should_execute = False
    current = situations
    while _consp_internal(current):
        sit = car(current)
        if isinstance(sit, lisptype.lispKeyword):
            sit_name = sit.name.upper()
        elif isinstance(sit, lisptype.LispSymbol):
            sit_name = sit.name.upper()
        else:
            sit_name = str(sit).upper()
        
        # :EXECUTE means "at runtime" - always applies for interpreter
        # :LOAD-TOPLEVEL means "when loading" - applies when loading files
        if sit_name in ('EXECUTE', ':EXECUTE', 'EVAL', 'LOAD-TOPLEVEL', ':LOAD-TOPLEVEL', 'LOAD'):
            should_execute = True
            break
        current = cdr(current)
    
    if should_execute:
        # Evaluate body forms, return value of last
        result = lisptype.NIL
        current = body
        while _consp_internal(current):
            result = eval(car(current), env)
            current = cdr(current)
        return result
    else:
        # Don't evaluate, return NIL
        return lisptype.NIL


def eval_cond(form, env):
    """Evaluate COND special form."""
    from .evaluation_core import eval
    
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
    from .evaluation_core import eval
    
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
    from .evaluation_core import eval
    
    args = cdr(form)
    
    while _consp_internal(args):
        result = eval(car(args), env)
        if result is not None and result != lisptype.NIL:
            return result
        args = cdr(args)
    
    # OR with no truthy values returns NIL, not Python None
    return lisptype.NIL


def eval_progn(form, env):
    """Evaluate PROGN special form."""
    from .evaluation_core import eval
    
    args = cdr(form)
    result = None
    
    while _consp_internal(args):
        result = eval(car(args), env)
        args = cdr(args)
    
    return result


def eval_locally(form, env):
    """Evaluate LOCALLY special form.
    
    (LOCALLY declaration* form*)
    
    LOCALLY is like PROGN but can include declarations at the start.
    The declarations affect only the body forms within the LOCALLY.
    For now, we skip declarations and just evaluate the body forms.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    result = lisptype.NIL
    
    # Skip DECLARE forms at the start
    while _consp_internal(args):
        first = car(args)
        # Check if this is a DECLARE form
        if (_consp_internal(first) and 
            isinstance(car(first), lisptype.LispSymbol) and 
            car(first).name == 'DECLARE'):
            # Skip this declaration
            args = cdr(args)
        else:
            # Not a declaration, start evaluating body
            break
    
    # Evaluate remaining body forms
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
    from .evaluation_core import eval
    
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
    # Track special variables that need special handling
    import fclpy.state as state
    old_package = None
    has_package_binding = False
    
    for var, value in bindings_list:
        if isinstance(var, lisptype.LispSymbol):
            let_env.add_variable(var, value)
            # Handle *PACKAGE* special variable - update state.current_package
            if var.name == '*PACKAGE*' and isinstance(value, lisptype.Package):
                old_package = getattr(state, 'current_package', None)
                state.current_package = value
                has_package_binding = True
    
    # Update state.current_environment for functions that need it (like LOAD)
    old_env = state.current_environment
    state.current_environment = let_env
    
    try:
        # Evaluate body in new environment
        result = None
        current = body
        while _consp_internal(current):
            result = eval(car(current), let_env)
            current = cdr(current)
        
        return result
    finally:
        # Restore the previous environment
        state.current_environment = old_env
        # Restore *PACKAGE* if it was dynamically bound
        if has_package_binding and old_package is not None:
            state.current_package = old_package


def eval_letstar(form, env):
    """Evaluate LET* special form with sequential binding semantics.
    
    (LET* ((var1 init1) (var2 init2) ...) body...)
    
    In LET*, each init form is evaluated in the environment AFTER previous
    bindings have been established. This is "sequential" binding.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("LET* requires at least a binding list")
    
    bindings_form = car(args)
    body = cdr(args)
    
    # Create new environment for LET* scope
    letstar_env = lisptype.Environment(env)
    
    # Track special variables that need special handling
    import fclpy.state as state
    old_package = getattr(state, 'current_package', None)
    has_package_binding = False
    
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
                # Handle *PACKAGE* special variable - update state.current_package
                if var.name == '*PACKAGE*' and isinstance(value, lisptype.Package):
                    if not has_package_binding:
                        old_package = getattr(state, 'current_package', None)
                        has_package_binding = True
                    state.current_package = value
        current = cdr(current)
    
    # Update state.current_environment for functions that need it (like LOAD)
    old_env = state.current_environment
    state.current_environment = letstar_env
    
    try:
        # Evaluate body in environment with all bindings
        result = None
        current = body
        while _consp_internal(current):
            result = eval(car(current), letstar_env)
            current = cdr(current)
        
        return result
    finally:
        # Restore the previous environment
        state.current_environment = old_env
        # Restore *PACKAGE* if it was dynamically bound
        if has_package_binding:
            state.current_package = old_package


def eval_flet(form, env):
    """Evaluate FLET special form - local function bindings.
    
    (FLET ((name1 (args1...) body1...) (name2 (args2...) body2...)) body...)
    
    FLET establishes local function bindings. The function definitions
    DO NOT see each other (use LABELS for mutually recursive functions).
    The body is evaluated in an environment with the local function bindings.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("FLET requires at least a binding list")
    
    bindings_form = car(args)
    body = cdr(args)
    
    # Create new environment for FLET scope
    flet_env = lisptype.Environment(env)
    
    # Process function definitions - create closures in OUTER environment
    current = bindings_form
    while _consp_internal(current):
        binding = car(current)
        if _consp_internal(binding):
            func_name = car(binding)
            func_lambda_list = car(cdr(binding))
            func_body = cdr(cdr(binding))
            
            # Create a lambda-like closure
            if isinstance(func_name, lisptype.LispSymbol):
                # Build the function closure
                closure = make_lambda_closure(func_lambda_list, func_body, env)
                # Bind the function in the new environment
                flet_env.add_function(func_name, closure)
        current = cdr(current)
    
    # Evaluate body in environment with local function bindings
    result = lisptype.NIL
    current = body
    while _consp_internal(current):
        result = eval(car(current), flet_env)
        current = cdr(current)
    
    return result


def eval_labels(form, env):
    """Evaluate LABELS special form - mutually recursive local function bindings.
    
    (LABELS ((name1 (args1...) body1...) (name2 (args2...) body2...)) body...)
    
    LABELS establishes local function bindings where the functions CAN see
    each other (for mutual recursion). The body is evaluated in an environment
    with the local function bindings.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("LABELS requires at least a binding list")
    
    bindings_form = car(args)
    body = cdr(args)
    
    # Create new environment for LABELS scope
    labels_env = lisptype.Environment(env)
    
    # Process function definitions - create closures in the NEW environment
    # so they can see each other
    current = bindings_form
    while _consp_internal(current):
        binding = car(current)
        if _consp_internal(binding):
            func_name = car(binding)
            func_lambda_list = car(cdr(binding))
            func_body = cdr(cdr(binding))
            
            # Create a lambda-like closure in the labels environment
            if isinstance(func_name, lisptype.LispSymbol):
                # Build the function closure - uses labels_env so functions can call each other
                closure = make_lambda_closure(func_lambda_list, func_body, labels_env)
                # Bind the function in the new environment
                labels_env.add_function(func_name, closure)
        current = cdr(current)
    
    # Evaluate body in environment with local function bindings
    result = lisptype.NIL
    current = body
    while _consp_internal(current):
        result = eval(car(current), labels_env)
        current = cdr(current)
    
    return result


def make_lambda_closure(lambda_list, body, env):
    """Create a closure function from a lambda list and body.
    
    This handles parsing the lambda list with &optional, &rest, &key, etc.
    """
    from .evaluation_core import eval
    
    # Parse the lambda list
    required_params = []
    optional_params = []  # (name, default_value)
    rest_param = None
    key_params = []  # (keyword, name, default_value)
    
    current = lambda_list
    mode = 'required'
    
    while _consp_internal(current):
        param = car(current)
        if isinstance(param, lisptype.LispSymbol):
            param_name = param.name.upper()
            if param_name == '&OPTIONAL':
                mode = 'optional'
            elif param_name == '&REST':
                mode = 'rest'
            elif param_name == '&KEY':
                mode = 'key'
            elif param_name == '&ALLOW-OTHER-KEYS':
                pass  # Ignore for now
            elif param_name == '&BODY':
                mode = 'rest'  # &body is similar to &rest
            elif param_name == '&AUX':
                mode = 'aux'
            else:
                if mode == 'required':
                    required_params.append(param)
                elif mode == 'optional':
                    optional_params.append((param, lisptype.NIL))
                elif mode == 'rest':
                    rest_param = param
                    mode = 'after_rest'  # Only one &rest param
                elif mode == 'key':
                    key_params.append((lisptype.lispKeyword(param.name), param, lisptype.NIL))
                elif mode == 'aux':
                    pass  # &aux params are local bindings, handle later
        elif _consp_internal(param):
            # Complex parameter form: (name default) or (name default supplied-p)
            pname = car(param)
            pdefault = car(cdr(param)) if _consp_internal(cdr(param)) else lisptype.NIL
            if mode == 'optional':
                optional_params.append((pname, pdefault))
            elif mode == 'key':
                # Key params can be (name default) or ((:keyword name) default)
                if _consp_internal(pname):
                    keyword = car(pname)
                    actual_name = car(cdr(pname))
                    key_params.append((keyword, actual_name, pdefault))
                else:
                    key_params.append((lisptype.lispKeyword(pname.name), pname, pdefault))
        current = cdr(current)
    
    def closure_function(*args):
        # Create local environment for this call
        call_env = lisptype.Environment(env)
        
        args_list = list(args)
        arg_idx = 0
        
        # Bind required parameters
        for param in required_params:
            if arg_idx < len(args_list):
                call_env.add_variable(param, args_list[arg_idx])
                arg_idx += 1
            else:
                call_env.add_variable(param, lisptype.NIL)
        
        # Bind optional parameters
        for param, default in optional_params:
            if arg_idx < len(args_list):
                call_env.add_variable(param, args_list[arg_idx])
                arg_idx += 1
            else:
                # Evaluate default in closure environment
                default_val = eval(default, env) if default != lisptype.NIL else lisptype.NIL
                call_env.add_variable(param, default_val)
        
        # Handle keyword arguments
        remaining_args = args_list[arg_idx:]
        keyword_values = {}
        i = 0
        while i < len(remaining_args):
            if isinstance(remaining_args[i], lisptype.lispKeyword):
                if i + 1 < len(remaining_args):
                    keyword_values[remaining_args[i].name.upper()] = remaining_args[i + 1]
                    i += 2
                else:
                    i += 1
            else:
                i += 1
        
        for keyword, param, default in key_params:
            key_name = keyword.name.upper()
            if key_name in keyword_values:
                call_env.add_variable(param, keyword_values[key_name])
            else:
                # Evaluate default
                default_val = eval(default, env) if default != lisptype.NIL else lisptype.NIL
                call_env.add_variable(param, default_val)
        
        # Bind rest parameter if present
        if rest_param:
            # Collect remaining non-keyword args
            rest_args = args_list[arg_idx:]
            # Build a list from rest args
            rest_list = lisptype.NIL
            for arg in reversed(rest_args):
                rest_list = cons(arg, rest_list)
            call_env.add_variable(rest_param, rest_list)
        
        # Evaluate body
        result = lisptype.NIL
        current_form = body
        while _consp_internal(current_form):
            result = eval(car(current_form), call_env)
            current_form = cdr(current_form)
        
        return result
    
    return closure_function


def eval_quasiquote(form, env):
    """Evaluate a QUASIQUOTE form by processing UNQUOTE and UNQUOTE-SPLICING.

    This is a simplified quasiquote evaluator that handles common patterns:
    - (QUASIQUOTE x) where x is a list will return a new list where elements
      of the form (UNQUOTE e) are replaced with the evaluated value of e,
      and elements of the form (UNQUOTE-SPLICING e) are spliced into the list.
    - Nested quasiquotes are not fully supported beyond a single level.
    """
    from .evaluation_core import eval
    
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
    from .evaluation_core import eval
    
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
    from .evaluation_core import eval
    
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


def eval_loop(form, env):
    """Evaluate LOOP special form with lexical environment support.
    
    Extended LOOP implementation supporting ANSI CL loop macro clauses.
    This is implemented as a special form to access the lexical environment.

    Supports forms such as:
      (LOOP FOR i FROM 0 TO 9 BY 1 DO (PRINT i))
      (LOOP WHILE <test> DO <forms>)
      (LOOP UNTIL <test> DO <forms>)
      (LOOP REPEAT <n> DO <forms>)
      (LOOP WHILE <test> UNLESS <cond> DO <action> APPEND <form>)
      (LOOP FOR x IN list COLLECT x)
    """
    from .evaluation_core import eval
    
    # Get loop clauses from form
    args = cdr(form)
    
    # Normalize args into a Python list of forms
    forms = []
    current = args
    while _consp_internal(current):
        forms.append(car(current))
        current = cdr(current)
    
    def sym_name(x):
        """Get uppercase symbol name or None."""
        return x.name.upper() if isinstance(x, lisptype.LispSymbol) else None

    def _bind_varspec(loop_env, varspec, value):
        """Bind a LOOP var spec (symbol or simple destructuring pattern)."""
        # Common case: single symbol
        if isinstance(varspec, lisptype.LispSymbol):
            loop_env.set_variable(varspec, value)
            return

        # Destructuring patterns used by ANSI tests (e.g., (KEY . VAL))
        if _consp_internal(varspec):
            # Dotted pair pattern: (A . B)
            left = car(varspec)
            right = cdr(varspec)
            if isinstance(left, lisptype.LispSymbol) and isinstance(right, lisptype.LispSymbol) and not _consp_internal(right):
                if _consp_internal(value):
                    loop_env.set_variable(left, car(value))
                    loop_env.set_variable(right, cdr(value))
                else:
                    loop_env.set_variable(left, lisptype.NIL)
                    loop_env.set_variable(right, lisptype.NIL)
                return

            # Proper list pattern: (A B C)
            pat = varspec
            cur_val = value
            while _consp_internal(pat):
                pitem = car(pat)
                if isinstance(pitem, lisptype.LispSymbol):
                    if _consp_internal(cur_val):
                        loop_env.set_variable(pitem, car(cur_val))
                        cur_val = cdr(cur_val)
                    else:
                        loop_env.set_variable(pitem, lisptype.NIL)
                pat = cdr(pat)
            return

        raise lisptype.LispNotImplementedError('LOOP FOR requires a symbol')

    # Parse loop clauses into structured form
    i = 0
    
    # Loop state
    iteration_type = None  # 'for-range', 'for-in', 'for-on', 'for-equals', 'while', 'until', 'repeat', None
    iteration_var = None
    iteration_test = None  # for WHILE/UNTIL termination
    termination_type = None  # 'while' or 'until' - can be used with FOR clauses
    iteration_start = 0
    iteration_end = None
    iteration_step = 1
    iteration_list = None  # for FOR ... IN/ON
    repeat_count = None

    # Primary + additional iteration drivers (for parallel FOR clauses).
    # Each driver is a dict with keys: var, kind, and kind-specific data.
    iteration_drivers = []

    # Additional FOR bindings that don't drive the primary iteration.
    # Minimal support for ANSI patterns like:
    #   (LOOP FOR I FROM 0 BELOW 256 FOR C = (CODE-CHAR I) WHEN C COLLECT C)
    aux_for_bindings = []  # list of (var, init_form, then_form_or_None)
    
    conditionals = []  # list of ('when'/'unless', test_form)
    body_forms = []
    accumulation = None  # ('collect'/'append'/'sum'/'count', form)
    accumulation_conditionals = []  # conditionals that apply to accumulation
    finally_forms = []
    
    # Parse clauses
    while i < len(forms):
        token = forms[i]
        name = sym_name(token)
        
        if name == 'FOR':
            candidate_var = forms[i+1]
            if not (isinstance(candidate_var, lisptype.LispSymbol) or _consp_internal(candidate_var)):
                raise lisptype.LispNotImplementedError('LOOP FOR requires a symbol')

            clause_stop = ('FOR', 'WHILE', 'UNTIL', 'REPEAT', 'DO', 'DOING',
                           'COLLECT', 'COLLECTING', 'APPEND', 'APPENDING',
                           'NCONC', 'NCONCING', 'SUM', 'SUMMING', 'COUNT', 'COUNTING',
                           'WHEN', 'UNLESS', 'IF', 'RETURN', 'FINALLY')

            # Parse the FOR clause into either a driver (IN/ON/ACROSS/FROM...) or an aux binding (=
            # without FROM/IN/etc) when a driver already exists.
            j = i + 2
            saw_driver_keyword = False
            driver_kind = None
            driver_start = None
            driver_end = None
            driver_step = None
            driver_list = None
            aux_init = None
            aux_then = None

            while j < len(forms):
                fname = sym_name(forms[j])
                if fname == 'FROM':
                    saw_driver_keyword = True
                    driver_start = forms[j+1]
                    j += 2
                elif fname == 'TO':
                    saw_driver_keyword = True
                    driver_end = forms[j+1]
                    driver_kind = 'for-range'
                    j += 2
                elif fname == 'BELOW':
                    saw_driver_keyword = True
                    driver_end = forms[j+1]
                    driver_kind = 'for-below'
                    j += 2
                elif fname == 'BY':
                    saw_driver_keyword = True
                    driver_step = forms[j+1]
                    j += 2
                elif fname == 'IN':
                    saw_driver_keyword = True
                    driver_list = forms[j+1]
                    driver_kind = 'for-in'
                    j += 2
                elif fname == 'ON':
                    saw_driver_keyword = True
                    driver_list = forms[j+1]
                    driver_kind = 'for-on'
                    j += 2
                elif fname == 'ACROSS':
                    saw_driver_keyword = True
                    driver_list = forms[j+1]
                    driver_kind = 'for-across'
                    j += 2
                elif fname == '=':
                    # FOR x = init-form [THEN step-form]
                    aux_init = forms[j+1]
                    j += 2
                    if j < len(forms) and sym_name(forms[j]) == 'THEN':
                        aux_then = forms[j+1]
                        j += 2
                    # '=' can be either a driver (if this is the first/only iteration clause)
                    # or an auxiliary binding when a driver already exists.
                    if not saw_driver_keyword and driver_kind is None:
                        driver_kind = 'for-equals'
                        driver_start = aux_init
                        driver_step = aux_then
                elif fname == 'THEN':
                    # THEN after = was already handled above
                    j += 2
                elif fname in clause_stop:
                    break
                else:
                    break

            # If we saw FROM but no end specifier, treat as an unbounded arithmetic progression.
            # This is valid in ANSI LOOP, and is often paired with another driver that
            # terminates the overall loop.
            if driver_kind is None and saw_driver_keyword and driver_start is not None:
                driver_kind = 'for-from'

            # Decide whether this clause is a driver or an aux binding.
            if iteration_drivers and (not saw_driver_keyword) and (driver_kind == 'for-equals'):
                # Second/subsequent "FOR var = ..." is treated as aux binding.
                aux_for_bindings.append((candidate_var, aux_init, aux_then))
                i = j
                continue

            if driver_kind is None:
                # e.g., "FOR X" without IN/FROM/=
                raise lisptype.LispNotImplementedError('LOOP FOR clause missing iteration spec')

            driver = {
                'var': candidate_var,
                'kind': driver_kind,
                'start': driver_start,
                'end': driver_end,
                'step': driver_step,
                'list': driver_list,
            }
            iteration_drivers.append(driver)

            # Preserve previous single-driver state for existing execution paths.
            if iteration_var is None:
                iteration_var = candidate_var
                iteration_type = driver_kind
                if driver_kind in ('for-range', 'for-below'):
                    iteration_start = driver_start if driver_start is not None else 0
                    iteration_end = driver_end
                    if driver_step is not None:
                        iteration_step = driver_step
                elif driver_kind in ('for-in', 'for-on', 'for-across'):
                    iteration_list = driver_list
                elif driver_kind == 'for-equals':
                    iteration_start = driver_start
                    iteration_step = driver_step

            i = j
            continue
            
        elif name == 'WHILE':
            # If we already have an iteration type (e.g. FOR clause), this is just a termination test
            if iteration_type is None:
                iteration_type = 'while'
            termination_type = 'while'
            iteration_test = forms[i+1]
            i += 2
            
        elif name == 'UNTIL':
            # If we already have an iteration type (e.g. FOR clause), this is just a termination test
            if iteration_type is None:
                iteration_type = 'until'
            termination_type = 'until'
            iteration_test = forms[i+1]
            i += 2
            
        elif name == 'REPEAT':
            iteration_type = 'repeat'
            repeat_count = forms[i+1]
            i += 2
            
        elif name in ('WHEN', 'IF'):
            conditionals.append(('when', forms[i+1]))
            i += 2
            
        elif name == 'UNLESS':
            conditionals.append(('unless', forms[i+1]))
            i += 2
            
        elif name in ('DO', 'DOING'):
            # Collect body forms until next clause keyword
            # DO clause consumes the current conditionals - they don't apply to subsequent clauses
            i += 1
            while i < len(forms):
                f = forms[i]
                fname = sym_name(f)
                if fname in ('FOR', 'WHILE', 'UNTIL', 'REPEAT', 'DO', 'DOING',
                             'COLLECT', 'COLLECTING', 'APPEND', 'APPENDING',
                             'NCONC', 'NCONCING', 'SUM', 'SUMMING', 'COUNT', 'COUNTING',
                             'WHEN', 'UNLESS', 'IF', 'RETURN', 'FINALLY'):
                    break
                body_forms.append(f)
                i += 1
                
        elif name in ('COLLECT', 'COLLECTING'):
            # If there are pending conditionals (no DO consumed them), they apply to this
            if not body_forms:  # No DO clause consumed the conditionals
                accumulation_conditionals = list(conditionals)
            accumulation = ('collect', forms[i+1])
            i += 2
            
        elif name in ('APPEND', 'APPENDING'):
            if not body_forms:
                accumulation_conditionals = list(conditionals)
            accumulation = ('append', forms[i+1])
            i += 2
            
        elif name in ('NCONC', 'NCONCING'):
            if not body_forms:
                accumulation_conditionals = list(conditionals)
            accumulation = ('nconc', forms[i+1])
            i += 2
            
        elif name in ('SUM', 'SUMMING'):
            if not body_forms:
                accumulation_conditionals = list(conditionals)
            accumulation = ('sum', forms[i+1])
            i += 2
            
        elif name in ('COUNT', 'COUNTING'):
            if not body_forms:
                accumulation_conditionals = list(conditionals)
            accumulation = ('count', forms[i+1])
            i += 2
            
        elif name == 'RETURN':
            # Immediate return from loop
            return eval(forms[i+1], env)
            
        elif name == 'FINALLY':
            i += 1
            while i < len(forms):
                finally_forms.append(forms[i])
                i += 1
                
        else:
            # Simple loop - just evaluate body repeatedly until explicit return
            # Or if no iteration, evaluate once
            if iteration_type is None:
                # Simple infinite loop or body evaluation
                body_forms.append(token)
            i += 1
    
    # Execute the loop
    result = None
    accumulated = []
    sum_result = 0
    count_result = 0

    aux_first_iter = True

    def bind_aux(loop_env):
        """Bind auxiliary FOR variables for this iteration."""
        nonlocal aux_first_iter
        if not aux_for_bindings:
            return
        for var, init_form, then_form in aux_for_bindings:
            form = init_form if (aux_first_iter or then_form is None) else then_form
            loop_env.set_variable(var, eval(form, loop_env))
        aux_first_iter = False
    
    def should_execute_body(loop_env):
        """Check conditionals for body forms."""
        for cond_type, cond_form in conditionals:
            cond_result = eval(cond_form, loop_env)
            if cond_type == 'when' and not lisptype.is_truthy(cond_result):
                return False
            if cond_type == 'unless' and lisptype.is_truthy(cond_result):
                return False
        return True
    
    def should_execute_accumulation(loop_env):
        """Check conditionals for accumulation (may be different from body)."""
        for cond_type, cond_form in accumulation_conditionals:
            cond_result = eval(cond_form, loop_env)
            if cond_type == 'when' and not lisptype.is_truthy(cond_result):
                return False
            if cond_type == 'unless' and lisptype.is_truthy(cond_result):
                return False
        return True
    
    def execute_iteration_body(loop_env):
        """Execute one iteration of the loop body."""
        nonlocal result, accumulated, sum_result, count_result
        
        # Execute body forms (controlled by main conditionals like WHEN/UNLESS)
        if should_execute_body(loop_env):
            for f in body_forms:
                result = eval(f, loop_env)
        
        # Handle accumulation - uses its own conditionals (may be empty)
        if accumulation and should_execute_accumulation(loop_env):
            acc_type, acc_form = accumulation
            acc_value = eval(acc_form, loop_env)
            if acc_type == 'collect':
                accumulated.append(acc_value)
            elif acc_type == 'append':
                # Append list to result
                if _consp_internal(acc_value):
                    cur = acc_value
                    while _consp_internal(cur):
                        accumulated.append(car(cur))
                        cur = cdr(cur)
                elif acc_value is not lisptype.NIL and acc_value is not None:
                    accumulated.append(acc_value)
            elif acc_type == 'nconc':
                # Similar to append but destructive (same behavior for us)
                if _consp_internal(acc_value):
                    cur = acc_value
                    while _consp_internal(cur):
                        accumulated.append(car(cur))
                        cur = cdr(cur)
            elif acc_type == 'sum':
                sum_result += acc_value
            elif acc_type == 'count':
                if lisptype.is_truthy(acc_value):
                    count_result += 1
    
    # Main loop execution with timeout warning
    loop_start_time = time.time()
    loop_iterations = 0
    warning_printed = False
    
    def check_loop_timeout():
        """Check if loop has been running too long and print warning."""
        nonlocal warning_printed, loop_iterations
        loop_iterations += 1
        if LOOP_TIMEOUT_WARNING > 0 and not warning_printed:
            elapsed = time.time() - loop_start_time
            if elapsed > LOOP_TIMEOUT_WARNING:
                warning_printed = True
                print(f"\n*** LOOP WARNING: Loop has been running for {elapsed:.1f}s ({loop_iterations} iterations) ***", file=sys.stderr)
                print(f"*** LOOP body_forms: {body_forms} ***", file=sys.stderr)
                print(f"*** LOOP iteration_type: {iteration_type}, iteration_test: {iteration_test} ***", file=sys.stderr)
                if iteration_var:
                    print(f"*** LOOP var: {iteration_var} ***", file=sys.stderr)
                sys.stderr.flush()

    def _termination_break(loop_env):
        """Check termination_type/iteration_test if present."""
        if iteration_test is None or termination_type is None:
            return False
        test_result = eval(iteration_test, loop_env)
        if termination_type == 'until' and lisptype.is_truthy(test_result):
            return True
        if termination_type == 'while' and not lisptype.is_truthy(test_result):
            return True
        return False

    def _init_driver(loop_env, driver):
        kind = driver['kind']
        if kind in ('for-in', 'for-on'):
            driver['_cur'] = eval(driver['list'], loop_env)
            return True
        if kind == 'for-across':
            driver['_seq'] = eval(driver['list'], loop_env)
            driver['_idx'] = 0
            return True
        if kind in ('for-range', 'for-below'):
            start_form = driver.get('start', 0)
            end_form = driver.get('end')
            step_form = driver.get('step')
            if step_form is None:
                step_form = 1
            start = eval(start_form, loop_env) if not isinstance(start_form, int) else start_form
            end = eval(end_form, loop_env)
            step = eval(step_form, loop_env) if not isinstance(step_form, int) else step_form
            if step == 0:
                raise lisptype.LispNotImplementedError('LOOP BY step cannot be 0')
            driver['_cur'] = start
            driver['_end'] = end
            driver['_step'] = step
            return True
        if kind == 'for-from':
            start_form = driver.get('start', 0)
            step_form = driver.get('step')
            if step_form is None:
                step_form = 1
            start = eval(start_form, loop_env) if not isinstance(start_form, int) else start_form
            step = eval(step_form, loop_env) if not isinstance(step_form, int) else step_form
            if step == 0:
                raise lisptype.LispNotImplementedError('LOOP BY step cannot be 0')
            driver['_cur'] = start
            driver['_step'] = step
            return True
        if kind == 'for-equals':
            start_form = driver.get('start')
            step_form = driver.get('step')
            driver['_cur'] = eval(start_form, loop_env)
            driver['_step_form'] = step_form
            return True
        raise lisptype.LispNotImplementedError(f'LOOP driver kind not implemented: {kind}')

    def _driver_has_value(driver):
        kind = driver['kind']
        if kind in ('for-in', 'for-on'):
            return _consp_internal(driver.get('_cur'))
        if kind == 'for-across':
            seq = driver.get('_seq')
            idx = driver.get('_idx', 0)
            if isinstance(seq, str):
                return idx < len(seq)
            if hasattr(seq, '__len__'):
                return idx < len(seq)
            return False
        if kind == 'for-range':
            cur = driver.get('_cur')
            end = driver.get('_end')
            step = driver.get('_step')
            if step is None:
                return False
            return (cur <= end) if step > 0 else (cur >= end)
        if kind == 'for-below':
            cur = driver.get('_cur')
            end = driver.get('_end')
            step = driver.get('_step')
            if step is None:
                return False
            return (cur < end) if step > 0 else (cur > end)
        if kind == 'for-from':
            return True
        if kind == 'for-equals':
            return True
        return False

    def _bind_driver(loop_env, driver):
        kind = driver['kind']
        var = driver['var']
        if kind == 'for-in':
            _bind_varspec(loop_env, var, car(driver['_cur']))
            return
        if kind == 'for-on':
            _bind_varspec(loop_env, var, driver['_cur'])
            return
        if kind == 'for-across':
            seq = driver['_seq']
            idx = driver['_idx']
            if isinstance(seq, str):
                _bind_varspec(loop_env, var, lisptype.Character(seq[idx]))
            else:
                _bind_varspec(loop_env, var, seq[idx])
            return
        if kind in ('for-range', 'for-below'):
            _bind_varspec(loop_env, var, driver['_cur'])
            return
        if kind == 'for-from':
            _bind_varspec(loop_env, var, driver['_cur'])
            return
        if kind == 'for-equals':
            _bind_varspec(loop_env, var, driver['_cur'])
            return

    def _step_driver(loop_env, driver):
        kind = driver['kind']
        if kind in ('for-in', 'for-on'):
            driver['_cur'] = cdr(driver['_cur'])
            return
        if kind == 'for-across':
            driver['_idx'] = driver.get('_idx', 0) + 1
            return
        if kind in ('for-range', 'for-below'):
            driver['_cur'] = driver['_cur'] + driver['_step']
            return
        if kind == 'for-from':
            driver['_cur'] = driver['_cur'] + driver['_step']
            return
        if kind == 'for-equals':
            step_form = driver.get('_step_form')
            if step_form is not None:
                driver['_cur'] = eval(step_form, loop_env)
            else:
                driver['_cur'] = eval(driver.get('start'), loop_env)
            return
    
    # Parallel drivers: iterate while all drivers can produce values.
    if len(iteration_drivers) > 1:
        loop_env = lisptype.Environment(env)
        for d in iteration_drivers:
            _init_driver(loop_env, d)

        while all(_driver_has_value(d) for d in iteration_drivers):
            check_loop_timeout()
            if _termination_break(loop_env):
                break

            for d in iteration_drivers:
                _bind_driver(loop_env, d)
            bind_aux(loop_env)
            execute_iteration_body(loop_env)

            for d in iteration_drivers:
                _step_driver(loop_env, d)

    elif iteration_type == 'while':
        while lisptype.is_truthy(eval(iteration_test, env)):
            check_loop_timeout()
            bind_aux(env)
            execute_iteration_body(env)
            
    elif iteration_type == 'until':
        while True:
            check_loop_timeout()
            bind_aux(env)
            execute_iteration_body(env)
            if lisptype.is_truthy(eval(iteration_test, env)):
                break
                
    elif iteration_type == 'repeat':
        count = eval(repeat_count, env)
        for _ in range(count):
            check_loop_timeout()
            bind_aux(env)
            execute_iteration_body(env)
            
    elif iteration_type == 'for-range':
        start = eval(iteration_start, env) if not isinstance(iteration_start, int) else iteration_start
        end = eval(iteration_end, env)
        step = eval(iteration_step, env) if not isinstance(iteration_step, int) else iteration_step
        if step == 0:
            raise lisptype.LispNotImplementedError('LOOP BY step cannot be 0')
        
        loop_env = lisptype.Environment(env)
        cur = start
        compare = (lambda a, b: a <= b) if step > 0 else (lambda a, b: a >= b)
        while compare(cur, end):
            check_loop_timeout()
            if _termination_break(loop_env):
                break
            _bind_varspec(loop_env, iteration_var, cur)
            bind_aux(loop_env)
            execute_iteration_body(loop_env)
            cur = cur + step
            
    elif iteration_type == 'for-below':
        start = eval(iteration_start, env) if not isinstance(iteration_start, int) else iteration_start
        end = eval(iteration_end, env)
        step = eval(iteration_step, env) if not isinstance(iteration_step, int) else iteration_step
        if step == 0:
            raise lisptype.LispNotImplementedError('LOOP BY step cannot be 0')
        
        loop_env = lisptype.Environment(env)
        cur = start
        compare = (lambda a, b: a < b) if step > 0 else (lambda a, b: a > b)
        while compare(cur, end):
            check_loop_timeout()
            if _termination_break(loop_env):
                break
            _bind_varspec(loop_env, iteration_var, cur)
            bind_aux(loop_env)
            execute_iteration_body(loop_env)
            cur = cur + step
            
    elif iteration_type == 'for-in':
        lst = eval(iteration_list, env)
        loop_env = lisptype.Environment(env)
        cur = lst
        while _consp_internal(cur):
            check_loop_timeout()
            if _termination_break(loop_env):
                break
            _bind_varspec(loop_env, iteration_var, car(cur))
            bind_aux(loop_env)
            execute_iteration_body(loop_env)
            cur = cdr(cur)
            
    elif iteration_type == 'for-on':
        lst = eval(iteration_list, env)
        loop_env = lisptype.Environment(env)
        cur = lst
        while _consp_internal(cur):
            check_loop_timeout()
            if _termination_break(loop_env):
                break
            _bind_varspec(loop_env, iteration_var, cur)
            bind_aux(loop_env)
            execute_iteration_body(loop_env)
            cur = cdr(cur)
            
    elif iteration_type == 'for-across':
        # FOR x ACROSS vector/string - iterate over array elements
        seq = eval(iteration_list, env)
        loop_env = lisptype.Environment(env)
        
        # Handle different sequence types
        if isinstance(seq, str):
            # String - iterate over characters
            for char in seq:
                check_loop_timeout()
                _bind_varspec(loop_env, iteration_var, lisptype.Character(char))
                bind_aux(loop_env)
                execute_iteration_body(loop_env)
        elif hasattr(seq, '__iter__') and hasattr(seq, '__len__') and not _consp_internal(seq):
            # Array/vector with __iter__ (AdjustableVector, list, tuple, etc.)
            for elem in seq:
                check_loop_timeout()
                _bind_varspec(loop_env, iteration_var, elem)
                bind_aux(loop_env)
                execute_iteration_body(loop_env)
        else:
            raise lisptype.LispNotImplementedError(f'LOOP FOR ACROSS requires a vector or string, got {type(seq).__name__}')
            
    elif iteration_type == 'for-equals':
        # FOR var = init-form [THEN step-form]
        # iteration_start = init-form, iteration_step = step-form or None
        loop_env = lisptype.Environment(env)
        # Initial value
        cur_value = eval(iteration_start, loop_env)
        _bind_varspec(loop_env, iteration_var, cur_value)
        
        first_iteration = True
        while True:
            check_loop_timeout()
            bind_aux(loop_env)
            
            # Check termination condition
            if iteration_test is not None:
                test_result = eval(iteration_test, loop_env)
                if termination_type == 'until' and lisptype.is_truthy(test_result):
                    break
                if termination_type == 'while' and not lisptype.is_truthy(test_result):
                    break
            
            execute_iteration_body(loop_env)
            first_iteration = False
            
            # Step to next value
            if iteration_step is not None:
                cur_value = eval(iteration_step, loop_env)
                _bind_varspec(loop_env, iteration_var, cur_value)
            else:
                # Without THEN, just re-evaluate init-form
                cur_value = eval(iteration_start, loop_env)
                _bind_varspec(loop_env, iteration_var, cur_value)
                
    elif iteration_type is None:
        # No iteration - simple loop body, execute once
        # Or infinite loop if there are body forms
        if body_forms or accumulation:
            execute_iteration_body(env)
    
    # Execute FINALLY forms
    for f in finally_forms:
        result = eval(f, env)
    
    # Return accumulated result or last result
    if accumulation:
        acc_type = accumulation[0]
        if acc_type in ('collect', 'append', 'nconc'):
            # Convert accumulated list to Lisp list
            if not accumulated:
                return lisptype.NIL
            result_list = lisptype.NIL
            for item in reversed(accumulated):
                result_list = cons(item, result_list)
            return result_list
        elif acc_type == 'sum':
            return sum_result
        elif acc_type == 'count':
            return count_result
    
    return result


def eval_do(form, env):
    """Evaluate DO special form.
    
    (DO ((var init [step])*)
        (end-test result-form*)
        declaration*
        {tag | statement}*)
    
    DO evaluates init forms in parallel (like LET) and binds variables.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    # Parse variable bindings
    var_list = car(args)
    args = cdr(args)
    
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DO requires end-test clause")
    
    # Parse end-test clause  
    end_clause = car(args)
    body = cdr(args)
    
    if not _consp_internal(end_clause):
        raise lisptype.LispNotImplementedError("DO end-test must be a list")
    
    end_test = car(end_clause)
    result_forms = cdr(end_clause)
    
    # Create new environment
    loop_env = lisptype.Environment(env)
    
    # Parse var specs and evaluate init forms IN PARALLEL (collect first)
    var_specs = []
    current = var_list
    while _consp_internal(current):
        spec = car(current)
        if isinstance(spec, lisptype.LispSymbol):
            var_specs.append((spec, lisptype.NIL, None))
        elif _consp_internal(spec):
            var = car(spec)
            init_form = car(cdr(spec)) if _consp_internal(cdr(spec)) else lisptype.NIL
            step_form = car(cdr(cdr(spec))) if _consp_internal(cdr(cdr(spec))) else None
            var_specs.append((var, init_form, step_form))
        current = cdr(current)
    
    # Evaluate all init forms first (parallel binding like LET)
    init_values = [eval(init_form, env) for var, init_form, _ in var_specs]
    
    # Bind variables
    for (var, _, _), value in zip(var_specs, init_values):
        loop_env.set_variable(var, value)
    
    # Main loop with timeout warning
    loop_start_time = time.time()
    loop_iterations = 0
    warning_printed = False
    
    while True:
        # Check timeout
        loop_iterations += 1
        if LOOP_TIMEOUT_WARNING > 0 and not warning_printed:
            elapsed = time.time() - loop_start_time
            if elapsed > LOOP_TIMEOUT_WARNING:
                warning_printed = True
                print(f"\n*** DO LOOP WARNING: Loop has been running for {elapsed:.1f}s ({loop_iterations} iterations) ***", file=sys.stderr)
                print(f"*** DO end_test: {end_test} ***", file=sys.stderr)
                print(f"*** DO var_specs: {var_specs} ***", file=sys.stderr)
                sys.stderr.flush()
        
        # Check end-test
        if lisptype.is_truthy(eval(end_test, loop_env)):
            # Evaluate result forms and return last value
            result = lisptype.NIL
            current = result_forms
            while _consp_internal(current):
                result = eval(car(current), loop_env)
                current = cdr(current)
            return result
        
        # Execute body
        current = body
        while _consp_internal(current):
            eval(car(current), loop_env)
            current = cdr(current)
        
        # Update variables (evaluate all step forms first, then update)
        new_values = []
        for var, _, step_form in var_specs:
            if step_form is not None:
                new_values.append((var, eval(step_form, loop_env)))
        
        for var, value in new_values:
            loop_env.set_variable(var, value)


def eval_do_star(form, env):
    """Evaluate DO* special form.
    
    (DO* ((var init [step])*)
         (end-test result-form*)
         declaration*
         {tag | statement}*)
    
    DO* evaluates init forms sequentially (like LET*) and binds variables.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    # Parse variable bindings
    var_list = car(args)
    args = cdr(args)
    
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DO* requires end-test clause")
    
    # Parse end-test clause  
    end_clause = car(args)
    body = cdr(args)
    
    if not _consp_internal(end_clause):
        raise lisptype.LispNotImplementedError("DO* end-test must be a list")
    
    end_test = car(end_clause)
    result_forms = cdr(end_clause)
    
    # Create new environment
    loop_env = lisptype.Environment(env)
    
    # Parse var specs and evaluate init forms SEQUENTIALLY (like LET*)
    var_specs = []
    current = var_list
    while _consp_internal(current):
        spec = car(current)
        if isinstance(spec, lisptype.LispSymbol):
            loop_env.set_variable(spec, lisptype.NIL)
            var_specs.append((spec, None))
        elif _consp_internal(spec):
            var = car(spec)
            init_form = car(cdr(spec)) if _consp_internal(cdr(spec)) else lisptype.NIL
            step_form = car(cdr(cdr(spec))) if _consp_internal(cdr(cdr(spec))) else None
            # Evaluate in current loop_env (sequential)
            init_value = eval(init_form, loop_env)
            loop_env.set_variable(var, init_value)
            var_specs.append((var, step_form))
        current = cdr(current)
    
    # Main loop with timeout warning
    loop_start_time = time.time()
    loop_iterations = 0
    warning_printed = False
    
    while True:
        # Check timeout
        loop_iterations += 1
        if LOOP_TIMEOUT_WARNING > 0 and not warning_printed:
            elapsed = time.time() - loop_start_time
            if elapsed > LOOP_TIMEOUT_WARNING:
                warning_printed = True
                print(f"\n*** DO* LOOP WARNING: Loop has been running for {elapsed:.1f}s ({loop_iterations} iterations) ***", file=sys.stderr)
                print(f"*** DO* end_test: {end_test} ***", file=sys.stderr)
                print(f"*** DO* var_specs: {var_specs} ***", file=sys.stderr)
                sys.stderr.flush()
        
        # Check end-test
        if lisptype.is_truthy(eval(end_test, loop_env)):
            # Evaluate result forms and return last value
            result = lisptype.NIL
            current = result_forms
            while _consp_internal(current):
                result = eval(car(current), loop_env)
                current = cdr(current)
            return result
        
        # Execute body
        current = body
        while _consp_internal(current):
            eval(car(current), loop_env)
            current = cdr(current)
        
        # Update variables sequentially
        for var, step_form in var_specs:
            if step_form is not None:
                new_value = eval(step_form, loop_env)
                loop_env.set_variable(var, new_value)


def eval_dolist(form, env):
    """Evaluate DOLIST special form.
    
    (DOLIST (var list-form [result-form]) declaration* {tag | statement}*)
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    # Parse (var list-form [result-form])
    var_clause = car(args)
    body = cdr(args)
    
    if not _consp_internal(var_clause):
        raise lisptype.LispNotImplementedError("DOLIST requires (var list-form) clause")
    
    var = car(var_clause)
    list_form = car(cdr(var_clause)) if _consp_internal(cdr(var_clause)) else lisptype.NIL
    result_form = car(cdr(cdr(var_clause))) if _consp_internal(cdr(cdr(var_clause))) else lisptype.NIL
    
    # Evaluate list
    lst = eval(list_form, env)
    
    # Create loop environment
    loop_env = lisptype.Environment(env)
    loop_env.set_variable(var, lisptype.NIL)
    
    # Iterate over list
    current_list = lst
    while _consp_internal(current_list):
        loop_env.set_variable(var, car(current_list))
        
        # Execute body
        current = body
        while _consp_internal(current):
            eval(car(current), loop_env)
            current = cdr(current)
        
        current_list = cdr(current_list)
    
    # Set var to NIL for result form
    loop_env.set_variable(var, lisptype.NIL)
    
    # Evaluate and return result form
    return eval(result_form, loop_env)


def eval_dotimes(form, env):
    """Evaluate DOTIMES special form.
    
    (DOTIMES (var count-form [result-form]) declaration* {tag | statement}*)
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    # Parse (var count-form [result-form])
    var_clause = car(args)
    body = cdr(args)
    
    if not _consp_internal(var_clause):
        raise lisptype.LispNotImplementedError("DOTIMES requires (var count-form) clause")
    
    var = car(var_clause)
    count_form = car(cdr(var_clause)) if _consp_internal(cdr(var_clause)) else 0
    result_form = car(cdr(cdr(var_clause))) if _consp_internal(cdr(cdr(var_clause))) else lisptype.NIL
    
    # Evaluate count
    count = eval(count_form, env)
    if not isinstance(count, (int, float)):
        count = 0
    count = int(count)
    
    # Create loop environment
    loop_env = lisptype.Environment(env)
    
    # Iterate count times
    for i in range(count):
        loop_env.set_variable(var, i)
        
        # Execute body
        current = body
        while _consp_internal(current):
            eval(car(current), loop_env)
            current = cdr(current)
    
    # Set var to count for result form
    loop_env.set_variable(var, count)
    
    # Evaluate and return result form
    return eval(result_form, loop_env)


__all__ = [
    'eval_when',
    'eval_unless',
    'eval_cond',
    'eval_and',
    'eval_or',
    'eval_progn',
    'eval_locally',
    'eval_let',
    'eval_letstar',
    'eval_quasiquote',
    'eval_prog1',
    'eval_prog2',
    'eval_loop',
    'eval_do',
    'eval_do_star',
    'eval_dolist',
    'eval_dotimes',
]
