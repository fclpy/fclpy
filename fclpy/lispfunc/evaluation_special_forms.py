"""Special forms: QUOTE, IF, DEFUN, DEFMACRO, LAMBDA, declarations.

This module contains handlers for special forms that don't fall into
control flow, loops/conditionals, or condition handling categories.

DEFSTRUCT: Accept keywords as structure names (v2).
"""

import fclpy.lisptype as lisptype
import fclpy.state as state
from fclpy.lispfunc.core import car, cdr, _consp_internal, cons
from . import registry as _registry
import logging
import sys

logger = logging.getLogger(__name__)


def _extract_tail_symbol_from_rest(rest_param):
    """Return the tail symbol from a rest destructuring spec or None.

    Handles cases like (HEAD . TAIL) where rest_param.cdr may be a symbol
    or wrapped as a tiny cons. This makes binding robust against different
    parser representations.
    """
    if not _consp_internal(rest_param):
        return None

    tail = getattr(rest_param, 'cdr', None)

    # Common direct symbol case
    if isinstance(tail, lisptype.LispSymbol):
        return tail

    # Defensive: tail might be a tiny cons whose cdr/car holds the symbol
    try:
        if isinstance(tail, lisptype.lispCons):
            # If the car is a symbol and cdr is NIL, treat car as the tail symbol
            if isinstance(tail.car, lisptype.LispSymbol):
                return tail.car
            # If the cdr is a symbol (rare representation), use it
            if isinstance(tail.cdr, lisptype.LispSymbol):
                return tail.cdr
    except Exception:
        pass

    # If object has a 'name' attribute that looks like a symbol name, intern it
    if hasattr(tail, 'name') and isinstance(getattr(tail, 'name'), str):
        try:
            return lisptype.intern_symbol(tail.name)
        except Exception:
            pass

    # If tail is a plain string, convert to a symbol via py_str_to_sym if available
    if isinstance(tail, str):
        try:
            return lisptype.py_str_to_sym(tail)
        except Exception:
            return lisptype.LispSymbol(tail)

    # Last-resort attempt: try to parse repr() as a symbol name
    try:
        rep = repr(tail)
        rep_clean = rep.strip("() ")
        if rep_clean.isidentifier() or rep_clean.isupper():
            try:
                return lisptype.intern_symbol(rep_clean)
            except Exception:
                return lisptype.LispSymbol(rep_clean)
    except Exception:
        pass

    return None

def eval_if(form, env):
    """Evaluate IF special form."""
    # Import eval lazily to avoid circular imports
    from .evaluation_core import eval
    import sys
    
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


def eval_psetq(form, env):
    """Evaluate PSETQ special form.

    Syntax: (PSETQ var1 val1 var2 val2 ...)

    Like SETQ, but all value-forms are evaluated first (left to right,
    using the OLD values of any vars they reference), and only then are
    all the vars assigned. Always returns NIL.
    """
    from .evaluation_core import eval

    args = cdr(form)
    pairs = []

    while _consp_internal(args) and _consp_internal(cdr(args)):
        var = car(args)
        value_form = car(cdr(args))

        if not isinstance(var, lisptype.LispSymbol):
            raise lisptype.LispNotImplementedError("PSETQ: variable must be a symbol")

        pairs.append((var, eval(value_form, env)))
        args = cdr(cdr(args))

    for var, value in pairs:
        env.set_variable(var, value)

    return lisptype.NIL


def eval_the(form, env):
    """Evaluate THE special operator.

    THE is a type assertion form. Per ANSI CL tests used here we should
    not evaluate the type-designator (first argument) as a variable; only
    evaluate and return the second argument (the expression). This keeps
    constructs like (THE SYMBOL 'T) from attempting to look up SYMBOL as
    a variable.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("THE requires at least two arguments")

    type_spec = car(args)
    expr = car(cdr(args)) if _consp_internal(cdr(args)) else lisptype.NIL

    # Do not evaluate type_spec here; evaluate and return the expression value
    return eval(expr, env)


def eval_incf(form, env):
    """Evaluate INCF special form - increment a place.
    
    (INCF place) increments place by 1
    (INCF place delta) increments place by delta
    
    Currently only supports simple variable places, not general setf-able places.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("INCF requires at least 1 argument")
    
    place = car(args)
    
    # Get delta (default 1)
    delta_form = car(cdr(args)) if _consp_internal(cdr(args)) else 1
    if delta_form != 1:
        delta = eval(delta_form, env)
    else:
        delta = 1
    
    # Handle simple variable case
    if isinstance(place, lisptype.LispSymbol):
        # Use find_variable (not lookup) to get the current binding
        if env.has_variable(place):
            current_value = env.find_variable(place)
        else:
            current_value = 0
        new_value = current_value + delta
        env.set_variable(place, new_value)
        return new_value

    # Support INCF on simple AREF/SVREF places (e.g., (INCF (AREF arr idx)))
    if _consp_internal(place):
        place_op = car(place)
        place_args = cdr(place)
        if isinstance(place_op, lisptype.LispSymbol):
            op_name = place_op.name
            if op_name in ('AREF', 'SVREF'):
                # Evaluate target and index
                arr = eval(car(place_args), env)
                idx = eval(car(cdr(place_args)), env)
                try:
                    current_value = arr[idx]
                except IndexError:
                    if isinstance(arr, list) and isinstance(idx, int) and idx >= 0:
                        # Treat missing elements as NIL
                        current_value = lisptype.NIL
                    else:
                        raise lisptype.LispError(f"INCF {op_name}: index out of range")

                # Compute new value (assume numeric increment semantics)
                try:
                    new_value = current_value + delta
                except Exception:
                    raise lisptype.LispTypeError(actual_value=current_value, expected_type='number', message="INCF: cannot add delta to place value")

                # Perform assignment using same safe behavior as SETF AREF
                try:
                    arr[idx] = new_value
                except IndexError:
                    if isinstance(arr, list) and isinstance(idx, int) and idx >= 0:
                        needed = idx + 1 - len(arr)
                        arr.extend([lisptype.NIL] * needed)
                        arr[idx] = new_value
                    else:
                        raise lisptype.LispError(f"INCF {op_name}: list assignment index out of range")
                return new_value

    raise lisptype.LispNotImplementedError(f"INCF: complex places not yet supported: {place}")


def eval_decf(form, env):
    """Evaluate DECF special form - decrement a place.
    
    (DECF place) decrements place by 1
    (DECF place delta) decrements place by delta
    
    Currently only supports simple variable places, not general setf-able places.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DECF requires at least 1 argument")
    
    place = car(args)
    
    # Get delta (default 1)
    delta_form = car(cdr(args)) if _consp_internal(cdr(args)) else 1
    if delta_form != 1:
        delta = eval(delta_form, env)
    else:
        delta = 1
    
    # Handle simple variable case
    if isinstance(place, lisptype.LispSymbol):
        # Use find_variable (not lookup) to get the current binding
        if env.has_variable(place):
            current_value = env.find_variable(place)
        else:
            current_value = 0
        new_value = current_value - delta
        env.set_variable(place, new_value)
        return new_value

    # Support DECF on simple AREF/SVREF places (e.g., (DECF (AREF arr idx)))
    if _consp_internal(place):
        place_op = car(place)
        place_args = cdr(place)
        if isinstance(place_op, lisptype.LispSymbol):
            op_name = place_op.name
            if op_name in ('AREF', 'SVREF'):
                arr = eval(car(place_args), env)
                idx = eval(car(cdr(place_args)), env)
                try:
                    current_value = arr[idx]
                except IndexError:
                    if isinstance(arr, list) and isinstance(idx, int) and idx >= 0:
                        current_value = lisptype.NIL
                    else:
                        raise lisptype.LispError(f"DECF {op_name}: index out of range")

                try:
                    new_value = current_value - delta
                except Exception:
                    raise lisptype.LispTypeError(actual_value=current_value, expected_type='number', message="DECF: cannot subtract delta from place value")

                try:
                    arr[idx] = new_value
                except IndexError:
                    if isinstance(arr, list) and isinstance(idx, int) and idx >= 0:
                        needed = idx + 1 - len(arr)
                        arr.extend([lisptype.NIL] * needed)
                        arr[idx] = new_value
                    else:
                        raise lisptype.LispError(f"DECF {op_name}: list assignment index out of range")
                return new_value

    raise lisptype.LispNotImplementedError(f"DECF: complex places not yet supported: {place}")


@_registry.cl_macro('WITH-OPEN-FILE', documentation='WITH-OPEN-FILE macro expander')
def with_open_file_macro(bindings, *body):
    """Macro expander for WITH-OPEN-FILE.

    Transforms:
      (WITH-OPEN-FILE (var filespec &key ...) body...)
    into:
      (LET ((var (OPEN filespec &key ...)))
        (UNWIND-PROTECT
            (PROGN body...)
            (CLOSE var)))
    """
    # Helper to convert Python iterable to lispCons list
    def to_cons(seq):
        cur = lisptype.NIL
        for e in reversed(seq):
            cur = lisptype.lispCons(e, cur)
        return cur

    # Normalize binding forms (single binding or list of bindings)
    binding_forms = []
    if isinstance(bindings, lisptype.lispCons) and isinstance(bindings.car if hasattr(bindings, 'car') else None, lisptype.LispSymbol):
        # Single binding: (var filespec ...)
        binding_forms = [bindings]
    else:
        # Multiple bindings
        cur = bindings
        while isinstance(cur, lisptype.lispCons):
            binding_forms.append(cur.car)
            cur = cur.cdr

    let_bindings = lisptype.NIL
    close_forms = lisptype.NIL

    for b in reversed(binding_forms):
        stream_sym = b.car
        filespec = b.cdr.car if isinstance(b.cdr, lisptype.lispCons) else lisptype.NIL

        # Build OPEN call: (OPEN filespec &key ...)
        elems = [lisptype.LispSymbol('OPEN'), filespec]
        rest = b.cdr.cdr if isinstance(b.cdr, lisptype.lispCons) else lisptype.NIL
        cur_kw = rest
        while isinstance(cur_kw, lisptype.lispCons):
            key = cur_kw.car
            val = cur_kw.cdr.car if isinstance(cur_kw.cdr, lisptype.lispCons) else lisptype.NIL
            elems.append(key)
            elems.append(val)
            if isinstance(cur_kw.cdr, lisptype.lispCons) and isinstance(cur_kw.cdr.cdr, lisptype.lispCons):
                cur_kw = cur_kw.cdr.cdr
            else:
                break

        open_call = to_cons(elems)

        # Create binding pair: (var open-call)
        binding_pair = lisptype.lispCons(stream_sym, lisptype.lispCons(open_call, lisptype.NIL))
        let_bindings = lisptype.lispCons(binding_pair, let_bindings)

        # Create close form: (CLOSE var)
        close_form = lisptype.lispCons(lisptype.LispSymbol('CLOSE'), lisptype.lispCons(stream_sym, lisptype.NIL))
        close_forms = lisptype.lispCons(close_form, close_forms)

    # Build PROGN for body
    if body:
        progn_sym = lisptype.LispSymbol('PROGN')
        body_list = lisptype.NIL
        for f in reversed(body):
            body_list = lisptype.lispCons(f, body_list)
        progn_form = lisptype.lispCons(progn_sym, body_list)
    else:
        progn_form = lisptype.NIL

    # Build UNWIND-PROTECT: (UNWIND-PROTECT progn (PROGN close-forms...))
    close_progn = lisptype.lispCons(lisptype.LispSymbol('PROGN'), close_forms) if close_forms is not lisptype.NIL else lisptype.NIL
    unwind = lisptype.lispCons(lisptype.LispSymbol('UNWIND-PROTECT'), lisptype.lispCons(progn_form, lisptype.lispCons(close_progn, lisptype.NIL)))

    # Build LET: (LET bindings unwind)
    let_form = lisptype.lispCons(lisptype.LispSymbol('LET'), lisptype.lispCons(let_bindings, lisptype.lispCons(unwind, lisptype.NIL)))
    return let_form


def eval_defun(form, env):
    """Evaluate DEFUN special form.
    
    DEFUN defines a function in the GLOBAL environment, not the local one.
    This is standard Common Lisp behavior - DEFUN creates top-level function bindings.
    
    Supports:
    - Required parameters
    - &optional parameters with default values
    - &rest parameter for collecting remaining arguments
    - &key parameters for keyword arguments
    - Function names as symbols or (SETF symbol) for setf functions
    """
    from .evaluation_core import eval, parse_lambda_list, ReturnFromException
    import fclpy.state as state

    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("DEFUN requires at least 2 arguments")

    func_name_spec = car(args)
    param_list = car(cdr(args))
    body = cdr(cdr(args))

    # func_name_spec can be a symbol or (SETF symbol) for setf functions
    if isinstance(func_name_spec, lisptype.LispSymbol):
        # Simple function name
        func_name = func_name_spec
        is_setf = False
        # DEFUN establishes an implicit block named after the function
        block_name_symbol = func_name
    elif _consp_internal(func_name_spec):
        # (SETF symbol) form for setf functions
        setf_sym = car(func_name_spec)
        if not (isinstance(setf_sym, lisptype.LispSymbol) and setf_sym.name == 'SETF'):
            raise lisptype.LispNotImplementedError("DEFUN: function name must be a symbol or (SETF symbol)")
        rest = cdr(func_name_spec)
        if not _consp_internal(rest):
            raise lisptype.LispNotImplementedError("DEFUN: (SETF symbol) requires a symbol")
        actual_func_name = car(rest)
        if not isinstance(actual_func_name, lisptype.LispSymbol):
            raise lisptype.LispNotImplementedError("DEFUN: (SETF symbol) requires symbol as second element")
        # Create a synthetic symbol for the setf function: (SETF |name|)
        # For storage, we create a LispSymbol with a compound name
        func_name = lisptype.LispSymbol(f"(SETF {actual_func_name.name})")
        is_setf = True
        # The implicit block for a (SETF symbol) function is named by symbol
        block_name_symbol = actual_func_name
    else:
        raise lisptype.LispNotImplementedError("DEFUN: function name must be a symbol or (SETF symbol)")
    
    # Extract docstring if present (first form in body can be a string)
    docstring = None
    actual_body = body
    if _consp_internal(body):
        first_form = car(body)
        if isinstance(first_form, (str, lisptype.LispString)):
            docstring = str(first_form)  # Convert to Python str for storage
            actual_body = cdr(body)
    
    # Parse the lambda list
    parsed = parse_lambda_list(param_list)
    required_params = parsed['required']
    optional_params = parsed['optional']
    rest_param = parsed['rest']
    keyword_params = parsed['keyword']
    aux_params = parsed.get('aux', [])
    
    # Create function closure
    # The closure captures the current lexical environment for variable lookups
    def user_function(*call_args):
        # Create new environment for function execution
        func_env = lisptype.Environment(env)
        
        arg_index = 0
        
        # Bind required parameters
        for param in required_params:
            if arg_index < len(call_args):
                func_env.add_variable(param, call_args[arg_index])
                arg_index += 1
            else:
                func_env.add_variable(param, lisptype.NIL)
        
        # Bind optional parameters (support supplied-p variable)
        for param_spec in optional_params:
            if _consp_internal(param_spec):
                param = car(param_spec)
                rest = cdr(param_spec)
                default_form = car(rest) if _consp_internal(rest) else None
                rest2 = cdr(rest) if _consp_internal(rest) else None
                supplied_p = car(rest2) if _consp_internal(rest2) else None
            else:
                param = param_spec
                default_form = None
                supplied_p = None

            if arg_index < len(call_args):
                func_env.add_variable(param, call_args[arg_index])
                if supplied_p is not None:
                    func_env.add_variable(supplied_p, lisptype.T)
                arg_index += 1
            else:
                # Use default value if provided, otherwise NIL
                if default_form is not None:
                    default_value = eval(default_form, func_env)
                    func_env.add_variable(param, default_value)
                else:
                    func_env.add_variable(param, lisptype.NIL)
                if supplied_p is not None:
                    func_env.add_variable(supplied_p, lisptype.NIL)
        
        # Collect remaining positional arguments for &rest
        remaining_positional = []
        
        # Find where keyword arguments start
        keyword_start = arg_index
        for i in range(arg_index, len(call_args)):
            if isinstance(call_args[i], lisptype.lispKeyword):
                keyword_start = i
                break
            remaining_positional.append(call_args[i])
            arg_index = i + 1
        
        # Bind &rest parameter if present
        if rest_param:
            # Rest gets all remaining positional args as a list
            if remaining_positional:
                rest_list = lisptype.NIL
                for item in reversed(remaining_positional):
                    rest_list = lisptype.lispCons(item, rest_list)
            else:
                rest_list = lisptype.NIL

            # Support destructuring rest spec: either a symbol or a cons
            if isinstance(rest_param, lisptype.LispSymbol):
                func_env.add_variable(rest_param, rest_list)
            elif _consp_internal(rest_param):
                # Dotted pair like (head . tail): bind head to first element,
                # tail to the cdr (list) of the rest_list.
                head = car(rest_param)
                tail = rest_param.cdr
                if _consp_internal(rest_list):
                    first = car(rest_list)
                    rest_tail = cdr(rest_list)
                else:
                    first = lisptype.NIL
                    rest_tail = lisptype.NIL

                if isinstance(head, lisptype.LispSymbol):
                    func_env.add_variable(head, first)
                if isinstance(tail, lisptype.LispSymbol):
                    func_env.add_variable(tail, rest_tail)
                else:
                    # If tail isn't a symbol, bind the whole rest_list
                    func_env.add_variable(rest_param, rest_list)
        
        # Bind keyword parameters
        # First, initialize all keyword params to their defaults and supplied-p to NIL
        for param_spec in keyword_params:
            if _consp_internal(param_spec):
                param = car(param_spec)
                rest = cdr(param_spec)
                default_form = car(rest) if _consp_internal(rest) else None
                # Check for supplied-p parameter (third element)
                rest2 = cdr(rest) if _consp_internal(rest) else None
                supplied_p = car(rest2) if _consp_internal(rest2) else None
            else:
                param = param_spec
                default_form = None
                supplied_p = None
            
            # Default value
            if default_form is not None:
                default_value = eval(default_form, func_env)
                func_env.add_variable(param, default_value)
            else:
                func_env.add_variable(param, lisptype.NIL)
            
            # Initialize supplied-p to NIL (not supplied yet)
            if supplied_p is not None:
                func_env.add_variable(supplied_p, lisptype.NIL)
        
        # Now process actual keyword arguments from the call
        i = keyword_start
        while i < len(call_args) - 1:
            key = call_args[i]
            value = call_args[i + 1]
            
            if isinstance(key, lisptype.lispKeyword):
                key_name = key.name.upper()
                # Find matching parameter
                for param_spec in keyword_params:
                    if _consp_internal(param_spec):
                        param = car(param_spec)
                        rest = cdr(param_spec)
                        rest2 = cdr(rest) if _consp_internal(rest) else None
                        supplied_p = car(rest2) if _consp_internal(rest2) else None
                    else:
                        param = param_spec
                        supplied_p = None
                    
                    if isinstance(param, lisptype.LispSymbol) and param.name.upper() == key_name:
                        func_env.add_variable(param, value)
                        # Set supplied-p to T when keyword is provided
                        if supplied_p is not None:
                            func_env.add_variable(supplied_p, lisptype.T)
                        break
                i += 2
            else:
                i += 1
        
        # Bind &aux parameters
        for param_spec in aux_params:
            if _consp_internal(param_spec):
                param = car(param_spec)
                init_form = car(cdr(param_spec))
                init_value = eval(init_form, func_env)
                func_env.add_variable(param, init_value)
            else:
                func_env.add_variable(param_spec, lisptype.NIL)
        
        # Execute body, enclosed in the implicit block DEFUN establishes
        # around the function body (named after the function).
        result = None
        try:
            current_body = actual_body
            while _consp_internal(current_body):
                result = eval(car(current_body), func_env)
                current_body = cdr(current_body)
        except ReturnFromException as e:
            tag = e.tag
            block_match = False
            if tag == block_name_symbol:
                block_match = True
            elif isinstance(tag, lisptype.LispSymbol) and isinstance(block_name_symbol, lisptype.LispSymbol):
                block_match = (tag.name == block_name_symbol.name)
            if block_match:
                result = e.value
            else:
                raise

        return result
    
    # Find the global/root environment for defining the function
    # DEFUN always creates global function bindings
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    # Add function to the GLOBAL environment (not local)
    global_env.add_function(func_name, user_function)
    
    # Also add to the current environment for immediate visibility
    # (this helps when the function is called later in the same file)
    if env is not global_env:
        env.add_function(func_name, user_function)
    
    # Store docstring on the function symbol's property list
    if docstring:
        if not hasattr(func_name, 'plist'):
            func_name.plist = {}
        func_name.plist['DOCUMENTATION'] = docstring
    
    return func_name
def _create_macro_function(macro_name, lambda_list, body, env):
    """Create a macro function callable from lambda-list and body.
    
    This is used by both DEFMACRO and MACROLET to create macro functions.
    The resulting function has __is_macro__ = True and captures the defining
    environment for proper lexical scoping.
    
    Args:
        macro_name: LispSymbol for the macro name (used for debugging)
        lambda_list: Lisp list of parameter specifications
        body: Lisp list of body forms
        env: The environment where the macro is defined (captured for closure)
    
    Returns:
        A callable with __is_macro__ = True
    """
    from .evaluation_core import eval, parse_lambda_list

    # Extract docstring if present (first form in body can be a string)
    docstring = None
    actual_body = body
    if _consp_internal(body):
        first_form = car(body)
        if isinstance(first_form, (str, lisptype.LispString)):
            docstring = str(first_form)  # Convert to Python str for storage
            actual_body = cdr(body)

    # Parse lambda list to handle &optional, &rest, &key, &whole, &environment etc.
    parsed_params = parse_lambda_list(lambda_list)
    
    required_params = parsed_params.get('required', [])
    optional_params = parsed_params.get('optional', [])
    rest_param = parsed_params.get('rest', None)
    keyword_params = parsed_params.get('keyword', [])
    environment_param = parsed_params.get('environment', None)

    # Create the macro callable
    def macro_callable(*call_args):
        # Create a new environment extending the definition environment
        macro_env = lisptype.Environment(parent=env)

        # Detect optional trailing expansion-time Environment argument.
        # The macroexpander may invoke the macro callable with the
        # expansion environment as the last positional argument. If so,
        # use it for &ENVIRONMENT bindings; otherwise use the captured
        # definition environment.
        expansion_env = env
        if len(call_args) > 0 and isinstance(call_args[-1], lisptype.Environment):
            expansion_env = call_args[-1]
            call_args = tuple(call_args[:-1])

        # Normalize NIL symbol arguments to the canonical NIL object
        new_args = []
        for a in call_args:
            if isinstance(a, lisptype.LispSymbol) and a.name.upper() == 'NIL':
                new_args.append(lisptype.NIL)
            else:
                new_args.append(a)
        call_args = tuple(new_args)
        
        arg_idx = 0

        # Handle &WHOLE parameter
        whole_param = parsed_params.get('whole') if isinstance(parsed_params, dict) else None
        if whole_param is not None:
            if len(call_args) > 0:
                macro_env.add_variable(whole_param, call_args[0])
                arg_idx = 1
            else:
                macro_env.add_variable(whole_param, lisptype.NIL)
                arg_idx = 1
        
        # Bind &ENVIRONMENT to the expansion-time environment if provided
        # The macro callable may be invoked with an extra trailing Environment
        # argument by the macroexpander. If so, prefer that; otherwise fall
        # back to the environment captured at definition time.
        if environment_param is not None:
            macro_env.add_variable(environment_param, expansion_env)
        
        # Bind required parameters
        for param in required_params:
            # Support destructuring parameter specs (e.g., (arg1 (&whole w arg2)))
            if isinstance(param, lisptype.lispCons):
                # The actual argument value (may be a list)
                val = call_args[arg_idx] if arg_idx < len(call_args) else lisptype.NIL
                # Detect nested destructuring patterns
                # Case 1: (&WHOLE w ...)
                whole_sym = None
                destruct_syms = []
                inner = param if _consp_internal(param) else None
                if _consp_internal(inner) and isinstance(car(inner), lisptype.LispSymbol) and car(inner).name.upper() == '&WHOLE':
                    # (&WHOLE w a b ...)
                    nxt = car(cdr(inner)) if _consp_internal(cdr(inner)) else None
                    if isinstance(nxt, lisptype.LispSymbol):
                        whole_sym = nxt
                        tmp = cdr(cdr(inner)) if _consp_internal(cdr(inner)) else lisptype.NIL
                        while _consp_internal(tmp):
                            s = car(tmp)
                            if isinstance(s, lisptype.LispSymbol):
                                destruct_syms.append(s)
                            tmp = cdr(tmp)

                # Case 2: ((&KEY foo bar)) or similar key-destructuring
                key_syms = None
                if _consp_internal(inner) and isinstance(car(inner), lisptype.LispSymbol) and car(inner).name.upper() == '&KEY':
                    # inner is (&KEY foo (bar default) (baz default supplied-p) ...)
                    # Preserve the full spec element (symbol or list) so defaults
                    # and supplied-p parameter names are available for binding.
                    key_syms = []
                    tmpk = cdr(inner)
                    while _consp_internal(tmpk):
                        s = car(tmpk)
                        # Keep either the symbol or the full spec list
                        if isinstance(s, lisptype.LispSymbol) or _consp_internal(s):
                            key_syms.append(s)
                        tmpk = cdr(tmpk)

                # Bind whole symbol if present
                if whole_sym is not None:
                    macro_env.add_variable(whole_sym, val)
                    # Also bind the remaining destructuring symbols
                    curv = val
                    for ds in destruct_syms:
                        if _consp_internal(curv):
                            macro_env.add_variable(ds, car(curv))
                            curv = cdr(curv)
                        else:
                            macro_env.add_variable(ds, lisptype.NIL)

                # Handle key destructuring: look up keywords in val list
                if key_syms is not None:
                    # val expected to be a list of keyword pairs or NIL
                    # initialize all keys to their defaults (if provided) and
                    # initialize any supplied-p variables to NIL.
                    for ks in key_syms:
                        if isinstance(ks, lisptype.LispSymbol):
                            macro_env.add_variable(ks, lisptype.NIL)
                        elif _consp_internal(ks):
                            key_name = car(ks)
                            default_form = car(cdr(ks)) if _consp_internal(cdr(ks)) else None
                            rest2 = cdr(cdr(ks)) if _consp_internal(cdr(ks)) else None
                            supplied_p = car(rest2) if _consp_internal(rest2) else None
                            if default_form is not None:
                                default_value = eval(default_form, macro_env)
                                macro_env.add_variable(key_name, default_value)
                            else:
                                macro_env.add_variable(key_name, lisptype.NIL)
                            if supplied_p is not None:
                                macro_env.add_variable(supplied_p, lisptype.NIL)
                    # Process keyword-value pairs if val is a cons
                    # Track which keys have been seen (CL uses first occurrence for duplicates)
                    seen_keys = set()
                    if isinstance(val, lisptype.lispCons):
                        curv = val
                        while _consp_internal(curv):
                            k = car(curv)
                            v = car(cdr(curv)) if _consp_internal(cdr(curv)) else lisptype.NIL
                            # Match keyword to parameter symbol
                            if isinstance(k, lisptype.lispKeyword):
                                name = k.name.upper()
                                # Only bind if this key hasn't been seen before (first occurrence wins)
                                if name not in seen_keys:
                                    for ks in key_syms:
                                        # ks may be a bare symbol or a spec list
                                        if isinstance(ks, lisptype.LispSymbol):
                                            if ks.name.upper() == name:
                                                macro_env.add_variable(ks, v)
                                                seen_keys.add(name)
                                                break
                                        elif _consp_internal(ks):
                                            key_name_sym = car(ks)
                                            if isinstance(key_name_sym, lisptype.LispSymbol) and key_name_sym.name.upper() == name:
                                                # bind the provided value
                                                macro_env.add_variable(key_name_sym, v)
                                                # set supplied-p to T if requested
                                                rest2 = cdr(cdr(ks)) if _consp_internal(cdr(ks)) else None
                                                supplied_p = car(rest2) if _consp_internal(rest2) else None
                                                if supplied_p is not None:
                                                    macro_env.add_variable(supplied_p, lisptype.T)
                                                seen_keys.add(name)
                                                break
                            # advance by two if well-formed, otherwise break
                            if _consp_internal(cdr(curv)):
                                curv = cdr(cdr(curv))
                            else:
                                break
                    arg_idx += 1
                    continue
                arg_idx += 1
                continue

            if arg_idx < len(call_args):
                macro_env.add_variable(param, call_args[arg_idx])
                arg_idx += 1
            else:
                macro_env.add_variable(param, lisptype.NIL)

        # If a parameter name is a destructuring pattern (a cons) rather than
        # a plain symbol, bind its elements against the value instead of
        # attempting to add the cons itself as a variable.
        def _bind_pattern(pat, val):
            # Bind a symbol directly
            if isinstance(pat, lisptype.LispSymbol):
                macro_env.add_variable(pat, val if val is not None else lisptype.NIL)
                return

            if not _consp_internal(pat):
                return

            # Collect pattern elements and detect dotted-tail
            pats = []
            cur = pat
            while _consp_internal(cur):
                pats.append(car(cur))
                cur = cdr(cur)

            dotted_tail = None
            if isinstance(cur, lisptype.LispSymbol):
                dotted_tail = cur

            # Walk value as list and bind elements
            cur_val = val
            for p in pats:
                if _consp_internal(cur_val):
                    v = car(cur_val)
                    if isinstance(p, lisptype.LispSymbol):
                        macro_env.add_variable(p, v)
                    cur_val = cdr(cur_val)
                else:
                    if isinstance(p, lisptype.LispSymbol):
                        macro_env.add_variable(p, lisptype.NIL)

            # Bind dotted tail to remaining list
            if dotted_tail is not None:
                macro_env.add_variable(dotted_tail, cur_val if cur_val is not None else lisptype.NIL)

        def _kw_parts(param):
            """Split a &key parameter name into (keyword-name, var-pattern).

            `param` is either a plain symbol (keyword name implied by the
            symbol) or a compound ((:keyword var-pattern)) spec, where
            var-pattern may itself be a destructuring pattern rather than a
            plain symbol.
            """
            if isinstance(param, lisptype.LispSymbol):
                return param.name.upper(), param
            if _consp_internal(param):
                kw = car(param)
                var_pattern = car(cdr(param)) if _consp_internal(cdr(param)) else None
                kw_name = kw.name.upper() if isinstance(kw, (lisptype.LispSymbol, lisptype.lispKeyword)) else None
                return kw_name, var_pattern
            return None, None

        # Bind optional parameters
        for param_spec in optional_params:
            # Support (name default supplied-p) optional syntax
            if isinstance(param_spec, lisptype.LispSymbol):
                opt_name = param_spec
                opt_default = None
                supplied_p = None
            elif _consp_internal(param_spec):
                opt_name = car(param_spec)
                rest = cdr(param_spec)
                opt_default = car(rest) if _consp_internal(rest) else None
                rest2 = cdr(rest) if _consp_internal(rest) else None
                supplied_p = car(rest2) if _consp_internal(rest2) else None
            else:
                # Unknown shape - skip defensively
                continue

            if arg_idx < len(call_args):
                val = call_args[arg_idx]
                if isinstance(opt_name, lisptype.LispSymbol):
                    macro_env.add_variable(opt_name, val)
                else:
                    _bind_pattern(opt_name, val)
                if supplied_p is not None:
                    macro_env.add_variable(supplied_p, lisptype.T)
                arg_idx += 1
            else:
                if opt_default is not None:
                    default_value = eval(opt_default, macro_env)
                else:
                    default_value = lisptype.NIL

                if isinstance(opt_name, lisptype.LispSymbol):
                    macro_env.add_variable(opt_name, default_value)
                else:
                    _bind_pattern(opt_name, default_value)

                if supplied_p is not None:
                    macro_env.add_variable(supplied_p, lisptype.NIL)

        # Bind &rest parameter
        if rest_param:
            remaining_args = call_args[arg_idx:]
            if remaining_args:
                rest_list = lisptype.NIL
                for arg in reversed(remaining_args):
                    rest_list = cons(arg, rest_list)
            else:
                rest_list = lisptype.NIL
            if isinstance(rest_param, lisptype.LispSymbol):
                macro_env.add_variable(rest_param, rest_list)
            elif _consp_internal(rest_param):
                head = car(rest_param)
                tail_sym = _extract_tail_symbol_from_rest(rest_param)
                if _consp_internal(rest_list):
                    first = car(rest_list)
                    rest_tail = cdr(rest_list)
                else:
                    first = lisptype.NIL
                    rest_tail = lisptype.NIL
                if isinstance(head, lisptype.LispSymbol):
                    macro_env.add_variable(head, first)
                if tail_sym is not None:
                    macro_env.add_variable(tail_sym, rest_tail)
        
        # Bind keyword parameters with defaults and supplied-p
        for param_spec in keyword_params:
            if _consp_internal(param_spec):
                param = car(param_spec)
                rest = cdr(param_spec)
                default_form = car(rest) if _consp_internal(rest) else None
                rest2 = cdr(rest) if _consp_internal(rest) else None
                supplied_p = car(rest2) if _consp_internal(rest2) else None
            else:
                param = param_spec
                default_form = None
                supplied_p = None

            _, var_pattern = _kw_parts(param)

            if default_form is not None:
                default_value = eval(default_form, macro_env)
            else:
                default_value = lisptype.NIL
            _bind_pattern(var_pattern, default_value)

            if supplied_p is not None:
                macro_env.add_variable(supplied_p, lisptype.NIL)
        
        # Process actual keyword arguments
        keyword_start = arg_idx
        i = keyword_start
        while i < len(call_args) - 1:
            key = call_args[i]
            value = call_args[i + 1]
            
            if isinstance(key, lisptype.lispKeyword):
                key_name = key.name.upper()
                for param_spec in keyword_params:
                    if _consp_internal(param_spec):
                        param = car(param_spec)
                        rest = cdr(param_spec)
                        rest2 = cdr(rest) if _consp_internal(rest) else None
                        supplied_p = car(rest2) if _consp_internal(rest2) else None
                    else:
                        param = param_spec
                        supplied_p = None

                    kw_name, var_pattern = _kw_parts(param)
                    if kw_name == key_name:
                        _bind_pattern(var_pattern, value)
                        if supplied_p is not None:
                            macro_env.add_variable(supplied_p, lisptype.T)
                        break
                i += 2
            else:
                i += 1

        # If no body, return NIL
        if not _consp_internal(actual_body):
            return lisptype.NIL

        # Evaluate the body inside an implicit BLOCK named for the macro.
        # This mirrors DEFUN/DEFMACRO semantics where the function/macro
        # body is implicitly a BLOCK so RETURN-FROM can target the name.
        block_form = lisptype.lispCons(lisptype.LispSymbol('BLOCK'), lisptype.lispCons(macro_name, actual_body))
        return eval(block_form, macro_env)

    # Mark as macro
    setattr(macro_callable, '__is_macro__', True)
    if isinstance(parsed_params, dict) and parsed_params.get('whole') is not None:
        setattr(macro_callable, '__expects_whole__', True)
    # Indicate that this macro function expects an expansion-time environment
    # if the lambda-list contained &ENVIRONMENT
    if environment_param is not None:
        setattr(macro_callable, '__expects_environment__', True)
    
    # Store docstring if present
    if docstring and isinstance(macro_name, lisptype.LispSymbol):
        if not hasattr(macro_name, 'plist'):
            macro_name.plist = {}
        macro_name.plist['DOCUMENTATION'] = docstring
    
    return macro_callable


def eval_defmacro(form, env):
    """Evaluate DEFMACRO special form: register a macro in the environment.

    This creates a Python callable that evaluates the macro body in an
    environment where the parameters are bound to the arguments. This allows
    QUASIQUOTE/UNQUOTE to work correctly in macro templates.
    """
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
    if _consp_internal(body):
        first_form = car(body)
        if isinstance(first_form, (str, lisptype.LispString)):
            docstring = str(first_form)  # Convert to Python str for storage
    
    # Create the macro function using the shared helper
    macro_callable = _create_macro_function(macro_name, lambda_list, body, env)
    
    # Find the global/root environment for defining the macro
    # DEFMACRO always creates global macro bindings (like DEFUN)
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    # Add macro to the GLOBAL environment (not local)
    global_env.add_function(macro_name, macro_callable)
    
    # Also add to the current environment for immediate visibility
    if env is not global_env:
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
        # Signal a PROGRAM-ERROR for wrong argument count per ANSI CL
        raise lisptype.LispProgramError("MACROEXPAND-1 requires 1 argument")
    
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
    try:
        macro_func = env.find_func(operator)
    except Exception:
        macro_func = None
        logger.error(f"[DEBUG] Error looking up macro function for {operator}", exc_info=True)

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
        expects_whole = getattr(macro_func, '__expects_whole__', False)
        expects_env = getattr(macro_func, '__expects_environment__', False)

        # Build call arguments based on macro function expectations
        call_args = []
        if expects_whole:
            call_args.append(form_to_expand)
        call_args.extend(args_list)

        # If macro expects expansion-time environment, append it as trailing arg
        if expects_env:
            call_args.append(env)

        return macro_func(*call_args)
    except Exception:
        # If macro expansion fails, return form unchanged
        return form_to_expand


def eval_macro_function(form, env):
    """Evaluate MACRO-FUNCTION special form.
    
    (MACRO-FUNCTION symbol) - return the macro function for a symbol, or NIL if not a macro.
    """
    from .evaluation_core import eval, ConditionException

    args = cdr(form)
    if not _consp_internal(args):
        cond = lisptype.ProgramError(message="MACRO-FUNCTION requires 1 argument")
        raise ConditionException(cond, recoverable=False)
    
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

    # Try to find the function in the environment first
    func = env.find_func(symbol)

    # If environment binding is a macro callable, return it immediately
    if callable(func) and getattr(func, '__is_macro__', False):
        return func

    # Otherwise fall back to the function registry
    entry = None
    try:
        from . import registry as _registry
        entry = _registry.function_registry.get(symbol.name)
    except Exception:
        entry = None

    if entry:
        # If the registry entry stores the actual Python callable, prefer it
        if getattr(entry, 'func', None) is not None:
            candidate = entry.func
            if callable(candidate) and getattr(candidate, '__is_macro__', False):
                return candidate
        # Otherwise try to resolve by py_name (modules may register py_name)
        py_name = entry.get('py_name') if hasattr(entry, 'get') else getattr(entry, 'py_name', None)
        if py_name:
            try:
                import importlib
                import fclpy.lispfunc as lispfunc_mod
                candidate = getattr(lispfunc_mod, py_name, None)
                if candidate is None:
                    # Try loading known submodules to find implementation
                    for sub in ('core', 'math', 'sequences', 'vectors', 'streams', 'pathnames', 'hashtables', 'evaluation', 'comparison', 'characters', 'io', 'io_read', 'io_write', 'utilities', 'classes', 'misc_macros'):
                        try:
                            mod = importlib.import_module(f'fclpy.lispfunc.{sub}')
                            candidate = getattr(mod, py_name, None)
                            if candidate:
                                try:
                                    setattr(lispfunc_mod, py_name, candidate)
                                except Exception:
                                    pass
                                break
                        except Exception:
                            continue
                if callable(candidate) and getattr(candidate, '__is_macro__', False):
                    return candidate
            except Exception:
                pass

    # As a last resort, look for a pure expansion function named
    # `_<lowercase-symbol>_expander` (returns a Lisp form, not a value).
    # We must NOT wrap the `eval_<name>` handler because those handlers
    # evaluate the expansion and return a value; the macroexpander would
    # then evaluate the value a second time causing double-evaluation.
    try:
        import fclpy.lispfunc.evaluation_special_forms as _self_mod
        expander_name = f"_{symbol.name.lower()}_expander"
        expander_fn = getattr(_self_mod, expander_name, None)
        if callable(expander_fn) and getattr(expander_fn, '__is_macro__', False):
            return expander_fn
    except Exception:
        pass

    return lisptype.NIL


def eval_destructuring_bind(form, env):
    """Evaluate DESTRUCTURING-BIND special form.

    Syntax: (DESTRUCTURING-BIND lambda-list expression &body body)
    This binds variables according to the lambda-list by destructuring the
    evaluated expression, then evaluates the body forms with those bindings.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("DESTRUCTURING-BIND requires a pattern, an expression and body")

    pattern = car(args)
    expr_form = car(cdr(args))
    body = cdr(cdr(args))

    # Evaluate the expression to destructure
    expr_val = eval(expr_form, env)

    # Create a new environment for the bindings
    bind_env = lisptype.Environment(parent=env)

    def _bind(pat, val):
        # Bind a symbol directly
        if isinstance(pat, lisptype.LispSymbol):
            bind_env.add_variable(pat, val if val is not None else lisptype.NIL)
            return

        # If pattern is not a cons, nothing to bind
        if not _consp_internal(pat):
            return

        # Collect pattern elements and detect dotted-tail
        pats = []
        cur = pat
        while _consp_internal(cur):
            pats.append(car(cur))
            cur = cdr(cur)

        dotted_tail = None
        if isinstance(cur, lisptype.LispSymbol):
            dotted_tail = cur

        # Walk value as list and bind elements
        cur_val = val
        for i, p in enumerate(pats):
            if _consp_internal(cur_val):
                v = car(cur_val)
                _bind(p, v)
                cur_val = cdr(cur_val)
            else:
                _bind(p, lisptype.NIL)

        # Bind dotted tail to remaining list
        if dotted_tail is not None:
            bind_env.add_variable(dotted_tail, cur_val if cur_val is not None else lisptype.NIL)

    _bind(pattern, expr_val)

    # Evaluate body forms in bind_env
    result = lisptype.NIL
    cur = body
    while _consp_internal(cur):
        result = eval(car(cur), bind_env)
        cur = cdr(cur)

    return result


def eval_lambda(form, env):
    """Evaluate LAMBDA special form."""
    from .evaluation_core import eval, parse_lambda_list

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("LAMBDA requires at least 1 argument")

    param_list = car(args)
    body = cdr(args)

    # Parse the lambda list to support &optional, &rest, &key, &aux, &environment
    parsed = parse_lambda_list(param_list)
    required_params = parsed.get('required', [])
    optional_params = parsed.get('optional', [])
    rest_param = parsed.get('rest', None)
    keyword_params = parsed.get('keyword', [])
    aux_params = parsed.get('aux', [])
    environment_param = parsed.get('environment', None)

    # Create function closure
    def lambda_function(*call_args):
        # Create new environment for function execution
        func_env = lisptype.Environment(env)

        # Normalize NIL symbol arguments to canonical NIL
        new_args = []
        for a in call_args:
            if isinstance(a, lisptype.LispSymbol) and a.name.upper() == 'NIL':
                new_args.append(lisptype.NIL)
            else:
                new_args.append(a)
        call_args = tuple(new_args)

        arg_index = 0

        # Bind required parameters
        for param in required_params:
            if arg_index < len(call_args):
                func_env.add_variable(param, call_args[arg_index])
                arg_index += 1
            else:
                func_env.add_variable(param, lisptype.NIL)

        # Bind optional parameters (support supplied-p variable)
        for param_spec in optional_params:
            if _consp_internal(param_spec):
                param = car(param_spec)
                rest = cdr(param_spec)
                default_form = car(rest) if _consp_internal(rest) else None
                rest2 = cdr(rest) if _consp_internal(rest) else None
                supplied_p = car(rest2) if _consp_internal(rest2) else None
            else:
                param = param_spec
                default_form = None
                supplied_p = None

            if arg_index < len(call_args):
                func_env.add_variable(param, call_args[arg_index])
                if supplied_p is not None:
                    func_env.add_variable(supplied_p, lisptype.T)
                arg_index += 1
            else:
                if default_form is not None:
                    default_value = eval(default_form, func_env)
                    func_env.add_variable(param, default_value)
                else:
                    func_env.add_variable(param, lisptype.NIL)
                if supplied_p is not None:
                    func_env.add_variable(supplied_p, lisptype.NIL)

        # Collect remaining positional arguments for &rest
        remaining_positional = []

        # Find where keyword arguments start
        keyword_start = arg_index
        for i in range(arg_index, len(call_args)):
            if isinstance(call_args[i], lisptype.lispKeyword):
                keyword_start = i
                break
            remaining_positional.append(call_args[i])
            arg_index = i + 1

        # Bind &rest parameter if present
        if rest_param:
            if remaining_positional:
                rest_list = lisptype.NIL
                for item in reversed(remaining_positional):
                    rest_list = lisptype.lispCons(item, rest_list)
            else:
                rest_list = lisptype.NIL

            if isinstance(rest_param, lisptype.LispSymbol):
                func_env.add_variable(rest_param, rest_list)
            elif _consp_internal(rest_param):
                head = car(rest_param)
                tail_sym = _extract_tail_symbol_from_rest(rest_param)
                if _consp_internal(rest_list):
                    first = car(rest_list)
                    rest_tail = cdr(rest_list)
                else:
                    first = lisptype.NIL
                    rest_tail = lisptype.NIL
                if isinstance(head, lisptype.LispSymbol):
                    func_env.add_variable(head, first)
                if tail_sym is not None:
                    func_env.add_variable(tail_sym, rest_tail)

        # Bind keyword parameters: initialize to defaults and supplied-p to NIL
        for param_spec in keyword_params:
            if _consp_internal(param_spec):
                param = car(param_spec)
                rest = cdr(param_spec)
                default_form = car(rest) if _consp_internal(rest) else None
                rest2 = cdr(rest) if _consp_internal(rest) else None
                supplied_p = car(rest2) if _consp_internal(rest2) else None
            else:
                param = param_spec
                default_form = None
                supplied_p = None

            if default_form is not None:
                default_value = eval(default_form, func_env)
                func_env.add_variable(param, default_value)
            else:
                func_env.add_variable(param, lisptype.NIL)

            if supplied_p is not None:
                func_env.add_variable(supplied_p, lisptype.NIL)

        # Now process actual keyword arguments from the call
        i = keyword_start
        while i < len(call_args) - 1:
            key = call_args[i]
            value = call_args[i + 1]

            if isinstance(key, lisptype.lispKeyword):
                key_name = key.name.upper()
                # Find matching parameter
                for param_spec in keyword_params:
                    if _consp_internal(param_spec):
                        param = car(param_spec)
                        rest = cdr(param_spec)
                        rest2 = cdr(rest) if _consp_internal(rest) else None
                        supplied_p = car(rest2) if _consp_internal(rest2) else None
                    else:
                        param = param_spec
                        supplied_p = None

                    if isinstance(param, lisptype.LispSymbol) and param.name.upper() == key_name:
                        func_env.add_variable(param, value)
                        if supplied_p is not None:
                            func_env.add_variable(supplied_p, lisptype.T)
                        break
                i += 2
            else:
                i += 1

        # Bind &aux parameters
        for param_spec in aux_params:
            if _consp_internal(param_spec):
                param = car(param_spec)
                init_form = car(cdr(param_spec))
                init_value = eval(init_form, func_env)
                func_env.add_variable(param, init_value)
            else:
                func_env.add_variable(param_spec, lisptype.NIL)

        # Bind &environment if requested
        if environment_param is not None:
            func_env.add_variable(environment_param, env)

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


def eval_proclaim(form, env):
    """Evaluate PROCLAIM special form.

    PROCLAIM takes declaration specifiers which may need evaluation
    (for example a backquoted form with unquote). Evaluate each spec
    in the current environment and apply it globally (similar to
    DECLAIM but evaluating the spec forms first).
    """
    from .evaluation_core import eval as lisp_eval

    args = cdr(form)

    # PROCLAIM must have at least one specifier
    if args is None or args is lisptype.NIL:
        raise lisptype.LispProgramError("PROCLAIM requires at least one declaration specifier")

    # Get the global/root environment to store effects
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent

    while _consp_internal(args):
        spec_expr = car(args)
        # Evaluate the spec expression so backquote/unquote is handled
        spec = lisp_eval(spec_expr, env)

        if _consp_internal(spec):
            spec_type = car(spec)
            if isinstance(spec_type, lisptype.LispSymbol):
                spec_name = spec_type.name.upper()
                if spec_name == 'OPTIMIZE':
                    _store_optimization_declaration(global_env, spec)
                elif spec_name == 'SPECIAL':
                    _store_special_declaration(global_env, spec)
                else:
                    if not hasattr(global_env, '_global_declarations'):
                        global_env._global_declarations = {}
                    if spec_name not in global_env._global_declarations:
                        global_env._global_declarations[spec_name] = []
                    global_env._global_declarations[spec_name].append(spec)

        args = cdr(args)

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
    
    DEFVAR defines variables in the GLOBAL environment, not the local one.
    This is standard Common Lisp behavior.
    
    Returns the symbol name.
    """
    from .evaluation_core import eval as lisp_eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFVAR requires at least a variable name")
    
    name = car(args)
    if not isinstance(name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFVAR: first argument must be a symbol")
    
    # Find the global/root environment for defining the variable
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    # Check if variable is already bound (in any environment)
    current_value = env.find_variable(name)
    is_already_bound = current_value is not None
    
    # Get the value form if present
    rest_args = cdr(args)
    has_value_form = _consp_internal(rest_args)
    
    if has_value_form and not is_already_bound:
        # Has initial value and not already bound - evaluate and bind globally
        value_form = car(rest_args)
        value = lisp_eval(value_form, env)
        global_env.add_variable(name, value)
    elif not is_already_bound:
        # No value form and not bound - bind to NIL globally
        global_env.add_variable(name, lisptype.NIL)
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
    
    # Mark as special variable in global environment
    if not hasattr(global_env, '_special_variables'):
        global_env._special_variables = {}
    global_env._special_variables[name.name] = True
    
    return name


def eval_defparameter(form, env):
    """Evaluate DEFPARAMETER special form.
    
    (DEFPARAMETER name value)     - declares and always sets value
    (DEFPARAMETER name value doc) - with documentation string
    
    Unlike DEFVAR, DEFPARAMETER always sets the value, even if already bound.
    
    DEFPARAMETER defines variables in the GLOBAL environment, not the local one.
    This is standard Common Lisp behavior.
    
    Returns the symbol name.
    """
    from .evaluation_core import eval as lisp_eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFPARAMETER requires a variable name")
    
    name = car(args)
    if not isinstance(name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFPARAMETER: first argument must be a symbol")
    
    # Find the global/root environment for defining the variable
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    # Get the value form (required for DEFPARAMETER)
    rest_args = cdr(args)
    if not _consp_internal(rest_args):
        raise lisptype.LispNotImplementedError("DEFPARAMETER requires an initial value")
    
    value_form = car(rest_args)
    value = lisp_eval(value_form, env)
    global_env.add_variable(name, value)
    
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
    
    # Mark as special variable in global environment
    if not hasattr(global_env, '_special_variables'):
        global_env._special_variables = {}
    global_env._special_variables[name.name] = True
    
    return name


def eval_defconstant(form, env):
    """Evaluate DEFCONSTANT special form.
    
    (DEFCONSTANT name value)     - declares and always sets constant value
    (DEFCONSTANT name value doc) - with documentation string
    
    DEFCONSTANT always sets the value and marks it as a constant.
    Constants cannot be rebound (though we don't enforce this strictly).
    
    DEFCONSTANT defines variables in the GLOBAL environment.
    
    Returns the symbol name.
    """
    from .evaluation_core import eval as lisp_eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFCONSTANT requires a variable name")
    
    name = car(args)
    if not isinstance(name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFCONSTANT: first argument must be a symbol")
    
    # Find the global/root environment for defining the variable
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    # Get the value form (required for DEFCONSTANT)
    rest_args = cdr(args)
    if not _consp_internal(rest_args):
        raise lisptype.LispNotImplementedError("DEFCONSTANT requires an initial value")
    
    value_form = car(rest_args)
    value = lisp_eval(value_form, env)
    global_env.add_variable(name, value)
    
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
    
    # Mark as constant in global environment
    if not hasattr(global_env, '_constants'):
        global_env._constants = {}
    global_env._constants[name.name] = True
    
    return name


def eval_defstruct(form, env):
    """Evaluate DEFSTRUCT special form.
    
    (DEFSTRUCT name slot...)
    (DEFSTRUCT (name option...) slot...)
    
    DEFSTRUCT does not evaluate its arguments - they are literal specifications.
    DEFSTRUCT creates GLOBAL function bindings like DEFUN does.
    """
    import fclpy.state as state
    
    # Get current package for interning accessor symbols
    current_pkg = getattr(state, 'current_package', None) or lisptype.COMMON_LISP_USER_PACKAGE
    
    # Find the global/root environment for defining functions
    # DEFSTRUCT always creates global function bindings (like DEFUN)
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFSTRUCT requires a name")
    
    name_and_options = car(args)
    slot_specs = cdr(args)
    
    # Parse name and options
    if isinstance(name_and_options, lisptype.lispKeyword):
        # Accept keywords as structure names and use their name
        struct_name = name_and_options
        conc_name = struct_name.name + '-'
        constructor_name = 'MAKE-' + struct_name.name
        copier_name = 'COPY-' + struct_name.name
        predicate_name = struct_name.name + '-P'
        include_parent = None
    elif isinstance(name_and_options, lisptype.LispSymbol):
        struct_name = name_and_options
        conc_name = struct_name.name + '-'
        constructor_name = 'MAKE-' + struct_name.name
        copier_name = 'COPY-' + struct_name.name
        predicate_name = struct_name.name + '-P'
        include_parent = None
    elif _consp_internal(name_and_options):
        struct_name = car(name_and_options)
        # Validate that struct_name is a symbol or keyword
        if isinstance(struct_name, lisptype.lispKeyword):
            # Accept keywords and use their name
            pass
        elif not isinstance(struct_name, lisptype.LispSymbol):
            raise lisptype.LispNotImplementedError(f"DEFSTRUCT: structure name must be a symbol, got {type(struct_name)}")
        conc_name = struct_name.name + '-'  # Default prefix
        constructor_name = 'MAKE-' + struct_name.name
        copier_name = 'COPY-' + struct_name.name
        predicate_name = struct_name.name + '-P'
        include_parent = None
        
        # Parse options
        options = cdr(name_and_options)
        while _consp_internal(options):
            opt = car(options)
            if _consp_internal(opt):
                opt_name = car(opt)
                opt_value = car(cdr(opt)) if _consp_internal(cdr(opt)) else None
                
                if isinstance(opt_name, lisptype.LispSymbol):
                    opt_name_str = opt_name.name.upper()
                elif isinstance(opt_name, lisptype.lispKeyword):
                    opt_name_str = opt_name.name.upper()
                else:
                    opt_name_str = str(opt_name).upper()
                
                if opt_name_str == 'CONC-NAME' or opt_name_str == ':CONC-NAME':
                    # Check for NIL value (can be None, the NIL constant, or a symbol named "NIL")
                    is_nil = (opt_value is None or 
                              opt_value == lisptype.NIL or
                              (isinstance(opt_value, lisptype.LispSymbol) and opt_value.name == 'NIL'))
                    if is_nil:
                        conc_name = ''  # No prefix
                    elif isinstance(opt_value, lisptype.LispSymbol):
                        conc_name = opt_value.name
                    else:
                        conc_name = str(opt_value)
                elif opt_name_str == 'CONSTRUCTOR' or opt_name_str == ':CONSTRUCTOR':
                    is_nil = (opt_value is None or 
                              opt_value == lisptype.NIL or
                              (isinstance(opt_value, lisptype.LispSymbol) and opt_value.name == 'NIL'))
                    if is_nil:
                        constructor_name = None
                    elif isinstance(opt_value, lisptype.LispSymbol):
                        constructor_name = opt_value.name
                elif opt_name_str == 'COPIER' or opt_name_str == ':COPIER':
                    is_nil = (opt_value is None or 
                              opt_value == lisptype.NIL or
                              (isinstance(opt_value, lisptype.LispSymbol) and opt_value.name == 'NIL'))
                    if is_nil:
                        copier_name = None
                    elif isinstance(opt_value, lisptype.LispSymbol):
                        copier_name = opt_value.name
                elif opt_name_str == 'PREDICATE' or opt_name_str == ':PREDICATE':
                    is_nil = (opt_value is None or 
                              opt_value == lisptype.NIL or
                              (isinstance(opt_value, lisptype.LispSymbol) and opt_value.name == 'NIL'))
                    if is_nil:
                        predicate_name = None
                    elif isinstance(opt_value, lisptype.LispSymbol):
                        predicate_name = opt_value.name
                elif opt_name_str == 'INCLUDE' or opt_name_str == ':INCLUDE':
                    if isinstance(opt_value, lisptype.LispSymbol):
                        include_parent = opt_value.name
            options = cdr(options)
    else:
        struct_name = name_and_options
        conc_name = str(struct_name) + '-'
        constructor_name = 'MAKE-' + str(struct_name)
        copier_name = 'COPY-' + str(struct_name)
        predicate_name = str(struct_name) + '-P'
        include_parent = None
    
    struct_class_name = struct_name.name if isinstance(struct_name, lisptype.LispSymbol) else str(struct_name)
    
    # Parse slot definitions
    slot_defs = []  # List of (slot_name, default_value)
    while _consp_internal(slot_specs):
        slot = car(slot_specs)
        if isinstance(slot, lisptype.LispSymbol):
            slot_defs.append((slot.name, lisptype.NIL))
        elif _consp_internal(slot):
            slot_name = car(slot)
            if isinstance(slot_name, lisptype.LispSymbol):
                slot_name_str = slot_name.name
            else:
                slot_name_str = str(slot_name)
            default_value = car(cdr(slot)) if _consp_internal(cdr(slot)) else lisptype.NIL
            slot_defs.append((slot_name_str, default_value))
        else:
            slot_defs.append((str(slot), lisptype.NIL))
        slot_specs = cdr(slot_specs)
    
    # Create the structure class
    class StructureInstance:
        def __init__(self, struct_type=struct_class_name, slot_defaults=None, **kwargs):
            self._struct_type = struct_type
            self._slots = {}
            # Initialize with defaults
            if slot_defaults is None:
                slot_defaults = slot_defs
            for slot_name, default_val in slot_defaults:
                self._slots[slot_name] = default_val
            # Override with provided values
            for key, value in kwargs.items():
                key_upper = key.upper()
                for slot_name, _ in slot_defaults:
                    if slot_name.upper() == key_upper:
                        self._slots[slot_name] = value
                        break
        
        def __repr__(self):
            slot_values = ' '.join(f':{k} {v}' for k, v in self._slots.items())
            return f'#S({self._struct_type} {slot_values})'
        
        def get_slot(self, name):
            return self._slots.get(name, lisptype.NIL)
        
        def set_slot(self, name, value):
            self._slots[name] = value
    
    # Store the structure class in a registry
    if not hasattr(state, '_structure_classes'):
        state._structure_classes = {}
    state._structure_classes[struct_class_name] = {
        'class': StructureInstance,
        'slots': slot_defs,
        'conc_name': conc_name
    }
    
    # Create constructor function
    if constructor_name:
        def constructor_wrapper(*args, **kwargs):
            # Convert keyword symbol arguments to kwargs
            result_kwargs = dict(kwargs)
            i = 0
            while i < len(args):
                if i + 1 < len(args):
                    key = args[i]
                    value = args[i + 1]
                    if isinstance(key, lisptype.lispKeyword):
                        result_kwargs[key.name.upper()] = value
                        i += 2
                    else:
                        i += 1
                else:
                    i += 1
            return StructureInstance(struct_class_name, slot_defs, **result_kwargs)
        
        constructor_sym = current_pkg.intern_symbol(constructor_name)
        global_env.add_function(constructor_sym, constructor_wrapper)
    
    # Create copier function
    if copier_name:
        def copy_structure(struct):
            if not isinstance(struct, StructureInstance):
                raise TypeError(f"Not a {struct_class_name}: {struct}")
            new_struct = StructureInstance(struct_class_name, slot_defs)
            new_struct._slots = dict(struct._slots)
            return new_struct
        
        copier_sym = current_pkg.intern_symbol(copier_name)
        global_env.add_function(copier_sym, copy_structure)
    
    # Create predicate function
    if predicate_name:
        def is_structure(obj):
            if hasattr(obj, '_struct_type') and obj._struct_type == struct_class_name:
                return lisptype.T
            return lisptype.NIL
        
        predicate_sym = current_pkg.intern_symbol(predicate_name)
        global_env.add_function(predicate_sym, is_structure)
    
    # Create accessor functions for each slot
    for slot_name, _ in slot_defs:
        accessor_name = conc_name + slot_name
        
        # Create getter
        def make_getter(sn):
            def getter(struct):
                if hasattr(struct, 'get_slot'):
                    return struct.get_slot(sn)
                raise TypeError(f"Not a {struct_class_name}: {struct}")
            return getter
        
        accessor_sym = current_pkg.intern_symbol(accessor_name)
        global_env.add_function(accessor_sym, make_getter(slot_name))
        
        # Create setter (for SETF)
        def make_setter(sn):
            def setter(struct, value):
                if hasattr(struct, 'set_slot'):
                    struct.set_slot(sn, value)
                    return value
                raise TypeError(f"Not a {struct_class_name}: {struct}")
            return setter
        
        setter_name = 'SET-' + accessor_name
        setter_sym = current_pkg.intern_symbol(setter_name)
        global_env.add_function(setter_sym, make_setter(slot_name))
    
    return struct_name


def _pop_expander(place, *_rest):
    """POP macro expander: return an expansion form for (POP place).

    This is the callable returned by (MACRO-FUNCTION 'POP).  It is flagged
    with __is_macro__ so the macro-expander recognises it.

    For a simple symbol place we expand into:
        (LET ((#:tmp (CAR place)))
          (SETQ place (CDR place))
          #:tmp)
    """
    from . import utilities_symbols as _utils

    if isinstance(place, lisptype.LispSymbol):
        tmp = _utils.gensym()
        car_call  = lisptype.lispCons(lisptype.LispSymbol('CAR'),
                        lisptype.lispCons(place, lisptype.NIL))
        binding   = lisptype.lispCons(tmp,
                        lisptype.lispCons(car_call, lisptype.NIL))
        bindings  = lisptype.lispCons(binding, lisptype.NIL)
        cdr_call  = lisptype.lispCons(lisptype.LispSymbol('CDR'),
                        lisptype.lispCons(place, lisptype.NIL))
        setq_call = lisptype.lispCons(lisptype.LispSymbol('SETQ'),
                        lisptype.lispCons(place,
                            lisptype.lispCons(cdr_call, lisptype.NIL)))
        body      = lisptype.lispCons(setq_call,
                        lisptype.lispCons(tmp, lisptype.NIL))
        return lisptype.lispCons(lisptype.LispSymbol('LET'),
                   lisptype.lispCons(bindings, body))

    raise lisptype.LispNotImplementedError(f"POP: unsupported place form: {place}")

# Mark as a proper macro callable and register it so (MACRO-FUNCTION 'POP) returns it.
_pop_expander.__is_macro__ = True
_registry.function_registry['POP'] = _registry.RegistryEntry(
    name='POP',
    py_name='_pop_expander',
    kind='macro',
    func=_pop_expander,
)


def eval_pop(form, env):
    """Evaluate POP special form (macro).

    (POP place) — Remove and return the first element from the list stored
    in place.  This handler is called directly by the evaluator fast-path;
    it builds the macro expansion and immediately evaluates it so that it
    returns a *value*, not a form.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("POP requires a place argument")

    place = car(args)

    if isinstance(place, lisptype.LispSymbol):
        let_form = _pop_expander(place)
        return eval(let_form, env)

    raise lisptype.LispNotImplementedError(f"POP not implemented for place: {place}")


def _push_expander(item, place, *_rest):
    """PUSH macro expander: return an expansion form for (PUSH item place).

    This is the callable returned by (MACRO-FUNCTION 'PUSH). It is flagged
    with __is_macro__ so the macro-expander recognises it.

    For a simple symbol place we expand into:
        (SETQ place (CONS item place))
    """
    if isinstance(place, lisptype.LispSymbol):
        cons_call = lisptype.lispCons(lisptype.LispSymbol('CONS'),
                        lisptype.lispCons(item,
                            lisptype.lispCons(place, lisptype.NIL)))
        return lisptype.lispCons(lisptype.LispSymbol('SETQ'),
                   lisptype.lispCons(place,
                       lisptype.lispCons(cons_call, lisptype.NIL)))

    raise lisptype.LispNotImplementedError(f"PUSH: unsupported place form: {place}")

# Mark as a proper macro callable and register it so (MACRO-FUNCTION 'PUSH) returns it.
_push_expander.__is_macro__ = True
_registry.function_registry['PUSH'] = _registry.RegistryEntry(
    name='PUSH',
    py_name='_push_expander',
    kind='macro',
    func=_push_expander,
)


def eval_push(form, env):
    """Evaluate PUSH special form (macro).

    (PUSH item place) — Prepend item to the list stored in place, and store
    the result back in place. This handler is called directly by the
    evaluator fast-path; it builds the macro expansion and immediately
    evaluates it so that it returns a *value*, not a form.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("PUSH requires an item and a place argument")

    item = car(args)
    place = car(cdr(args))

    if isinstance(place, lisptype.LispSymbol):
        setq_form = _push_expander(item, place)
        return eval(setq_form, env)

    raise lisptype.LispNotImplementedError(f"PUSH not implemented for place: {place}")


def _place_accessor(place_form, env):
    """Evaluate a place form's shared subforms exactly once and return a
    (get, set) pair of closures for reading/writing it.

    Supports plain variables and (CAR x), (CDR x), (AREF arr idx),
    (SVREF arr idx) place forms -- enough for ROTATEF/SHIFTF's common
    cases. Other place kinds raise LispNotImplementedError.
    """
    from .evaluation_core import eval

    if isinstance(place_form, lisptype.LispSymbol):
        sym = place_form
        return (lambda: eval(sym, env), lambda v: env.set_variable(sym, v))

    if _consp_internal(place_form) and isinstance(car(place_form), lisptype.LispSymbol):
        op_name = car(place_form).name
        place_args = cdr(place_form)

        if op_name in ('CAR', 'FIRST') and _consp_internal(place_args):
            target = eval(car(place_args), env)
            if not _consp_internal(target):
                raise lisptype.LispError(f"{op_name} place: target is not a cons")
            return (lambda: target.car, lambda v: setattr(target, 'car', v))

        if op_name in ('CDR', 'REST') and _consp_internal(place_args):
            target = eval(car(place_args), env)
            if not _consp_internal(target):
                raise lisptype.LispError(f"{op_name} place: target is not a cons")
            return (lambda: target.cdr, lambda v: setattr(target, 'cdr', v))

        if op_name == 'CADR' and _consp_internal(place_args):
            target = eval(car(place_args), env)
            if not _consp_internal(target) or not _consp_internal(target.cdr):
                raise lisptype.LispError("CADR place: invalid structure")
            cell = target.cdr
            return (lambda: cell.car, lambda v: setattr(cell, 'car', v))

        if op_name in ('AREF', 'SVREF') and _consp_internal(place_args) and _consp_internal(cdr(place_args)):
            arr = eval(car(place_args), env)
            idx = eval(car(cdr(place_args)), env)
            return (lambda: arr[idx], lambda v: arr.__setitem__(idx, v))

    raise lisptype.LispNotImplementedError(f"place not supported: {place_form}")


def eval_rotatef(form, env):
    """Evaluate ROTATEF special form.

    (ROTATEF place*) — Evaluates each place's shared subforms exactly
    once (left to right), then rotates their values: place[i] gets the
    old value of place[i+1], with the last place getting the first
    place's old value. Always returns NIL.
    """
    args = cdr(form)

    places = []
    current = args
    while _consp_internal(current):
        places.append(car(current))
        current = cdr(current)

    if not places:
        return lisptype.NIL

    accessors = [_place_accessor(p, env) for p in places]
    old_values = [get() for get, _ in accessors]

    n = len(accessors)
    for i in range(n):
        accessors[i][1](old_values[(i + 1) % n])

    return lisptype.NIL


def eval_defclass(form, env):
    """Evaluate DEFCLASS special form.
    
    DEFCLASS defines a new class (CLOS). The name should not be evaluated,
    but superclasses should be looked up as class objects.
    
    Syntax:
        (defclass name (superclass*) (slot-spec*) option*)
    
    Slot specs:
        (slot-name [:initarg keyword] [:initform form] [:reader name] [:writer name])
    
    Options:
        (:metaclass class)
        (:documentation string)
    """
    from .evaluation_core import eval
    import fclpy.classes
    
    # Import the defclass function directly
    from fclpy.lispfunc.classes import defclass
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFCLASS requires at least a name and superclass list")
    
    # Get class name (NOT evaluated - it's a symbol to define)
    class_name = car(args)
    if not isinstance(class_name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFCLASS: class name must be a symbol")
    
    # Get superclasses list
    rest = cdr(args)
    if not _consp_internal(rest):
        raise lisptype.LispNotImplementedError("DEFCLASS requires a superclass list")
    
    superclasses_form = car(rest)
    slots_rest = cdr(rest)
    
    # Parse superclasses: convert symbols to class objects
    superclasses_list = []
    if superclasses_form is not None and superclasses_form != lisptype.NIL:
        current = superclasses_form
        while _consp_internal(current):
            sc = car(current)
            if isinstance(sc, lisptype.LispSymbol):
                # Try to look up the class
                sc_class = fclpy.classes.find_class(sc.name)
                if sc_class is not None:
                    superclasses_list.append(sc_class)
                else:
                    # If not found as a class, treat as forward reference
                    superclasses_list.append(sc)
            else:
                superclasses_list.append(sc)
            current = cdr(current)
    
    # Get slot definitions
    slots_form = None
    if _consp_internal(slots_rest):
        slots_form = car(slots_rest)
    
    options_rest = cdr(slots_rest) if _consp_internal(slots_rest) else lisptype.NIL
    
    # Parse slot definitions - convert to format expected by defclass function
    slots_list = []
    if slots_form is not None and slots_form != lisptype.NIL:
        current = slots_form
        while _consp_internal(current):
            slot_spec = car(current)
            # Slot spec can be just a symbol or a list
            if isinstance(slot_spec, lisptype.LispSymbol):
                # Simple slot: just a symbol
                slots_list.append(slot_spec)
            elif _consp_internal(slot_spec):
                # Complex slot: (name :key1 val1 :key2 val2 ...)
                # Convert to list format: [name_symbol, :key1, val1, :key2, val2, ...]
                slot_list = []
                slot_spec_current = slot_spec
                while _consp_internal(slot_spec_current):
                    element = car(slot_spec_current)
                    slot_list.append(element)
                    slot_spec_current = cdr(slot_spec_current)
                slots_list.append(slot_list)
            current = cdr(current)
    
    # Parse options
    metaclass = None
    documentation = None
    if _consp_internal(options_rest):
        current = options_rest
        while _consp_internal(current):
            option = car(current)
            if _consp_internal(option):
                opt_key = car(option)
                if isinstance(opt_key, lisptype.lispKeyword):
                    opt_name = opt_key.name.lower()
                    opt_vals = cdr(option)
                    if opt_name == 'metaclass' and _consp_internal(opt_vals):
                        metaclass = car(opt_vals)
                    elif opt_name == 'documentation' and _consp_internal(opt_vals):
                        documentation = car(opt_vals)
            current = cdr(current)
    
    # Call the defclass function to create the class
    result = defclass(
        class_name,
        direct_superclasses=superclasses_list,
        slots=slots_list,
        metaclass=metaclass,
        documentation=documentation
    )
    
    return class_name


def eval_defgeneric(form, env):
    """Evaluate DEFGENERIC special form.
    
    DEFGENERIC defines a generic function - a function that can dispatch
    on the types of its arguments. In FCLpy, we implement a simplified
    version that:
    
    1. Creates a generic function object that stores methods
    2. Supports :method options for inline method definitions
    3. The generic function dispatches based on argument types
    
    Syntax:
        (defgeneric name lambda-list [[option | method-description]]*)
    
    Supported options:
        (:method qualifiers* specialized-lambda-list body)
        (:documentation string)
    """
    from .evaluation_core import eval, parse_lambda_list
    import fclpy.state as state
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFGENERIC requires at least a name and lambda-list")
    
    func_name = car(args)
    rest = cdr(args)
    
    if not isinstance(func_name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFGENERIC: function name must be a symbol")
    
    # Get lambda-list
    if not _consp_internal(rest):
        raise lisptype.LispNotImplementedError("DEFGENERIC requires a lambda-list")
    
    lambda_list = car(rest)
    options = cdr(rest)
    
    # Parse the lambda list to get parameter names
    parsed = parse_lambda_list(lambda_list)
    required_params = parsed['required']
    
    # Collect methods and documentation
    methods = []  # List of (specializers, function) tuples
    documentation = None
    
    # Process options
    current = options
    while _consp_internal(current):
        option = car(current)
        if _consp_internal(option):
            opt_name = car(option)
            if isinstance(opt_name, lisptype.lispKeyword) and opt_name.name == 'METHOD':
                # Parse (:method specialized-lambda-list body...)
                method_rest = cdr(option)
                if _consp_internal(method_rest):
                    specialized_lambda_list = car(method_rest)
                    method_body = cdr(method_rest)
                    
                    # Extract specializers from specialized-lambda-list
                    # e.g., ((x integer) (y integer) (z integer)) -> [integer, integer, integer]
                    specializers = []
                    params_for_method = []
                    spec_current = specialized_lambda_list
                    while _consp_internal(spec_current):
                        param_spec = car(spec_current)
                        
                        # Skip lambda-list keywords like &KEY, &OPTIONAL, &REST, etc.
                        if isinstance(param_spec, lisptype.LispSymbol):
                            spec_keyword = param_spec.name.upper()
                            if spec_keyword.startswith('&'):
                                # Skip this keyword and consume the rest (which are key/value pairs or args)
                                # For &KEY, we need to skip key-var pairs
                                # For &OPTIONAL, &REST, we just move to the next element
                                spec_current = cdr(spec_current)
                                
                                # If it's &KEY, skip the key-value pairs
                                if spec_keyword == '&KEY':
                                    while _consp_internal(spec_current):
                                        next_item = car(spec_current)
                                        # If it's a symbol starting with &, we hit another keyword, stop
                                        if isinstance(next_item, lisptype.LispSymbol) and next_item.name.upper().startswith('&'):
                                            break
                                        # Otherwise skip the key variable
                                        spec_current = cdr(spec_current)
                                continue
                        
                        if _consp_internal(param_spec):
                            # Specialized: (param-name type)
                            param_name = car(param_spec)
                            param_type = car(cdr(param_spec))
                            
                            # Try to resolve the type to a class
                            # If param_type is a symbol, try to look it up as a class
                            if isinstance(param_type, lisptype.LispSymbol):
                                class_obj = None
                                param_type_name = param_type.name.upper()
                                
                                # Handle T specially - it means unspecialized
                                if param_type_name == 'T':
                                    specializers.append(None)
                                    params_for_method.append(param_name)
                                    spec_current = cdr(spec_current)
                                    continue
                                
                                # Try to find the class in the registry
                                try:
                                    import fclpy.classes
                                    class_obj = fclpy.classes.find_class(param_type.name)
                                except Exception:
                                    pass
                                
                                if class_obj:
                                    specializers.append(class_obj)
                                else:
                                    # Class not found - use None (unspecialized) as fallback
                                    # This prevents errors when the class isn't in the registry yet
                                    specializers.append(None)
                            else:
                                specializers.append(param_type)
                            
                            params_for_method.append(param_name)
                        else:
                            # Unspecialized: just param-name (matches any type)
                            specializers.append(None)
                            params_for_method.append(param_spec)
                        spec_current = cdr(spec_current)
                    
                    # Create method function
                    def make_method_function(params, body, captured_env):
                        def method_func(*call_args):
                            method_env = lisptype.Environment(captured_env)
                            for i, param in enumerate(params):
                                if i < len(call_args):
                                    method_env.add_variable(param, call_args[i])
                                else:
                                    method_env.add_variable(param, lisptype.NIL)
                            
                            result = lisptype.NIL
                            body_current = body
                            while _consp_internal(body_current):
                                result = eval(car(body_current), method_env)
                                body_current = cdr(body_current)
                            return result
                        return method_func
                    
                    method_fn = make_method_function(params_for_method, method_body, env)
                    methods.append((specializers, method_fn))
            
            elif isinstance(opt_name, lisptype.lispKeyword) and opt_name.name == 'DOCUMENTATION':
                doc_rest = cdr(option)
                if _consp_internal(doc_rest):
                    documentation = car(doc_rest)
        current = cdr(current)
    
    # Create the generic function
    class GenericFunction:
        """A generic function that dispatches on argument types."""
        def __init__(self, name, lambda_list, methods):
            self.name = name
            self.lambda_list = lambda_list
            self.methods = methods  # List of (specializers, function) tuples
            self.__name__ = str(name)
        
        def add_method(self, specializers, function):
            """Add a method to this generic function."""
            self.methods.append((specializers, function))
        
        def find_applicable_method(self, args):
            """Find the most specific applicable method for the given args."""
            for specializers, method in self.methods:
                if self._matches_specializers(args, specializers):
                    return method
            return None
        
        def _matches_specializers(self, args, specializers):
            """Check if args match the specializers."""
            for i, spec in enumerate(specializers):
                if spec is None:
                    continue  # Matches any type
                if i >= len(args):
                    return False
                arg = args[i]
                # Check type match
                if isinstance(spec, lisptype.LispSymbol):
                    spec_name = spec.name.upper()
                    if spec_name == 'INTEGER':
                        if not isinstance(arg, int):
                            return False
                    elif spec_name == 'FLOAT':
                        if not isinstance(arg, float):
                            return False
                    elif spec_name == 'NUMBER':
                        if not isinstance(arg, (int, float, complex)):
                            return False
                    elif spec_name == 'STRING':
                        if not isinstance(arg, str):
                            return False
                    elif spec_name == 'T':
                        pass  # T matches anything
                    # Add more type checks as needed
            return True
        
        def __call__(self, *args):
            method = self.find_applicable_method(args)
            if method is None:
                raise lisptype.LispError(f"No applicable method for {self.name} with args {args}")
            return method(*args)
        
        def __repr__(self):
            return f"#<GENERIC-FUNCTION {self.name}>"
    
    gf = GenericFunction(func_name, lambda_list, methods)
    
    # Walk up to global environment
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    # Bind the generic function in the global environment
    global_env.add_function(func_name, gf)
    
    return func_name


def eval_defmethod(form, env):
    """Evaluate DEFMETHOD special form.
    
    DEFMETHOD adds a method to an existing generic function.
    
    Syntax:
        (defmethod name specializers body...)
        (defmethod name qualifiers specializers body...)
    
    Example:
        (defmethod is-similar* ((x number) (y number))
          (and (eq (class-of x) (class-of y))
               (= x y)))
    """
    from .evaluation_core import eval
    import fclpy.state as state
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFMETHOD requires at least a name")
    
    func_name = car(args)
    rest = cdr(args)
    
    if not isinstance(func_name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFMETHOD: function name must be a symbol")
    
    # Skip qualifiers if present (for now we ignore them)
    # Qualifiers are symbols like :BEFORE, :AFTER, :AROUND
    qualifiers = []
    while _consp_internal(rest):
        first = car(rest)
        if isinstance(first, lisptype.lispKeyword):
            qualifiers.append(first)
            rest = cdr(rest)
        else:
            break
    
    # Get specialized lambda list
    if not _consp_internal(rest):
        raise lisptype.LispNotImplementedError("DEFMETHOD requires a specialized lambda list")
    
    specialized_lambda_list = car(rest)
    method_body = cdr(rest)
    
    # Extract specializers and parameters from specialized-lambda-list
    # e.g., ((x number) (y number)) -> specializers: [NUMBER, NUMBER], params: [x, y]
    specializers = []
    params = []
    current = specialized_lambda_list
    while _consp_internal(current):
        param_spec = car(current)
        if _consp_internal(param_spec):
            # Specialized: (param-name type)
            param_name = car(param_spec)
            param_type = car(cdr(param_spec))
            specializers.append(param_type)
            params.append(param_name)
        else:
            # Unspecialized: just param-name (matches any type)
            specializers.append(None)
            params.append(param_spec)
        current = cdr(current)
    
    # Create the method function
    def make_method_function(param_list, body, captured_env):
        def method_func(*call_args):
            method_env = lisptype.Environment(captured_env)
            for i, param in enumerate(param_list):
                if i < len(call_args):
                    method_env.add_variable(param, call_args[i])
                else:
                    method_env.add_variable(param, lisptype.NIL)
            
            result = lisptype.NIL
            body_current = body
            while _consp_internal(body_current):
                result = eval(car(body_current), method_env)
                body_current = cdr(body_current)
            return result
        return method_func
    
    method_fn = make_method_function(params, method_body, env)
    
    # Find the generic function and add the method
    # Walk up to global environment
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    gf = global_env.find_func(func_name)
    if gf is None:
        # Auto-create a generic function if it doesn't exist
        class GenericFunction:
            def __init__(self, name):
                self.name = name
                self.lambda_list = None
                self.methods = []
                self.__name__ = str(name)
            
            def add_method(self, spec, fn):
                self.methods.append((spec, fn))
            
            def find_applicable_method(self, args):
                for spec_list, method in self.methods:
                    if self._matches_specializers(args, spec_list):
                        return method
                return None
            
            def _matches_specializers(self, args, spec_list):
                for i, spec in enumerate(spec_list):
                    if spec is None:
                        continue
                    if i >= len(args):
                        return False
                    arg = args[i]
                    if isinstance(spec, lisptype.LispSymbol):
                        spec_name = spec.name.upper()
                        if spec_name == 'INTEGER':
                            if not isinstance(arg, int):
                                return False
                        elif spec_name == 'NUMBER':
                            if not isinstance(arg, (int, float, complex)):
                                return False
                        elif spec_name == 'FLOAT':
                            if not isinstance(arg, float):
                                return False
                        elif spec_name == 'STRING':
                            if not isinstance(arg, str):
                                return False
                        elif spec_name == 'CHARACTER':
                            from fclpy.character import Character
                            if not isinstance(arg, Character):
                                return False
                        elif spec_name == 'SYMBOL':
                            if not isinstance(arg, lisptype.LispSymbol):
                                return False
                        elif spec_name == 'CONS':
                            if not _consp_internal(arg):
                                return False
                        elif spec_name == 'T':
                            pass  # T matches anything
                return True
            
            def __call__(self, *args):
                method = self.find_applicable_method(args)
                if method is None:
                    raise lisptype.LispError(f"No applicable method for {self.name} with args {args}")
                return method(*args)
            
            def __repr__(self):
                return f"#<GENERIC-FUNCTION {self.name}>"
        
        gf = GenericFunction(func_name)
        global_env.add_function(func_name, gf)
    
    # Add the method to the generic function
    if hasattr(gf, 'add_method'):
        gf.add_method(specializers, method_fn)
    elif hasattr(gf, 'methods'):
        gf.methods.append((specializers, method_fn))
    
    return func_name


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
    'eval_defconstant',
    'eval_defstruct',
    'eval_defclass',
    'eval_pop',
    'eval_defgeneric',
    'eval_defmethod',
    'eval_define_method_combination',
    '_store_optimization_declaration',
    '_store_special_declaration',
]


def eval_define_method_combination(form, env):
    """Evaluate DEFINE-METHOD-COMBINATION special form.
    
    DEFINE-METHOD-COMBINATION does not evaluate its name argument.
    It creates a method combination object and binds it in the global environment.
    
    Syntax:
        (define-method-combination name [options...])
    
    In FCLpy, we implement a simplified version that just creates a
    named method combination object without full CLOS semantics.
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFINE-METHOD-COMBINATION requires a name")
    
    name = car(args)
    # Name is NOT evaluated
    
    if not isinstance(name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFINE-METHOD-COMBINATION: name must be a symbol")
    
    # Create a method combination object
    class MethodCombination:
        def __init__(self, mc_name):
            self.name = mc_name.name if isinstance(mc_name, lisptype.LispSymbol) else str(mc_name)
        def __repr__(self):
            return f"#<METHOD-COMBINATION {self.name}>"
    
    mc = MethodCombination(name)
    
    # Walk up to global environment
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    # Bind the method combination
    global_env.add_variable(name, mc)
    
    return name
