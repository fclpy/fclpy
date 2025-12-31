"""Condition handling: SIGNAL, ERROR, restarts, multiple values."""

import fclpy.state as state
import fclpy.lisptype as lisptype
from .core import car, cdr, _consp_internal
from . import registry as _registry
from .evaluation_core import ConditionException
import fclpy.lispfunc as lispfunc


def eval_signal(form, env):
    """Implement SIGNAL special form.
    
    Syntax: (SIGNAL condition-object)
    
    Signal a condition, which may be handled by surrounding handlers.
    If not handled, SIGNAL returns NIL (unlike ERROR which doesn't return).
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("SIGNAL requires a condition argument")
    
    try:
        condition = eval(car(args), env)
    except ConditionException as e:
        # If evaluating the argument raises a condition, re-wrap it as recoverable
        raise ConditionException(e.condition, recoverable=True)
    
    # For now, just raise a ConditionException
    # In a complete implementation, this would consult handler-bind handlers
    raise ConditionException(condition, recoverable=True)


def eval_error(form, env):
    """Implement ERROR special form.
    
    Syntax: (ERROR) or (ERROR condition-object) or (ERROR format-control &rest format-arguments)
    
    Signal an error condition. This is like SIGNAL but the condition must be handled,
    or the program is aborted.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    
    # If no arguments, create a generic error
    if not _consp_internal(args):
        condition = lisptype.Error(message="Unspecified error")
        raise ConditionException(condition, recoverable=False)
    
    first_arg = car(args)
    
    # Check if it's a condition object or format string
    if isinstance(first_arg, str):
        # String case: (ERROR "format ~a" arg1 arg2 ...)
        condition = lisptype.SimpleCondition(format_string=first_arg)
    else:
        # Evaluate as condition object
        first_arg = eval(first_arg, env)
        condition = first_arg
    
    raise ConditionException(condition, recoverable=False)


def eval_cerror(form, env):
    """Implement CERROR special form.
    
    Syntax: (CERROR continue-format-control condition &optional (format-control) format-args...)
    
    Signal an error that has a built-in continue restart. If the user continues,
    CERROR returns NIL and execution resumes.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("CERROR requires at least condition argument")
    
    continue_format = car(args)  # Format for the continue option
    condition_form = car(cdr(args))
    
    try:
        condition = eval(condition_form, env)
    except ConditionException as e:
        # If evaluating the argument raises a condition, use that condition
        condition = e.condition
    
    if not isinstance(condition, lisptype.Condition):
        condition = lisptype.Error(message=str(condition))
    
    # Mark this as recoverable with a continue restart
    exception = ConditionException(condition, recoverable=True)
    exception.continue_format = continue_format
    raise exception


def eval_warn(form, env):
    """Implement WARN special form.
    
    Syntax: (WARN condition-object) or (WARN format-control &rest format-arguments)
    
    Signal a warning condition. Unlike ERROR, warnings don't require handling
    and execution normally continues.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("WARN requires at least one argument")
    
    try:
        first_arg = eval(car(args), env)
    except ConditionException as e:
        # If evaluating the argument raises a condition, convert it to a warning
        # Warnings don't interrupt execution, so we just return NIL
        return lisptype.NIL
    
    # Check if it's already a condition object
    if isinstance(first_arg, lisptype.Condition):
        condition = first_arg
    elif isinstance(first_arg, lisptype.Warning):
        condition = first_arg
    else:
        # Create a warning condition with the given message
        condition = lisptype.Warning(message=str(first_arg))
    
    # For warnings, we might want to print/log them but not raise
    # For now, return NIL (warnings don't interrupt execution)
    return lisptype.NIL


def eval_restart_case(form, env):
    """Implement RESTART-CASE special form.
    
    Syntax: (RESTART-CASE protected-form {restart-clause}*)
    
    Establishes named restarts with handlers that can be invoked during condition handling.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("RESTART-CASE requires a protected form")
    
    protected_form = car(args)
    restart_clauses = cdr(args)
    
    # Parse restart clauses into handlers
    restarts = {}
    current = restart_clauses
    while _consp_internal(current):
        clause = car(current)
        if _consp_internal(clause):
            restart_name = car(clause)
            clause_body = cdr(clause)
            
            if isinstance(restart_name, lisptype.LispSymbol):
                # Create handler that evaluates the clause body
                def make_handler(body):
                    def handler(*args):
                        result = lisptype.NIL
                        current_body = body
                        while _consp_internal(current_body):
                            result = eval(car(current_body), env)
                            current_body = cdr(current_body)
                        return result
                    return handler
                
                restarts[restart_name.name] = make_handler(clause_body)
        
        current = cdr(current)
    
    # Push restarts onto stack
    state.restart_stack.append(restarts)
    
    try:
        # Evaluate protected form
        result = eval(protected_form, env)
        return result
    except lisptype.RestartException as e:
        # Restart was invoked
        if e.restart_name in restarts:
            handler = restarts[e.restart_name]
            return handler(*e.args)
        raise
    finally:
        # Pop restarts from stack
        state.restart_stack.pop()


def eval_restart_bind(form, env):
    """Implement RESTART-BIND special form.
    
    Syntax: (RESTART-BIND ((name function) ...) {body}*)
    
    Binds restart functions for invocation.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("RESTART-BIND requires bindings")
    
    binding_clauses = car(args)
    body_forms = cdr(args)
    
    # Parse bindings
    restarts = {}
    current = binding_clauses
    while _consp_internal(current):
        binding = car(current)
        if _consp_internal(binding) and _consp_internal(cdr(binding)):
            restart_name = car(binding)
            handler_form = car(cdr(binding))
            
            handler = eval(handler_form, env)
            
            if isinstance(restart_name, lisptype.LispSymbol):
                restarts[restart_name.name] = handler
        
        current = cdr(current)
    
    # Push restarts onto stack
    state.restart_stack.append(restarts)
    
    try:
        # Evaluate body
        result = lisptype.NIL
        current = body_forms
        while _consp_internal(current):
            result = eval(car(current), env)
            current = cdr(current)
        return result
    finally:
        # Pop restarts from stack
        state.restart_stack.pop()


def eval_invoke_restart(form, env):
    """Implement INVOKE-RESTART special form.
    
    Syntax: (INVOKE-RESTART restart-name &rest arguments)
    
    Invokes a restart by name.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("INVOKE-RESTART requires a restart name")
    
    restart_name_form = car(args)
    restart_args = cdr(args)
    
    # Evaluate restart name
    if isinstance(restart_name_form, lisptype.LispSymbol):
        restart_name = restart_name_form.name
    else:
        restart_name = str(eval(restart_name_form, env))
    
    # Evaluate arguments
    evaluated_args = []
    current = restart_args
    while _consp_internal(current):
        evaluated_args.append(eval(car(current), env))
        current = cdr(current)
    
    # Search restart stack
    for restarts in reversed(state.restart_stack):
        if restart_name in restarts:
            handler = restarts[restart_name]
            result = handler(*evaluated_args) if evaluated_args else handler()
            raise lisptype.RestartException(restart_name, [result])
    
    # Restart not found
    raise lisptype.LispError(f"No restart named {restart_name}")


def eval_abort(form, env):
    """Implement ABORT special form.
    
    Syntax: (ABORT)
    
    Invokes the ABORT restart.
    """
    # Try to invoke ABORT restart
    for restarts in reversed(state.restart_stack):
        if 'ABORT' in restarts:
            handler = restarts['ABORT']
            result = handler()
            raise lisptype.RestartException('ABORT', [result])
    
    # No ABORT restart found
    raise lisptype.LispError("ABORT: No abort restart available")


@_registry.cl_special('MULTIPLE-VALUE-SETQ')
def eval_multiple_value_setq(form, env):
    """Evaluate MULTIPLE-VALUE-SETQ special form (stub).
    
    Full implementation would destructure values from the value form.
    """
    # For now, just return NIL - proper implementation later
    return lisptype.NIL


@_registry.cl_special('MULTIPLE-VALUE-CALL')
def eval_multiple_value_call(form, env):
    """Evaluate MULTIPLE-VALUE-CALL special form.
    
    Syntax: (MULTIPLE-VALUE-CALL function value-form1 value-form2 ...)
    
    Each value-form is evaluated. If a value-form returns a MultipleValues,
    all its values are passed as separate arguments to the function.
    If it returns a single value, that value is passed as one argument.
    
    Returns the result of calling the function.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("MULTIPLE-VALUE-CALL requires at least a function")
    
    # Evaluate the function form
    function_form = car(args)
    func = eval(function_form, env)
    
    # If func is a symbol, look it up in the environment (function position)
    if isinstance(func, lisptype.LispSymbol):
        func = env.find_func(func)
        if func is None:
            # Try auto-loading from registry
            try:
                from . import registry as _registry
                py_name = _registry.get_function_py_name(function_form.name)
                if py_name:
                    func = getattr(lispfunc, py_name, None)
                    if func:
                        # Bind into environment for future lookups
                        env.add_function(function_form, func)
            except Exception:
                pass
        if func is None:
            raise lisptype.LispNotImplementedError(f"MULTIPLE-VALUE-CALL: undefined function: {function_form}")
    
    # Collect all arguments from the value forms
    call_args = []
    value_forms = cdr(args)
    while _consp_internal(value_forms):
        result = eval(car(value_forms), env)
        if isinstance(result, lisptype.MultipleValues):
            # Add all values from MultipleValues
            call_args.extend(result.get_all())
        else:
            # Add single value
            call_args.append(result)
        value_forms = cdr(value_forms)
    
    # Call the function with collected arguments
    if callable(func):
        return func(*call_args) if call_args else func()
    else:
        raise lisptype.LispNotImplementedError(f"MULTIPLE-VALUE-CALL: not a function: {func}")


@_registry.cl_special('MULTIPLE-VALUE-BIND')
def eval_multiple_value_bind(form, env):
    """Evaluate MULTIPLE-VALUE-BIND special form.
    
    Syntax: (MULTIPLE-VALUE-BIND (var1 var2 ...) value-form body...)
    
    Evaluates value-form. If it returns a MultipleValues, each variable
    is bound to the corresponding value (or NIL if there aren't enough values).
    If it returns a single value, the first variable gets that value and
    others get NIL. Then evaluates the body forms.
    
    Returns the value of the last body form.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("MULTIPLE-VALUE-BIND requires vars, value-form, and body")
    
    # Extract the variable list and value-form
    vars = car(args)
    value_form = car(cdr(args))
    body = cdr(cdr(args))
    
    # Evaluate the value-form
    values = eval(value_form, env)
    
    # Create new environment for bindings
    new_env = lisptype.Environment(parent=env)
    
    # Extract variable list (it's a Lisp list of symbols)
    var_list = []
    current = vars
    while _consp_internal(current):
        var = car(current)
        var_list.append(var)
        current = cdr(current)
    
    # Bind variables to values
    if isinstance(values, lisptype.MultipleValues):
        value_tuple = values.get_all()
        for i, var in enumerate(var_list):
            if i < len(value_tuple):
                new_env.add_variable(var, value_tuple[i])
            else:
                new_env.add_variable(var, lisptype.NIL)
    else:
        # Single value - bind to first variable, rest get NIL
        if var_list:
            new_env.add_variable(var_list[0], values)
            for var in var_list[1:]:
                new_env.add_variable(var, lisptype.NIL)
    
    # Evaluate body forms and return last result
    result = lisptype.NIL
    while _consp_internal(body):
        result = eval(car(body), new_env)
        body = cdr(body)
    
    return result


def eval_handler_bind(form, env):
    """Implement HANDLER-BIND special form.
    
    Syntax: (HANDLER-BIND (binding*) form*)
    
    Where each binding is: (condition-type handler-function)
    
    Establishes condition handlers for the dynamic extent of the body forms.
    If a condition matching one of the types is signaled, the corresponding
    handler function is called.
    
    For now, this is a minimal implementation that just evaluates the body,
    without actually setting up handlers (since full condition system isn't complete).
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    
    # First arg is the bindings list
    if not _consp_internal(args):
        # No bindings and no body - return NIL
        return lisptype.NIL
    
    bindings = car(args)
    body = cdr(args)
    
    # For now, we just evaluate the body without setting up handlers
    # A full implementation would:
    # 1. Parse the binding clauses to extract condition types and handler functions
    # 2. Push the handlers onto a handler stack
    # 3. Evaluate the body
    # 4. Pop the handlers
    
    # Note: bindings might be NIL (empty) which is common for #+/-sbcl conditional code
    # that excludes certain bindings for non-SBCL implementations
    
    # Evaluate body forms and return last result
    result = lisptype.NIL
    while _consp_internal(body):
        result = eval(car(body), env)
        body = cdr(body)
    
    return result


def eval_handler_case(form, env):
    """Implement HANDLER-CASE special form.
    
    Syntax: (HANDLER-CASE expression
              (condition-type ([var]) form*) ...)
    
    Evaluates expression. If a condition of one of the specified types is signaled,
    control transfers to the corresponding handler clause and the forms are evaluated.
    
    For now, this is a minimal implementation.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    
    if not _consp_internal(args):
        return lisptype.NIL
    
    expression = car(args)
    clauses = cdr(args)
    
    # Condition type hierarchy for matching
    # In CL, arithmetic-error is a subtype of error
    # type-error is a subtype of error
    # All errors are subtypes of condition
    condition_hierarchy = {
        'ARITHMETIC-ERROR': ['ARITHMETIC-ERROR', 'ERROR', 'SERIOUS-CONDITION', 'CONDITION', 'T'],
        'TYPE-ERROR': ['TYPE-ERROR', 'ERROR', 'SERIOUS-CONDITION', 'CONDITION', 'T'],
        'ERROR': ['ERROR', 'SERIOUS-CONDITION', 'CONDITION', 'T'],
        'SERIOUS-CONDITION': ['SERIOUS-CONDITION', 'CONDITION', 'T'],
        'CONDITION': ['CONDITION', 'T'],
    }
    
    def matches_condition_type(handler_type, error):
        """Check if error matches the handler condition type."""
        handler_type_name = handler_type.upper() if isinstance(handler_type, str) else handler_type.name.upper()
        
        # TYPE-ERROR matches LispTypeError
        if handler_type_name in ('TYPE-ERROR', 'ARITHMETIC-ERROR', 'ERROR', 'SERIOUS-CONDITION', 'CONDITION', 'T'):
            if isinstance(error, lisptype.LispTypeError):
                # LispTypeError is both a type-error and can be arithmetic-error for numeric operations
                return handler_type_name in condition_hierarchy.get('TYPE-ERROR', ['TYPE-ERROR', 'ERROR', 'CONDITION', 'T'])
            elif isinstance(error, lisptype.LispError):
                # Generic error
                return handler_type_name in condition_hierarchy.get('ERROR', ['ERROR', 'CONDITION', 'T'])
        return False
    
    try:
        # Try to evaluate the expression
        return eval(expression, env)
    except lisptype.LispError as e:
        # Check if any clause handles this error
        current = clauses
        while _consp_internal(current):
            clause = car(current)
            if _consp_internal(clause):
                condition_type = car(clause)
                # Check if this clause matches the error
                if isinstance(condition_type, lisptype.LispSymbol):
                    if matches_condition_type(condition_type.name, e):
                        # This clause handles the error
                        var_list = car(cdr(clause))
                        clause_body = cdr(cdr(clause))
                        
                        # Create new environment with optional error variable
                        new_env = lisptype.Environment(parent=env)
                        if _consp_internal(var_list):
                            var = car(var_list)
                            new_env.add_variable(var, str(e))
                        
                        # Evaluate clause body
                        result = lisptype.NIL
                        while _consp_internal(clause_body):
                            result = eval(car(clause_body), new_env)
                            clause_body = cdr(clause_body)
                        return result
            current = cdr(current)
        # No handler found, re-raise
        raise


def eval_ignore_errors(form, env):
    """Implement IGNORE-ERRORS special form.
    
    Syntax: (IGNORE-ERRORS form*)
    
    Evaluates the body forms in sequence. If any form signals an error,
    execution stops and IGNORE-ERRORS returns two values: NIL and the
    condition object. If no error occurs, returns the primary value of
    the last form and NIL.
    
    Returns: MultipleValues(primary-value, condition-or-nil)
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    
    try:
        # Evaluate body forms
        result = lisptype.NIL
        while _consp_internal(args):
            result = eval(car(args), env)
            args = cdr(args)
        
        # Success: return primary value and NIL
        return lisptype.MultipleValues(result, lisptype.NIL)
    
    except Exception as e:
        # Error occurred: return NIL and the condition
        # Convert Python exception to a string representation
        condition = str(e) if not isinstance(e, lisptype.LispError) else e
        return lisptype.MultipleValues(lisptype.NIL, condition)


__all__ = [
    'eval_signal',
    'eval_error',
    'eval_cerror',
    'eval_warn',
    'eval_restart_case',
    'eval_restart_bind',
    'eval_invoke_restart',
    'eval_abort',
    'eval_multiple_value_setq',
    'eval_multiple_value_call',
    'eval_multiple_value_bind',
    'eval_handler_bind',
    'eval_handler_case',
    'eval_ignore_errors',
]
