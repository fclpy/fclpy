"""Condition handling: SIGNAL, ERROR, restarts, multiple values."""

import fclpy.state as state
import fclpy.lisptype as lisptype
from .core import car, cdr, cons, _consp_internal
from . import registry as _registry
from .evaluation_core import ConditionException, ThrowException
import re
import fclpy.lispfunc as lispfunc


# Condition type hierarchy for matching handler/handler-case clause types
# against signaled condition objects. In CL, ARITHMETIC-ERROR, TYPE-ERROR,
# and PROGRAM-ERROR are all subtypes of ERROR, which is a subtype of
# SERIOUS-CONDITION, which is a subtype of CONDITION.
_CONDITION_HIERARCHY = {
    'ARITHMETIC-ERROR': ['ARITHMETIC-ERROR', 'ERROR', 'SERIOUS-CONDITION', 'CONDITION', 'T'],
    'TYPE-ERROR': ['TYPE-ERROR', 'ERROR', 'SERIOUS-CONDITION', 'CONDITION', 'T'],
    'PROGRAM-ERROR': ['PROGRAM-ERROR', 'ERROR', 'SERIOUS-CONDITION', 'CONDITION', 'T'],
    'ERROR': ['ERROR', 'SERIOUS-CONDITION', 'CONDITION', 'T'],
    'SERIOUS-CONDITION': ['SERIOUS-CONDITION', 'CONDITION', 'T'],
    'CONDITION': ['CONDITION', 'T'],
}


def _condition_class_for_name(name):
    """Map a CL condition type name (e.g. "TYPE-ERROR") to its Python class
    in lisptype (e.g. lisptype.TypeError), or None if there isn't one.
    """
    camel = ''.join(part.capitalize() for part in name.replace('_', '-').split('-') if part)
    return getattr(lisptype, camel, None)


def _make_condition_from_designator(type_name_symbol, args_forms, env):
    """Build a condition instance from a (DATUM &rest ARGUMENTS) designator
    where DATUM names a condition type, per CLHS condition designators (used
    by ERROR, SIGNAL, CERROR, WARN): the type is instantiated via its
    keyword init-args, evaluating ARGS_FORMS as alternating keyword/value
    pairs. Returns None if the type name isn't a known condition class.
    """
    from .evaluation_core import eval

    condition_class = _condition_class_for_name(type_name_symbol.name)
    if condition_class is None:
        return None

    kwargs = {}
    cur = args_forms
    while _consp_internal(cur) and _consp_internal(cdr(cur)):
        key = eval(car(cur), env)
        value = eval(car(cdr(cur)), env)
        if isinstance(key, (lisptype.LispSymbol, lisptype.lispKeyword)):
            kwargs[key.name.lower().replace('-', '_')] = value
        cur = cdr(cdr(cur))
    return condition_class(**kwargs)


def _condition_matches(handler_type, error):
    """Check whether `error` (a signaled condition/exception object) matches
    the handler/handler-case clause type name `handler_type` (str or symbol).
    """
    handler_type_name = handler_type.upper() if isinstance(handler_type, str) else handler_type.name.upper()
    try:
        if isinstance(error, lisptype.Condition):
            # Convert CamelCase class name into hyphenated CL-style name,
            # e.g. TypeError -> TYPE-ERROR, SimpleCondition -> SIMPLE-CONDITION
            orig = error.__class__.__name__
            hyphenated = re.sub(r'([a-z0-9])([A-Z])', r'\1-\2', orig).upper()
            cond_name = hyphenated
            return handler_type_name in _CONDITION_HIERARCHY.get(cond_name, [cond_name, 'ERROR', 'CONDITION', 'T'])
    except Exception:
        pass

    if isinstance(error, lisptype.LispProgramError):
        return handler_type_name in _CONDITION_HIERARCHY.get('PROGRAM-ERROR', ['PROGRAM-ERROR', 'ERROR', 'CONDITION', 'T'])
    elif isinstance(error, lisptype.LispTypeError):
        return handler_type_name in _CONDITION_HIERARCHY.get('TYPE-ERROR', ['TYPE-ERROR', 'ERROR', 'CONDITION', 'T'])
    elif isinstance(error, lisptype.LispError):
        return handler_type_name in _CONDITION_HIERARCHY.get('ERROR', ['ERROR', 'CONDITION', 'T'])
    return False


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


def _build_condition_from_datum(datum_form, remaining_args_form, env):
    """Evaluate an ERROR/CERROR datum and build the condition object it
    designates (CLHS 9.1). Shared by ERROR and CERROR: CERROR's datum and
    arguments are specified to behave "as if by (apply #'error datum
    arguments)", so this is one dispatch, not two.

    Always evaluates the datum *before* deciding how to build the condition,
    so a string datum is treated the same whether it arrived as a literal
    in the form or via a variable/expression -- deciding based on the raw,
    unevaluated form would make (error fmt) behave differently from
    (error "literal") for the same runtime value, which is exactly the bug
    this replaced (handler-case (simple-error ...) could never match a
    string datum that came from a variable).
    """
    from .evaluation_core import eval
    datum = eval(datum_form, env)

    if isinstance(datum, (str, lisptype.LispString)):
        # String datum: signals a condition of type SIMPLE-ERROR (a subtype
        # of ERROR), not bare SIMPLE-CONDITION (which handler-case/
        # handler-bind ERROR clauses would not match).
        format_arguments = []
        cur = remaining_args_form
        while _consp_internal(cur):
            format_arguments.append(eval(car(cur), env))
            cur = cdr(cur)
        return lisptype.SimpleError(format_control=str(datum), format_arguments=format_arguments)
    elif isinstance(datum, (lisptype.LispSymbol, lisptype.lispKeyword)):
        # Per ANSI condition designators, if the (evaluated) datum is a
        # symbol naming a condition type, build an instance of that type
        # from the remaining keyword init-args rather than signaling the
        # bare type-name symbol itself.
        built = _make_condition_from_designator(datum, remaining_args_form, env)
        return built if built is not None else datum
    else:
        return datum


def eval_error(form, env):
    """Implement ERROR special form.

    Syntax: (ERROR) or (ERROR condition-object) or (ERROR format-control &rest format-arguments)

    Signal an error condition. This is like SIGNAL but the condition must be handled,
    or the program is aborted.
    """
    args = cdr(form)

    # If no arguments, create a generic error
    if not _consp_internal(args):
        condition = lisptype.Error(message="Unspecified error")
        raise ConditionException(condition, recoverable=False)

    condition = _build_condition_from_datum(car(args), cdr(args), env)
    raise ConditionException(condition, recoverable=False)


def eval_cerror(form, env):
    """Implement CERROR special form.
    
    Syntax: (CERROR continue-format-control condition &optional (format-control) format-args...)
    
    Signal an error that has a built-in continue restart. If the user continues,
    CERROR returns NIL and execution resumes.
    """
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("CERROR requires at least condition argument")

    continue_format = car(args)  # Format for the continue option
    condition_form = car(cdr(args))
    # CLHS 9.1: cerror's datum/arguments behave "as if by (apply #'error
    # datum arguments)" -- same dispatch as ERROR, including string datums
    # from a variable building a proper SIMPLE-ERROR.
    remaining_args_form = cdr(cdr(args))

    try:
        condition = _build_condition_from_datum(condition_form, remaining_args_form, env)
    except ConditionException as e:
        # If evaluating the argument raises a condition, use that condition
        condition = e.condition

    if not isinstance(condition, lisptype.Condition):
        condition = lisptype.Error(message=str(condition))

    # Mark this as recoverable with a continue restart
    exception = ConditionException(condition, recoverable=True)
    exception.continue_format = continue_format
    raise exception


def _make_condition_from_evaluated_designator(type_name_symbol, arguments):
    """Like `_make_condition_from_designator`, but for a condition-type
    designator whose init-args have already been evaluated (the case for
    ordinary function calls, e.g. `(funcall #'warn 'my-warning :foo 1)`,
    where the registry evaluates all arguments before the call).
    """
    condition_class = _condition_class_for_name(type_name_symbol.name)
    if condition_class is None:
        return None

    kwargs = {}
    it = iter(arguments)
    for key in it:
        value = next(it, lisptype.NIL)
        if isinstance(key, (lisptype.LispSymbol, lisptype.lispKeyword)):
            kwargs[key.name.lower().replace('-', '_')] = value
    return condition_class(**kwargs)


def signal_warning(datum, arguments):
    """Build the warning condition for WARN's (DATUM &rest ARGUMENTS) designator,
    print it, and return NIL. Shared by both the WARN special form (eval_warn,
    for unevaluated call sites) and the WARN function designator (warn_fn in
    utilities_errors.py, used by FUNCALL/APPLY/#'WARN) so there is exactly one
    place that knows how a warning is built and reported.

    Real handler-stack dispatch (so HANDLER-BIND/MUFFLE-WARNING can intercept
    this before it prints) requires the signal-before-unwind rewrite planned
    for M8; until then this always prints, matching WARN's unhandled behavior.
    """
    from fclpy.lispfunc.io_write import format_fn

    if isinstance(datum, (str, lisptype.LispString)):
        control_str = str(datum)
        message = format_fn(lisptype.NIL, control_str, *arguments)
        condition = lisptype.SimpleWarning(format_control=control_str, format_arguments=list(arguments))
    elif isinstance(datum, lisptype.Condition):
        condition = datum
        message = str(datum)
    elif isinstance(datum, (lisptype.LispSymbol, lisptype.lispKeyword)):
        built = _make_condition_from_evaluated_designator(datum, arguments)
        condition = built if built is not None else lisptype.Warning(message=str(datum))
        message = str(condition)
    else:
        condition = lisptype.Warning(message=str(datum))
        message = str(condition)

    print(f"Warning: {message}")
    return lisptype.NIL


def eval_warn(form, env):
    """Implement WARN special form.

    Syntax: (WARN format-control &rest format-arguments) or (WARN condition-designator ...)

    Signal a warning condition. Unlike ERROR, warnings don't require handling
    and execution normally continues.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("WARN requires at least one argument")

    datum = eval(car(args), env)
    arguments = []
    cur = cdr(args)
    while _consp_internal(cur):
        arguments.append(eval(car(cur), env))
        cur = cdr(cur)

    return signal_warning(datum, arguments)


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


def _assign_variable_or_place(var, result, env):
    """Assign `result` to `var`, honoring a SYMBOL-MACROLET expansion.

    If `var` is a plain variable name, SETQ it directly. If it names a
    symbol-macro (e.g. established by SYMBOL-MACROLET binding it to
    (CAR X)), re-evaluate the expansion's sub-forms fresh (they may have
    side effects, per ANSI) and mutate the resulting place. Only CAR/CDR
    expansions are supported as places here; anything else falls back to
    plain variable assignment.
    """
    from .evaluation_core import eval

    expansion = env.get_symbol_macro(var)
    if expansion is None:
        env.set_variable(var, result)
        return

    if isinstance(expansion, lisptype.LispSymbol):
        _assign_variable_or_place(expansion, result, env)
        return

    if _consp_internal(expansion) and isinstance(car(expansion), lisptype.LispSymbol):
        op_name = car(expansion).name
        place_args = cdr(expansion)
        if op_name == 'CAR' and _consp_internal(place_args):
            target = eval(car(place_args), env)
            if _consp_internal(target):
                target.car = result
                return
        elif op_name == 'CDR' and _consp_internal(place_args):
            target = eval(car(place_args), env)
            if _consp_internal(target):
                target.cdr = result
                return

    raise lisptype.LispNotImplementedError(
        f"MULTIPLE-VALUE-SETQ: unsupported symbol-macro place expansion for {var}")


@_registry.cl_special('MULTIPLE-VALUE-SETQ')
def eval_multiple_value_setq(form, env):
    """Evaluate MULTIPLE-VALUE-SETQ special form.

    Syntax: (MULTIPLE-VALUE-SETQ (var1 var2 ...) value-form)

    Evaluates value-form once. If it returns a MultipleValues, each
    variable is SETQ'd to the corresponding value (or NIL if there aren't
    enough values). If it returns a single value, the first variable gets
    that value and the rest get NIL. Returns the primary (first) value of
    value-form, regardless of how many variables are given. A var naming a
    symbol-macro is assigned through its expansion (see
    _assign_variable_or_place) rather than as a plain variable.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("MULTIPLE-VALUE-SETQ requires vars and a value-form")

    vars = car(args)
    value_form = car(cdr(args))

    values = eval(value_form, env)

    var_list = []
    current = vars
    while _consp_internal(current):
        var_list.append(car(current))
        current = cdr(current)

    if isinstance(values, lisptype.MultipleValues):
        value_tuple = values.get_all()
        for i, var in enumerate(var_list):
            _assign_variable_or_place(var, value_tuple[i] if i < len(value_tuple) else lisptype.NIL, env)
        return value_tuple[0] if value_tuple else lisptype.NIL
    else:
        primary = values if values is not None else lisptype.NIL
        for i, var in enumerate(var_list):
            _assign_variable_or_place(var, primary if i == 0 else lisptype.NIL, env)
        return primary


@_registry.cl_special('MULTIPLE-VALUE-PROG1')
def eval_multiple_value_prog1(form, env):
    """Evaluate MULTIPLE-VALUE-PROG1 special form.

    Syntax: (MULTIPLE-VALUE-PROG1 first-form form*)

    Evaluates first-form, saving all of its values (primary and any
    secondary values). Then evaluates the remaining forms in order, for
    effect only, discarding their results. Finally returns the saved
    values from first-form.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("MULTIPLE-VALUE-PROG1 requires at least one form")

    saved_values = eval(car(args), env)

    rest = cdr(args)
    while _consp_internal(rest):
        eval(car(rest), env)
        rest = cdr(rest)

    return saved_values


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
    handler function is called with the condition object. Unlike HANDLER-CASE,
    HANDLER-BIND does not itself transfer control: if the handler returns
    normally (rather than performing a non-local exit via RETURN-FROM, THROW,
    a restart, etc.), signaling continues outward past this HANDLER-BIND.

    Note: bindings may be NIL (empty), which is common for #+/-sbcl conditional
    code that excludes certain bindings for non-SBCL implementations; an empty
    binding list simply means nothing here can handle the condition.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL

    bindings = car(args)
    body = cdr(args)

    parsed_bindings = []
    current = bindings
    while _consp_internal(current):
        binding = car(current)
        if _consp_internal(binding) and _consp_internal(cdr(binding)):
            condition_type = car(binding)
            handler_form = car(cdr(binding))
            handler_fn = eval(handler_form, env)
            parsed_bindings.append((condition_type, handler_fn))
        current = cdr(current)

    def run_body():
        result = lisptype.NIL
        cur = body
        while _consp_internal(cur):
            result = eval(car(cur), env)
            cur = cdr(cur)
        return result

    try:
        return run_body()
    except (ConditionException, lisptype.LispError) as exc:
        # ConditionException wraps its condition in `.condition`; plain
        # LispError-style exceptions (an older raising convention still used
        # in parts of the codebase) are themselves the condition object.
        cond_obj = exc.condition if isinstance(exc, ConditionException) else exc
        for condition_type, handler_fn in parsed_bindings:
            if isinstance(condition_type, lisptype.LispSymbol) and _condition_matches(condition_type.name, cond_obj):
                if callable(handler_fn):
                    # If the handler performs a non-local exit (RETURN-FROM,
                    # THROW, invoking a restart, ...) that exception simply
                    # propagates out of this call and out of eval_handler_bind.
                    handler_fn(cond_obj)
        # No matching handler transferred control: continue signaling outward.
        raise


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

    matches_condition_type = _condition_matches

    try:
        # Try to evaluate the expression
        return eval(expression, env)
    except ThrowException as e:
        # Uncaught THROW - convert to a CONTROL-ERROR condition
        control_error = lisptype.ControlError(message=f"Uncaught THROW {e.tag}")
        ce = ConditionException(control_error, recoverable=False)
        cond_obj = ce.condition
        current = clauses
        while _consp_internal(current):
            clause = car(current)
            if _consp_internal(clause):
                condition_type = car(clause)
                # Check if this clause matches the condition
                if isinstance(condition_type, lisptype.LispSymbol):
                    if matches_condition_type(condition_type.name, cond_obj):
                        var_list = car(cdr(clause))
                        clause_body = cdr(cdr(clause))

                        # Create new environment with optional condition variable
                        new_env = lisptype.Environment(parent=env)
                        if _consp_internal(var_list):
                            var = car(var_list)
                            # Store the condition object in the variable
                            new_env.add_variable(var, cond_obj)

                        # Evaluate clause body
                        result = lisptype.NIL
                        while _consp_internal(clause_body):
                            result = eval(car(clause_body), new_env)
                            clause_body = cdr(clause_body)
                        return result
            current = cdr(current)
        # No handler found for the condition; re-raise as ConditionException
        raise ce
    except ConditionException as ce:
        # A Lisp condition was signaled; try to match clauses against the condition object
        cond_obj = ce.condition
        current = clauses
        while _consp_internal(current):
            clause = car(current)
            if _consp_internal(clause):
                condition_type = car(clause)
                # Check if this clause matches the condition
                if isinstance(condition_type, lisptype.LispSymbol):
                    if matches_condition_type(condition_type.name, cond_obj):
                        var_list = car(cdr(clause))
                        clause_body = cdr(cdr(clause))

                        # Create new environment with optional condition variable
                        new_env = lisptype.Environment(parent=env)
                        if _consp_internal(var_list):
                            var = car(var_list)
                            # Store the condition object in the variable
                            new_env.add_variable(var, cond_obj)

                        # Evaluate clause body
                        result = lisptype.NIL
                        while _consp_internal(clause_body):
                            result = eval(car(clause_body), new_env)
                            clause_body = cdr(clause_body)
                        return result
            current = cdr(current)
        # No handler found for the condition; re-raise the original exception
        raise
    except lisptype.LispError as e:
        # Legacy behavior: handle LispError exceptions similarly
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
                            # Store the actual exception object, not just its string
                            new_env.add_variable(var, e)

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
