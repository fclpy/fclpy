"""Error handling, conditions, warnings, and restart operations."""

import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry


# --- Condition definition and handling ---
## `DEFINE-CONDITION` is a special form handled by the evaluator;
## do not register it as a regular function here.
def define_condition(name, parent_types, slot_specs, *options):
    """Define condition type (stub kept for reference)."""
    return name


@_registry.cl_function('MAKE-CONDITION')
def make_condition(type_designator, *args):
    """Create and return a condition object (CLHS 9.2 MAKE-CONDITION).

    type_designator names a condition type; the remaining args are
    alternating initarg keyword/value pairs, exactly like MAKE-INSTANCE.
    Previously a stub that returned type_designator itself -- the bare type
    symbol, not a condition -- so (error (make-condition 'simple-error ...))
    signaled an unmatchable non-condition object (plan.md Finding E) instead
    of a real one.
    """
    from fclpy.lispfunc.evaluation_conditions import make_condition_of_type

    built = make_condition_of_type(type_designator, list(args))
    if built is not None:
        return built

    raise lisptype.LispTypeError(
        f"MAKE-CONDITION: {type_designator!r} does not designate a known condition type",
        expected_type='condition-type-designator', actual_value=type_designator)


@_registry.cl_function('SIGNAL')
def signal_fn(datum, *arguments):
    """SIGNAL as a function designator (used by FUNCALL/APPLY/#'SIGNAL).

    Delegates to the same signaling core as the SIGNAL special form, so both
    build the condition the same way and walk the same handler stack. This used
    to be `return None` -- it notified nothing and signaled nothing, so
    (funcall #'signal ...) silently did nothing at all.
    """
    from fclpy.lispfunc.evaluation_conditions import build_condition, signal_condition_object

    condition = build_condition(datum, list(arguments), lisptype.SimpleCondition)
    return signal_condition_object(condition)


@_registry.cl_function('ERROR')
def error_fn(datum, *arguments):
    """ERROR as a function designator (used by FUNCALL/APPLY/#'ERROR).

    Delegates to the same signaling core as the ERROR special form. This used
    to raise a bare Python `Exception` carrying a formatted message: no
    condition object existed, so no HANDLER-BIND/HANDLER-CASE clause could
    match it -- not even (ERROR (C) ...) -- and it escaped every handler
    (plan.md Finding E). The ANSI suite reaches this path constantly, because
    RT's own `report-error` calls (apply #'error args).
    """
    from fclpy.lispfunc.evaluation_conditions import build_condition, signal_error_object

    condition = build_condition(datum, list(arguments), lisptype.SimpleError)
    return signal_error_object(condition)


@_registry.cl_function('WARN')
def warn_fn(datum, *arguments):
    """Warn about condition (function-designator entry point, used by
    FUNCALL/APPLY/#'WARN). Delegates to the same warning-signaling logic as
    the WARN special form (evaluation_conditions.eval_warn) so there is one
    implementation, not two that can silently drift apart.
    """
    from fclpy.lispfunc.evaluation_conditions import signal_warning
    return signal_warning(datum, list(arguments))


@_registry.cl_function('MUFFLE-WARNING')
def muffle_warning(condition=None):
    """Muffle warning condition."""
    return None


# --- Restart operations ---
@_registry.cl_function('COMPUTE-RESTARTS')
def compute_restarts(condition=None):
    """Compute available restarts for condition."""
    return []


@_registry.cl_function('FIND-RESTART')
def find_restart(identifier, condition=None):
    """Find a restart by identifier."""
    return lisptype.NIL


@_registry.cl_function('RESTART-BIND')
def restart_bind(restart_definitions, *body):
    """Restart bind macro."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('RESTART-CASE')
def restart_case(form, *restart_clauses):
    """Restart case macro."""
    return form


@_registry.cl_function('INVOKE-RESTART')
def invoke_restart(restart, *arguments):
    """Invoke a restart."""
    return None


@_registry.cl_function('INVOKE-RESTART-INTERACTIVELY')
def invoke_restart_interactively(restart):
    """Invoke restart interactively."""
    return None


@_registry.cl_function('RESTART-NAME')
def restart_name(restart):
    """Get restart name."""
    return str(restart)


def make_restart(name, function, **kwargs):
    """Create restart object."""
    return name


# --- Handler operations ---
def handler_bind(handler_definitions, *body):
    """Handler bind macro."""
    result = None
    for form in body:
        result = form
    return result


def handler_case(form, *handler_clauses):
    """Handler case macro."""
    return form


# --- Condition and restart utilities ---
@_registry.cl_function('ABORT')
def abort(condition=True):
    """Abort with restart (stub)."""
    return None


def cerror(continue_format_control, datum, *arguments):
    """Continuable error."""
    print(f"Error: {datum}")
    return None


def ignore_errors(*body):
    """Ignore errors in body forms."""
    try:
        result = None
        for form in body:
            result = form
        return result
    except:
        return None


@_registry.cl_function('WITH-CONDITION-RESTARTS')
def with_condition_restarts(condition_form, restarts_form, *body):
    """With condition restarts."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WITH-SIMPLE-RESTART')
def with_simple_restart(name, format_control, *body):
    """With simple restart."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('INVOKE-DEBUGGER')
def invoke_debugger(condition=None):
    """Invoke debugger - lightweight stub for test environment."""
    print("Debugger invoked", condition)
    return None


@_registry.cl_function('STORE-VALUE')
def store_value(value):
    """Store value restart."""
    return value


@_registry.cl_function('USE-VALUE')
def use_value(value):
    """Use value restart."""
    return value


# --- Method combination and condition errors ---
# DEFINE-METHOD-COMBINATION lives in `evaluation_special_forms.py` and is
# dispatched by the evaluator, because none of its subforms may be
# evaluated. The second implementation that used to sit here -- a
# `cl_function` building a private one-off MethodCombination class and
# binding it as a *variable* -- was reachable through `SYMBOL-FUNCTION` and
# `FUNCALL`, defined nothing, and disagreed with the special form about
# what the operator even produces (standing rule 3).


@_registry.cl_function('METHOD-COMBINATION-ERROR')
def method_combination_error(format_control, *format_arguments):
    """METHOD-COMBINATION-ERROR (CLHS 7.6.6): signal that the applicable
    methods cannot be combined. It *signals*; returning NIL (what this did)
    let a method combination whose own body detected an inconsistency carry
    on and produce an answer anyway."""
    from fclpy.lispfunc.io_write import format_fn
    import fclpy.classes as classes
    message = format_fn(lisptype.NIL, format_control, *format_arguments)
    raise classes.MethodCombinationError(str(message))


@_registry.cl_function('INVALID-METHOD-ERROR')
def invalid_method_error(method, format_control, *format_args):
    """Invalid method error."""
    raise lisptype.LispNotImplementedError('INVALID-METHOD-ERROR')


@_registry.cl_function('PACKAGE-ERROR-PACKAGE')
def package_error_package(condition):
    """Get package from package error condition."""
    return None


@_registry.cl_function('TYPE-ERROR-DATUM')
def type_error_datum(*args):
    """Get the datum (offending value) from a type-error condition."""
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"TYPE-ERROR-DATUM: wrong number of arguments (got {len(args)}, expected 1)"
        )
    condition = args[0]
    # If condition is a Python-level LispTypeError, get its actual_value attribute
    if isinstance(condition, lisptype.LispTypeError):
        return getattr(condition, 'actual_value', lisptype.NIL)
    # If condition is the Lisp Condition TypeError (from lisptype_extended),
    # retrieve the 'datum' slot
    try:
        if isinstance(condition, lisptype.TypeError):
            return condition._slots.get('datum', lisptype.NIL)
    except Exception:
        pass
    # If it's a string representation of an error, return NIL
    return lisptype.NIL


@_registry.cl_function('TYPE-ERROR-EXPECTED-TYPE')
def type_error_expected_type(*args):
    """Get the expected type from a type-error condition."""
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"TYPE-ERROR-EXPECTED-TYPE: wrong number of arguments (got {len(args)}, expected 1)"
        )
    condition = args[0]
    # If condition is a LispTypeError, get its expected_type attribute
    if isinstance(condition, lisptype.LispTypeError):
        return getattr(condition, 'expected_type', lisptype.NIL)
    # If condition is the Lisp Condition TypeError, retrieve the 'expected-type' slot
    try:
        if isinstance(condition, lisptype.TypeError):
            return condition._slots.get('expected-type', lisptype.NIL)
    except Exception:
        pass
    # If it's a string representation of an error, return NIL
    return lisptype.NIL


@_registry.cl_function('CELL-ERROR-NAME')
def cell_error_name(*args):
    """Return the name associated with a CELL-ERROR condition.

    Usage: (CELL-ERROR-NAME condition)
    Returns the value of the 'name' slot if present, otherwise NIL.
    """
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"CELL-ERROR-NAME: wrong number of arguments (got {len(args)}, expected 1)"
        )
    condition = args[0]
    # If it's a lisptype.Condition, use its get_slot method
    try:
        if isinstance(condition, lisptype.Condition):
            name = condition.get_slot('name')
            return name if name is not None else lisptype.NIL
    except Exception:
        pass
    # If it's a legacy LispError with attributes, try common keys
    try:
        if isinstance(condition, lisptype.LispError):
            return getattr(condition, 'name', lisptype.NIL)
    except Exception:
        pass
    return lisptype.NIL


__all__ = [
    'define_condition',
    'make_condition',
    'signal_fn',
    'error_fn',
    'warn_fn',
    'muffle_warning',
    'compute_restarts',
    'find_restart',
    'restart_bind',
    'restart_case',
    'invoke_restart',
    'invoke_restart_interactively',
    'restart_name',
    'make_restart',
    'handler_bind',
    'handler_case',
    'abort',
    'cerror',
    'ignore_errors',
    'with_condition_restarts',
    'with_simple_restart',
    'invoke_debugger',
    'store_value',
    'use_value',
    'method_combination_error',
    'invalid_method_error',
    'package_error_package',
    'type_error_datum',
    'type_error_expected_type',
]
