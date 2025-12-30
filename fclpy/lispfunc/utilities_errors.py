"""Error handling, conditions, warnings, and restart operations."""

import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry


# --- Condition definition and handling ---
@_registry.cl_function('DEFINE-CONDITION')
def define_condition(name, parent_types, slot_specs, *options):
    """Define condition type."""
    return name


@_registry.cl_function('MAKE-CONDITION')
def make_condition(type_designator, *args):
    """Create condition object."""
    return type_designator


@_registry.cl_function('SIGNAL')
def signal_fn(datum, *arguments):
    """Signal condition (notify handlers without stopping)."""
    return None


@_registry.cl_function('ERROR')
def error_fn(datum, *arguments):
    """Signal error condition (stop execution)."""
    raise Exception(str(datum))


@_registry.cl_function('WARN')
def warn_fn(datum, *arguments):
    """Warn about condition."""
    print(f"Warning: {datum}")
    return None


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
# NOTE: DEFINE-METHOD-COMBINATION is implemented as a special form in evaluation_core.py
# This function stub is kept for backward compatibility if called directly
@_registry.cl_function('DEFINE-METHOD-COMBINATION')
def define_method_combination(name, *args):
    """Define method combination.
    
    In FCLpy, we create a simple method combination object that can be
    used as a value. Full method combination semantics are not implemented.
    
    NOTE: This should typically be called as a special form that doesn't evaluate
    its name argument. This function exists for direct invocation.
    """
    import fclpy.state as state
    import fclpy.lisptype as lt
    
    # Create a simple method combination object
    class MethodCombination:
        def __init__(self, name):
            self.name = name
        def __repr__(self):
            return f"#<METHOD-COMBINATION {self.name}>"
    
    mc = MethodCombination(name if isinstance(name, str) else name.name if hasattr(name, 'name') else str(name))
    
    # Bind to global environment if available
    if hasattr(state, 'current_environment') and state.current_environment is not None:
        env = state.current_environment
        # Walk to global environment
        while env.parent is not None:
            env = env.parent
        if isinstance(name, lt.LispSymbol):
            env.add_variable(name, mc)
    
    return mc


@_registry.cl_function('METHOD-COMBINATION-ERROR')
def method_combination_error(format_control, *format_arguments):
    """Method combination error."""
    return None


@_registry.cl_function('INVALID-METHOD-ERROR')
def invalid_method_error(method, format_control, *format_args):
    """Invalid method error."""
    raise lisptype.LispNotImplementedError('INVALID-METHOD-ERROR')


@_registry.cl_function('PACKAGE-ERROR-PACKAGE')
def package_error_package(condition):
    """Get package from package error condition."""
    return None


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
    'define_method_combination',
    'method_combination_error',
    'invalid_method_error',
    'package_error_package',
]
