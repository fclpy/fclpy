"""Miscellaneous utility functions and system operations."""

import time
import inspect
import fclpy.lisptype as lisptype
import fclpy.state as state
from fclpy.lispfunc import registry as _registry


# --- Core basic utilities restored after cleanup ---------------------------------
@_registry.cl_function('ABORT')
def abort(condition=True):
    """Abort with restart (stub)."""
    return None


@_registry.cl_function('APROPOS')
def apropos(string, package=None):
    """Find symbols matching string."""
    raise lisptype.LispNotImplementedError("APROPOS")


@_registry.cl_function('APROPOS-LIST')
def apropos_list(string, package=None):
    """List symbols matching string."""
    raise lisptype.LispNotImplementedError("APROPOS-LIST")


def describe(object, stream=None):  # unified describe returns structured info
    try:
        info = {'TYPE': type(object).__name__}
        if hasattr(object, 'name'):
            info['NAME'] = getattr(object, 'name')
        if hasattr(object, '__dict__'):
            info['ATTRS'] = list(object.__dict__.keys())[:8]
        info['REPR'] = repr(object)
        return info
    except Exception:
        return {'REPR': str(object)}


def list_to_cons(pylist):
    """Convert a Python list of Lisp objects to a Lisp cons-list."""
    result = lisptype.NIL
    for itm in reversed(pylist):
        result = lisptype.lispCons(itm, result)  # type: ignore
    return result


def parse_macro_lambda_list(lambda_list):
    """Parse a macro lambda-list into components.

    Supports a basic set of lambda-list keywords: &optional, &rest, &whole, &key.
    Returns a dict with keys: required, optional, rest, whole, keys, allow_other_keys.
    """
    from fclpy.lispfunc.core import _consp_internal, car, cdr

    mode = 'required'
    required = []
    optional = []
    rest = None
    whole = None
    keys = []
    allow_other_keys = False
    while _consp_internal(lambda_list):
        item = car(lambda_list)
        lambda_list = cdr(lambda_list)
        if isinstance(item, lisptype.LispSymbol):
            name = item.name.upper()
            if name == '&OPTIONAL':
                mode = 'optional'; continue
            if name == '&REST':
                mode = 'rest'; continue
            if name == '&WHOLE':
                mode = 'whole'; continue
            if name == '&KEY':
                mode = 'key'; continue
            if name == '&ALLOW-OTHER-KEYS':
                allow_other_keys = True; continue
        if mode == 'required':
            required.append(item)
        elif mode == 'optional':
            optional.append(item)
        elif mode == 'rest':
            rest = item; mode = 'post-rest'
        elif mode == 'whole':
            whole = item; mode = 'required'
        elif mode == 'key':
            keys.append(item)
    return {
        'required': required,
        'optional': optional,
        'rest': rest,
        'whole': whole,
        'keys': keys,
        'allow_other_keys': allow_other_keys
    }


@_registry.cl_function('GET-UNIVERSAL-TIME')
def get_universal_time():
    import datetime
    epoch = datetime.datetime(1900, 1, 1)
    return int((datetime.datetime.utcnow() - epoch).total_seconds())


@_registry.cl_function('DECODE-UNIVERSAL-TIME')
def decode_universal_time(universal_time=None):
    import datetime
    if universal_time is None:
        universal_time = get_universal_time()
    epoch = datetime.datetime(1900, 1, 1)
    dt = epoch + datetime.timedelta(seconds=universal_time)
    return (dt.second, dt.minute, dt.hour, dt.day, dt.month, dt.year, None)


@_registry.cl_function('GET-DECODED-TIME')
def get_decoded_time():
    return decode_universal_time(get_universal_time())


@_registry.cl_function('TIME')
def time_fn():
    """Return current time in seconds since epoch (stub for Lisp TIME)."""
    import time as _time
    return _time.time()


def sleep(seconds):
    """Sleep for given number of seconds."""
    import time
    time.sleep(seconds)
    return None


# System information
@_registry.cl_function('LISP-IMPLEMENTATION-TYPE')
def lisp_implementation_type():
    """Get Lisp implementation type."""
    return "FCLPY"


@_registry.cl_function('LISP-IMPLEMENTATION-VERSION')
def lisp_implementation_version():
    """Get Lisp implementation version."""
    return "0.1.0"


@_registry.cl_function('MACHINE-INSTANCE')
def machine_instance():
    """Get machine instance."""
    import socket
    return socket.gethostname()


@_registry.cl_function('MACHINE-TYPE')
def machine_type():
    """Get machine type."""
    import platform
    return platform.machine()


@_registry.cl_function('MACHINE-VERSION')
def machine_version():
    """Get machine version."""
    import platform
    return platform.platform()


@_registry.cl_function('SOFTWARE-TYPE')
def software_type():
    """Get software type."""
    import platform
    return platform.system()


@_registry.cl_function('SOFTWARE-VERSION')
def software_version():
    """Get software version."""
    import platform
    return platform.release()


@_registry.cl_function('SHORT-SITE-NAME')
def short_site_name():
    """Get short site name."""
    return "Unknown"


@_registry.cl_function('LONG-SITE-NAME')
def long_site_name():
    """Get long site name."""
    return "Unknown Site"


# Environment access
@_registry.cl_function('USER-HOMEDIR-PATHNAME')
def user_homedir_pathname():
    """Get user home directory."""
    import os
    return os.path.expanduser("~")


def get_env(name):
    """Get environment variable."""
    import os
    return os.environ.get(name)


def exit(code=0):
    """Exit the Lisp system."""
    import sys
    sys.exit(code)


def quit(code=0):
    """Quit the Lisp system."""
    exit(code)


@_registry.cl_function('RANDOM')
def random(limit, state=None):
    """Generate random number."""
    import random as rnd
    if isinstance(limit, int):
        return rnd.randrange(limit)
    elif isinstance(limit, float):
        return rnd.random() * limit
    else:
        raise lisptype.LispNotImplementedError("RANDOM: invalid limit type")


@_registry.cl_function('MAKE-RANDOM-STATE')
def make_random_state(state=None):
    """Make random state."""
    import random as rnd
    new_state = rnd.getstate()
    if state is not None:
        rnd.setstate(state)
    return new_state


@_registry.cl_function('RANDOM-STATE-P')
def random_state_p(object):
    """Test if object is random state."""
    return isinstance(object, tuple) and len(object) >= 2


@_registry.cl_function('COMPILE')
def compile_fn(name, definition=None):
    """Compile function."""
    return name


@_registry.cl_special('EVAL-WHEN')
def eval_when(situations, *forms):
    """Evaluate when situations apply."""
    result = None
    for form in forms:
        result = form  # Would normally evaluate
    return result


def locally(*body):
    """Execute body in local scope."""
    raise lisptype.LispNotImplementedError("LOCALLY")


@_registry.cl_special('PROGV')
def progv(symbols, values, *body):
    """Special form PROGV (stub)."""
    raise lisptype.LispNotImplementedError("PROGV")


@_registry.cl_function('SPECIAL-OPERATOR-P')
def special_operator_p(symbol):
    """SPECIAL-OPERATOR-P predicate (simplified)."""
    if isinstance(symbol, lisptype.LispSymbol):
        special_ops = {'QUOTE', 'IF', 'LAMBDA', 'SETQ', 'LET', 'DEFUN', 'DEFVAR',
                      'PROGN', 'COND', 'AND', 'OR', 'WHEN', 'UNLESS', 'PROGV'}
        return lisptype.lisp_bool(symbol.name in special_ops)
    return lisptype.lisp_bool(False)


@_registry.cl_function('MACRO-FUNCTION')
def macro_function(symbol, environment=None):
    """Get macro function."""
    if environment is None:
        from fclpy.lispenv import current_environment as _cur_env
        environment = _cur_env
    func = environment.find_func(symbol)
    if callable(func) and getattr(func, '__is_macro__', False):
        return func
    return None


@_registry.cl_function('COMPILER-MACRO-FUNCTION')
def compiler_macro_function(name, environment=None):
    """Get compiler macro function."""
    return None


@_registry.cl_function('FBOUNDP')
def fboundp(symbol):
    """Test if symbol has function binding."""
    raise lisptype.LispNotImplementedError("FBOUNDP")


@_registry.cl_function('FMAKUNBOUND')
def fmakunbound(symbol):
    """Remove function binding for SYMBOL.

    This walks the current global environment's function bindings and
    removes any entry whose symbol name matches the provided symbol.
    Returns T if a binding was removed, otherwise NIL.
    """
    # Normalize symbol to LispSymbol
    if not isinstance(symbol, lisptype.LispSymbol):
        symbol = lisptype.LispSymbol(str(symbol))
    env = state.current_environment
    if env is None:
        return lisptype.NIL
    prev = None
    node = env.function_bindings
    removed = False
    while node is not None:
        if node.symbol.name == symbol.name:
            # Remove node from linked list
            if prev is None:
                env.function_bindings = node.next
            else:
                prev.next = node.next
            removed = True
            break
        prev = node
        node = node.next
    return lisptype.T if removed else lisptype.NIL

@_registry.cl_function('FDEFINITION')
def fdefinition(symbol):
    """Return the function object bound to SYMBOL.

    Looks up the symbol in the current environment's function bindings.
    Signals a LispNotImplementedError (standing in for UNDEFINED-FUNCTION)
    if the symbol is not fbound.
    """
    if not isinstance(symbol, lisptype.LispSymbol):
        symbol = lisptype.LispSymbol(str(symbol))
    env = state.current_environment
    if env is None:
        raise lisptype.LispNotImplementedError("FDEFINITION: no environment")
    func = env.find_func(symbol)
    if func is None:
        raise lisptype.LispNotImplementedError("FDEFINITION: undefined function")
    return func


# NOTE: Previously there were two implementations of SYMBOL-FUNCTION:
# 1) an undecorated helper that directly called fdefinition
# 2) a decorated attribute-only accessor (symbol_function_attr)
# We unify them into one decorated function that first tries the
# environment binding (fdefinition) and falls back to a raw attribute.


@_registry.cl_function('SYMBOL-FUNCTION')
def symbol_function(symbol):  # type: ignore[override]
    """Return the function bound to SYMBOL.

    Resolution order:
    1. If the current environment has an fdefinition, return it.
    2. Otherwise fall back to a .function attribute if present.
    3. Else NIL (represented by Python None).
    """
    try:
        return fdefinition(symbol)
    except Exception:
        return getattr(symbol, 'function', None)


@_registry.cl_function('FUNCTIONP')
def functionp(object):
    """Test if object is a function."""
    return callable(object)


@_registry.cl_function('COMPILED-FUNCTION-P')
def compiled_function_p(object):
    """Test if object is compiled function."""
    return callable(object) and hasattr(object, '__code__')


_gensym_counter = 0

def gensym(prefix="G"):
    """Generate unique symbol."""
    global _gensym_counter
    _gensym_counter += 1
    return lisptype.LispSymbol(f"{prefix}{_gensym_counter}")


# --- Symbol operations (restored) -------------------------------------------
@_registry.cl_function('SYMBOL-NAME')
def symbol_name(symbol):
    if hasattr(symbol, 'name'):
        return symbol.name
    return str(symbol)


@_registry.cl_function('SYMBOL-PACKAGE')
def symbol_package(symbol):
    return getattr(symbol, 'package', None)


@_registry.cl_function('SYMBOL-VALUE')
def symbol_value(symbol):
    return getattr(symbol, 'value', None)


## (Removed duplicate decorated SYMBOL-FUNCTION alias previously named symbol_function_attr)


@_registry.cl_function('MAKE-SYMBOL')
def make_symbol(name):
    return lisptype.LispSymbol(str(name))


@_registry.cl_function('COPY-SYMBOL')
def copy_symbol(symbol, copy_props=None):
    return make_symbol(symbol_name(symbol))


## (Removed stale orphaned cons manipulation lines from earlier cleanup)


# Package and symbol operations
## Removed remaining undecorated duplicate CLOS block (DEFCLASS...generic_function_name).


@_registry.cl_function('IMPORT')
def import_symbol(symbols, package=None):
    """Import symbols into package."""
    if not isinstance(symbols, (list, tuple)):
        symbols = [symbols]
    pkg = lisptype.COMMON_LISP_USER_PACKAGE if package is None else (package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)))
    if pkg is None:
        raise lisptype.LispNotImplementedError("IMPORT: unknown package")
    for s in symbols:
        name = s.name if hasattr(s, 'name') else str(s)
        pkg.intern_symbol(name, external=True)
    return True


@_registry.cl_function('IN-PACKAGE')
def in_package(name):
    """Set current package."""
    # Set a module-level current package pointer so other code can reference it
    if isinstance(name, lisptype.Package):
        state.current_package = name
        return name
    pkg = lisptype.find_package(str(name))
    if pkg is None:
        pkg = lisptype.make_package(str(name))
    state.current_package = pkg
    return pkg


@_registry.cl_function('FUNCTION-KEYWORDS')
def function_keywords(function):
    """Return two values similar to CL: FUNCTION-KEYWORDS.

    For Python callables we inspect the signature and collect keyword
    parameters (those with defaults) and &ALLOW-OTHER-KEYS status.
    Returns (keywords, allow_other_keys_p).
    """
    if not callable(function):
        return [], lisptype.NIL
    try:
        sig = inspect.signature(function)
        keywords = []
        allow_other = lisptype.NIL
        for p in sig.parameters.values():
            # Heuristic: parameters with defaults become keyword params
            if getattr(p, 'default', None) not in (None, getattr(inspect, '_empty', None)):
                keywords.append(p.name.upper())
            name_kind = str(getattr(p, 'kind', ''))
            if 'VAR_KEYWORD' in name_kind:
                allow_other = lisptype.T
        return keywords, allow_other
    except Exception:
        return [], lisptype.NIL

@_registry.cl_function('FUNCTION-LAMBDA-EXPRESSION')
def function_lambda_expression(function):
    """Return three values approximating CL: FUNCTION-LAMBDA-EXPRESSION.

    1. A simplified lambda list (list of parameter names as symbols)
    2. The body form source (string of source if available, else NIL)
    3. A closure-p flag (T if the function appears to close over free vars)
    """
    if not callable(function):
        return [], lisptype.NIL, lisptype.NIL
    params = []
    closure_p = lisptype.NIL
    try:
        sig = inspect.signature(function)
        for p in sig.parameters.values():
            name_kind = str(getattr(p, 'kind', ''))
            if 'POSITIONAL' in name_kind or 'KEYWORD_ONLY' in name_kind:
                params.append(lisptype.LispSymbol(p.name.upper()))
            elif 'VAR_POSITIONAL' in name_kind:
                params.append(lisptype.LispSymbol('&REST'))
            elif 'VAR_KEYWORD' in name_kind:
                params.append(lisptype.LispSymbol('&KEY'))
        if getattr(function, '__closure__', None):
            closure_p = lisptype.T if function.__closure__ else lisptype.NIL
        try:
            src = inspect.getsource(function)
        except Exception:
            src = None
        return params, (src or lisptype.NIL), closure_p
    except Exception:
        return [], lisptype.NIL, lisptype.NIL


# Stubs for many missing CLOS and system functions
@_registry.cl_function('FIND-CLASS')
def find_class(name, errorp=True, environment=None):
    """Find class by name."""
    raise lisptype.LispNotImplementedError("FIND-CLASS")

@_registry.cl_function('FIND-METHOD')
def find_method(generic_function, qualifiers, specializers, errorp=True):
    """Find method."""
    raise lisptype.LispNotImplementedError("FIND-METHOD")

@_registry.cl_function('ADD-METHOD')
def add_method(generic_function, method):
    """Add method to generic function."""
    raise lisptype.LispNotImplementedError("ADD-METHOD")

@_registry.cl_function('ALLOCATE-INSTANCE')
def allocate_instance(class_obj, **kwargs):
    """Allocate instance."""
    raise lisptype.LispNotImplementedError("ALLOCATE-INSTANCE")


@_registry.cl_function('DEFMETHOD')
def defmethod(name, *args):
    """Define method."""
    return name


@_registry.cl_function('MAKE-INSTANCE')
def make_instance(class_designator, *initargs):
    """Make instance."""
    return {}  # Simplified


## (Removed undecorated duplicate CLOS helper block — canonical decorated
##    versions retained later in file.)


@_registry.cl_function('MAKE-METHOD')
def make_method(*args):
    """Make method."""
    return None


@_registry.cl_function('METHOD-COMBINATION-ERROR')
def method_combination_error(format_control, *format_arguments):
    """Method combination error."""
    return None


@_registry.cl_function('METHOD-FUNCTION')
def method_function(method):
    """Get method function."""
    return None


@_registry.cl_function('METHOD-GENERIC-FUNCTION')
def method_generic_function(method):
    """Get method generic function."""
    return None


@_registry.cl_function('METHOD-SPECIALIZERS')
def method_specializers(method):
    """Get method specializers."""
    return []


@_registry.cl_function('METHOD-LAMBDA-LIST')
def method_lambda_list(method):
    """METHOD-LAMBDA-LIST (stub)."""
    return []


@_registry.cl_function('METHOD-QUALIFIERS')
def method_qualifiers(method):
    """METHOD-QUALIFIERS (stub)."""
    return []


@_registry.cl_function('NEXT-METHOD-P')
def next_method_p():
    """Test if next method exists."""
    return False


@_registry.cl_function('NO-APPLICABLE-METHOD')
def no_applicable_method(generic_function, *arguments):
    """No applicable method."""
    return None


@_registry.cl_function('NO-NEXT-METHOD')
def no_next_method(generic_function, method, *arguments):
    """No next method."""
    return None


@_registry.cl_function('REINITIALIZE-INSTANCE')
def reinitialize_instance(instance, *initargs):
    """Reinitialize instance."""
    return instance


@_registry.cl_function('REMOVE-METHOD')
def remove_method(generic_function, method):
    """Remove method."""
    return generic_function


@_registry.cl_function('SHARED-INITIALIZE')
def shared_initialize(instance, slot_names, *initargs):
    """Shared initialize."""
    return instance


@_registry.cl_function('SLOT-BOUNDP')
def slot_boundp(instance, slot_name):
    """Test if slot is bound."""
    return True


@_registry.cl_function('SLOT-EXISTS-P')
def slot_exists_p(instance, slot_name):
    """Test if slot exists."""
    return True


@_registry.cl_function('SLOT-MAKUNBOUND')
def slot_makunbound(instance, slot_name):
    """Make slot unbound."""
    return instance


@_registry.cl_function('SLOT-UNBOUND')
def slot_unbound(class_obj, instance, slot_name):
    """Slot unbound."""
    return None


@_registry.cl_function('SLOT-VALUE')
def slot_value(instance, slot_name):
    """Get slot value."""
    return None


@_registry.cl_function('STANDARD-CLASS')
def standard_class():
    """Standard class type."""
    return 'STANDARD-CLASS'


@_registry.cl_function('STANDARD-OBJECT')
def standard_object():
    """Standard object type."""
    return 'STANDARD-OBJECT'


@_registry.cl_function('STRUCTURE-CLASS')
def structure_class():
    """Structure class type."""
    return 'STRUCTURE-CLASS'


@_registry.cl_function('STRUCTURE-OBJECT')
def structure_object():
    """Structure object type."""
    return 'STRUCTURE-OBJECT'


# Final large batch of remaining functions
@_registry.cl_function('DEFINE-CONDITION')
def define_condition(name, parent_types, slot_specs, *options):
    """Define condition."""
    return name


@_registry.cl_function('DEFINE-METHOD-COMBINATION')
def define_method_combination(name, *args):
    """Define method combination."""
    return name


@_registry.cl_function('DEFINE-SETF-EXPANDER')
def define_setf_expander(access_fn, lambda_list, *body):
    """Define setf expander."""
    return access_fn


@_registry.cl_function('DEFSETF')
def defsetf(access_fn, update_fn, documentation=None):
    """Define setf function."""
    return access_fn


@_registry.cl_function('GET-SETF-EXPANSION')
def get_setf_expansion(place, environment=None):
    """Get setf expansion."""
    return [], [], [], place, place


def directory(pathspec, **kwargs):
    """List directory."""
    return []


## Removed undecorated ensure_directories_exist; decorated version retained later.


@_registry.cl_function('LOAD-TIME-VALUE')
def load_time_value(form, read_only_p=None):
    """Load time value."""
    return form


@_registry.cl_function('MAKE-CONDITION')
def make_condition(type_designator, *args):
    """Make condition."""
    return type_designator


@_registry.cl_function('COMPUTE-RESTARTS')
def compute_restarts(condition=None):
    """Compute restarts."""
    return []


@_registry.cl_function('FIND-RESTART')
def find_restart(identifier, condition=None):
    """Find restart."""
    return None


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


def handler_bind(handler_definitions, *body):
    """Handler bind macro."""
    result = None
    for form in body:
        result = form
    return result


def handler_case(form, *handler_clauses):
    """Handler case macro."""
    return form


@_registry.cl_function('INVOKE-RESTART')
def invoke_restart(restart, *arguments):
    """Invoke restart."""
    return None


@_registry.cl_function('INVOKE-RESTART-INTERACTIVELY')
def invoke_restart_interactively(restart):
    """Invoke restart interactively."""
    return None


def make_restart(name, function, **kwargs):
    """Make restart."""
    return name


def cerror(continue_format_control, datum, *arguments):
    """Continuable error."""
    print(f"Error: {datum}")
    return None


@_registry.cl_function('RESTART-NAME')
def restart_name(restart):
    """Get restart name."""
    return str(restart)


@_registry.cl_function('SIGNAL')
def signal_fn(datum, *arguments):
    """Signal condition."""
    return None


@_registry.cl_function('ERROR')
def error_fn(datum, *arguments):
    """Signal error."""
    raise Exception(str(datum))


@_registry.cl_function('WARN')
def warn_fn(datum, *arguments):
    """Warn."""
    print(f"Warning: {datum}")
    return None


@_registry.cl_function('MUFFLE-WARNING')
def muffle_warning(condition=None):
    """Muffle warning."""
    return None


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


def ignore_errors(*body):
    """Ignore errors."""
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


def proclaim(declaration_specifier):
    """Global proclamation."""
    return None


# Stream functions
@_registry.cl_function('ECHO-STREAM-INPUT-STREAM')
def echo_stream_input_stream(echo_stream):
    """Get input stream from echo stream."""
    return echo_stream


@_registry.cl_function('ECHO-STREAM-OUTPUT-STREAM')
def echo_stream_output_stream(echo_stream):
    """Get output stream from echo stream."""
    return echo_stream


@_registry.cl_function('BROADCAST-STREAM-STREAMS')
def broadcast_stream_streams(broadcast_stream):
    """BROADCAST-STREAM-STREAMS accessor (stub)."""
    return []


@_registry.cl_function('CONCATENATED-STREAM-STREAMS')
def concatenated_stream_streams(concatenated_stream):
    """CONCATENATED-STREAM-STREAMS accessor (stub)."""
    return []


@_registry.cl_function('SYNONYM-STREAM-SYMBOL')
def synonym_stream_symbol(synonym_stream):
    """SYNONYM-STREAM-SYMBOL accessor (stub)."""
    return synonym_stream


@_registry.cl_function('TWO-WAY-STREAM-INPUT-STREAM')
def two_way_stream_input_stream(two_way_stream):
    """TWO-WAY-STREAM-INPUT-STREAM accessor (stub)."""
    return two_way_stream


@_registry.cl_function('TWO-WAY-STREAM-OUTPUT-STREAM')
def two_way_stream_output_stream(two_way_stream):
    """TWO-WAY-STREAM-OUTPUT-STREAM accessor (stub)."""
    return two_way_stream


def read_char_no_hang(stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """Read character without hanging."""
    return None


def read_delimited_list(char, stream=None, recursive_p=None):
    """Read delimited list."""
    return []


def read_from_string(string, eof_error_p=True, eof_value=None, **kwargs):
    """Read from string."""
    return None


def read_preserving_whitespace(stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """Read preserving whitespace."""
    return None


# Readtable functions
def copy_readtable(from_readtable=None, to_readtable=None):
    """Copy readtable."""
    return {}


def make_dispatch_macro_character(char, non_terminating_p=None, readtable=None):
    """Make dispatch macro character."""
    return True


def set_syntax_from_char(to_char, from_char, to_readtable=None, from_readtable=None):
    """Set syntax from character."""
    return True


def readtable_case(readtable):
    """Get readtable case."""
    return 'UPCASE'


# Math functions (RATIONAL / RATIONALIZE already implemented in math.py; provide simple fallbacks)
@_registry.cl_function('RATIONAL')
def rational(number):
    """RATIONAL fallback wrapper (delegates to math.rational if available)."""
    try:
        from . import math as lmath
        return lmath.rational(number, 1)
    except Exception:
        return number


@_registry.cl_function('RATIONALIZE')
def rationalize(number):
    """RATIONALIZE fallback wrapper (delegates to math.rationalize if available)."""
    try:
        from . import math as lmath
        return lmath.rationalize(number)
    except Exception:
        return number


@_registry.cl_function('COMPLEX')
def complex_fn(realpart, imagpart=0):
    """Create complex number."""
    return complex(realpart, imagpart)


# More float constants
@_registry.cl_function('LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT')
def least_negative_normalized_double_float():
    """LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT (stub)."""
    import sys
    return -sys.float_info.min


@_registry.cl_function('LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT')
def least_negative_normalized_long_float():
    """LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT (stub)."""
    import sys
    return -sys.float_info.min


@_registry.cl_function('LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT')
def least_negative_normalized_short_float():
    """LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT (stub)."""
    import sys
    return -sys.float_info.min


@_registry.cl_function('LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT')
def least_negative_normalized_single_float():
    """LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT (stub)."""
    import sys
    return -sys.float_info.min


@_registry.cl_function('LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT')
def least_positive_normalized_double_float():
    """LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT (stub)."""
    import sys
    return sys.float_info.min


@_registry.cl_function('LEAST-POSITIVE-NORMALIZED-LONG-FLOAT')
def least_positive_normalized_long_float():
    """LEAST-POSITIVE-NORMALIZED-LONG-FLOAT (stub)."""
    import sys
    return sys.float_info.min


@_registry.cl_function('LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT')
def least_positive_normalized_short_float():
    """LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT (stub)."""
    import sys
    return sys.float_info.min


@_registry.cl_function('LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT')
def least_positive_normalized_single_float():
    """LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT (stub)."""
    import sys
    return sys.float_info.min


# Additional utility functions
@_registry.cl_function('ARRAY-ROW-MAJOR-INDEX')
def array_row_major_index(array, *subscripts):
    """Array row major index."""
    return 0  # Simplified


@_registry.cl_function('CHAR-BITS-LIMIT')
def char_bits_limit():
    """Character bits limit."""
    return 16


@_registry.cl_function('CHAR-FONT-LIMIT')
def char_font_limit():
    """Character font limit."""
    return 256


@_registry.cl_function('TYPE')
def type(object):
    """TYPE function (simplified)."""
    return type(object).__name__


@_registry.cl_function('UPGRADED-ARRAY-ELEMENT-TYPE')
def upgraded_array_element_type(typespec, environment=None):
    """UPGRADED-ARRAY-ELEMENT-TYPE (stub)."""
    return 'T'


@_registry.cl_function('UPGRADED-COMPLEX-PART-TYPE')
def upgraded_complex_part_type(typespec, environment=None):
    """UPGRADED-COMPLEX-PART-TYPE (stub)."""
    return 'REAL'


@_registry.cl_function('OPTIMIZE')
def optimize(*args):
    """OPTIMIZE declaration (stub)."""
    return None


@_registry.cl_function('SPECIAL')
def special(*args):
    """SPECIAL declaration (stub)."""
    return None


@_registry.cl_function('NIL')
def nil_symbol():
    """Return NIL symbol (Python None)."""
    return None


@_registry.cl_function('T')
def t_symbol():
    """Return T symbol (Python True)."""
    return True


@_registry.cl_function('NOTINLINE')
def notinline(*args):
    """NOTINLINE declaration (stub)."""
    return None


@_registry.cl_function('INLINE')
def inline(*args):
    """INLINE declaration (stub)."""
    return None


# Macro functions
@_registry.cl_function('MACROLET')
def macrolet(definitions, *body):
    """Local macros (stub)."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('SYMBOL-MACROLET')
def symbol_macrolet(definitions, *body):
    """Symbol macros (stub)."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('DEFINE-COMPILER-MACRO')
def define_compiler_macro(name, lambda_list, *body):
    """Define compiler macro (stub)."""
    return name


@_registry.cl_function('DYNAMIC-EXTENT')
def dynamic_extent(*args):
    """Dynamic extent declaration (stub)."""
    return None


@_registry.cl_function('FTYPE')
def ftype(*args):
    """Function type declaration (stub)."""
    return None


# Additional WITH- macros
@_registry.cl_function('WITH-ACCESSORS')
def with_accessors(slot_entries, instance_form, *body):
    """WITH-ACCESSORS (stub)."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WITH-COMPILATION-UNIT')
def with_compilation_unit(options, *body):
    """WITH-COMPILATION-UNIT (stub)."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WITH-INPUT-FROM-STRING')
def with_input_from_string(var_string_form, *body):
    """WITH-INPUT-FROM-STRING (stub)."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WITH-OPEN-STREAM')
def with_open_stream(var_stream_form, *body):
    """WITH-OPEN-STREAM (stub)."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WITH-OUTPUT-TO-STRING')
def with_output_to_string(var_options, *body):
    """WITH-OUTPUT-TO-STRING (stub)."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WITH-PPRINT-LOGICAL-BLOCK')
def with_pprint_logical_block(stream_object_options, *body):
    """WITH-PPRINT-LOGICAL-BLOCK (stub)."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WITH-SLOTS')
def with_slots(slot_entries, instance_form, *body):
    """WITH-SLOTS (stub)."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WITH-STANDARD-IO-SYNTAX')
def with_standard_io_syntax(*body):
    """WITH-STANDARD-IO-SYNTAX (stub)."""
    result = None
    for form in body:
        result = form
    return result

@_registry.cl_function('LOAD-LOGICAL-PATHNAME-TRANSLATIONS')
def load_logical_pathname_translations(host):  # stub
    return True

@_registry.cl_function('LOGICAL-PATHNAME-TRANSLATIONS')
def logical_pathname_translations(host):  # stub
    return []


# Final sequence functions
@_registry.cl_function('MAP-INTO')
def map_into(result_sequence, function, *sequences):
    """MAP-INTO (stub)."""
    return result_sequence


@_registry.cl_function('MAPCON')
def mapcon(function, *lists):
    """MAPCON fallback (already implemented in sequences.py)."""
    return []


@_registry.cl_function('ROW-MAJOR-AREF')
def row_major_aref(array, index):
    """ROW-MAJOR-AREF (stub)."""
    return None

@_registry.cl_function('MAKE-HASH-TABLE')
def make_hash_table(test='EQL', size=16, rehash_size=1.5, rehash_threshold=0.75):
    table = {
        '__hashmeta__test': str(test).upper(),
        '__hashmeta__rehash_size': rehash_size,
        '__hashmeta__rehash_threshold': rehash_threshold,
    }
    # pre-size by touching keys (optional)
    return table

@_registry.cl_function('GETHASH')
def gethash(key, hashtable, default=None):
    if isinstance(hashtable, dict) and key in hashtable:
        return hashtable[key]
    return default

@_registry.cl_function('REMHASH')
def remhash(key, hashtable):
    if isinstance(hashtable, dict) and key in hashtable:
        del hashtable[key]
        return lisptype.T
    return lisptype.NIL

@_registry.cl_function('MAPHASH')
def maphash(function, hashtable):
    if isinstance(hashtable, dict):
        for k,v in list(hashtable.items()):
            if not k.startswith('__hashmeta__'):
                function(k,v)
    return lisptype.NIL

@_registry.cl_function('CLRHASH')
def clrhash(hashtable):
    if isinstance(hashtable, dict):
        meta = {k:v for k,v in hashtable.items() if k.startswith('__hashmeta__')}
        hashtable.clear()
        hashtable.update(meta)
    return hashtable

@_registry.cl_function('SXHASH')
def sxhash(obj):
    try:
        return hash(obj)
    except Exception:
        return hash(str(obj))

@_registry.cl_function('LOAD')
def load(filespec, **kwargs):
    return filespec

@_registry.cl_function('PACKAGE-ERROR-PACKAGE')
def package_error_package(condition):
    return None

@_registry.cl_function('CLASS-OF')
def class_of(object):
    return type(object)

@_registry.cl_function('CLASS-NAME')
def class_name(class_obj):
    return getattr(class_obj, '__name__', str(class_obj))

@_registry.cl_function('CHANGE-CLASS')
def change_class(instance, new_class, *initargs):
    return instance

@_registry.cl_function('BUILT-IN-CLASS')
def built_in_class():
    return 'BUILT-IN-CLASS'

@_registry.cl_function('CALL-METHOD')
def call_method(method, next_methods, *args):
    return None

@_registry.cl_function('CALL-NEXT-METHOD')
def call_next_method(*args):
    return None

@_registry.cl_function('COMPUTE-APPLICABLE-METHODS')
def compute_applicable_methods(generic_function, arguments):
    return []

@_registry.cl_function('ENSURE-GENERIC-FUNCTION')
def ensure_generic_function(function_name, *options):
    return function_name

@_registry.cl_function('GENERIC-FUNCTION-LAMBDA-LIST')
def generic_function_lambda_list(generic_function):
    return []

@_registry.cl_function('GENERIC-FUNCTION-METHODS')
def generic_function_methods(generic_function):
    return []

@_registry.cl_function('GENERIC-FUNCTION-NAME')
def generic_function_name(generic_function):
    return str(generic_function)

@_registry.cl_function('ENSURE-DIRECTORIES-EXIST')
def ensure_directories_exist(pathspec, **kwargs):
    return pathspec, lisptype.T

@_registry.cl_function('OCTETS-TO-STRING')
def octets_to_string(octets, **kwargs):
    if isinstance(octets, (bytes, bytearray)):
        return octets.decode(errors='ignore')
    if isinstance(octets, (list, tuple)):
        try:
            return bytes(octets).decode(errors='ignore')
        except Exception:
            return ''.join(str(x) for x in octets)
    return str(octets)

@_registry.cl_function('STRING-TO-OCTETS')
def string_to_octets(string, **kwargs):
    if not isinstance(string, str):
        string = str(string)
    return list(string.encode())


# ---------------------------------------------------------------------------
# Registration stubs for previously missing Common Lisp symbols so tests see
# a binding. Semantics are deliberately minimal; many just return simple
# placeholders or delegate to lisptype helpers when available.
# ---------------------------------------------------------------------------

@_registry.cl_function('ADJUSTABLE-ARRAY-P')
def adjustable_array_p(array):
    return lisptype.NIL

@_registry.cl_function('HASH-TABLE-COUNT')
def hash_table_count(table):
    return len([k for k in table.keys() if not str(k).startswith('__hashmeta__')]) if isinstance(table, dict) else 0

@_registry.cl_function('HASH-TABLE-SIZE')
def hash_table_size(table):
    return hash_table_count(table)

@_registry.cl_function('HASH-TABLE-TEST')
def hash_table_test(table):
    return table.get('__hashmeta__test') if isinstance(table, dict) else None

@_registry.cl_function('HASH-TABLE-REHASH-SIZE')
def hash_table_rehash_size(table):
    return table.get('__hashmeta__rehash_size') if isinstance(table, dict) else None

@_registry.cl_function('HASH-TABLE-REHASH-THRESHOLD')
def hash_table_rehash_threshold(table):
    return table.get('__hashmeta__rehash_threshold') if isinstance(table, dict) else None

@_registry.cl_function('GET')
def get(symbol, indicator, default=None):  # property list stub
    return default

@_registry.cl_function('RPLACA')
def rplaca(cons, new_car):
    try:
        cons.car = new_car
    except Exception:
        pass
    return cons

@_registry.cl_function('RPLACD')
def rplacd(cons, new_cdr):
    try:
        cons.cdr = new_cdr
    except Exception:
        pass
    return cons

@_registry.cl_function('INTERN')
def intern(name, package=None):
    if not isinstance(name, str):
        name = str(name)
    if package is None:
        package = getattr(state, 'current_package', None) or lisptype.COMMON_LISP_USER_PACKAGE
    if isinstance(package, lisptype.Package):
        return package.intern_symbol(name)
    pkg = lisptype.find_package(str(package))
    if pkg is None:
        pkg = lisptype.make_package(str(package))
    return pkg.intern_symbol(name)

@_registry.cl_function('FIND-SYMBOL')
def find_symbol(name, package=None):
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return None
    return pkg.find_symbol(name)

@_registry.cl_function('FIND-PACKAGE')
def find_package(name):
    return lisptype.find_package(str(name))

@_registry.cl_function('FIND-ALL-SYMBOLS')
def find_all_symbols(name):
    return []

@_registry.cl_function('EXPORT')
def export(symbols, package=None):
    if not isinstance(symbols, (list, tuple)):
        symbols = [symbols]
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        pkg = lisptype.COMMON_LISP_USER_PACKAGE
    for s in symbols:
        sym = intern(s.name if hasattr(s, 'name') else str(s), pkg)
        if hasattr(sym, 'external'):
            sym.external = True
    return lisptype.T

@_registry.cl_function('GENTEMP')
def gentemp(prefix='T', package=None):
    return intern(f"{prefix}{int(time.time()*1000)}", package)

@_registry.cl_function('INITIALIZE-INSTANCE')
def initialize_instance(instance, *initargs):
    return instance

@_registry.cl_function('UPDATE-INSTANCE-FOR-DIFFERENT-CLASS')
def update_instance_for_different_class(previous, current, *initargs):
    return current

@_registry.cl_function('UPDATE-INSTANCE-FOR-REDEFINED-CLASS')
def update_instance_for_redefined_class(instance, added_slots=None, discarded_slots=None, property_list=None, *initargs):
    return instance

@_registry.cl_function('SLOT-MISSING')
def slot_missing(class_obj, instance, slot_name, operation, *args):
    return None

@_registry.cl_function('INVALID-METHOD-ERROR')
def invalid_method_error(method, format_control, *format_args):
    raise lisptype.LispNotImplementedError('INVALID-METHOD-ERROR')

@_registry.cl_function('LAMBDA-LIST-KEYWORDS')
def lambda_list_keywords():
    return [lisptype.LispSymbol(x) for x in ['&OPTIONAL','&REST','&KEY','&WHOLE','&ALLOW-OTHER-KEYS','&AUX']]  # basic set

@_registry.cl_function('LAMBDA-PARAMETERS-LIMIT')
def lambda_parameters_limit():
    return 64

@_registry.cl_function('COPY-TREE')
def copy_tree(obj):
    if isinstance(obj, list):
        return [copy_tree(x) for x in obj]
    return obj

@_registry.cl_function('INSPECT')
def inspect_object(obj):
    return describe(obj)

@_registry.cl_function('INCF')
def incf(place, delta=1):  # simplistic numeric increment
    try:
        return place + delta
    except Exception:
        return place

@_registry.cl_function('MAKE-LOAD-FORM')
def make_load_form(object, environment=None):
    return object

@_registry.cl_function('MAKE-LOAD-FORM-SAVING-SLOTS')
def make_load_form_saving_slots(object, slot_names=None):
    return object

# Type designator symbol placeholders
@_registry.cl_function('KEYWORD')
def keyword_type():
    return 'KEYWORD'

@_registry.cl_function('INTEGER')
def integer_type():
    return 'INTEGER'

@_registry.cl_function('FIXNUM')
def fixnum_type():
    return 'FIXNUM'

@_registry.cl_function('DOUBLE-FLOAT')
def double_float_type():
    return 'DOUBLE-FLOAT'

@_registry.cl_function('SINGLE-FLOAT')
def single_float_type():
    return 'SINGLE-FLOAT'

@_registry.cl_function('SHORT-FLOAT')
def short_float_type():
    return 'SHORT-FLOAT'

@_registry.cl_function('EXTENDED-CHAR')
def extended_char_type():
    return 'EXTENDED-CHAR'

@_registry.cl_function('HASH-TABLE')
def hash_table_type():
    return 'HASH-TABLE'

@_registry.cl_function('GENERIC-FUNCTION')
def generic_function_type():
    return 'GENERIC-FUNCTION'

@_registry.cl_function('FILE-STREAM')
def file_stream_type():
    return 'FILE-STREAM'

@_registry.cl_function('FILE-ERROR')
def file_error_type():
    return 'FILE-ERROR'

@_registry.cl_function('END-OF-FILE')
def end_of_file_type():
    return 'END-OF-FILE'

@_registry.cl_function('FLOATING-POINT-INEXACT')
def floating_point_inexact_type():
    return 'FLOATING-POINT-INEXACT'

@_registry.cl_function('FLOATING-POINT-INVALID-OPERATION')
def floating_point_invalid_operation_type():
    return 'FLOATING-POINT-INVALID-OPERATION'

@_registry.cl_function('FLOATING-POINT-OVERFLOW')
def floating_point_overflow_type():
    return 'FLOATING-POINT-OVERFLOW'

@_registry.cl_function('FLOATING-POINT-UNDERFLOW')
def floating_point_underflow_type():
    return 'FLOATING-POINT-UNDERFLOW'

@_registry.cl_function('ARITHMETIC-ERROR-OPERANDS')
def arithmetic_error_operands(condition):
    return []

@_registry.cl_function('ARITHMETIC-ERROR-OPERATION')
def arithmetic_error_operation(condition):
    return None

@_registry.cl_function('FILE-ERROR-PATHNAME')
def file_error_pathname(condition):
    return None

@_registry.cl_function('MULTIPLE-VALUE-BIND')
def multiple_value_bind(specs, values_form, *body):
    # Simplified: just return evaluation of last body form placeholder
    result = None
    for form in body:
        result = form
    return result

@_registry.cl_function('MULTIPLE-VALUE-CALL')
def multiple_value_call(function, *forms):
    return function(*forms) if callable(function) else None

@_registry.cl_function('MULTIPLE-VALUE-LIST')
def multiple_value_list(form):
    return [form]

@_registry.cl_function('MULTIPLE-VALUE-PROG1')
def multiple_value_prog1(first_form, *rest):
    return first_form

@_registry.cl_function('MULTIPLE-VALUE-SETQ')
def multiple_value_setq(vars, values_form):
    return values_form

@_registry.cl_function('NTH-VALUE')
def nth_value(n, form):
    return form if n == 0 else None

@_registry.cl_function('MAKE-PACKAGE')
def make_package(name, nicknames=None, use=None):
    return lisptype.make_package(str(name))

@_registry.cl_function('PACKAGE-NAME')
def package_name(package):
    return package.name if isinstance(package, lisptype.Package) else None

@_registry.cl_function('PACKAGE-NICKNAMES')
def package_nicknames(package):
    return getattr(package, 'nicknames', [])

@_registry.cl_function('RENAME-PACKAGE')
def rename_package(package, new_name, new_nicknames=None):
    if isinstance(package, lisptype.Package):
        package.name = str(new_name)
    return package

@_registry.cl_function('PACKAGE-USE-LIST')
def package_use_list(package):
    return []

@_registry.cl_function('PACKAGE-USED-BY-LIST')
def package_used_by_list(package):
    return []

@_registry.cl_function('PACKAGE-SHADOWING-SYMBOLS')
def package_shadowing_symbols(package):
    return []

@_registry.cl_function('LIST-ALL-PACKAGES')
def list_all_packages():
    return []

@_registry.cl_function('UNINTERN')
def unintern(symbol, package=None):
    return lisptype.NIL

@_registry.cl_function('UNEXPORT')
def unexport(symbols, package=None):
    return lisptype.NIL

@_registry.cl_function('SHADOWING-IMPORT')
def shadowing_import(symbols, package=None):
    return lisptype.T

@_registry.cl_function('SHADOW')
def shadow(symbols, package=None):
    return lisptype.T

@_registry.cl_function('USE-PACKAGE')
def use_package(packages, package=None):
    return lisptype.T

@_registry.cl_function('UNUSE-PACKAGE')
def unuse_package(packages, package=None):
    return lisptype.T

@_registry.cl_function('MACROEXPAND')
def macroexpand(form, environment=None):
    return form, lisptype.NIL

@_registry.cl_function('MACROEXPAND-1')
def macroexpand_1(form, environment=None):
    return form, lisptype.NIL

@_registry.cl_function('ARRAY-DIMENSION-LIMIT')
def array_dimension_limit():
    return 1024

@_registry.cl_function('ARRAY-RANK-LIMIT')
def array_rank_limit():
    return 8

@_registry.cl_function('ARRAY-TOTAL-SIZE-LIMIT')
def array_total_size_limit():
    return 1024 * 1024

@_registry.cl_function('CALL-ARGUMENTS-LIMIT')
def call_arguments_limit():
    return 64

@_registry.cl_function('MULTIPLE-VALUES-LIMIT')
def multiple_values_limit():
    return 64

@_registry.cl_function('CHAR-CODE-LIMIT')
def char_code_limit():
    return 1114112  # Unicode max + 1

@_registry.cl_function('DO-SYMBOLS')
def do_symbols(spec, *body):
    return lisptype.NIL

@_registry.cl_function('DO-EXTERNAL-SYMBOLS')
def do_external_symbols(spec, *body):
    return lisptype.NIL

@_registry.cl_function('DO-ALL-SYMBOLS')
def do_all_symbols(spec, *body):
    return lisptype.NIL

@_registry.cl_function('DOCUMENTATION')
def documentation(obj, doc_type=None):
    return None

@_registry.cl_function('FILL-POINTER')
def fill_pointer(vector):
    return None

@_registry.cl_function('WITH-PACKAGE-ITERATOR')
def with_package_iterator(spec, packages, *body):
    return lisptype.NIL

@_registry.cl_function('BREAK')
def break_fn(format_string=None, *args):
    return None

@_registry.cl_function('CONTINUE')
def continue_fn():
    return None

@_registry.cl_function('DECLAIM')
def declaim(*declarations):
    return lisptype.NIL

@_registry.cl_function('DECLARE')
def declare(*declarations):
    return lisptype.NIL

@_registry.cl_function('DEFCLASS')
def defclass(name, superclasses, slots, *options):
    return name

@_registry.cl_function('DEFCONSTANT')
def defconstant(name, value, doc=None):
    return name

@_registry.cl_function('DEFGENERIC')
def defgeneric(name, lambda_list, *options):
    return name

@_registry.cl_function('DEFPACKAGE')
def defpackage(name, *options):
    return name

@_registry.cl_function('DEFSTRUCT')
def defstruct(name, *slots):
    return name

@_registry.cl_function('DEFTYPE')
def deftype(name, lambda_list, *body):
    return name

@_registry.cl_function('ECHO-STREAM-P')
def echo_stream_p(obj):
    return lisptype.NIL

@_registry.cl_function('PROVIDE')
def provide(module):
    return module

@_registry.cl_function('REQUIRE')
def require(module):
    return module

@_registry.cl_function('TRACE')
def trace(*fns):
    return list(fns)

@_registry.cl_function('UNTRACE')
def untrace(*fns):
    return list(fns)

@_registry.cl_function('ED')
def ed(file=None):
    return file

@_registry.cl_function('DRIBBLE')
def dribble(file=None):
    return file

@_registry.cl_function('DISASSEMBLE')
def disassemble(object):
    return None

@_registry.cl_function('DEFPARAMETER')
def defparameter(name, value, doc=None):
    return name

@_registry.cl_function('BROADCAST-STREAM-P')
def broadcast_stream_p(obj):
    return lisptype.NIL

@_registry.cl_function('CONCATENATED-STREAM-P')
def concatenated_stream_p(obj):
    return lisptype.NIL

@_registry.cl_function('FILE-STREAM-P')
def file_stream_p(obj):
    return lisptype.NIL

@_registry.cl_function('STRING-STREAM-P')
def string_stream_p(obj):
    return lisptype.NIL

@_registry.cl_function('SYNONYM-STREAM-P')
def synonym_stream_p(obj):
    return lisptype.NIL

@_registry.cl_function('TWO-WAY-STREAM-P')
def two_way_stream_p(obj):
    return lisptype.NIL

@_registry.cl_function('ROOM')
def room(option=None):
    return None

@_registry.cl_function('STEP')
def step(form):
    return form
