"""Miscellaneous utility functions and system operations."""

import time
import inspect
import fclpy.lisptype as lisptype
import fclpy.state as state
from fclpy.lispfunc import registry as _registry


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


# NOTE: describe already provided elsewhere; retain single implementation
def describe(object, stream=None):  # keep simple description
    return str(object)


# --- Macro helper utilities (module level) ---------------------------------
def list_to_cons(pylist):
    """Convert a Python list of Lisp objects to a Lisp cons-list."""
    lst = lisptype.NIL
    for itm in reversed(pylist):
        lst = lisptype.lispCons(itm, lst)
    return lst


def parse_macro_lambda_list(lambda_list):
    """Parse a macro lambda-list into components.

    Supports a basic set of lambda-list keywords: &optional, &rest, &whole, &key.
    Returns a dict with keys: required, optional, rest, whole, keys, allow_other_keys.
    """
    from fclpy.lispfunc.core import _consp_internal, car, cdr

    mode = 'required'
    required = []
    optional = []  # list of (name, default)
    rest = None
    whole = None
    keys = []  # list of (name, default)
    allow_other_keys = False
    cur = lambda_list
    while _consp_internal(cur):
        item = car(cur)
        cur = cdr(cur)
        if isinstance(item, lisptype.LispSymbol):
            nm = item.name.upper()
            if nm == '&OPTIONAL':
                mode = 'optional'
                continue
            if nm == '&REST':
                # next element is rest var
                if _consp_internal(cur):
                    rest_item = car(cur)
                    cur = cdr(cur)
                    if isinstance(rest_item, lisptype.LispSymbol):
                        rest = rest_item.name
                continue
            if nm == '&WHOLE':
                if _consp_internal(cur):
                    whole_item = car(cur)
                    cur = cdr(cur)
                    if isinstance(whole_item, lisptype.LispSymbol):
                        whole = whole_item.name
                continue
            if nm == '&KEY':
                mode = 'key'
                continue
            if nm == '&ALLOW-OTHER-KEYS':
                allow_other_keys = True
                continue

        # process according to mode
        if mode == 'required':
            if isinstance(item, lisptype.LispSymbol):
                required.append(item.name)
        elif mode == 'optional':
            # optional element may be (name default) or symbol
            if _consp_internal(item):
                name = car(item)
                default = car(cdr(item)) if _consp_internal(cdr(item)) else None
                if isinstance(name, lisptype.LispSymbol):
                    optional.append((name.name, default))
            elif isinstance(item, lisptype.LispSymbol):
                optional.append((item.name, None))
        elif mode == 'key':
            if _consp_internal(item):
                name = car(item)
                default = car(cdr(item)) if _consp_internal(cdr(item)) else None
                if isinstance(name, lisptype.LispSymbol):
                    keys.append((name.name, default))
            elif isinstance(item, lisptype.LispSymbol):
                keys.append((item.name, None))

    return {
        'required': required,
        'optional': optional,
        'rest': rest,
        'whole': whole,
        'keys': keys,
        'allow_other_keys': allow_other_keys,
    }

# ---------------------------------------------------------------------------
@_registry.cl_function('ENCODE-UNIVERSAL-TIME')
def encode_universal_time(year, month, date, hour, minute, second):
    """Encode date/time to universal time (seconds since 1900-01-01)."""
    import datetime
    try:
        dt = datetime.datetime(year, month, date, hour, minute, second)
        epoch = datetime.datetime(1900, 1, 1)
        return int((dt - epoch).total_seconds())
    except ValueError:
        raise lisptype.LispNotImplementedError("ENCODE-UNIVERSAL-TIME: invalid date/time")


@_registry.cl_function('GET-UNIVERSAL-TIME')
def get_universal_time():
    """Return current universal time (seconds since 1900-01-01)."""
    import datetime
    epoch = datetime.datetime(1900, 1, 1)
    return int((datetime.datetime.utcnow() - epoch).total_seconds())


@_registry.cl_function('DECODE-UNIVERSAL-TIME')
def decode_universal_time(universal_time=None):
    """Decode universal time (seconds since 1900-01-01) into components.

    Returns a Python tuple: (second, minute, hour, day, month, year, zone).
    """
    import datetime
    if universal_time is None:
        universal_time = get_universal_time()
    epoch = datetime.datetime(1900, 1, 1)
    dt = epoch + datetime.timedelta(seconds=universal_time)
    return (dt.second, dt.minute, dt.hour, dt.day, dt.month, dt.year, None)


@_registry.cl_function('GET-DECODED-TIME')
def get_decoded_time():
    """Return decoded components for the current universal time."""
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


def symbol_function(symbol):
    """Get symbol's function."""
    return fdefinition(symbol)


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


@_registry.cl_function('GENTEMP')
def gentemp(prefix="T", package=None):
    """Generate temporary symbol."""
    count = 0
    while True:
        name = f"{prefix}{count}"
        symbol = lisptype.LispSymbol(name)
        # In a real implementation, would check if symbol exists in package
        return symbol


@_registry.cl_function('FILL-POINTER')
def fill_pointer(vector):
    """Return the fill pointer of a vector."""
    if hasattr(vector, '_fill_pointer'):
        return vector._fill_pointer
    else:
        raise TypeError("Vector does not have a fill pointer")


# Hash table operations
@_registry.cl_function('HASH-TABLE-COUNT')
def hash_table_count(hash_table):
    """Return number of entries in hash table."""
    if isinstance(hash_table, dict):
        return len(hash_table)
    return 0


@_registry.cl_function('HASH-TABLE-SIZE')
def hash_table_size(hash_table):
    """Return current size of hash table."""
    if isinstance(hash_table, dict):
        return len(hash_table)
    return 0


@_registry.cl_function('HASH-TABLE-TEST')
def hash_table_test(hash_table):
    """Return test function for hash table."""
    return 'EQ'  # Default test


@_registry.cl_function('HASH-TABLE-REHASH-SIZE')
def hash_table_rehash_size(hash_table):
    """Return rehash size of hash table."""
    return 1.5  # Default rehash size


@_registry.cl_function('HASH-TABLE-REHASH-THRESHOLD')
def hash_table_rehash_threshold(hash_table):
    """Return rehash threshold of hash table."""
    return 0.75  # Default threshold


# Property list operations
@_registry.cl_function('GET')
def get(symbol, indicator, default=None):
    """Get property from symbol's property list."""
    if hasattr(symbol, '_plist'):
        plist = symbol._plist
        for i in range(0, len(plist), 2):
            if i + 1 < len(plist) and plist[i] == indicator:
                return plist[i + 1]
    return default


@_registry.cl_function('RPLACA')
def rplaca(cons, new_car):
    """Replace car of cons cell."""
    if hasattr(cons, 'car'):
        cons.car = new_car
    return cons


@_registry.cl_function('RPLACD')
def rplacd(cons, new_cdr):
    """Replace cdr of cons cell."""
    if hasattr(cons, 'cdr'):
        cons.cdr = new_cdr
    return cons


# Package and symbol operations
@_registry.cl_function('INTERN')
def intern(string, package=None):
    """Intern symbol in package."""
    # Support keyword syntax ":FOO" -> intern into KEYWORD package
    name = string if isinstance(string, str) else str(string)
    if name.startswith(":"):
        name = name[1:]
        pkg = lisptype.KEYWORD_PACKAGE
        sym = pkg.intern_symbol(name, external=True)
        # Prefer LispKeyword for keyword package symbols
        kw = lisptype.lispKeyword(sym.name)
        kw.package = pkg
        pkg.symbols[sym.name] = kw
        return kw

    # Resolve package
    if package is None:
        pkg = lisptype.COMMON_LISP_USER_PACKAGE
    elif isinstance(package, lisptype.Package):
        pkg = package
    else:
        pkg = lisptype.find_package(str(package)) or lisptype.make_package(str(package))

    return pkg.intern_symbol(name, external=False)


@_registry.cl_function('FIND-SYMBOL')
def find_symbol(string, package=None):
    """Find symbol in package."""
    name = string if isinstance(string, str) else str(string)
    if package is None:
        # search all known packages
        results = []
        for p in set(state.packages.values()):
            sym, status = p.find_symbol(name)
            if sym is not None:
                results.append((sym, status))
        return results[0] if results else (None, None)
    if isinstance(package, lisptype.Package):
        return package.find_symbol(name)
    pkg = lisptype.find_package(str(package))
    if pkg is None:
        return None, None
    return pkg.find_symbol(name)


@_registry.cl_function('FIND-PACKAGE')
def find_package(name):
    """Find package by name."""
    if isinstance(name, lisptype.Package):
        return name
    return lisptype.find_package(str(name))


@_registry.cl_function('FIND-ALL-SYMBOLS')
def find_all_symbols(string):
    """Find all symbols with given name."""
    name = string if isinstance(string, str) else str(string)
    out = []
    for p in set(state.packages.values()):
        sym, status = p.find_symbol(name)
        if sym is not None:
            out.append(sym)
    return out


@_registry.cl_function('EXPORT')
def export_symbol(symbols, package=None):
    """Export symbols from package."""
    if not isinstance(symbols, (list, tuple)):
        symbols = [symbols]
    pkg = lisptype.COMMON_LISP_USER_PACKAGE if package is None else (package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)))
    if pkg is None:
        raise lisptype.LispNotImplementedError("EXPORT: unknown package")
    for s in symbols:
        name = s.name if hasattr(s, 'name') else str(s)
        pkg.intern_symbol(name, external=True)
        pkg.export_symbol(name)
    return True


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

@_registry.cl_function('INITIALIZE-INSTANCE')
def initialize_instance(instance, **kwargs):
    """Initialize instance."""
    raise lisptype.LispNotImplementedError("INITIALIZE-INSTANCE")

@_registry.cl_function('UPDATE-INSTANCE-FOR-DIFFERENT-CLASS')
def update_instance_for_different_class(instance, new_class):
    """Update instance for different class."""
    raise lisptype.LispNotImplementedError("UPDATE-INSTANCE-FOR-DIFFERENT-CLASS")

@_registry.cl_function('UPDATE-INSTANCE-FOR-REDEFINED-CLASS')
def update_instance_for_redefined_class(instance, added_slots, discarded_slots, property_list):
    """Update instance for redefined class."""
    raise lisptype.LispNotImplementedError("UPDATE-INSTANCE-FOR-REDEFINED-CLASS")

@_registry.cl_function('SLOT-MISSING')
def slot_missing(class_obj, instance, slot_name, operation, new_value=None):
    """Handle missing slot."""
    raise lisptype.LispNotImplementedError("SLOT-MISSING")

@_registry.cl_function('INVALID-METHOD-ERROR')
def invalid_method_error(method, format_control, *args):
    """Signal invalid method error."""
    raise lisptype.LispNotImplementedError("INVALID-METHOD-ERROR")

def deposit_field(newbyte, bytespec, integer):
    """Deposit field."""
    raise lisptype.LispNotImplementedError("DEPOSIT-FIELD")

def mask_field(bytespec, integer):
    """Mask field.""" 
    raise lisptype.LispNotImplementedError("MASK-FIELD")

# Constants
def most_positive_fixnum():
    """Most positive fixnum."""
    return 2**63 - 1

def most_negative_fixnum():
    """Most negative fixnum."""
    return -2**63

def most_negative_double_float():
    """Most negative double float."""
    return float('-inf')

def most_negative_long_float():
    """Most negative long float."""
    return float('-inf')

def most_negative_short_float():
    """Most negative short float."""
    return float('-inf')

@_registry.cl_function('LAMBDA-LIST-KEYWORDS')
def lambda_list_keywords():
    """Return implementation lambda list keyword symbols.

    Stub implementation returning a Python list of keyword strings.
    """
    return ['&optional', '&rest', '&key', '&allow-other-keys', '&aux']

@_registry.cl_function('LAMBDA-PARAMETERS-LIMIT')
def lambda_parameters_limit():
    """Return an arbitrary large implementation limit for lambda parameters."""
    return 65536

@_registry.cl_function('IDENTITY')
def identity(x):
    """Identity function (returns its single argument)."""
    return x

@_registry.cl_function('COPY-TREE')
def copy_tree(tree):
    """Return a (shallow) copy of a cons tree.

    Recursively copies cons cells (simple stub) leaving non-cons leaves intact.
    """
    if hasattr(tree, 'car') and hasattr(tree, 'cdr'):
        return lisptype.lispCons(copy_tree(tree.car), copy_tree(tree.cdr))
    return tree

@_registry.cl_function('INSPECT')
def inspect(object):
    """Very small stub for INSPECT: prints object and returns it."""
    print(f"Object: {object}")
    print(f"Type: {type(object)}")
    return object

@_registry.cl_function('INCF')
def incf(place, delta=1):
    """INCF stub.

    In a full implementation this would modify a generalized PLACE. For now,
    if a numeric value is passed we return the incremented result; otherwise
    we raise a not implemented error to signal unsupported place types.
    """
    if isinstance(place, (int, float)):
        return place + delta
    raise lisptype.LispNotImplementedError("INCF generalized PLACE not implemented")

@_registry.cl_function('MAKE-LOAD-FORM')
def make_load_form(object, environment=None):
    """Stub MAKE-LOAD-FORM returning (creation-form . init-form).

    We simply return a cons-like Python tuple of two identical objects.
    """
    return (object, object)

@_registry.cl_function('MAKE-LOAD-FORM-SAVING-SLOTS')
def make_load_form_saving_slots(object, **kwargs):
    """Stub MAKE-LOAD-FORM-SAVING-SLOTS returning simple reconstruction tuple."""
    return (object, kwargs or {})

@_registry.cl_function('PPRINT-FILL')
def pprint_fill(stream, object, colon_p=False, at_sign_p=False):
    """Minimal PPRINT-FILL stub: write object's string representation to stream if possible."""
    try:
        if hasattr(stream, 'write'):
            stream.write(str(object))
    except Exception:
        pass
    return object


# Type/condition constants - simple stub implementations
@_registry.cl_function('KEYWORD')
def keyword():
    """Return a placeholder representing the KEYWORD type."""
    return 'KEYWORD'

@_registry.cl_function('INTEGER')
def integer():
    """Return a placeholder representing the INTEGER type."""
    return 'INTEGER'

@_registry.cl_function('FIXNUM')
def fixnum():
    """Return a placeholder representing the FIXNUM type."""
    return 'FIXNUM'

@_registry.cl_function('DOUBLE-FLOAT')
def double_float():
    """Return a placeholder representing the DOUBLE-FLOAT type."""
    return 'DOUBLE-FLOAT'

@_registry.cl_function('SINGLE-FLOAT')
def single_float():
    """Return a placeholder representing the SINGLE-FLOAT type."""
    return 'SINGLE-FLOAT'

@_registry.cl_function('SHORT-FLOAT')
def short_float():
    """Return a placeholder representing the SHORT-FLOAT type."""
    return 'SHORT-FLOAT'

@_registry.cl_function('EXTENDED-CHAR')
def extended_char():
    """Return a placeholder representing the EXTENDED-CHAR type."""
    return 'EXTENDED-CHAR'

@_registry.cl_function('HASH-TABLE')
def hash_table():
    """Return a placeholder representing the HASH-TABLE type."""
    return 'HASH-TABLE'

@_registry.cl_function('GENERIC-FUNCTION')
def generic_function():
    """Return a placeholder representing the GENERIC-FUNCTION type."""
    return 'GENERIC-FUNCTION'

@_registry.cl_function('FILE-STREAM')
def file_stream():
    """Return a placeholder representing the FILE-STREAM type."""
    return 'FILE-STREAM'

@_registry.cl_function('FILE-ERROR')
def file_error():
    """Return a placeholder representing the FILE-ERROR condition type."""
    return 'FILE-ERROR'

@_registry.cl_function('END-OF-FILE')
def end_of_file():
    """Return a placeholder representing the END-OF-FILE condition type."""
    return 'END-OF-FILE'

@_registry.cl_function('FLOATING-POINT-INEXACT')
def floating_point_inexact():
    """Return placeholder for FLOATING-POINT-INEXACT condition."""
    return 'FLOATING-POINT-INEXACT'

@_registry.cl_function('FLOATING-POINT-INVALID-OPERATION')
def floating_point_invalid_operation():
    """Return placeholder for FLOATING-POINT-INVALID-OPERATION condition."""
    return 'FLOATING-POINT-INVALID-OPERATION'

@_registry.cl_function('FLOATING-POINT-OVERFLOW')
def floating_point_overflow():
    """Return placeholder for FLOATING-POINT-OVERFLOW condition."""
    return 'FLOATING-POINT-OVERFLOW'

@_registry.cl_function('FLOATING-POINT-UNDERFLOW')
def floating_point_underflow():
    """Return placeholder for FLOATING-POINT-UNDERFLOW condition."""
    return 'FLOATING-POINT-UNDERFLOW'

@_registry.cl_function('ARITHMETIC-ERROR-OPERANDS')
def arithmetic_error_operands(condition):
    """Stub accessor returning an empty list for arithmetic error operands."""
    return []

@_registry.cl_function('ARITHMETIC-ERROR-OPERATION')
def arithmetic_error_operation(condition):
    """Stub accessor returning None for arithmetic error operation."""
    return None

@_registry.cl_function('FILE-ERROR-PATHNAME')
def file_error_pathname(condition):
    """Stub accessor returning None for file error pathname."""
    return None


# (Old non-decorated hash table functions removed after migration)


# Multiple values
@_registry.cl_function('MULTIPLE-VALUE-BIND')
def multiple_value_bind(*args):
    """Stub MULTIPLE-VALUE-BIND returns NIL (Python None)."""
    return None


@_registry.cl_function('MULTIPLE-VALUE-CALL')
def multiple_value_call(function, *args):
    """Stub MULTIPLE-VALUE-CALL applies function to args."""
    return function(*args)


@_registry.cl_function('MULTIPLE-VALUE-LIST')
def multiple_value_list(*values):
    """Return Python list collecting multiple values."""
    return list(values)


@_registry.cl_function('MULTIPLE-VALUE-PROG1')
def multiple_value_prog1(first_form, *forms):
    """Return first_form ignoring remaining forms (stub)."""
    return first_form


@_registry.cl_function('MULTIPLE-VALUE-SETQ')
def multiple_value_setq(*args):
    """Stub MULTIPLE-VALUE-SETQ returns NIL."""
    return None


@_registry.cl_function('NTH-VALUE')
def nth_value(n, values_form):
    """Return nth value from an iterable (stub)."""
    if isinstance(values_form, (list, tuple)) and n < len(values_form):
        return values_form[n]
    return None


# Symbol operations
@_registry.cl_function('SYMBOL-NAME')
def symbol_name(symbol):
    """Get symbol name."""
    if hasattr(symbol, 'name'):
        return symbol.name
    return str(symbol)


@_registry.cl_function('SYMBOL-PACKAGE')
def symbol_package(symbol):
    """Get symbol package."""
    if hasattr(symbol, 'package'):
        return symbol.package
    return None


@_registry.cl_function('SYMBOL-VALUE')
def symbol_value(symbol):
    """Get symbol value."""
    if hasattr(symbol, 'value'):
        return symbol.value
    return None


@_registry.cl_function('SYMBOL-FUNCTION')
def symbol_function(symbol):
    """Get symbol function."""
    if hasattr(symbol, 'function'):
        return symbol.function
    return None


@_registry.cl_function('MAKE-SYMBOL')
def make_symbol(name):
    """Make uninterned symbol."""
    return lisptype.LispSymbol(name)


@_registry.cl_function('COPY-SYMBOL')
def copy_symbol(symbol, copy_props=None):
    """Copy symbol."""
    new_sym = make_symbol(symbol_name(symbol))
    return new_sym


@_registry.cl_function('MAKE-PACKAGE')
def make_package(name, **kwargs):
    """Create a new package (wrapper around lisptype.make_package)."""
    nicknames = kwargs.get('nicknames') or kwargs.get('nicknames', None)
    use_list = kwargs.get('use_list') or kwargs.get('use_list', None)
    return lisptype.make_package(str(name), nicknames or None, use_list or None)


@_registry.cl_function('PACKAGE-NAME')
def package_name(package):
    """Get package name."""
    if isinstance(package, lisptype.Package):
        return package.name
    return str(package)


@_registry.cl_function('PACKAGE-NICKNAMES')
def package_nicknames(package):
    """Get package nicknames."""
    if isinstance(package, lisptype.Package):
        return package.nicknames
    return []


@_registry.cl_function('RENAME-PACKAGE')
def rename_package(package, new_name, new_nicknames=None):
    """Rename package."""
    if isinstance(package, lisptype.Package):
        package.name = str(new_name).upper()
        if new_nicknames is not None:
            package.nicknames = [n.upper() for n in new_nicknames]
        return package
    return lisptype.make_package(str(new_name), new_nicknames or None, None)


@_registry.cl_function('PACKAGE-USE-LIST')
def package_use_list(package):
    """Get packages used by package."""
    if isinstance(package, lisptype.Package):
        return package.use_list
    return []


@_registry.cl_function('PACKAGE-USED-BY-LIST')
def package_used_by_list(package):
    """Get packages that use package."""
    if not isinstance(package, lisptype.Package):
        return []
    out = []
    for p in set(state.packages.values()):
        if package in p.use_list:
            out.append(p)
    return out


@_registry.cl_function('PACKAGE-SHADOWING-SYMBOLS')
def package_shadowing_symbols(package):
    """Get package shadowing symbols."""
    if isinstance(package, lisptype.Package):
        return list(package.shadowing_symbols)
    return []


@_registry.cl_function('LIST-ALL-PACKAGES')
def list_all_packages():
    """List all packages."""
    seen = set()
    out = []
    for p in set(state.packages.values()):
        if p.name not in seen:
            seen.add(p.name)
            out.append(p)
    return out


@_registry.cl_function('UNINTERN')
def unintern(symbol, package=None):
    """Unintern symbol."""
    if isinstance(symbol, lisptype.LispSymbol):
        name = symbol.name
    else:
        name = str(symbol)
    pkg = lisptype.COMMON_LISP_USER_PACKAGE if package is None else (package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)))
    if pkg is None:
        return False
    if name.upper() in pkg.symbols:
        del pkg.symbols[name.upper()]
        pkg.external_symbols.discard(name.upper())
        return True
    return False


@_registry.cl_function('UNEXPORT')
def unexport(symbols, package=None):
    """Unexport symbols."""
    if not isinstance(symbols, (list, tuple)):
        symbols = [symbols]
    pkg = lisptype.COMMON_LISP_USER_PACKAGE if package is None else (package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)))
    if pkg is None:
        return False
    for s in symbols:
        name = s.name if hasattr(s, 'name') else str(s)
        pkg.unexport_symbol(name)
    return True


@_registry.cl_function('SHADOWING-IMPORT')
def shadowing_import(symbols, package=None):
    """Import symbols with shadowing."""
    if not isinstance(symbols, (list, tuple)):
        symbols = [symbols]
    pkg = lisptype.COMMON_LISP_USER_PACKAGE if package is None else (package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)))
    if pkg is None:
        return False
    for s in symbols:
        name = s.name if hasattr(s, 'name') else str(s)
        pkg.intern_symbol(name, external=False)
        pkg.shadowing_symbols.add(name.upper())
    return True


@_registry.cl_function('SHADOW')
def shadow(symbol_names, package=None):
    """Shadow symbols."""
    pkg = lisptype.COMMON_LISP_USER_PACKAGE if package is None else (package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)))
    if pkg is None:
        return False
    if not isinstance(symbol_names, (list, tuple)):
        symbol_names = [symbol_names]
    for n in symbol_names:
        pkg.shadowing_symbols.add(str(n).upper())
    return True


@_registry.cl_function('USE-PACKAGE')
def use_package(packages_to_use, package=None):
    """Use packages."""
    pkg = lisptype.COMMON_LISP_USER_PACKAGE if package is None else (package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)))
    if pkg is None:
        return False
    if not isinstance(packages_to_use, (list, tuple)):
        packages_to_use = [packages_to_use]
    for p in packages_to_use:
        if isinstance(p, lisptype.Package):
            pkg.use_list.append(p)
        else:
            target = lisptype.find_package(str(p)) or lisptype.make_package(str(p))
            pkg.use_list.append(target)
    return True


@_registry.cl_function('UNUSE-PACKAGE')
def unuse_package(packages_to_unuse, package=None):
    """Unuse packages."""
    pkg = lisptype.COMMON_LISP_USER_PACKAGE if package is None else (package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)))
    if pkg is None:
        return False
    if not isinstance(packages_to_unuse, (list, tuple)):
        packages_to_unuse = [packages_to_unuse]
    for p in packages_to_unuse:
        if isinstance(p, lisptype.Package):
            if p in pkg.use_list:
                pkg.use_list.remove(p)
        else:
            target = lisptype.find_package(str(p))
            if target and target in pkg.use_list:
                pkg.use_list.remove(target)
    return True


# Macro operations
@_registry.cl_function('MACROEXPAND')
def macroexpand(form, env=None):
    """Macroexpand form repeatedly until no change."""
    if env is None:
        from fclpy.lispenv import current_environment as _cur_env
        env = _cur_env
    expanded, changed = macroexpand_1(form, env)
    while changed:
        expanded, changed = macroexpand_1(expanded, env)
    return expanded, False


@_registry.cl_function('MACROEXPAND-1')
def macroexpand_1(form, env=None):
    """Macroexpand form once.

    If top-level operator is a macro, call it with raw args and return result.
    """
    if env is None:
        from fclpy.lispenv import current_environment as _cur_env
        env = _cur_env

    from fclpy.lispfunc.core import _consp_internal, car, cdr
    # Only cons cells may be macro calls
    if not _consp_internal(form):
        return form, False

    op = car(form)
    if isinstance(op, lisptype.LispSymbol):
        func = env.find_func(op)
        if callable(func) and getattr(func, '__is_macro__', False):
            # Collect raw args
            raw_args = []
            cur = cdr(form)
            while _consp_internal(cur):
                raw_args.append(car(cur))
                cur = cdr(cur)
            expanded = func(*raw_args)
            return expanded, True

    return form, False


@_registry.cl_function('ARRAY-DIMENSION-LIMIT')
def array_dimension_limit():
    """Array dimension limit."""
    return 2**16


@_registry.cl_function('ARRAY-RANK-LIMIT')
def array_rank_limit():
    """Array rank limit."""
    return 8


@_registry.cl_function('ARRAY-TOTAL-SIZE-LIMIT')
def array_total_size_limit():
    """Array total size limit."""
    return 2**24


@_registry.cl_function('CALL-ARGUMENTS-LIMIT')
def call_arguments_limit():
    """Call arguments limit."""
    return 2**16


@_registry.cl_function('MULTIPLE-VALUES-LIMIT')
def multiple_values_limit():
    """Multiple values limit."""
    return 20


@_registry.cl_function('CHAR-CODE-LIMIT')
def char_code_limit():
    """Character code limit."""
    return 2**16


# Essential predicates
def constantp(form, env=None):
    """Test if form is constant."""
    return isinstance(form, (int, float, str, bool))


def special_operator_p(symbol):
    """Test if symbol is special operator."""
    if hasattr(symbol, 'name'):
        special_ops = {'QUOTE', 'IF', 'LAMBDA', 'SETQ', 'LET', 'DEFUN', 'DEFVAR',
                      'PROGN', 'COND', 'AND', 'OR', 'WHEN', 'UNLESS', 'BLOCK',
                      'RETURN-FROM', 'CATCH', 'THROW', 'TAGBODY', 'GO', 'UNWIND-PROTECT'}
        return symbol.name.upper() in special_ops
    return False


# Essential predicates and comparisons
def equalp(x, y):
    """Test equalp (case-insensitive equality)."""
    if isinstance(x, str) and isinstance(y, str):
        return x.upper() == y.upper()
    return x == y


def not_fn(x):
    """Logical NOT."""
    return not x


def eql(x, y):
    """Test EQL equality."""
    return x is y or (type(x) == type(y) and x == y)


def equal_fn(x, y):
    """Test EQUAL equality."""
    return x == y


# Iteration macros
@_registry.cl_function('DO-SYMBOLS')
def do_symbols(var_package_result_decl, *body):
    """Do symbols macro."""
    return None  # Simplified


@_registry.cl_function('DO-EXTERNAL-SYMBOLS')
def do_external_symbols(var_package_result_decl, *body):
    """Do external symbols macro."""
    return None  # Simplified


@_registry.cl_function('DO-ALL-SYMBOLS')
def do_all_symbols(var_result_decl, *body):
    """Do all symbols macro."""
    return None  # Simplified


# Additional predicates 
def simple_string_p(obj):
    """Test if object is simple string."""
    return isinstance(obj, str)


def simple_vector_p(obj):
    """Test if object is simple vector."""
    return isinstance(obj, list)


def arrayp(obj):
    """Test if object is array."""
    return isinstance(obj, (list, tuple, str))


@_registry.cl_function('ADJUSTABLE-ARRAY-P')
def adjustable_array_p(obj):
    """Test if array is adjustable."""
    return False  # Simplified


def vectorp(obj):
    """Test if object is vector."""
    return isinstance(obj, (list, tuple))


def simple_array_p(obj):
    """Test if object is simple array."""
    return isinstance(obj, (list, tuple, str))


# Stream predicates
@_registry.cl_function('STREAMP')
def streamp(obj):
    """STREAMP predicate (simplified)."""
    return hasattr(obj, 'read') or hasattr(obj, 'write')  # Simplified


@_registry.cl_function('BROADCAST-STREAM-P')
def broadcast_stream_p(obj):
    """BROADCAST-STREAM-P predicate (stub)."""
    return False  # Simplified


@_registry.cl_function('CONCATENATED-STREAM-P')
def concatenated_stream_p(obj):
    """CONCATENATED-STREAM-P predicate (stub)."""
    return False  # Simplified


@_registry.cl_function('ECHO-STREAM-P')
def echo_stream_p(obj):
    """Test if object is echo stream."""
    return lisptype.lisp_bool(False)  # Simplified


@_registry.cl_function('FILE-STREAM-P')
def file_stream_p(obj):
    """FILE-STREAM-P predicate (stub)."""
    return False  # Simplified


@_registry.cl_function('STRING-STREAM-P')
def string_stream_p(obj):
    """STRING-STREAM-P predicate (stub)."""
    return False  # Simplified


@_registry.cl_function('SYNONYM-STREAM-P')
def synonym_stream_p(obj):
    """SYNONYM-STREAM-P predicate (stub)."""
    return False  # Simplified


@_registry.cl_function('TWO-WAY-STREAM-P')
def two_way_stream_p(obj):
    """TWO-WAY-STREAM-P predicate (stub)."""
    return False  # Simplified


# Pathname predicates
def pathnamep(obj):
    """Test if object is pathname."""
    return isinstance(obj, str)  # Simplified


def logical_pathname_p(obj):
    """Test if object is logical pathname."""
    return False  # Simplified


# System and debugging functions
def describe(object, stream=None):
    """Describe object."""
    print(f"Description of {object}")
    return None


def ed(x=None):
    """Invoke editor."""
    return None


def dribble(pathname=None):
    """Start/stop dribbling."""
    return None


@_registry.cl_function('BREAK')
def break_fn(format_control=None, *format_args):
    """Enter debugger."""
    return None


@_registry.cl_function('CONTINUE')
def continue_fn(condition=None):
    """Continue from debugger."""
    return None


@_registry.cl_function('TRACE')
def trace_fn(*function_names):
    """Trace functions."""
    return None


def untrace(*function_names):
    """Untrace functions (stub)."""
    return None


def disassemble(function):
    """Disassemble a function (stub)."""
    try:
        import inspect
        src = inspect.getsource(function)
        return src
    except Exception:
        return str(function)


def room(*args, **kwargs):
    """Room function stub (introspection/storage)"""
    return None


def step(form):
    """Step through form."""
    return None


# Package system functions
def package_error_package(condition):
    """Get package from package error."""
    return None


@_registry.cl_function('WITH-PACKAGE-ITERATOR')
def with_package_iterator(name, package_list_form, symbol_types, *body):
    """Stub WITH-PACKAGE-ITERATOR: sequentially evaluate body forms and return last.

    Common Lisp semantics are complex; this simplified version just returns the last form.
    """
    result = None
    for form in body:
        result = form
    return result


def export_fn(symbols, package=None):
    """Export symbols."""
    return True


def import_fn(symbols, package=None):
    """Import symbols."""
    return True


@_registry.cl_function('PROVIDE')
def provide(module_name):
    """Provide module."""
    return module_name


def documentation(item=None):
    """Return documentation string for an item (stub)."""
    return f"Documentation for {item}"


@_registry.cl_function('REQUIRE')
def require(module_name, pathname=None):
    """Require module."""
    return None


# Declaration and definition functions
@_registry.cl_function('DECLAIM')
def declaim(*declaration_specifiers):
    """Global declarations."""
    return None


@_registry.cl_function('DECLARE')
def declare(*declaration_specifiers):
    """Local declarations."""
    return None


@_registry.cl_function('DEFCONSTANT')
def defconstant(name, initial_value, documentation=None):
    """Define constant."""
    return name


@_registry.cl_function('DEFPARAMETER')
def defparameter(name, initial_value, documentation=None):
    """Define parameter."""
    return name


# defmacro_fn stub removed; use the full implementation earlier in this file.


def deftype(name, lambda_list, *body):
    """Define type."""
    return name


@_registry.cl_function('DEFSTRUCT')
def defstruct(name_and_options, *slot_descriptions):
    """Define structure."""
    return None


@_registry.cl_function('DEFPACKAGE')
def defpackage(package_name, *options):
    """Define package."""
    return str(package_name)


# CLOS functions (simplified)
@_registry.cl_function('DEFCLASS')
def defclass(name, superclasses, slots, *class_options):
    """Define class."""
    return name


@_registry.cl_function('DEFGENERIC')
def defgeneric(name, lambda_list, *options):
    """Define generic function."""
    return name


@_registry.cl_function('DEFMETHOD')
def defmethod(name, *args):
    """Define method."""
    return name


@_registry.cl_function('MAKE-INSTANCE')
def make_instance(class_designator, *initargs):
    """Make instance."""
    return {}  # Simplified


def class_of(object):
    """Get class of object."""
    return type(object)


def class_name(class_obj):
    """Get class name."""
    return str(class_obj)


def change_class(instance, new_class, *initargs):
    """Change class of instance."""
    return instance


# More CLOS functions
def built_in_class():
    """Built-in class type."""
    return 'BUILT-IN-CLASS'


def call_method(method, next_methods, *args):
    """Call method."""
    return None


def call_next_method(*args):
    """Call next method."""
    return None


def compute_applicable_methods(generic_function, arguments):
    """Compute applicable methods."""
    return []


def ensure_generic_function(function_name, *options):
    """Ensure generic function."""
    return function_name


def generic_function_lambda_list(generic_function):
    """Get generic function lambda list."""
    return []


def generic_function_methods(generic_function):
    """Get generic function methods."""
    return []


def generic_function_name(generic_function):
    """Get generic function name."""
    return str(generic_function)


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


def ensure_directories_exist(pathspec, **kwargs):
    """Ensure directories exist."""
    return pathspec, True


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
