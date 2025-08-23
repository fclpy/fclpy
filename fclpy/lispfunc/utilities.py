"""Miscellaneous utility functions and system operations."""

import time
import fclpy.lisptype as lisptype
import fclpy.state as state
from fclpy.lispfunc import registry as _registry


def abort(condition=True):
    """Abort with restart."""
    raise lisptype.LispNotImplementedError("ABORT")


def apropos(string, package=None):
    """Find symbols matching string."""
    raise lisptype.LispNotImplementedError("APROPOS")


def apropos_list(string, package=None):
    """List symbols matching string."""
    raise lisptype.LispNotImplementedError("APROPOS-LIST")


def describe(object, stream=None):
    """Describe object."""
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
def encode_universal_time(year, month, date, hour, minute, second):
    """Encode date/time to universal time (seconds since 1900-01-01)."""
    import datetime
    try:
        dt = datetime.datetime(year, month, date, hour, minute, second)
        epoch = datetime.datetime(1900, 1, 1)
        return int((dt - epoch).total_seconds())
    except ValueError:
        raise lisptype.LispNotImplementedError("ENCODE-UNIVERSAL-TIME: invalid date/time")


def get_universal_time():
    """Return current universal time (seconds since 1900-01-01)."""
    import datetime
    epoch = datetime.datetime(1900, 1, 1)
    return int((datetime.datetime.utcnow() - epoch).total_seconds())


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
def lisp_implementation_type():
    """Get Lisp implementation type."""
    return "FCLPY"


def lisp_implementation_version():
    """Get Lisp implementation version."""
    return "0.1.0"


def machine_instance():
    """Get machine instance."""
    import socket
    return socket.gethostname()


def machine_type():
    """Get machine type."""
    import platform
    return platform.machine()


def machine_version():
    """Get machine version."""
    import platform
    return platform.platform()


def software_type():
    """Get software type."""
    import platform
    return platform.system()


def software_version():
    """Get software version."""
    import platform
    return platform.release()


def short_site_name():
    """Get short site name."""
    return "Unknown"


def long_site_name():
    """Get long site name."""
    return "Unknown Site"


# Environment access
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


# Random number generation
def random(limit, state=None):
    """Generate random number."""
    import random as rnd
    if isinstance(limit, int):
        return rnd.randrange(limit)
    elif isinstance(limit, float):
        return rnd.random() * limit
    else:
        raise lisptype.LispNotImplementedError("RANDOM: invalid limit type")


def make_random_state(state=None):
    """Make random state."""
    import random as rnd
    new_state = rnd.getstate()
    if state is not None:
        rnd.setstate(state)
    return new_state


def random_state_p(object):
    """Test if object is random state."""
    return isinstance(object, tuple) and len(object) >= 2


# Load and compile
def load(filespec, **kwargs):
    """Load file."""
    raise lisptype.LispNotImplementedError("LOAD")


def compile_fn(name, definition=None):
    """Compile function."""
    raise lisptype.LispNotImplementedError("COMPILE")


def eval_when(situations, *forms):
    """Evaluate when situations apply."""
    # Simplified - always evaluate
    result = None
    for form in forms:
        result = form  # Would normally evaluate
    return result


def locally(*body):
    """Execute body in local scope."""
    raise lisptype.LispNotImplementedError("LOCALLY")


def progv(symbols, values, *body):
    """Execute with dynamic bindings."""
    raise lisptype.LispNotImplementedError("PROGV")


def special_operator_p(symbol):
    """Test if symbol is special operator."""
    if isinstance(symbol, lisptype.LispSymbol):
        special_ops = {'QUOTE', 'IF', 'LAMBDA', 'SETQ', 'LET', 'DEFUN', 'DEFVAR',
                      'PROGN', 'COND', 'AND', 'OR', 'WHEN', 'UNLESS'}
        return symbol.name in special_ops
    return False


def macro_function(symbol, environment=None):
    """Get macro function."""
    raise lisptype.LispNotImplementedError("MACRO-FUNCTION")


def compiler_macro_function(name, environment=None):
    """Get compiler macro function."""
    raise lisptype.LispNotImplementedError("COMPILER-MACRO-FUNCTION")


@_registry.cl_function('FBOUNDP')
def fboundp(symbol):
    """Test if symbol has function binding."""
    raise lisptype.LispNotImplementedError("FBOUNDP")


def fmakunbound(symbol):
    """Remove function binding."""
    raise lisptype.LispNotImplementedError("FMAKUNBOUND")


def fdefinition(symbol):
    """Get function definition."""
    raise lisptype.LispNotImplementedError("FDEFINITION")


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


def gentemp(prefix="T", package=None):
    """Generate temporary symbol."""
    count = 0
    while True:
        name = f"{prefix}{count}"
        symbol = lisptype.LispSymbol(name)
        # In a real implementation, would check if symbol exists in package
        return symbol


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
def get(symbol, indicator, default=None):
    """Get property from symbol's property list."""
    if hasattr(symbol, '_plist'):
        plist = symbol._plist
        for i in range(0, len(plist), 2):
            if i + 1 < len(plist) and plist[i] == indicator:
                return plist[i + 1]
    return default


def rplaca(cons, new_car):
    """Replace car of cons cell."""
    if hasattr(cons, 'car'):
        cons.car = new_car
    return cons


def rplacd(cons, new_cdr):
    """Replace cdr of cons cell."""
    if hasattr(cons, 'cdr'):
        cons.cdr = new_cdr
    return cons


# Package and symbol operations
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


def find_package(name):
    """Find package by name."""
    if isinstance(name, lisptype.Package):
        return name
    return lisptype.find_package(str(name))


def find_all_symbols(string):
    """Find all symbols with given name."""
    name = string if isinstance(string, str) else str(string)
    out = []
    for p in set(state.packages.values()):
        sym, status = p.find_symbol(name)
        if sym is not None:
            out.append(sym)
    return out


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


def function_keywords(function):
    """Return function's keyword parameters."""
    raise lisptype.LispNotImplementedError("FUNCTION-KEYWORDS")


def function_lambda_expression(function):
    """Return function's lambda expression."""
    raise lisptype.LispNotImplementedError("FUNCTION-LAMBDA-EXPRESSION")


# Stubs for many missing CLOS and system functions
def find_class(name, errorp=True, environment=None):
    """Find class by name."""
    raise lisptype.LispNotImplementedError("FIND-CLASS")

def find_method(generic_function, qualifiers, specializers, errorp=True):
    """Find method."""
    raise lisptype.LispNotImplementedError("FIND-METHOD")

def add_method(generic_function, method):
    """Add method to generic function."""
    raise lisptype.LispNotImplementedError("ADD-METHOD")

def allocate_instance(class_obj, **kwargs):
    """Allocate instance."""
    raise lisptype.LispNotImplementedError("ALLOCATE-INSTANCE")

def initialize_instance(instance, **kwargs):
    """Initialize instance."""
    raise lisptype.LispNotImplementedError("INITIALIZE-INSTANCE")

def update_instance_for_different_class(instance, new_class):
    """Update instance for different class."""
    raise lisptype.LispNotImplementedError("UPDATE-INSTANCE-FOR-DIFFERENT-CLASS")

def update_instance_for_redefined_class(instance, added_slots, discarded_slots, property_list):
    """Update instance for redefined class."""
    raise lisptype.LispNotImplementedError("UPDATE-INSTANCE-FOR-REDEFINED-CLASS")

def slot_missing(class_obj, instance, slot_name, operation, new_value=None):
    """Handle missing slot."""
    raise lisptype.LispNotImplementedError("SLOT-MISSING")

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

def lambda_list_keywords():
    """Lambda list keywords."""
    return ['&optional', '&rest', '&key', '&allow-other-keys', '&aux']

def lambda_parameters_limit():
    """Lambda parameters limit."""
    return 65536

def identity(x):
    """Identity function."""
    return x

def copy_tree(tree):
    """Copy tree structure."""
    if hasattr(tree, 'car') and hasattr(tree, 'cdr'):
        return lisptype.lispCons(copy_tree(tree.car), copy_tree(tree.cdr))
    return tree

def inspect(object):
    """Inspect object."""
    print(f"Object: {object}")
    print(f"Type: {type(object)}")
    return object

def incf(place, delta=1):
    """Increment place."""
    raise lisptype.LispNotImplementedError("INCF")

def make_load_form(object, environment=None):
    """Make load form."""
    raise lisptype.LispNotImplementedError("MAKE-LOAD-FORM")

def make_load_form_saving_slots(object, **kwargs):
    """Make load form saving slots."""
    raise lisptype.LispNotImplementedError("MAKE-LOAD-FORM-SAVING-SLOTS")

def pprint_fill(stream, object, colon_p=False, at_sign_p=False):
    """Pretty print fill."""
    raise lisptype.LispNotImplementedError("PPRINT-FILL")


# Type constants - simple stub implementations
def keyword():
    """Keyword type."""
    return 'KEYWORD'

def integer():
    """Integer type."""
    return 'INTEGER'

def fixnum():
    """Fixnum type."""
    return 'FIXNUM'

def double_float():
    """Double float type."""
    return 'DOUBLE-FLOAT'

def single_float():
    """Single float type."""
    return 'SINGLE-FLOAT'

def short_float():
    """Short float type."""
    return 'SHORT-FLOAT'

def extended_char():
    """Extended char type."""
    return 'EXTENDED-CHAR'

def hash_table():
    """Hash table type."""
    return 'HASH-TABLE'

def generic_function():
    """Generic function type."""
    return 'GENERIC-FUNCTION'

def file_stream():
    """File stream type."""
    return 'FILE-STREAM'

def file_error():
    """File error type."""
    return 'FILE-ERROR'

def end_of_file():
    """End of file condition."""
    return 'END-OF-FILE'

def floating_point_inexact():
    """Floating point inexact condition."""
    return 'FLOATING-POINT-INEXACT'

def floating_point_invalid_operation():
    """Floating point invalid operation."""
    return 'FLOATING-POINT-INVALID-OPERATION'

def floating_point_overflow():
    """Floating point overflow."""
    return 'FLOATING-POINT-OVERFLOW'

def floating_point_underflow():
    """Floating point underflow."""
    return 'FLOATING-POINT-UNDERFLOW'

def arithmetic_error_operands(condition):
    """Get arithmetic error operands."""
    raise lisptype.LispNotImplementedError("ARITHMETIC-ERROR-OPERANDS")

def arithmetic_error_operation(condition):
    """Get arithmetic error operation."""
    raise lisptype.LispNotImplementedError("ARITHMETIC-ERROR-OPERATION")

def file_error_pathname(condition):
    """Get file error pathname."""
    raise lisptype.LispNotImplementedError("FILE-ERROR-PATHNAME")


# Hash table operations
def make_hash_table(**kwargs):
    """Create hash table."""
    return {}


def gethash(key, hash_table, default=None):
    """Get value from hash table."""
    return hash_table.get(key, default)


def remhash(key, hash_table):
    """Remove entry from hash table."""
    if key in hash_table:
        del hash_table[key]
        return True
    return False


def maphash(function, hash_table):
    """Map function over hash table."""
    for key, value in hash_table.items():
        function(key, value)
    return None


def clrhash(hash_table):
    """Clear hash table."""
    hash_table.clear()
    return hash_table


def sxhash(object):
    """Compute hash code."""
    return hash(object) % (2**32)  # Keep it positive and bounded


# Multiple values
def multiple_value_bind(*args):
    """Bind multiple values."""
    return None  # Simplified


def multiple_value_call(function, *args):
    """Call function with multiple values."""
    return function(*args)


def multiple_value_list(*values):
    """Create list from multiple values."""
    return list(values)


def multiple_value_prog1(first_form, *forms):
    """Multiple value prog1."""
    return first_form


def multiple_value_setq(*args):
    """Multiple value setq."""
    return None


def nth_value(n, values_form):
    """Get nth value."""
    if isinstance(values_form, (list, tuple)) and n < len(values_form):
        return values_form[n]
    return None


# Symbol operations
def symbol_name(symbol):
    """Get symbol name."""
    if hasattr(symbol, 'name'):
        return symbol.name
    return str(symbol)


def symbol_package(symbol):
    """Get symbol package."""
    if hasattr(symbol, 'package'):
        return symbol.package
    return None


def symbol_value(symbol):
    """Get symbol value."""
    if hasattr(symbol, 'value'):
        return symbol.value
    return None


def symbol_function(symbol):
    """Get symbol function."""
    if hasattr(symbol, 'function'):
        return symbol.function
    return None


def make_symbol(name):
    """Make uninterned symbol."""
    return lisptype.LispSymbol(name)


def copy_symbol(symbol, copy_props=None):
    """Copy symbol."""
    new_sym = make_symbol(symbol_name(symbol))
    return new_sym


# Package operations
def make_package(name, **kwargs):
    """Make package using canonical lisptype implementation."""
    nicknames = kwargs.get('nicknames') or kwargs.get('nicknames', None)
    use_list = kwargs.get('use_list') or kwargs.get('use_list', None)
    return lisptype.make_package(str(name), nicknames or None, use_list or None)


def package_name(package):
    """Get package name."""
    if isinstance(package, lisptype.Package):
        return package.name
    return str(package)


def package_nicknames(package):
    """Get package nicknames."""
    if isinstance(package, lisptype.Package):
        return package.nicknames
    return []


def rename_package(package, new_name, new_nicknames=None):
    """Rename package."""
    if isinstance(package, lisptype.Package):
        package.name = str(new_name).upper()
        if new_nicknames is not None:
            package.nicknames = [n.upper() for n in new_nicknames]
        return package
    return lisptype.make_package(str(new_name), new_nicknames or None, None)


def package_use_list(package):
    """Get packages used by package."""
    if isinstance(package, lisptype.Package):
        return package.use_list
    return []


def package_used_by_list(package):
    """Get packages that use package."""
    # Reverse lookup through all packages
    if not isinstance(package, lisptype.Package):
        return []
    out = []
    for p in set(state.packages.values()):
        if package in p.use_list:
            out.append(p)
    return out


def package_shadowing_symbols(package):
    """Get package shadowing symbols."""
    if isinstance(package, lisptype.Package):
        return list(package.shadowing_symbols)
    return []


def list_all_packages():
    """List all packages."""
    # Return unique packages by name
    seen = set()
    out = []
    for p in set(state.packages.values()):
        if p.name not in seen:
            seen.add(p.name)
            out.append(p)
    return out


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
def macroexpand(form, env=None):
    """Macroexpand form repeatedly until no change."""
    if env is None:
        from fclpy.lispenv import current_environment as _cur_env
        env = _cur_env
    expanded, changed = macroexpand_1(form, env)
    while changed:
        expanded, changed = macroexpand_1(expanded, env)
    return expanded, False


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


def macro_function(symbol, environment=None):
    """Get macro function bound to symbol in environment (or None)."""
    if environment is None:
        from fclpy.lispenv import current_environment as _cur_env
        environment = _cur_env
    func = environment.find_func(symbol)
    if callable(func) and getattr(func, '__is_macro__', False):
        return func
    return None


def defmacro_fn(name, lambda_list, *body):
    """Define a macro in the current environment (utilities-level)."""
    from fclpy.lispenv import current_environment as _cur_env
    env = _cur_env
    from fclpy.lispfunc.core import _consp_internal, car, cdr
    # Helper: convert Python list of raw forms into Lisp cons list
    def list_to_cons(pylist):
        lst = lisptype.NIL
        for itm in reversed(pylist):
            lst = lisptype.lispCons(itm, lst)
        return lst

    # Parse macro lambda-list with basic support for &optional, &rest, &whole, &key
    def parse_macro_lambda_list(lambda_list):
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
                    mode = 'optional'; continue
                if nm == '&REST':
                    # next element is rest var
                    if _consp_internal(cur):
                        rest_item = car(cur); cur = cdr(cur)
                        if isinstance(rest_item, lisptype.LispSymbol):
                            rest = rest_item.name
                    continue
                if nm == '&WHOLE':
                    if _consp_internal(cur):
                        whole_item = car(cur); cur = cdr(cur)
                        if isinstance(whole_item, lisptype.LispSymbol):
                            whole = whole_item.name
                    continue
                if nm == '&KEY':
                    mode = 'key'; continue
                if nm == '&ALLOW-OTHER-KEYS':
                    allow_other_keys = True; continue

            # process according to mode
            if mode == 'required':
                if isinstance(item, lisptype.LispSymbol):
                    required.append(item.name)
            elif mode == 'optional':
                # optional element may be (name default) or symbol
                if _consp_internal(item):
                    name = car(item); default = car(cdr(item)) if _consp_internal(cdr(item)) else None
                    if isinstance(name, lisptype.LispSymbol):
                        optional.append((name.name, default))
                elif isinstance(item, lisptype.LispSymbol):
                    optional.append((item.name, None))
            elif mode == 'key':
                if _consp_internal(item):
                    name = car(item); default = car(cdr(item)) if _consp_internal(cdr(item)) else None
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

    def substitute(form, mapping):
        if isinstance(form, lisptype.LispSymbol):
            if form.name in mapping:
                return mapping[form.name]
            return form
        if _consp_internal(form):
            new_car = substitute(car(form), mapping)
            tail = cdr(form)
            new_tail = substitute(tail, mapping) if _consp_internal(tail) else tail
            return lisptype.lispCons(new_car, new_tail)
        return form

    # Build macro callable
    parsed = parse_macro_lambda_list(lambda_list)

    def macro_callable(*call_args):
        # raw args as Python list
        raw = list(call_args)
        mapping = {}

        # whole binds entire raw list if requested
        if parsed['whole']:
            mapping[parsed['whole']] = list_to_cons(raw)

        # positional required
        idx = 0
        for name in parsed['required']:
            mapping[name] = raw[idx] if idx < len(raw) else None
            idx += 1

        # optional
        for name, default in parsed['optional']:
            mapping[name] = raw[idx] if idx < len(raw) else default
            if idx < len(raw): idx += 1

        # rest
        if parsed['rest']:
            mapping[parsed['rest']] = list_to_cons(raw[idx:])
            idx = len(raw)

        # keys: scan raw for keyword/value pairs
        for kname, kdefault in parsed['keys']:
            found = False
            for i in range(0, len(raw)):
                keycand = raw[i]
                # Accept either :KEY or KEY as keyword
                if isinstance(keycand, lisptype.LispSymbol) and (keycand.name == kname or keycand.name == (':' + kname)):
                    # take next element as value if present
                    val = raw[i+1] if i+1 < len(raw) else None
                    mapping[kname] = val
                    found = True
                    break
            if not found:
                mapping[kname] = kdefault

        # Perform substitution across body
        substituted = []
        for b in body:
            substituted.append(substitute(b, mapping))
        if len(substituted) == 1:
            return substituted[0]
        progn_sym = lisptype.LispSymbol('PROGN')
        lst = lisptype.NIL
        for f in reversed(substituted):
            lst = lisptype.lispCons(f, lst)
        return lisptype.lispCons(progn_sym, lst)

    setattr(macro_callable, '__is_macro__', True)
    env.add_function(name, macro_callable)
    return name


# Constants and limits
def array_dimension_limit():
    """Array dimension limit."""
    return 2**16


def array_rank_limit():
    """Array rank limit."""
    return 8


def array_total_size_limit():
    """Array total size limit."""
    return 2**24


def call_arguments_limit():
    """Call arguments limit."""
    return 2**16


def multiple_values_limit():
    """Multiple values limit."""
    return 20


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
def do_symbols(var_package_result_decl, *body):
    """Do symbols macro."""
    return None  # Simplified


def do_external_symbols(var_package_result_decl, *body):
    """Do external symbols macro."""
    return None  # Simplified


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
def streamp(obj):
    """Test if object is stream."""
    return hasattr(obj, 'read') or hasattr(obj, 'write')  # Simplified


def broadcast_stream_p(obj):
    """Test if object is broadcast stream."""
    return False  # Simplified


def concatenated_stream_p(obj):
    """Test if object is concatenated stream."""
    return False  # Simplified


@_registry.cl_function('ECHO-STREAM-P')
def echo_stream_p(obj):
    """Test if object is echo stream."""
    return lisptype.lisp_bool(False)  # Simplified


def file_stream_p(obj):
    """Test if object is file stream."""
    return False  # Simplified


def string_stream_p(obj):
    """Test if object is string stream."""
    return False  # Simplified


def synonym_stream_p(obj):
    """Test if object is synonym stream."""
    return False  # Simplified


def two_way_stream_p(obj):
    """Test if object is two-way stream."""
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


def with_package_iterator(name, package_list_form, symbol_types, *body):
    """Iterate over package symbols."""
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


def make_method(*args):
    """Make method."""
    return None


def method_combination_error(format_control, *format_arguments):
    """Method combination error."""
    return None


def method_function(method):
    """Get method function."""
    return None


def method_generic_function(method):
    """Get method generic function."""
    return None


def method_specializers(method):
    """Get method specializers."""
    return []


def method_lambda_list(method):
    """Get method lambda list."""
    return []


def method_qualifiers(method):
    """Get method qualifiers."""
    return []


def next_method_p():
    """Test if next method exists."""
    return False


def no_applicable_method(generic_function, *arguments):
    """No applicable method."""
    return None


def no_next_method(generic_function, method, *arguments):
    """No next method."""
    return None


def reinitialize_instance(instance, *initargs):
    """Reinitialize instance."""
    return instance


def remove_method(generic_function, method):
    """Remove method."""
    return generic_function


def shared_initialize(instance, slot_names, *initargs):
    """Shared initialize."""
    return instance


def slot_boundp(instance, slot_name):
    """Test if slot is bound."""
    return True


def slot_exists_p(instance, slot_name):
    """Test if slot exists."""
    return True


def slot_makunbound(instance, slot_name):
    """Make slot unbound."""
    return instance


def slot_unbound(class_obj, instance, slot_name):
    """Slot unbound."""
    return None


def slot_value(instance, slot_name):
    """Get slot value."""
    return None


def standard_class():
    """Standard class type."""
    return 'STANDARD-CLASS'


def standard_object():
    """Standard object type."""
    return 'STANDARD-OBJECT'


def structure_class():
    """Structure class type."""
    return 'STRUCTURE-CLASS'


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


def invoke_restart(restart, *arguments):
    """Invoke restart."""
    return None


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


def broadcast_stream_streams(broadcast_stream):
    """Get streams from broadcast stream."""
    return []


def concatenated_stream_streams(concatenated_stream):
    """Get streams from concatenated stream."""
    return []


def synonym_stream_symbol(synonym_stream):
    """Get symbol from synonym stream."""
    return synonym_stream


def two_way_stream_input_stream(two_way_stream):
    """Get input stream from two-way stream."""
    return two_way_stream


def two_way_stream_output_stream(two_way_stream):
    """Get output stream from two-way stream."""
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


# Math functions
def rational(number):
    """Convert to rational."""
    return number


def rationalize(number):
    """Rationalize number."""
    return number


@_registry.cl_function('COMPLEX')
def complex_fn(realpart, imagpart=0):
    """Create complex number."""
    return complex(realpart, imagpart)


# More float constants
def least_negative_normalized_double_float():
    """Least negative normalized double float."""
    import sys
    return -sys.float_info.min


def least_negative_normalized_long_float():
    """Least negative normalized long float."""
    import sys
    return -sys.float_info.min


def least_negative_normalized_short_float():
    """Least negative normalized short float."""
    import sys
    return -sys.float_info.min


def least_negative_normalized_single_float():
    """Least negative normalized single float."""
    import sys
    return -sys.float_info.min


def least_positive_normalized_double_float():
    """Least positive normalized double float."""
    import sys
    return sys.float_info.min


def least_positive_normalized_long_float():
    """Least positive normalized long float."""
    import sys
    return sys.float_info.min


def least_positive_normalized_short_float():
    """Least positive normalized short float."""
    import sys
    return sys.float_info.min


def least_positive_normalized_single_float():
    """Least positive normalized single float."""
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


def octets_to_string(octets, **kwargs):
    """Convert octets to string."""
    return str(octets)


def string_to_octets(string, **kwargs):
    """Convert string to octets."""
    return list(string.encode())


def type(object):
    """Get type of object."""
    return type(object).__name__


def upgraded_array_element_type(typespec, environment=None):
    """Upgraded array element type."""
    return 'T'


def upgraded_complex_part_type(typespec, environment=None):
    """Upgraded complex part type."""
    return 'REAL'


def optimize(*args):
    """Optimize declaration."""
    return None


def special(*args):
    """Special declaration."""
    return None


def nil_symbol():
    """NIL symbol."""
    return None


def t_symbol():
    """T symbol."""
    return True


def notinline(*args):
    """Not inline declaration."""
    return None


def inline(*args):
    """Inline declaration."""
    return None


# Macro functions
def macrolet(definitions, *body):
    """Local macros."""
    result = None
    for form in body:
        result = form
    return result


def symbol_macrolet(definitions, *body):
    """Symbol macros."""
    result = None
    for form in body:
        result = form
    return result


def define_compiler_macro(name, lambda_list, *body):
    """Define compiler macro."""
    return name


def dynamic_extent(*args):
    """Dynamic extent declaration."""
    return None


def ftype(*args):
    """Function type declaration."""
    return None


# Additional WITH- macros
def with_accessors(slot_entries, instance_form, *body):
    """With accessors."""
    result = None
    for form in body:
        result = form
    return result


def with_compilation_unit(options, *body):
    """With compilation unit."""
    result = None
    for form in body:
        result = form
    return result


def with_input_from_string(var_string_form, *body):
    """With input from string."""
    result = None
    for form in body:
        result = form
    return result


def with_open_stream(var_stream_form, *body):
    """With open stream."""
    result = None
    for form in body:
        result = form
    return result


def with_output_to_string(var_options, *body):
    """With output to string."""
    result = None
    for form in body:
        result = form
    return result


def with_pprint_logical_block(stream_object_options, *body):
    """With pretty print logical block."""
    result = None
    for form in body:
        result = form
    return result


def with_slots(slot_entries, instance_form, *body):
    """With slots."""
    result = None
    for form in body:
        result = form
    return result


def with_standard_io_syntax(*body):
    """With standard IO syntax."""
    result = None
    for form in body:
        result = form
    return result


def load_logical_pathname_translations(host):
    """Load logical pathname translations."""
    return True


def logical_pathname_translations(host):
    """Get logical pathname translations."""
    return []


# Final sequence functions
def map_into(result_sequence, function, *sequences):
    """Map into result sequence."""
    return result_sequence


def mapcon(function, *lists):
    """Map concatenate."""
    return []


def row_major_aref(array, index):
    """Row major array access."""
    return None
