"""Hash tables, arrays, CLOS operations, WITH macros, and miscellaneous utilities."""

import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry


# --- Hash table operations ---
@_registry.cl_function('MAKE-HASH-TABLE')
def make_hash_table(test='EQL', size=16, rehash_size=1.5, rehash_threshold=0.75):
    """Create hash table with specified parameters."""
    table = {
        '__hashmeta__test': str(test).upper(),
        '__hashmeta__rehash_size': rehash_size,
        '__hashmeta__rehash_threshold': rehash_threshold,
    }
    return table


@_registry.cl_function('GETHASH')
def gethash(key, hashtable, default=None):
    """Get value from hash table."""
    if isinstance(hashtable, dict) and key in hashtable:
        return hashtable[key]
    return default


@_registry.cl_function('REMHASH')
def remhash(key, hashtable):
    """Remove entry from hash table."""
    if isinstance(hashtable, dict) and key in hashtable:
        del hashtable[key]
        return lisptype.T
    return lisptype.NIL


@_registry.cl_function('MAPHASH')
def maphash(function, hashtable):
    """Apply function to all hash table entries."""
    if isinstance(hashtable, dict):
        for k, v in list(hashtable.items()):
            if not k.startswith('__hashmeta__'):
                function(k, v)
    return lisptype.NIL


@_registry.cl_function('CLRHASH')
def clrhash(hashtable):
    """Clear all entries from hash table."""
    if isinstance(hashtable, dict):
        meta = {k: v for k, v in hashtable.items() if k.startswith('__hashmeta__')}
        hashtable.clear()
        hashtable.update(meta)
    return hashtable


@_registry.cl_function('SXHASH')
def sxhash(obj):
    """Stable hash for object."""
    try:
        return hash(obj)
    except Exception:
        return hash(str(obj))


@_registry.cl_function('HASH-TABLE-COUNT')
def hash_table_count(table):
    """Count entries in hash table."""
    return len([k for k in table.keys() if not str(k).startswith('__hashmeta__')]) if isinstance(table, dict) else 0


@_registry.cl_function('HASH-TABLE-SIZE')
def hash_table_size(table):
    """Get hash table size."""
    return hash_table_count(table)


@_registry.cl_function('HASH-TABLE-TEST')
def hash_table_test(table):
    """Get hash table test function."""
    return table.get('__hashmeta__test') if isinstance(table, dict) else None


@_registry.cl_function('HASH-TABLE-REHASH-SIZE')
def hash_table_rehash_size(table):
    """Get hash table rehash size."""
    return table.get('__hashmeta__rehash_size') if isinstance(table, dict) else None


@_registry.cl_function('HASH-TABLE-REHASH-THRESHOLD')
def hash_table_rehash_threshold(table):
    """Get hash table rehash threshold."""
    return table.get('__hashmeta__rehash_threshold') if isinstance(table, dict) else None


# --- Array operations ---
@_registry.cl_function('ARRAY-ROW-MAJOR-INDEX')
def array_row_major_index(array, *subscripts):
    """Compute row-major index for array."""
    return 0


@_registry.cl_function('UPGRADED-ARRAY-ELEMENT-TYPE')
def upgraded_array_element_type(typespec, environment=None):
    """Get upgraded array element type."""
    return 'T'


@_registry.cl_function('UPGRADED-COMPLEX-PART-TYPE')
def upgraded_complex_part_type(typespec, environment=None):
    """Get upgraded complex part type."""
    return 'REAL'


@_registry.cl_function('ADJUSTABLE-ARRAY-P')
def adjustable_array_p(array):
    """Test if array is adjustable."""
    return lisptype.NIL


@_registry.cl_function('ROW-MAJOR-AREF')
def row_major_aref(array, index):
    """Get array element by row-major index."""
    return None


# --- Stream operations ---
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
    """Get streams from broadcast stream."""
    return []


@_registry.cl_function('CONCATENATED-STREAM-STREAMS')
def concatenated_stream_streams(concatenated_stream):
    """Get streams from concatenated stream."""
    return []


@_registry.cl_function('SYNONYM-STREAM-SYMBOL')
def synonym_stream_symbol(synonym_stream):
    """Get symbol from synonym stream."""
    return synonym_stream


@_registry.cl_function('TWO-WAY-STREAM-INPUT-STREAM')
def two_way_stream_input_stream(two_way_stream):
    """Get input stream from two-way stream."""
    return two_way_stream


@_registry.cl_function('TWO-WAY-STREAM-OUTPUT-STREAM')
def two_way_stream_output_stream(two_way_stream):
    """Get output stream from two-way stream."""
    return two_way_stream


# --- CLOS class and instance operations ---
@_registry.cl_function('FIND-CLASS')
def find_class(name, errorp=True, environment=None):
    """Find class by name."""
    raise lisptype.LispNotImplementedError("FIND-CLASS")


@_registry.cl_function('MAKE-INSTANCE')
def make_instance(class_designator, *initargs):
    """Make instance of class."""
    return {}


@_registry.cl_function('ALLOCATE-INSTANCE')
def allocate_instance(class_obj, **kwargs):
    """Allocate instance."""
    raise lisptype.LispNotImplementedError("ALLOCATE-INSTANCE")


@_registry.cl_function('INITIALIZE-INSTANCE')
def initialize_instance(instance, *initargs):
    """Initialize instance."""
    return instance


@_registry.cl_function('REINITIALIZE-INSTANCE')
def reinitialize_instance(instance, *initargs):
    """Reinitialize instance."""
    return instance


@_registry.cl_function('SHARED-INITIALIZE')
def shared_initialize(instance, slot_names, *initargs):
    """Shared initialize."""
    return instance


@_registry.cl_function('UPDATE-INSTANCE-FOR-DIFFERENT-CLASS')
def update_instance_for_different_class(previous, current, *initargs):
    """Update instance for different class."""
    return current


@_registry.cl_function('UPDATE-INSTANCE-FOR-REDEFINED-CLASS')
def update_instance_for_redefined_class(instance, added_slots=None, discarded_slots=None, property_list=None, *initargs):
    """Update instance for redefined class."""
    return instance


@_registry.cl_function('CLASS-OF')
def class_of(object):
    """Get class of object."""
    return type(object)


@_registry.cl_function('CLASS-NAME')
def class_name(class_obj):
    """Get class name."""
    return getattr(class_obj, '__name__', str(class_obj))


@_registry.cl_function('CHANGE-CLASS')
def change_class(instance, new_class, *initargs):
    """Change class of instance."""
    return instance


@_registry.cl_function('BUILT-IN-CLASS')
def built_in_class():
    """Get built-in class type."""
    return 'BUILT-IN-CLASS'


@_registry.cl_function('STANDARD-CLASS')
def standard_class():
    """Get standard class type."""
    return 'STANDARD-CLASS'


@_registry.cl_function('STANDARD-OBJECT')
def standard_object():
    """Get standard object type."""
    return 'STANDARD-OBJECT'


@_registry.cl_function('STRUCTURE-CLASS')
def structure_class():
    """Get structure class type."""
    return 'STRUCTURE-CLASS'


@_registry.cl_function('STRUCTURE-OBJECT')
def structure_object():
    """Get structure object type."""
    return 'STRUCTURE-OBJECT'


# --- Slot operations ---
@_registry.cl_function('SLOT-BOUNDP')
def slot_boundp(instance, slot_name):
    """Test if slot is bound."""
    return lisptype.T


@_registry.cl_function('SLOT-EXISTS-P')
def slot_exists_p(instance, slot_name):
    """Test if slot exists."""
    return lisptype.T


@_registry.cl_function('SLOT-MAKUNBOUND')
def slot_makunbound(instance, slot_name):
    """Make slot unbound."""
    return instance


@_registry.cl_function('SLOT-UNBOUND')
def slot_unbound(class_obj, instance, slot_name):
    """Handle unbound slot access."""
    return None


@_registry.cl_function('SLOT-VALUE')
def slot_value(instance, slot_name):
    """Get slot value."""
    return None


@_registry.cl_function('SLOT-MISSING')
def slot_missing(class_obj, instance, slot_name, operation, *args):
    """Handle missing slot."""
    return None


# --- Method operations ---
@_registry.cl_function('FIND-METHOD')
def find_method(generic_function, qualifiers, specializers, errorp=True):
    """Find method in generic function."""
    raise lisptype.LispNotImplementedError("FIND-METHOD")


@_registry.cl_function('ADD-METHOD')
def add_method(generic_function, method):
    """Add method to generic function."""
    raise lisptype.LispNotImplementedError("ADD-METHOD")


@_registry.cl_function('REMOVE-METHOD')
def remove_method(generic_function, method):
    """Remove method from generic function."""
    return generic_function


@_registry.cl_function('DEFMETHOD')
def defmethod(name, *args):
    """Define method (simplified)."""
    return name


@_registry.cl_function('MAKE-METHOD')
def make_method(*args):
    """Create method object."""
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
    """Get method lambda list."""
    return []


@_registry.cl_function('METHOD-QUALIFIERS')
def method_qualifiers(method):
    """Get method qualifiers."""
    return []


@_registry.cl_function('NEXT-METHOD-P')
def next_method_p():
    """Test if next method exists."""
    return lisptype.NIL


@_registry.cl_function('NO-APPLICABLE-METHOD')
def no_applicable_method(generic_function, *arguments):
    """Handle no applicable method."""
    return None


@_registry.cl_function('NO-NEXT-METHOD')
def no_next_method(generic_function, method, *arguments):
    """Handle no next method."""
    return None


@_registry.cl_function('CALL-METHOD')
def call_method(method, next_methods, *args):
    """Call method with next methods."""
    return None


@_registry.cl_function('CALL-NEXT-METHOD')
def call_next_method(*args):
    """Call next method in call chain."""
    return None


@_registry.cl_function('COMPUTE-APPLICABLE-METHODS')
def compute_applicable_methods(generic_function, arguments):
    """Compute applicable methods."""
    return []


@_registry.cl_function('ENSURE-GENERIC-FUNCTION')
def ensure_generic_function(function_name, *options):
    """Ensure generic function exists."""
    return function_name


@_registry.cl_function('GENERIC-FUNCTION-LAMBDA-LIST')
def generic_function_lambda_list(generic_function):
    """Get generic function lambda list."""
    return []


@_registry.cl_function('GENERIC-FUNCTION-METHODS')
def generic_function_methods(generic_function):
    """Get generic function methods."""
    return []


@_registry.cl_function('GENERIC-FUNCTION-NAME')
def generic_function_name(generic_function):
    """Get generic function name."""
    return str(generic_function)


# --- WITH- macros ---
@_registry.cl_function('WITH-ACCESSORS')
def with_accessors(slot_entries, instance_form, *body):
    """WITH-ACCESSORS macro."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WITH-COMPILATION-UNIT')
def with_compilation_unit(options, *body):
    """WITH-COMPILATION-UNIT macro."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WITH-INPUT-FROM-STRING')
def with_input_from_string(var_string_form, *body):
    """WITH-INPUT-FROM-STRING macro."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WITH-OPEN-STREAM')
def with_open_stream(var_stream_form, *body):
    """WITH-OPEN-STREAM macro."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WITH-OUTPUT-TO-STRING')
def with_output_to_string(var_options, *body):
    """WITH-OUTPUT-TO-STRING macro."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WITH-PPRINT-LOGICAL-BLOCK')
def with_pprint_logical_block(stream_object_options, *body):
    """WITH-PPRINT-LOGICAL-BLOCK macro."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WITH-SLOTS')
def with_slots(slot_entries, instance_form, *body):
    """WITH-SLOTS macro."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WITH-STANDARD-IO-SYNTAX')
def with_standard_io_syntax(*body):
    """WITH-STANDARD-IO-SYNTAX macro."""
    result = None
    for form in body:
        result = form
    return result


# --- Miscellaneous utilities ---
@_registry.cl_function('COMPLEX')
def complex_fn(realpart, imagpart=0):
    """Create complex number."""
    return complex(realpart, imagpart)


@_registry.cl_function('LOAD-TIME-VALUE')
def load_time_value(form, read_only_p=None):
    """Load time value."""
    return form


@_registry.cl_function('LOAD')
def load(filespec, **kwargs):
    """Load file (stub)."""
    return filespec


@_registry.cl_function('LOAD-LOGICAL-PATHNAME-TRANSLATIONS')
def load_logical_pathname_translations(host):
    """Load logical pathname translations."""
    return lisptype.T


@_registry.cl_function('LOGICAL-PATHNAME-TRANSLATIONS')
def logical_pathname_translations(host):
    """Get logical pathname translations."""
    return []


def directory(pathspec, **kwargs):
    """List directory."""
    return []


@_registry.cl_function('ENSURE-DIRECTORIES-EXIST')
def ensure_directories_exist(pathspec, **kwargs):
    """Ensure directories exist."""
    return pathspec, lisptype.T


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


@_registry.cl_function('PROCLAIM')
def proclaim(declaration_specifier):
    """Global proclamation."""
    return None


def describe(object, stream=None):
    """Describe object, return structured info."""
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


@_registry.cl_function('INSPECT')
def inspect_object(obj):
    """Inspect object."""
    return describe(obj)


@_registry.cl_function('TYPE')
def type_fn(object):
    """Get type of object."""
    return type(object).__name__


@_registry.cl_function('COPY-TREE')
def copy_tree(obj):
    """Deep copy tree structure."""
    if isinstance(obj, list):
        return [copy_tree(x) for x in obj]
    return obj


@_registry.cl_function('INCF')
def incf(place, delta=1):
    """Increment numeric value."""
    try:
        return place + delta
    except Exception:
        return place


@_registry.cl_function('OCTETS-TO-STRING')
def octets_to_string(octets, **kwargs):
    """Convert octets (bytes) to string."""
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
    """Convert string to octets (bytes)."""
    if not isinstance(string, str):
        string = str(string)
    return list(string.encode())


@_registry.cl_function('GET')
def get(symbol, indicator, default=None):
    """Get property from property list."""
    return default


@_registry.cl_function('RPLACA')
def rplaca(cons, new_car):
    """Replace CAR of cons cell."""
    try:
        cons.car = new_car
    except Exception:
        pass
    return cons


@_registry.cl_function('RPLACD')
def rplacd(cons, new_cdr):
    """Replace CDR of cons cell."""
    try:
        cons.cdr = new_cdr
    except Exception:
        pass
    return cons


@_registry.cl_function('CHAR-BITS-LIMIT')
def char_bits_limit():
    """Character bits limit."""
    return 16


@_registry.cl_function('CHAR-FONT-LIMIT')
def char_font_limit():
    """Character font limit."""
    return 256


@_registry.cl_function('OPTIMIZE')
def optimize(*args):
    """OPTIMIZE declaration (stub)."""
    return None


@_registry.cl_function('SPECIAL')
def special(*args):
    """SPECIAL declaration (stub)."""
    return None


def nil_symbol():
    """Return NIL symbol."""
    return lisptype.NIL


@_registry.cl_function('NIL')
def nil_symbol_function():
    """Compatibility wrapper returning NIL symbol."""
    return lisptype.NIL


def t_symbol():
    """Return T symbol."""
    return lisptype.T


@_registry.cl_function('T')
def t_symbol_function():
    """Compatibility wrapper returning T symbol."""
    return lisptype.T


@_registry.cl_function('MAP-INTO')
def map_into(result_sequence, function, *sequences):
    """MAP-INTO (stub)."""
    return result_sequence


@_registry.cl_function('MAPCON')
def mapcon(function, *lists):
    """MAPCON fallback."""
    return []


# --- Type designators and system constants ---
@_registry.cl_function('KEYWORD')
def keyword_type():
    """Get KEYWORD type designator."""
    return 'KEYWORD'


@_registry.cl_function('INTEGER')
def integer_type():
    """Get INTEGER type designator."""
    return 'INTEGER'


@_registry.cl_function('FIXNUM')
def fixnum_type():
    """Get FIXNUM type designator."""
    return 'FIXNUM'


@_registry.cl_function('DOUBLE-FLOAT')
def double_float_type():
    """Get DOUBLE-FLOAT type designator."""
    return 'DOUBLE-FLOAT'


@_registry.cl_function('SINGLE-FLOAT')
def single_float_type():
    """Get SINGLE-FLOAT type designator."""
    return 'SINGLE-FLOAT'


@_registry.cl_function('SHORT-FLOAT')
def short_float_type():
    """Get SHORT-FLOAT type designator."""
    return 'SHORT-FLOAT'


@_registry.cl_function('EXTENDED-CHAR')
def extended_char_type():
    """Get EXTENDED-CHAR type designator."""
    return 'EXTENDED-CHAR'


@_registry.cl_function('HASH-TABLE')
def hash_table_type():
    """Get HASH-TABLE type designator."""
    return 'HASH-TABLE'


@_registry.cl_function('GENERIC-FUNCTION')
def generic_function_type():
    """Get GENERIC-FUNCTION type designator."""
    return 'GENERIC-FUNCTION'


@_registry.cl_function('FILE-STREAM')
def file_stream_type():
    """Get FILE-STREAM type designator."""
    return 'FILE-STREAM'


@_registry.cl_function('FILE-ERROR')
def file_error_type():
    """Get FILE-ERROR type designator."""
    return 'FILE-ERROR'


@_registry.cl_function('END-OF-FILE')
def end_of_file_type():
    """Get END-OF-FILE type designator."""
    return 'END-OF-FILE'


@_registry.cl_function('FLOATING-POINT-INEXACT')
def floating_point_inexact_type():
    """Get FLOATING-POINT-INEXACT type designator."""
    return 'FLOATING-POINT-INEXACT'


@_registry.cl_function('FLOATING-POINT-INVALID-OPERATION')
def floating_point_invalid_operation_type():
    """Get FLOATING-POINT-INVALID-OPERATION type designator."""
    return 'FLOATING-POINT-INVALID-OPERATION'


@_registry.cl_function('FLOATING-POINT-OVERFLOW')
def floating_point_overflow_type():
    """Get FLOATING-POINT-OVERFLOW type designator."""
    return 'FLOATING-POINT-OVERFLOW'


@_registry.cl_function('FLOATING-POINT-UNDERFLOW')
def floating_point_underflow_type():
    """Get FLOATING-POINT-UNDERFLOW type designator."""
    return 'FLOATING-POINT-UNDERFLOW'


@_registry.cl_function('ARITHMETIC-ERROR-OPERANDS')
def arithmetic_error_operands(condition):
    """Get operands from arithmetic error condition."""
    return []


@_registry.cl_function('ARITHMETIC-ERROR-OPERATION')
def arithmetic_error_operation(condition):
    """Get operation from arithmetic error condition."""
    return None


@_registry.cl_function('FILE-ERROR-PATHNAME')
def file_error_pathname(condition):
    """Get pathname from file error condition."""
    return None


# --- Multiple values operations ---
@_registry.cl_function('MULTIPLE-VALUE-BIND')
def multiple_value_bind(specs, values_form, *body):
    """Multiple value bind macro."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('MULTIPLE-VALUE-CALL')
def multiple_value_call(function, *forms):
    """Multiple value call."""
    return function(*forms) if callable(function) else None


# --- Package operations (advanced) ---
@_registry.cl_function('MAKE-PACKAGE')
def make_package(name, nicknames=None, use=None):
    """Create a new package."""
    return lisptype.make_package(str(name))


@_registry.cl_function('PACKAGE-NAME')
def package_name(package):
    """Get package name."""
    return package.name if isinstance(package, lisptype.Package) else None


@_registry.cl_function('PACKAGE-NICKNAMES')
def package_nicknames(package):
    """Get package nicknames."""
    return getattr(package, 'nicknames', [])


@_registry.cl_function('RENAME-PACKAGE')
def rename_package(package, new_name, new_nicknames=None):
    """Rename a package."""
    if isinstance(package, lisptype.Package):
        package.name = str(new_name)
    return package


@_registry.cl_function('PACKAGE-USE-LIST')
def package_use_list(package):
    """Get packages this package uses."""
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return []
    return list(pkg.use_list)


@_registry.cl_function('PACKAGE-USED-BY-LIST')
def package_used_by_list(package):
    """Get packages that use this package."""
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return []
    used_by = []
    for p in list({id(p): p for p in state.packages.values()}.values()):
        if pkg in getattr(p, 'use_list', []):
            used_by.append(p)
    return used_by


@_registry.cl_function('PACKAGE-SHADOWING-SYMBOLS')
def package_shadowing_symbols(package):
    """Get shadowing symbols in package."""
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return []
    syms = []
    for name in getattr(pkg, 'shadowing_symbols', set()):
        s = pkg.symbols.get(name)
        if s is not None:
            syms.append(s)
    return syms


@_registry.cl_function('LIST-ALL-PACKAGES')
def list_all_packages():
    """List all known packages."""
    unique = {id(p): p for p in state.packages.values()}
    return list(unique.values())


@_registry.cl_function('UNINTERN')
def unintern(symbol, package=None):
    """Remove symbol from package."""
    if not isinstance(symbol, str) and hasattr(symbol, 'name'):
        name = symbol.name
    else:
        name = str(symbol)
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return lisptype.NIL
    name = name.upper()
    if name in pkg.symbols:
        del pkg.symbols[name]
        pkg.external_symbols.discard(name)
        pkg.shadowing_symbols.discard(name)
        return lisptype.T
    return lisptype.NIL


@_registry.cl_function('UNEXPORT')
def unexport(symbols, package=None):
    """Unexport symbols from package."""
    if not isinstance(symbols, (list, tuple)):
        symbols = [symbols]
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return lisptype.NIL
    for s in symbols:
        name = s.name if hasattr(s, 'name') else str(s)
        pkg.external_symbols.discard(name.upper())
    return lisptype.T


@_registry.cl_function('SHADOWING-IMPORT')
def shadowing_import(symbols, package=None):
    """Shadowing import symbols."""
    return lisptype.T


@_registry.cl_function('SHADOW')
def shadow(symbols, package=None):
    """Create shadowing symbols in package."""
    return lisptype.T


@_registry.cl_function('USE-PACKAGE')
def use_package(packages, package=None):
    """Install packages into use-list."""
    if not isinstance(packages, (list, tuple)):
        packages = [packages]
    target = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if target is None:
        return lisptype.NIL
    for p in packages:
        pkgobj = p if isinstance(p, lisptype.Package) else lisptype.find_package(str(p))
        if pkgobj is None:
            pkgobj = lisptype.make_package(str(p))
        if pkgobj not in target.use_list:
            target.use_list.append(pkgobj)
    return lisptype.T


@_registry.cl_function('UNUSE-PACKAGE')
def unuse_package(packages, package=None):
    """Remove packages from use-list."""
    if not isinstance(packages, (list, tuple)):
        packages = [packages]
    target = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if target is None:
        return lisptype.NIL
    for p in packages:
        pkgobj = p if isinstance(p, lisptype.Package) else lisptype.find_package(str(p))
        if pkgobj in target.use_list:
            target.use_list.remove(pkgobj)
    return lisptype.T


# --- Macro expansion ---
@_registry.cl_function('MACROEXPAND')
def macroexpand(form, environment=None):
    """Expand macros fully."""
    return form, lisptype.NIL


@_registry.cl_function('MACROEXPAND-1')
def macroexpand_1(form, environment=None):
    """Expand macros once."""
    return form, lisptype.NIL


# --- System limits and constants ---
@_registry.cl_function('ARRAY-DIMENSION-LIMIT')
def array_dimension_limit():
    """Maximum array dimension."""
    return 1024


@_registry.cl_function('ARRAY-RANK-LIMIT')
def array_rank_limit():
    """Maximum array rank."""
    return 8


@_registry.cl_function('ARRAY-TOTAL-SIZE-LIMIT')
def array_total_size_limit():
    """Maximum total array size."""
    return 1024 * 1024


@_registry.cl_function('CALL-ARGUMENTS-LIMIT')
def call_arguments_limit():
    """Maximum function arguments."""
    return 64


@_registry.cl_function('MULTIPLE-VALUES-LIMIT')
def multiple_values_limit():
    """Maximum multiple values."""
    return 64


@_registry.cl_function('CHAR-CODE-LIMIT')
def char_code_limit():
    """Maximum character code."""
    return 1114112


# --- Symbol iteration ---
@_registry.cl_function('DO-SYMBOLS')
def do_symbols(spec, *body):
    """Iterate over symbols in package."""
    return lisptype.NIL


@_registry.cl_function('DO-EXTERNAL-SYMBOLS')
def do_external_symbols(spec, *body):
    """Iterate over external symbols in package."""
    return lisptype.NIL


@_registry.cl_function('DO-ALL-SYMBOLS')
def do_all_symbols(spec, *body):
    """Iterate over all symbols in all packages."""
    return lisptype.NIL


@_registry.cl_function('WITH-PACKAGE-ITERATOR')
def with_package_iterator(spec, packages, *body):
    """With package iterator macro."""
    return lisptype.NIL


# --- Declaration and definition macros ---
@_registry.cl_function('DECLAIM')
def declaim(*declarations):
    """Global declaration."""
    return lisptype.NIL


@_registry.cl_function('DECLARE')
def declare(*declarations):
    """Local declaration."""
    return lisptype.NIL


@_registry.cl_function('DEFCLASS')
def defclass(name, superclasses, slots, *options):
    """Define class."""
    return name


@_registry.cl_function('DEFCONSTANT')
def defconstant(name, value, doc=None):
    """Define constant."""
    return name


@_registry.cl_function('DEFGENERIC')
def defgeneric(name, lambda_list, *options):
    """Define generic function."""
    return name


@_registry.cl_function('DEFPACKAGE')
def defpackage(name, *options):
    """Define package."""
    return name


@_registry.cl_function('DEFSTRUCT')
def defstruct(name, *slots):
    """Define structure."""
    return name


@_registry.cl_function('DEFTYPE')
def deftype(name, lambda_list, *body):
    """Define type."""
    return name


@_registry.cl_function('DEFPARAMETER')
def defparameter(name, value, doc=None):
    """Define parameter."""
    return name


# --- Stream type predicates ---
@_registry.cl_function('ECHO-STREAM-P')
def echo_stream_p(obj):
    """Test if object is echo stream."""
    return lisptype.NIL


@_registry.cl_function('BROADCAST-STREAM-P')
def broadcast_stream_p(obj):
    """Test if object is broadcast stream."""
    return lisptype.NIL


@_registry.cl_function('CONCATENATED-STREAM-P')
def concatenated_stream_p(obj):
    """Test if object is concatenated stream."""
    return lisptype.NIL


@_registry.cl_function('FILE-STREAM-P')
def file_stream_p(obj):
    """Test if object is file stream."""
    return lisptype.NIL


@_registry.cl_function('STRING-STREAM-P')
def string_stream_p(obj):
    """Test if object is string stream."""
    return lisptype.NIL


@_registry.cl_function('SYNONYM-STREAM-P')
def synonym_stream_p(obj):
    """Test if object is synonym stream."""
    return lisptype.NIL


@_registry.cl_function('TWO-WAY-STREAM-P')
def two_way_stream_p(obj):
    """Test if object is two-way stream."""
    return lisptype.NIL


# --- Debugging and development tools ---
@_registry.cl_function('BREAK')
def break_fn(format_string=None, *args):
    """Break to debugger."""
    return None


@_registry.cl_function('CONTINUE')
def continue_fn():
    """Continue from break."""
    return None


@_registry.cl_function('ED')
def ed(file=None):
    """Edit file."""
    return file


@_registry.cl_function('DRIBBLE')
def dribble(file=None):
    """Dribble output to file."""
    return file


@_registry.cl_function('DISASSEMBLE')
def disassemble(object):
    """Disassemble compiled code."""
    return None


@_registry.cl_function('ROOM')
def room(option=None):
    """Show memory status."""
    return None


@_registry.cl_function('STEP')
def step(form):
    """Step through evaluation."""
    return form


@_registry.cl_function('TRACE')
def trace(*fns):
    """Trace function calls."""
    return list(fns)


@_registry.cl_function('UNTRACE')
def untrace(*fns):
    """Untrace function calls."""
    return list(fns)


@_registry.cl_function('PROVIDE')
def provide(module):
    """Provide module."""
    return module


@_registry.cl_function('REQUIRE')
def require(module):
    """Require module."""
    return module


# --- Form utilities ---
@_registry.cl_function('MAKE-LOAD-FORM')
def make_load_form(object, environment=None):
    """Make load form."""
    return object


@_registry.cl_function('MAKE-LOAD-FORM-SAVING-SLOTS')
def make_load_form_saving_slots(object, slot_names=None):
    """Make load form saving slots."""
    return object


@_registry.cl_function('FILL-POINTER')
def fill_pointer(vector):
    """Get/set fill pointer of vector."""
    return None


# --- Documentation ---
@_registry.cl_function('DOCUMENTATION')
def documentation(symbol, doc_type=None):
    """Get documentation for symbol."""
    if not isinstance(symbol, lisptype.LispSymbol):
        return lisptype.NIL
    if doc_type is None or (isinstance(doc_type, lisptype.LispSymbol) and doc_type.name == 'FUNCTION'):
        if hasattr(symbol, 'plist') and isinstance(symbol.plist, dict):
            doc = symbol.plist.get('DOCUMENTATION')
            if doc:
                return doc
    elif isinstance(doc_type, lisptype.LispSymbol):
        doc_type_name = doc_type.name.upper()
        if doc_type_name in ('VARIABLE', 'TYPE', 'STRUCTURE', 'SETF'):
            pass
    return lisptype.NIL


def get_optimization_policy(env=None):
    """Get the current optimization policy from the environment.
    
    Returns a dictionary with keys: speed, safety, debug, compilation-speed, space
    Each value is 0-3 (minimum to maximum).
    """
    if env is None:
        try:
            import fclpy.state as _state
            import fclpy.lispenv as lispenv
            env = _state.current_environment
            if env is None:
                env = lispenv.setup_standard_environment()
        except Exception:
            pass
    
    # Find root environment if we have one
    if env is not None:
        root_env = env
        while root_env.parent is not None:
            root_env = root_env.parent
        
        # Return optimization policy if it exists
        if hasattr(root_env, '_optimization_policy'):
            return root_env._optimization_policy
    
    # Return default policy
    return {
        'speed': 1,
        'safety': 1,
        'debug': 1,
        'compilation-speed': 1,
        'space': 1
    }


def is_variable_special(symbol, env=None):
    """Check if a symbol is declared as special.
    
    Returns True if the symbol is in the global special variables list.
    """
    if not isinstance(symbol, lisptype.LispSymbol):
        return False
    
    if env is None:
        try:
            import fclpy.state as _state
            import fclpy.lispenv as lispenv
            env = _state.current_environment
            if env is None:
                env = lispenv.setup_standard_environment()
        except Exception:
            pass
    
    if env is None:
        return False
    
    # Find root environment
    root_env = env
    while root_env.parent is not None:
        root_env = root_env.parent
    
    # Check if symbol is in special variables
    if hasattr(root_env, '_special_variables'):
        return symbol.name in root_env._special_variables
    
    return False


__all__ = [
    'make_hash_table',
    'gethash',
    'remhash',
    'maphash',
    'clrhash',
    'sxhash',
    'hash_table_count',
    'hash_table_size',
    'hash_table_test',
    'hash_table_rehash_size',
    'hash_table_rehash_threshold',
    'array_row_major_index',
    'upgraded_array_element_type',
    'upgraded_complex_part_type',
    'adjustable_array_p',
    'row_major_aref',
    'echo_stream_input_stream',
    'echo_stream_output_stream',
    'broadcast_stream_streams',
    'concatenated_stream_streams',
    'synonym_stream_symbol',
    'two_way_stream_input_stream',
    'two_way_stream_output_stream',
    'find_class',
    'make_instance',
    'allocate_instance',
    'initialize_instance',
    'reinitialize_instance',
    'shared_initialize',
    'update_instance_for_different_class',
    'update_instance_for_redefined_class',
    'class_of',
    'class_name',
    'change_class',
    'built_in_class',
    'standard_class',
    'standard_object',
    'structure_class',
    'structure_object',
    'slot_boundp',
    'slot_exists_p',
    'slot_makunbound',
    'slot_unbound',
    'slot_value',
    'slot_missing',
    'find_method',
    'add_method',
    'remove_method',
    'defmethod',
    'make_method',
    'method_function',
    'method_generic_function',
    'method_specializers',
    'method_lambda_list',
    'method_qualifiers',
    'next_method_p',
    'no_applicable_method',
    'no_next_method',
    'call_method',
    'call_next_method',
    'compute_applicable_methods',
    'ensure_generic_function',
    'generic_function_lambda_list',
    'generic_function_methods',
    'generic_function_name',
    'with_accessors',
    'with_compilation_unit',
    'with_input_from_string',
    'with_open_stream',
    'with_output_to_string',
    'with_pprint_logical_block',
    'with_slots',
    'with_standard_io_syntax',
    'complex_fn',
    'load_time_value',
    'load',
    'load_logical_pathname_translations',
    'logical_pathname_translations',
    'directory',
    'ensure_directories_exist',
    'define_setf_expander',
    'defsetf',
    'get_setf_expansion',
    'proclaim',
    'describe',
    'inspect_object',
    'type_fn',
    'copy_tree',
    'incf',
    'octets_to_string',
    'string_to_octets',
    'get',
    'rplaca',
    'rplacd',
    'char_bits_limit',
    'char_font_limit',
    'optimize',
    'special',
    'nil_symbol',
    'nil_symbol_function',
    't_symbol',
    't_symbol_function',
    'map_into',
    'mapcon',
    'keyword_type',
    'integer_type',
    'fixnum_type',
    'double_float_type',
    'single_float_type',
    'short_float_type',
    'extended_char_type',
    'hash_table_type',
    'generic_function_type',
    'file_stream_type',
    'file_error_type',
    'end_of_file_type',
    'floating_point_inexact_type',
    'floating_point_invalid_operation_type',
    'floating_point_overflow_type',
    'floating_point_underflow_type',
    'arithmetic_error_operands',
    'arithmetic_error_operation',
    'file_error_pathname',
    'multiple_value_bind',
    'multiple_value_call',
    'make_package',
    'package_name',
    'package_nicknames',
    'rename_package',
    'package_use_list',
    'package_used_by_list',
    'package_shadowing_symbols',
    'list_all_packages',
    'unintern',
    'unexport',
    'shadowing_import',
    'shadow',
    'use_package',
    'unuse_package',
    'macroexpand',
    'macroexpand_1',
    'array_dimension_limit',
    'array_rank_limit',
    'array_total_size_limit',
    'call_arguments_limit',
    'multiple_values_limit',
    'char_code_limit',
    'do_symbols',
    'do_external_symbols',
    'do_all_symbols',
    'with_package_iterator',
    'declaim',
    'declare',
    'defclass',
    'defconstant',
    'defgeneric',
    'defpackage',
    'defstruct',
    'deftype',
    'defparameter',
    'echo_stream_p',
    'broadcast_stream_p',
    'concatenated_stream_p',
    'file_stream_p',
    'string_stream_p',
    'synonym_stream_p',
    'two_way_stream_p',
    'break_fn',
    'continue_fn',
    'ed',
    'dribble',
    'disassemble',
    'room',
    'step',
    'trace',
    'untrace',
    'provide',
    'require',
    'make_load_form',
    'make_load_form_saving_slots',
    'fill_pointer',
    'documentation',
    'get_optimization_policy',
    'is_variable_special',
]
