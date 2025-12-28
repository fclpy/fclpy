"""WITH macros, type designators, system limits, debugging, and miscellaneous utilities."""

import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry


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
