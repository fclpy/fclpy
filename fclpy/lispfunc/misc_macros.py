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


# WITH-INPUT-FROM-STRING, WITH-OUTPUT-TO-STRING and WITH-OPEN-STREAM are
# implemented as real macro expanders in evaluation_special_forms.py. They
# used to be `cl_function` stubs here that returned their last body form
# without evaluating anything; because `cl_function` evaluates arguments
# eagerly, the binding spec `(stream)` was evaluated as a call to a function
# named STREAM. Keeping a second registration would silently win or lose
# depending on module import order (standing rule 3).


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
# ASSERT is a macro expander in evaluation_special_forms.py, not a
# `cl_function` here -- see that module for why (its `(place*)` list is
# syntax, never evaluated, and `cl_function` evaluates every argument
# eagerly).


@_registry.cl_function('COMPLEX')
def complex_fn(realpart, imagpart=0):
    """Create complex number."""
    return complex(realpart, imagpart)


@_registry.cl_function('LOAD-TIME-VALUE')
def load_time_value(form, read_only_p=None):
    """Load time value."""
    return form


@_registry.cl_function('LOAD')
def load(filespec, verbose=None, print_p=None, if_does_not_exist=None, 
         external_format=None):
    """Load a Lisp file.
    
    Args:
        filespec: Path to file (string or pathname)
        verbose: If true, print loading messages
        print_p: If true, print values of evaluated forms

        if_does_not_exist: :ERROR (default), :LOAD (try anyway), or NIL (return NIL)
        external_format: Character encoding (not fully supported)
    
    Returns:
        T if successful, NIL otherwise
    """
    import os
    import fclpy.state as state
    from fclpy.lispfunc.pathnames import Pathname, truename
    
    # Get the environment
    env = state.current_environment
    if env is None:
        raise lisptype.LispNotImplementedError("LOAD: No environment available")
    
    # Convert filespec to path string
    if isinstance(filespec, Pathname):
        path_str = filespec.original
    else:
        path_str = str(filespec)
    
    # If path is relative, resolve it against various directories in priority order:
    # 1. LISP_CWD env var (allows Python CWD and Lisp CWD to differ)
    # 2. *LOAD-TRUENAME* directory (for nested loads relative to current file)
    # 3. *DEFAULT-PATHNAME-DEFAULTS*
    # 4. Python's CWD as last resort
    if not os.path.isabs(path_str):
        resolved = False
        
        # First, try LISP_CWD environment variable
        lisp_cwd = os.environ.get('LISP_CWD')
        if lisp_cwd:
            candidate = os.path.join(lisp_cwd, path_str)
            candidate = os.path.normpath(candidate)
            if os.path.exists(candidate):
                path_str = candidate
                resolved = True
        
        # Second, try to resolve against *LOAD-TRUENAME* (directory of currently loading file)
        if not resolved:
            load_truename_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-TRUENAME*')
            load_truename = env.find_variable(load_truename_sym)
            
            if load_truename and load_truename is not lisptype.NIL:
                if isinstance(load_truename, Pathname):
                    # Get the directory containing the currently loading file
                    current_file_path = load_truename.original
                    current_dir = os.path.dirname(current_file_path)
                    if current_dir:
                        candidate = os.path.join(current_dir, path_str)
                        candidate = os.path.normpath(candidate)
                        if os.path.exists(candidate):
                            path_str = candidate
                            resolved = True
        
        # Third, try *DEFAULT-PATHNAME-DEFAULTS*
        if not resolved:
            default_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*DEFAULT-PATHNAME-DEFAULTS*')
            default_pathname = env.find_variable(default_sym)
            
            if default_pathname and default_pathname is not lisptype.NIL:
                # Merge with default pathname
                if isinstance(default_pathname, Pathname):
                    # Check if the default pathname is a directory
                    default_path = default_pathname.original
                    if os.path.isdir(default_path):
                        # It's a directory - use it directly
                        default_dir = default_path
                    else:
                        # It's a file - use its parent directory
                        default_dir = os.path.dirname(default_path)
                    if default_dir:
                        path_str = os.path.join(default_dir, path_str)
                        path_str = os.path.normpath(path_str)
    
    # Handle if-does-not-exist: try .lsp if .fasl not found (for FCLpy)
    if not os.path.exists(path_str):
        # If looking for a .fasl file, try .lsp instead (FCLpy doesn't compile)
        if path_str.endswith('.fasl'):
            source_path = path_str[:-5] + '.lsp'
            if os.path.exists(source_path):
                path_str = source_path
        
        # Still not found?
        if not os.path.exists(path_str):
            if if_does_not_exist is lisptype.NIL or if_does_not_exist is None:
                # Default behavior - raise error
                raise FileNotFoundError(f"LOAD: File not found: {path_str}")
            elif if_does_not_exist == lisptype.NIL:
                return lisptype.NIL
    
    # Create pathname objects
    pathname_obj = Pathname(path_str)
    try:
        truename_obj = truename(pathname_obj)
    except FileNotFoundError:
        truename_obj = pathname_obj  # Fall back to pathname if truename fails
    
    # Save old values of load variables
    load_truename_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-TRUENAME*')
    load_pathname_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-PATHNAME*')
    
    old_truename = env.find_variable(load_truename_sym)
    old_pathname = env.find_variable(load_pathname_sym)
    
    try:
        # Set load variables for this file
        env.set_variable(load_truename_sym, truename_obj)
        env.set_variable(load_pathname_sym, pathname_obj)
        
        # Actually load the file using runtime
        import fclpy.runtime as runtime
        is_verbose = verbose is True or verbose is lisptype.T
        result = runtime.load_and_evaluate_file(path_str, env, verbose=is_verbose)
        
        return result
    finally:
        # Restore old values
        if old_truename is not None:
            env.set_variable(load_truename_sym, old_truename)
        else:
            env.set_variable(load_truename_sym, lisptype.NIL)
        
        if old_pathname is not None:
            env.set_variable(load_pathname_sym, old_pathname)
        else:
            env.set_variable(load_pathname_sym, lisptype.NIL)


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


## `DEFSETF` is a special form handled by the evaluator; do not
## register it as a regular function here. Arguments should not be evaluated.
def defsetf(access_fn, update_fn, documentation=None):
    """Define setf function (stub kept for reference)."""
    return access_fn


@_registry.cl_function('GET-SETF-EXPANSION')
def get_setf_expansion(place, environment=None):
    """Get setf expansion."""
    return [], [], [], place, place


@_registry.cl_special('PROCLAIM')
def proclaim(form):
    """Handle PROCLAIM as a special form so declaration specifiers
    are not evaluated. Treat as a no-op and return NIL."""
    return lisptype.NIL


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


@_registry.cl_function('DESCRIBE-OBJECT')
def describe_object(obj, stream=None):
    """Print description of object to stream.
    
    This is the generic function called by DESCRIBE. Users can add methods
    for their own classes to customize the description output.
    """
    if stream is None:
        stream = True  # *standard-output*
    info = describe(obj)
    # Format output (simplified version)
    return info


@_registry.cl_function('PRINT-OBJECT')
def print_object(obj, stream=None):
    """Print object to stream.
    
    This is the primary interface to the Lisp printer. Users can add methods
    for their own classes to customize print output.
    """
    if stream is None:
        stream = True  # *standard-output*
    # Return the string representation
    return repr(obj) if hasattr(obj, '__repr__') else str(obj)


@_registry.cl_function('CONDITION-P')
def condition_p(obj):
    """Test if object is a condition."""
    from fclpy.lisptype_extended import Condition
    return lisptype.lisp_bool(isinstance(obj, Condition))


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


# Note: INCF is now implemented as a special form in evaluation_special_forms.py
# The old function-based INCF has been removed as it didn't properly modify places.


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
def get(*args):
    """Get property from property list.

    Signature: (GET SYMBOL INDICATOR &OPTIONAL DEFAULT)
    Supports SYMBOL.plist stored as a Python dict or a Lisp cons-list.
    """
    if len(args) < 2 or len(args) > 3:
        raise lisptype.LispProgramError(
            f"GET: wrong number of arguments (got {len(args)}, expected 2-3)"
        )
    symbol = args[0]
    indicator = args[1]
    default = args[2] if len(args) == 3 else lisptype.NIL

    # Retrieve plist from symbol (if available)
    plist = getattr(symbol, 'plist', lisptype.NIL)

    # If stored as a Python dict, use direct lookup
    if isinstance(plist, dict):
        return plist.get(indicator, default)

    # For cons-list style plists, delegate to GETF implementation
    try:
        from .core import getf

        return getf(plist, indicator, default)
    except Exception:
        # Fallback: return default when in doubt
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
def t_symbol_function(*args):
    """Compatibility wrapper returning T symbol.
    
    Accepts optional arguments to handle cases where T is used as a type
    specifier in function calls like (coerce x 't).
    """
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


# --- Type designators from ANSI target list ---
@_registry.cl_function('BIT-VECTOR')
def bit_vector_type():
    """Get BIT-VECTOR type designator."""
    return 'BIT-VECTOR'


@_registry.cl_function('SIMPLE-BIT-VECTOR')
def simple_bit_vector_type():
    """Get SIMPLE-BIT-VECTOR type designator."""
    return 'SIMPLE-BIT-VECTOR'


@_registry.cl_function('SIMPLE-VECTOR')
def simple_vector_type():
    """Get SIMPLE-VECTOR type designator."""
    return 'SIMPLE-VECTOR'


@_registry.cl_function('SIMPLE-STRING')
def simple_string_type():
    """Get SIMPLE-STRING type designator."""
    return 'SIMPLE-STRING'


@_registry.cl_function('TYPE-ERROR')
def type_error_type():
    """Get TYPE-ERROR type designator."""
    return 'TYPE-ERROR'


@_registry.cl_function('SIMPLE-ERROR')
def simple_error_type():
    """Get SIMPLE-ERROR type designator."""
    return 'SIMPLE-ERROR'


@_registry.cl_function('CLASS')
def class_type():
    """Get CLASS type designator."""
    return 'CLASS'


@_registry.cl_function('METHOD-COMBINATION')
def method_combination_type():
    """Get METHOD-COMBINATION type designator."""
    return 'METHOD-COMBINATION'


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
# These are special forms handled by the evaluator
@_registry.cl_special('DO-SYMBOLS')
def do_symbols_special(form):
    """Iterate over symbols in package. Handled by evaluator."""
    raise lisptype.LispNotImplementedError('DO-SYMBOLS', 'special form handled by evaluator')


@_registry.cl_special('DO-EXTERNAL-SYMBOLS')
def do_external_symbols_special(form):
    """Iterate over external symbols in package. Handled by evaluator."""
    raise lisptype.LispNotImplementedError('DO-EXTERNAL-SYMBOLS', 'special form handled by evaluator')


@_registry.cl_special('DO-ALL-SYMBOLS')
def do_all_symbols_special(form):
    """Iterate over all symbols in all packages. Handled by evaluator."""
    raise lisptype.LispNotImplementedError('DO-ALL-SYMBOLS', 'special form handled by evaluator')


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
def defstruct(name_and_options, *slots):
    """Define a structure type.
    
    Supports:
    - (DEFSTRUCT name slot...)
    - (DEFSTRUCT (name option...) slot...)
    
    Options:
    - :CONC-NAME prefix - prefix for slot accessors (NIL for no prefix)
    - :CONSTRUCTOR name - name of constructor function
    - :COPIER name - name of copier function
    - :PREDICATE name - name of predicate function
    - :INCLUDE parent - inherit from another structure
    
    Slots can be:
    - symbol - just the slot name
    - (slot-name default-value) - slot with default
    - (slot-name default-value :type type :read-only bool) - with options
    """
    import fclpy.state as state
    from fclpy.lispfunc.core import car, cdr, _consp_internal
    
    env = state.current_environment
    
    # Parse name and options
    if isinstance(name_and_options, lisptype.LispSymbol):
        struct_name = name_and_options
        conc_name = struct_name.name + '-'
        constructor_name = 'MAKE-' + struct_name.name
        copier_name = 'COPY-' + struct_name.name
        predicate_name = struct_name.name + '-P'
        include_parent = None
    elif _consp_internal(name_and_options):
        struct_name = car(name_and_options)
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
                    if opt_value is None or opt_value == lisptype.NIL:
                        conc_name = ''  # No prefix
                    elif isinstance(opt_value, lisptype.LispSymbol):
                        conc_name = opt_value.name
                    else:
                        conc_name = str(opt_value)
                elif opt_name_str == 'CONSTRUCTOR' or opt_name_str == ':CONSTRUCTOR':
                    if opt_value is None or opt_value == lisptype.NIL:
                        constructor_name = None
                    elif isinstance(opt_value, lisptype.LispSymbol):
                        constructor_name = opt_value.name
                elif opt_name_str == 'COPIER' or opt_name_str == ':COPIER':
                    if opt_value is None or opt_value == lisptype.NIL:
                        copier_name = None
                    elif isinstance(opt_value, lisptype.LispSymbol):
                        copier_name = opt_value.name
                elif opt_name_str == 'PREDICATE' or opt_name_str == ':PREDICATE':
                    if opt_value is None or opt_value == lisptype.NIL:
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
    
    # Parse slot definitions
    slot_defs = []  # List of (slot_name, default_value)
    for slot in slots:
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
    
    # Create the structure class
    struct_class_name = struct_name.name if isinstance(struct_name, lisptype.LispSymbol) else str(struct_name)
    
    # Define the structure as a simple dictionary-based type
    class StructureInstance:
        def __init__(self, **kwargs):
            self._struct_type = struct_class_name
            self._slots = {}
            # Initialize with defaults
            for slot_name, default_val in slot_defs:
                self._slots[slot_name] = default_val
            # Override with provided values
            for key, value in kwargs.items():
                if key.upper() in [s[0].upper() for s in slot_defs]:
                    for slot_name, _ in slot_defs:
                        if slot_name.upper() == key.upper():
                            self._slots[slot_name] = value
                            break
        
        def __repr__(self):
            slot_values = ' '.join(f':{k} {v}' for k, v in self._slots.items())
            return f'#S({struct_class_name} {slot_values})'
        
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
        def make_structure(**kwargs):
            return StructureInstance(**kwargs)
        
        # Handle keyword arguments for constructor
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
            return StructureInstance(**result_kwargs)
        
        constructor_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol(constructor_name)
        env.add_function(constructor_sym, constructor_wrapper)
    
    # Create copier function
    if copier_name:
        def copy_structure(struct):
            if not isinstance(struct, StructureInstance):
                raise TypeError(f"Not a {struct_class_name}: {struct}")
            new_struct = StructureInstance()
            new_struct._slots = dict(struct._slots)
            return new_struct
        
        copier_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol(copier_name)
        env.add_function(copier_sym, copy_structure)
    
    # Create predicate function
    if predicate_name:
        def is_structure(obj):
            if isinstance(obj, StructureInstance) and obj._struct_type == struct_class_name:
                return lisptype.T
            return lisptype.NIL
        
        predicate_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol(predicate_name)
        env.add_function(predicate_sym, is_structure)
    
    # Create accessor functions for each slot
    for slot_name, _ in slot_defs:
        accessor_name = conc_name + slot_name
        
        # Create getter
        def make_getter(sn):
            def getter(struct):
                if isinstance(struct, StructureInstance):
                    return struct.get_slot(sn)
                raise TypeError(f"Not a {struct_class_name}: {struct}")
            return getter
        
        accessor_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol(accessor_name)
        env.add_function(accessor_sym, make_getter(slot_name))
        
        # Create setter (for SETF)
        def make_setter(sn):
            def setter(struct, value):
                if isinstance(struct, StructureInstance):
                    struct.set_slot(sn, value)
                    return value
                raise TypeError(f"Not a {struct_class_name}: {struct}")
            return setter
        
        setter_name = 'SET-' + accessor_name
        setter_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol(setter_name)
        env.add_function(setter_sym, make_setter(slot_name))
    
    return struct_name


## `DEFTYPE` is a special form handled by the evaluator;
## do not register it as a regular function here.
def deftype(name, lambda_list, *body):
    """Define type (stub kept for reference)."""
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
    # Note: 'incf' removed - now a special form in evaluation_special_forms.py
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
    # do_symbols, do_external_symbols, do_all_symbols are now special forms
    'with_package_iterator',
    'declaim',
    'declare',
    # NOTE: defclass, defgeneric, defpackage, defstruct are NOT exported here
    # because they are stubs that would override real implementations from classes.py
    'defconstant',
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
    'documentation',
    'get_optimization_policy',
    'is_variable_special',
]
