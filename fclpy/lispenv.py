import fclpy.lisptype
import fclpy.state as state
from fclpy.cl_symbol_names import CL_SYMBOL_NAMES


# Initialize the central environment object in state if not present
if state.current_environment is None:
    state.current_environment = fclpy.lisptype.Environment()

# Backward-compatible module-level name used across the codebase
current_environment = state.current_environment


# CLHS Figure 25-1, the standard variables. Every one of these is proclaimed
# special, which is what makes a binding form bind it in the symbol's value
# cell -- the one home a global variable has (see `Environment`) and the only
# one the Python-side readers in printer.py / readtable.py / streams.py can
# reach. This is the authoritative list rather than a record of which
# variables the bootstrap below happens to give a value to: proclaiming a
# variable special does not require it to be bound, so the two cannot drift.
STANDARD_SPECIAL_VARIABLES = (
    '*BREAK-ON-SIGNALS*', '*COMPILE-FILE-PATHNAME*', '*COMPILE-FILE-TRUENAME*',
    '*COMPILE-PRINT*', '*COMPILE-VERBOSE*', '*DEBUG-IO*', '*DEBUGGER-HOOK*',
    '*DEFAULT-PATHNAME-DEFAULTS*', '*ERROR-OUTPUT*', '*FEATURES*',
    '*GENSYM-COUNTER*', '*LOAD-PATHNAME*', '*LOAD-PRINT*', '*LOAD-TRUENAME*',
    '*LOAD-VERBOSE*', '*MACROEXPAND-HOOK*', '*MODULES*', '*PACKAGE*',
    '*PRINT-ARRAY*', '*PRINT-BASE*', '*PRINT-CASE*', '*PRINT-CIRCLE*',
    '*PRINT-ESCAPE*', '*PRINT-GENSYM*', '*PRINT-LENGTH*', '*PRINT-LEVEL*',
    '*PRINT-LINES*', '*PRINT-MISER-WIDTH*', '*PRINT-PPRINT-DISPATCH*',
    '*PRINT-PRETTY*', '*PRINT-RADIX*', '*PRINT-READABLY*',
    '*PRINT-RIGHT-MARGIN*', '*QUERY-IO*', '*RANDOM-STATE*', '*READ-BASE*',
    '*READ-DEFAULT-FLOAT-FORMAT*', '*READ-EVAL*', '*READ-SUPPRESS*',
    '*READTABLE*', '*STANDARD-INPUT*', '*STANDARD-OUTPUT*', '*TERMINAL-IO*',
    '*TRACE-OUTPUT*',
)


def setup_standard_environment():
    """Initialize or return the standard Lisp environment.

    All function and special form registrations come from the decorator
    registry in fclpy.lispfunc.
    """
    # If functions were already loaded, return the environment.
    # BUT: if current_environment is None, always re-initialize (test reset)
    # OR: if functions_loaded is False (test explicitly reset), re-initialize
    if state.functions_loaded and state.current_environment is not None:
        # Extra safety check: ensure key functions are actually bound
        # This catches cases where state is corrupted across test boundaries
        try:
            star_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*')
            if state.current_environment.find_func(star_sym) is not None:
                return state.current_environment
            # If multiplication operator is missing, environment is stale - reinitialize
        except Exception:
            pass
    
    if state.current_environment is None:
        state.current_environment = fclpy.lisptype.Environment()

    # Canonical CL package membership (plan.md M1 step 1 / Finding A): the 978
    # symbols CLHS requires to be external in COMMON-LISP are interned and
    # exported unconditionally, independent of whether the registry below
    # happens to provide a binding for them. The registry supplies *bindings*
    # only; it must never decide *membership*.
    for _cl_name in CL_SYMBOL_NAMES:
        _cl_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(_cl_name)
        fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(_cl_sym)

    # The standard variables are *special*, and saying so is what makes a
    # binding form bind them dynamically. Without the proclamation,
    # `(let ((*print-base* 2)) ...)` puts a lexical binding in the LET's own
    # environment, which neither SYMBOL-VALUE nor a function called from the
    # body can see -- the printer, the reader and the stream functions all read
    # these from Python, through the global environment, and would go on seeing
    # the old value. A proclamation needs no value, so this is independent of
    # the initial values assigned further down.
    from fclpy.lispfunc.binding import proclaim_special
    for _special_name in STANDARD_SPECIAL_VARIABLES:
        proclaim_special(
            fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(_special_name),
            state.current_environment)

    import fclpy.lispfunc as lispfunc  # local import to avoid circulars
    try:
        from fclpy.lispfunc import registry as _registry
    except Exception:
        _registry = None

    if _registry:
        # Functions - intern into COMMON-LISP (exported) only when the name
        # is one of the 978 canonical ANSI symbols; everything else the
        # registry auto-discovered (implementation helpers, dead/duplicate
        # code paths - Finding A/L) goes into FCLPY-INTERNAL instead, so it
        # never pollutes the CL namespace real Common Lisp libraries expect
        # to be clean. Binding lookup (find_func/add_function) is name-keyed
        # (RC-1), so this does not change which functions are callable.
        for lisp_name, meta in _registry.function_registry.items():
            # meta is now a RegistryEntry, not a dict
            # Prefer the stored callable; fall back to getattr for legacy entries
            fn = getattr(meta, 'func', None)
            if fn is None:
                py_name = meta.py_name if hasattr(meta, 'py_name') else (meta.get('py_name') if isinstance(meta, dict) else None)
                if py_name:
                    fn = getattr(lispfunc, py_name, None)
            if fn:
                if lisp_name in CL_SYMBOL_NAMES:
                    sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(lisp_name)
                    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(sym)
                else:
                    sym = fclpy.lisptype.FCLPY_INTERNAL_PACKAGE.intern_symbol(lisp_name)
                if state.current_environment.find_func(sym) is None:
                    state.current_environment.add_function(sym, fn)
        # Specials - same canonical-membership routing as functions above.
        for lisp_name, meta in _registry.special_registry.items():
            # meta is now a RegistryEntry, not a dict
            # Prefer the stored callable; fall back to getattr for legacy entries
            fn = getattr(meta, 'func', None)
            if fn is None:
                py_name = meta.py_name if hasattr(meta, 'py_name') else (meta.get('py_name') if isinstance(meta, dict) else None)
                if py_name:
                    fn = getattr(lispfunc, py_name, None)
            if lisp_name in CL_SYMBOL_NAMES:
                sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(lisp_name)
                fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(sym)
            else:
                sym = fclpy.lisptype.FCLPY_INTERNAL_PACKAGE.intern_symbol(lisp_name)
            if state.current_environment.find_func(sym) is None:
                state.current_environment.add_function(sym, fn or (lambda *a: f"SPECIAL:{lisp_name}"))

    # Ensure core Lisp symbols have variable bindings in the environment
    # so that symbols like T and NIL evaluate to their Lisp values rather
    # than resolving to function bindings when no variable binding exists.
    try:
        for name, val in (('T', fclpy.lisptype.T), ('NIL', fclpy.lisptype.NIL)):
            sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(name)
            fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(sym)
            if state.current_environment.find_variable(sym) is None:
                state.current_environment.add_variable(sym, val)
    except Exception:
        # Defensive: if lisptype is not fully available yet, ignore
        pass
    
    # Initialize special variables for file loading (ANSI CL requirements)
    import os
    # *LOAD-TRUENAME* - absolute truename of file being loaded (NIL if not loading)
    load_truename_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-TRUENAME*')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(load_truename_sym)
    if state.current_environment.find_variable(load_truename_sym) is None:
        state.current_environment.add_variable(load_truename_sym, fclpy.lisptype.NIL)
        
    # *LOAD-PATHNAME* - pathname of file being loaded (NIL if not loading)
    load_pathname_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-PATHNAME*')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(load_pathname_sym)
    if state.current_environment.find_variable(load_pathname_sym) is None:
        state.current_environment.add_variable(load_pathname_sym, fclpy.lisptype.NIL)
        
    # *COMPILE-FILE-TRUENAME* - pathname of file being compiled (NIL if not compiling)
    compile_file_truename_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*COMPILE-FILE-TRUENAME*')
    if state.current_environment.find_variable(compile_file_truename_sym) is None:
        state.current_environment.add_variable(compile_file_truename_sym, fclpy.lisptype.NIL)
        
    # *COMPILE-FILE-PATHNAME* - pathname of file being compiled (NIL if not compiling)
    compile_file_pathname_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*COMPILE-FILE-PATHNAME*')
    if state.current_environment.find_variable(compile_file_pathname_sym) is None:
        state.current_environment.add_variable(compile_file_pathname_sym, fclpy.lisptype.NIL)
        
    # *DEFAULT-PATHNAME-DEFAULTS* - default pathname for pathname functions
    # Initialize to current directory as a Pathname object, a *directory*
    # (no :name component) -- `pathname_from_os_path` is what tells the two
    # apart, since `os.getcwd()` carries no trailing separator to parse.
    from fclpy.lispfunc.pathnames import pathname_from_os_path
    default_pathname_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*DEFAULT-PATHNAME-DEFAULTS*')
    if state.current_environment.find_variable(default_pathname_sym) is None:
        cwd_pathname = pathname_from_os_path(os.getcwd())
        state.current_environment.add_variable(default_pathname_sym, cwd_pathname)
        
    # *MODULES* - the names of the modules PROVIDE has recorded (CLHS 24.1.5).
    # Initially the empty list: a fresh image has provided nothing, and
    # `(every #'stringp *modules*)` must still answer T rather than signal
    # UNBOUND-VARIABLE.
    modules_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*MODULES*')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(modules_sym)
    if state.current_environment.find_variable(modules_sym) is None:
        state.current_environment.add_variable(modules_sym, fclpy.lisptype.NIL)

    # *FEATURES* - list of feature keywords for #+/- conditional read
    # Standard features include: :FCLPY (our implementation), :COMMON-LISP
    features_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*FEATURES*')
    if state.current_environment.find_variable(features_sym) is None:
        # Create a basic features list with our implementation identifier
        fclpy_feature = fclpy.lisptype.intern_keyword('FCLPY')
        cl_feature = fclpy.lisptype.intern_keyword('COMMON-LISP')
        ansi_cl_feature = fclpy.lisptype.intern_keyword('ANSI-CL')
        features_list = fclpy.lisptype.lispCons(fclpy_feature, 
                        fclpy.lisptype.lispCons(cl_feature,
                        fclpy.lisptype.lispCons(ansi_cl_feature, fclpy.lisptype.NIL)))
        state.current_environment.add_variable(features_sym, features_list)
        
    # Standard I/O stream variables.
    #
    # These are (re)initialized unconditionally, because reaching this point
    # *is* start-up: the early return above means a call that finds the
    # environment already built never gets here. The streams wrap Python's
    # `sys.stdin`/`stdout`/`stderr` objects as they are *now*, and a caller
    # that asks for a fresh environment (`state.functions_loaded = False`)
    # is asking for streams onto the current ones -- with an `is None` guard
    # they would instead keep wrapping whatever `sys.stdout` was the first
    # time this ran, since a variable's value now lives in the symbol's value
    # cell and outlives any one environment object.
    #
    # It also removes a latent `UnboundLocalError`: `stdout_stream` used to be
    # created inside `*STANDARD-OUTPUT*`'s guard, yet the four variables below
    # it referenced that name whether or not the guard had run.
    import sys
    from fclpy.lispfunc.streams import Stream, TwoWayStream

    stdin_stream = Stream('*STANDARD-INPUT*', sys.stdin, 'input')
    stdout_stream = Stream('*STANDARD-OUTPUT*', sys.stdout, 'output')
    stderr_stream = Stream('*ERROR-OUTPUT*', sys.stderr, 'output')
    # *DEBUG-IO*, *QUERY-IO* and *TERMINAL-IO* are bidirectional (CLHS
    # Figure 21-2), not output-only -- `(input-stream-p *terminal-io*)` must
    # be true (make-synonym-stream.4 asks exactly that of a synonym stream
    # onto *TERMINAL-IO*). A real two-way-stream over the same stdin/stdout
    # objects everything else uses keeps a write through it visible on
    # *STANDARD-OUTPUT* and vice versa, rather than introducing a second,
    # disconnected pair of streams.
    terminal_io_stream = TwoWayStream(stdin_stream, stdout_stream)
    for stream_var, stream in (
            ('*STANDARD-INPUT*', stdin_stream),
            ('*STANDARD-OUTPUT*', stdout_stream),
            ('*ERROR-OUTPUT*', stderr_stream),
            ('*TRACE-OUTPUT*', stdout_stream),
            ('*DEBUG-IO*', terminal_io_stream),
            ('*QUERY-IO*', terminal_io_stream),
            ('*TERMINAL-IO*', terminal_io_stream)):
        state.current_environment.add_variable(
            fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(stream_var), stream)


    # The printer control variables (CLHS Figure 22-1), with their ANSI
    # initial values. These must be real variables with real values: the
    # printer reads them from this environment, and until they were bound here
    # a reference to `*print-base*` fell through the evaluator's variable
    # lookup into the *function* registry and evaluated to a Python function
    # object. `printer.PRINTER_VARIABLES` is the single table of defaults so
    # the bootstrap and the printer cannot disagree about them.
    from fclpy.printer import PRINTER_VARIABLES
    for print_var_name, print_var_default in PRINTER_VARIABLES.items():
        print_var_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(print_var_name)
        fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(print_var_sym)
        if state.current_environment.find_variable(print_var_sym) is None:
            if print_var_default is True:
                initial = fclpy.lisptype.T
            elif print_var_default is False or print_var_default is None:
                initial = fclpy.lisptype.NIL
            elif print_var_name == '*PRINT-CASE*':
                initial = fclpy.lisptype.intern_keyword(print_var_default)
            else:
                initial = print_var_default
            state.current_environment.add_variable(print_var_sym, initial)

    # Reader/loader control variables
    # *LOAD-VERBOSE* - whether LOAD should print messages
    load_verbose_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-VERBOSE*')
    if state.current_environment.find_variable(load_verbose_sym) is None:
        state.current_environment.add_variable(load_verbose_sym, fclpy.lisptype.NIL)
        
    # *LOAD-PRINT* - whether LOAD should print values
    load_print_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-PRINT*')
    if state.current_environment.find_variable(load_print_sym) is None:
        state.current_environment.add_variable(load_print_sym, fclpy.lisptype.NIL)
        
    # *COMPILE-VERBOSE* - whether COMPILE-FILE should print messages
    compile_verbose_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*COMPILE-VERBOSE*')
    if state.current_environment.find_variable(compile_verbose_sym) is None:
        state.current_environment.add_variable(compile_verbose_sym, fclpy.lisptype.NIL)
        
    # *COMPILE-PRINT* - whether COMPILE-FILE should print values
    compile_print_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*COMPILE-PRINT*')
    if state.current_environment.find_variable(compile_print_sym) is None:
        state.current_environment.add_variable(compile_print_sym, fclpy.lisptype.NIL)
        
    # *READTABLE* - the current readtable used for reading
    from fclpy.readtable import get_current_readtable
    readtable_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*READTABLE*')
    if state.current_environment.find_variable(readtable_sym) is None:
        state.current_environment.add_variable(readtable_sym, get_current_readtable())
        
    # *PACKAGE* - current package (if not already set from state)
    package_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*PACKAGE*')
    if state.current_environment.find_variable(package_sym) is None:
        current_pkg = getattr(state, 'current_package', None) or fclpy.lisptype.COMMON_LISP_USER_PACKAGE
        state.current_environment.add_variable(package_sym, current_pkg)
        
    # *GENSYM-COUNTER* - counter used by GENSYM
    gensym_counter_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*GENSYM-COUNTER*')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(gensym_counter_sym)
    if state.current_environment.find_variable(gensym_counter_sym) is None:
        state.current_environment.add_variable(gensym_counter_sym, 0)
        # Also set symbol value for direct access
        gensym_counter_sym.value = 0
        
    # *PRINT-PPRINT-DISPATCH* - the current pretty print dispatch table. It
    # starts out holding the *standard* table (CLHS 23.4), which is the same
    # object WITH-STANDARD-IO-SYNTAX rebinds it to, so the table has one home
    # in io_write rather than a class declared inline here.
    from fclpy.lispfunc.io_write import standard_pprint_dispatch
    pprint_dispatch_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*PRINT-PPRINT-DISPATCH*')
    if state.current_environment.find_variable(pprint_dispatch_sym) is None:
        state.current_environment.add_variable(pprint_dispatch_sym, standard_pprint_dispatch())

    # *RANDOM-STATE* - proclaimed special above, but proclamation alone
    # leaves the value cell empty; RANDOM/MAKE-RANDOM-STATE read it through
    # fclpy.lispfunc.utilities_system.current_random_state(), which raises
    # rather than silently defaulting if this binding is ever missing.
    random_state_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*RANDOM-STATE*')
    if state.current_environment.find_variable(random_state_sym) is None:
        from fclpy.lispfunc.utilities_system import RandomState
        state.current_environment.add_variable(random_state_sym, RandomState())

    # === The standard constant variables (CLHS 1.9, 12.1.3, 15.1.1, 25.1.4.1) ===
    #
    # Every name below is a *constant variable*: an external symbol of
    # COMMON-LISP whose value is fixed and for which CONSTANTP must answer T.
    # `binding.proclaim_constant` is the one place that is recorded, and going
    # through it here is what makes `(constantp 'pi)` true -- these used to be
    # established as ordinary global variables with nothing noting that they
    # were constant, while DEFCONSTANT wrote a separate table nothing read.
    #
    # This was forty copies of the same three lines (intern, export, add if
    # absent), which is how the inconsistencies in it survived: the array and
    # character limits were never exported even though CLHS makes them
    # external, INTERNAL-TIME-UNITS-PER-SECOND was missing entirely (it was
    # registered as a *function*, so the symbol evaluated to a Python function
    # object), and MOST-POSITIVE-FIXNUM carried its own `2**63 - 1` literal --
    # a fourth copy of the boundary `typespec.MOST_POSITIVE_FIXNUM` is
    # supposed to be the one home of.
    import math
    from fclpy.typespec import MOST_POSITIVE_FIXNUM, MOST_NEGATIVE_FIXNUM
    from fclpy.lispfunc.utilities_system import INTERNAL_TIME_UNITS_PER_SECOND
    from fclpy.lispfunc.binding import proclaim_constant

    # Python's float *is* an IEEE double, so the short/single pair and the
    # long/double pair each name one representation here. The single-float
    # values are the true IEEE single limits rather than the double ones,
    # because `(typep x 'single-float)` and the printer both read them.
    _SINGLE_LIMITS = (
        ('MOST-POSITIVE-{}-FLOAT', 3.4028235e+38),
        ('LEAST-POSITIVE-{}-FLOAT', 1.4e-45),
        ('LEAST-POSITIVE-NORMALIZED-{}-FLOAT', 1.17549435e-38),
        ('MOST-NEGATIVE-{}-FLOAT', -3.4028235e+38),
        ('LEAST-NEGATIVE-{}-FLOAT', -1.4e-45),
        ('LEAST-NEGATIVE-NORMALIZED-{}-FLOAT', -1.17549435e-38),
        ('{}-FLOAT-EPSILON', 1.1920929e-7),
        ('{}-FLOAT-NEGATIVE-EPSILON', 5.9604645e-8),
    )
    _DOUBLE_LIMITS = (
        ('MOST-POSITIVE-{}-FLOAT', 1.7976931348623157e+308),
        ('LEAST-POSITIVE-{}-FLOAT', 5e-324),
        ('LEAST-POSITIVE-NORMALIZED-{}-FLOAT', 2.2250738585072014e-308),
        ('MOST-NEGATIVE-{}-FLOAT', -1.7976931348623157e+308),
        ('LEAST-NEGATIVE-{}-FLOAT', -5e-324),
        ('LEAST-NEGATIVE-NORMALIZED-{}-FLOAT', -2.2250738585072014e-308),
        ('{}-FLOAT-EPSILON', 2.220446049250313e-16),
        ('{}-FLOAT-NEGATIVE-EPSILON', 1.1102230246251565e-16),
    )

    # LAMBDA-LIST-KEYWORDS' value is a list of symbols, and those symbols are
    # themselves external symbols of COMMON-LISP (CLHS 3.4.1).
    _lambda_list_keyword_names = ['&ALLOW-OTHER-KEYS', '&AUX', '&BODY',
                                 '&ENVIRONMENT', '&KEY', '&OPTIONAL',
                                 '&REST', '&WHOLE']
    _lambda_list_keywords = fclpy.lisptype.NIL
    for _name in reversed(_lambda_list_keyword_names):
        _keyword_symbol = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(_name)
        fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(_keyword_symbol)
        _lambda_list_keywords = fclpy.lisptype.lispCons(_keyword_symbol,
                                                       _lambda_list_keywords)

    STANDARD_CONSTANTS = [
        # Integer limits (CLHS 12.1.3)
        ('MOST-POSITIVE-FIXNUM', MOST_POSITIVE_FIXNUM),
        ('MOST-NEGATIVE-FIXNUM', MOST_NEGATIVE_FIXNUM),
        ('PI', math.pi),
        # Array and character limits (CLHS 15.1.1, 13.1.2)
        ('CHAR-CODE-LIMIT', 1114112),          # Unicode max + 1
        ('ARRAY-DIMENSION-LIMIT', 2**31),
        ('ARRAY-RANK-LIMIT', 64),
        ('ARRAY-TOTAL-SIZE-LIMIT', 2**31),
        # Call limits (CLHS 3.4)
        ('CALL-ARGUMENTS-LIMIT', 2**20),
        ('LAMBDA-PARAMETERS-LIMIT', 2**20),
        ('MULTIPLE-VALUES-LIMIT', 2**20),
        ('LAMBDA-LIST-KEYWORDS', _lambda_list_keywords),
        # Time (CLHS 25.1.4.1)
        ('INTERNAL-TIME-UNITS-PER-SECOND', INTERNAL_TIME_UNITS_PER_SECOND),
    ]
    for _precision, _limits in (('SHORT', _SINGLE_LIMITS),
                                ('SINGLE', _SINGLE_LIMITS),
                                ('DOUBLE', _DOUBLE_LIMITS),
                                ('LONG', _DOUBLE_LIMITS)):
        for _template, _value in _limits:
            STANDARD_CONSTANTS.append((_template.format(_precision), _value))

    for _name, _value in STANDARD_CONSTANTS:
        _symbol = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(_name)
        fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(_symbol)
        proclaim_constant(_symbol, state.current_environment)
        if state.current_environment.find_variable(_symbol) is None:
            state.current_environment.add_variable(_symbol, _value)

    # === Type Specifier Symbols (ANSI CL type system) ===
    # These symbols should evaluate to themselves - they are used as type specifiers
    # Note: We EXCLUDE type names that are also function names (LIST, CONS, etc.)
    # Those work as types when quoted: (typep x 'list), not (typep x list)
    type_symbols = [
        # Basic types (excluding LIST, CONS which are functions)
        'NULL', 'ATOM',
        # Numeric types  
        'NUMBER', 'REAL', 'RATIONAL', 'INTEGER', 'FIXNUM', 'BIGNUM',
        'RATIO', 'FLOAT', 'SHORT-FLOAT', 'SINGLE-FLOAT', 'DOUBLE-FLOAT', 'LONG-FLOAT',
        'COMPLEX', 'BIT',
        # Character types
        'CHARACTER', 'BASE-CHAR', 'STANDARD-CHAR', 'EXTENDED-CHAR',
        # Sequence types (excluding STRING, VECTOR which may be functions)
        'SEQUENCE', 'SIMPLE-STRING', 'BASE-STRING', 'SIMPLE-BASE-STRING',
        'SIMPLE-VECTOR', 'BIT-VECTOR', 'SIMPLE-BIT-VECTOR',
        'ARRAY', 'SIMPLE-ARRAY',
        # Function types
        'COMPILED-FUNCTION',
        # Other built-in types (excluding PATHNAME which is a function)
        'HASH-TABLE', 'PACKAGE', 'LOGICAL-PATHNAME',
        'STREAM', 'FILE-STREAM', 'STRING-STREAM', 'BROADCAST-STREAM',
        'CONCATENATED-STREAM', 'ECHO-STREAM', 'SYNONYM-STREAM', 'TWO-WAY-STREAM',
        'RANDOM-STATE', 'READTABLE', 'RESTART',
        # Structure/class types
        'STRUCTURE-OBJECT', 'STANDARD-OBJECT', 'CLASS', 'STRUCTURE-CLASS',
        'STANDARD-CLASS', 'BUILT-IN-CLASS', 'METHOD', 'STANDARD-METHOD',
        'METHOD-COMBINATION', 'GENERIC-FUNCTION', 'STANDARD-GENERIC-FUNCTION',
        # Condition types
        'CONDITION', 'SIMPLE-WARNING', 'STYLE-WARNING',
        'SERIOUS-CONDITION', 'SIMPLE-ERROR', 'CELL-ERROR',
        'TYPE-ERROR', 'SIMPLE-TYPE-ERROR', 'PARSE-ERROR', 'PROGRAM-ERROR',
        'CONTROL-ERROR', 'PACKAGE-ERROR', 'STREAM-ERROR', 'END-OF-FILE',
        'FILE-ERROR', 'PRINT-NOT-READABLE', 'READER-ERROR',
        'ARITHMETIC-ERROR', 'DIVISION-BY-ZERO', 'FLOATING-POINT-OVERFLOW',
        'FLOATING-POINT-UNDERFLOW', 'FLOATING-POINT-INEXACT',
        'FLOATING-POINT-INVALID-OPERATION', 'STORAGE-CONDITION',
        'UNBOUND-SLOT', 'UNBOUND-VARIABLE',
        # Boolean
        'BOOLEAN',
    ]
        
    for type_name in type_symbols:
        sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(type_name)
        fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(sym)
        # Type symbols evaluate to themselves
        if state.current_environment.find_variable(sym) is None:
            state.current_environment.add_variable(sym, sym)
            

    # Update module-level variable for backward compatibility with code that uses lispenv.current_environment
    global current_environment
    current_environment = state.current_environment
    
    state.functions_loaded = True
    return state.current_environment
