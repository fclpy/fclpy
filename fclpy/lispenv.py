import math as _math

import fclpy.lisptype
import fclpy.state as state
from fclpy.cl_symbol_names import CL_SYMBOL_NAMES


# Initialize the central environment object in state if not present
if state.current_environment is None:
    state.current_environment = fclpy.lisptype.Environment()

# Backward-compatible module-level name used across the codebase
current_environment = state.current_environment


# CLHS Figure 25-1, the standard variables, plus the ten top-level loop
# variables of CLHS 25.1.1 (`-`, `+`, `++`, `+++`, `*`, `**`, `***`, `/`,
# `//`, `///`), which are dynamic variables of the same kind. Every one of
# these is proclaimed special, which is what makes a binding form bind it
# in the symbol's value cell -- the one home a global variable has (see
# `Environment`) and the only one the Python-side readers in printer.py /
# readtable.py / streams.py can reach. This is the authoritative list rather
# than a record of which variables the bootstrap below happens to give a
# value to: proclaiming a variable special does not require it to be bound,
# so the two cannot drift.
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
    # CLHS 25.1.1 top-level loop variables
    '-', '+', '++', '+++', '*', '**', '***', '/', '//', '///',
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
        # Clean up incorrectly registered constants and type specifiers. These
        # should be variables, not functions. They were mistakenly decorated
        # with @cl_function in misc_macros.py, math_advanced.py,
        # utilities_functions.py, and misc_clos.py.
        _type_and_const_symbols_to_remove = {
            # Type specifiers (should be variables, not functions)
            'FIXNUM', 'KEYWORD', 'INTEGER', 'DOUBLE-FLOAT', 'SINGLE-FLOAT',
            'SHORT-FLOAT', 'EXTENDED-CHAR', 'HASH-TABLE', 'GENERIC-FUNCTION',
            'FILE-STREAM', 'FILE-ERROR', 'END-OF-FILE', 'FLOATING-POINT-INEXACT',
            'FLOATING-POINT-INVALID-OPERATION', 'FLOATING-POINT-OVERFLOW',
            'FLOATING-POINT-UNDERFLOW', 'SIMPLE-BIT-VECTOR', 'SIMPLE-VECTOR',
            'SIMPLE-STRING', 'TYPE-ERROR', 'SIMPLE-ERROR', 'METHOD-COMBINATION',
            'TYPE', 'NIL', 'T',
            # Type/class names (should be class objects, not functions)
            'BUILT-IN-CLASS', 'STANDARD-CLASS', 'STANDARD-OBJECT',
            'STRUCTURE-CLASS', 'STRUCTURE-OBJECT',
            # Constants (should be variables, not functions)
            'MOST-POSITIVE-FIXNUM', 'MOST-NEGATIVE-FIXNUM', 'PI',
            'LEAST-POSITIVE-DOUBLE-FLOAT', 'LEAST-NEGATIVE-DOUBLE-FLOAT',
            'MOST-POSITIVE-DOUBLE-FLOAT', 'MOST-NEGATIVE-DOUBLE-FLOAT',
            'LEAST-POSITIVE-SHORT-FLOAT', 'LEAST-NEGATIVE-SHORT-FLOAT',
            'MOST-POSITIVE-SHORT-FLOAT', 'MOST-NEGATIVE-SHORT-FLOAT',
            'LEAST-POSITIVE-SINGLE-FLOAT', 'LEAST-NEGATIVE-SINGLE-FLOAT',
            'MOST-POSITIVE-SINGLE-FLOAT', 'MOST-NEGATIVE-SINGLE-FLOAT',
            'LEAST-POSITIVE-LONG-FLOAT', 'LEAST-NEGATIVE-LONG-FLOAT',
            'MOST-POSITIVE-LONG-FLOAT', 'MOST-NEGATIVE-LONG-FLOAT',
            'LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT', 'LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT',
            'LEAST-POSITIVE-NORMALIZED-LONG-FLOAT', 'LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT',
            'LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT', 'LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT',
            'LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT', 'LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT',
            # Limit constants (should be variables, not functions)
            'ARRAY-DIMENSION-LIMIT', 'ARRAY-RANK-LIMIT', 'ARRAY-TOTAL-SIZE-LIMIT',
            'CALL-ARGUMENTS-LIMIT', 'CHAR-CODE-LIMIT', 'MULTIPLE-VALUES-LIMIT',
            # Declaration identifiers (should not be functions)
            'DYNAMIC-EXTENT', 'FTYPE', 'NOTINLINE', 'INLINE', 'OPTIMIZE',
            'SPECIAL', 'LAMBDA-LIST-KEYWORDS', 'LAMBDA-PARAMETERS-LIMIT',
            'IGNORABLE', 'IGNORE',
        }
        for _sym_to_remove in _type_and_const_symbols_to_remove:
            if _sym_to_remove in _registry.function_registry:
                del _registry.function_registry[_sym_to_remove]
            # Also clean from special_registry if present (declaration keywords,
            # etc. that were misregistered as special operators)
            if _sym_to_remove in _registry.special_registry:
                del _registry.special_registry[_sym_to_remove]

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
        # T is a LispSymbol and gets a value-cell binding like any constant.
        # NIL is *not* bound here: it is the `lispNull` singleton (not a
        # LispSymbol, which `Environment.add_variable` rejects), it is
        # self-evaluating (eval special-cases it), and it is always bound
        # (`is_constant_symbol` answers for it directly) -- so it has no
        # value cell to fill.
        sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('T')
        fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(sym)
        if state.current_environment.find_variable(sym) is None:
            state.current_environment.add_variable(sym, fclpy.lisptype.T)
    except Exception:
        # Defensive: if lisptype is not fully available yet, ignore
        pass
    
    # Initialize special variables for file loading (ANSI CL requirements)
    from fclpy.system.shell import shell
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
    # apart, since the startup cwd carries no trailing separator to parse.
    from fclpy.lispfunc.pathnames import pathname_from_os_path
    default_pathname_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*DEFAULT-PATHNAME-DEFAULTS*')
    if state.current_environment.find_variable(default_pathname_sym) is None:
        cwd_pathname = pathname_from_os_path(shell.get_startup_cwd())
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
    from fclpy.lispfunc.streams import Stream, TwoWayStream

    stdin_stream = Stream('*STANDARD-INPUT*', shell.get_stdin(), 'input')
    stdout_stream = Stream('*STANDARD-OUTPUT*', shell.get_stdout(), 'output')
    stderr_stream = Stream('*ERROR-OUTPUT*', shell.get_stderr(), 'output')
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

    # The reader control variables (CLHS Figure 23-1), from the one table the
    # reader itself reads them through. All four were **unbound**: they are
    # proclaimed special above, but nothing ever gave them a value, so
    # `(boundp '*read-base*)` was NIL and evaluating `*read-eval*` signalled
    # UNBOUND-VARIABLE. `*READTABLE*` is not in that table -- its initial value
    # is an object, and it is bound a few lines below.
    from fclpy.lispreader import READER_VARIABLES
    for read_var_name, read_var_default in READER_VARIABLES.items():
        read_var_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(read_var_name)
        fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(read_var_sym)
        if state.current_environment.find_variable(read_var_sym) is None:
            if read_var_default is True:
                initial = fclpy.lisptype.T
            elif read_var_default is False:
                initial = fclpy.lisptype.NIL
            elif isinstance(read_var_default, str):
                # `*READ-DEFAULT-FLOAT-FORMAT*` holds a *type name*, so an
                # interned COMMON-LISP symbol -- not a keyword.
                initial = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(
                    read_var_default)
            else:
                initial = read_var_default
            state.current_environment.add_variable(read_var_sym, initial)

    # Loader control variables
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

    # === The REPL loop variables (CLHS 25.1.1) and the condition/macroexpansion
    # hooks (CLHS 9.1.1, 3.1.2.1.1) ===
    #
    # All of these are *bound* in a fresh image -- `cl-variable-symbols.1`
    # collects every COMMON-LISP variable boundp answers NIL for, and these
    # were the unbound ones: the ten top-level loop variables (all NIL until
    # a REPL writes them), *BREAK-ON-SIGNALS* (initially false) and
    # *DEBUGGER-HOOK* (initially nil). *MACROEXPAND-HOOK*'s initial value is
    # the FUNCALL function (CLHS: "The initial value of *macroexpand-hook* is
    # funcall"). They are proclaimed special above; proclamation alone left
    # the value cells empty, which is what boundp answers NIL to.
    for repl_var_name in ('-', '+', '++', '+++', '*', '**', '***',
                          '/', '//', '///',
                          '*BREAK-ON-SIGNALS*', '*DEBUGGER-HOOK*'):
        repl_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(repl_var_name)
        fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(repl_sym)
        if state.current_environment.find_variable(repl_sym) is None:
            state.current_environment.add_variable(repl_sym, fclpy.lisptype.NIL)

    macroexpand_hook_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('*MACROEXPAND-HOOK*')
    fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(macroexpand_hook_sym)
    if state.current_environment.find_variable(macroexpand_hook_sym) is None:
        funcall_sym = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol('FUNCALL')
        hook_value = state.current_environment.find_func(funcall_sym)
        state.current_environment.add_variable(
            macroexpand_hook_sym,
            hook_value if hook_value is not None else fclpy.lisptype.NIL)

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

    # Python's float *is* an IEEE double, so all four CL float formats name
    # one representation here -- and therefore **one range**. The SHORT/SINGLE
    # entries used to carry the true IEEE single limits, but that described a
    # range the arithmetic does not have: `exp`, `expt` and `+` all compute in
    # double and happily answer values between 3.4e38 and 1.79e308, so
    # `exp.error.4/.5/.8/.9`'s `(exp (+ (log most-positive-short-float) 100))`
    # returned 9.1e81 where the test requires a signal, and the *lattice* had
    # already given the game away -- `typespec` answers (T T) for
    # `(subtypep 'single-float 'double-float)`, and mutually-subtype types
    # cannot have different bounds. The epsilons below make the same argument
    # for precision. CLISP ships exactly this model (one float format, four
    # names), and `subtypep-float.lsp`'s conditional tests adapt to it.
    _FLOAT_LIMITS = (
        ('MOST-POSITIVE-{}-FLOAT', 1.7976931348623157e+308),
        ('LEAST-POSITIVE-{}-FLOAT', 5e-324),
        ('LEAST-POSITIVE-NORMALIZED-{}-FLOAT', 2.2250738585072014e-308),
        ('MOST-NEGATIVE-{}-FLOAT', -1.7976931348623157e+308),
        ('LEAST-NEGATIVE-{}-FLOAT', -5e-324),
        ('LEAST-NEGATIVE-NORMALIZED-{}-FLOAT', -2.2250738585072014e-308),
    )

    # The epsilons are deliberately **not** in the two tables above, because
    # they are not range limits: CLHS defines `<format>-float-epsilon` as the
    # smallest positive float of that format with
    # `(/= (float 1 e) (+ (float 1 e) e))` -- a property of the *arithmetic*,
    # not of the representable magnitude. Every float here is a Python float,
    # i.e. an IEEE double, so all four formats share one arithmetic and
    # therefore one epsilon; giving SHORT/SINGLE the IEEE *single* epsilon
    # described a precision this implementation does not have, and
    # `numbers/epsilons.lsp` measures the contradiction directly -- it binary-
    # searches for the real epsilon and compares it against the constant.
    #
    # Both are *computed* from the definition rather than written as literals,
    # because the answer is one ULP away from the value one would guess and
    # the guess is silently wrong. `2**-53` is not the epsilon: `1 + 2**-53`
    # lands exactly halfway between 1 and its successor, and IEEE
    # round-half-to-even takes it back down to 1, so `2**-53` fails the very
    # test that defines epsilon. The answer is the next representable float
    # above it -- and likewise `2**-54` for the negative epsilon. The double
    # entry previously held `2**-52`, the conventional "machine epsilon",
    # which is a different quantity from the one CLHS names, and its negative
    # counterpart held a near-copy of the *positive* value.
    _FLOAT_EPSILON = _math.nextafter(2.0 ** -53, 1.0)
    _FLOAT_NEGATIVE_EPSILON = _math.nextafter(2.0 ** -54, 1.0)
    _EPSILON_LIMITS = (
        ('{}-FLOAT-EPSILON', _FLOAT_EPSILON),
        ('{}-FLOAT-NEGATIVE-EPSILON', _FLOAT_NEGATIVE_EPSILON),
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
        # BOOLE's sixteen op codes (CLHS 12.1.4/BOOLE): constant variables,
        # not functions -- `numbers/boole.lsp` builds `*boole-vals*` by
        # evaluating each name as a *variable*
        # (`(list boole-1 boole-2 boole-and ...)`), which is exactly the
        # "registered as a function" defect this table exists to prevent (see
        # the docstring above): `core.py` had a same-named zero-argument
        # *function* for each of these, so referencing the bare symbol fell
        # through to `evaluation_core.eval`'s function-cell fallback and
        # returned a raw Python function object as the value -- and two of
        # them (`boole_1`/`boole_and`) both happened to return the Python
        # int `1`, so even calling through would have collapsed two distinct
        # operations onto one code. The values themselves are
        # implementation-defined (CLHS places no requirement beyond pairwise
        # distinctness); these match the common SBCL/CMUCL assignment.
        ('BOOLE-CLR', 0), ('BOOLE-SET', 1), ('BOOLE-1', 2), ('BOOLE-2', 3),
        ('BOOLE-C1', 4), ('BOOLE-C2', 5), ('BOOLE-AND', 6), ('BOOLE-IOR', 7),
        ('BOOLE-XOR', 8), ('BOOLE-EQV', 9), ('BOOLE-NAND', 10), ('BOOLE-NOR', 11),
        ('BOOLE-ANDC1', 12), ('BOOLE-ANDC2', 13), ('BOOLE-ORC1', 14), ('BOOLE-ORC2', 15),
    ]
    for _precision in ('SHORT', 'SINGLE', 'DOUBLE', 'LONG'):
        # The limits and the epsilons are the same for all four formats --
        # see the notes above: one arithmetic, one representation, one range.
        for _template, _value in tuple(_FLOAT_LIMITS) + _EPSILON_LIMITS:
            STANDARD_CONSTANTS.append((_template.format(_precision), _value))

    for _name, _value in STANDARD_CONSTANTS:
        _symbol = fclpy.lisptype.COMMON_LISP_PACKAGE.intern_symbol(_name)
        fclpy.lisptype.COMMON_LISP_PACKAGE.export_symbol(_symbol)
        proclaim_constant(_symbol, state.current_environment)
        if state.current_environment.find_variable(_symbol) is None:
            state.current_environment.add_variable(_symbol, _value)

    # Type specifier symbols (NUMBER, ARRAY, CONDITION, ...) get **no
    # variable binding** here: they are *type names*, not variables, and a
    # type name is only ever used quoted -- `(typep x 'number)`, never
    # `(typep x number)`. Binding each one's value cell (to the symbol
    # itself, as this block once did "so they evaluate to themselves") made
    # every one of the 82 class names answer T to `boundp`, which
    # `boundp.5` collects over and asserts NIL: in Common Lisp a class
    # lives in its class cell (`FIND-CLASS`), never in the value cell.

    # Update module-level variable for backward compatibility with code that uses lispenv.current_environment
    global current_environment
    current_environment = state.current_environment
    
    state.functions_loaded = True
    return state.current_environment
