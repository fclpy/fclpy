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


# WITH-COMPILATION-UNIT is a real macro expander in
# evaluation_special_forms.py. It was a `cl_function` stub here whose body was
# "evaluate every argument eagerly, return the last"; because `cl_function`
# evaluates arguments, its option list `(:OVERRIDE NIL)` was evaluated as a
# call to a function named OVERRIDE. Keeping a second registration would
# silently win or lose depending on module import order (standing rule 3).


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


# WITH-STANDARD-IO-SYNTAX is a real macro expander in
# evaluation_special_forms.py, for the same reason as the WITH-*-STRING macros
# above: it was a `cl_function` here whose body was "evaluate every argument
# eagerly, return the last", so it established none of the twenty-one bindings
# CLHS 23.4 gives it and its subforms ran in the *caller's* dynamic
# environment. Keeping a second registration would silently win or lose
# depending on module import order (standing rule 3).


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
def load(filespec, *, verbose=lisptype.OMITTED, print=lisptype.OMITTED,
         if_does_not_exist=lisptype.OMITTED, external_format=None):
    """LOAD (CLHS 24.2): read and evaluate every form in a file or stream.

    Four things here are the mechanism rather than a detail, and each was a
    cluster of failures in `system-construction/load-file.lsp`:

    **A filespec may be a stream.** CLHS: "filespec---a stream, or a pathname
    designator". Loading from a stream and loading from a file are the same
    operation; only where the characters come from, and what the load
    variables hold, differ. The previous implementation ran `str(filespec)` on
    whatever it got, so a string-input stream became the *pathname*
    ``"<StringInputStream pos=0 len=59>"`` and seven tests failed on
    "file not found".

    **The forms are read one at a time through READ**, not through a reader
    built once at the top. READ consults `*READTABLE*` and `*PACKAGE*` at each
    call, and that is the semantics: a form in the file that assigns either of
    them governs how the *rest of the file* is read (load.15a, load.16a).

    **`*PACKAGE*`, `*READTABLE*`, `*LOAD-PATHNAME*` and `*LOAD-TRUENAME*` are
    bound, not assigned** -- through `BindingFrame`, the mechanism LET uses,
    over the global environment where all four are proclaimed special and
    therefore live in their value cells. So a file's IN-PACKAGE or
    SET-MACRO-CHARACTER is undone when the file finishes, however it finishes,
    and the hand-rolled save/restore pairs that used to do this (and leaked on
    a non-local exit) are gone.

    **A missing file is a FILE-ERROR, and `:if-does-not-exist` decides whether
    it is signalled at all.** The default is true (CLHS); NIL means return NIL.
    The old code had this inverted -- ``if_does_not_exist is NIL or is None``
    *raised* -- so `(load "nope" :if-does-not-exist nil)` signalled, and what
    it signalled was a Python `FileNotFoundError`, which no handler matches.

    `verbose`/`print` default to `*LOAD-VERBOSE*`/`*LOAD-PRINT*`, and an
    explicitly supplied NIL overrides them -- which is why they take the
    OMITTED sentinel rather than defaulting to None. Spelling all four
    keyword-only is what makes `(load f :bad-key-arg t)` the PROGRAM-ERROR
    CLHS 3.5.1.5 requires (CLAUDE.md, "a builtin's ANSI lambda list is its
    Python signature").
    """
    import builtins
    import os
    import fclpy.state as state
    from .pathnames import Pathname, resolve_filespec
    from .streams import Stream
    from .binding import BindingFrame, root_environment, dynamic_value
    from .evaluation_conditions import signal_file_error
    from .io_write import write_text
    from .io_read import read as read_form
    from .evaluation_core import eval as lisp_eval
    from fclpy.printer import write_object
    from fclpy.readtable import get_current_readtable

    def _cl(name):
        return lisptype.COMMON_LISP_PACKAGE.intern_symbol(name)

    verbose_p = lisptype.is_truthy(
        verbose if lisptype.supplied(verbose)
        else dynamic_value(_cl('*LOAD-VERBOSE*'), lisptype.NIL))
    print_p = lisptype.is_truthy(
        print if lisptype.supplied(print)
        else dynamic_value(_cl('*LOAD-PRINT*'), lisptype.NIL))

    env = state.current_environment
    if env is None:
        raise lisptype.LispEnvironmentError("LOAD: no environment available")

    # A stream argument is loaded as it stands; the load variables then
    # describe the stream's file, which for a string stream is nothing at all.
    if isinstance(filespec, Stream):
        stream = filespec
        opened_here = None
        # The load variables hold "the pathname of the file being loaded"
        # (CLHS 24.2), which for a stream is the file it is associated with --
        # NIL for a string stream, which has none.
        name = getattr(filespec, 'name', None)
        if isinstance(name, str) and name and not name.startswith('<') \
                and os.path.exists(name):
            pathname_obj = Pathname(name)
            truename_obj = Pathname(os.path.realpath(name))
        else:
            pathname_obj = lisptype.NIL
            truename_obj = lisptype.NIL
        label = name or 'stream'
    else:
        path_str = resolve_filespec(filespec)
        if not os.path.exists(path_str):
            # A "fasl" this implementation produces is the source file copied
            # (see COMPILE-FILE), so fall back to the source when only it is
            # present -- otherwise `(load (compile-file-pathname f))` from an
            # image that never ran COMPILE-FILE could not work.
            source = path_str[:-5] + '.lsp' if path_str.endswith('.fasl') else None
            if source and os.path.exists(source):
                path_str = source
            elif (lisptype.supplied(if_does_not_exist)
                  and not lisptype.is_truthy(if_does_not_exist)):
                return lisptype.NIL
            else:
                return signal_file_error(
                    Pathname(path_str), f"LOAD: file not found: {path_str}")

        pathname_obj = Pathname(path_str)
        truename_obj = Pathname(os.path.realpath(path_str))
        label = path_str
        opened_here = builtins.open(path_str, 'r', encoding='utf-8')
        stream = Stream(path_str, opened_here, 'input')

    if verbose_p:
        # The leading `;` is not decoration: a verbose load's output is Lisp
        # comment syntax, and `load-file-test` checks for that character.
        write_text("; loading " + str(label) + "\n")

    frame = BindingFrame(root_environment(env))
    try:
        frame.bind(_cl('*LOAD-PATHNAME*'), pathname_obj)
        frame.bind(_cl('*LOAD-TRUENAME*'), truename_obj)
        # `*PACKAGE*` and `*READTABLE*` are bound to their own current values:
        # the point is not to change them but to make the loaded forms'
        # assignments to them local to the load (CLHS 24.2).
        current_package = dynamic_value(_cl('*PACKAGE*'))
        if not isinstance(current_package, lisptype.Package):
            current_package = (getattr(state, 'current_package', None)
                               or lisptype.COMMON_LISP_USER_PACKAGE)
        frame.bind(_cl('*PACKAGE*'), current_package)
        frame.bind(_cl('*READTABLE*'), get_current_readtable())

        eof = object()
        while True:
            form = read_form(stream, lisptype.NIL, eof)
            if form is eof:
                break
            result = lisp_eval(form, state.current_environment)
            if print_p:
                for value in _values_of(result):
                    write_text(write_object(value) + "\n")
        return lisptype.T
    finally:
        frame.unwind()
        if opened_here is not None:
            opened_here.close()


def _values_of(result):
    """The values a form produced, as a Python list -- `:print` prints "the
    results of evaluating each form" (CLHS 24.2), which may be none or many."""
    if isinstance(result, lisptype.MultipleValues):
        return list(result.values)
    return [result]



#: The top-level operators the compiler must *evaluate* while compiling a file
#: (CLHS 3.2.3.1). This is a whitelist rather than "evaluate everything",
#: because the defining difference between COMPILE-FILE and LOAD is that
#: COMPILE-FILE does **not** run the program: after `(compile-file f)` the
#: functions f defines must still be undefined, which is what
#: `compile-file-test` asserts with `(not (fboundp funname))`. The previous
#: implementation was a `shutil.copy2` -- it read nothing, so it evaluated
#: nothing, bound none of the compile-file variables, resolved no `#.`, and
#: could not tell an `(eval-when (:compile-toplevel) ...)` form from any other.
#:
#: What *is* evaluated is what later forms in the same file need in order to be
#: read and processed at all: the package the reader interns into, macro and
#: type definitions, and anything the program itself asked for with
#: `(eval-when (:compile-toplevel) ...)`.
COMPILE_TIME_OPERATORS = frozenset((
    'IN-PACKAGE', 'DEFPACKAGE',
    'DEFMACRO', 'DEFINE-COMPILER-MACRO', 'DEFINE-SYMBOL-MACRO',
    'DEFINE-MODIFY-MACRO', 'DEFSETF', 'DEFINE-SETF-EXPANDER',
    'DEFTYPE', 'DEFSTRUCT', 'DEFCLASS', 'DEFINE-CONDITION',
    'DECLAIM', 'PROCLAIM',
))

#: Operators whose body is itself a sequence of top-level forms (CLHS 3.2.3.1),
#: so the compile-time processing rule applies through them.
COMPILE_TIME_TRANSPARENT = frozenset(('PROGN', 'LOCALLY'))

#: The type this implementation gives a compiled file. fclpy has no code
#: generator; a "compiled" file is the forms the compiler read, printed back
#: out (see `compile_file`), so it stays readable Lisp.
COMPILED_FILE_TYPE = '.fasl'


def _operator_name(form):
    """The name of `form`'s operator, upper-cased, or None if it has none."""
    if not isinstance(form, lisptype.lispCons):
        return None
    head = form.car
    return head.name.upper() if isinstance(head, lisptype.LispSymbol) else None


def _eval_when_situations(form):
    """The situation keywords of an EVAL-WHEN form, upper-cased."""
    rest = form.cdr
    if not isinstance(rest, lisptype.lispCons):
        return ()
    situations = []
    cur = rest.car
    while isinstance(cur, lisptype.lispCons):
        item = cur.car
        if isinstance(item, lisptype.LispSymbol):
            situations.append(item.name.upper())
        cur = cur.cdr
    return tuple(situations)


def _compile_time_forms(form):
    """The forms COMPILE-FILE must evaluate for this top-level `form`
    (CLHS 3.2.3.1), as a Python list.

    Recursive, because PROGN and LOCALLY splice their subforms into the
    top-level sequence, and EVAL-WHEN's body is itself processed as top-level
    forms when the `:compile-toplevel` situation applies.
    """
    name = _operator_name(form)
    if name is None:
        return []
    if name in COMPILE_TIME_OPERATORS:
        return [form]
    if name in COMPILE_TIME_TRANSPARENT:
        out = []
        cur = form.cdr
        while isinstance(cur, lisptype.lispCons):
            out.extend(_compile_time_forms(cur.car))
            cur = cur.cdr
        return out
    if name == 'EVAL-WHEN':
        situations = set(_eval_when_situations(form))
        if not situations & {'COMPILE-TOPLEVEL', 'COMPILE'}:
            return []
        out = []
        cur = form.cdr
        cur = cur.cdr if isinstance(cur, lisptype.lispCons) else lisptype.NIL
        while isinstance(cur, lisptype.lispCons):
            out.append(cur.car)
            cur = cur.cdr
        return out
    return []


def _lisp_list_of(items):
    result = lisptype.NIL
    for item in reversed(list(items)):
        result = lisptype.lispCons(item, result)
    return result


class _CompilationDiagnostics:
    """Records whether a compilation signalled warnings or errors, so
    COMPILE-FILE can answer its second and third values.

    CLHS COMPILE-FILE: `warnings-p` is true if the compilation signalled any
    condition of type ERROR or WARNING, and `failure-p` is true if it signalled
    an ERROR or a WARNING that is **not** a STYLE-WARNING. Both used to be
    hard-wired NIL, so the two tests that ask specifically about them --
    `compile-file.2` (a style warning must set warnings-p) and
    `compile-file.2a` (a plain warning must set failure-p) -- could not pass
    however the compilation behaved.

    Observation pushes a handler cluster onto `state.handler_stack`, i.e. goes
    through the same mechanism HANDLER-BIND uses, so conditions are seen *at
    the signal point*; the handler declines by returning, because it must not
    intercept them -- the program being compiled may have handlers of its own.
    """

    def __init__(self):
        self.warnings = False
        self.failure = False

    def note(self, condition):
        from .comparison import typep
        self.warnings = True
        if typep(condition, 'STYLE-WARNING') != lisptype.T:
            self.failure = True
        return lisptype.NIL

    def cluster(self):
        # One handler for (OR ERROR WARNING) -- the exact set CLHS names.
        specifier = _lisp_list_of([
            lisptype.LispSymbol('OR'),
            lisptype.LispSymbol('ERROR'),
            lisptype.LispSymbol('WARNING'),
        ])
        return [(specifier, self.note)]


#: The printer controls COMPILE-FILE pins while writing its output, and the
#: values it pins them to. Every one of these can make the printer *lose*
#: information: `*PRINT-LENGTH*`/`*PRINT-LEVEL*`/`*PRINT-LINES*` truncate to
#: `...`, `*PRINT-BASE*` would write integers in another radix with nothing to
#: say so, `*PRINT-ESCAPE*` NIL drops the quotes off strings, and
#: `*PRINT-PRETTY*` may insert line breaks mid-token. A compiled file is meant
#: to be read back, so a caller's `(let ((*print-length* 3)) (compile-file f))`
#: must not silently truncate it -- that is a corrupt output file reported as a
#: successful compilation, which is worse than a failure.
#:
#: `*PACKAGE*` and `*READTABLE*` are deliberately *not* pinned: they are the
#: file's own, and the output's IN-PACKAGE forms appear in the same order, so
#: symbols print and read back relative to the same package.
OUTPUT_PRINTER_CONTROLS = (
    ('*PRINT-ARRAY*', True),
    ('*PRINT-BASE*', 10),
    ('*PRINT-CASE*', 'UPCASE'),
    ('*PRINT-ESCAPE*', True),
    ('*PRINT-GENSYM*', True),
    ('*PRINT-LENGTH*', None),
    ('*PRINT-LEVEL*', None),
    ('*PRINT-LINES*', None),
    ('*PRINT-PRETTY*', False),
    ('*PRINT-RADIX*', False),
    ('*PRINT-RIGHT-MARGIN*', None),
)


def _print_for_output(form):
    """`form`'s printed representation, written so it can be read back.

    The printer controls are pinned for the duration (see
    `OUTPUT_PRINTER_CONTROLS`) through `BindingFrame`, the same mechanism LET
    uses, and unwound immediately -- so the *compile-time* forms COMPILE-FILE
    evaluates still see the caller's printer environment.
    """
    import fclpy.state as state
    from .binding import BindingFrame, root_environment
    from fclpy.printer import write_object

    frame = BindingFrame(root_environment(state.current_environment))
    try:
        for name, value in OUTPUT_PRINTER_CONTROLS:
            symbol = lisptype.COMMON_LISP_PACKAGE.intern_symbol(name)
            if value is True:
                bound = lisptype.T
            elif value is False or value is None:
                bound = lisptype.NIL
            elif name == '*PRINT-CASE*':
                bound = lisptype.intern_keyword(value)
            else:
                bound = value
            frame.bind(symbol, bound)
        return write_object(form)
    finally:
        frame.unwind()


@_registry.cl_function('COMPILE-FILE-PATHNAME')
def compile_file_pathname(input_file, *, output_file=None, **kwargs):
    """The pathname COMPILE-FILE would write for `input_file` (CLHS 24.2).

    `:output-file`, when supplied, *is* the answer; otherwise the input's type
    is replaced by this implementation's compiled-file type. Both go through
    `pathnames.resolve_filespec`, so this and COMPILE-FILE cannot disagree
    about where a relative name points -- they used to, each carrying its own
    copy of the search.
    """
    import os
    from .pathnames import Pathname, resolve_filespec

    if output_file is not None and output_file is not lisptype.NIL:
        return Pathname(resolve_filespec(output_file))
    base = os.path.splitext(resolve_filespec(input_file))[0]
    return Pathname(base + COMPILED_FILE_TYPE)


@_registry.cl_function('COMPILE-FILE')
def compile_file(input_file, *, output_file=None, verbose=lisptype.OMITTED,
                 print=lisptype.OMITTED, external_format=None):
    """COMPILE-FILE (CLHS 24.2): read `input_file`, process its top-level
    forms, and write the result where LOAD can read it back.

    fclpy has no code generator, so "compiling" is: read each top-level form
    with `*PACKAGE*`, `*READTABLE*`, `*COMPILE-FILE-PATHNAME*` and
    `*COMPILE-FILE-TRUENAME*` bound as CLHS requires; evaluate the ones CLHS
    3.2.3.1 says the compiler must evaluate (see `COMPILE_TIME_OPERATORS`);
    and print every form to the output file.

    Printing the forms rather than copying the source bytes is the point:

    * `#.` is *read*-time evaluation, so it must be resolved now, while
      `*COMPILE-FILE-TRUENAME*` is bound. `compile-file.16` compiles a file
      whose body is ``'#.*compile-file-truename*`` and then checks that
      loading the output yields that truename; a byte copy defers the `#.` to
      load time, when the variable is NIL.
    * a macro character the compiling environment established is likewise
      resolved now, so the output loads correctly whatever the readtable is
      then (`compile-file.15`).

    Returns the three values CLHS specifies: the output truename, `warnings-p`
    and `failure-p`.
    """
    import builtins
    import os
    import fclpy.state as state
    from .pathnames import Pathname, resolve_filespec
    from .streams import Stream
    from .binding import BindingFrame, root_environment, dynamic_value
    from .evaluation_conditions import signal_file_error
    from .io_write import write_text
    from .io_read import read as read_form
    from .evaluation_core import eval as lisp_eval, ConditionException
    from fclpy.printer import write_object
    from fclpy.readtable import get_current_readtable

    def _cl(name):
        return lisptype.COMMON_LISP_PACKAGE.intern_symbol(name)

    verbose_p = lisptype.is_truthy(
        verbose if lisptype.supplied(verbose)
        else dynamic_value(_cl('*COMPILE-VERBOSE*'), lisptype.NIL))
    print_p = lisptype.is_truthy(
        print if lisptype.supplied(print)
        else dynamic_value(_cl('*COMPILE-PRINT*'), lisptype.NIL))

    input_path = resolve_filespec(input_file)
    if not os.path.exists(input_path):
        return signal_file_error(
            Pathname(input_path), "COMPILE-FILE: file not found: " + input_path)

    if output_file is not None and output_file is not lisptype.NIL:
        output_path = resolve_filespec(output_file)
    else:
        output_path = os.path.splitext(input_path)[0] + COMPILED_FILE_TYPE

    if verbose_p:
        write_text("; compiling " + input_path + "\n")

    env = state.current_environment
    if env is None:
        raise lisptype.LispEnvironmentError(
            "COMPILE-FILE: no environment available")

    diagnostics = _CompilationDiagnostics()
    frame = BindingFrame(root_environment(env))
    source = builtins.open(input_path, 'r', encoding='utf-8')
    stream = Stream(input_path, source, 'input')
    printed_forms = []
    try:
        frame.bind(_cl('*COMPILE-FILE-PATHNAME*'), Pathname(input_path))
        frame.bind(_cl('*COMPILE-FILE-TRUENAME*'),
                   Pathname(os.path.realpath(input_path)))
        current_package = dynamic_value(_cl('*PACKAGE*'))
        if not isinstance(current_package, lisptype.Package):
            current_package = (getattr(state, 'current_package', None)
                               or lisptype.COMMON_LISP_USER_PACKAGE)
        frame.bind(_cl('*PACKAGE*'), current_package)
        frame.bind(_cl('*READTABLE*'), get_current_readtable())

        state.handler_stack.append(diagnostics.cluster())
        try:
            eof = object()
            while True:
                form = read_form(stream, lisptype.NIL, eof)
                if form is eof:
                    break
                # Printed *before* its compile-time effects run, so every
                # symbol is printed relative to the package current when the
                # form was read -- which is the package the reader will be in
                # when the output is loaded back, because the output's own
                # IN-PACKAGE forms appear there in the same order.
                text = _print_for_output(form)
                printed_forms.append(text)
                if print_p:
                    write_text(text + "\n")
                for compile_time_form in _compile_time_forms(form):
                    lisp_eval(compile_time_form, state.current_environment)
        except ConditionException as exception:
            # An error during compilation is a failure, not an abort: CLHS
            # has COMPILE-FILE return with failure-p true.
            diagnostics.note(exception.condition)
        finally:
            state.handler_stack.pop()
    finally:
        frame.unwind()
        source.close()

    with builtins.open(output_path, 'w', encoding='utf-8') as out:
        for text in printed_forms:
            out.write(text)
            out.write("\n")

    return lisptype.MultipleValues(
        Pathname(os.path.realpath(output_path)),
        lisptype.lisp_bool(diagnostics.warnings),
        lisptype.lisp_bool(diagnostics.failure))


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
    """Print object to stream (CLHS 9.1.3 default method / 22.1.3).

    Actually writes to `stream`, honouring the current *PRINT-ESCAPE* binding
    -- the previous stub returned `repr(obj)` without touching the stream at
    all, so `(with-output-to-string (s) (print-object c s))` always captured
    the empty string regardless of what the object's printed representation
    was (the same "measurement gate" shape C7 found in front of every
    `def-print-test`; see plan.md).
    """
    from fclpy.lispfunc.io_write import write_text
    from fclpy.printer import write_object as _write_object
    write_text(_write_object(obj), stream)
    return obj


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
    """Copy every cons of a tree, sharing every leaf (CLHS 14.2).

    It copied a Python `list` and returned everything else unchanged -- and a
    Python list is a *vector* here (plan.md Finding M), so the one shape it
    handled was the one shape a Lisp tree never has, and COPY-TREE was the
    identity function on every actual cons tree. `copy-tree.1`/`.2` check that
    no cons is shared with the original *and* that every atom is.

    Recursion follows both car and cdr, which is what distinguishes COPY-TREE
    from COPY-LIST: a dotted tail is a leaf and is shared, but a sublist in
    either position is copied.
    """
    from .core import _consp_internal
    if not _consp_internal(obj):
        return obj
    return lisptype.lispCons(copy_tree(obj.car), copy_tree(obj.cdr))


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


def _check_cons(value, operator):
    """Signal unless `value` is a cons (CLHS 14.2).

    RPLACA/RPLACD take a CONS, not a list: NIL has no car to replace, so
    `(rplaca nil 1)` is a TYPE-ERROR just as `(rplaca 'a 1)` is.
    """
    from .core import _consp_internal
    if not _consp_internal(value):
        raise lisptype.LispTypeError(
            f"{operator}: {value!r} is not a cons",
            expected_type="CONS", actual_value=value)
    return value


@_registry.cl_function('RPLACA')
def rplaca(cons, new_car):
    """Replace the car of a cons, returning that cons (CLHS 14.2).

    The `except Exception: pass` this replaced is standing rule 4 in its purest
    form: for every non-cons argument the assignment failed, the failure was
    discarded, and RPLACA returned the argument unchanged -- so
    `(rplaca 'a 1)` answered A instead of signalling, and a caller had no way
    to tell a successful mutation from a silently skipped one.
    """
    _check_cons(cons, 'RPLACA')
    cons.car = new_car
    return cons


@_registry.cl_function('RPLACD')
def rplacd(cons, new_cdr):
    """Replace the cdr of a cons, returning that cons (CLHS 14.2)."""
    _check_cons(cons, 'RPLACD')
    cons.cdr = new_cdr
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


# MAP-INTO and MAPCON are implemented in `sequences_higher`. The stubs that
# used to shadow them here returned the destination unchanged and `[]`
# respectively -- silently wrong answers under the same registered names
# (standing rules 3 and 4).


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
    """CLHS FILE-ERROR-PATHNAME: the PATHNAME slot of a FILE-ERROR.

    The value is whatever was given as :PATHNAME -- a namestring, a pathname,
    a logical pathname or a stream -- and is returned unchanged, because CLHS
    specifies the reader of a slot, not a coercion: file-error-pathname.1/.3
    require the *string* back out, and .5/.6 require the *stream* object back
    out. Coercing here (the obvious "return a pathname" reading) makes all
    four fail.
    """
    if isinstance(condition, lisptype.Condition):
        value = condition.get_slot('pathname')
        return lisptype.NIL if value is None else value
    raise lisptype.LispTypeError(
        f"FILE-ERROR-PATHNAME: {condition} is not a FILE-ERROR",
        expected_type='FILE-ERROR', actual_value=condition)


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


# --- Modules (CLHS 24.1.5) ---
#
# `*MODULES*`, PROVIDE and REQUIRE were three stubs that returned their own
# argument: `*MODULES*` had no value at all (so a bare reference signalled
# UNBOUND-VARIABLE and every one of the thirteen modules.lsp tests failed on
# the *reference*, before reaching what it was testing), and neither operator
# touched it. The mechanism they were missing is small but it is a mechanism:
# a module name is a **string designator**, `*MODULES*` is a list of the
# *strings* those designators denote, and REQUIRE's job is "load it unless
# PROVIDE has already recorded it".

def _modules_symbol():
    return lisptype.COMMON_LISP_PACKAGE.intern_symbol('*MODULES*')


def _module_name(designator):
    """The string a module-name designator denotes (CLHS PROVIDE/REQUIRE).

    Goes through `misc_packages._designator_to_string`, the existing single
    string-designator resolver, rather than adding a fourth copy: it already
    handles a symbol (including a keyword, and a reader-escaped `|FOO|`
    name), a character, and every specialized character-array shape the suite
    exercises -- which is exactly what modules.5/.10/.12 test.
    """
    from .misc_packages import _designator_to_string
    return _designator_to_string(designator)


def _modules_list():
    """`*MODULES*` as a Python list of strings, whatever shape it holds."""
    from .binding import dynamic_value
    from .sequence_protocol import list_elements
    value = dynamic_value(_modules_symbol(), lisptype.NIL)
    if value is lisptype.NIL or value is None:
        return []
    return list(list_elements(value))


@_registry.cl_function('PROVIDE')
def provide(module_name):
    """Record that `module_name` has been provided (CLHS PROVIDE).

    Adds the module's *name string* to `*MODULES*` unless a STRING= entry is
    already there -- modules.3 checks exactly that idempotence, by counting
    the entries after two PROVIDEs of the same name.
    """
    from .binding import set_dynamic_value
    from .misc_packages import _lisp_list
    name = _module_name(module_name)
    modules = _modules_list()
    if not any(str(m) == name for m in modules):
        set_dynamic_value(_modules_symbol(),
                          _lisp_list([lisptype.LispString(name)] + modules))
    return lisptype.NIL


@_registry.cl_function('REQUIRE')
def require(module_name, pathname_list=None):
    """Load `module_name` unless PROVIDE has already recorded it (CLHS REQUIRE).

    `pathname_list` is a single pathname designator or a list of them, each
    LOADed in order. With no pathname list and no already-provided module
    there is nothing this implementation can consult, so it signals an
    error -- which is what CLHS requires ("If the module-name is not
    [provided] ... an error of type ERROR is signaled") and what modules.9
    checks. Returning the name, as the stub did, reported success for a
    module that was never loaded.
    """
    from .sequence_protocol import list_elements
    name = _module_name(module_name)
    if any(str(m) == name for m in _modules_list()):
        return lisptype.NIL

    if pathname_list is None or pathname_list is lisptype.NIL:
        from .evaluation_conditions import signal_error_object
        return signal_error_object(lisptype.SimpleError(
            format_control=f"REQUIRE: module {name} has not been provided "
                           f"and no pathname was supplied"))

    if isinstance(pathname_list, lisptype.lispCons):
        pathnames = list(list_elements(pathname_list))
    else:
        pathnames = [pathname_list]

    for pathname in pathnames:
        load(pathname)
    return lisptype.T


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
    'with_pprint_logical_block',
    'with_slots',
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
