"""I/O write operations - stream output, printing, pathnames, and file operations."""

import re
from fractions import Fraction
from decimal import Decimal, ROUND_HALF_EVEN

import fclpy.lisptype as lisptype
from . import registry as _registry
from .streams import open_file as open_fn, close_stream as close_fn, open_stream_p
from .core import _null_internal, _consp_internal, _listp_internal
from fclpy import typespec


# === The printer, and where output goes ===
#
# The Lisp printer lives in `fclpy/printer.py` -- one implementation, reading
# the printer control variables from the live dynamic environment.
#
# What used to be here was a `PrinterSettings` object holding those variables as
# Python globals, a set of `@cl_function('*PRINT-...*')` accessors onto it, and
# a `_print_with_limits` printer that honoured them. None of it was reachable.
# No binding form can assign a Python global, so `(let ((*print-base* 2)) ...)`
# could not affect the settings object; nothing called `_print_with_limits`, so
# every CL entry point below printed via `lisptype.lisp_str`/`lisp_repr`, which
# are `str()`/`repr()`; and registering a *function* named `*PRINT-BASE*` is
# what made a reference to `*print-base*` evaluate to a Python function object,
# since the function registry is where the evaluator looks after failing to
# find a variable. The variables are now bound with their ANSI initial values
# in `lispenv.setup_standard_environment`, from `printer.PRINTER_VARIABLES`.
from fclpy import printer as _printer
from fclpy.printer import write_object as _write_object


@_registry.cl_function('STREAM-ELEMENT-TYPE')
def stream_element_type(stream):
    """STREAM-ELEMENT-TYPE (CLHS 21.1).

    This returned a raw Python string -- a Python object standing in for a
    Lisp value (standing rule 2), so `(eq (stream-element-type s) 'character)`
    was false regardless of the stream -- and it was hardcoded to CHARACTER
    unconditionally, which was honest while `OPEN` had no binary streams at
    all (plan.md C11). Now that `OPEN` records the declared `:element-type`
    on the stream (`streams.py`'s `_classify_element_type`), this returns it
    back verbatim -- a symbol/cons the caller supplied, or the interned
    CHARACTER symbol for the default/text case -- which is what lets
    `(subtypep '(unsigned-byte 12) (stream-element-type s))` consult the
    real SUBTYPEP lattice instead of a constant.

    Explicitly decorated rather than left to `register_module`'s auto
    registration: that heuristic strips a trailing "-TYPE" as an assumed
    Python-naming artifact (as it should for e.g. `array_element_type`'s
    callers that already spell the CLHS name without it), which silently
    registered this one under the wrong name, `STREAM-ELEMENT` -- CLHS's
    name for this operator, unlike `ARRAY-ELEMENT-TYPE`, ends in TYPE for
    real.
    """
    from .streams import Stream
    if not isinstance(stream, Stream):
        raise lisptype.LispTypeError(
            f"STREAM-ELEMENT-TYPE: not a stream: {stream!r}",
            expected_type="STREAM", actual_value=stream)
    element_type = stream.element_type
    if isinstance(element_type, (lisptype.LispSymbol, lisptype.lispKeyword, lisptype.lispCons)):
        return element_type
    if isinstance(element_type, str):
        return lisptype.COMMON_LISP_PACKAGE.intern_symbol(element_type.upper())
    return lisptype.COMMON_LISP_PACKAGE.intern_symbol('CHARACTER')


def stream_external_format(stream):
    """STREAM-EXTERNAL-FORMAT (CLHS 21.1.2, implementation-defined).

    `:DEFAULT` per CLHS glossary's own default-value note, not `:UTF-8` --
    the latter is a real, more specific answer this implementation has no
    per-stream external-format model to actually back up.
    """
    return lisptype.intern_keyword('DEFAULT')


def resolve_output_stream(designator):
    """Resolve an output stream designator (CLHS 21.1.3).

    NIL -- which is what a missing optional stream argument is -- designates the
    current value of `*STANDARD-OUTPUT*`, and T designates `*TERMINAL-IO*`.

    Every output function has to come through here. They used to default to
    Python's `print()`, so output went to the process's stdout regardless of
    what `*STANDARD-OUTPUT*` was bound to. That one defect is why the printer
    was unmeasurable: `(with-output-to-string (*standard-output*) (prin1 x))` is
    the shape every `def-print-test` in `printer/` uses to capture output, and
    it returned the empty string for all of them no matter what the printer did.
    """
    import fclpy.state as state

    if designator is True or designator is lisptype.T:
        name = '*TERMINAL-IO*'
    elif designator is None or designator is lisptype.NIL:
        name = '*STANDARD-OUTPUT*'
    else:
        return designator

    env = getattr(state, 'current_environment', None)
    symbol = lisptype.COMMON_LISP_PACKAGE.intern_symbol(name)
    if env is not None and env.has_variable(symbol):
        return env.find_variable(symbol)
    return getattr(symbol, 'value', None)


def write_text(text, stream=None):
    """Write `text` to an output stream designator -- the one place text is written.

    `PRIN1` and `PRINC` each carried their own four-branch copy of this
    dispatch (stdout / fclpy `Stream` / Python file-like / bare `except`), which
    is how they drifted apart from each other and from FORMAT about where
    unspecified output goes.
    """
    target = resolve_output_stream(stream)
    if target is None:
        # Reachable only before the standard environment exists, i.e. during
        # bootstrap. A real gap rather than the normal path, so it is not
        # quietly equivalent to writing to a stream.
        import sys
        sys.stdout.write(text)
        return
    from .streams import Stream
    if isinstance(target, Stream):
        target.write_sequence(text)
    elif isinstance(target, lisptype.LispString):
        # FORMAT accepts a string with a fill pointer as its destination and
        # appends to it (CLHS 22.3.1). Reaching here used to raise an
        # AttributeError that a bare `except` turned into printing at stdout,
        # so the output silently went somewhere other than the string.
        if target.fill_pointer is None:
            raise lisptype.LispTypeError(
                "FORMAT to a string requires a fill pointer: "
                f"{_write_object(target, escape=True)}")
        target._data[target.fill_pointer:] = list(text)
        target.fill_pointer += len(text)
    else:
        writer = getattr(target, 'write', None)
        if writer is None:
            raise lisptype.LispTypeError(f"Not an output stream: {target!r}")
        writer(text)
    if text:
        # Record the column for FRESH-LINE. Only a non-empty write moves it.
        try:
            setattr(target, _AT_LINE_START, text.endswith('\n'))
            nl = text.rfind('\n')
            col = len(text) - nl - 1 if nl != -1 else getattr(target, _COLUMN_ATTR, 0) + len(text)
            setattr(target, _COLUMN_ATTR, col)
        except AttributeError:
            # A stream that refuses attributes (e.g. a raw file object with
            # __slots__) simply has no recorded column; `_at_line_start` then
            # reports "at a line start", so FRESH-LINE emits nothing rather
            # than a spurious newline. `PPRINT-LOGICAL-BLOCK`'s own in-memory
            # buffer (`_PPBuffer`) deliberately falls in here too -- it holds
            # unresolved sentinel tags, not real characters, so a numeric
            # column computed from its raw length would be meaningless; its
            # column is instead read from its *resolved* text on demand
            # (`_pp_outer_column`).
            pass


# --- PRINT-UNREADABLE-OBJECT (CLHS 22.4) ---
#
# The macro itself is in `evaluation_special_forms.py` with the other `WITH-*`
# expanders; these are the two runtime halves it expands into. Splitting it
# that way keeps the *layout* -- what `#<...>` actually contains -- here in the
# printer, next to `write_text`, rather than encoded in a macroexpansion.
#
# CLHS fixes the layout, and `printer/print-unreadable-object.lsp` pins the
# spacing down exactly:
#
#     #<                     no type, no identity, empty body
#     #<TYPE >               :type t, empty body
#     #<TYPE body>           :type t  -- the space belongs to the *type*
#     #<body identity>       :identity t -- the space belongs to the *identity*
#
# so a space follows the type when one is printed, and precedes the identity
# when one is printed; nothing else inserts one.


@_registry.cl_function('%PRINT-UNREADABLE-PREFIX')
def print_unreadable_prefix(object, stream, type_p):
    """Write `#<`, and the object's type plus a space when `type_p`.

    The `*PRINT-READABLY*` check lives here because it must happen before any
    output at all: CLHS says PRINT-UNREADABLE-OBJECT signals
    PRINT-NOT-READABLE if `*print-readably*` is true, and
    `print-unreadable-object.error.1` asserts the stream is left *empty* when
    it does.
    """
    from .binding import dynamic_value
    from .evaluation_conditions import signal_error_object
    readably = dynamic_value(lisptype.COMMON_LISP_PACKAGE.intern_symbol('*PRINT-READABLY*'))
    if lisptype.is_truthy(readably):
        return signal_error_object(lisptype.PrintNotReadable(object=object))
    write_text('#<', stream)
    if lisptype.is_truthy(type_p):
        from .comparison import type_of
        write_text(f"{_write_object(type_of(object), escape=False)} ", stream)
    return lisptype.NIL


@_registry.cl_function('%PRINT-UNREADABLE-SUFFIX')
def print_unreadable_suffix(object, stream, identity_p):
    """Write the identity (preceded by a space) when `identity_p`, then `>`."""
    if lisptype.is_truthy(identity_p):
        write_text(f" {id(object):x}", stream)
    write_text('>', stream)
    return lisptype.NIL


@_registry.cl_function('PRINT-NOT-READABLE-OBJECT')
def print_not_readable_object(condition):
    """PRINT-NOT-READABLE-OBJECT (CLHS 22.4): the OBJECT slot of a
    PRINT-NOT-READABLE condition.

    The condition class carried the slot but this accessor was never
    registered, so every reference to it was an UNDEFINED-FUNCTION -- including
    from `documentation.lsp`, which has nothing to do with printing and merely
    printed a condition while reporting an unrelated failure.
    """
    if isinstance(condition, lisptype.Condition):
        value = condition.get_slot('object')
        return value if value is not None else lisptype.NIL
    return getattr(condition, 'object', lisptype.NIL)


# Re-export pathname functions from pathnames module for backward compatibility
# Note: `pathname` (registered as 'PATHNAME', coerces a designator) and
# `make_pathname_function` (registered as 'MAKE-PATHNAME', builds one from
# components) are different functions!
from .pathnames import (
    pathname,  # PATHNAME function - coerces a pathname designator
    make_pathname_function,  # MAKE-PATHNAME function - constructs pathname from components
    pathnamep,
    pathname_host,
    pathname_device,
    pathname_directory,
    pathname_name,
    pathname_type,
    pathname_version,
    namestring,
    directory_namestring,
    file_namestring,
    host_namestring,
    enough_namestring,
    parse_namestring,
    merge_pathnames,
    wild_pathname_p,
    pathname_match_p,
    translate_pathname,
    logical_pathname,
    translate_logical_pathname,
    truename,
    probe_file,
    file_write_date,  # FILE-WRITE-DATE: the one registration lives in pathnames.py
)


@_registry.cl_function('CLEAR-OUTPUT')
def clear_output(stream=None):
    """CLEAR-OUTPUT: discard buffered output (CLHS 21.2).

    Raises:
        LispTypeError if stream is not a stream, NIL, or T
    """
    from .streams import Stream
    # Accept NIL, T, or a stream; anything else is an error
    if stream is not None and stream is not True and stream is not lisptype.NIL and stream is not lisptype.T:
        if not isinstance(stream, Stream):
            raise lisptype.LispTypeError(
                f"CLEAR-OUTPUT: not a stream: {stream!r}",
                expected_type='(OR STREAM (MEMBER NIL T))', actual_value=stream)

    # For now, this is a no-op - output goes directly to Python's file object
    # with its own buffering that we don't control at the Lisp level
    return lisptype.NIL


@_registry.cl_function('OUTPUT-STREAM-P')
def output_stream_p(stream):
    """OUTPUT-STREAM-P: can `stream` be used for output (CLHS 21.1)?

    Raises:
        LispTypeError if stream is not a stream
    """
    from .streams import Stream
    if not isinstance(stream, Stream):
        raise lisptype.LispTypeError(
            f"OUTPUT-STREAM-P: not a stream: {stream!r}",
            expected_type='STREAM', actual_value=stream)
    return lisptype.lisp_bool(stream.direction in ('output', 'io'))


# OPEN-STREAM-P is streams.py's, not registered a second time here: that
# copy already asks `stream.is_open()`, but being imported *after*
# streams.py meant a stub here that always answered T used to win the
# registry regardless (standing rule 3).


# I/O write operations
@_registry.cl_function('WRITE-CHAR')
def write_char(character, stream=None):
    """Write a character to a stream (CLHS 21.2)."""
    text = character.char if isinstance(character, lisptype.Character) else str(character)
    write_text(text, stream)
    return character


def _string_to_text(string, what='WRITE-STRING'):
    """Extract text from a string or string-like object.

    Handles LispString, plain str, lists/tuples of Characters, and character arrays.
    """
    if isinstance(string, lisptype.LispString):
        return ''.join(string)
    if isinstance(string, str):
        return string
    if isinstance(string, (list, tuple)) and all(isinstance(c, lisptype.Character) for c in string):
        return ''.join(c.char for c in string)
    from . import arrays as _arrays
    if isinstance(string, _arrays.LispArray) and _arrays.array_rank_of(string) == 1:
        et = _arrays.element_type_of(string)
        et_name = et.name if isinstance(et, lisptype.LispSymbol) else str(et)
        if et_name.upper() in ('CHARACTER', 'BASE-CHAR', 'STANDARD-CHAR', 'NIL'):
            from .sequence_protocol import seq_elements
            chars = seq_elements(string, what)
            return ''.join(c.char if isinstance(c, lisptype.Character) else str(c)
                           for c in chars)
    raise lisptype.LispTypeError(
        f"{what}: argument must be a string, not {_write_object(string, escape=True)}",
        expected_type='STRING', actual_value=string)


@_registry.cl_function('WRITE-STRING')
def write_string(string, stream=None, *, start=0, end=None):
    """Write a string's characters to a stream, without escapes (CLHS 21.2)."""
    text = _string_to_text(string, 'WRITE-STRING')
    if end is None or end is lisptype.NIL:
        end = len(text)
    write_text(text[start:end], stream)
    return string


@_registry.cl_function('WRITE-LINE')
def write_line(string, stream=None, *, start=0, end=None):
    """WRITE-STRING followed by a newline (CLHS 21.2)."""
    text = _string_to_text(string, 'WRITE-LINE')
    if end is None or end is lisptype.NIL:
        end = len(text)
    write_text(text[start:end] + '\n', stream)
    return string


@_registry.cl_function('WRITE-BYTE')
def write_byte(byte, stream):
    """WRITE-BYTE (CLHS 21.2): write one integer element to a binary stream.

    Was a complete no-op -- `return byte` never touched the stream at all,
    so every OPEN test built on a binary `:element-type` (streams/open.lsp's
    `(unsigned-byte n)`/`bit`/`signed-byte` family) read back whatever the
    file already held from an earlier test. `stream.byte_width`/
    `byte_signed`, set by OPEN from the declared element-type
    (`streams._classify_element_type`), decide the physical encoding; it
    only has to round-trip through this implementation's own WRITE-BYTE/
    READ-BYTE, not match any other implementation's on-disk layout.
    """
    from .streams import Stream
    if not isinstance(stream, Stream) or not stream.binary:
        raise lisptype.LispTypeError(
            f"WRITE-BYTE: {stream!r} is not a binary output stream",
            expected_type='STREAM', actual_value=stream)
    value = int(byte)
    data = value.to_bytes(stream.byte_width, 'big', signed=stream.byte_signed)
    stream.file_obj.write(data)
    return byte


def _print_keywords(kwargs):
    """Translate WRITE's keyword arguments into printer overrides.

    WRITE and WRITE-TO-STRING accept a keyword for each printer control
    variable (CLHS 22.3.1), each overriding that variable for the call. They
    used to be collected into `**kwargs` and dropped on the floor, so
    `(write-to-string x :base 2)` ignored the base and `(write-to-string '(a)
    :escape nil)` still escaped. `printer.WRITE_KEYWORD_VARIABLES` is the one
    table of which keyword maps to which variable.
    """
    normalized = {key.lower().replace('-', '_'): value
                   for key, value in kwargs.items()}
    # `:allow-other-keys` is accepted by every function that takes keyword
    # arguments, and a true value makes unrecognized keywords legal to pass and
    # ignore (CLHS 3.4.1.4) -- `(write 5 :allow-other-keys t :foo 'bar)` prints
    # "5" rather than signalling.
    permissive = _printer._true(normalized.pop('allow_other_keys', None))

    overrides = {}
    for name, value in normalized.items():
        if name not in _printer.WRITE_KEYWORD_VARIABLES:
            if permissive:
                continue
            raise lisptype.LispProgramError(
                f"WRITE: unknown keyword argument :{name.upper().replace('_', '-')}")
        overrides[name] = value
    return overrides


def _dispatch_print(object, overrides, stream):
    """`printer.write_object` routed through the pprint dispatch when pretty.

    CLHS 22.1.3: when `*print-pretty*` is true, writing is "controlled by the
    pretty printer", and the pretty printer routes each object through
    `PPRINT-DISPATCH` to find the function that actually produces its
    representation. The dispatch function takes `(stream, object)`, so when
    the dispatch is consulted the standard `printer.write_object` is *not*
    called for the object -- the registered function writes to the stream
    directly (`pprint-dispatch.3`'s `(write "ABC" :stream stream)` body).

    Returns a *string* representation of `object` regardless of whether
    `stream` is a real stream or NIL/T (`WRITE-TO-STRING` / `PRIN1-TO-STRING`
    pass NIL and want a string back; `WRITE` / `PRIN1` pass a real stream
    and the result is what `write_text` writes to that stream). The
    dispatch function does the actual writing through `stream`; we collect
    the result into a string and return it so `write_text` is a single
    consumer in both paths.

    When `*print-pretty*` is false, or the dispatch falls through to the
    default fallback, the ordinary `printer.write_object` does the work --
    a dispatch entry cannot make output less correct than the standard
    printer produces, only more elaborate.

    Every write here is also where a print inside an open `PPRINT-LOGICAL-BLOCK`
    honours `*PRINT-CIRCLE*` (CLHS 22.2.2: "the arguments of the standard
    printing functions ... are all checked (when necessary) for circularity
    and sharing"): a labelled object prints an `#n=` before it the first
    time, or collapses to `#n#` when it was already printed. The check does
    not apply to WRITE-STRING/WRITE-LINE/WRITE-CHAR (CLHS, same paragraph),
    and those do not come through here.
    """
    circle = _pp_circle_active()
    label_prefix = ''
    if circle is not None and _pp_is_circle_aggregate(object):
        label, skip = _pp_circle_label(circle, object)
        if skip:
            return label
        label_prefix = label
    if not label_prefix:
        # The overwhelmingly common case -- no labelling owed -- must return
        # the body's result *as is*: some callers return a `LispString`
        # (FORMAT's capture), which a `str +` would reject.
        return _dispatch_print_body(object, overrides, stream)
    return label_prefix + _dispatch_print_body(object, overrides, stream)


def _dispatch_print_body(object, overrides, stream):
    """The pre-`*PRINT-CIRCLE*` body of `_dispatch_print`."""
    # An explicit `:pretty` argument overrides the variable for this one
    # call (CLHS 22.3.1's "each of the keyword arguments ... which correspond
    # to printer control variables" are bound around the print) -- WRITE's
    # `:pretty nil` must print unprettily even inside a pretty dynamic
    # context, and PPRINT.1's random write-args exercise exactly that.
    if 'pretty' in overrides and overrides['pretty'] is not None:
        pretty = _printer._true(overrides['pretty'])
    else:
        pretty = _printer._true(_printer.resolve_control('*PRINT-PRETTY*'))
    if not pretty:
        return _write_object(object, **overrides)
    dispatch_table = _current_pprint_table()
    dispatch_fn, found_p = pprint_dispatch(object, dispatch_table)
    # A dispatch entry that matched: collect its output by writing to a
    # throwaway string stream, regardless of the caller's `stream`. The
    # dispatch function is documented to write through its own stream
    # argument; the caller (WRITE / WRITE-TO-STRING) gets the captured
    # text through `_dispatch_print`'s return value and decides where it
    # actually goes (a real stream, or up to the caller's `stream` arg).
    if found_p is lisptype.T or found_p is True:
        from .streams import make_string_output_stream
        capture_stream = make_string_output_stream()
        dispatch_fn(capture_stream, object)
        # The registered dispatch function wrote to its own stream; we collect
        # that text here. `make_string_output_stream` returns a
        # `StringOutputStream`, whose `get_string()` is the one accessor
        # `get-output-stream-string` itself delegates to.
        return capture_stream.get_string()
    # No entry matched -- the fallback is the print-object dispatch function,
    # which writes via the standard printer. The standard printer already
    # honours the `*print-pretty*` bound here, so a fresh pretty-print
    # consults the same dispatch for sub-aggregates and falls through again,
    # which is what keeps `*print-pretty*` propagating through the print.
    return _write_object(object, **overrides)


def _current_pprint_table():
    """The `*PRINT-PPRINT-DISPATCH*` value, looking through the dynamic chain.

    Reads through `binding.dynamic_value` so a `(let ((*print-pprint-dispatch*
    ...)) ...)` binding is honoured here, the way the pretty printer's
    own CALL into pprint-dispatch (which would read the variable cell
    directly) would not -- the entry point needs to see the binding, not
    the symbol's value cell.
    """
    from .binding import dynamic_value
    return dynamic_value(
        lisptype.COMMON_LISP_PACKAGE.intern_symbol('*PRINT-PPRINT-DISPATCH*'),
        default=standard_pprint_dispatch())


@_registry.cl_function('WRITE')
def write(object, *, stream=None, **kwargs):
    """Print an object to a stream, honouring the printer keyword arguments.

    CLHS 22.3.1. WRITE is the general entry point: `PRIN1` and `PRINC` are it
    with `*PRINT-ESCAPE*` forced true and false respectively.

    `stream` is an `&key` parameter, keyword-only in the Python signature --
    the one mapping of CLHS's `&key` (CLAUDE.md's lambda-list rule) -- so a
    second *positional* argument is an arity error
    (`write-byte.error.5`'s `(write 1 s)` on a binary stream) rather than a
    silently accepted stream designator.

    Routes through the pprint dispatch when `*print-pretty*` is true; calls
    `printer.write_object` otherwise. The same routing is applied to the
    string-producing variants below.
    """
    write_text(_dispatch_print(object, _print_keywords(kwargs), stream), stream)
    return object


@_registry.cl_function('PRIN1-TO-STRING')
def prin1_to_string(object):
    """The escaped printed representation, as a string (CLHS 22.3.1).

    ``prin1-to-string`` acts like ``write-to-string`` with ``:escape t`` --
    `*PRINT-ESCAPE*` is bound to true for the print, overriding whatever the
    caller had (the same binding ``prin1`` itself makes). ``*PRINT-READABLY*``
    is *not* bound: CLHS's note is explicit that prin1 and print do not bind
    it, so readable output requested by the caller stays readable.
    """
    return lisptype.LispString(_dispatch_print(object, {'escape': True}, None))


@_registry.cl_function('PRINC-TO-STRING')
def princ_to_string(object):
    """The unescaped printed representation, as a string (CLHS 22.3.1).

    Binds `*PRINT-READABLY*` to NIL as well as `*PRINT-ESCAPE*` (X3J13
    PRINC-READABLY; CLHS 22.3.4.1 states the same binding for `~A`): PRINC's
    output is not required to be readable, and the inherited
    `*PRINT-READABLY*` from an enclosing `WITH-STANDARD-IO-SYNTAX` forced the
    escape back on -- `(princ #\\a ...)` printed `#\a` for every character,
    which is exactly what PRINT.CHAR.1 collects.
    """
    return lisptype.LispString(_dispatch_print(
        object, {'escape': False, 'readably': False}, None))


@_registry.cl_function('WRITE-TO-STRING')
def write_to_string(object, **kwargs):
    """WRITE to a string instead of a stream (CLHS 22.3.1).

    Defaults to escaped output like `PRIN1`, not to `PRINC` -- it is WRITE, and
    WRITE honours `*PRINT-ESCAPE*`, whose initial value is true.
    """
    return lisptype.LispString(_dispatch_print(object, _print_keywords(kwargs), None))


@_registry.cl_function('PRINT')
def print_fn(object, stream=None):
    """Newline, then the object escaped, then a space (CLHS 22.3.1).

    The order matters and was reversed: PRINT is defined as a `TERPRI`, then a
    `PRIN1`, then a space -- not `PRIN1` followed by a newline. Like `prin1`,
    it binds `*PRINT-ESCAPE*` to true -- an enclosing escape-nil binding
    (e.g. the random printer-control draws of `random-print-test`) must not
    turn it off, or the output stops being readable. `*PRINT-READABLY*` is
    not bound (CLHS's note on WRITE/PRIN1/PRINT).
    """
    write_text('\n' + _dispatch_print(object, {'escape': True}, stream) + ' ', stream)
    return object


@_registry.cl_function('PRIN1')
def prin1(object, stream=None):
    """Print an object escaped, with no surrounding whitespace (CLHS 22.3.1).

    ``(prin1 object output-stream) == (write object :stream output-stream
    :escape t)`` -- `*PRINT-ESCAPE*` is bound to true for the print, so an
    enclosing escape-nil binding does not affect the output. `*PRINT-READABLY*`
    is not bound (CLHS's note: prin1 and print do not bind it).
    """
    write_text(_dispatch_print(object, {'escape': True}, stream), stream)
    return object


@_registry.cl_function('PRINC')
def princ(object, stream=None):
    """Print an object with escaping off (CLHS 22.3.1).

    Not a separate representation from `PRIN1`: the same printer with
    `*PRINT-ESCAPE*` bound to NIL (CLHS 22.1.3.2). `*PRINT-READABLY*` is
    bound to NIL too (X3J13 PRINC-READABLY; CLHS 22.3.4.1 states the same
    binding for `~A`): an inherited `*print-readably*` -- `with-standard-io-syntax`
    binds it T -- forced the escape back on, so PRINC of a character printed
    `#\a` (PRINT.CHAR.1) and PRINC of an unnamed character signalled
    PRINT-NOT-READABLE (PRINT.CHAR.2).
    """
    write_text(_dispatch_print(
        object, {'escape': False, 'readably': False}, stream), stream)
    return object


@_registry.cl_function('TERPRI')
def terpri(stream=None):
    """Output a newline (CLHS 21.2)."""
    write_text('\n', stream)
    return lisptype.NIL


@_registry.cl_function('FRESH-LINE')
def fresh_line(stream=None):
    """Output a newline only if not already at the start of a line (CLHS 21.2).

    Returns T if it output one, NIL otherwise. It used to output one
    unconditionally and return NIL, so `~&` could not be distinguished from
    `~%`. Column tracking is per stream, so a stream that does not report its
    position is assumed to need the newline.
    """
    target = resolve_output_stream(stream)
    if _at_line_start(target):
        return lisptype.NIL
    write_text('\n', stream)
    return lisptype.T


#: Attribute `write_text` stamps on a stream to record whether its last write
#: ended a line. FRESH-LINE needs to know the column, and neither fclpy's
#: `Stream` nor a Python file object reports one; tracking it at the single
#: point where text is written is cheaper and more accurate than trying to
#: recover it afterwards, and it works for file streams too, not just the
#: string streams whose buffer can be re-read.
_AT_LINE_START = '_fclpy_at_line_start'

#: Attribute `write_text` stamps alongside `_AT_LINE_START`: the 0-based
#: column its last write left the stream at. `PPRINT-LOGICAL-BLOCK` needs a
#: real starting column to resolve indentation against (CLHS 22.2.1), and
#: unlike `_at_line_start` a boolean cannot answer "how far into the line".
_COLUMN_ATTR = '_fclpy_column'


def _at_line_start(target):
    """True when `target`'s next character would begin a line.

    A string output stream's buffer is authoritative and is preferred, since
    text can reach it by paths other than `write_text`. Otherwise the flag
    `write_text` recorded is used. A stream that has never been written to is at
    the start of a line.
    """
    if target is None:
        return False
    from .streams import StringOutputStream
    if isinstance(target, StringOutputStream):
        text = target.peek_string()
        return text == '' or text.endswith('\n')
    return getattr(target, _AT_LINE_START, True)

@_registry.cl_function('FINISH-OUTPUT')
def finish_output(stream=None):
    """FINISH-OUTPUT (CLHS 21.2): ensure `stream`'s output has actually
    reached its destination before returning.

    Was a complete no-op (`return None`) -- text written to a stream stayed
    in Python's file-object buffer, invisible to anything reading the same
    underlying file through a second stream, which is exactly what
    `streams/open.lsp`'s OPEN.66/OPEN.67/OPEN.OUTPUT.30/OPEN.IO.30 do:
    write to `s`, `(finish-output s)`, then `(open s :direction :input)` and
    expect to read what was just written.

    Raises:
        LispTypeError if stream is not a stream, NIL, or T
    """
    from .streams import Stream
    # Accept NIL, T, or a stream; anything else is an error
    if stream is not None and stream is not True and stream is not lisptype.NIL and stream is not lisptype.T:
        if not isinstance(stream, Stream):
            raise lisptype.LispTypeError(
                f"FINISH-OUTPUT: not a stream: {stream!r}",
                expected_type='(OR STREAM (MEMBER NIL T))', actual_value=stream)

    target = resolve_output_stream(stream)
    if isinstance(target, Stream):
        target.flush()
    return lisptype.NIL


@_registry.cl_function('FORCE-OUTPUT')
def force_output(stream=None):
    """FORCE-OUTPUT (CLHS 21.2): initiate output without necessarily
    waiting for it to complete. This implementation has no asynchronous
    I/O, so there is nothing weaker than FINISH-OUTPUT to do here.

    Raises:
        LispTypeError if stream is not a stream, NIL, or T
    """
    from .streams import Stream
    # Accept NIL, T, or a stream; anything else is an error
    if stream is not None and stream is not True and stream is not lisptype.NIL and stream is not lisptype.T:
        if not isinstance(stream, Stream):
            raise lisptype.LispTypeError(
                f"FORCE-OUTPUT: not a stream: {stream!r}",
                expected_type='(OR STREAM (MEMBER NIL T))', actual_value=stream)

    target = resolve_output_stream(stream)
    if isinstance(target, Stream):
        target.flush()
    return lisptype.NIL


# MAKE-STRING-OUTPUT-STREAM and GET-OUTPUT-STREAM-STRING are registered exactly
# once, in streams.py next to the StringOutputStream object model. The thin
# io_write.py delegates that used to win by import order were removed as part
# of the duplicate-register cleanup -- the streams.py versions are what every
# caller reaches. Re-export them under the io_write names so `from .io_write
# import *` (in io.py) and any direct importers keep working.
from .streams import make_string_output_stream, get_output_stream_string  # noqa: F401  -- re-export


# MAKE-BROADCAST-STREAM, MAKE-CONCATENATED-STREAM, MAKE-ECHO-STREAM,
# MAKE-SYNONYM-STREAM and MAKE-TWO-WAY-STREAM now live in streams.py, next to
# the Stream object model and the composite-stream classes (TwoWayStream,
# EchoStream, ConcatenatedStream, BroadcastStream, SynonymStream) they build --
# they used to be here as stubs that returned one of their own arguments
# unchanged (or, for MAKE-SYNONYM-STREAM, `str(symbol)`), which is a Python
# object standing in for a Lisp stream (standing rule 2), not a real
# implementation of the CLHS 21.1.2 composition.


# === Pretty printing operations ===
#
# The pretty printer itself is absent (every PPRINT-* below is a stub), but the
# *dispatch table* is an object CLHS names in two places that are already
# reachable: `*PRINT-PPRINT-DISPATCH*` and WITH-STANDARD-IO-SYNTAX's binding
# list, which CLHS 23.4 says binds it to "the standard pprint dispatch table".
# So the table needs an object model and one home, exactly as the readtable
# does -- `readtable.standard_readtable()` is the same shape. Without it,
# `COPY-PPRINT-DISPATCH` answered a bare Python `dict` (standing rule 2) and
# `lispenv` built the initial table from a class declared inline inside
# `setup_standard_environment`, so nothing else could name the object
# WITH-STANDARD-IO-SYNTAX has to rebind to.


class PprintDispatchTable:
    """A pretty-print dispatch table (CLHS 22.2.1.4).

    `entries` is the (type-specifier, function, priority) list SET-PPRINT-DISPATCH
    writes and PPRINT-DISPATCH reads. The pretty printer itself is still absent
    (no line breaking, no logical blocks), but PPRINT-DISPATCH and
    SET-PPRINT-DISPATCH consult the entries now, and the writer in this module
    routes through the dispatch when `*PRINT-PRETTY*` is true -- which is what
    `printer/pprint-dispatch.lsp` and `printer/copy-pprint-dispatch.lsp` check:
    setting `(EQL X)` on the dispatch and then `(write-to-string X)` must call
    the registered function, not the default printer.
    """

    def __init__(self, entries=None):
        self.entries = list(entries) if entries else []

    def copy(self):
        return PprintDispatchTable(self.entries)

    def remove_spec(self, type_specifier):
        """Drop every entry whose type specifier is `equal` to `type_specifier`.

        CLHS 22.2.1.4 guarantees that there is never more than one entry per
        type specifier in a given table; the first thing SET-PPRINT-DISPATCH
        does is remove any pre-existing entry. Equality is by `equal`, not by
        `eq`, so `(EQL X)` and `(EQL X)` match even when read at different
        times (different LispSymbol identities for X but same name).
        """
        self.entries = [e for e in self.entries
                        if not _equal_specifiers(e[0], type_specifier)]

    def __repr__(self):
        return "#<PPRINT-DISPATCH-TABLE>"


def _equal_specifiers(a, b):
    """CLHS 22.2.1.4's "equality of type specifiers is tested by EQUAL" rule.

    A type specifier is a Lisp list/symbol/form, so the EQUAL predicate is
    exactly the right test: two `(EQL X)` forms read from different places
    are EQUAL even though the cons cells (and X) are not EQ.
    """
    return _lisp_equal(a, b)


def _lisp_equal(a, b):
    """A small EQUAL covering the shapes type specifiers take.

    The same recursion as `comparison.equal`, kept local so the dispatch
    table's equality check does not depend on whichever comparison helper the
    evaluator happens to have on the import path. Returning `False` on any
    structural mismatch is fine -- the only way two entries collide is by
    EQUAL specifiers, and the only specifier shapes in the test suite are
    symbols and short lists.
    """
    if a is b:
        return True
    if a is None or b is None:
        return a is b
    if a is lisptype.NIL or b is lisptype.NIL:
        return a is b
    if isinstance(a, lisptype.LispSymbol) or isinstance(b, lisptype.LispSymbol):
        return (isinstance(a, lisptype.LispSymbol)
                and isinstance(b, lisptype.LispSymbol)
                and a.name == b.name
                and getattr(a, 'package', None) is getattr(b, 'package', None))
    if isinstance(a, lisptype.lispCons) and isinstance(b, lisptype.lispCons):
        return (_lisp_equal(a.car, b.car) and _lisp_equal(a.cdr, b.cdr))
    if isinstance(a, lisptype.Character) and isinstance(b, lisptype.Character):
        return a.char == b.char
    if isinstance(a, lisptype.LispString) and isinstance(b, lisptype.LispString):
        return str(a) == str(b)
    return a == b


_standard_pprint_dispatch = None


def standard_pprint_dispatch():
    """The **standard pprint dispatch table** (CLHS 22.2.1.4, 23.4).

    One shared object, the way `readtable.standard_readtable()` is: it is what
    `*PRINT-PPRINT-DISPATCH*` starts out holding and what
    WITH-STANDARD-IO-SYNTAX rebinds it to, so both must name the *same* table
    or the rebinding is unobservable.
    """
    global _standard_pprint_dispatch
    if _standard_pprint_dispatch is None:
        _standard_pprint_dispatch = PprintDispatchTable()
    return _standard_pprint_dispatch


@_registry.cl_function('COPY-PPRINT-DISPATCH')
def copy_pprint_dispatch(table=None):
    """Copy a pretty print dispatch table (CLHS 22.2.1.4).

    NIL denotes the standard table, as it does for every readtable designator.
    A non-table argument is a TYPE-ERROR (`copy-pprint-dispatch.error.2`
    hands every element of `*mini-universe*` that is not NIL and requires an
    error, not a `LispNotImplementedError` -- that is the wrong class, the
    same one that the previous implementation threw and that the
    `check-type-error` helper classifies as "not a type-error").
    """
    if table is None or table is lisptype.NIL:
        return standard_pprint_dispatch().copy()
    if isinstance(table, PprintDispatchTable):
        return table.copy()
    raise lisptype.LispTypeError(
        f"COPY-PPRINT-DISPATCH: not a pprint dispatch table: {table!r}",
        expected_type='PPRINT-DISPATCH-TABLE', actual_value=table)


class _PPBuffer:
    """In-memory sink for one `PPRINT-LOGICAL-BLOCK` frame's body output.

    CLHS 22.2.1's fitting decisions need the *whole* section between one
    conditional newline and the next -- a `:fill` break may depend on text
    that has not been written yet when the break itself runs, and a
    `:linear`/`:miser` break depends on whether the *entire* enclosing block
    fits. Evaluating the body left-to-right, writing straight to the real
    stream as `PPRINT-NEWLINE`/`PPRINT-INDENT` are called, cannot see that
    far ahead. So the body writes here instead -- as a list of `('text', s)`
    / `('break', kind)` / `('indent', relative_to, n)` tokens, plus a
    `('block', suffix_text, per_line_text, subtokens)` token for each nested
    `PPRINT-LOGICAL-BLOCK` -- and the whole tree is resolved in one
    left-to-right pass once the *outermost* block closes (`flush_pprint_frame`
    / `_pp_render_block`), mirroring the tokenize/measure/render shape
    FORMAT's `~<...~:>` logical block already uses, but keeping a nested
    block as a node in the tree rather than pre-flattened text.

    A nested block's own fitting decision needs the column it actually
    starts at, which depends on whether the *enclosing* block's own earlier
    breaks fired -- not decidable until the whole enclosing block is
    resolved. Deferring nested blocks as tree nodes, rather than resolving
    each one as soon as its body finishes, is what lets the single render
    pass reach every nested block with a real, already-decided column
    instead of a best-effort guess made before the enclosing breaks were
    decided (the gap `pprint-newline.miser.8`/`.9` exposed in an earlier,
    eager-resolve version of this).
    """

    __slots__ = ('tokens',)

    def __init__(self):
        self.tokens = []

    def write(self, text):
        if text:
            self.tokens.append(('text', text))


def _pprint_block_text(value, argument_name):
    """Coerce a `:prefix`/`:per-line-prefix`/`:suffix` argument to text.

    CLHS requires a string designator here in the general-array sense (CLHS
    14.1's `string` includes any character vector, fill-pointered or
    displaced, not only `LispString`) -- `pprint-logical-block.7`'s
    zero-length `(array nil (0))` and `.8`'s fill-pointered/adjustable
    character arrays as `:prefix`/`:suffix` both rely on this, so a bare
    `isinstance(x, LispString)` is too narrow. Reuses `sequence_protocol`'s
    one element-accessor rather than a second array-walking copy.
    """
    if isinstance(value, lisptype.LispString):
        # Not `str(value)`: `LispString.__str__` returns the whole backing
        # buffer, ignoring `fill_pointer` -- `.8`'s 10-character array with a
        # fill pointer of 3 as `:prefix` needs just "abc". `__iter__` is the
        # one place on this class that already stops at the fill pointer.
        return ''.join(value)
    if isinstance(value, str):
        return value
    if isinstance(value, (list, tuple)) and all(isinstance(c, lisptype.Character) for c in value):
        # A zero-length `(array nil (0))` reaches here as a plain empty
        # Python list (`arrays.py`'s general-vector representation), which is
        # a string of length 0 per CLHS -- `.7`'s empty-array prefix/suffix.
        # Gated on every element being a CHARACTER: a general vector holding
        # anything else (e.g. `#(nil nil)`) is not a string and must still
        # fall through to the TYPE-ERROR below.
        return ''.join(c.char for c in value)
    from . import arrays as _arrays
    if isinstance(value, _arrays.LispArray) and _arrays.array_rank_of(value) == 1:
        et = _arrays.element_type_of(value)
        et_name = et.name if isinstance(et, lisptype.LispSymbol) else str(et)
        if et_name.upper() in ('CHARACTER', 'BASE-CHAR', 'STANDARD-CHAR', 'NIL'):
            from .sequence_protocol import seq_elements
            chars = seq_elements(value, argument_name)
            return ''.join(c.char if isinstance(c, lisptype.Character) else str(c)
                           for c in chars)
    raise lisptype.LispTypeError(
        f"PPRINT-LOGICAL-BLOCK: {argument_name} must be a string, not "
        f"{_write_object(value, escape=True)}",
        expected_type='STRING', actual_value=value)


def _pprint_unpretty(object, stream):
    """Write `object` to `stream` through the printer, without line breaking.

    **The `PPRINT-*` operators were stubs that called Python's `print()`**, and
    that is two separate defects rather than an honest gap:

    * It writes to Python's stdout instead of the value of `*STANDARD-OUTPUT*`
      or the stream argument. Every `printer/` test captures output as
      `(with-output-to-string (s) (pprint-fill s obj))`, so all of them saw the
      empty string no matter what -- the same *measurement* gate that hid the
      whole printer before 2026-08-14 (plan.md C7), still in place here.
    * `print(obj)` renders through `lispCons.__str__`, i.e. the pre-printer
      representation, which knows nothing about the printer's control variables
      **or its circularity guards**. `pprint-fill.13` prints a circular list, so
      `PPRINT-FILL` recursed until the process held 11GB -- a stub aborting the
      whole ANSI run (standing rule 4: a loud gap is measurable, a silent wrong
      answer is not, and this one was neither).

    **This does not implement CLHS 22.2.2.** There is no pretty printer: no
    line breaking, no `*PRINT-RIGHT-MARGIN*`, no logical blocks, and the
    `prefix`/`suffix`/`colon-p` arguments that decide a block's delimiters are
    ignored, so a list arrives with the ordinary `(`...`)` the printer gives it.
    Building any of that here would be a second printer, and the pretty printer
    is its own milestone (plan.md C2/M10, recorded in section 5). What this
    *does* fix is the two things that made the stubs actively harmful: output
    goes to the stream the caller named, and a circular argument terminates.
    """
    write_text(_printer.prin1_to_string(object), stream)
    return lisptype.NIL


@_registry.cl_function('PPRINT')
def pprint(object, stream=None):
    """A newline, then the object pretty-printed (CLHS 22.3.1).

    PPRINT is a `TERPRI` followed by a WRITE with `*PRINT-ESCAPE*` and
    `*PRINT-PRETTY*` forced true for the call, and it returns **no values**
    -- `pprint.1`'s `(assert (null (multiple-value-list (pprint obj))))`
    holds that against a NIL return as firmly as it holds the output
    equality.
    """
    write_text('\n', stream)
    write_text(_dispatch_print(object, {'escape': True, 'pretty': True}, stream), stream)
    return lisptype.MultipleValues()


def _pprint_dispatch_default(stream, object):
    """The fallback dispatched function when no entry matches.

    Dispatch entries follow the CLHS 22.2.1.3 convention `(stream, object)`,
    but `print-object` itself takes `(object, stream)`. This wrapper is the
    shape `pprint-dispatch` actually returns, so the printer can call it
    without knowing the argument order `print-object` happens to use.

    Registered under `_PPRINT-DISPATCH-DEFAULT` so `(typep fn 'function)` is T
    (the `pprint-dispatch.1` check) without polluting the public name space.
    The leading underscore is the same convention `_s_print_` and the other
    `interned` symbols in this module use to mark implementation helpers.
    """
    from .misc_macros import print_object
    print_object(object, stream)
    return lisptype.NIL


@_registry.cl_function('_PPRINT-DISPATCH-DEFAULT')
def _pprint_dispatch_default_fn(stream, object):
    return _pprint_dispatch_default(stream, object)


_dispatch_default_fn = _pprint_dispatch_default_fn


@_registry.cl_function('PPRINT-DISPATCH')
def pprint_dispatch(object, table=None):
    """PPRINT-DISPATCH (CLHS 22.2.1.3): the pretty printer's dispatch function.

    Walks `table`'s entries; for each `(type-spec, function, priority)` triple,
    asks `typespec.type_contains` whether `object` matches the type specifier.
    CLHS says "an arbitrary choice is made" among entries with the same
    priority, so the *first* highest-priority match wins (the test in
    `pprint-dispatch.7`/`pprint-dispatch.8` checks this: setting `(EQL X)` at
    priority 0 then `(MEMBER X Y)` at +/- 0.0001 makes the latter win or lose
    by exactly the priority comparison).

    When no entry matches, returns `(print-object, NIL)`. `print-object` is
    the one home of the default representation (CLHS 22.1.3.4 / 22.1.3.13);
    it is wrapped here to match the dispatch convention `(stream, object)`
    rather than `print-object`'s `(object, stream)`. The wrapper is a
    registered cl_function (so `(typep fn 'function)` is T, the
    `pprint-dispatch.1` check), and a side-effecting stream-write that calls
    the original `print-object` with the arguments flipped.

    `table` omitted or NIL means the value of `*print-pprint-dispatch*`,
    matching every other dispatcher-shaped operator in this module.
    """
    if table is None or table is lisptype.NIL:
        from .binding import dynamic_value
        table = dynamic_value(
            lisptype.COMMON_LISP_PACKAGE.intern_symbol('*PRINT-PPRINT-DISPATCH*'),
            default=standard_pprint_dispatch())

    if not isinstance(table, PprintDispatchTable):
        raise lisptype.LispTypeError(
            f"PPRINT-DISPATCH: not a pprint dispatch table: {table!r}",
            expected_type='PPRINT-DISPATCH-TABLE', actual_value=table)

    best_fn = None
    best_pri = -1  # any non-negative priority beats this initial sentinel
    for type_specifier, function, priority in table.entries:
        try:
            if not typespec.type_contains(object, type_specifier):
                continue
        except Exception:
            # An unparseable specifier is a programmer error in the
            # dispatch table, not a reason to abort PPRINT-DISPATCH; the
            # standard says the entry simply does not match, so skip it.
            continue
        if best_fn is None or priority > best_pri:
            best_fn = function
            best_pri = priority
    if best_fn is None:
        return lisptype.MultipleValues(_dispatch_default_fn, lisptype.NIL)
    return lisptype.MultipleValues(best_fn, lisptype.T)


class PPrintFrame:
    """One dynamic extent of `PPRINT-LOGICAL-BLOCK` (CLHS 22.2.2).

    `remaining` is the tail still to be consumed by `PPRINT-POP`; `count` is
    the number of `PPRINT-POP` calls already made against this frame, checked
    against `*PRINT-LENGTH*`; `started_as_nil` distinguishes an `object` that
    was `NIL` from the very start from one that *became* `NIL` by being fully
    popped -- only the latter is the "natural end" that `*PRINT-LENGTH*` must
    not also elide as `...` (`pprint-pop.5`'s length-5 case on a 5-element
    list vs. `pprint-pop.1`'s length-0 case on `NIL` itself: the same
    "remaining is NIL" state means something different in each).
    """

    __slots__ = ('remaining', 'stream', 'count', 'started_as_nil',
                 'outer_target', 'body_col', 'per_line_text',
                 'skip_output', 'circle_owner', 'circle_visits')

    def __init__(self, remaining, stream, outer_target=None, body_col=0, per_line_text=None,
                 skip_output=False, circle_owner=False, circle_visits=None):
        self.remaining = remaining
        self.stream = stream
        self.count = 0
        self.started_as_nil = _null_internal(remaining)
        # The real stream (or enclosing frame's own `_PPBuffer`) this frame's
        # resolved text is written to once it closes, the column it starts
        # at, and its `:per-line-prefix` text (`None` if it has none) --
        # `flush_pprint_frame`'s inputs. `stream` above is this frame's OWN
        # `_PPBuffer`, bound to the Lisp stream-symbol for the body's extent.
        self.outer_target = outer_target
        self.body_col = body_col
        self.per_line_text = per_line_text
        # `*PRINT-CIRCLE*` support (CLHS 22.2.2): `skip_output` marks a block
        # whose object was already printed (a shared reference) -- only "#n#"
        # was emitted at setup, so the body runs against a throwaway buffer
        # and `flush_pprint_frame` discards everything. `circle_owner` marks
        # the frame that *created* the ambient circle state, so the state is
        # popped exactly once. `circle_visits` is the recording pre-pass
        # (SBCL's first, output-discarded pass): how many times the walk
        # reached each cons position -- a cons reached twice is where
        # PPRINT-POP's dotted back-reference fires (pprint-pop.7/.8).
        self.skip_output = skip_output
        self.circle_owner = circle_owner
        self.circle_visits = circle_visits


def _current_pprint_frame(operator_name):
    """The innermost open `PPRINT-LOGICAL-BLOCK` frame, or a PROGRAM-ERROR.

    `PPRINT-POP`/`PPRINT-EXIT-IF-LIST-EXHAUSTED` are only meaningful inside
    one (CLHS 22.2.2); `pprint-pop.error.1` and
    `pprint-exit-if-list-exhausted.error.1` call them at top level and require
    an error rather than a Python `IndexError` leaking as the result.
    """
    import fclpy.state as state
    stack = getattr(state, 'pprint_stack', None)
    if not stack:
        raise lisptype.LispProgramError(
            f"{operator_name}: not inside a PPRINT-LOGICAL-BLOCK")
    return stack[-1]


# === *PRINT-CIRCLE* support for the pprint operators (CLHS 22.2.2) ===
#
# pprint-logical-block (and therefore PPRINT-FILL/LINEAR/TABULAR, which are
# defined in terms of it) performs its own circularity detection: the block's
# object gets an "#n=" before it the first time it is printed, an "#n#" (and
# nothing else -- no prefix, no suffix, no body) when it is printed again.
# CLHS 22.2.2 is explicit that the arguments of WRITE and friends are checked
# too, so an element popped by PPRINT-POP and written with WRITE participates
# in the same labelling.
#
# The state is one labelling table shared by a whole top-level pprint
# operation, established when the outermost block is entered and popped when
# it flushes. Nested blocks (and every WRITE under them) consult the innermost
# entry. A depth is recorded with each entry so an abnormal exit -- which
# skips `flush_pprint_frame` -- cannot leave a stale table poisoning every
# later print: the next entry point trims entries whose frame has already
# unwound.

_PP_CIRCLE_STATES = []


def _pp_circle_active():
    """The innermost ambient circle-labelling state, or `None`."""
    return _PP_CIRCLE_STATES[-1] if _PP_CIRCLE_STATES else None


def _pp_is_circle_aggregate(value):
    """Whether `value` is an object the circle labelling can mark.

    The same aggregate set `printer._aggregate_pieces` walks, asked through
    `printer._is_aggregate` so the pprint side and the printer side cannot
    disagree about what counts (plan.md standing rule 3).
    """
    from fclpy.printer import _is_aggregate
    return _is_aggregate(value)


def _pp_circle_map_compute(root):
    """Label every shared or cyclic aggregate reachable from `root`.

    The standard algorithm with a visited set: the first visit descends,
    a second reach assigns a label and does *not* descend again. Not
    descending is what keeps a shared cons's *contents* from being
    labelled too -- `pprint-fill.14` prints `(X X)` (two references to
    one `(A)`) as `(#1=(A) #1#)`, where `A` must stay unlabelled, while
    a whole-object `write` pass that re-walks both references would hand
    out a second label to `A`.
    """
    from fclpy.printer import _aggregate_pieces
    label_map = {}
    seen = set()
    counter = [1]

    def visit(obj):
        if obj is None or obj is lisptype.NIL:
            return
        if not _pp_is_circle_aggregate(obj):
            return
        key = id(obj)
        if key in seen:
            if key not in label_map:
                label_map[key] = counter[0]
                counter[0] += 1
            return
        seen.add(key)
        pieces = _aggregate_pieces(obj)
        if pieces:
            for sub in pieces:
                visit(sub)

    visit(root)
    return label_map


def _pp_circle_enter(object):
    """Establish (or re-enter) the ambient circle state for a pprint block.

    Returns `(state, created)`. `state` is `None` when `*PRINT-CIRCLE*` is
    false or the object is not an aggregate -- nothing to track. When a
    state is already active (a nested block, or an element print under one)
    it is *reused*: one top-level print owns one labelling table, so label
    numbers stay consistent across the whole operation.
    """
    import fclpy.state as state
    depth = len(getattr(state, 'pprint_stack', []) or [])
    # Trim entries whose frame already unwound without flushing (a non-local
    # exit that skipped `flush_pprint_frame`). An entry pushed when the stack
    # was `d` deep is live exactly while the stack is deeper than `d`.
    while _PP_CIRCLE_STATES and _PP_CIRCLE_STATES[-1]['depth'] >= depth:
        _PP_CIRCLE_STATES.pop()
    if not _printer._true(_printer.resolve_control('*PRINT-CIRCLE*')):
        return None, False
    if not _pp_is_circle_aggregate(object):
        return None, False
    existing = _PP_CIRCLE_STATES[-1] if _PP_CIRCLE_STATES else None
    if existing is not None:
        return existing, False
    if _consp_internal(object):
        labels, visits = _pp_circle_visit_prepass(object)
    else:
        labels, visits = _pp_circle_map_compute(object), None
    entry = {'map': labels, 'seen': set(), 'depth': depth, 'visits': visits}
    _PP_CIRCLE_STATES.append(entry)
    return entry, True


def _pp_circle_label(state, object):
    """The "#n="/"#n#" a pprint block owes `object`, per `state`.

    Returns `('#n=', False)` to print before the block's own prefix,
    `('#n#', True)` when the object was already printed and the whole
    block must collapse to just the back-reference, or `('', False)` for
    an unlabelled object.
    """
    if state is None:
        return '', False
    label = state['map'].get(id(object))
    if label is None:
        return '', False
    if label in state['seen']:
        return f'#{label}#', True
    state['seen'].add(label)
    return f'#{label}=', False


def _pp_circle_visit_prepass(object):
    """The recording pre-pass behind the pprint operators' circle labels.

    This mirrors the walk a `PPRINT-LOGICAL-BLOCK` body performs over its
    list -- the canonical `(pprint-exit-if-list-exhausted) (write
    (pprint-pop) ...)` loop -- *including where that walk stops*, and
    records two things:

    * `labels` -- every aggregate encountered a second time, numbered in
      encounter order. An element printed twice (`pprint-fill.14`'s
      `(X X)`) and a tail reached twice (`pprint-pop.7`) both get labels;
      an object cut off by `*PRINT-LENGTH*` before its second encounter
      gets none, which is why `pprint-pop.7` under `*PRINT-LENGTH*` 1
      prints `<(1) ...>` with no `#1=` at all.
    * `visits` -- how many times each cons was reached as a *position* of
      the list. A cons reached twice is where the real walk terminates
      with a ". #n#" / ". #n=(...)" back-reference (`pprint-pop.7`/`.8`);
      `pprint_pop` consults this to decide the unprinted-cycle case.

    The stop rules are the observable ones: NIL ends the walk, a dotted
    tail ends it, `*PRINT-LENGTH*` ends it, and a repeated *position*
    ends it (the dot). A repeated *element* only labels; the walk goes on.
    The pass is bounded -- each cons is recorded at most twice -- so a
    circular list cannot spin it forever.
    """
    length = _pprint_length()
    labels = {}
    visits = {}
    counter = [1]

    def encounter(obj):
        """Record one encounter; True when this is a repeat."""
        if not _pp_is_circle_aggregate(obj):
            return False
        key = id(obj)
        if key in labels:
            return True
        visits[key] = visits.get(key, 0) + 1
        if visits[key] >= 2:
            labels[key] = counter[0]
            counter[0] += 1
            return True
        return False

    def element_encounter(obj):
        """Record an element encounter: labels a repeat, never stops."""
        if not _pp_is_circle_aggregate(obj):
            return
        key = id(obj)
        if key in labels or key in visits:
            if key not in labels:
                labels[key] = counter[0]
                counter[0] += 1
        else:
            visits[key] = 1

    position = object
    count = 0
    encounter(position)
    while True:
        if _null_internal(position):
            return labels, visits
        if not _consp_internal(position):
            return labels, visits
        if length is not None and count >= length:
            return labels, visits
        if count > 0 and encounter(position):
            return labels, visits
        element_encounter(position.car)
        count += 1
        position = position.cdr


def pprint_logical_block_setup(stream_designator, object, prefix, per_line_prefix, suffix,
                                 prefix_given=False, per_line_prefix_given=False, suffix_given=False):
    """Resolve a `PPRINT-LOGICAL-BLOCK` call before its body runs (CLHS 22.2.2).

    The special form (`evaluation_special_forms.eval_pprint_logical_block`)
    handles the unevaluated stream-symbol binding and the body's `BLOCK NIL`;
    everything else -- the "not a list" bypass, `*PRINT-LEVEL*` truncation,
    prefix/per-line-prefix output and the pretty-stream wrapper -- lives here
    so it is not duplicated between that call site and any future one.

    Returns a `(kind, stream, frame, suffix_text)` tuple:

    * `kind='atom'` -- `object` is not a list (CLHS: printed as if by `WRITE`,
      with prefix/suffix/body all omitted -- `pprint-logical-block.16`).
    * `kind='level-exceeded'` -- nesting has reached `*PRINT-LEVEL*`; print
      `#` and skip the body (`pprint-logical-block.9`/`.10`, which apply even
      when `*PRINT-PRETTY*` is NIL: `.13`).
    * `kind='run'` -- push `frame` onto `state.pprint_stack`, evaluate the
      body against `frame.stream` (a fresh `_PPBuffer`), then
      `flush_pprint_frame` and pop the frame.

    The prefix (or per-line-prefix) is still written immediately, to
    `outer_target`, exactly as before -- an abnormal exit from the body (a
    non-local `RETURN-FROM`/`GO`/`THROW`, or a Python exception) must still
    have printed it, matching `PPRINT-LOGICAL-BLOCK`'s CLHS-specified
    behavior of printing the prefix *before* the body runs. Only the body's
    own output -- and thus every `PPRINT-NEWLINE`/`PPRINT-INDENT` decision --
    is deferred to `flush_pprint_frame`, since only that needs the margin.
    """
    import fclpy.state as state

    outer_target = resolve_output_stream(stream_designator)

    if not _listp_internal(object):
        return ('atom', outer_target, None, None)

    depth = len(getattr(state, 'pprint_stack', []) or [])
    level = _printer._as_count(_printer.resolve_control('*PRINT-LEVEL*'))
    if level is not None and depth >= level:
        return ('level-exceeded', outer_target, None, None)

    # CLHS 22.2.2: with *PRINT-CIRCLE* true and `object` already printed, an
    # "#n#" is printed and the prefix, suffix and body are all skipped. The
    # label (when the object is printed for the first time) goes out *before*
    # the prefix. The same enter also establishes the ambient labelling table
    # for this top-level print, which WRITE and PPRINT-POP consult.
    circle_state, circle_created = _pp_circle_enter(object)
    label_text, skip = _pp_circle_label(circle_state, object)

    # Gated on whether the keyword was syntactically *given*, not on whether
    # its value is NIL: `:prefix nil` is a supplied non-string value and must
    # fail `_pprint_block_text`'s check (`pprint-logical-block.error.1`),
    # whereas an omitted `:prefix` defaults to "" with no validation at all.
    prefix_text = _pprint_block_text(prefix, ':PREFIX') if prefix_given else ''
    per_line_text = (_pprint_block_text(per_line_prefix, ':PER-LINE-PREFIX')
                      if per_line_prefix_given else None)
    suffix_text = _pprint_block_text(suffix, ':SUFFIX') if suffix_given else ''

    write_text(label_text, outer_target)
    if skip:
        # Only the back-reference: the body still runs (CLHS models the
        # detection as a first, output-suppressed pass), but against a
        # throwaway buffer that `flush_pprint_frame` discards.
        body_buffer = _PPBuffer()
        frame = PPrintFrame(object, body_buffer, outer_target=outer_target,
                            skip_output=True, circle_owner=circle_created,
                            circle_visits=circle_state['visits'])
        return ('run', body_buffer, frame, '')

    # PPRINT-POP's dotted back-reference consults the recording pre-pass,
    # which the circle enter already ran (and only under *PRINT-CIRCLE*:
    # its ". x" for a plain dotted tail is older behavior and needs no
    # table).
    circle_visits = circle_state['visits'] if circle_state is not None else None

    write_text(per_line_text if per_line_text is not None else prefix_text, outer_target)
    # Only the outermost frame's column is needed now -- a nested frame's
    # own start column is not decidable until the enclosing block's earlier
    # breaks are (`flush_pprint_frame`'s `'block'`-token deferral), so it is
    # resolved later, from the *real* running column reached in that single
    # left-to-right render pass, not guessed here.
    body_col = 0 if isinstance(outer_target, _PPBuffer) else _pp_outer_column(outer_target)

    body_buffer = _PPBuffer()
    frame = PPrintFrame(object, body_buffer, outer_target=outer_target,
                        body_col=body_col, per_line_text=per_line_text,
                        circle_owner=circle_created, circle_visits=circle_visits)
    return ('run', body_buffer, frame, suffix_text)


def _pp_outer_column(target):
    """The 0-based column `target` is at right now, for a logical block's own
    `start_column` (CLHS 22.2.1) -- exact for the stream types ansi-test
    actually uses, best-effort (`write_text`'s own running tally, 0 if never
    written to) otherwise.

    A `StringOutputStream`/`FillPointerOutputStream`'s buffer is authoritative
    (`peek_string`, the same source `_at_line_start` already trusts over the
    write-time bookkeeping, since text can reach it by paths other than
    `write_text`).
    """
    from .streams import StringOutputStream, FillPointerOutputStream
    if isinstance(target, (StringOutputStream, FillPointerOutputStream)):
        text = target.peek_string()
        nl = text.rfind('\n')
        return len(text) - nl - 1 if nl != -1 else len(text)
    return getattr(target, _COLUMN_ATTR, 0)


def flush_pprint_frame(frame, suffix_text):
    """Resolve one `PPRINT-LOGICAL-BLOCK` frame's buffered body against the
    margin (CLHS 22.2.1) -- or, if nested, defer it -- and write the suffix.

    Called once, when the frame's body finishes (normally, or via the
    `RETURN-FROM NIL` `PPRINT-EXIT-IF-LIST-EXHAUSTED` raises) -- never
    incrementally, since a conditional newline's firing can depend on text
    the body has not written yet.

    If `frame.outer_target` is itself another frame's `_PPBuffer` (this
    block is nested), resolving now would need a start column that is not
    yet known -- the enclosing block's own breaks, still unresolved, may or
    may not put this block at column 0. So nothing is rendered here; a
    `'block'` token carrying this frame's own tokens (plus its suffix and
    per-line-prefix) is appended to the *enclosing* buffer instead, and
    resolved together with everything else once an actual stream is
    reached, by `_pp_render_block` recursing into it with the real column
    that point in the single left-to-right pass has reached.
    """
    try:
        if frame.skip_output:
            # CLHS 22.2.2's "#n#" case: the back-reference was already
            # written at setup; the body's output (captured in a throwaway
            # buffer) and the suffix are discarded wholesale.
            return
        if isinstance(frame.outer_target, _PPBuffer):
            frame.outer_target.tokens.append(
                ('block', suffix_text, frame.per_line_text, frame.stream.tokens))
            return
        rendered = _pp_render_top(frame.stream.tokens, frame.body_col,
                                   frame.per_line_text, len(suffix_text))
        write_text(rendered + suffix_text, frame.outer_target)
    finally:
        if frame.circle_owner:
            _PP_CIRCLE_STATES.pop()


def _pprint_exit_nil():
    """Raise the implicit `(RETURN-FROM NIL)` CLHS 22.2.2 gives both macros.

    Reusing `ReturnFromException` rather than a private exception type means
    `PPRINT-LOGICAL-BLOCK`'s own `BLOCK NIL` catches this exactly like any
    other `RETURN-FROM`, with no second non-local-exit mechanism to keep in
    sync with `evaluation_core`'s pass-through tuples.
    """
    from .evaluation_core import ReturnFromException
    raise ReturnFromException(lisptype.NIL, lisptype.NIL)


def _pprint_length():
    return _printer._as_count(_printer.resolve_control('*PRINT-LENGTH*'))


@_registry.cl_function('%PPRINT-EXIT-IF-LIST-EXHAUSTED')
def pprint_exit_if_list_exhausted():
    """Exit the enclosing `PPRINT-LOGICAL-BLOCK` if its list is used up (CLHS 22.2.2).

    Registered under a `%`-prefixed name because CLHS 22.2.2 specifies
    `PPRINT-EXIT-IF-LIST-EXHAUSTED` itself as a *macro*, not a function;
    `standard_macros.py` registers that macro and expands it to a call of
    this runtime. The two are not interchangeable even at zero arity --
    only a macro answers `MACRO-FUNCTION`, and `#'pprint-pop` must not
    name a function.

    Three outcomes, checked in this order against the innermost frame's
    remaining tail:

    1. `NIL` (a proper list genuinely exhausted, or an `object` that was `NIL`
       to begin with) -- exit with no output. `pprint-exit-if-list-exhausted.1`
       pins this ahead of the `*PRINT-LENGTH*` check: it is what makes
       `pprint-pop.5`'s length-5 case print no `...` for a 5-element list.
    2. A cons whose element count already reached `*PRINT-LENGTH*` -- print
       `...` and exit. Gated on `is_cons` because a dotted tail is not a
       "next element" for `*PRINT-LENGTH*` to truncate (`pprint-pop.6`'s
       length-2 case reaches the dot, not `...`, even though the count matches).
    3. Otherwise (a cons under the length limit, or a dotted atom tail) --
       return `NIL` and let the body continue; a dotted atom's own `. `
       rendering is `PPRINT-POP`'s job (`pprint-exit-if-list-exhausted.3`'s
       own assertion requires *this* call to return normally at that point).

    The usual calling convention is `(pprint-exit-if-list-exhausted) (write
    #\\Space) (write (pprint-pop))` inside a `LOOP`, so when case 2 fires
    after at least one element has already been printed, the space that
    would have separated it from the next element never gets written -- the
    loop exits before reaching it. `...` stands in for the whole "separator +
    element" pair that was elided, not just the element, so it needs that
    leading space itself (`pprint-pop.5`/`.6`'s length-1 cases: `"{1 ...}"`,
    not `"{1...}"`). At `count == 0` there is nothing to separate from.
    """
    frame = _current_pprint_frame('PPRINT-EXIT-IF-LIST-EXHAUSTED')
    remaining = frame.remaining
    if _null_internal(remaining):
        _pprint_exit_nil()
    is_cons = _consp_internal(remaining)
    length = _pprint_length()
    if is_cons and length is not None and frame.count >= length:
        write_text(' ...' if frame.count > 0 else '...', frame.stream)
        _pprint_exit_nil()
    return lisptype.NIL


@_registry.cl_function('%PPRINT-POP')
def pprint_pop():
    """Pop the next element from the enclosing `PPRINT-LOGICAL-BLOCK`'s list (CLHS 22.2.2).

    `%`-prefixed for the same reason as
    `%PPRINT-EXIT-IF-LIST-EXHAUSTED` above: the CLHS operator is a macro,
    and this is the runtime its expansion calls.

    Unlike `PPRINT-EXIT-IF-LIST-EXHAUSTED`, the `*PRINT-LENGTH*` check here
    fires *before* the "remaining is NIL" check, but only when either the
    remaining tail is still a real cons (there is a genuine next element being
    elided) or the frame's `object` was `NIL` from the start (`started_as_nil`)
    -- `pprint-pop.1`/`pprint-pop.9`'s zero/one-length cases on a `NIL` object
    require `...` even though `remaining` has been `NIL` all along, which is
    exactly the case `PPRINT-EXIT-IF-LIST-EXHAUSTED`'s ordering must *not*
    elide for a list that was genuinely consumed down to its natural end.
    """
    frame = _current_pprint_frame('PPRINT-POP')
    remaining = frame.remaining
    is_cons = _consp_internal(remaining)
    is_nil = _null_internal(remaining)
    length = _pprint_length()
    if length is not None and frame.count >= length and (is_cons or (is_nil and frame.started_as_nil)):
        write_text('...', frame.stream)
        _pprint_exit_nil()
    if is_cons and frame.count > 0:
        # CLHS 22.2.2's circularity/sharing case. Two ways the termination
        # fires: the tail was *already printed* within this operation (a
        # shared reference -- pprint-pop.7's tail is element 1 -- so only
        # the back-reference may follow), or the recording pre-pass says
        # this position gets revisited before the walk ends (pprint-pop.8's
        # cycle: "[[1 2 ...]]" under *PRINT-LENGTH* 2 but "[[1 . #1=(2 . #1#)]]"
        # under 3 -- the difference is exactly whether the pre-pass, which
        # stops where the real walk stops, got that far). An already-printed
        # tail always dots; an unprinted one only when the cycle re-enters.
        circle = _pp_circle_active()
        label = circle['map'].get(id(remaining)) if circle is not None else None
        already_seen = label is not None and label in circle['seen']
        visits = frame.circle_visits
        if already_seen or (visits is not None and visits.get(id(remaining), 0) >= 2):
            if already_seen:
                write_text(f'. #{label}#', frame.stream)
            else:
                if label is not None:
                    circle['seen'].add(label)
                write_text('. ' + _write_object(remaining), frame.stream)
            _pprint_exit_nil()
    frame.count += 1
    if is_nil:
        return lisptype.NIL
    if not is_cons:
        # A dotted tail: PPRINT-EXIT-IF-LIST-EXHAUSTED let it through as "not
        # yet exhausted", so rendering the terminator is this call's job.
        write_text('. ' + _write_object(remaining), frame.stream)
        _pprint_exit_nil()
    value = remaining.car
    frame.remaining = remaining.cdr
    return value


def _current_pprint_frame_or_none():
    """Like `_current_pprint_frame`, but `None` rather than a PROGRAM-ERROR
    when there is no enclosing `PPRINT-LOGICAL-BLOCK` -- `PPRINT-INDENT` and
    `PPRINT-NEWLINE`, unlike `PPRINT-POP`/`PPRINT-EXIT-IF-LIST-EXHAUSTED`, are
    meaningful only *inside* one but are not specified to error outside one.
    """
    import fclpy.state as state
    stack = getattr(state, 'pprint_stack', None)
    return stack[-1] if stack else None


@_registry.cl_function('PPRINT-INDENT')
def pprint_indent(relative_to, n, stream=None):
    """Set indentation for the innermost open `PPRINT-LOGICAL-BLOCK` (CLHS 22.2.2).

    `relative_to` must be `:BLOCK` or `:CURRENT` regardless of
    `*PRINT-PRETTY*` or nesting -- `pprint-indent.error.4`/`-unsafe` require
    an ERROR for every other value in `*mini-universe*`. Past that check,
    this has no effect if `*PRINT-PRETTY*` is false *at the moment of this
    call* -- checked here, not when the enclosing block resolves, because a
    `LET` can rebind `*PRINT-PRETTY*` around just this one call while the
    block's own dynamic extent stays pretty (`pprint-indent.17`/`.18`) -- or
    if there is no enclosing block. Otherwise a sentinel recording
    `relative_to`/`n` is appended to that block's buffer; the actual column
    is decided once the whole block's fit against the margin is known
    (`flush_pprint_frame`), the same engine FORMAT's `~<...~:>` uses for
    `~I`.
    """
    name = relative_to.name.upper() if isinstance(relative_to, lisptype.LispSymbol) else None
    if name not in ('BLOCK', 'CURRENT'):
        raise lisptype.LispTypeError(
            f"PPRINT-INDENT: relative-to must be :BLOCK or :CURRENT, not "
            f"{_write_object(relative_to, escape=True)}",
            expected_type='(MEMBER :BLOCK :CURRENT)', actual_value=relative_to)
    if not _printer._true(_printer.resolve_control('*PRINT-PRETTY*')):
        return lisptype.NIL
    frame = _current_pprint_frame_or_none()
    if frame is None:
        return lisptype.NIL
    try:
        offset = int(round(n))
    except TypeError:
        offset = 0
    frame.stream.tokens.append(('indent', 'block' if name == 'BLOCK' else 'current', offset))
    return lisptype.NIL


def _pprint_list_unpretty(object, colon_p, target):
    """*PRINT-PRETTY* false (CLHS 22.2.2's fallback for the list styles):
    print with a minimum of whitespace per CLHS 22.1.3.5, parenthesized iff
    `colon_p`. With the parens this is an ordinary `write`; without them the
    elements are written space-separated, still honouring `*PRINT-LENGTH*`
    and guarded against a circular tail.
    """
    if colon_p:
        write_text(_write_object(object), target)
        return
    pieces = []
    tail = object
    limit = _pprint_length()
    seen = set()
    truncated = False
    while _consp_internal(tail):
        if limit is not None and len(pieces) >= limit:
            truncated = True
            break
        if id(tail) in seen:
            truncated = True
            break
        seen.add(id(tail))
        pieces.append(_write_object(tail.car))
        tail = tail.cdr
    if not truncated and not _null_internal(tail):
        pieces.append('. ' + _write_object(tail))
    write_text(' '.join(pieces), target)


def _pprint_list_style(stream_designator, object, colon_p, break_kind, tabsize=None):
    """The shared body of PPRINT-FILL, PPRINT-LINEAR and PPRINT-TABULAR.

    CLHS defines the three as `PPRINT-LOGICAL-BLOCK` loops that differ only
    in what goes between the elements (its `pprint-tabular` note gives the
    tabular loop verbatim; fill and linear are the same loop with a `:fill`
    / `:linear` `PPRINT-NEWLINE` in place of the tab). All three parenthesize
    iff `colon_p`, ignore their at-p argument, print a non-list as by
    `write`, and fall back to minimum-whitespace printing when
    `*PRINT-PRETTY*` is false. This is one driver rather than three loop
    bodies for the same reason the CLHS gives one `pprint-logical-block`
    shape: the abbreviation, circularity and buffer plumbing are the
    block's, not the loop's.
    """
    if not _listp_internal(object):
        # CLHS: "uses write to print object when it is a non-list".
        write(object, stream=stream_designator)
        return lisptype.NIL
    colon_p = _colon_default(colon_p)
    if not _printer._true(_printer.resolve_control('*PRINT-PRETTY*')):
        _pprint_list_unpretty(object, colon_p, resolve_output_stream(stream_designator))
        return lisptype.NIL

    kind, buffer, frame, suffix_text = pprint_logical_block_setup(
        stream_designator, object,
        '(' if colon_p else '', None,
        ')' if colon_p else '',
        prefix_given=True, per_line_prefix_given=False, suffix_given=True)
    if kind == 'level-exceeded':
        write_text('#', resolve_output_stream(stream_designator))
        return lisptype.NIL

    import fclpy.state as state
    from .evaluation_core import ReturnFromException
    state.pprint_stack.append(frame)
    try:
        try:
            pprint_exit_if_list_exhausted()
            while True:
                element = pprint_pop()
                write(element, stream=frame.stream)
                pprint_exit_if_list_exhausted()
                write_text(' ', frame.stream)
                if tabsize is not None:
                    frame.stream.tokens.append(
                        ('tab', 'section-relative', 0, tabsize))
                frame.stream.tokens.append(('break', break_kind))
        except ReturnFromException as exc:
            if not _null_internal(exc.tag):
                raise
    finally:
        state.pprint_stack.pop()
    flush_pprint_frame(frame, suffix_text)
    return lisptype.NIL


def _truthy(value):
    """A Lisp generalized boolean as Python truth, NIL-aware."""
    if value is None or value is lisptype.NIL:
        return False
    return _printer._true(value)


def _colon_default(colon_p):
    """`colon-p` with its CLHS default applied: true when omitted.

    The three spellings must be told apart here: a Python `None` is the
    *omitted* argument (the parameter's own default), an explicit Lisp NIL
    arrives as the `lisptype.NIL` singleton, and the `~/.../` dispatch hands
    Python `True`/`False` for the flags it read. Only the first takes the
    CLHS default -- `pprint-fill.5`'s explicit NIL must suppress the
    parentheses that the same call with the argument omitted would print.
    """
    if colon_p is None:
        return True
    return _truthy(colon_p)


@_registry.cl_function('PPRINT-LINEAR')
def pprint_linear(stream, object, colon_p=None, at_p=None):
    """The list on one line, or one element per line (CLHS 22.2.2).

    `colon-p` defaults to true and decides the parentheses; `at-p` is
    accepted and ignored (CLHS: "Each function ignores its at-sign-p
    argument", kept so the functions work via `~/.../` and
    `set-pprint-dispatch`).
    """
    return _pprint_list_style(stream, object, colon_p, 'linear')


_PP_NEWLINE_KINDS = {'LINEAR': 'linear', 'FILL': 'fill', 'MISER': 'miser', 'MANDATORY': 'mandatory'}


@_registry.cl_function('PPRINT-NEWLINE')
def pprint_newline(kind, stream=None):
    """A conditional (or, for `:MANDATORY`, unconditional) newline inside the
    innermost open `PPRINT-LOGICAL-BLOCK` (CLHS 22.2.2).

    It used to emit an *unconditional* newline to Python's stdout regardless
    of `kind`: wrong stream, and wrong even on the right one, since three of
    the four kinds are conditional on whether the enclosing block fits.
    `kind` must be one of the four CLHS keywords regardless of context
    (`pprint-newline.error.1`/`-unsafe`); past that, a no-op if
    `*PRINT-PRETTY*` is currently false, or if there is no enclosing block.
    Otherwise records a break sentinel in that block's buffer -- whether it
    actually breaks is decided once the whole block's fit against
    `*PRINT-RIGHT-MARGIN*` is known (`flush_pprint_frame`).
    """
    name = kind.name.upper() if isinstance(kind, lisptype.LispSymbol) else None
    if name not in _PP_NEWLINE_KINDS:
        raise lisptype.LispTypeError(
            f"PPRINT-NEWLINE: kind must be :LINEAR, :FILL, :MISER or "
            f":MANDATORY, not {_write_object(kind, escape=True)}",
            expected_type='(MEMBER :LINEAR :FILL :MISER :MANDATORY)', actual_value=kind)
    if not _printer._true(_printer.resolve_control('*PRINT-PRETTY*')):
        return lisptype.NIL
    frame = _current_pprint_frame_or_none()
    if frame is None:
        return lisptype.NIL
    frame.stream.tokens.append(('break', _PP_NEWLINE_KINDS[name]))
    return lisptype.NIL


@_registry.cl_function('PPRINT-TAB')
def pprint_tab(kind, colnum, colinc, stream=None):
    """Tab to a column inside a logical block (CLHS 22.2.1.2).

    `kind` is `:line`, `:section`, `:line-relative` or `:section-relative`;
    the sectioned kinds measure from the start of the enclosing logical
    block's section rather than the line start. The tab is meaningful only
    on a *pretty-printing* stream -- which, in this implementation's shape,
    is the buffered stream a `PPRINT-LOGICAL-BLOCK` binds its stream symbol
    to: a tab aimed at a raw stream does nothing at all even with
    `*PRINT-PRETTY*` true (`pprint-tab.non-pretty.1`-`.4`), and a tab
    inside a block whose `*PRINT-PRETTY*` is false does nothing either
    (`.5`-`.8`). Inside a block it records a `'tab'` token in that block's
    buffer; the spaces are emitted by the render pass, which is the only
    place the column the tab runs from is actually known.
    """
    name = kind.name.upper() if isinstance(kind, lisptype.LispSymbol) else None
    if name not in ('LINE', 'SECTION', 'LINE-RELATIVE', 'SECTION-RELATIVE'):
        raise lisptype.LispTypeError(
            f"PPRINT-TAB: kind must be :LINE, :SECTION, :LINE-RELATIVE or "
            f":SECTION-RELATIVE, not {_write_object(kind, escape=True)}",
            expected_type='(MEMBER :LINE :SECTION :LINE-RELATIVE :SECTION-RELATIVE)',
            actual_value=kind)
    if not _printer._true(_printer.resolve_control('*PRINT-PRETTY*')):
        return lisptype.NIL
    try:
        stop, step = int(colnum), int(colinc)
    except (TypeError, ValueError):
        raise lisptype.LispTypeError(
            f"PPRINT-TAB: colnum and colinc must be non-negative integers, "
            f"got {_write_object(colnum, escape=True)} and "
            f"{_write_object(colinc, escape=True)}",
            expected_type='(MOD 536870911)', actual_value=colnum)
    target = resolve_output_stream(stream)
    import fclpy.state as state
    for frame in reversed(getattr(state, 'pprint_stack', []) or []):
        if frame.stream is target:
            if frame.skip_output:
                return lisptype.NIL
            frame.stream.tokens.append(
                ('tab', name.lower(), stop, step))
            return lisptype.NIL
    return lisptype.NIL


@_registry.cl_function('PPRINT-TABULAR')
def pprint_tabular(stream, object, colon_p=None, at_sign_p=None, tabsize=None):
    """The list in columns (CLHS 22.2.2).

    Like PPRINT-FILL, but each separator is also a
    `(pprint-tab :section-relative 0 tabsize)` -- the definition in CLHS's
    own note on these functions -- so element starts align on a `tabsize`
    grid measured from the section start. `tabsize` defaults to 16 when
    omitted or NIL.
    """
    if tabsize is None or tabsize is lisptype.NIL or tabsize is False:
        size = 16
    else:
        size = int(tabsize)
    return _pprint_list_style(stream, object, colon_p, 'fill', tabsize=size)


@_registry.cl_function('PPRINT-FILL')
def pprint_fill(stream, object, colon_p=None, at_sign_p=None):
    """As many elements per line as the margin allows (CLHS 22.2.2).

    `colon-p` decides the parentheses; `at-p` is accepted and ignored.
    """
    return _pprint_list_style(stream, object, colon_p, 'fill')


@_registry.cl_function('SET-PPRINT-DISPATCH')
def set_pprint_dispatch(type_specifier, function, priority=0, table=None):
    """Set a pretty-print dispatch table entry (CLHS 22.2.1.4).

    The dispatch table's *entries* are still not consulted by anything --
    see `PprintDispatchTable`'s docstring, the pretty printer itself is
    unimplemented -- but `priority` is a required argument type CLHS spells
    out ("priority --- a real") independent of the rest of the mechanism
    ever being finished, and skipping the check here was a silent-acceptance
    path (CLAUDE.md standing rule 4): `set-pprint-dispatch.error.4`/`-unsafe`
    hand every non-real in `*mini-universe*` through as `priority` and
    require an error for each. `table` omitted defaults to the current
    `*PRINT-PPRINT-DISPATCH*`, NIL denotes the standard table (the same
    designator convention `COPY-PPRINT-DISPATCH` uses just above), matching
    the entries list the docstring above says this operator writes.
    """
    if priority is None:
        # Only a truly *omitted* argument defaults -- NIL is not "omitted"
        # (CLAUDE.md's OMITTED-vs-NIL distinction): `priority` NIL must
        # still fail the type check below, which is exactly what
        # `set-pprint-dispatch.error.4`'s NIL element of `*mini-universe*`
        # requires an error for.
        priority = 0
    if isinstance(priority, bool) or not isinstance(priority, (int, float, Fraction)):
        raise lisptype.LispTypeError(
            f"SET-PPRINT-DISPATCH: priority must be a real number, got {priority!r}",
            expected_type="REAL", actual_value=priority)
    if table is None or table is lisptype.NIL:
        from .binding import dynamic_value
        dispatch_table = dynamic_value(
            lisptype.COMMON_LISP_PACKAGE.intern_symbol('*PRINT-PPRINT-DISPATCH*'),
            default=standard_pprint_dispatch())
    else:
        dispatch_table = table
    if not isinstance(dispatch_table, PprintDispatchTable):
        raise lisptype.LispTypeError(
            f"SET-PPRINT-DISPATCH: not a pprint dispatch table: {dispatch_table!r}",
            expected_type='PPRINT-DISPATCH-TABLE', actual_value=dispatch_table)
    # CLHS 22.2.1.4: "The first action of set-pprint-dispatch is to remove
    # any pre-existing entry associated with type-specifier." So `function`
    # NIL is *not* an error -- it is the documented way to remove a dispatch
    # entry, and the post-condition is just that no entry with that spec is
    # left. Any other `function` value is a function designator (CLHS 5.1.1):
    # a function object, a symbol whose function binding is a function, or
    # NIL. The test that uses a symbol is pprint-dispatch.9; here we
    # resolve it through the same path `funcall` would, so the entry the
    # printer actually stores is callable.
    resolved_fn = _coerce_to_dispatch_function(function, 'SET-PPRINT-DISPATCH')
    # Drop any pre-existing entry with the same EQUAL specifier. Equality
    # is by `equal` per CLHS, which `PprintDispatchTable.remove_spec` already
    # implements -- specifier lists are compared structurally.
    dispatch_table.remove_spec(type_specifier)
    # A NIL `function` is the documented "remove this entry" path; do not
    # re-add. A function value is the documented "install this entry" path.
    if resolved_fn is not None and resolved_fn is not lisptype.NIL:
        dispatch_table.entries.append((type_specifier, resolved_fn, priority))
    return lisptype.NIL


def _coerce_to_dispatch_function(function, what):
    """CLHS 5.1.1: a function designator is a function, a symbol naming one,
    or NIL.

    `set-pprint-dispatch` accepts a function designator for the `function`
    argument; the dispatch table needs the callable itself, so a symbol is
    resolved via the function-cell (CLHS 5.1.2's `symbol-function` rule).
    Anything else -- a list, a number, an unbound symbol -- is a TYPE-ERROR.
    `pprint-dispatch.9` exercises a function-bound symbol; that path is the
    one this coercion exists to make pass.
    """
    if function is None or function is lisptype.NIL:
        return lisptype.NIL
    if isinstance(function, lisptype.LispSymbol):
        # Resolve the symbol's function binding, mirroring SYMBOL-FUNCTION's
        # own UNDEFINED-FUNCTION signal so an unbound symbol stays an error
        # rather than being silently treated as NIL.
        from .utilities_functions import symbol_function
        try:
            return symbol_function(function)
        except Exception:
            from fclpy.lispfunc.evaluation_core import ConditionException
            cond = lisptype.UndefinedFunction(name=function)
            raise ConditionException(cond, recoverable=False)
    if callable(function):
        return function
    raise lisptype.LispTypeError(
        f"{what}: function is not a function designator: {function!r}",
        expected_type='(OR FUNCTION (MEMBER NIL) SYMBOL)', actual_value=function)


# Format operations

class _FormatCursor:
    """Mutable argument cursor for FORMAT.

    This is the structural fix for FORMAT's argument-consumption model:
    directives that give a nested control string access to the *same*
    argument stream (~<...~>, ~(...~), ~[...~]) share one cursor instance,
    so that arguments consumed inside the nested directive (including via
    ~:*, ~*, ~:P) are visible to whatever follows the directive in the
    outer control string. Previously each nested call sliced `args` and
    started a fresh index at 0, which silently discarded consumption.

    Directives that give a nested control string its *own* independent
    argument scope per CLHS (~{...~} iterating over a list argument's
    elements; ~? with a separate format-args list) construct a fresh
    cursor instead of sharing this one - that is correct, not a bug.

    `last_item` records, for a cursor driving one pass of a `~:{...~}` or
    `~:@{...~}` iteration, whether the sublist it holds is the LAST one --
    the fact CLHS 22.3.9.2's `~:^` needs ("the entire iteration process is
    terminated if and only if the sublist that is supplying the arguments
    for the current iteration step is the last"): a no-parameter `~:^` fires
    only on that pass. It is meaningless on every other cursor, and the
    default True keeps `~:^` outside any iteration behaving as it always
    did (terminating).
    """
    __slots__ = ('args', 'idx', 'last_item')

    def __init__(self, args, idx=0, last_item=True):
        self.args = list(args) if args else []
        self.idx = idx
        self.last_item = last_item

    def next(self):
        if self.idx < len(self.args):
            val = self.args[self.idx]
            self.idx += 1
            return val
        return None

    def prev(self):
        """The argument last consumed, without consuming another (~:P)."""
        if 0 < self.idx <= len(self.args):
            return self.args[self.idx - 1]
        return None

    def remaining(self):
        return self.args[self.idx:]

    def remaining_count(self):
        return len(self.args) - self.idx


class _FormatEscape(Exception):
    """Raised by `~^` to terminate the enclosing iteration or control string.

    CLHS 22.3.9.2 makes `~^` a *control transfer*, not a character: it
    abandons the rest of the control string it appears in. The previous
    implementation returned an in-band `'\\u0000'` marker and let callers
    `str.replace` it out, which is the same defect class as standing rule 4
    -- it cannot represent "stop here" distinctly from "emit a NUL", it
    silently corrupted any output that legitimately contained NUL, and the
    marker had to be re-detected by every construct that might contain a
    `~^`.

    `partial` carries the text produced *before* the escape, which CLHS
    requires be kept: `~{~A~^, ~}` over `(1 2 3)` must emit `1, 2, 3` --
    the final pass's `1`-equivalent survives, only the trailing `, ` is
    abandoned. Each `_format_process_cursor` frame prepends its own
    accumulated output as the exception unwinds, so the text is assembled
    in the same order it would have been concatenated.

    `terminate_outer` is set by `~:^`, which terminates the iteration one
    level out rather than the innermost one.
    """

    def __init__(self, partial='', terminate_outer=False):
        super().__init__('FORMAT ~^ escape')
        self.partial = partial
        self.terminate_outer = terminate_outer


def _format_args_list(value):
    """Coerce a FORMAT argument that is *required to be a list* into a Python list.

    `~{...~}` and `~?` take a list argument and iterate over its elements.
    Every such site previously tested `isinstance(value, (list, tuple))` and
    fell through to `[value]`, but a Lisp list reaching FORMAT is a
    `lispCons`, never a Python list -- so the test was false for exactly the
    argument shape the directive exists to handle, and the whole list was
    treated as a single opaque element. `(format nil "~{~A ~}" '(1 2 3))`
    returned `"(1 2 3) "` instead of `"1 2 3 "`.

    Cons traversal is delegated to the sequence protocol's `seq_elements`
    rather than open-coded a third time (standing rule 3); this function
    only adds the FORMAT-specific edges: NIL is the empty argument list, and
    a non-list argument stays wrapped rather than silently becoming empty.
    """
    if value is None or value is lisptype.NIL:
        return []
    if isinstance(value, (list, tuple)):
        return list(value)
    if hasattr(value, 'car') and hasattr(value, 'cdr'):
        from .sequence_protocol import seq_elements
        return seq_elements(value)
    return [value]


def _check_format_sublist(item, directive):
    """Each element feeding one pass of a `~:{...~}`/`~:@{...~}` iteration
    must be a list (CLHS 22.3.7.2: "each element ... must be a list"), and a
    dotted one is not -- a dotted tail is not a set of arguments for the
    pass. NIL is the empty sublist. Anything else is a TYPE-ERROR
    (`format.:{.error.1`/`.4`/`.5`, `format.:@{.error.1`-`.4`), not a
    one-element pass over the non-list as if it were `([item])` -- which is
    what `_format_args_list`'s fall-through would have made of it.

    The proper-list walk (and its dotted-tail TYPE-ERROR) is `list_cells`'s
    own, not a second copy. Note a *vector* is not a list either: the suite
    reads `#(X Y Z)` as a Python list, which is why this check does not
    accept one the way the outer `~{` argument check does.
    """
    if item is None or item is lisptype.NIL:
        return
    from .core import _consp_internal
    if _consp_internal(item):
        from .sequence_protocol import list_cells
        for _ in list_cells(item, f"FORMAT {directive}", dotted='error'):
            pass
        return
    raise lisptype.LispTypeError(
        f"FORMAT: {directive} argument is not a list: "
        f"{_write_object(item, escape=True)}",
        expected_type='LIST', actual_value=item)


def _lisp_number(value, default=0):
    """Read a `~^` prefix parameter as an integer.

    A parameter is either a literal from the control string (already an
    int), a `'c` character literal, or whatever `~V` pulled off the argument
    list, which may be a Lisp integer or character object. A character
    parameter (CLHS 22.3.3's `'c` syntax) compares by its code, the same way
    `EQL` would -- not as 0, which would make every `'c`-parameterised `~^`
    fire unconditionally. Anything else non-numeric falls back to `default`
    rather than raising, because `~^`'s parameters only select between
    "terminate" and "keep going" -- a malformed one must not abort the whole
    FORMAT.
    """
    if value is None:
        return default
    if isinstance(value, bool):
        return int(value)
    if isinstance(value, int):
        return value
    if isinstance(value, lisptype.Character):
        return ord(value.char)
    if isinstance(value, str) and len(value) == 1:
        return ord(value)
    try:
        return int(value)
    except (TypeError, ValueError):
        return default


def _pad_params(params):
    """Read the `mincol,colinc,minpad,padchar` prefix parameters shared by
    `~A`, `~S` and `~<...~>`, applying each one's CLHS default."""
    def _param(i, default):
        # `_is_unspecified`, not `is not None`: a `~V` parameter whose argument
        # is Lisp NIL means "this slot was not supplied" (CLHS 22.3), and NIL
        # reaches Python as `lisptype.NIL` rather than as `None`. Reading it as
        # a value made `(format nil "~,,1,v<~A~;~A~>" nil "ABC" "DEF")` pad
        # with the first character of "NIL" -- `"ABCNDEF"` where the padding
        # character must default to space.
        if len(params) > i and not _is_unspecified(params[i]):
            return params[i]
        return default

    return (
        _lisp_number(_param(0, 0)),
        _lisp_number(_param(1, 1)) or 1,
        _lisp_number(_param(2, 0)),
        # `_format_char_param` is the one reader of a character-valued prefix
        # parameter, shared with `~D`/`~R`'s padchar and commachar; this used
        # to be a third copy of the same `Character`-or-`str` unwrapping.
        _format_char_param(_param(3, ' '), ' '),
    )


def _format_pad(text, params, at_flag):
    """Apply CLHS 22.3's `mincol,colinc,minpad,padchar` column padding.

    Shared by `~A` and `~S`, which specify identical padding behaviour and
    previously each honoured only `mincol` with a hardcoded space -- so
    `~,,2A` (minpad) and `~4,,,'xA` (padchar) were silently ignored rather
    than being unimplemented loudly.

    The rule: emit at least `minpad` copies of `padchar`, then keep adding
    `colinc` more until the total width is at least `mincol`. Padding goes
    on the right, or on the left when the `@` modifier is present.
    """
    mincol, colinc, minpad, padchar = _pad_params(params)

    pad = minpad
    while len(text) + pad < mincol:
        pad += colinc

    if pad <= 0:
        return text
    padding = padchar * pad
    return padding + text if at_flag else text + padding


def _justify(texts, params, colon_flag, at_flag):
    """Lay out `~mincol,colinc,minpad,padchar<seg~;seg~>` per CLHS 22.3.6.2.

    Padding is inserted at the *gaps between* segments, not around the whole
    string: `~;` is a padding point. The `:` modifier adds a padding point
    before the first segment and `@` one after the last, so `~10<abc~>` --
    one segment, no modifiers -- has its single padding point on the left
    and therefore right-justifies, which is the common case.

    Total width grows from `minpad` per gap in steps of `colinc` until it
    reaches `mincol`; the resulting spaces are spread as evenly as possible,
    with the leftmost gaps taking the remainder.
    """
    if not texts:
        texts = ['']

    mincol, colinc, minpad, padchar = _pad_params(params)

    # Where padding may go: one point per `~;` separator, plus a leading one
    # for `:` and a trailing one for `@`.
    specified_gaps = (len(texts) - 1) + (1 if colon_flag else 0) + (1 if at_flag else 0)

    # With no specified point at all, one is *assumed* at the left, which is
    # what makes `~10<abc~>` right-justify. It is an assumed point and not a
    # specified one, and the difference is `minpad`: minpad is "the minimum
    # number of padding characters at each padding location", and there is no
    # location here for it to be the minimum of. Collapsing the two (this
    # counted the assumed point as a gap) made `~39,,6<~A~>` on 36 characters
    # emit six spaces where it must emit three, and `~5,,1<~A~>` on sixteen
    # emit one where it must emit none.
    gaps = specified_gaps or 1

    content_width = sum(len(t) for t in texts)

    # CLHS 22.3.6.2 constrains the *total width*, not just the padding: it is
    # `mincol + k*colinc` for the smallest non-negative k that still fits the
    # content plus each specified point's minpad. Growing the padding only
    # while short of `mincol` misses the `colinc` rounding whenever mincol is
    # already satisfied -- `~,6<~A~>` on two characters must produce six
    # columns, not two, because zero plus one step of six is the smallest
    # admissible width.
    required = content_width + minpad * specified_gaps
    width = mincol
    while width < required:
        width += colinc
    total_pad = width - content_width

    # "As evenly as possible", with a remainder that will not divide going to
    # the *rightmost* gaps: `~15,,,'*<AA~4T~;BBBB~;CCCC~>` is
    # `AA  *BBBB**CCCC` -- three pad characters over two gaps as 1 then 2, not
    # 2 then 1 (`format.justify.34`/`.36` pin this from both parameter forms).
    base, extra = divmod(total_pad, gaps)
    widths = [base + (1 if i >= gaps - extra else 0) for i in range(gaps)]

    out = []
    gap_index = 0
    if colon_flag:
        out.append(padchar * widths[gap_index])
        gap_index += 1
    for i, text in enumerate(texts):
        out.append(text)
        is_last = (i == len(texts) - 1)
        if not is_last:
            out.append(padchar * widths[gap_index])
            gap_index += 1
        elif gap_index < len(widths):
            # Either the trailing @ point, or the implicit leading point of a
            # lone segment -- which must go *before* the text, not after.
            if at_flag or len(texts) > 1:
                out.append(padchar * widths[gap_index])
            else:
                out.insert(len(out) - 1, padchar * widths[gap_index])
            gap_index += 1
    return ''.join(out)


def _scan_directive(control_string, pos):
    """Skip one directive's *syntax*, starting at its `~`. No interpretation.

    Returns `(params, colon, at, directive, next_pos)`. `params` holds the
    literal prefix parameters -- an int, a one-character string, or None for an
    empty slot and for the `V`/`#` forms, whose values need an argument cursor
    this function deliberately does not have. `directive` is upper-cased, or
    None at end of string.

    This is the one place a directive's syntax is walked without also being
    executed, which two callers need: the `~<...~>` segment scan, and CLHS
    22.3.6.2's conflict check. The segment scan used to advance over the
    character set `'0123456789,:#@'` and stop at anything outside it, so a
    `'c` character parameter ended the parameter run at the quote and the
    quoted character itself was then read as the directive -- and it recorded
    no parameters at all, which is why a `~n,m:;` separator's own line-width
    parameter had nowhere to come from.
    """
    n = len(control_string)
    i = pos + 1
    params = []
    colon = at = False
    slot_has_value = False
    while i < n:
        c = control_string[i]
        if c.isdigit() or (c in '+-' and i + 1 < n and control_string[i + 1].isdigit()):
            start = i
            if c in '+-':
                i += 1
            while i < n and control_string[i].isdigit():
                i += 1
            params.append(int(control_string[start:i]))
            slot_has_value = True
        elif c == "'":
            params.append(control_string[i + 1] if i + 1 < n else None)
            i += 2
            slot_has_value = True
        elif c in 'Vv#':
            params.append(None)
            i += 1
            slot_has_value = True
        elif c == ',':
            i += 1
            if not slot_has_value:
                params.append(None)
            slot_has_value = False
        elif c == ':':
            colon = True
            i += 1
        elif c == '@':
            at = True
            i += 1
        else:
            break
    if i >= n:
        return params, colon, at, None, n
    return params, colon, at, control_string[i].upper(), i + 1


def _current_column(emitted):
    """The column the next character emitted would land in.

    `emitted` is the chunks this control string has produced so far, so this
    is a control-string-local column, the same best-effort one `~&` and
    `~<...~:>` already use (plan.md's recorded FRESH-LINE gap). It is
    nevertheless the *real* answer for the directives that need it here:
    `~T` and a `~:;`-terminated justification both measure from the start of
    the current line, and FORMAT emitted that line itself.
    """
    text = ''.join(emitted) if emitted else ''
    return _pp_visible_width(text.rsplit('\n', 1)[-1])


def _compute_tab_size(kind, colnum, colinc, column, section_start):
    """The number of spaces one tab directive emits (CLHS 22.2.1.2, 22.3.6.1).

    One function for both `PPRINT-TAB`'s four kinds and FORMAT's `~T` family,
    which are the same operation (`~T` is `:line`, `~:T` `:section`, `~@T`
    `:line-relative`, `~:@T` `:section-relative`). `colnum` is the stop
    column for the absolute kinds and the offset for the relative ones;
    `colinc` is the grid the relative kinds round up to. The sectioned kinds
    measure from `section_start` -- the column where the enclosing logical
    block's section began -- instead of the line start.

    The formulas are the ones `pprint-tab.line.1` and `pprint-tab.line-
    relative.1` pin down: tab to `colnum`, or -- already at or past it -- to
    the next `colnum + k*colinc` beyond the current column (no move at all
    when `colinc` is 0); and for the relative kinds, `colnum` spaces then
    enough more to reach the next multiple of `colinc`.
    """
    relative = kind in ('LINE-RELATIVE', 'SECTION-RELATIVE')
    position = column - (section_start if kind in ('SECTION', 'SECTION-RELATIVE') else 0)
    if relative:
        colnum = max(colnum, 0)
        if colinc > 1:
            rem = (position + colnum) % colinc
            if rem:
                colnum += colinc - rem
        return colnum
    if position < colnum:
        return colnum - position
    if colinc <= 0:
        return 0
    return colinc - ((position - colnum) % colinc)


def _tab_padding(column, params, colon_flag, at_flag):
    """The spaces `~T` emits from `column` (CLHS 22.3.6.1).

    `_compute_tab_size` owns the arithmetic; `~T` has no section of its own,
    so the section origin is 0. Both forms used to be `' ' * colnum`, under
    the comment "we don't track column, so just emit spaces" -- a different
    directive entirely: `AA~4T` is *move to column 4*, two spaces, and
    answered four. The column was in fact available all along (`emitted`),
    which is what `_current_column` reads.
    """
    def param(index, default):
        if len(params) > index and not _is_unspecified(params[index]):
            value = params[index]
            if isinstance(value, bool):
                return default
            if isinstance(value, int):
                return value
        return default

    if at_flag:
        colrel = param(0, 1)
        colinc = param(1, 1)
        return ' ' * _compute_tab_size('LINE-RELATIVE', colrel, colinc, column, 0)

    colnum = param(0, 1)
    colinc = param(1, 1)
    return ' ' * _compute_tab_size('LINE', colnum, colinc, column, 0)


#: The directives that mean something only to the pretty printer, and so may
#: not keep company with a `~<...~>` justification (CLHS 22.3.6.2). `~:T` is
#: the fourth -- it is `PPRINT-TAB :section`, a different directive from the
#: plain `~T` handled above, and is recognized by the colon flag.
_PRETTY_ONLY_DIRECTIVES = ('W', '_', 'I')


def _check_justification_conflicts(control_string):
    """CLHS 22.3.6.2's two restrictions on `~<...~>`, in one scan.

    A plain justification is not a pretty-printing construct -- it lays its
    own segments out and has no logical block, no indentation and no
    conditional newlines -- so `~W`, `~_`, `~I` and `~:T` may not appear
    *inside* one. And when the justification uses the `~:;` line-overflow
    form, whose whole decision is about where the line ends, they may not
    appear anywhere in the same control string, nor may a `~<...~:>` logical
    block.

    The two rules are the same restriction seen from two distances, which is
    why they share a scan; ansi-test asks them in matching triples
    (`format.justify.error.w.1` is inside, `.2` and `.3` are before and after,
    and `~_`, `~I` and `~:T` each repeat all three).

    Whether a `~<` opened a justification or a logical block is known only at
    its `~>` -- the colon flag lives on the *closer* -- so offending
    directives are recorded against the innermost open `~<` and judged when it
    closes.
    """
    blocks = []       # one record per `~<` seen, in order
    open_stack = []   # indices into `blocks` for the `~<`s still open
    all_offenders = []
    colon_semi_justification = None
    logical_block = False

    pos = 0
    n = len(control_string)
    while pos < n:
        if control_string[pos] != '~':
            pos += 1
            continue
        _params, colon, _at, directive, pos = _scan_directive(control_string, pos)
        if directive is None:
            break
        if directive == '<':
            blocks.append({'offenders': [], 'colon_semi': False})
            open_stack.append(len(blocks) - 1)
        elif directive == '>':
            if open_stack:
                block = blocks[open_stack.pop()]
                if colon:
                    logical_block = True
                elif block['offenders']:
                    raise lisptype.LispProgramError(
                        f"FORMAT: {block['offenders'][0]} cannot appear inside "
                        "~<...~> (justification) -- CLHS 22.3.6.2")
                elif block['colon_semi']:
                    colon_semi_justification = True
        elif directive == ';':
            if open_stack and colon:
                blocks[open_stack[-1]]['colon_semi'] = True
        elif directive in _PRETTY_ONLY_DIRECTIVES or (directive == 'T' and colon):
            name = '~' + (':' if colon else '') + directive
            all_offenders.append(name)
            if open_stack:
                blocks[open_stack[-1]]['offenders'].append(name)

    if colon_semi_justification:
        if all_offenders:
            raise lisptype.LispProgramError(
                f"FORMAT: {all_offenders[0]} cannot appear in a control string "
                "that also contains a ~<...~:;...~> justification -- CLHS 22.3.6.2")
        if logical_block:
            raise lisptype.LispProgramError(
                "FORMAT: ~<...~:> (logical block) cannot appear in a control "
                "string that also contains a ~<...~:;...~> justification -- "
                "CLHS 22.3.6.2")


def _capitalize_words(s):
    """~:( ... ~) - capitalize the first letter of each word, force the
    rest of each word to lower case. A word starts after whitespace."""
    result = []
    at_word_start = True
    for ch in s:
        if ch.isalpha():
            if at_word_start:
                # Capitalize: use uppercase if it's a single character
                uppered = ch.upper()
                result.append(uppered if len(uppered) == 1 else ch)
            else:
                # Lowercase: use lowercase if it's a single character
                lowered = ch.lower()
                result.append(lowered if len(lowered) == 1 else ch)
            at_word_start = False
        else:
            result.append(ch)
            # Preserve at_word_start unless we see whitespace (which ends a word)
            # Non-whitespace, non-alpha chars (digits, punctuation) don't reset word-start
            if ch.isspace():
                at_word_start = True
            # else: keep current at_word_start value
    return ''.join(result)


def _capitalize_first_word(s):
    """~@( ... ~) - capitalize the first letter of the first word, force
    the rest of the output to lower case."""
    result = []
    capitalized_any = False
    at_word_start = True
    for ch in s:
        if ch.isalpha():
            if not capitalized_any and at_word_start:
                # Capitalize: use uppercase if it's a single character
                uppered = ch.upper()
                result.append(uppered if len(uppered) == 1 else ch)
                capitalized_any = True
            else:
                # Lowercase: use lowercase if it's a single character
                lowered = ch.lower()
                result.append(lowered if len(lowered) == 1 else ch)
            at_word_start = False
        else:
            result.append(ch)
            # Preserve at_word_start unless we see whitespace (which ends a word)
            # Non-whitespace, non-alpha chars (digits, punctuation) don't reset word-start
            if ch.isspace():
                at_word_start = True
            # else: keep current at_word_start value
    return ''.join(result)


def _is_unspecified(value):
    """A FORMAT prefix parameter position that was left blank (`None`, an
    empty comma slot) or supplied as Lisp NIL (typically via `~V`)."""
    return value is None or value is lisptype.NIL


def _format_repeat_count(params, default=1):
    """The repeat count of a `~n%` / `~n&` / `~n~` / `~n|` directive.

    The one place that distinction is made, because it is exactly the one the
    obvious spelling gets wrong. `params[0] if params and params[0] else 1`
    treats an explicit **zero** as "no parameter supplied" -- Python says 0 is
    falsy -- so `(format nil "~0~")` emitted one tilde where CLHS requires
    none, and likewise for `~0%` and `~0|`. Every failing test in
    `format-tilde.lsp` was the n=0 case, and the count reaches here as a real 0
    from `~0~`, from `~V~` given 0, and from `~#~` with no arguments left.

    `_is_unspecified` is the existing resolver for "blank or NIL", so an
    omitted parameter still defaults; only a *supplied* value is honoured, 0
    included.
    """
    if not params or _is_unspecified(params[0]):
        return default
    return params[0]


def _format_char_param(value, default):
    """Read a pad/comma character prefix parameter: `'x` literal, `~V`
    (which supplies a CHARACTER object or a one-char string), or unspecified."""
    if _is_unspecified(value):
        return default
    if isinstance(value, lisptype.Character):
        return value.char
    s = str(value)
    return s[:1] if s else default


def _numeric_pad_params(params):
    """Read the `mincol,padchar,commachar,comma-interval` prefix parameters
    shared by `~D`, `~X`, `~O`, `~B` and `~R` (CLHS 22.3.2), applying each
    one's default. `comma-interval` defaults to 3 and a non-positive value
    (which would make grouping meaningless) falls back to the same default."""
    def _p(i):
        return params[i] if len(params) > i else None

    mincol = _lisp_number(_p(0), 0)
    padchar = _format_char_param(_p(1), ' ')
    commachar = _format_char_param(_p(2), ',')
    comma_interval = _lisp_number(_p(3), 3)
    if comma_interval <= 0:
        comma_interval = 3
    return mincol, padchar, commachar, comma_interval


_RADIX_DIGITS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"


def _int_to_radix_digits(num, radix):
    """Render a non-negative integer's magnitude in `radix` (2-36), one
    engine for `~D`/`~X`/`~O`/`~B` and explicit-radix `~R` instead of four
    copies each hardcoded to a Python builtin (`hex`/`oct`/`bin`) that only
    covers three of the five radices FORMAT needs."""
    if num == 0:
        return '0'
    chars = []
    while num:
        num, rem = divmod(num, radix)
        chars.append(_RADIX_DIGITS[rem])
    return ''.join(reversed(chars))


def _insert_comma_groups(digits, commachar, interval):
    """Group `digits` from the right in chunks of `interval`, per CLHS
    22.3.2's `:` modifier. A single group (interval >= len(digits)) gets no
    separator at all -- that is what makes `~,,,#:d` applied with a
    comma-interval equal to the digit count print with no commas."""
    n = len(digits)
    if interval <= 0 or interval >= n:
        return digits
    first_len = n % interval or interval
    parts = [digits[:first_len]]
    for i in range(first_len, n, interval):
        parts.append(digits[i:i + interval])
    return commachar.join(parts)


def _format_A_fallback(val, radix=None):
    """CLHS 22.3.2.1: `~nR` -- and so `~D`/`~X`/`~O`/`~B` -- binds
    *PRINT-ESCAPE*, *PRINT-RADIX*, *PRINT-BASE* and *PRINT-READABLY* for the
    extent of the directive, so a non-integer argument is printed as if by
    `~A` with *PRINT-BASE* bound to the radix: `format.b.18`'s `~b` of 3/5
    is "11/101", not the base-10 "3/5" plain PRINC would give. `radix=None`
    is the ~A fall-through for the float directives (~F/~E/~G, CLHS
    22.3.3.1), which bind nothing."""
    if radix is None:
        return _printer.princ_to_string(val)
    from .binding import dynamic_value, set_dynamic_value
    saved = []
    try:
        for name, value in (('*PRINT-ESCAPE*', lisptype.NIL),
                            ('*PRINT-RADIX*', lisptype.NIL),
                            ('*PRINT-BASE*', radix),
                            ('*PRINT-READABLY*', lisptype.NIL)):
            symbol = lisptype.COMMON_LISP_PACKAGE.intern_symbol(name)
            saved.append((symbol, dynamic_value(symbol)))
            set_dynamic_value(symbol, value)
        return _printer.princ_to_string(val)
    finally:
        for symbol, old in reversed(saved):
            set_dynamic_value(symbol, old)


def _format_integer_directive(val, radix, params, colon_flag, at_flag):
    """The shared digit-printing engine behind `~D`/`~X`/`~O`/`~B` and
    explicit-radix `~R`: sign, radix conversion, `:` comma-grouping, then
    `mincol`/`padchar` right-justification -- in that order, since padding
    must see the sign and commas already in place (CLHS 22.3.2)."""
    if not isinstance(val, int):
        return _format_A_fallback(val, radix)
    mincol, padchar, commachar, comma_interval = _numeric_pad_params(params)
    neg = val < 0
    digits = _int_to_radix_digits(abs(val), radix)
    if colon_flag:
        digits = _insert_comma_groups(digits, commachar, comma_interval)
    sign = '-' if neg else ('+' if at_flag else '')
    body = sign + digits
    if len(body) < mincol:
        body = padchar * (mincol - len(body)) + body
    return body


def _param_int_or_none(value):
    """A numeric FORMAT prefix parameter (`w`, `d`, `k`, ...) that
    distinguishes "omitted" from "explicitly 0" -- CLHS 22.3.3/22.3.4 give
    `~F`/`~E` different behaviour for the two (an omitted `d` chooses digits
    naturally; `d=0` truncates the fraction to nothing)."""
    if _is_unspecified(value):
        return None
    return _lisp_number(value, None)


def _format_pad_or_overflow(text, w, overflowchar, padchar):
    """CLHS 22.3.3/22.3.4's shared tail: right-justify to `w` with
    `padchar`, or -- if the natural text is longer than `w` -- replace it
    with `w` copies of `overflowchar`. With no `overflowchar`, a field that
    does not fit is printed in full and `w` is only ever a minimum."""
    if w is None:
        return text
    if len(text) < w:
        return padchar * (w - len(text)) + text
    if len(text) > w and overflowchar is not None:
        return overflowchar * w
    return text


_shortest_round_trip_digits = _printer.float_shortest_digits


def _split_digits(digits, decpt):
    """Place a decimal point `decpt` digits into `digits` (CLHS's `0.<digits>
    * 10**decpt` convention), producing `(int_part, frac_part)`. No digit is
    forced on either side: a whole number gives an empty fraction and a
    number below 1 gives an integer part of `"0"`. This is what `~E` wants
    (`1.e+0`, no trailing zero) and what a *quantized* digit string for `~F`
    wants too, since quantizing already pads to the requested length."""
    if decpt <= 0:
        return '0', '0' * (-decpt) + digits
    if decpt >= len(digits):
        return digits + '0' * (decpt - len(digits)), ''
    return digits[:decpt], digits[decpt:]


def _split_digits_prin1(digits, decpt):
    """Same as `_split_digits`, but for `~F` with `d` omitted, which mimics
    `PRIN1` (CLHS 22.1.3.1.3: a float always shows a digit on each side of
    the point) rather than `~E`'s trailing-zero-trimming rule -- a whole
    number gets a forced `.0`, not an empty fraction."""
    if decpt <= 0:
        return '0', '0' * (-decpt) + digits
    if decpt >= len(digits):
        return digits + '0' * (decpt - len(digits)), '0'
    return digits[:decpt], digits[decpt:]


def _coerce_format_float(val):
    """`~F`/`~E`/`~G` accept any real (CLHS 22.3.3): a rational is coerced,
    a float is used as-is."""
    if isinstance(val, bool) or val is None:
        raise TypeError('not a real number')
    return float(val)


def _format_fixed_directive(val, params, at_flag):
    """`~w,d,k,overflowchar,padcharF` -- fixed-format floating point
    (CLHS 22.3.3).

    `k` is an exact decimal scale factor (the value is multiplied by
    `10**k` before printing), applied as a shift of the decimal digit
    string's implied point rather than as float multiplication, so it
    never adds rounding error beyond what `d` itself requests. With `d`
    omitted and no `w`, the natural, `PRIN1`-equivalent digit string is used
    (matching `format.f.1`-`.3`, which check exactly that equivalence).
    With `d` omitted but `w` given, CLHS 22.3.3.1 still requires a specific
    `d`: as many of the natural digits as fit in `w` (never more), so
    `~2f` of `1.1` -- one natural fraction digit, but no room for even that
    once the sign, `1` and `.` claim the rest of a 2-column field -- rounds
    to 0 fraction digits and prints `1.0`, the forced single zero digit
    being CLHS's own workaround for "arg is printed as a float" (`1.` reads
    back as the integer 1, not a float) -- see `format.f.45`-`.47`, which
    exist to pin exactly this down. With `d` given explicitly, the value is
    rounded to exactly `d` fraction digits from its *exact* value
    (`Decimal(float)`), not from the shortest round-trip string, so
    rounding beyond the shortest repr's precision is still correct. A lone
    leading `0` in the integer part is dropped when `w` is given, the
    natural text would otherwise overflow it, and a fraction digit still
    remains to keep the result recognizable as a float (`format.f.46b`'s
    `~0,0f` of `0.01` keeps the zero -- `0.` with the zero dropped would be
    just `.`, not a float at all) -- which is why `~3,2F` of `0.5` prints
    `.50`, not `0.50`.
    """
    w = _param_int_or_none(params[0] if len(params) > 0 else None)
    d = _param_int_or_none(params[1] if len(params) > 1 else None)
    k = _param_int_or_none(params[2] if len(params) > 2 else None) or 0
    overflowchar = _format_char_param(params[3] if len(params) > 3 else None, None)
    padchar = _format_char_param(params[4] if len(params) > 4 else None, ' ')

    num = _coerce_format_float(val)
    negative = num < 0 or (num == 0.0 and str(num)[0] == '-')
    mag = abs(num)
    sign = '-' if negative else ('+' if at_flag else '')

    if mag == 0.0:
        int_part, frac_part = ('0', '0') if d is None else ('0', '0' * d)
    elif d is None and w is None:
        digits, decpt = _shortest_round_trip_digits(mag)
        int_part, frac_part = _split_digits_prin1(digits, decpt + k)
    else:
        d_was_omitted = d is None
        if d_was_omitted:
            digits, decpt = _shortest_round_trip_digits(mag)
            natural_int, natural_frac = _split_digits_prin1(digits, decpt + k)
            avail = w - len(sign) - len(natural_int) - 1
            d = min(len(natural_frac), max(avail, 0))
        scaled = Decimal(mag) * (Decimal(10) ** k) if k else Decimal(mag)
        quantum = Decimal(1).scaleb(-d)
        rounded = scaled.quantize(quantum, rounding=ROUND_HALF_EVEN)
        _, digs, exp = rounded.as_tuple()
        digits = ''.join(map(str, digs)) or '0'
        decpt = len(digits) + exp
        int_part, frac_part = _split_digits(digits, decpt)
        if d_was_omitted and not frac_part:
            frac_part = '0'  # CLHS: still recognizable as a float (format.f.45-47)

    body = int_part + '.' + frac_part
    if w is not None and int_part == '0' and frac_part and len(sign) + len(body) > w:
        body = body[1:]
    return _format_pad_or_overflow(sign + body, w, overflowchar, padchar)


def _format_exponential_directive(val, params, at_flag):
    """`~w,d,e,k,overflowchar,padchar,exponentcharE` -- exponential-format
    floating point (CLHS 22.3.4).

    The scale factor `k` (default 1) does not just shift the exponent: it
    picks how many of a *fixed* `d+1`-digit budget fall before the decimal
    point (`max(k, 1)` of them; the rest, `d+1-max(k,1)`, are the fraction)
    -- so `~,2,,3e` of `0.05` prints `500.e-4` (all 3 budget digits used as
    integer digits, none left for the fraction) while `~,2,,-1e` of the same
    magnitude prints `0.05e+2`. This is the CLHS rule that makes
    `format.e.16`-`.19` fan out the same way; deriving `k`'s effect as a
    plain float multiplication instead reproduces none of them. With `d`
    omitted, CLHS's own trimming rule applies instead (no trailing fraction
    zero unless the value is exactly the number 0, and `w` -- if it makes
    the natural text too short -- extends the fraction with zeros rather
    than padding with spaces, since padding happens only after that).
    """
    w = _param_int_or_none(params[0] if len(params) > 0 else None)
    d = _param_int_or_none(params[1] if len(params) > 1 else None)
    e_digits = _param_int_or_none(params[2] if len(params) > 2 else None)
    k = _param_int_or_none(params[3] if len(params) > 3 else None)
    if k is None:
        k = 1
    overflowchar = _format_char_param(params[4] if len(params) > 4 else None, None)
    padchar = _format_char_param(params[5] if len(params) > 5 else None, ' ')
    exponentchar = _format_char_param(params[6] if len(params) > 6 else None, 'e')

    num = _coerce_format_float(val)
    negative = num < 0 or (num == 0.0 and str(num)[0] == '-')
    mag = abs(num)

    if mag == 0.0:
        int_part, frac_part, printed_exponent = '0', '0', 0
    else:
        digits0, decpt0 = _shortest_round_trip_digits(mag)
        scientific_exponent = decpt0 - 1
        printed_exponent = scientific_exponent - (k - 1)
        if d is None:
            int_part, frac_part = _split_digits(digits0, k)
        else:
            frac_digits = max(d + 1 - max(k, 1), 0)
            while True:
                mantissa = Decimal(mag).scaleb(-printed_exponent)
                quantum = Decimal(1).scaleb(-frac_digits)
                rounded = mantissa.quantize(quantum, rounding=ROUND_HALF_EVEN)
                _, digs, exp = rounded.as_tuple()
                digits = ''.join(map(str, digs)) or '0'
                decpt = len(digits) + exp
                int_part, frac_part = _split_digits(digits, decpt)
                if digits != '0' and decpt > max(k, 0):
                    # Rounding carried (e.g. 9.996 -> 10.0, or -- with k<=0,
                    # where the integer part must stay a literal "0" -- a
                    # mantissa like 0.99996 carrying to 1.0): the mantissa
                    # spilled past its digit budget, so shift one digit into
                    # the exponent and re-round at the new position.
                    printed_exponent += 1
                    continue
                break

    exp_sign = '-' if printed_exponent < 0 else '+'
    exp_digits = str(abs(printed_exponent))
    if e_digits is not None and len(exp_digits) < e_digits:
        exp_digits = '0' * (e_digits - len(exp_digits)) + exp_digits
    exponent_text = exponentchar + exp_sign + exp_digits

    sign = '-' if negative else ('+' if at_flag else '')

    if d is None and w is not None:
        natural_len = len(sign) + len(int_part) + 1 + len(frac_part) + len(exponent_text)
        if natural_len < w:
            frac_part = frac_part + '0' * (w - natural_len)

    body = sign + int_part + '.' + frac_part + exponent_text
    return _format_pad_or_overflow(body, w, overflowchar, padchar)


def _format_general_directive(val, params, colon_flag, at_flag):
    """`~w,d,e,k,overflowchar,padchar,exponentcharG` -- general floating
    point (CLHS 22.3.5): fixed-format when the magnitude's natural
    (shortest round-trip) scientific exponent falls in `[0, 7)` -- CLHS's
    example range for a value that would print reasonably in `~F` -- and
    exponential otherwise, each with the same `w`/`d`/`k`/... parameters
    `~F`/`~E` already implement. `w`'s exact exponent-field bookkeeping
    (CLHS's `ee`) is not implemented -- this covers `~G`'s choice of
    notation, not its column-exact width contract; no ansi-test file
    currently exercises `~G`."""
    num = _coerce_format_float(val)
    mag = abs(num)
    if mag == 0.0 or 0 <= _shortest_round_trip_digits(mag)[1] - 1 < 7:
        return _format_fixed_directive(val, params, at_flag)
    return _format_exponential_directive(val, params, at_flag)


_ENGLISH_ONES = [
    "zero", "one", "two", "three", "four", "five", "six", "seven", "eight",
    "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
    "sixteen", "seventeen", "eighteen", "nineteen",
]
_ENGLISH_TENS = [
    "", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy",
    "eighty", "ninety",
]
_ENGLISH_SCALES = [
    "", "thousand", "million", "billion", "trillion", "quadrillion",
    "quintillion", "sextillion", "septillion", "octillion", "nonillion",
    "decillion", "undecillion", "duodecillion", "tredecillion",
]


def _english_below_100(n):
    if n < 20:
        return _ENGLISH_ONES[n]
    tens, rem = divmod(n, 10)
    return _ENGLISH_TENS[tens] + ("-" + _ENGLISH_ONES[rem] if rem else "")


def _english_below_1000(n):
    hundreds, rem = divmod(n, 100)
    if hundreds == 0:
        return _english_below_100(rem)
    text = _ENGLISH_ONES[hundreds] + " hundred"
    return text + (" " + _english_below_100(rem) if rem else "")


def _english_cardinal(n):
    """Spell `n` out in English (CLHS 22.3.2's `~R` with no radix and
    neither modifier). Groups by thousands rather than a 0-100 lookup table
    so it is not silently wrong the moment a test uses 101."""
    if n == 0:
        return "zero"
    neg = n < 0
    n = abs(n)
    groups = []
    scale = 0
    while n > 0:
        n, grp = divmod(n, 1000)
        if grp:
            groups.append((grp, scale))
        scale += 1
    parts = []
    for grp, idx in reversed(groups):
        text = _english_below_1000(grp)
        if idx > 0:
            scale_name = (_ENGLISH_SCALES[idx] if idx < len(_ENGLISH_SCALES)
                          else "*1000^%d" % idx)
            text += " " + scale_name
        parts.append(text)
    return ("negative " if neg else "") + " ".join(parts)


_ENGLISH_ORDINAL_ONES = {
    "zero": "zeroth", "one": "first", "two": "second", "three": "third",
    "four": "fourth", "five": "fifth", "six": "sixth", "seven": "seventh",
    "eight": "eighth", "nine": "ninth", "ten": "tenth", "eleven": "eleventh",
    "twelve": "twelfth", "thirteen": "thirteenth", "fourteen": "fourteenth",
    "fifteen": "fifteenth", "sixteen": "sixteenth", "seventeen": "seventeenth",
    "eighteen": "eighteenth", "nineteen": "nineteenth",
}
_ENGLISH_ORDINAL_TENS = {
    "twenty": "twentieth", "thirty": "thirtieth", "forty": "fortieth",
    "fifty": "fiftieth", "sixty": "sixtieth", "seventy": "seventieth",
    "eighty": "eightieth", "ninety": "ninetieth",
}
_ENGLISH_ORDINAL_SCALES = {
    name: name + "th" for name in _ENGLISH_SCALES if name
}
_ENGLISH_ORDINAL_SCALES["hundred"] = "hundredth"


def _ordinal_word(word):
    if '-' in word:
        prefix, suffix = word.rsplit('-', 1)
        return prefix + '-' + _ENGLISH_ORDINAL_ONES.get(suffix, suffix)
    return (_ENGLISH_ORDINAL_TENS.get(word)
            or _ENGLISH_ORDINAL_SCALES.get(word)
            or _ENGLISH_ORDINAL_ONES.get(word, word))


def _english_ordinal(n):
    """`~:R`: only the *last* word of the cardinal spelling becomes ordinal
    (CLHS gives `one hundredth`, not `oneth hundredth`) -- so this builds on
    `_english_cardinal` rather than a second number-to-words implementation."""
    cardinal = _english_cardinal(n)
    prefix = ""
    if cardinal.startswith("negative "):
        prefix, cardinal = "negative ", cardinal[len("negative "):]
    words = cardinal.split(" ")
    words[-1] = _ordinal_word(words[-1])
    return prefix + " ".join(words)


_ROMAN_TABLE = [
    (1000, 'M'), (900, 'CM'), (500, 'D'), (400, 'CD'),
    (100, 'C'), (90, 'XC'), (50, 'L'), (40, 'XL'),
    (10, 'X'), (9, 'IX'), (5, 'V'), (4, 'IV'), (1, 'I'),
]
_OLD_ROMAN_TABLE = [
    (1000, 'M'), (500, 'D'), (100, 'C'), (50, 'L'), (10, 'X'), (5, 'V'), (1, 'I'),
]


def _roman_numeral(n, table):
    if n <= 0:
        return ''
    parts = []
    for value, sym in table:
        count, n = divmod(n, value)
        parts.append(sym * count)
    return ''.join(parts)


# === FORMAT's pretty-printing directives: ~<...~:>, ~_, ~I (CLHS 22.3.5) ===
#
# `~_`/`~I` and a `~<...~:>` logical block need to be resolved together,
# against a margin, only once the *whole* body between an opening and closing
# delimiter is known -- exactly the problem `_format_process_cursor` cannot
# see, since it emits text left-to-right as it goes. Rather than a second,
# parallel control-string walker (a duplicate of `_format_process_cursor`,
# standing rule 3), a conditional newline or indent directive is left behind
# as a sentinel *tag string* in the ordinary returned string, and a run of
# literal control-string spaces is bracketed the same way. Both kinds of
# marker ride unchanged through every existing consumer (`~(...~)`,
# `~[...~]`, `~{...~}`, `~<...~>` justification, `~?`) exactly like any
# other text, and are resolved -- or, if no `~<...~:>` ever claims them,
# stripped -- in exactly one place: `_resolve_pretty_body`, called from the
# `<` directive itself and once more at the true top level
# (`_format_process_with_tail`) for a bare `~_`/`~I` with no enclosing block.
#
# **These must not be single reserved codepoints.** A first version used one
# private-use-area character per marker, on the assumption that a PUA
# codepoint could never be *data*. `CHAR-CODE-LIMIT` here is 1114112 -- the
# full Unicode range, surrogates included (`(code-char 55296)` answers a
# character, not an error) -- so `FORMAT.C.1A`, which calls `~C` on every one
# of the first 65536 codepoints, walks straight through the reserved block
# and corrupted it: the character *was* the marker, so the top-level cleanup
# silently deleted it from the output. `PRINT.STRING.RANDOM.1` hit the same
# defect from a random codepoint. A long, specific multi-character tag has
# the same practical safety a canary string does elsewhere: unlike a single
# codepoint, no ansi-test literal or per-character/per-codepoint random test
# can produce it by coincidence, only a real `~_`/`~I`/logical-block-body
# path can ever write one.
_PP_BREAK = {
    'linear': '\x01\x02FCLPY:PPBREAK:LINEAR\x03',
    'fill': '\x01\x02FCLPY:PPBREAK:FILL\x03',
    'miser': '\x01\x02FCLPY:PPBREAK:MISER\x03',
    'mandatory': '\x01\x02FCLPY:PPBREAK:MANDATORY\x03',
}
_PP_BREAK_KIND = {v: k for k, v in _PP_BREAK.items()}
_PP_INDENT_OPEN = '\x01\x02FCLPY:PPINDENT:'
_PP_INDENT_CLOSE = '\x03'
_PP_INDENT_RE = re.compile(re.escape(_PP_INDENT_OPEN) + r'([BC])(-?\d+)' + re.escape(_PP_INDENT_CLOSE))
_PP_LIT_SPACE_OPEN = '\x01\x02FCLPY:PPSPACEOPEN\x03'
_PP_LIT_SPACE_CLOSE = '\x01\x02FCLPY:PPSPACECLOSE\x03'
_PP_LIT_SPACE_RUN_RE = re.compile(re.escape(_PP_LIT_SPACE_OPEN) + ' +' + re.escape(_PP_LIT_SPACE_CLOSE))
# Any not-yet-resolved break or indent-open tag (not the literal-space
# brackets, which need no margin/indent resolution -- just stripping).
_PP_ANY_BREAK_OR_INDENT_RE = re.compile(
    '|'.join(re.escape(tag) for tag in list(_PP_BREAK.values()) + [_PP_INDENT_OPEN]))
_PP_ANY_SENTINEL_RE = re.compile(
    '|'.join(re.escape(tag) for tag in list(_PP_BREAK.values())
             + [_PP_INDENT_OPEN, _PP_LIT_SPACE_OPEN, _PP_LIT_SPACE_CLOSE]))
# One combined pattern for `_pp_tokenize`: each break tag verbatim, or the
# indent tag with its payload captured.
_PP_TOKEN_RE = re.compile(
    '(?:' + '|'.join(re.escape(tag) for tag in _PP_BREAK.values()) + ')'
    + '|' + re.escape(_PP_INDENT_OPEN) + r'[BC]-?\d+' + re.escape(_PP_INDENT_CLOSE))


def _pp_indent_sentinel(relative_to, n):
    return _PP_INDENT_OPEN + relative_to + str(n) + _PP_INDENT_CLOSE


def _pp_strip_lit_space(text):
    """Remove the literal-space bracketing, leaving the spaces themselves."""
    return text.replace(_PP_LIT_SPACE_OPEN, '').replace(_PP_LIT_SPACE_CLOSE, '')


def _ansi_lower(text):
    """Convert text to lowercase using ANSI character semantics.

    Unlike Python's str.lower(), this preserves individual character boundaries:
    if a character's lowercase form would be multiple characters (e.g., ß -> ss,
    or U+0130 -> i with combining dot), the character is left unchanged instead.
    This matches CHAR-DOWNCASE behavior (CLHS 13.1.1).
    """
    result = []
    for char in text:
        lowered = char.lower()
        # Only apply lowercase if it's still a single character
        result.append(lowered if len(lowered) == 1 else char)
    return ''.join(result)


def _ansi_upper(text):
    """Convert text to uppercase using ANSI character semantics.

    Unlike Python's str.upper(), this preserves individual character boundaries:
    if a character's uppercase form would be multiple characters, the character
    is left unchanged instead. This matches CHAR-UPCASE behavior (CLHS 13.1.1).
    """
    result = []
    for char in text:
        uppered = char.upper()
        # Only apply uppercase if it's still a single character
        result.append(uppered if len(uppered) == 1 else char)
    return ''.join(result)


def _pp_case_convert(text, convert):
    """Apply `convert` to the *printable* parts of `text`, leaving any
    pretty-printer sentinel spans untouched.

    `~(...~)` case-converts whatever its body produced, and that body may
    contain unresolved sentinels -- `~<...~:>`'s literal-space brackets, break
    tags, indent tags. Those spans are ASCII text spelling things like
    `FCLPY:PPSPACEOPEN`, so converting them along with everything else
    *renamed the sentinels*: they then matched none of the resolution regexes
    and survived into the output, which is why `(format nil "~@(this is a
    TEST.~)")` answered
    `"Thisfclpy:ppspaceopen fclpy:ppspacecloseis..."` instead of
    `"This is a test."`.

    `convert` takes the concatenated printable text and returns it converted;
    it is called **once** on the whole of it, because the case directives are
    word-sensitive (`~@(` capitalizes the first word of the entire body) and a
    per-segment call would restart that logic at every sentinel. The converted
    text is then redistributed over the original segment boundaries, which is
    safe because every conversion here is length-preserving and per-character.
    """
    segments = _PP_ANY_SENTINEL_RE.split(text)
    if len(segments) == 1:
        return convert(text)
    separators = _PP_ANY_SENTINEL_RE.findall(text)
    converted = convert(''.join(segments))
    out, at = [], 0
    for index, segment in enumerate(segments):
        out.append(converted[at:at + len(segment)])
        at += len(segment)
        if index < len(separators):
            out.append(separators[index])
    return ''.join(out)


def _pp_visible_width(text):
    """Length of `text` as printed, ignoring any not-yet-resolved sentinels.

    Used only to estimate the column a `~<...~:>` starts at from `emitted`,
    the same best-effort, control-string-local column `~&` already uses
    (plan.md's recorded `~&`/FRESH-LINE gap) -- not a claim of a real,
    stream-wide column.
    """
    text = _PP_INDENT_RE.sub('', text)
    text = _PP_ANY_SENTINEL_RE.sub('', text)
    return len(text)


def _pp_tokenize(text):
    """Split resolved-argument text into literal runs, breaks and indents.

    Matches whole sentinel *tags*, not single characters -- see the module
    note above on why a single reserved codepoint was not safe.
    """
    tokens = []
    pos = 0
    for m in _PP_TOKEN_RE.finditer(text):
        if m.start() > pos:
            tokens.append(('text', text[pos:m.start()]))
        matched = m.group(0)
        if matched in _PP_BREAK_KIND:
            tokens.append(('break', _PP_BREAK_KIND[matched]))
        else:
            im = _PP_INDENT_RE.match(matched)
            tokens.append(('indent', 'block' if im.group(1) == 'B' else 'current',
                           int(im.group(2))))
        pos = m.end()
    if pos < len(text):
        tokens.append(('text', text[pos:]))
    return tokens


def _pp_flat_width(tokens):
    """Width of `tokens` if every break stayed a space (or nothing).

    `None` means "cannot be one line regardless of margin": a `:mandatory`
    break, or a nested block that already decided (during its own, earlier
    resolution) to break, leaving a real newline in one of its text tokens.
    """
    width = 0
    for kind, *rest in tokens:
        if kind == 'text':
            text = rest[0]
            if '\n' in text:
                return None
            width += len(text)
        elif kind == 'break' and rest[0] == 'mandatory':
            return None
    return width


def _pp_render(tokens, start_col, indent_baseline, right_margin, block_fits, miser_active):
    """Resolve `tokens`' breaks/indents into plain text (CLHS 22.2.1).

    `:linear`/`:miser` break all-or-none, decided once for the whole block
    from `block_fits`; `:fill` decides per break, from whether the material
    up to the *next* break fits; `:mandatory` always breaks. A firing break
    strips whitespace already queued since the last one -- the reason a
    literal space kept in the control string before `~_` does not survive
    into a broken line (`format.logical-block.18`'s "1\\n2\\n3", not "1 \\n2 ").
    """
    col = start_col
    indent = indent_baseline
    out = []

    def lookahead(idx):
        width = 0
        for kind, *rest in tokens[idx + 1:]:
            if kind == 'break':
                break
            if kind == 'text':
                text = rest[0]
                if '\n' in text:
                    width += len(text.split('\n', 1)[0])
                    break
                width += len(text)
        return width

    def rstrip_pending():
        while out:
            if out[-1] == '':
                out.pop()
                continue
            stripped = out[-1].rstrip(' ')
            if stripped == out[-1]:
                return
            out[-1] = stripped
            if not stripped:
                out.pop()
            return

    for idx, (kind, *rest) in enumerate(tokens):
        if kind == 'text':
            text = rest[0]
            if '\n' not in text:
                out.append(text)
                col += len(text)
                continue
            parts = text.split('\n')
            for j, part in enumerate(parts):
                if j > 0:
                    out.append('\n')
                out.append(part)
                col = len(part) if j == len(parts) - 1 else 0
        elif kind == 'indent':
            # No effect while the enclosing section is in miser mode (CLHS
            # 22.2.2's pprint-indent) -- `pprint-indent.22`'s :current/:block
            # calls are both ignored once miser mode is active, and every
            # line instead indents to the block's own start column, which is
            # exactly `indent`'s value before any indent token is ever seen.
            if not miser_active:
                relative_to, n = rest
                indent = (indent_baseline + n) if relative_to == 'block' else (col + n)
        else:  # break
            bkind = rest[0]
            if bkind == 'mandatory':
                fire = True
            elif right_margin is None:
                fire = False
            elif bkind == 'linear':
                fire = not block_fits
            elif bkind == 'miser':
                fire = miser_active and not block_fits
            elif miser_active:
                # CLHS 22.2.1.1: in miser mode, `:fill` also breaks like
                # `:linear` (all-or-none on the whole block) rather than at
                # its own per-chunk lookahead -- see the identical case in
                # `_pp_render_block`, `PPRINT-LOGICAL-BLOCK`'s own renderer.
                fire = not block_fits
            else:  # fill, outside miser mode
                fire = False if block_fits else (col + lookahead(idx)) > right_margin
            if fire:
                rstrip_pending()
                col = max(indent, 0)
                out.append('\n' + ' ' * col)
    return ''.join(out)


def _pp_tab_static_width(token):
    """An upper bound on the spaces a `'tab'` token will emit.

    `_compute_tab_size` answers exactly, but only given the column the tab
    runs at -- which the flat-width scan (`_pp_block_flat_width`) and the
    fill-break lookahead (`_pp_render_block`'s `lookahead`) both lack, since
    they walk the tokens without a running column. The bound is what those
    two static passes use: `colnum` is the furthest an absolute tab can
    move (and `colinc` the furthest it moves once past the stop), and a
    relative tab emits `colnum` spaces plus at most `colinc - 1` more.
    """
    _, kind, colnum, colinc = token
    if kind in ('line', 'section'):
        return max(colnum, colinc if colinc > 0 else 0)
    return max(colnum, 0) + max(colinc - 1, 0)


def _pp_block_flat_width(tokens):
    """Width of a `PPRINT-LOGICAL-BLOCK` token list if every break stayed
    unbroken -- `None` if that is impossible (a `:mandatory` break anywhere,
    including inside a nested `'block'` token, forces the *enclosing* block
    off one line too, since the nested one would still contain a real
    newline). A nested block's own prefix is not counted here: it was
    already written as an ordinary preceding `'text'` token in the same list
    (`pprint_logical_block_setup`), so it is already part of some earlier
    token's width.
    """
    width = 0
    for tok in tokens:
        kind = tok[0]
        if kind == 'text':
            if '\n' in tok[1]:
                return None
            width += len(tok[1])
        elif kind == 'break':
            if tok[1] == 'mandatory':
                return None
        elif kind == 'tab':
            width += _pp_tab_static_width(tok)
        elif kind == 'block':
            _, suffix_text, _per_line, subtokens = tok
            sub_flat = _pp_block_flat_width(subtokens)
            if sub_flat is None:
                return None
            width += sub_flat + len(suffix_text)
    return width


def _pp_render_block(tokens, start_col, indent_baseline, right_margin, miser_width,
                      block_fits, miser_active, suffix_len=0):
    """Render a `PPRINT-LOGICAL-BLOCK` token list (CLHS 22.2.1), recursing into
    any nested `'block'` token with the column this same left-to-right pass
    has *actually* reached by the time it gets there -- the enclosing
    block's own earlier breaks, decided by this same call, are already
    resolved into real text (or real absence of a break) before a nested
    block is ever reached, so its own fit-on-one-line and miser-mode
    determinations are exact, not a guess made before the enclosing block's
    breaks were known.
    """
    col = start_col
    indent = indent_baseline
    # The section origin for `:section`/`:section-relative` tabs (CLHS
    # 22.2.1.2): the column where this block's section began -- the block's
    # own start column. A nested block's render pass (the `'block'` case
    # below) gets its own, so tabs inside it measure from the nested start.
    section_start = start_col
    # Whether a newline has been emitted since the last break -- the fill
    # rule's "the preceding section was not printed on a single line".
    section_broken = False
    out = []

    def lookahead(idx):
        width = 0
        for tok in tokens[idx + 1:]:
            kind = tok[0]
            if kind == 'break':
                break
            if kind == 'text':
                text = tok[1]
                if '\n' in text:
                    width += len(text.split('\n', 1)[0])
                    break
                width += len(text)
            elif kind == 'tab':
                width += _pp_tab_static_width(tok)
            elif kind == 'block':
                sub_flat = _pp_block_flat_width(tok[3])
                if sub_flat is None:
                    break
                width += sub_flat + len(tok[1])
        else:
            # No further break: the section being measured runs to the end
            # of the block, whose suffix terminates it (CLHS 22.2.1.1's
            # section extends "to the end of the enclosing logical block").
            # Without the suffix, `pprint-tabular.21`'s last element counted
            # one character short and the final fill break failed to fire.
            width += suffix_len
        return width

    def rstrip_pending():
        while out:
            if out[-1] == '':
                out.pop()
                continue
            stripped = out[-1].rstrip(' ')
            if stripped == out[-1]:
                return
            out[-1] = stripped
            if not stripped:
                out.pop()
            return

    for idx, tok in enumerate(tokens):
        kind = tok[0]
        if kind == 'text':
            text = tok[1]
            if '\n' not in text:
                out.append(text)
                col += len(text)
                continue
            parts = text.split('\n')
            for j, part in enumerate(parts):
                if j > 0:
                    out.append('\n')
                out.append(part)
                col = len(part) if j == len(parts) - 1 else 0
            section_broken = True
        elif kind == 'indent':
            # No effect while the enclosing section is in miser mode (CLHS
            # 22.2.2's pprint-indent) -- see `_pp_render`'s identical guard.
            if not miser_active:
                _, relative_to, n = tok
                indent = (indent_baseline + n) if relative_to == 'block' else (col + n)
        elif kind == 'tab':
            # PPRINT-TAB (CLHS 22.2.1.2): real spaces at the real column.
            # The sectioned kinds measure from the block's own start, which
            # is `section_start` here -- for the first line of a block with
            # a `:prefix`, the prefix was already written by setup, so the
            # body start *is* the section start.
            _tag, tab_kind, tab_colnum, tab_colinc = tok
            pad = _compute_tab_size(tab_kind.upper(), tab_colnum, tab_colinc,
                                    col, section_start)
            if pad > 0:
                out.append(' ' * pad)
                col += pad
        elif kind == 'block':
            _, suffix_text, sub_per_line, subtokens = tok
            sub_flat = _pp_block_flat_width(subtokens)
            sub_fits = (right_margin is not None and sub_flat is not None
                        and col + sub_flat + len(suffix_text) <= right_margin)
            sub_miser = (right_margin is not None and miser_width is not None
                         and (right_margin - col) <= miser_width)
            sub_indent_baseline = 0 if sub_per_line is not None else col
            rendered_sub = _pp_render_block(subtokens, col, sub_indent_baseline,
                                             right_margin, miser_width, sub_fits, sub_miser,
                                             suffix_len=len(suffix_text))
            if sub_per_line is not None:
                rendered_sub = rendered_sub.replace('\n', '\n' + sub_per_line)
            combined = rendered_sub + suffix_text
            out.append(combined)
            nl = combined.rfind('\n')
            col = len(combined) - nl - 1 if nl != -1 else col + len(combined)
            if nl != -1:
                section_broken = True
        else:  # break
            bkind = tok[1]
            if bkind == 'mandatory':
                fire = True
            elif right_margin is None:
                fire = False
            elif bkind == 'linear':
                fire = not block_fits
            elif bkind == 'miser':
                fire = miser_active and not block_fits
            elif miser_active:
                # CLHS 22.2.1.1: in miser mode, `:fill` also breaks like
                # `:linear` (all-or-none on the whole block) rather than at
                # its own per-chunk lookahead -- `pprint-newline.fill.5` sets
                # margin=miser=10 on a block that cannot fit and requires
                # every element on its own line, not the every-5th-element
                # wrapping plain `:fill` lookahead would give.
                fire = not block_fits
            else:  # fill, outside miser mode
                # CLHS 22.2.1.1's fill rule is three-way: break if the
                # following section does not fit on the end of the current
                # line, OR if "the preceding section was not printed on a
                # single line" -- a fill break follows a section that itself
                # wrapped (`pprint-newline.fill.7`: the section holding the
                # eight-element fill block wrapped, so the very next break
                # fires even though "X" would fit). `section_broken` tracks
                # exactly that: any newline emitted since the last break.
                fire = ((section_broken or (col + lookahead(idx)) > right_margin)
                        if not block_fits else False)
            section_broken = False
            if fire:
                rstrip_pending()
                col = max(indent, 0)
                out.append('\n' + ' ' * col)
    return ''.join(out)


def _pp_render_top(tokens, body_col, per_line_text, suffix_len):
    """Entry point for resolving an *outermost* `PPRINT-LOGICAL-BLOCK`'s
    whole token tree (CLHS 22.2.1) -- reads the margin/miser-width/pretty
    controls once, for every block in the tree, then a single
    `_pp_render_block` pass resolves the outer block and every block nested
    in it together.
    """
    right_margin = _printer._as_count(_printer.resolve_control('*PRINT-RIGHT-MARGIN*'))
    miser_width = _printer._as_count(_printer.resolve_control('*PRINT-MISER-WIDTH*'))
    pretty = _printer._true(_printer.resolve_control('*PRINT-PRETTY*'))

    if pretty and right_margin is not None:
        flat = _pp_block_flat_width(tokens)
        block_fits = flat is not None and body_col + flat + suffix_len <= right_margin
        # CLHS 22.2.1.1: miser mode is in effect once the space available for
        # the *whole* logical block is at or below `*print-miser-width*`, not
        # only strictly below it -- `pprint-newline.miser.4` sets margin=10,
        # miser=10 on a block starting at column 0 (10-0 == 10) and requires
        # miser mode active; `.11`/`.12`'s 19-vs-18 pair pins the same
        # boundary from the other side.
        miser_active = miser_width is not None and (right_margin - body_col) <= miser_width
        rm = right_margin
    else:
        block_fits = True
        miser_active = False
        rm = None

    indent_baseline = 0 if per_line_text is not None else body_col
    rendered = _pp_render_block(tokens, body_col, indent_baseline, rm, miser_width,
                                 block_fits, miser_active, suffix_len=suffix_len)
    if per_line_text is not None:
        rendered = rendered.replace('\n', '\n' + per_line_text)
    return rendered


def _resolve_pretty_body(body_text, start_column, prefix_text, suffix_text,
                          per_line, auto_fill, allow_miser=True):
    """Render one logical block's body (CLHS 22.2/22.3.5.2) and wrap it.

    `auto_fill` is `~:@>`'s own effect: every run of literal control-string
    blanks directly in the body becomes a `:fill` conditional newline too,
    not just the explicit `~_`-family directives -- CLHS 22.3.5.2, "a
    fill-style conditional newline is automatically inserted after each
    group of blanks immediately contained in the body". Only *literal*
    blanks: the space characters bracketed by `_format_process_cursor`'s
    literal-run branch, never ones inside an argument's own printed text
    (`format.logical-block.26`'s `~A` of the string `"1 2 3"` must not wrap).

    `allow_miser=False` is `_format_process_with_tail`'s own case: a bare
    `~_`/`~I` with no enclosing `~<...~:>` is still resolved, against an
    *implicit* block spanning the whole control string (CLHS restricts
    these directives from appearing inside a plain `~<...~>` justification,
    not from needing a real logical block at all) -- but that implicit
    block has no real CLHS-specified start column of its own, so CLHS
    22.2.1.1's miser-mode determination, which is a property of an actual
    logical block, cannot apply to it: `format.@_.10` sets margin=miser=4 on
    exactly such a bare directive and requires it to stay flat, which real
    miser mode (correctly active for a real block at this same boundary --
    `pprint-newline.miser.4`) would not.
    """
    if auto_fill:
        body_text = _PP_LIT_SPACE_RUN_RE.sub(
            lambda m: m.group(0) + _PP_BREAK['fill'], body_text)
    body_text = _pp_strip_lit_space(body_text)

    right_margin = _printer._as_count(_printer.resolve_control('*PRINT-RIGHT-MARGIN*'))
    miser_width = _printer._as_count(_printer.resolve_control('*PRINT-MISER-WIDTH*'))
    pretty = _printer._true(_printer.resolve_control('*PRINT-PRETTY*'))

    tokens = _pp_tokenize(body_text)
    body_col = start_column + len(prefix_text)

    if pretty and right_margin is not None:
        flat = _pp_flat_width(tokens)
        block_fits = flat is not None and body_col + flat + len(suffix_text) <= right_margin
        # CLHS 22.2.1.1: miser mode is in effect once the space available for
        # the *whole* logical block is at or below `*print-miser-width*`, not
        # only strictly below it -- `pprint-newline.miser.4` sets margin=10,
        # miser=10 on a block starting at column 0 (10-0 == 10) and requires
        # miser mode active; `.11`/`.12`'s 19-vs-18 pair pins the same
        # boundary from the other side.
        miser_active = (allow_miser and miser_width is not None
                         and (right_margin - body_col) <= miser_width)
    else:
        # No margin (or *print-pretty* nil) to break against: only a
        # `:mandatory` break -- or a nested block that already broke -- is
        # honoured, which `_pp_render` does on its own once `block_fits` is
        # true and `right_margin` carries no fill/linear/miser decision.
        block_fits = True
        miser_active = False

    indent_baseline = 0 if per_line else body_col
    rendered = _pp_render(tokens, body_col, indent_baseline,
                           right_margin if pretty else None, block_fits, miser_active)

    if per_line:
        rendered = rendered.replace('\n', '\n' + prefix_text)
    return prefix_text + rendered + suffix_text


#: A backstop, not a spec-accurate answer -- see `_pp_bounded_list_elements`.
#: Deliberately much smaller than `printer.PRINT_BUDGET`: that one bounds
#: total aggregates across a whole recursive print, this one bounds a single
#: flat list that the `~{...~}`/`~@{...~}` consuming it re-slices per pass
#: (quadratic in the list length), so a six-figure cap would itself take
#: minutes on a genuinely circular argument even though it terminates.
_PP_LIST_BUDGET = 1_000


def _pp_bounded_list_elements(obj):
    """The elements of a proper-or-dotted Lisp list `obj`, for a `~<...~:>`
    logical block's local argument stream -- capped against a circular one.

    Unlike `PPRINT-POP`, which checks `*PRINT-LENGTH*` once per element as
    the body actually consumes them, `~<...~:>` decomposes its whole list
    argument up front (CLHS 22.3.5.2's "the argument ... becomes a list of
    arguments to be used"), before the body -- and *PRINT-LENGTH* -- ever
    run. Under `*PRINT-CIRCLE*` the block goes through
    `_format_logical_block_items` instead, whose walk terminates the way
    PPRINT-POP's does; this capped decomposition remains the circle-off
    path, where a genuinely circular argument would otherwise walk forever:
    `format.logical-block.circle.2`/`.3` hang the whole suite the same way
    plan.md's printer/DIRECTORY incidents did, not merely fail. Capped via
    `itertools.islice` over `list_cells` directly rather than `_pp_list`'s
    `seq_elements`/`_format_args_list`, which fully materialize before
    returning and so cannot be capped from the outside.
    """
    import itertools
    from .sequence_protocol import list_cells
    return [cell.car for cell in
            itertools.islice(list_cells(obj, 'FORMAT ~<...~:>', dotted='allow'), _PP_LIST_BUDGET)]


class _FormatDotText(str):
    """A pre-rendered ``. #n#`` / ``...`` fragment riding a `~<...~:>` block's
    item stream (see `_format_logical_block_items`).

    A `str` subclass so the FORMAT string machinery -- padding, case
    conversion -- keeps working on it, while the exact type lets the
    printing directives (~A/~S/~W) tell "user data" from "this fragment"
    and emit it verbatim instead of re-printing it as data.
    """


def _format_print_arg(val, escape):
    """~A's/~S's argument print, wrapped in the ambient *PRINT-CIRCLE*
    labelling an enclosing `~:<...~:>` logical block established (CLHS
    22.3.5.2: the block's items "are extracted from this list using
    pprint-pop", whose elements are checked for circularity and sharing as
    they are printed -- `format.logical-block.circle.1`'s `(x x)` prints
    `(#1=(0) #1#)`, the second ~A emitting only the `#1#` back-reference).

    Outside such a block the ambient state is `None` and this is exactly
    `printer.princ_to_string`/`prin1_to_string` -- the same call the
    handlers made before, so no format output changes when `*PRINT-CIRCLE*`
    is off. The body print deliberately stays on `printer.write_object`'s
    path: the ambient table owns the labelling for the whole operation, and
    a per-call `PrintContext` would run its own `_compute_circle_map` and
    number any internal sharing from 1, colliding with the ambient labels.
    """
    circle = _pp_circle_active()
    if circle is None or not _pp_is_circle_aggregate(val):
        if escape:
            return _printer.prin1_to_string(val)
        return _printer.princ_to_string(val)
    label, skip = _pp_circle_label(circle, val)
    if skip:
        return label
    if escape:
        body = _printer.prin1_to_string(val)
    else:
        body = _printer.princ_to_string(val)
    return label + body


def _format_circle_enter(obj):
    """Establish the ambient *PRINT-CIRCLE* labelling for a `~:<...~:>`
    logical block's data list (CLHS 22.3.5.2: the argument "is treated in
    the same way as the list argument to pprint-logical-block, thereby
    providing automatic support for non-list arguments and the detection of
    circularity, sharing, and depth abbreviation").

    Returns `(state, label_text, owned, skip)`. `state` is `None` when
    `*PRINT-CIRCLE*` is off or the argument is not an aggregate -- nothing
    to track, and the block keeps `_pp_bounded_list_elements`' capped
    decomposition. This is the pprint path's own `_pp_circle_enter`
    mechanism, not a second one: the same `_PP_CIRCLE_STATES` stack, the
    same recording pre-pass (`_pp_circle_visit_prepass`, which stops where
    the pprint-pop walk stops) and the same `_pp_circle_label` assignment,
    so label numbers stay consistent whether an object is reached through
    `WRITE` or through a `~A` in a block body. When a state is already
    active (a nested block, or a `~A` inside a real PPRINT-LOGICAL-BLOCK)
    it is *reused*; `owned` says whether this call pushed the entry that
    the `<` handler must pop in its `finally`.

    `label_text` is the `#n=`/`#n#` the block itself owes before its prefix
    (its list may be a shared or circular object), and `skip` is true when
    that object was *already printed* -- the whole block collapses to the
    `#n#` back-reference, the way `pprint_logical_block_setup`'s skip path
    does.
    """
    if not _printer._true(_printer.resolve_control('*PRINT-CIRCLE*')):
        return None, '', False, False
    if not _pp_is_circle_aggregate(obj):
        return None, '', False, False
    existing = _PP_CIRCLE_STATES[-1] if _PP_CIRCLE_STATES else None
    if existing is not None:
        label_text, skip = _pp_circle_label(existing, obj)
        return existing, label_text, False, skip
    if _consp_internal(obj):
        labels, visits = _pp_circle_visit_prepass(obj)
    else:
        labels, visits = _pp_circle_map_compute(obj), None
    # `depth` is pprint-stack based in `_pp_circle_enter`, whose trim pass
    # would misread it; a FORMAT block's entry lives exactly for the
    # handler's try/finally, so -1 keeps the trim from ever popping it.
    entry = {'map': labels, 'seen': set(), 'depth': -1, 'visits': visits}
    _PP_CIRCLE_STATES.append(entry)
    label_text, skip = _pp_circle_label(entry, obj)
    return entry, label_text, True, skip


def _format_logical_block_items(obj, state):
    """The item stream a `~:<...~:>` logical block's body consumes, walked
    the way PPRINT-POP walks PPRINT-LOGICAL-BLOCK's list (CLHS 22.3.5.2:
    elements "are extracted from this list using pprint-pop, thereby
    providing automatic support for malformed lists, and the detection of
    circularity, sharing, and length abbreviation").

    `state` is the ambient circle state `_format_circle_enter` pushed. Its
    `map` -- built by the recording pre-pass `_pp_circle_visit_prepass`,
    which stops exactly where this walk stops -- assigns every element or
    tail reached a second time its label number, in encounter order. The
    walk itself re-derives the *stop*: a position the walk touches twice
    means PPRINT-POP prints `" . #n#"` -- or `" . "` plus a fresh print of
    the tail when nothing has printed it yet (`pprint-pop.8`'s
    unprinted-cycle case) -- and exits, so the fragment rides the item
    stream as a `_FormatDotText` and the body's `~^` ends the iteration
    right after it. `*PRINT-LENGTH*`'s `...` truncates the same way, and a
    dotted atom tail renders as its dot.

    The `printed` set is the labels the walk will *have emitted* by the
    time it reaches a dot -- the block's own label (already in
    `state['seen']`) plus every labelled element extracted so far, since
    the body prints items in extraction order. PPRINT-POP consults the
    live `seen` instead, because it interleaves with the printing it
    observes; the reified walk runs before any of it.
    """
    labels = state['map']
    printed = set(state['seen'])
    reached = set()
    items = []
    position = obj
    count = 0
    length = _pprint_length()
    while True:
        if _null_internal(position):
            return items
        if not _consp_internal(position):
            return items + [_FormatDotText('. ' + _write_object(position))]
        if length is not None and count >= length:
            return items + [_FormatDotText('...')]
        if count > 0 and id(position) in reached:
            label = labels.get(id(position))
            if label is not None and label in printed:
                items.append(_FormatDotText(f'. #{label}#'))
            else:
                if label is not None:
                    printed.add(label)
                items.append(_FormatDotText('. ' + _write_object(position)))
            return items
        reached.add(id(position))
        element = position.car
        if _pp_is_circle_aggregate(element):
            elabel = labels.get(id(element))
            if elabel is not None:
                printed.add(elabel)
            reached.add(id(element))
        items.append(element)
        count += 1
        position = position.cdr


#: Depth of `~<...~:>` logical blocks whose bodies are currently being
#: processed. `~:T`/`~:@T` are PPRINT-TAB's section forms (CLHS 22.3.6.1):
#: they mean something only to a pretty-printing stream, and a `~<...~:>`
#: body is the one such context the FORMAT engine has -- outside one (or
#: with `*PRINT-PRETTY*` false) the directives do nothing. A counter, not a
#: boolean, because the bodies nest; `_format_directive` is not re-entrant
#: across threads, and an escape unwinds it through the `finally`.
_FORMAT_SECTION_DEPTH = 0


def _in_pretty_section():
    """True when a `~<...~:>` logical-block body is being processed and
    `*PRINT-PRETTY*` is currently true -- the only context in which the
    section forms of `~T` act (CLHS 22.3.6.1)."""
    if _FORMAT_SECTION_DEPTH <= 0:
        return False
    return _printer._true(_printer.resolve_control('*PRINT-PRETTY*'))


def _format_directive(control_string, cursor, pos, emitted=None):
    """Process a single format directive starting at pos (after ~).

    Consumes arguments from `cursor` (a _FormatCursor), mutating it in
    place. Returns (output_string, new_pos).

    `emitted` is the list of output chunks produced so far in this control
    string, which `~&` needs in order to know whether it is already at the
    start of a line.
    """
    if pos >= len(control_string):
        return ('~', pos)

    # Parse optional parameters: [prefix_params][:][@][directive]
    # Prefix params can be: number, 'char, V (next arg), #, or comma-separated
    colon_flag = False
    at_flag = False
    params = []
    
    # Skip optional numeric/char parameters and commas. `slot_has_value`
    # tracks whether the *current* parameter slot (since the last comma, or
    # since the start) has already had a value appended for it -- a comma is
    # an empty slot only when it isn't. This is not the same as peeking at
    # the raw previous character: a `'x` character parameter can itself
    # consume a comma as its literal value (`'~c` with `overflowchar` bound
    # to `#\,`, which `format.f.42` exercises), and the old check --
    # `control_string[pos-2] == ','` -- read that consumed comma as if it
    # were an empty-slot marker for the *next* slot, inserting a spurious
    # `None` and shifting every parameter after it by one.
    slot_has_value = False
    while pos < len(control_string):
        c = control_string[pos]
        if c.isdigit() or c == '-' or c == '+':
            # Parse number
            num_start = pos
            if c in '-+':
                pos += 1
            while pos < len(control_string) and control_string[pos].isdigit():
                pos += 1
            params.append(int(control_string[num_start:pos]))
            slot_has_value = True
        elif c == "'":
            # Character parameter 'X
            if pos + 1 < len(control_string):
                params.append(control_string[pos + 1])
                pos += 2
            else:
                pos += 1
            slot_has_value = True
        elif c == 'V' or c == 'v':
            # Use next argument as parameter
            params.append(cursor.next())
            pos += 1
            slot_has_value = True
        elif c == '#':
            # Number of remaining arguments
            params.append(cursor.remaining_count())
            pos += 1
            slot_has_value = True
        elif c == ',':
            pos += 1
            if not slot_has_value:
                params.append(None)
            slot_has_value = False
        elif c == ':':
            colon_flag = True
            pos += 1
        elif c == '@':
            at_flag = True
            pos += 1
        else:
            break

    if pos >= len(control_string):
        return ('~', pos)

    directive = control_string[pos].upper()
    pos += 1

    # Helper to get next arg
    def get_arg():
        return cursor.next()

    # Process directives
    if directive == 'A':
        # ~A - Aesthetic: print as PRINC does, i.e. with *PRINT-ESCAPE* nil.
        # `~:A` prints NIL as "()" rather than "NIL" (CLHS 22.3.4.1).
        val = get_arg()
        if isinstance(val, _FormatDotText):
            # A pre-rendered ". #n#"/"..." fragment from a ~<...~:> logical
            # block's pprint-pop walk -- printed verbatim, not re-printed
            # as data (see _format_logical_block_items).
            result = str(val)
        elif colon_flag and (val is None or val is lisptype.NIL):
            result = "()"
        else:
            # A function argument prints as the printer prints any other
            # function object (#<FUNCTION ...>), exactly as PRINC would.
            # The old "a callable is a format control" shortcut was not
            # CLHS: the function-as-control rule belongs to FORMAT's own
            # control-string designator and to `~{~}`'s empty body -- not
            # to ~A's argument -- and it crashed `(format nil "~a" #'cons)`
            # inside `format.b.18`'s mini-universe sweep.
            result = _format_print_arg(val, escape=False)
        return (_format_pad(result, params, at_flag), pos)

    elif directive == 'S':
        # ~S - Standard: print as PRIN1 does, i.e. with *PRINT-ESCAPE* true.
        val = get_arg()
        if isinstance(val, _FormatDotText):
            result = str(val)
        elif colon_flag and (val is None or val is lisptype.NIL):
            result = "()"
        else:
            result = _format_print_arg(val, escape=True)
        return (_format_pad(result, params, at_flag), pos)


    elif directive == 'W':
        # ~W - Write (CLHS 22.3.4.3): the argument is printed as by WRITE,
        # obeying every printer control variable. `:` binds *PRINT-PRETTY*
        # true and `@` binds *PRINT-LEVEL*/*PRINT-LENGTH* nil, for the
        # extent of the print; ~W takes no prefix parameters. Without the
        # directive, `~W` fell through to the unknown-directive fallback
        # and printed "~W" literally -- `format.:_.6` builds its whole
        # tabulation out of ~W's.
        val = get_arg()
        if isinstance(val, _FormatDotText):
            return (str(val), pos)
        from .binding import dynamic_value, set_dynamic_value
        bindings = []
        if colon_flag:
            bindings.append(('*PRINT-PRETTY*', lisptype.T))
        if at_flag:
            bindings.append(('*PRINT-LEVEL*', lisptype.NIL))
            bindings.append(('*PRINT-LENGTH*', lisptype.NIL))
        saved = []
        try:
            for name, value in bindings:
                symbol = lisptype.COMMON_LISP_PACKAGE.intern_symbol(name)
                saved.append((symbol, dynamic_value(symbol)))
                set_dynamic_value(symbol, value)
            result = _dispatch_print(val, {}, None)
        finally:
            for symbol, old in reversed(saved):
                set_dynamic_value(symbol, old)
        return (result, pos)

    elif directive == 'D':
        # ~D - Decimal integer (CLHS 22.3.2)
        return (_format_integer_directive(get_arg(), 10, params, colon_flag, at_flag), pos)

    elif directive == 'X':
        # ~X - Hexadecimal
        return (_format_integer_directive(get_arg(), 16, params, colon_flag, at_flag), pos)

    elif directive == 'O':
        # ~O - Octal
        return (_format_integer_directive(get_arg(), 8, params, colon_flag, at_flag), pos)

    elif directive == 'B':
        # ~B - Binary
        return (_format_integer_directive(get_arg(), 2, params, colon_flag, at_flag), pos)

    elif directive == 'R':
        # ~R - explicit radix (same mincol/padchar/commachar/comma-interval
        # tail as ~D/~X/~O/~B, radix is just an extra leading parameter) or,
        # with no parameters at all, English/Roman spelling per CLHS 22.3.2.
        if params and not _is_unspecified(params[0]):
            radix = _lisp_number(params[0], 10)
            val = get_arg()
            if isinstance(val, int):
                result = _format_integer_directive(val, radix, params[1:], colon_flag, at_flag)
            else:
                result = _format_A_fallback(val, radix)
        else:
            val = get_arg()
            if not isinstance(val, int):
                result = _format_A_fallback(val)
            elif at_flag and colon_flag:
                result = _roman_numeral(val, _OLD_ROMAN_TABLE) if val > 0 else ('-' + _roman_numeral(-val, _OLD_ROMAN_TABLE) if val < 0 else '')
            elif at_flag:
                result = _roman_numeral(val, _ROMAN_TABLE) if val > 0 else ('-' + _roman_numeral(-val, _ROMAN_TABLE) if val < 0 else '')
            elif colon_flag:
                result = _english_ordinal(val)
            else:
                result = _english_cardinal(val)
        return (result, pos)
    
    elif directive == 'C':
        # ~C - Character
        val = get_arg()
        if isinstance(val, lisptype.Character):
            if colon_flag:
                # ~:C prints the character "as it would be printed by PRINC
                # with its name spelled out" (CLHS 22.3.1.4) -- ansi-test
                # string-compares `(format nil "~:c" c)` against
                # `(char-name c)` for every non-graphic character
                # (format.c.4a/formatter.c.4a), so the name table lives in
                # one place, CHAR-NAME, and ~:C is its delegation. A
                # graphic character has no name and prints as itself;
                # CHAR-NAME answers "Space" for the space.
                from .characters import char_name
                result = char_name(val) or val.char
            elif at_flag:
                # Lisp readable form
                char_names = {' ': '#\\Space', '\n': '#\\Newline', '\t': '#\\Tab', '\r': '#\\Return'}
                result = char_names.get(val.char, '#\\' + val.char)
            else:
                result = val.char
        elif isinstance(val, str) and len(val) == 1:
            result = val
        else:
            result = str(val) if val else ''
        return (result, pos)
    
    elif directive == 'F':
        # ~F - Fixed-format floating point (CLHS 22.3.3)
        val = get_arg()
        try:
            result = _format_fixed_directive(val, params, at_flag)
        except (TypeError, ValueError):
            result = _format_A_fallback(val)
        return (result, pos)

    elif directive == 'E':
        # ~E - Exponential floating point (CLHS 22.3.4)
        val = get_arg()
        try:
            result = _format_exponential_directive(val, params, at_flag)
        except (TypeError, ValueError):
            result = _format_A_fallback(val)
        return (result, pos)

    elif directive == 'G':
        # ~G - General floating point (choose F or E, CLHS 22.3.5)
        val = get_arg()
        try:
            result = _format_general_directive(val, params, colon_flag, at_flag)
        except (TypeError, ValueError):
            result = _format_A_fallback(val)
        return (result, pos)
    
    elif directive == '%':
        # ~n% - n newlines (CLHS 22.3.1.1). `~0%` emits none.
        return ('\n' * _format_repeat_count(params), pos)

    elif directive == '&':
        # ~n& - a fresh line, then n-1 further newlines (CLHS 22.3.1.3).
        #
        # It used to emit n newlines unconditionally, with the comment "we
        # don't track column". The column within this control string is exactly
        # what has been emitted so far, so `~&` is a fresh line for the same
        # reason FRESH-LINE is: emit one only if the output does not already
        # end at a line boundary. `~0&` emits nothing at all.
        count = _format_repeat_count(params)
        if count <= 0:
            return ('', pos)
        preceding = ''.join(emitted) if emitted else ''
        needs_fresh_line = preceding != '' and not preceding.endswith('\n')
        return ('\n' * (count - 1 + int(needs_fresh_line)), pos)

    elif directive == '~':
        # ~n~ - n literal tildes (CLHS 22.3.1.5). `~0~` emits none.
        return ('~' * _format_repeat_count(params), pos)

    elif directive == '|':
        # ~n| - n page separators (CLHS 22.3.1.4). `~0|` emits none.
        return ('\f' * _format_repeat_count(params), pos)

    elif directive == 'T':
        # ~T - Tabulation (CLHS 22.3.6.1). `_tab_padding` owns the arithmetic;
        # the column comes from what this control string has emitted so far.
        # `~:T`/`~:@T` are PPRINT-TAB's :section forms, which are meaningful
        # only to a pretty-printing stream: outside a `~<...~:>` logical
        # block -- or with *PRINT-PRETTY* false -- they do nothing at all
        # (`format.:t.1`-`.3`, `format.:@t.4`/`.5`). Inside one they measure
        # from the start of the block's body, which is what the body-local
        # `_current_column` answers.
        if colon_flag and not _in_pretty_section():
            return ('', pos)
        return (_tab_padding(_current_column(emitted), params,
                             colon_flag, at_flag), pos)

    elif directive == '*':
        # ~* - Go to argument (CLHS 22.3.9.1). A `~V` parameter whose
        # argument is NIL means the parameter was not supplied (CLHS 22.3),
        # so the directive's own default applies -- 1 for the plain and
        # colon forms, 0 for the absolute `~@*` form. Reading an
        # unspecified parameter as a count made `~v*` attempt `idx + NIL`
        # and blow up as a Python TypeError (`format.*.5`).
        if at_flag:
            # Go to absolute argument position
            count = 0 if not params or _is_unspecified(params[0]) \
                else _lisp_number(params[0])
            cursor.idx = max(0, count)
        elif colon_flag:
            # Go backwards
            count = 1 if not params or _is_unspecified(params[0]) \
                else _lisp_number(params[0])
            cursor.idx = max(0, cursor.idx - count)
        else:
            # Go forwards
            count = 1 if not params or _is_unspecified(params[0]) \
                else _lisp_number(params[0])
            cursor.idx = min(len(cursor.args), cursor.idx + count)
        return ('', pos)

    elif directive == '?':
        # ~? - Recursive processing
        # The next arg is a format string, and the one after is args for it
        fmt_str = get_arg()
        # A `~^` inside the sub-format terminates the ~? construct itself,
        # not the control string that contains it (CLHS 22.3.9.2): "the
        # string being processed will be terminated ... Processing then
        # continues within the string containing the ~? directive at the
        # point following that directive". Letting the escape propagate
        # instead abandoned everything after the ~? as well
        # (`format.^.?.1`'s "1Y2X3" came out as "1Y2"). A `~:^` -- whose
        # only legal use is terminating a `~:{`/`~:@{` (X3J13
        # FORMAT-COLON-UPARROW-SCOPE) -- keeps propagating outward.
        try:
            if at_flag:
                # ~@? shares the outer argument stream: the recursive format
                # consumes from the same cursor, and only what it actually uses
                # is unavailable to directives that follow the ~? in the outer
                # control string.
                result = _format_process_cursor(str(fmt_str) if fmt_str else '', cursor)
            else:
                # ~? without @ takes its own separate argument list - not the
                # outer cursor - so it gets a fresh, independent cursor.
                fmt_args = get_arg()
                sub_cursor = _FormatCursor(_format_args_list(fmt_args))
                result = _format_process_cursor(str(fmt_str) if fmt_str else '', sub_cursor)
        except _FormatEscape as esc:
            if esc.terminate_outer:
                raise
            result = esc.partial
        return (result, pos)

    elif directive == '_':
        # ~_ - Conditional newline (CLHS 22.3.5.1), same four kinds as
        # PPRINT-NEWLINE: no flags linear, `:` fill, `@` miser, `:@` mandatory.
        # Resolved later, against a margin, by whichever `~<...~:>` encloses
        # this one -- or by `_format_process_with_tail` if none does -- since
        # only that point knows whether the surrounding material fits.
        # With *PRINT-PRETTY* false the whole family is a no-op: every kind
        # is a pretty-printing request, and even `~:@_`'s mandatory break
        # must not fire (`format.:@_.4`'s "A A A A " with :pretty nil).
        if not _printer._true(_printer.resolve_control('*PRINT-PRETTY*')):
            return ('', pos)
        if colon_flag and at_flag:
            kind = 'mandatory'
        elif colon_flag:
            kind = 'fill'
        elif at_flag:
            kind = 'miser'
        else:
            kind = 'linear'
        return (_PP_BREAK[kind], pos)

    elif directive == 'I':
        # ~I - Indent (CLHS 22.3.5.3): (pprint-indent :block n), or
        # (pprint-indent :current n) with the colon flag. Resolved alongside
        # `~_` by the enclosing block.
        n = params[0] if params and params[0] is not None else 0
        return (_pp_indent_sentinel('C' if colon_flag else 'B', _lisp_number(n)), pos)

    elif directive == '<':
        # ~<...~> is Justification (CLHS 22.3.6.2) if it ends in a plain
        # ~>, or a Logical Block (CLHS 22.3.5.2) if it ends in ~:>/~:@> --
        # the colon flag on the *closing* delimiter, not the opening one,
        # decides which of the two unrelated directives this is. One scan
        # serves both: it records each top-level ~;'s own colon/at flags
        # (a separator can be `~;`, `~:;` or `~@;`) and, separately, the
        # flags on whichever `~>` finally closes nesting back to 0, plus
        # whether any *nested* pair (closing before that) was itself a
        # colon-closed logical block -- CLHS forbids nesting one of those
        # inside a plain justification (`format.logical-block.error.25`).
        nesting = 1
        end_pos = pos
        segments = []
        sep_flags = []
        sep_params = []
        nested_logical_closers = []
        segment_start = pos
        closer_colon = False
        closer_at = False

        while end_pos < len(control_string) and nesting > 0:
            if control_string[end_pos] == '~' and end_pos + 1 < len(control_string):
                # `_scan_directive` rather than a local character-class skip:
                # it is the one place a directive's syntax is walked, and it
                # also yields the separator's own prefix parameters, which
                # `~n,m:;`'s line-width rule below needs.
                seg_params, seg_colon, seg_at, next_char, after = \
                    _scan_directive(control_string, end_pos)
                if next_char is not None:
                    if next_char == '<':
                        nesting += 1
                        end_pos = after
                    elif next_char == '>':
                        nesting -= 1
                        if nesting == 0:
                            segments.append(control_string[segment_start:end_pos])
                            closer_colon, closer_at = seg_colon, seg_at
                            end_pos = after
                            break
                        nested_logical_closers.append(seg_colon)
                        end_pos = after
                    elif next_char == ';' and nesting == 1:
                        segments.append(control_string[segment_start:end_pos])
                        sep_flags.append((seg_colon, seg_at))
                        sep_params.append(seg_params)
                        segment_start = after
                        end_pos = after
                    else:
                        end_pos = after
                else:
                    end_pos += 1
            else:
                end_pos += 1
        else:
            segments.append(control_string[segment_start:])
            end_pos = len(control_string)

        if not closer_colon:
            # Justification (CLHS 22.3.6.2). A logical block cannot appear
            # nested inside it.
            if any(nested_logical_closers):
                raise lisptype.LispProgramError(
                    "FORMAT: ~<...~:> (logical block) cannot be nested "
                    "inside ~<...~> (justification)")

            # Every segment is processed -- including a `~:;`-terminated first
            # one, which CLHS 22.3.6.2 is explicit about: "the first clause is
            # always processed, and so any arguments it refers to will be used;
            # the decision is whether to use the resulting segment of text".
            # All segments share the outer cursor, so arguments consumed inside
            # the block are not re-offered to directives after the `~>`.
            # Literal-space brackets are stripped here rather than left for the
            # top level: this branch resolves against no margin, so `_justify`'s
            # width arithmetic must see the real character count.
            texts = []
            escaped = False
            for seg in segments:
                try:
                    texts.append(_pp_strip_lit_space(_format_process_cursor(seg, cursor)))
                except _FormatEscape:
                    # CLHS 22.3.6.2: a `~^` inside a segment terminates the
                    # whole justification, and the segment it appears in is
                    # *discarded* along with every later one -- only segments
                    # completed before it are laid out. Keeping the partial
                    # text made `~<XXXXXX~^~>` answer "XXXXXX" where it must
                    # answer "", and `~6<abc~;def~^~>` answer "abcdef" where
                    # it must justify "abc" alone into six columns.
                    escaped = True
                    break

            # A first segment terminated by `~:;` is not content: it is the
            # text emitted *only* when the padded result will not fit on the
            # current line. `~n,m:;` gives n = columns that must still be
            # spare and m = a line width overriding `*PRINT-RIGHT-MARGIN*`.
            # This used to be unconditionally discarded, with the note "there
            # is no line-width model for plain justification" -- but the
            # column is knowable from what FORMAT has already emitted, which
            # is what `_current_column` answers, and the width is a parameter
            # of the directive itself.
            overflow_prefix = None
            if (not escaped and len(segments) > 1
                    and sep_flags and sep_flags[0][0] and texts):
                overflow_prefix = texts[0]
                texts = texts[1:]
                first_params = sep_params[0] if sep_params else []
                spare = first_params[0] if first_params and isinstance(
                    first_params[0], int) else 0
                if len(first_params) > 1 and isinstance(first_params[1], int):
                    line_width = first_params[1]
                else:
                    line_width = _printer._as_count(
                        _printer.resolve_control('*PRINT-RIGHT-MARGIN*'))

            padded = _justify(texts, params, colon_flag, at_flag)
            if overflow_prefix is not None and line_width is not None:
                column = _current_column(emitted)
                if column + len(padded) + spare > line_width:
                    return (overflow_prefix + padded, end_pos)
            return (padded, end_pos)

        # Logical block (CLHS 22.3.5.2). The body is split by top-level ~;
        # into at most three sections: prefix ; body ; suffix. Two sections
        # is prefix + body (suffix defaults); one section is just body
        # (prefix and suffix both default). A first section is a per-line
        # prefix, re-output after every line break the body causes, rather
        # than a one-shot prefix, when its own separator carried `@`
        # (`~@;`) -- `format.logical-block.27`'s "**" before every line.
        n_sections = len(segments)
        if n_sections == 1:
            prefix_src, body_src, suffix_src, per_line = None, segments[0], None, False
        elif n_sections == 2:
            prefix_src, body_src, suffix_src = segments[0], segments[1], None
            per_line = sep_flags[0][1]
        else:
            prefix_src, suffix_src = segments[0], segments[-1]
            body_src = '~;'.join(segments[1:-1])
            per_line = sep_flags[0][1]

        def _check_constant_section(section, label):
            if section is not None and '~' in section:
                raise lisptype.LispProgramError(
                    f"FORMAT: the {label} of ~<...~:> must be a constant "
                    f"string, not {section!r}")

        _check_constant_section(prefix_src, 'prefix')
        _check_constant_section(suffix_src, 'suffix')

        # The colon flag on the *opening* ~< supplies "(" / ")" as the
        # prefix/suffix defaults (only when no explicit section overrode
        # them); the at flag decides whether the object is the next single
        # argument or the rest of them (CLHS 22.3.5.2).
        prefix_text = prefix_src if prefix_src is not None else ('(' if colon_flag else '')
        suffix_text = suffix_src if suffix_src is not None else (')' if colon_flag else '')

        # CLHS 22.3.5.2: circularity detection is applied to the block's
        # *data list* -- the `~:<` argument -- and explicitly not to the
        # format argument list a top-level `~@<...~:>` receives, so these
        # stay unset on the at-flag path below.
        circle_state = None
        block_label = ''
        circle_owned = False
        block_skip = False

        if at_flag:
            items = cursor.remaining()
            cursor.idx = len(cursor.args)
        else:
            obj = get_arg()
            if not _listp_internal(obj):
                # A non-list object is printed as if by WRITE, with the
                # prefix, suffix and body all skipped entirely (CLHS
                # 22.3.5.2 / pprint-logical-block's "atom" case --
                # `format.logical-block.8`).
                escape = _printer._true(_printer.resolve_control('*PRINT-ESCAPE*'))
                return (_write_object(obj, escape=escape), end_pos)
            # The block's list argument gets the same circularity/sharing
            # detection as PPRINT-LOGICAL-BLOCK's ("treated in the same way
            # as the list argument to pprint-logical-block"). Under
            # *PRINT-CIRCLE* the walk is PPRINT-POP's, the block's own
            # label prefixes its output, and the ambient labelling table
            # lives for exactly this body -- `~A`/`~S`/`~W` inside it
            # consult it through `_dispatch_print`
            # (`format.logical-block.circle.1`-`.3`).
            circle_state, block_label, circle_owned, block_skip = \
                _format_circle_enter(obj)
            if circle_state is not None and not block_skip:
                items = _format_logical_block_items(obj, circle_state)
            else:
                items = _pp_bounded_list_elements(obj)

        sub_cursor = _FormatCursor(items)
        global _FORMAT_SECTION_DEPTH
        _FORMAT_SECTION_DEPTH += 1
        try:
            if block_skip:
                # The block's object was already printed: only the "#n#"
                # back-reference is output, and the body -- the CLHS
                # "first, output-suppressed pass" the pprint path models
                # too -- runs against the throwaway cursor and is
                # discarded.
                try:
                    _format_process_cursor(body_src, sub_cursor)
                except _FormatEscape:
                    pass
                return (block_label, end_pos)
            body_text = _format_process_cursor(body_src, sub_cursor)
        except _FormatEscape as esc:
            # Within the body, ~^ acts like PPRINT-EXIT-IF-LIST-EXHAUSTED:
            # it ends the body, not the whole enclosing control string
            # (`format.logical-block.escape.1`/`.2`).
            body_text = esc.partial
        finally:
            _FORMAT_SECTION_DEPTH -= 1
            if circle_owned:
                # The entry this handler pushed lives exactly for the
                # body -- the pprint path's `flush_pprint_frame` pops its
                # own; a leftover depth=-1 entry here would label every
                # later print against a table whose walk is long over.
                _PP_CIRCLE_STATES.pop()

        # ~:@> -- CLHS 22.3.5.2: "a fill-style conditional newline is
        # automatically inserted after each group of blanks immediately
        # contained in the body", on top of whatever `~_`-family directives
        # the body already spelled out explicitly.
        auto_fill = closer_colon and closer_at
        preceding = ''.join(emitted) if emitted else ''
        start_column = _pp_visible_width(preceding.rsplit('\n', 1)[-1])
        # The block's own `#n=` goes before everything, prefix included --
        # the same order `pprint_logical_block_setup` emits it in.
        return (block_label + _resolve_pretty_body(body_text, start_column,
                                                   prefix_text,
                                                   suffix_text, per_line,
                                                   auto_fill),
                end_pos)

    elif directive == '>':
        # End of justification - should not be reached directly
        return ('', pos)

    elif directive == '(':
        # ~( ... ~) - Case conversion
        # Find matching ~)
        nesting = 1
        end_pos = pos
        while end_pos < len(control_string) and nesting > 0:
            if control_string[end_pos] == '~':
                if end_pos + 1 < len(control_string):
                    # Skip any modifiers to find directive char
                    j = end_pos + 1
                    while j < len(control_string) and control_string[j] in '0123456789,:#@':
                        j += 1
                    if j < len(control_string):
                        next_char = control_string[j].upper()
                        if next_char == '(':
                            nesting += 1
                            end_pos = j + 1
                        elif next_char == ')':
                            nesting -= 1
                            if nesting == 0:
                                # end_pos points to ~, j points to )
                                inner = control_string[pos:end_pos]
                                end_pos = j + 1  # Position after the closing )
                                break
                            end_pos = j + 1
                        else:
                            end_pos += 1
                    else:
                        end_pos += 1
                else:
                    end_pos += 1
            else:
                end_pos += 1
        else:
            # If we exited the loop without finding closing ~)
            inner = control_string[pos:]
        
        # Shares the outer cursor: consumption is now exact (the cursor
        # tracks it directly), replacing the old inner.count('~') estimate.
        # Every variant converts through `_pp_case_convert`, which keeps its
        # hands off any unresolved pretty-printer sentinel in the body -- see
        # that function for what went wrong when they were converted too.
        if colon_flag and at_flag:
            # ~:@( ... ~) - force everything to upper case
            convert = _ansi_upper
        elif colon_flag:
            # ~:( ... ~) - capitalize each word
            convert = _capitalize_words
        elif at_flag:
            # ~@( ... ~) - capitalize just the first word, lower case the rest
            convert = _capitalize_first_word
        else:
            # ~( ... ~) - force everything to lower case
            convert = _ansi_lower

        try:
            inner_result = _format_process_cursor(inner, cursor)
        except _FormatEscape as esc:
            # CLHS 22.3.9.2: a `~^` inside a `~(` terminates the case
            # conversion, but "all the commands up to the ~^ are properly
            # ... case-converted" first, and "the outward search continues
            # for a ~{ or ~< construct to be terminated" -- so the partial
            # text is converted and the escape re-raised
            # (`format.^.:(.1`'s "Xy" is "XY" converted, with the `~{`
            # iteration stopping rather than continuing).
            raise _FormatEscape(_pp_case_convert(esc.partial, convert),
                                terminate_outer=esc.terminate_outer) from esc

        return (_pp_case_convert(inner_result, convert), end_pos)
    
    elif directive == ')':
        # End of case conversion - should not be reached directly
        return ('', pos)
    
    elif directive == '[':
        # ~[ ... ~] - Conditional
        # Find the clauses and closing ~]
        nesting = 1
        angle_nesting = 0  # Track ~< ~> nesting
        paren_nesting = 0  # Track ~( ~) nesting
        brace_nesting = 0  # Track ~{ ~} nesting
        clauses = []
        clause_start = pos
        i = pos
        default_clause = None
        end_pos = pos
        
        while i < len(control_string) and nesting > 0:
            if control_string[i] == '~':
                if i + 1 < len(control_string):
                    # Skip params to find directive
                    j = i + 1
                    while j < len(control_string) and control_string[j] in '0123456789,:#@':
                        j += 1
                    if j < len(control_string):
                        d = control_string[j].upper()
                        if d == '[':
                            nesting += 1
                            i = j + 1
                        elif d == ']':
                            nesting -= 1
                            if nesting == 0:
                                clauses.append(control_string[clause_start:i])
                                end_pos = j + 1  # Position after ]
                            i = j + 1
                        elif d == '<':
                            angle_nesting += 1
                            i = j + 1
                        elif d == '>':
                            angle_nesting -= 1
                            i = j + 1
                        elif d == '(':
                            paren_nesting += 1
                            i = j + 1
                        elif d == ')':
                            paren_nesting -= 1
                            i = j + 1
                        elif d == '{':
                            brace_nesting += 1
                            i = j + 1
                        elif d == '}':
                            brace_nesting -= 1
                            i = j + 1
                        elif d == ';' and nesting == 1 and angle_nesting == 0 and paren_nesting == 0 and brace_nesting == 0:
                            # Only treat as clause separator if we're not inside nested ~< ~> or ~( ~) or ~{ ~}
                            clauses.append(control_string[clause_start:i])
                            # Check for :; (default clause)
                            if ':' in control_string[i+1:j+1]:
                                default_clause = len(clauses)
                            clause_start = j + 1
                            i = j + 1
                        else:
                            i += 1
                    else:
                        i += 1
                else:
                    i += 1
            else:
                i += 1
        
        if end_pos == pos:
            end_pos = i  # Fallback if we didn't find proper closing
        
        # All branches below share the outer cursor: whatever a clause
        # consumes must be visible to directives that follow the ~[...~].
        if colon_flag:
            # ~:[ test ~; else ~]
            val = get_arg()
            # T is truthy, NIL/False/None are falsy
            is_true = val is not None and val is not lisptype.NIL and val is not False
            # Also check for T symbol
            if val is lisptype.T:
                is_true = True
            if is_true:
                result = _format_process_cursor(clauses[1] if len(clauses) > 1 else '', cursor)
            else:
                result = _format_process_cursor(clauses[0] if clauses else '', cursor)
        elif at_flag:
            # ~@[ test ~] - if arg is non-nil, process with arg, else skip
            val = get_arg()
            if val is not None and val is not lisptype.NIL and val is not False:
                # Put the value back; the clause consumes it itself.
                cursor.idx -= 1
                result = _format_process_cursor(clauses[0] if clauses else '', cursor)
            else:
                result = ''
        else:
            # ~[ clause0 ~; clause1 ~; ... ~] - select by index.
            # CLHS 22.3.7.2: `~n[` (and so `~#[`, where the parameter is the
            # count of remaining arguments) takes the index from the prefix
            # parameter and consumes *no* argument. Unconditionally calling
            # get_arg() here both selected the wrong clause and stole an
            # argument from whatever followed. A `~V` parameter whose
            # argument is NIL counts as "not supplied" (CLHS 22.3), so the
            # index then comes from the argument list after all
            # (`format.cond.14`'s `~v[...]` with NIL in the V slot).
            if params and not _is_unspecified(params[0]):
                idx = _lisp_number(params[0], -1)
            else:
                idx = _lisp_number(get_arg(), -1)
            if 0 <= idx < len(clauses):
                result = _format_process_cursor(clauses[idx], cursor)
            elif default_clause is not None and default_clause < len(clauses):
                result = _format_process_cursor(clauses[default_clause], cursor)
            else:
                result = ''

        return (result, end_pos)
    
    elif directive == ']':
        return ('', pos)
    
    elif directive == '{':
        # ~{ ... ~} - Iteration
        # Find matching ~} (or ~:}) taking nesting into account. The
        # colon form of the closing delimiter -- `~:}` -- is shorthand
        # for "the body executes at least once even with an empty list"
        # (CLHS 22.3.7.4). `format.{.23`/`.29`/`.30` exercise this: a
        # `~{FOO~:}` with NIL as the list still produces "FOO". Treat
        # the `~:` as part of the close delimiter; set a flag and step
        # past the colon to keep the inner-text scan correct.
        nesting = 1
        i = pos
        end_inner = pos
        end_pos = pos
        close_colon = False
        while i < len(control_string) and nesting > 0:
            if control_string[i] == '~' and i + 1 < len(control_string):
                ch = control_string[i+1]
                if ch == '{':
                    nesting += 1
                    i += 2
                    continue
                elif ch == ':' and i + 2 < len(control_string) and control_string[i+2] == '}':
                    # The colon form of the close delimiter; the
                    # `~:` colon-flag here is a *no-print* marker (the
                    # body must run once even with NIL), not a directive
                    # flag on the iteration. Step past all three chars
                    # so the inner-text scan ends cleanly.
                    nesting -= 1
                    if nesting == 0:
                        end_inner = i
                        end_pos = i + 3
                        close_colon = True
                        break
                    i += 3
                    continue
                elif ch == '}':
                    nesting -= 1
                    if nesting == 0:
                        end_inner = i
                        end_pos = i + 2  # position after ~}
                        break
                    i += 2
                    continue
            i += 1

        # Fallback if no proper closing found
        if nesting == 0:
            inner = control_string[pos:end_inner]
        else:
            inner = control_string[pos:i]
            end_pos = i
            close_colon = False

        # CLHS 22.3.7.4: an empty body means the next argument is the
        # format control for each pass, taken *before* the list argument.
        # The argument may be a string (the common case, `format.{.15`/
        # /.16), a LispString, or a function (the result of `FORMATTER`,
        # `format.{.17`/.18/.19/.20). All three are accepted: a string
        # control is recursed into per item; a function is *called* per
        # item (so `(formatter "~A")` prints each list element -- the
        # test in `format.{.19` expects "1234" for `(1 2 3 4)`); a
        # function returning "" (the `format.{.17` case) produces "".
        if not inner:
            inner_arg = get_arg()
            if isinstance(inner_arg, (str, lisptype.LispString)):
                inner = '' if inner_arg is None else str(inner_arg)
            elif callable(inner_arg) and not isinstance(inner_arg, (int, float, bool, lisptype.LispSymbol, lisptype.Character)):
                # The inner is a function. Per CLHS 22.3.4.1, a function
                # is a valid format control and is called per pass with
                # `(stream, &rest args)` where `args` is each list item --
                # same convention as ~A/~S treating a function arg as
                # a recursive format. The simplest implementation is to
                # keep `inner_arg` as the function and let the per-pass
                # loop call it directly; mark that with a sentinel
                # attribute on the local closure so the iteration loop
                # below can tell string from function without an extra
                # `if isinstance(...)` test on every iteration.
                inner = ('__function__', inner_arg)
            else:
                inner = ''

        if at_flag:
            # ~@{...~} - use the rest of the outer arguments as the items,
            # directly from the outer cursor: they belong to the same
            # argument stream, not a separate list argument (CLHS 22.3.7.3).
            # Whatever the iteration below does *not* consume -- because a
            # `~n@{`, an internal `~^`, or the arguments simply not dividing
            # evenly among passes stops it early -- must stay visible to
            # directives that follow `~@{...~}` in the outer control string,
            # so the outer cursor is only advanced by `outer_consumed`,
            # computed after the loop runs, not slurped up front.
            items = cursor.remaining()
            if at_flag and not inner:
                # `~v@{~}` with empty body -- the format-control arg has
                # already been consumed by `get_arg()` above, and the
                # remaining items are now everything *after* that control.
                # The capture-must-happen-here rule: take a snapshot of
                # the cursor right now, then run the loop with that
                # snapshot so the per-pass cursor manipulation does not
                # see the format-control arg in items.
                pass
        else:
            # ~{...~} / ~:{...~} - the next argument is the list of items,
            # a scope of its own (per CLHS 22.3.7): only the single "list"
            # argument itself (already taken by get_arg()) is removed from
            # the outer cursor; what the iteration body does with its
            # elements never touches the outer cursor further.
            list_arg = get_arg()
            # CLHS 22.3.7: the list arg must be a proper list. A non-list
            # (or a dotted list) is a type-error -- `format.{.error.1`-`.5`
            # and `format.:{.error.1`-`.5` exercise this. `_format_args_list`
            # itself silently wraps a non-list in `[value]`, so the type
            # check has to happen *here*, before that coercion. (The `~@{`
            # form takes its items from the outer cursor, not a separate
            # list, so it does not need this check.)
            if list_arg is not None and list_arg is not lisptype.NIL:
                from .core import _consp_internal
                # A string is a *vector*, not a list, and does not qualify
                # (`format.{.error.3`'s "foo" must signal a type error, not
                # iterate once over the string as a single "argument").
                if not _consp_internal(list_arg) and not isinstance(list_arg, (list, tuple)):
                    raise lisptype.LispTypeError(
                        f"FORMAT: ~{{...~}} argument is not a list: "
                        f"{_write_object(list_arg, escape=True)}",
                        expected_type='LIST', actual_value=list_arg)
            items = _format_args_list(list_arg)

        # ~n{...~} bounds the number of iterations (CLHS 22.3.7.4); without
        # a parameter the only bound is the argument list running out.
        # A `~V`-sourced parameter that comes back as NIL is the same as
        # "no parameter" -- the directive defaults to unbounded -- so
        # treat both `None` and Lisp `NIL` as "no max" rather than as
        # the integer 0 (which would forbid any iteration at all).
        _mi = params[0] if params and len(params) > 0 else None
        if _mi is None or _mi is lisptype.NIL or _mi is False:
            max_iterations = None
        else:
            max_iterations = _mi

        result_parts = []
        iterations = 0

        if colon_flag:
            # ~:{...~} / ~:@{...~} - each item is itself a list, and one pass
            # is made per item with that sublist standing in as the whole
            # argument stream. Each item must be a (proper) list -- CLHS
            # 22.3.7.2 -- so a symbol, number, string or vector as an item is
            # a TYPE-ERROR, not a one-element pass (`format.:{.error.1`/
            # `.4`/`.5`, `format.:@{.error.1`-`.4`). Checked per item as the
            # loop reaches it, not up front: `~v:@{`'s max-iterations bound
            # may stop the iteration before the first non-list
            # (`formatter.:@.10`'s trailing 'a 'b must survive).
            last_index = len(items) - 1
            for item_index, item in enumerate(items):
                if max_iterations is not None and iterations >= max_iterations:
                    break
                if at_flag:
                    _check_format_sublist(item, '~:@{...~}')
                else:
                    _check_format_sublist(item, '~:{...~}')
                iterations += 1
                if isinstance(inner, tuple) and inner and inner[0] == '__function__':
                    # Empty body with a function (FORMATTER) control. Each
                    # item is a sublist whose elements are the per-pass
                    # args -- call the function with that sublist. The
                    # function's return value (a function's return) is
                    # the iteration's text.
                    from .streams import make_string_output_stream as _make_sos
                    from .streams import get_output_stream_string as _get_oss
                    capture = _make_sos()
                    sub_args = _format_args_list(item)
                    inner[1](capture, *sub_args)
                    # `_get_oss` returns a `LispString`, not a Python str;
                    # `''.join` later (or the caller) expects a str, so
                    # convert explicitly. The `str()` honours the
                    # fill-pointer of any backing buffer (CLHS 22.1.3.4
                    # is irrelevant here, but a buffer with content
                    # past the fill pointer would otherwise leak in).
                    result_parts.append(str(_get_oss(capture)))
                else:
                    sub_cursor = _FormatCursor(_format_args_list(item),
                                               last_item=(item_index == last_index))
                    try:
                        result_parts.append(_format_process_cursor(inner, sub_cursor))
                    except _FormatEscape as esc:
                        # CLHS 22.3.9.2: inside ~:{...~}, a plain ~^ ends only
                        # the current sublist's pass, while ~:^ ends the whole
                        # iteration. Treating both as "stop everything" silently
                        # dropped every sublist after the first short one.
                        result_parts.append(esc.partial)
                        if esc.terminate_outer:
                            break
                        continue
            # Every pass counted in `iterations` took exactly one item off
            # the outer stream, whether it ran to completion or a `^`
            # ended it partway through.
            outer_consumed = iterations
        else:
            # One pass at a time over what is left; each pass gets a fresh
            # cursor, and however much it consumed is where the next starts.
            # The colon-form of the close delimiter (`~:}`) is shorthand
            # for "the body executes at least once even with an empty
            # list" (CLHS 22.3.7.4), so seed the iteration with one
            # empty-arg pass if `items` is empty.
            item_list = list(items)
            if close_colon and not item_list:
                item_list = [lisptype.NIL]
            if at_flag and isinstance(inner, tuple) and inner and inner[0] == '__function__':
                # ~@{~} with a function control: the control was consumed
                # once, above, and now the SAME function is called per pass
                # with (stream &rest args) over the *shared* outer argument
                # stream. A function control reports what it consumed by its
                # return value -- the tail of the arguments it was handed
                # (CLHS FORMATTER: "returns any remaining arguments") -- so
                # the cursor advances by exactly what it used, and whatever
                # it left is still visible to directives after the ~}
                # (`formatter.@{.13` requires 'foo to survive one pass of a
                # control that ignores its arguments). An iteration whose
                # body consumes nothing terminates (CLHS 22.3.7.3), which
                # also bounds the loop.
                from .streams import make_string_output_stream as _make_sos
                from .streams import get_output_stream_string as _get_oss
                while not (max_iterations is not None
                           and iterations >= max_iterations):
                    if not item_list:
                        break
                    iterations += 1
                    capture = _make_sos()
                    rest = cursor.remaining()
                    tail = inner[1](capture, *rest)
                    consumed = len(rest) - len(tail)
                    result_parts.append(str(_get_oss(capture)))
                    if consumed <= 0:
                        break
                    cursor.idx += consumed
                    item_list = item_list[consumed:]
                outer_consumed = len(items) - len(item_list)
                cursor.idx += outer_consumed
                return (''.join(result_parts), end_pos)
            while item_list:
                if max_iterations is not None and iterations >= max_iterations:
                    break
                iterations += 1
                if isinstance(inner, tuple) and inner and inner[0] == '__function__':
                    # Empty body with a function control: per-pass, the
                    # current item is a *single* value (not a sublist), and
                    # the function is called with `(stream, item)` -- the
                    # list-element-as-arg convention `~A`/`~S` already use
                    # for a function arg (CLHS 22.3.4.1).
                    from .streams import make_string_output_stream as _make_sos
                    from .streams import get_output_stream_string as _get_oss
                    capture = _make_sos()
                    inner[1](capture, item_list[0])
                    result_parts.append(str(_get_oss(capture)))
                    item_list = item_list[1:]
                    if max_iterations is not None and iterations >= max_iterations:
                        break
                    continue
                sub_cursor = _FormatCursor(item_list)
                try:
                    result_parts.append(_format_process_cursor(inner, sub_cursor))
                except _FormatEscape as esc:
                    result_parts.append(esc.partial)
                    # The escaping pass may have consumed some items before
                    # ~^ fired (e.g. ~A~^); reflect that in item_list so the
                    # outer-consumption count below is exact, not "all of it".
                    item_list = item_list[sub_cursor.idx:]
                    break
                consumed = sub_cursor.idx
                if consumed <= 0:
                    # A body consuming nothing would iterate forever; advance
                    # by one so the loop stays bounded.
                    consumed = 1
                item_list = item_list[consumed:]
            outer_consumed = len(items) - len(item_list)

        if at_flag:
            cursor.idx += outer_consumed

        return (''.join(result_parts), end_pos)
    
    elif directive == '}':
        return ('', pos)
    
    elif directive == '^':
        # ~^ - CLHS 22.3.9.2: terminate the enclosing ~{...~} iteration, or
        # the whole control string when not inside one. It is a control
        # transfer, not a character, so it raises rather than returning an
        # in-band marker for callers to string-replace out.
        #
        # Whether it fires depends on how many prefix parameters are
        # *actually supplied* (CLHS 22.3.9.2):
        #   none    - terminate if no arguments remain
        #   n       - terminate if n is zero
        #   n,m     - terminate if n equals m
        #   n,m,p   - terminate if n <= m <= p
        # A `~V`-sourced parameter that evaluates to NIL counts as omitted
        # (CLHS 22.3.3), so arity is decided by `_is_unspecified`, not by
        # position in `params` -- a blank/NIL leading parameter must shift
        # the remaining ones down rather than being read as a literal 0.
        supplied = [p for p in params if not _is_unspecified(p)]
        if len(supplied) >= 3:
            n, m, p = supplied[0], supplied[1], supplied[2]
            should_escape = _lisp_number(n) <= _lisp_number(m) <= _lisp_number(p)
        elif len(supplied) == 2:
            should_escape = _lisp_number(supplied[0]) == _lisp_number(supplied[1])
        elif len(supplied) == 1:
            should_escape = _lisp_number(supplied[0]) == 0
        elif colon_flag:
            # ~:^ with no parameters, inside ~:{...~}/~:@{...~}: fires if
            # and only if the sublist supplying the current step is the LAST
            # one, and then terminates the entire iteration (CLHS 22.3.9.2;
            # `format.:^.{.2`/`.3` pin both halves -- `~:{~:^~A~}` over four
            # sublists prints the first three and drops the last). The
            # per-pass cursor records which pass it is; on any other cursor
            # the default (True) keeps ~:^ terminating, as it always did.
            should_escape = cursor.last_item
        else:
            should_escape = cursor.remaining_count() <= 0

        if should_escape:
            # ~:^ terminates the iteration one level out (CLHS 22.3.9.2),
            # which is what ~:{...~} bodies use to stop the outer sweep.
            raise _FormatEscape(terminate_outer=colon_flag)
        return ('', pos)

    elif directive == '\n':
        # ~<newline> - Ignored newline (CLHS 22.3.9.3). Plain: ignore the
        # newline *and* any following non-newline whitespace. Colon: ignore
        # the newline but leave following whitespace in place
        # (`format.newline.2`'s "A X"). At: keep the newline but ignore
        # following whitespace (`format.newline.3`'s "A\nX", not "A\n X").
        if at_flag:
            while pos < len(control_string) and control_string[pos] in ' \t':
                pos += 1
            return ('\n', pos)
        elif colon_flag:
            return ('', pos)
        else:
            while pos < len(control_string) and control_string[pos] in ' \t':
                pos += 1
            return ('', pos)

    elif directive == 'P':
        # ~P - Plural (CLHS 22.3.5.4).
        # ~:P re-examines the previously consumed argument without consuming a new one.
        # Plain ~P outputs '' if arg==1 (EQL), else 's'
        # ~@P outputs 'y' if arg==1 (EQL), else 'ies'
        if colon_flag:
            val = cursor.prev()
        else:
            val = get_arg()

        # Check EQL to the integer 1, not numeric equality
        is_one = val == 1 and isinstance(val, int)

        if at_flag:
            result = 'y' if is_one else 'ies'
        else:
            result = '' if is_one else 's'
        return (result, pos)

    elif directive == '/':
        # ~/name/ - Call a function (CLHS 22.3.10). The name spans from
        # here to the next `/`, and is parsed like a symbol: upcased
        # (standard readtable case), and everything before the FIRST `:`
        # is a package name (`:` and `::` are the same to the lookup --
        # `format./.10`'s comment: "Single : doesn't mean it has to be
        # exported" -- so FIND-SYMBOL's internal-symbols-too semantics is
        # the rule), everything after it the symbol name, which may itself
        # contain colons (`format./.11`'s |FUNCTION:FOR::FORMAT:SLASH:11|).
        # Without a prefix the name is looked up in the current package.
        #
        # The function is called as (function stream arg colon at . params)
        # -- every prefix parameter, specified or not, passed on positionally
        # (`format./.19`'s `~v,v,v,v,v,v,v,v,v,v@/` hands ten of them over).
        slash = control_string.find('/', pos)
        if slash == -1:
            raise lisptype.LispError(
                "FORMAT: unterminated ~/ directive (no closing /)")
        name_text = control_string[pos:slash]
        pos = slash + 1
        if not name_text:
            raise lisptype.LispError("FORMAT: ~/ directive requires a function name")
        if ':' in name_text:
            package_text, symbol_text = name_text.split(':', 1)
            # A `::` prefix is the two-colon marker, not part of the symbol
            # name: strip the second colon, leaving any FURTHER colons in
            # place (`format./.11`'s symbol name contains `::` itself).
            if symbol_text.startswith(':'):
                symbol_text = symbol_text[1:]
        else:
            package_text, symbol_text = None, name_text
        if package_text is None:
            from .utilities_symbols import get_current_package
            package = get_current_package()
        else:
            package = lisptype.find_package(package_text.upper())
            if package is None:
                raise lisptype.LispError(
                    f"FORMAT: ~/{name_text}/ names no package {package_text.upper()!r}")
        symbol, _status = package.find_symbol(symbol_text.upper())
        if symbol is None:
            raise lisptype.LispError(
                f"FORMAT: ~/{name_text}/ names no symbol in package "
                f"{getattr(package, 'name', package_text)}")
        from .utilities_functions import symbol_function
        function = symbol_function(symbol)
        from .streams import make_string_output_stream as _make_sos
        from .streams import get_output_stream_string as _get_oss
        capture = _make_sos()
        arg = get_arg()
        function(capture, arg, colon_flag, at_flag, *params)
        return (str(_get_oss(capture)), pos)

    else:
        # Unknown directive - just output the tilde and char
        return ('~' + directive, pos)


def _format_process_cursor(control_string, cursor):
    """Process a format control string, consuming arguments from `cursor`.

    This is the shared core: passing the *same* cursor into a nested call
    (used by ~<...~>, ~(...~), ~[...~], ~@?) makes consumption inside the
    nested directive visible to whatever follows it in the outer control
    string - the structural fix for FORMAT's argument-cursor model. Passing
    a *fresh* cursor (used per-item by ~{...~}, and by plain ~?) gives a
    nested control string its own independent argument scope, per CLHS.
    """
    result = []
    pos = 0
    n = len(control_string)
    while pos < n:
        c = control_string[pos]
        if c == '~':
            pos += 1
            try:
                output, pos = _format_directive(control_string, cursor, pos,
                                               emitted=result)
            except _FormatEscape as esc:
                # `~^` abandons the rest of *this* control string but keeps
                # what it already produced (CLHS 22.3.9.2). Each frame
                # prepends its own accumulated output as the escape unwinds,
                # so the text is assembled in the order it was generated.
                esc.partial = ''.join(result) + esc.partial
                raise
            result.append(output)
        elif c == ' ':
            # Bracket a run of *literal* control-string spaces so a
            # `~<...~:@>` enclosing this text can later tell them apart from
            # spaces inside an argument's own printed value (CLHS 22.3.5.2's
            # auto-fill only wraps "blanks immediately contained in the
            # body" -- see `_resolve_pretty_body`). Invisible outside a
            # logical block: `_format_process_with_tail` strips the brackets
            # from any result that never reaches one.
            run_start = pos
            while pos < n and control_string[pos] == ' ':
                pos += 1
            result.append(_PP_LIT_SPACE_OPEN + control_string[run_start:pos]
                           + _PP_LIT_SPACE_CLOSE)
        else:
            result.append(c)
            pos += 1
    return ''.join(result)


def _format_process(control_string, args):
    """Process a format control string with arguments (fresh cursor)."""
    return _format_process_with_tail(control_string, args)[0]


def _pp_resolve_bare_breaks(text):
    """Resolve `~_`-family sentinels for a control string whose destination
    is NOT a pretty-printing stream -- no `~<...~:>` block in the control
    string and no `PPRINT-LOGICAL-BLOCK` frame around the call. Every kind,
    `~:@_`'s `:mandatory` included, is a PPRINT-NEWLINE, and a
    PPRINT-NEWLINE to a stream that is not pretty-printing does nothing
    (CLHS 22.2.2; `format._.9` and `format.:_.7` with margin 4 must stay
    flat, and so must `format.:@_.5`). The implicit-block resolution this
    replaced computed a fit from the control string's own width against
    `*print-right-margin*` and fired `:linear` breaks on that basis -- a
    fit no real block ever determined.
    """
    for kind in ('linear', 'fill', 'miser', 'mandatory'):
        text = text.replace(_PP_BREAK[kind], '')
    text = _PP_INDENT_RE.sub('', text)
    return _pp_strip_lit_space(text)


def _emit_format_result(formatted, stream):
    """Deliver a processed FORMAT result to `stream`.

    When the destination is the current `PPRINT-LOGICAL-BLOCK`'s own buffer
    -- `(format t ...)` / `(format s ...)` running inside the block's body --
    and the result still carries unresolved `~_`/`~I` sentinels, the result
    is forwarded as *tokens* into that buffer instead of being resolved
    here: the block's flush (`_pp_render_block`) is the only place that
    knows whether its breaks fire, and a `~_` the body emitted is the
    block's own conditional newline (`format._.1`'s `(format t "B ~_")`
    must break with the enclosing block, not resolve against a margin
    guessed at format time).

    Anywhere else the sentinels are resolved in place: only `~:@_` can
    fire without an enclosing block (see `_pp_resolve_bare_breaks`).
    """
    if _PP_ANY_BREAK_OR_INDENT_RE.search(formatted):
        frame = _current_pprint_frame_or_none()
        if frame is not None and stream is frame.stream:
            for token in _pp_tokenize(formatted):
                if token[0] == 'text':
                    # The literal-space brackets are FORMAT-engine
                    # bookkeeping for `~<...~:>`'s auto-fill; a frame's
                    # buffer holds plain text, so the brackets come off
                    # here rather than leaking into the render.
                    token = ('text', _pp_strip_lit_space(token[1]))
                frame.stream.tokens.append(token)
            return
        formatted = _pp_resolve_bare_breaks(formatted)
    write_text(formatted, stream)


def _format_process_with_tail(control_string, args):
    """Like _format_process but also return the number of arguments consumed
    (i.e. the index of the first remaining argument)."""
    # CLHS 22.3.6.2's restrictions are properties of the *control string*, not
    # of any one directive, so they are checked once here -- the one place a
    # whole control string is entered -- rather than at each directive, which
    # cannot see whether a `~<...~:;...~>` appears elsewhere in the string.
    _check_justification_conflicts(control_string)

    cursor = _FormatCursor(args)
    try:
        result = _format_process_cursor(control_string, cursor)
    except _FormatEscape as esc:
        # A `~^` outside any iteration terminates the control string itself;
        # this is the outermost frame, so the escape stops here rather than
        # escaping FORMAT as a Python exception (standing rule 2).
        result = esc.partial

    # `~_`/`~I` bare at the top of a control string resolve here when no
    # `PPRINT-LOGICAL-BLOCK` encloses the FORMAT call: only `~:@_` can fire,
    # everything else is a no-op (`_pp_resolve_bare_breaks`). Inside one the
    # sentinels are left for `_emit_format_result`, which either forwards
    # them into that block's buffer as tokens (when the destination is the
    # block's own stream) or resolves them the same bare way.
    if _PP_ANY_BREAK_OR_INDENT_RE.search(result):
        if _current_pprint_frame_or_none() is None:
            result = _pp_resolve_bare_breaks(result)
    else:
        result = _pp_strip_lit_space(result)
    return result, cursor.idx


@_registry.cl_function('FORMAT')
def format_fn(destination, control_string, *args):
    """Format output according to Common Lisp FORMAT directives.
    
    Args:
        destination: T for stdout, NIL for string, or stream
        control_string: Format control string with ~ directives
        *args: Arguments to format
    
    Returns:
        NIL if destination is T or stream, formatted string if NIL
    
    Supported directives:
        ~A    Aesthetic (princ-style)
        ~S    Standard (prin1-style)
        ~D    Decimal integer
        ~X    Hexadecimal
        ~O    Octal
        ~B    Binary
        ~R    Radix or English
        ~C    Character
        ~F    Fixed-format float
        ~E    Exponential float
        ~G    General float
        ~%    Newline
        ~&    Fresh line
        ~~    Literal tilde
        ~|    Page separator
        ~T    Tabulation
        ~*    Go to argument
        ~?    Recursive processing
        ~(~) Case conversion
        ~[~] Conditional
        ~{~} Iteration
        ~^    Escape from iteration
        ~P    Plural
    """
    if callable(control_string) and not isinstance(control_string, (str, lisptype.LispString)):
        # CLHS 22.3.1 / the "format control" glossary entry: control-string
        # is a designator for either a string or a function of (stream
        # &rest args) -- the latter is what FORMATTER returns. Call it
        # directly instead of falling into str(control_string) below, which
        # would hand FORMAT the function's Python repr ("<function ... at
        # 0x...>") to interpret as literal directive text.
        if destination is None or destination is lisptype.NIL:
            from .streams import make_string_output_stream as _make_sos, get_output_stream_string as _get_oss
            capture = _make_sos()
            control_string(capture, *args)
            return _get_oss(capture)
        elif destination is True or destination is lisptype.T:
            control_string(lisptype.T, *args)
            return lisptype.NIL
        else:
            control_string(destination, *args)
            return lisptype.NIL

    if control_string is None:
        control_string = ""
    elif not isinstance(control_string, str):
        control_string = str(control_string)

    formatted = _format_process(control_string, args)

    if destination is True or destination is lisptype.T:
        # FORMAT's `destination` is not a plain stream designator: `t` means
        # `*STANDARD-OUTPUT*` here (CLHS 22.3.1), whereas for a stream
        # designator `t` means `*TERMINAL-IO*` (CLHS 21.1.3). Printing to the
        # process's stdout instead meant `(format t ...)` escaped any
        # `(with-output-to-string (*standard-output*) ...)` around it.
        _emit_format_result(formatted, resolve_output_stream(lisptype.NIL))
        return lisptype.NIL
    elif destination is None or destination is lisptype.NIL:
        if _PP_ANY_BREAK_OR_INDENT_RE.search(formatted):
            # A string result the caller keeps as data: any sentinels a
            # frame-deferral left behind must not leak into it.
            formatted = _pp_resolve_bare_breaks(formatted)
        # FORMAT to NIL returns a Lisp STRING object (CLHS 22.3.1), not the
        # Python `str` the engine assembles internally. A bare `str` is not a
        # VECTOR under TYPEP, so ansi-test's own `equalp-with-case` (rt.lsp),
        # which element-compares only objects that answer T to
        # `(typep x 'vector)` and otherwise falls back to EQL, failed every
        # deftest whose result was a `(format nil ...)` string -- even when
        # expected and actual printed identically (17 failures in
        # format-conditional.lsp). The FORMATTER half of every
        # def-format-test already returned a LispString through
        # GET-OUTPUT-STREAM-STRING; this makes the FORMAT half agree.
        return lisptype.LispString(formatted)
    else:
        _emit_format_result(formatted, resolve_output_stream(destination))
        return lisptype.NIL


@_registry.cl_function('%FORMATTER')
def formatter(control_string):
    """Runtime primitive behind the `FORMATTER` macro (CLHS 22.3.1:
    (FORMATTER control-string)). Registered under a private name because
    `FORMATTER` itself is a macro: its control-string is the literal
    string object appearing syntactically in the form, never evaluated
    (that is *why* it is a macro rather than a function -- the string
    must be known at macroexpansion time), and `standard_macros.py`'s
    expander quotes it and calls this.

    Returns a function of (stream &rest args) -- the function-valued half of
    the "format control" designator FORMAT and ERROR/WARN/CERROR datums also
    accept -- that formats args per control-string and writes the result to
    stream, returning the list of arguments it did not consume.
    """
    control_string_str = str(control_string)

    def format_func(stream, *args):
        # Use internal processor to obtain remaining-args index (tail)
        formatted, consumed = _format_process_with_tail(control_string_str, args)
        # `_emit_format_result` rather than `write_text`: a formatter called
        # inside a `PPRINT-LOGICAL-BLOCK` body with `~_`/`~I` in its control
        # string must hand its breaks to that block, exactly like FORMAT.
        _emit_format_result(formatted, resolve_output_stream(stream))
        # Return the tail (remaining args) as a proper Lisp list -- a bare
        # Python list here is a second, incompatible list representation
        # (finding M), so `(equal (funcall fn stream ... 'a) '(a))` was
        # comparing a `lispCons` against a Python list and always failing,
        # regardless of whether the tail's contents were otherwise correct.
        tail = lisptype.NIL
        for item in reversed(args[consumed:]):
            tail = lisptype.lispCons(item, tail)
        return tail

    return format_func


# NOTE: Pathname operations are defined in pathnames.py with proper Pathname class support
# Functions like PATHNAME, PATHNAMEP, PATHNAME-DIRECTORY, etc. are all in pathnames.py


# Stream operations
# NOTE: actual OPEN/CLOSE and stream operations are implemented in
# lispfunc/streams.py. The simplified stubs were removed to avoid
# clashing registrations that override the full implementations.


# File operations
# NOTE: PROBE-FILE is defined in pathnames.py and imported above


@_registry.cl_function('DELETE-FILE')
def delete_file(filespec):
    """DELETE-FILE: delete the file `filespec` names (CLHS 20.2).

    `filespec` is a *pathname designator*, which includes a stream associated
    with a file -- `compile-file.14` passes exactly that, an output stream it
    opened and closed, and this used to `str()` it and hand
    ``<fclpy.lispfunc.streams.Stream object at 0x...>`` to `os.remove`. Both
    the designator rule and the relative-pathname search now live in
    `pathnames.resolve_filespec`; this carried a fourth copy of that search
    (LOAD, COMPILE-FILE and COMPILE-FILE-PATHNAME had the others), which is
    how they came to disagree about which package `*DEFAULT-PATHNAME-DEFAULTS*`
    lives in.

    A file that is not there is a FILE-ERROR, per CLHS -- not NIL, which
    conflated "deleted nothing" with "deleted it".
    """
    import os
    from fclpy.lispfunc.pathnames import pathname_from_namestring, resolve_filespec
    from fclpy.lispfunc.evaluation_conditions import signal_file_error

    path_str = resolve_filespec(filespec)
    try:
        if os.path.isdir(path_str):
            # CLHS defines DELETE-FILE on files, but the suite's own cleanup
            # (ensure-directories-exist.8) deletes the scratch directories it
            # created; `os.remove` cannot remove a directory on Windows,
            # `os.rmdir` can (an empty one -- a non-empty one still errors,
            # which is the honest failure).
            os.rmdir(path_str)
        else:
            os.remove(path_str)
    except FileNotFoundError:
        return signal_file_error(
            pathname_from_namestring(path_str), "DELETE-FILE: file not found: " + path_str)
    except OSError as error:
        return signal_file_error(
            pathname_from_namestring(path_str), "DELETE-FILE: " + str(error))
    return lisptype.T


@_registry.cl_function('RENAME-FILE')
def rename_file(filespec, new_name):
    """RENAME-FILE (CLHS 20.2): rename a file and answer three values.

    The three values are the standard's: the *defaulted* new name, the old
    truename, and the new truename. This returned one value -- a Python string
    -- from `os.rename(str(filespec), str(new_name))`, which got three separate
    things wrong at once: a `str()` on a designator (so a stream argument
    became its Python repr), no defaulting of `new-name` against the file being
    renamed, and no truenames, which is what the caller needs in order to find
    the file afterwards.

    `new-name` is merged with the *pathname* of the file being renamed, on
    the untranslated designators, rather than with their OS translations:
    CLHS 20.2 says the components new-name does not supply come from the
    file, and `rename-file.5` renames one logical pathname to another and
    requires the defaulted name back as a logical pathname. The merge is
    MERGE-PATHNAMES' job and is delegated to it.

    **The file is renamed to the name this returns.** There is one merged
    name and it is resolved once, because the two used to be computed
    independently and disagreed: the OS-side target was
    `merge_pathnames(pathname_from_namestring(resolve_filespec(new_name)),
    old_path)`, and `resolve_filespec` resolves a *relative* new-name against
    `*DEFAULT-PATHNAME-DEFAULTS*` before the merge ever runs -- so its first
    argument arrived already absolute, and MERGE-PATHNAMES fills in only
    *missing* components, so the file being renamed could no longer supply its
    directory. `(rename-file "some/dir/f.txt" (make-pathname :name "g"))`
    therefore moved the file into `*DEFAULT-PATHNAME-DEFAULTS*` while
    returning, and reporting as the new truename, a name in `some/dir/`
    (rename-file.3: `(probe-file defaulted-new-name)` was NIL for a rename
    that had just "succeeded").
    """
    import os
    from fclpy.lispfunc.pathnames import (
        pathname_from_namestring, pathname_from_os_path, resolve_filespec, merge_pathnames)
    from fclpy.lispfunc.evaluation_conditions import signal_file_error

    old_path = resolve_filespec(filespec)
    if not os.path.exists(old_path):
        return signal_file_error(
            pathname_from_namestring(old_path), "RENAME-FILE: file not found: " + old_path)

    old_truename = pathname_from_os_path(os.path.realpath(old_path))
    # The defaulted new name is merged in pathname space, on the untranslated
    # designators -- CLHS 20.2 says new-name is merged with the pathname of the
    # file, not with its OS translation, and rename-file.5 renames one logical
    # pathname to another and requires the defaulted name back as a LOGICAL
    # pathname.
    from fclpy.lispfunc.pathnames import _coerce_pathname_designator
    defaulted_new_name = merge_pathnames(
        _coerce_pathname_designator(new_name, 'RENAME-FILE'),
        _coerce_pathname_designator(filespec, 'RENAME-FILE'))
    # ...and the file goes to exactly that name, resolved once through the one
    # designator-to-OS-path resolver. See the docstring: resolving `new_name`
    # itself instead put the file somewhere the returned name did not name.
    new_path = resolve_filespec(defaulted_new_name)

    try:
        os.replace(old_path, new_path)
    except OSError as error:
        return signal_file_error(
            pathname_from_namestring(old_path), "RENAME-FILE: " + str(error))

    return lisptype.MultipleValues(
        defaulted_new_name, old_truename, pathname_from_os_path(os.path.realpath(new_path)))


@_registry.cl_function('FILE-AUTHOR')
def file_author(pathspec):
    """FILE-AUTHOR (CLHS 21.1.2): the author of the file `pathspec` names.

    The file systems this runs on record no author in the CLHS sense, so the
    string this answers is implementation-defined. What the spec does pin
    down are the failure modes, and they are what `file-author.error.3`/`.4`
    check: a *wild* pathname cannot name one file, and a file that is not
    there cannot be asked about -- each a FILE-ERROR, not a fabricated
    answer for whatever the designator happened to spell.
    """
    import os
    from fclpy.lispfunc.pathnames import (
        _coerce_pathname_designator, _error_if_wild, resolve_filespec)
    from fclpy.lispfunc.evaluation_conditions import signal_file_error

    pn = _coerce_pathname_designator(pathspec, 'FILE-AUTHOR')
    _error_if_wild(pn, 'FILE-AUTHOR')
    path_str = resolve_filespec(pn)
    if not os.path.exists(path_str):
        return signal_file_error(
            pn, "FILE-AUTHOR: file not found: " + path_str)
    return "unknown"


@_registry.cl_function('FILE-LENGTH')
def file_length(stream):
    """FILE-LENGTH (CLHS 21.1.2): length of the file `stream` is open to,
    in units of its element type.

    Was a stub returning 0 unconditionally, regardless of the stream or
    what had been written to it. CLHS defines this only for a FILE-STREAM
    (or a BROADCAST-STREAM, which delegates to one of its own targets) --
    every other stream kind (string streams, echo/two-way/concatenated
    streams) is a TYPE-ERROR, which `streams/file-length.lsp`'s
    FILE-LENGTH.ERROR.3 checks across every stream kind at once. For a
    binary stream, the byte count is divided by `byte_width` (set by OPEN
    from the declared `:element-type`, `streams._classify_element_type`)
    to answer in elements, not bytes -- FILE-LENGTH.2/.3/.4 write 17
    elements of various bit widths and require exactly 17 back.
    """
    from .streams import Stream, BroadcastStream, SynonymStream, stream_type_matches
    if isinstance(stream, BroadcastStream):
        # CLHS 21.1.2: a broadcast stream's file operations act on its LAST
        # component ("ensure that the last component is taken" is the
        # broadcast-stream-streams.3 test's own comment) -- delegating to the
        # first handed a string stream to FILE-LENGTH's type-error check. An
        # empty broadcast stream is length 0 (`make-broadcast-stream.5`).
        if not stream.streams:
            return 0
        return file_length(stream.streams[-1])
    if isinstance(stream, SynonymStream):
        return file_length(stream._target())
    if not (isinstance(stream, Stream) and stream_type_matches(stream, 'FILE-STREAM')):
        raise lisptype.LispTypeError(
            f"FILE-LENGTH: {stream!r} is not a file-stream",
            expected_type='(OR FILE-STREAM BROADCAST-STREAM)', actual_value=stream)
    import os
    try:
        size = os.fstat(stream.file_obj.fileno()).st_size
    except (OSError, AttributeError, ValueError):
        return lisptype.NIL
    if stream.binary and stream.byte_width:
        return size // stream.byte_width
    return size


@_registry.cl_function('FILE-POSITION')
def file_position(stream, position=None):
    """FILE-POSITION (CLHS 21.2): get or set a stream's file position.

    Was a complete stub that returned its own argument without touching the
    file at all -- `(progn (file-position s :start) (read-line s))` after a
    write read from wherever the write had left the cursor (typically EOF),
    not the start, because no `seek` ever happened. A pending PEEK-CHAR/
    UNREAD-CHAR pushback is stale the instant the underlying position moves,
    so a successful seek drops it.
    """
    import os
    from .streams import Stream, BroadcastStream

    if isinstance(stream, BroadcastStream):
        # CLHS 21.1.2's "last component" rule, same as FILE-LENGTH's: the
        # position of a broadcast stream is the position of the last stream
        # it broadcasts to, and 0 when there is none
        # (`make-broadcast-stream.6`, `broadcast-stream-streams.4`).
        if not stream.streams:
            return 0
        return file_position(stream.streams[-1], position)

    if not isinstance(stream, Stream) or not hasattr(stream.file_obj, 'seek'):
        return lisptype.NIL

    file_obj = stream.file_obj
    if position is None:
        try:
            return file_obj.tell()
        except (OSError, ValueError):
            return lisptype.NIL

    if isinstance(position, (lisptype.lispKeyword, lisptype.LispSymbol)):
        name = position.name.upper()
        if name == 'START':
            whence, offset = os.SEEK_SET, 0
        elif name == 'END':
            whence, offset = os.SEEK_END, 0
        else:
            return lisptype.NIL
    else:
        whence, offset = os.SEEK_SET, int(position)

    try:
        file_obj.seek(offset, whence)
    except (OSError, ValueError):
        return lisptype.NIL
    stream._pending.clear()
    stream.position = file_obj.tell()
    return lisptype.T


@_registry.cl_function('FILE-STRING-LENGTH')
def file_string_length(stream, string):
    """Length of string in file (CLHS 21.1.2).

    "If stream has no component streams, then the result is 1" is CLHS's own
    special case for a broadcast-stream with nothing to broadcast to --
    `make-broadcast-stream.7`/`broadcast-stream-streams.4` require it
    literally, not `(length string)`.
    """
    from .streams import BroadcastStream
    if isinstance(stream, BroadcastStream) and not stream.streams:
        return 1
    # Handle Character objects (which represent a single character)
    if isinstance(string, lisptype.Character):
        return 1
    # Handle Python strings and sequences
    return len(string)


# FILE-WRITE-DATE is registered exactly once, in pathnames.py next to the
# `resolve_filespec` designator resolver. The io_write.py copy that used to
# win by import order returned 0 for a missing file (it caught the OSError)
# and fell back to `str(pathspec)` rather than going through the designator
# resolver, so a pathname designator reached the OS only because Pathname's
# __fspath__ happened to carry it -- a namestring wrapper is not a designator
# resolution. It is the dead half of a duplicate-register entry (plan.md §2).


# COMPILE-FILE and COMPILE-FILE-PATHNAME live next to LOAD in
# misc_macros.py. They are the same operation read from the other end -- both
# read a file form by form with `*PACKAGE*` and `*READTABLE*` bound, and
# COMPILE-FILE's output is what LOAD then reads -- and while they were apart
# they had drifted: each carried its own ~35-line copy of "resolve a relative
# pathname", and the copies looked `*DEFAULT-PATHNAME-DEFAULTS*` up in two
# *different packages*, so the same relative name resolved differently
# depending on which operator asked. That search now has one home,
# `pathnames.resolve_filespec`.


# Condition operations
@_registry.cl_function('SIMPLE-CONDITION-FORMAT-ARGUMENTS')
def simple_condition_format_arguments(condition):
    """Get the format-arguments slot of a simple-condition (CLHS 9.2).

    Previously a stub that always returned () regardless of what the
    condition actually stored, so any simple-condition/simple-error/
    simple-warning signaled with format arguments (e.g. (error "~A" 10))
    lost them the moment a handler tried to read them back via this
    accessor -- FORMAT would then be called with no arguments at all.
    """
    if isinstance(condition, lisptype.Condition):
        return list(condition.get_slot('format-arguments') or [])
    return []


@_registry.cl_function('SIMPLE-CONDITION-FORMAT-CONTROL')
def simple_condition_format_control(condition):
    """Get the format-control slot of a simple-condition (CLHS 9.2).

    Previously a stub that returned str(condition) -- the condition's
    *report message*, not its format-control slot -- so this only
    happened to work when format-control was a plain string with no
    arguments and the message hadn't diverged from it; a function-valued
    format-control (FORMATTER's result) or one with format arguments was
    silently discarded.
    """
    if isinstance(condition, lisptype.Condition):
        return condition.get_slot('format-control')
    return str(condition)


def end_of_file():
    """End of file condition."""
    return EOFError()


def file_error():
    """File error condition."""
    return FileNotFoundError()


def file_error_pathname(condition):
    """Get pathname from file error."""
    return str(condition)  # Simplified


# Error handling
# ERROR is registered in utilities_errors.py -- the live implementation goes
# through build_condition/signal_error_object in evaluation_conditions.py so
# HANDLER-BIND/HANDLER-CASE/IGNORE-ERRORS can match it. The earlier copy here
# raised a bare Python Exception, which is not a condition and therefore
# matched no handler clause (not even (ERROR (C) ...)). It is the dead half of
# a duplicate register (plan.md §2); utilities_errors.error_fn is the survivor.


# Interactive I/O
def y_or_n_p(control_string=None, *args):
    """Ask yes/no question."""
    if control_string:
        print(control_string.format(*args), end=' ')
    response = input("(y or n) ").strip().lower()
    return lisptype.lisp_bool(response in ('y', 'yes'))


def yes_or_no_p(control_string=None, *args):
    """Ask yes/no question with full words."""
    if control_string:
        print(control_string.format(*args), end=' ')
    response = input("(yes or no) ").strip().lower()
    return lisptype.lisp_bool(response == 'yes')


# WITH- macros (simplified implementations)
def with_open_file(var_filespec_options, *body):
    """Execute with open file."""
    # Simplified - just execute body
    result = None
    for form in body:
        result = form
    return result


# NOTE: the real macro expander lives in evaluation_special_forms.py.
# This module-level stub neither evaluated its body nor created a stream,
# and register_module() would auto-register it as a *function* (its Python
# name differs from the expander's, so the decorator dedup misses it),
# clobbering the macro depending on import order -- standing rule 3.


# NOTE: the real macro expander lives in evaluation_special_forms.py.
# This module-level stub neither evaluated its body nor created a stream,
# and register_module() would auto-register it as a *function* (its Python
# name differs from the expander's, so the decorator dedup misses it),
# clobbering the macro depending on import order -- standing rule 3.


__all__ = [
    # Stream predicates and control
    'clear_output', 'output_stream_p', 'open_stream_p',
    # Write operations
    'write_char', 'write_string', 'write_line', 'write_byte', 'write',
    'prin1_to_string', 'princ_to_string', 'write_to_string',
    'print_fn', 'prin1', 'princ', 'terpri', 'fresh_line',
    'finish_output', 'force_output',
    # Stream creation
    'make_string_output_stream', 'get_output_stream_string',
    # Pretty printing
    'copy_pprint_dispatch', 'pprint', 'pprint_dispatch',
    'pprint_exit_if_list_exhausted', 'pprint_indent', 'pprint_linear',
    'pprint_logical_block_setup', 'pprint_newline', 'pprint_pop', 'pprint_tab',
    'pprint_tabular', 'pprint_fill', 'set_pprint_dispatch',
    # Format operations
    'format_fn', 'formatter',
    # Pathname operations
    'pathname', 'pathnamep', 'pathname_host', 'pathname_device',
    'pathname_directory', 'pathname_name', 'pathname_type',
    'pathname_version', 'make_pathname_function', 'namestring',
    'directory_namestring', 'host_namestring', 'file_namestring',
    'enough_namestring', 'parse_namestring', 'merge_pathnames',
    'wild_pathname_p', 'pathname_match_p', 'translate_pathname',
    'logical_pathname', 'translate_logical_pathname', 'truename',
    # File/Stream operations
    'open_fn', 'close_fn', 'stream_element_type', 'stream_external_format',
    # File operations
    'probe_file', 'delete_file', 'rename_file', 'file_author',
    'file_length', 'file_position', 'file_string_length',
    'file_write_date',
    # Condition operations
    'simple_condition_format_arguments', 'simple_condition_format_control',
    'end_of_file', 'file_error', 'file_error_pathname',
    # Interactive I/O
    'y_or_n_p', 'yes_or_no_p',
    # WITH- macros
    'with_open_file',]
