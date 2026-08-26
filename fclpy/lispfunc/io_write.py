"""I/O write operations - stream output, printing, pathnames, and file operations."""

import re
from fractions import Fraction
from decimal import Decimal, ROUND_HALF_EVEN

import fclpy.lisptype as lisptype
from . import registry as _registry
from .streams import open_file as open_fn, close_stream as close_fn, open_stream_p
from .core import _null_internal, _consp_internal, _listp_internal


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
    readably = dynamic_value(lisptype.py_str_to_sym('*PRINT-READABLY*'))
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
)


@_registry.cl_function('CLEAR-OUTPUT')
def clear_output(stream=None):
    """Clear output from stream."""
    return None


@_registry.cl_function('OUTPUT-STREAM-P')
def output_stream_p(stream):
    """OUTPUT-STREAM-P: can `stream` be used for output (CLHS 21.1)?"""
    from .streams import Stream
    if isinstance(stream, Stream):
        return lisptype.lisp_bool(stream.direction in ('output', 'io'))
    return lisptype.NIL


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


@_registry.cl_function('WRITE-STRING')
def write_string(string, stream=None, start=0, end=None):
    """Write a string's characters to a stream, without escapes (CLHS 21.2)."""
    text = str(string)
    if end is None or end is lisptype.NIL:
        end = len(text)
    write_text(text[start:end], stream)
    return string


@_registry.cl_function('WRITE-LINE')
def write_line(string, stream=None, start=0, end=None):
    """WRITE-STRING followed by a newline (CLHS 21.2)."""
    text = str(string)
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


@_registry.cl_function('WRITE')
def write(object, stream=None, **kwargs):
    """Print an object to a stream, honouring the printer keyword arguments.

    CLHS 22.3.1. WRITE is the general entry point: `PRIN1` and `PRINC` are it
    with `*PRINT-ESCAPE*` forced true and false respectively.
    """
    write_text(_write_object(object, **_print_keywords(kwargs)), stream)
    return object


@_registry.cl_function('PRIN1-TO-STRING')
def prin1_to_string(object):
    """The escaped printed representation, as a string (CLHS 22.3.1)."""
    return lisptype.LispString(_printer.prin1_to_string(object))


@_registry.cl_function('PRINC-TO-STRING')
def princ_to_string(object):
    """The unescaped printed representation, as a string (CLHS 22.3.1)."""
    return lisptype.LispString(_printer.princ_to_string(object))


@_registry.cl_function('WRITE-TO-STRING')
def write_to_string(object, **kwargs):
    """WRITE to a string instead of a stream (CLHS 22.3.1).

    Defaults to escaped output like `PRIN1`, not to `PRINC` -- it is WRITE, and
    WRITE honours `*PRINT-ESCAPE*`, whose initial value is true.
    """
    return lisptype.LispString(_write_object(object, **_print_keywords(kwargs)))


@_registry.cl_function('PRINT')
def print_fn(object, stream=None):
    """Newline, then the object escaped, then a space (CLHS 22.3.1).

    The order matters and was reversed: PRINT is defined as a `TERPRI`, then a
    `PRIN1`, then a space -- not `PRIN1` followed by a newline.
    """
    write_text('\n' + _printer.prin1_to_string(object) + ' ', stream)
    return object


@_registry.cl_function('PRIN1')
def prin1(object, stream=None):
    """Print an object escaped, with no surrounding whitespace (CLHS 22.3.1)."""
    write_text(_printer.prin1_to_string(object), stream)
    return object


@_registry.cl_function('PRINC')
def princ(object, stream=None):
    """Print an object with escaping off (CLHS 22.3.1).

    Not a separate representation from `PRIN1`: the same printer with
    `*PRINT-ESCAPE*` bound to NIL (CLHS 22.1.3.2).
    """
    write_text(_printer.princ_to_string(object), stream)
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
    """
    from .streams import Stream
    target = resolve_output_stream(stream)
    if isinstance(target, Stream):
        target.flush()
    return lisptype.NIL


@_registry.cl_function('FORCE-OUTPUT')
def force_output(stream=None):
    """FORCE-OUTPUT (CLHS 21.2): initiate output without necessarily
    waiting for it to complete. This implementation has no asynchronous
    I/O, so there is nothing weaker than FINISH-OUTPUT to do here.
    """
    from .streams import Stream
    target = resolve_output_stream(stream)
    if isinstance(target, Stream):
        target.flush()
    return lisptype.NIL


@_registry.cl_function('MAKE-STRING-OUTPUT-STREAM')
def make_string_output_stream(**kwargs):
    """Make string output stream - delegates to streams.py."""
    from .streams import make_string_output_stream as _make_sos
    element_type = kwargs.get('element_type', 'character')
    return _make_sos(element_type)


@_registry.cl_function('GET-OUTPUT-STREAM-STRING')
def get_output_stream_string(stream):
    """Get string from output stream - delegates to streams.py."""
    from .streams import get_output_stream_string as _get_oss
    return _get_oss(stream)


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
    writes and PPRINT-DISPATCH reads. Nothing consumes it yet -- the pretty
    printer is not implemented -- but the object's *identity* is already
    observable through `*PRINT-PPRINT-DISPATCH*`, which is what
    WITH-STANDARD-IO-SYNTAX needs.
    """

    def __init__(self, entries=None):
        self.entries = list(entries) if entries else []

    def copy(self):
        return PprintDispatchTable(self.entries)

    def __repr__(self):
        return "#<PPRINT-DISPATCH-TABLE>"


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
    """
    if table is None or table is lisptype.NIL:
        return standard_pprint_dispatch().copy()
    if isinstance(table, PprintDispatchTable):
        return table.copy()
    raise lisptype.LispNotImplementedError(
        f"COPY-PPRINT-DISPATCH: not a pprint dispatch table: {table!r}")


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
    """A newline, then the object printed with `*PRINT-PRETTY*` true (CLHS 22.3.1)."""
    write_text('\n', stream)
    return _pprint_unpretty(object, stream)


@_registry.cl_function('PPRINT-DISPATCH')
def pprint_dispatch(object, table=None):
    """Get pretty print dispatch function (stub)."""
    return print, lisptype.NIL  # Simplified


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
                 'outer_target', 'body_col', 'per_line_text')

    def __init__(self, remaining, stream, outer_target=None, body_col=0, per_line_text=None):
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

    # Gated on whether the keyword was syntactically *given*, not on whether
    # its value is NIL: `:prefix nil` is a supplied non-string value and must
    # fail `_pprint_block_text`'s check (`pprint-logical-block.error.1`),
    # whereas an omitted `:prefix` defaults to "" with no validation at all.
    prefix_text = _pprint_block_text(prefix, ':PREFIX') if prefix_given else ''
    per_line_text = (_pprint_block_text(per_line_prefix, ':PER-LINE-PREFIX')
                      if per_line_prefix_given else None)
    suffix_text = _pprint_block_text(suffix, ':SUFFIX') if suffix_given else ''

    write_text(per_line_text if per_line_text is not None else prefix_text, outer_target)
    # Only the outermost frame's column is needed now -- a nested frame's
    # own start column is not decidable until the enclosing block's earlier
    # breaks are (`flush_pprint_frame`'s `'block'`-token deferral), so it is
    # resolved later, from the *real* running column reached in that single
    # left-to-right render pass, not guessed here.
    body_col = 0 if isinstance(outer_target, _PPBuffer) else _pp_outer_column(outer_target)

    body_buffer = _PPBuffer()
    frame = PPrintFrame(object, body_buffer, outer_target=outer_target,
                         body_col=body_col, per_line_text=per_line_text)
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
    if isinstance(frame.outer_target, _PPBuffer):
        frame.outer_target.tokens.append(('block', suffix_text, frame.per_line_text, frame.stream.tokens))
        return
    rendered = _pp_render_top(frame.stream.tokens, frame.body_col,
                               frame.per_line_text, len(suffix_text))
    write_text(rendered + suffix_text, frame.outer_target)


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


@_registry.cl_function('PPRINT-EXIT-IF-LIST-EXHAUSTED')
def pprint_exit_if_list_exhausted():
    """Exit the enclosing `PPRINT-LOGICAL-BLOCK` if its list is used up (CLHS 22.2.2).

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


@_registry.cl_function('PPRINT-POP')
def pprint_pop():
    """Pop the next element from the enclosing `PPRINT-LOGICAL-BLOCK`'s list (CLHS 22.2.2).

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


@_registry.cl_function('PPRINT-LINEAR')
def pprint_linear(stream, object, prefix=None, per_line_prefix=None, suffix=None):
    """The list on one line, in a `(`...`)` block (CLHS 22.2.2).

    Linear style breaks either at every conditional newline or at none; with no
    line breaking available this is the "at none" case, which is a legal
    rendering for any list that fits. See `_pprint_unpretty`.
    """
    return _pprint_unpretty(object, stream)


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
    """Pretty print tab (stub)."""
    return None


@_registry.cl_function('PPRINT-TABULAR')
def pprint_tabular(stream, object, prefix=None, per_line_prefix=None, suffix=None,
                   tabsize=None):
    """The list in tabular style (CLHS 22.2.2), on one line without column stops."""
    return _pprint_unpretty(object, stream)


@_registry.cl_function('PPRINT-FILL')
def pprint_fill(stream, object, colon_p=None, at_sign_p=None):
    """The list in fill style (CLHS 22.2.2), on one line.

    `colon_p` (whether to parenthesize) is accepted and ignored, like every
    other block-delimiter argument here -- see `_pprint_unpretty`.
    """
    return _pprint_unpretty(object, stream)


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
    dispatch_table.entries.append((type_specifier, function, priority))
    return lisptype.T


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
    """
    __slots__ = ('args', 'idx')

    def __init__(self, args, idx=0):
        self.args = list(args) if args else []
        self.idx = idx

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


def _tab_padding(column, params, colon_flag, at_flag):
    """The spaces `~T` emits from `column` (CLHS 22.3.6.1).

    `~colnum,colincT` moves to `colnum`, or -- already at or past it -- to the
    first `colnum + k*colinc` beyond the current column, with `colinc` 0
    meaning "then do not move". `~colrel,colinc@T` emits `colrel` spaces and
    then as few more as it takes to reach a multiple of `colinc`.

    Both used to be `' ' * colnum`, under the comment "we don't track column,
    so just emit spaces" -- a different directive entirely: `AA~4T` is *move
    to column 4*, two spaces, and answered four. The column was in fact
    available all along (`emitted`), which is what `_current_column` reads.
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
        pad = max(colrel, 0)
        target = column + pad
        if colinc > 0 and target % colinc:
            pad += colinc - (target % colinc)
        return ' ' * pad

    colnum = param(0, 1)
    colinc = param(1, 1)
    if column < colnum:
        return ' ' * (colnum - column)
    if colinc <= 0:
        return ''
    steps = (column - colnum) // colinc + 1
    return ' ' * (colnum + steps * colinc - column)


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
    rest of each word to lower case."""
    result = []
    at_word_start = True
    for ch in s:
        if ch.isalpha():
            result.append(ch.upper() if at_word_start else ch.lower())
            at_word_start = False
        else:
            result.append(ch)
            at_word_start = True
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
                result.append(ch.upper())
                capitalized_any = True
            else:
                result.append(ch.lower())
            at_word_start = False
        else:
            result.append(ch)
            at_word_start = True
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


def _format_A_fallback(val):
    """CLHS 22.3.2: if the argument to `~D`/`~X`/`~O`/`~B`/`~R` is not an
    integer, it is printed as if by `~A` -- a fresh, unmodified `~A`, not
    this directive's own colon/at flags reinterpreted."""
    return _printer.princ_to_string(val)


def _format_integer_directive(val, radix, params, colon_flag, at_flag):
    """The shared digit-printing engine behind `~D`/`~X`/`~O`/`~B` and
    explicit-radix `~R`: sign, radix conversion, `:` comma-grouping, then
    `mincol`/`padchar` right-justification -- in that order, since padding
    must see the sign and commas already in place (CLHS 22.3.2)."""
    if not isinstance(val, int):
        return _format_A_fallback(val)
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
        elif kind == 'block':
            _, suffix_text, _per_line, subtokens = tok
            sub_flat = _pp_block_flat_width(subtokens)
            if sub_flat is None:
                return None
            width += sub_flat + len(suffix_text)
    return width


def _pp_render_block(tokens, start_col, indent_baseline, right_margin, miser_width,
                      block_fits, miser_active):
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
            elif kind == 'block':
                sub_flat = _pp_block_flat_width(tok[3])
                if sub_flat is None:
                    break
                width += sub_flat + len(tok[1])
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
        elif kind == 'indent':
            # No effect while the enclosing section is in miser mode (CLHS
            # 22.2.2's pprint-indent) -- see `_pp_render`'s identical guard.
            if not miser_active:
                _, relative_to, n = tok
                indent = (indent_baseline + n) if relative_to == 'block' else (col + n)
        elif kind == 'block':
            _, suffix_text, sub_per_line, subtokens = tok
            sub_flat = _pp_block_flat_width(subtokens)
            sub_fits = (right_margin is not None and sub_flat is not None
                        and col + sub_flat + len(suffix_text) <= right_margin)
            sub_miser = (right_margin is not None and miser_width is not None
                         and (right_margin - col) <= miser_width)
            sub_indent_baseline = 0 if sub_per_line is not None else col
            rendered_sub = _pp_render_block(subtokens, col, sub_indent_baseline,
                                             right_margin, miser_width, sub_fits, sub_miser)
            if sub_per_line is not None:
                rendered_sub = rendered_sub.replace('\n', '\n' + sub_per_line)
            combined = rendered_sub + suffix_text
            out.append(combined)
            nl = combined.rfind('\n')
            col = len(combined) - nl - 1 if nl != -1 else col + len(combined)
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
                fire = False if block_fits else (col + lookahead(idx)) > right_margin
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
                                 block_fits, miser_active)
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
    run. `*PRINT-CIRCLE*` has no shared-structure detector yet (plan.md), so
    a genuinely circular argument here would otherwise walk forever:
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
        if colon_flag and (val is None or val is lisptype.NIL):
            result = "()"
        else:
            result = _printer.princ_to_string(val)
        return (_format_pad(result, params, at_flag), pos)

    elif directive == 'S':
        # ~S - Standard: print as PRIN1 does, i.e. with *PRINT-ESCAPE* true.
        val = get_arg()
        if colon_flag and (val is None or val is lisptype.NIL):
            result = "()"
        else:
            result = _printer.prin1_to_string(val)
        return (_format_pad(result, params, at_flag), pos)


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
                result = _format_A_fallback(val)
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
                # Pretty print special characters
                char_names = {' ': 'Space', '\n': 'Newline', '\t': 'Tab', '\r': 'Return'}
                result = char_names.get(val.char, val.char)
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
        # `~:T`/`~:@T` are PPRINT-TAB's :section forms, which measure from the
        # start of the enclosing logical block rather than the line; with no
        # block established that is the same column, so they share this path
        # (and inside a `~<...~>` they are rejected outright -- see
        # `_check_justification_conflicts`).
        return (_tab_padding(_current_column(emitted), params,
                             colon_flag, at_flag), pos)

    elif directive == '*':
        # ~* - Go to argument
        if at_flag:
            # Go to absolute argument position
            cursor.idx = params[0] if params and params[0] is not None else 0
        elif colon_flag:
            # Go backwards
            count = params[0] if params and params[0] is not None else 1
            cursor.idx = max(0, cursor.idx - count)
        else:
            # Go forwards
            count = params[0] if params and params[0] is not None else 1
            cursor.idx = min(len(cursor.args), cursor.idx + count)
        return ('', pos)

    elif directive == '?':
        # ~? - Recursive processing
        # The next arg is a format string, and the one after is args for it
        fmt_str = get_arg()
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
        return (result, pos)

    elif directive == '_':
        # ~_ - Conditional newline (CLHS 22.3.5.1), same four kinds as
        # PPRINT-NEWLINE: no flags linear, `:` fill, `@` miser, `:@` mandatory.
        # Resolved later, against a margin, by whichever `~<...~:>` encloses
        # this one -- or by `_format_process_with_tail` if none does -- since
        # only that point knows whether the surrounding material fits.
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
            items = _pp_bounded_list_elements(obj)

        sub_cursor = _FormatCursor(items)
        try:
            body_text = _format_process_cursor(body_src, sub_cursor)
        except _FormatEscape as esc:
            # Within the body, ~^ acts like PPRINT-EXIT-IF-LIST-EXHAUSTED:
            # it ends the body, not the whole enclosing control string
            # (`format.logical-block.escape.1`/`.2`).
            body_text = esc.partial

        # ~:@> -- CLHS 22.3.5.2: "a fill-style conditional newline is
        # automatically inserted after each group of blanks immediately
        # contained in the body", on top of whatever `~_`-family directives
        # the body already spelled out explicitly.
        auto_fill = closer_colon and closer_at
        preceding = ''.join(emitted) if emitted else ''
        start_column = _pp_visible_width(preceding.rsplit('\n', 1)[-1])
        return (_resolve_pretty_body(body_text, start_column, prefix_text,
                                      suffix_text, per_line, auto_fill),
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
        inner_result = _format_process_cursor(inner, cursor)

        # Every variant converts through `_pp_case_convert`, which keeps its
        # hands off any unresolved pretty-printer sentinel in the body -- see
        # that function for what went wrong when they were converted too.
        if colon_flag and at_flag:
            # ~:@( ... ~) - force everything to upper case
            convert = str.upper
        elif colon_flag:
            # ~:( ... ~) - capitalize each word
            convert = _capitalize_words
        elif at_flag:
            # ~@( ... ~) - capitalize just the first word, lower case the rest
            convert = _capitalize_first_word
        else:
            # ~( ... ~) - force everything to lower case
            convert = str.lower

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
            # argument from whatever followed.
            if params and params[0] is not None:
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
        # Find matching ~} taking nesting into account
        nesting = 1
        i = pos
        end_inner = pos
        end_pos = pos
        while i < len(control_string) and nesting > 0:
            if control_string[i] == '~' and i + 1 < len(control_string):
                ch = control_string[i+1]
                if ch == '{':
                    nesting += 1
                    i += 2
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

        # CLHS 22.3.7.4: an empty body means the control string to iterate
        # with is itself an argument, taken *before* the list argument.
        if not inner:
            inner_arg = get_arg()
            # Same coercion FORMAT itself applies to its control-string
            # argument (see format_fn), so a LispString behaves identically
            # whether it arrives literally or through ~{~}.
            inner = '' if inner_arg is None else str(inner_arg)

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
        else:
            # ~{...~} / ~:{...~} - the next argument is the list of items,
            # a scope of its own (per CLHS 22.3.7): only the single "list"
            # argument itself (already taken by get_arg()) is removed from
            # the outer cursor; what the iteration body does with its
            # elements never touches the outer cursor further.
            items = _format_args_list(get_arg())

        # ~n{...~} bounds the number of iterations (CLHS 22.3.7.4); without
        # a parameter the only bound is the argument list running out.
        max_iterations = params[0] if params and params[0] is not None else None

        result_parts = []
        iterations = 0

        if colon_flag:
            # ~:{...~} - each item is itself a list, and one pass is made per
            # item with that sublist standing in as the whole argument stream.
            for item in items:
                if max_iterations is not None and iterations >= max_iterations:
                    break
                iterations += 1
                sub_cursor = _FormatCursor(_format_args_list(item))
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
            # the outer stream, whether it ran to completion or a `~^`
            # ended it partway through.
            outer_consumed = iterations
        else:
            # One pass at a time over what is left; each pass gets a fresh
            # cursor, and however much it consumed is where the next starts.
            item_list = list(items)
            while item_list:
                if max_iterations is not None and iterations >= max_iterations:
                    break
                iterations += 1
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
        else:
            should_escape = cursor.remaining_count() <= 0

        if should_escape:
            # ~:^ terminates the iteration one level out (CLHS 22.3.9.2),
            # which is what ~:{...~} bodies use to stop the outer sweep.
            raise _FormatEscape(terminate_outer=colon_flag)
        return ('', pos)

    elif directive == '\n':
        # ~<newline> - Ignored newline
        if at_flag:
            # Keep the newline
            return ('\n', pos)
        else:
            # Ignore newline and following whitespace
            while pos < len(control_string) and control_string[pos] in ' \t':
                pos += 1
            return ('', pos)

    elif directive == 'P':
        # ~P - Plural. ~:P re-examines the previously consumed argument
        # without consuming a new one, so the cursor must not move at all
        # (net-zero) - unlike the old code, which shifted it by one extra.
        if colon_flag:
            val = cursor.prev()
        else:
            val = get_arg()
        try:
            num = int(val) if val is not None else 1
            if at_flag:
                result = 'y' if num == 1 else 'ies'
            else:
                result = '' if num == 1 else 's'
        except (TypeError, ValueError):
            result = 's'
        return (result, pos)

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

    # `~_`/`~I` bare at the top of a control string, with no enclosing
    # `~<...~:>`, still resolve (CLHS restricts them only from appearing
    # inside a plain `~<...~>` justification, not from needing one at all) --
    # against an implicit block spanning the whole string. Every other
    # result -- the overwhelming majority of FORMAT calls -- just needs its
    # literal-space brackets (`_format_process_cursor`'s own bookkeeping)
    # stripped back out.
    if _PP_ANY_BREAK_OR_INDENT_RE.search(result):
        result = _resolve_pretty_body(result, 0, '', '', False, False, allow_miser=False)
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
        write_text(formatted, lisptype.NIL)
        return lisptype.NIL
    elif destination is None or destination is lisptype.NIL:
        return formatted
    else:
        write_text(formatted, destination)
        return lisptype.NIL


@_registry.cl_function('FORMATTER')
def formatter(control_string):
    """Create formatter function (CLHS 22.3.1: (FORMATTER control-string)).

    Returns a function of (stream &rest args) -- the function-valued half of
    the "format control" designator FORMAT and ERROR/WARN/CERROR datums also
    accept -- that formats args per control-string and writes the result to
    stream, returning the list of arguments it did not consume.
    """
    control_string_str = str(control_string)

    def format_func(stream, *args):
        # Use internal processor to obtain remaining-args index (tail)
        formatted, consumed = _format_process_with_tail(control_string_str, args)
        write_text(formatted, stream)
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

    `new-name` is merged with `filespec` rather than with
    `*default-pathname-defaults*`: CLHS says the components new-name does not
    supply come from the file being renamed, which is what makes
    `(rename-file "a/b.txt" "c")` land in `a/` and keep the type. That merge is
    MERGE-PATHNAMES' job and is delegated to it -- it is still namestring-based
    rather than component-based here, so a new-name that omits the *type*
    (rename-file.3) does not yet inherit it; that is the pathname component
    model, tracked separately, not something to work around in this operator.
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
    defaulted_new_name = merge_pathnames(
        pathname_from_namestring(resolve_filespec(new_name)),
        pathname_from_namestring(old_path))
    new_path = defaulted_new_name.namestring()

    try:
        os.replace(old_path, new_path)
    except OSError as error:
        return signal_file_error(
            pathname_from_namestring(old_path), "RENAME-FILE: " + str(error))

    return lisptype.MultipleValues(
        defaulted_new_name, old_truename, pathname_from_os_path(os.path.realpath(new_path)))


@_registry.cl_function('FILE-AUTHOR')
def file_author(pathspec):
    """Get file author."""
    return "unknown"  # Simplified


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
        if not stream.streams:
            return 1
        return file_length(stream.streams[0])
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
    from .streams import Stream

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
    return len(string)


@_registry.cl_function('FILE-WRITE-DATE')
def file_write_date(pathspec):
    """Get file write date."""
    import os
    import time
    try:
        return int(os.path.getmtime(str(pathspec)))
    except:
        return 0


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
@_registry.cl_function('ERROR')
def error(format_control, *args):
    """Signal error."""
    msg = format_control.format(*args) if args else str(format_control)
    raise Exception(msg)


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
    # Error handling
    'error',
    # Interactive I/O
    'y_or_n_p', 'yes_or_no_p',
    # WITH- macros
    'with_open_file',]
