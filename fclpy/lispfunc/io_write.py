"""I/O write operations - stream output, printing, pathnames, and file operations."""

import re
from fractions import Fraction

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
        except AttributeError:
            # A stream that refuses attributes (e.g. a raw file object with
            # __slots__) simply has no recorded column; `_at_line_start` then
            # reports "at a line start", so FRESH-LINE emits nothing rather
            # than a spurious newline.
            pass


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


class _PrettyStream:
    """Wraps a stream, re-emitting a per-line prefix after every newline.

    CLHS 22.2.1: `:per-line-prefix`, unlike `:prefix`, is not printed once --
    it begins *every* line the logical block outputs, including ones produced
    by an ordinary `TERPRI` inside the body (`pprint-logical-block.12`). A
    thin wrapper bound to the block's stream-symbol for the block's dynamic
    extent is enough: `write_text` already funnels every output call (WRITE,
    WRITE-CHAR, TERPRI, ...) through one place, so intercepting `.write` here
    covers all of them without a second output path.
    """

    def __init__(self, target, prefix_text):
        self._target = target
        self._prefix_text = prefix_text

    def write(self, text):
        if not text:
            return
        write_text(text.replace('\n', '\n' + self._prefix_text), self._target)


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

    __slots__ = ('remaining', 'stream', 'count', 'started_as_nil')

    def __init__(self, remaining, stream):
        self.remaining = remaining
        self.stream = stream
        self.count = 0
        self.started_as_nil = _null_internal(remaining)


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
      body, then write `suffix_text` and pop the frame.
    """
    import fclpy.state as state

    stream = resolve_output_stream(stream_designator)

    if not _listp_internal(object):
        return ('atom', stream, None, None)

    depth = len(getattr(state, 'pprint_stack', []) or [])
    level = _printer._as_count(_printer.resolve_control('*PRINT-LEVEL*'))
    if level is not None and depth >= level:
        return ('level-exceeded', stream, None, None)

    # Gated on whether the keyword was syntactically *given*, not on whether
    # its value is NIL: `:prefix nil` is a supplied non-string value and must
    # fail `_pprint_block_text`'s check (`pprint-logical-block.error.1`),
    # whereas an omitted `:prefix` defaults to "" with no validation at all.
    prefix_text = _pprint_block_text(prefix, ':PREFIX') if prefix_given else ''
    per_line_text = (_pprint_block_text(per_line_prefix, ':PER-LINE-PREFIX')
                      if per_line_prefix_given else None)
    suffix_text = _pprint_block_text(suffix, ':SUFFIX') if suffix_given else ''

    if per_line_text is not None:
        write_text(per_line_text, stream)
        body_stream = _PrettyStream(stream, per_line_text)
    else:
        write_text(prefix_text, stream)
        body_stream = stream

    frame = PPrintFrame(object, body_stream)
    return ('run', body_stream, frame, suffix_text)


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


@_registry.cl_function('PPRINT-INDENT')
def pprint_indent(relative_to, n, stream=None):
    """Set pretty print indent (stub)."""
    return None


@_registry.cl_function('PPRINT-LINEAR')
def pprint_linear(stream, object, prefix=None, per_line_prefix=None, suffix=None):
    """The list on one line, in a `(`...`)` block (CLHS 22.2.2).

    Linear style breaks either at every conditional newline or at none; with no
    line breaking available this is the "at none" case, which is a legal
    rendering for any list that fits. See `_pprint_unpretty`.
    """
    return _pprint_unpretty(object, stream)


@_registry.cl_function('PPRINT-NEWLINE')
def pprint_newline(kind, stream=None):
    """A conditional newline (CLHS 22.2.2) -- nothing, with no line breaking.

    It used to emit an *unconditional* newline to Python's stdout: wrong stream,
    and wrong even on the right one, since every `kind` here is conditional and
    all four conditions are "only if the enclosing block does not fit".
    """
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
        if len(params) > i and params[i] is not None:
            return params[i]
        return default

    padchar = _param(3, ' ')
    # A `'x` prefix parameter is parsed as a bare Python character, but a
    # `~V` parameter supplies whatever argument was passed -- which for a
    # pad character is a Lisp CHARACTER object, not a str.
    if isinstance(padchar, lisptype.Character):
        padchar = padchar.char
    padchar = str(padchar)[:1] or ' '

    return (
        _lisp_number(_param(0, 0)),
        _lisp_number(_param(1, 1)) or 1,
        _lisp_number(_param(2, 0)),
        padchar,
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
    # for `:` and a trailing one for `@`. A lone segment with neither
    # modifier still gets its single implicit point, on the left -- which is
    # what makes `~10<abc~>` right-justify.
    gaps = len(texts) - 1
    if colon_flag:
        gaps += 1
    if at_flag:
        gaps += 1
    if gaps == 0:
        gaps = 1

    content_width = sum(len(t) for t in texts)
    total_pad = minpad * gaps
    while content_width + total_pad < mincol:
        total_pad += colinc

    base, extra = divmod(total_pad, gaps)
    widths = [base + (1 if i < extra else 0) for i in range(gaps)]

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
                out.append(part)
                col = len(part) if j == len(parts) - 1 else 0
        elif kind == 'indent':
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
            else:  # fill
                fire = False if block_fits else (col + lookahead(idx)) > right_margin
            if fire:
                rstrip_pending()
                col = max(indent, 0)
                out.append('\n' + ' ' * col)
    return ''.join(out)


def _resolve_pretty_body(body_text, start_column, prefix_text, suffix_text,
                          per_line, auto_fill):
    """Render one logical block's body (CLHS 22.2/22.3.5.2) and wrap it.

    `auto_fill` is `~:@>`'s own effect: every run of literal control-string
    blanks directly in the body becomes a `:fill` conditional newline too,
    not just the explicit `~_`-family directives -- CLHS 22.3.5.2, "a
    fill-style conditional newline is automatically inserted after each
    group of blanks immediately contained in the body". Only *literal*
    blanks: the space characters bracketed by `_format_process_cursor`'s
    literal-run branch, never ones inside an argument's own printed text
    (`format.logical-block.26`'s `~A` of the string `"1 2 3"` must not wrap).
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
        miser_active = miser_width is not None and (right_margin - body_col) < miser_width
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
    
    # Skip optional numeric/char parameters and commas
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
        elif c == "'":
            # Character parameter 'X
            if pos + 1 < len(control_string):
                params.append(control_string[pos + 1])
                pos += 2
            else:
                pos += 1
        elif c == 'V' or c == 'v':
            # Use next argument as parameter
            params.append(cursor.next())
            pos += 1
        elif c == '#':
            # Number of remaining arguments
            params.append(cursor.remaining_count())
            pos += 1
        elif c == ',':
            pos += 1
            # Empty parameter slot
            if not params or control_string[pos-2] == ',':
                params.append(None)
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
        # ~F - Fixed-format floating point
        val = get_arg()
        try:
            num = float(val) if val is not None else 0.0
            # params: width, digits, scale, overflow-char, pad-char
            width = params[0] if params else None
            digits = params[1] if len(params) > 1 else None
            if digits is not None:
                result = f'{num:.{digits}f}'
            else:
                result = str(num)
            if at_flag and num >= 0:
                result = '+' + result
            if width and len(result) < width:
                result = ' ' * (width - len(result)) + result
        except (TypeError, ValueError):
            result = str(val)
        return (result, pos)
    
    elif directive == 'E':
        # ~E - Exponential floating point
        val = get_arg()
        try:
            num = float(val) if val is not None else 0.0
            digits = params[1] if len(params) > 1 and params[1] else 6
            result = f'{num:.{digits}e}'.upper()
            if at_flag and num >= 0:
                result = '+' + result
        except (TypeError, ValueError):
            result = str(val)
        return (result, pos)
    
    elif directive == 'G':
        # ~G - General floating point (choose F or E)
        val = get_arg()
        try:
            num = float(val) if val is not None else 0.0
            result = f'{num:g}'
            if at_flag and num >= 0:
                result = '+' + result
        except (TypeError, ValueError):
            result = str(val)
        return (result, pos)
    
    elif directive == '%':
        # ~% - Newline
        count = params[0] if params and params[0] else 1
        return ('\n' * count, pos)

    elif directive == '&':
        # ~n& - a fresh line, then n-1 further newlines (CLHS 22.3.1.3).
        #
        # It used to emit n newlines unconditionally, with the comment "we
        # don't track column". The column within this control string is exactly
        # what has been emitted so far, so `~&` is a fresh line for the same
        # reason FRESH-LINE is: emit one only if the output does not already
        # end at a line boundary. `~0&` emits nothing at all.
        count = 1 if not params or params[0] is None else params[0]
        if count <= 0:
            return ('', pos)
        preceding = ''.join(emitted) if emitted else ''
        needs_fresh_line = preceding != '' and not preceding.endswith('\n')
        return ('\n' * (count - 1 + int(needs_fresh_line)), pos)

    elif directive == '~':
        # ~~ - Literal tilde
        count = params[0] if params and params[0] else 1
        return ('~' * count, pos)

    elif directive == '|':
        # ~| - Page separator (form feed)
        count = params[0] if params and params[0] else 1
        return ('\f' * count, pos)

    elif directive == 'T':
        # ~T - Tabulation
        colnum = params[0] if params else 1
        colinc = params[1] if len(params) > 1 else 1
        # We don't track column, so just emit spaces
        return (' ' * (colnum if colnum else 1), pos)

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
        nested_logical_closers = []
        segment_start = pos
        closer_colon = False
        closer_at = False

        while end_pos < len(control_string) and nesting > 0:
            if control_string[end_pos] == '~' and end_pos + 1 < len(control_string):
                j = end_pos + 1
                while j < len(control_string) and control_string[j] in '0123456789,:#@':
                    j += 1
                if j < len(control_string):
                    next_char = control_string[j].upper()
                    seg_colon = ':' in control_string[end_pos + 1:j]
                    seg_at = '@' in control_string[end_pos + 1:j]

                    if next_char == '<':
                        nesting += 1
                        end_pos = j + 1
                    elif next_char == '>':
                        nesting -= 1
                        if nesting == 0:
                            segments.append(control_string[segment_start:end_pos])
                            closer_colon, closer_at = seg_colon, seg_at
                            end_pos = j + 1
                            break
                        nested_logical_closers.append(seg_colon)
                        end_pos = j + 1
                    elif next_char == ';' and nesting == 1:
                        segments.append(control_string[segment_start:end_pos])
                        sep_flags.append((seg_colon, seg_at))
                        segment_start = j + 1
                        end_pos = j + 1
                    else:
                        end_pos = j + 1
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

            # A first segment terminated by `~:;` is not content -- it is
            # the prefix emitted only when the block has to be broken across
            # lines. There is no line-width model for plain justification,
            # so the block is always one line and the prefix is omitted.
            if len(segments) > 1 and sep_flags and sep_flags[0][0]:
                segments = segments[1:]

            # Every segment is output, with padding distributed among the
            # gaps so the whole reaches mincol. All segments share the outer
            # cursor: arguments consumed inside the block must not be
            # re-offered to directives that follow the ~>. Literal-space
            # brackets are stripped here (not left for the top level): this
            # branch never resolves against a margin, so `_justify`'s own
            # width/padding math must see the real character count.
            texts = []
            for seg in segments:
                try:
                    texts.append(_pp_strip_lit_space(_format_process_cursor(seg, cursor)))
                except _FormatEscape as esc:
                    texts.append(_pp_strip_lit_space(esc.partial))
                    break

            return (_justify(texts, params, colon_flag, at_flag), end_pos)

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

        if colon_flag and at_flag:
            # ~:@( ... ~) - force everything to upper case
            result = inner_result.upper()
        elif colon_flag:
            # ~:( ... ~) - capitalize each word
            result = _capitalize_words(inner_result)
        elif at_flag:
            # ~@( ... ~) - capitalize just the first word, lower case the rest
            result = _capitalize_first_word(inner_result)
        else:
            # ~( ... ~) - force everything to lower case
            result = inner_result.lower()

        return (result, end_pos)
    
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
        result = _resolve_pretty_body(result, 0, '', '', False, False)
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
