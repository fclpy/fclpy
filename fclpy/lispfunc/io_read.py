"""I/O read operations - stream input and character reading."""

import fclpy.lisptype as lisptype
import fclpy.readtable as _rt
import fclpy.lispreader as _lispreader
from . import registry as _registry
from .streams import Stream, resolve_input_stream


# The reader control variables are *variables*, and they live where every
# other standard variable does: proclaimed special by
# `lispenv.STANDARD_SPECIAL_VARIABLES` and given their ANSI initial value from
# `lispreader.READER_VARIABLES`. What used to be here instead was a
# `cl_function` named `*READ-BASE*` returning a module global -- plan.md's C7
# defect, a function registered under a variable's name. Nothing read the
# global, and the registration meant a reference to `*read-base*` that fell
# through variable lookup resolved to a *Python function object*. The one
# reader of the variable is `lispreader.resolve_read_base`.


def _supplied_true(value):
    """Truth of an optional argument whose omitted value is false.

    `lisptype.is_truthy(False)` answers True (plan.md S5, owned by M2), so a
    Python `False` cannot simply be handed to it.
    """
    return value is not None and value is not False and lisptype.is_truthy(value)


def _char_of(char, what):
    """The Python character a **character designator** names.

    The readtable is keyed by one-character strings, so a `Character` object --
    which is what `#\\(` evaluates to -- used to miss every entry and answer
    "not a macro character" for characters that plainly are.
    """
    if isinstance(char, lisptype.Character):
        return char.char
    if isinstance(char, str) and len(char) == 1:
        return char
    text = getattr(char, 'value', None)
    if isinstance(text, str) and len(text) == 1:
        return text
    raise lisptype.LispTypeError(
        f"{what}: {char!r} is not a character",
        expected_type="CHARACTER", actual_value=char)


@_registry.cl_function('READTABLEP')
def readtablep(obj):
    """READTABLEP: is `obj` a readtable? (CLHS 23.2)

    This returned NIL unconditionally -- "we don't have readtable objects yet"
    -- long after `Readtable` existed, so `(readtablep *readtable*)` denied the
    very object `*READTABLE*` was bound to.
    """
    return lisptype.lisp_bool(isinstance(obj, _rt.Readtable))


@_registry.cl_function('STREAMP')
def streamp(obj):
    """STREAMP: is `obj` a stream (CLHS 21.1)?

    This answered an inclusive Python heuristic ("has a `read`/`write`/
    `flush` attribute") that happened to catch fclpy `Stream` objects only
    by accident -- `Stream` has a `flush` method, so `hasattr(obj, 'flush')`
    was true, but the heuristic could just as easily answer T for an
    unrelated object that merely happens to define one of those names.
    `Stream` is the one stream object model (streams.py); STREAMP asks it
    directly rather than guessing from shape.
    """
    return lisptype.lisp_bool(isinstance(obj, Stream))


@_registry.cl_function('INPUT-STREAM-P')
def input_stream_p(stream):
    """INPUT-STREAM-P: can `stream` be used for input (CLHS 21.1)?

    Raises:
        LispTypeError if stream is not a stream
    """
    if not isinstance(stream, Stream):
        raise lisptype.LispTypeError(
            f"INPUT-STREAM-P: not a stream: {stream!r}",
            expected_type='STREAM', actual_value=stream)
    return lisptype.lisp_bool(stream.direction in ('input', 'io'))


@_registry.cl_function('INTERACTIVE-STREAM-P')
def interactive_stream_p(stream):
    """INTERACTIVE-STREAM-P: is `stream` connected to an interactive terminal (CLHS 21.1)?

    Raises:
        LispTypeError if stream is not a stream
    """
    if not isinstance(stream, Stream):
        raise lisptype.LispTypeError(
            f"INTERACTIVE-STREAM-P: not a stream: {stream!r}",
            expected_type='STREAM', actual_value=stream)
    isatty = getattr(stream.file_obj, 'isatty', None)
    return lisptype.lisp_bool(bool(isatty and isatty()))


class _StreamFileAdapter:
    """Presents a `streams.Stream` as the `.read(1)`-shaped object
    `lispreader.LispStream` expects, so the reader machinery can read
    through the *same* stream object (and its pushback buffer) that
    READ-CHAR/PEEK-CHAR/UNREAD-CHAR operate on, rather than a third,
    independent character source.
    """
    __slots__ = ('_stream',)

    def __init__(self, stream):
        self._stream = stream

    def read(self, n=1):
        char = self._stream.read_char()
        return char if char is not None else ''


def _reader_bridge(target):
    """A `lispreader.LispStream` reading from input designator `target`."""
    if isinstance(target, Stream):
        return _lispreader.LispStream(_StreamFileAdapter(target))
    if target is not None and hasattr(target, 'read'):
        return _lispreader.LispStream(target)
    return _lispreader.STDIN


def _read_via_reader(stream, eof_error_p, eof_value, what, preserve_whitespace=False):
    """READ / READ-PRESERVING-WHITESPACE's shared body: resolve the stream
    designator, read one form through the existing reader machinery
    (`lispreader.LispReader`), and hand any character it looked ahead past
    the form's end back to the stream so a second call sees it.

    `preserve_whitespace` is CLHS 23.1.2's whole distinction between READ and
    READ-PRESERVING-WHITESPACE: ordinary READ consumes the single whitespace
    character that terminates a token, and READ-PRESERVING-WHITESPACE must
    not. `LispReader.read_8` is the one place a token's terminating character
    is decided, so this only threads the flag there rather than duplicating
    the token loop.
    """
    target = resolve_input_stream(stream)
    bridge = _reader_bridge(target)
    readtable = _rt.get_current_readtable()
    reader = _lispreader.LispReader(readtable, bridge)
    try:
        result = reader.read_1(preserve_whitespace)
    except EOFError:
        # CLHS glossary "eof-error-p": that argument governs only the case
        # where the stream is *already* at end of file before any part of an
        # object has been read -- `read_1` reports that case by returning
        # None, not by raising. An `EOFError` here means a macro-character
        # handler (`_left_paren_reader` et al.) started reading a compound
        # form and ran out of input, which is unconditionally an error no
        # matter what `eof-error-p` says (`(read-from-string "(A B " nil)`
        # must still signal). Conflating the two here used to make a
        # truncated list silently answer NIL instead of erroring.
        raise lisptype.LispEndOfFileError(target, what)
    except _lispreader.ReaderErrorSignal as exc:
        # Malformed input from the token loop -- CLHS 2.1.4.2's invalid
        # constituent trait. Converted here rather than raised there so the
        # condition carries the real stream in its STREAM slot (ansi-test
        # reads it back with `stream-error-stream`), and signalled through
        # `signal_error_object` rather than `raise`d so handlers run at the
        # signal point like any other ERROR -- a bare `raise` of a condition
        # *object* matches no HANDLER-CASE clause at all.
        from .evaluation_conditions import signal_error_object
        return signal_error_object(
            lisptype.ReaderError(stream=target, message=f"{what}: {exc}"))
    if isinstance(target, Stream):
        while bridge.buff:
            target.unread_char(bridge.buff.pop())
    if result is None:
        if _supplied_true(eof_error_p):
            raise lisptype.LispEndOfFileError(target, what)
        return eof_value
    return result


# I/O read operations
@_registry.cl_function('READ-LINE')
def read_line(stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """READ-LINE: read up to (and discard) the next newline (CLHS 21.2).

    Returns two values -- the line, and whether the stream ended before a
    newline was found -- per CLHS, not just the text.
    """
    target = resolve_input_stream(stream)
    if isinstance(target, Stream):
        result = target.read_line()
    elif target is not None and hasattr(target, 'readline'):
        raw = target.readline()
        result = None if not raw else (
            (raw[:-1], False) if raw.endswith('\n') else (raw, True))
    else:
        result = None

    if result is None:
        if _supplied_true(eof_error_p):
            raise lisptype.LispEndOfFileError(target, "READ-LINE")
        return lisptype.MultipleValues(eof_value, lisptype.T)

    text, missing_newline_p = result
    return lisptype.MultipleValues(
        lisptype.LispString(text), lisptype.lisp_bool(missing_newline_p))


@_registry.cl_function('READ-CHAR')
def read_char(stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """READ-CHAR: the next character from `stream` (CLHS 21.2)."""
    target = resolve_input_stream(stream)
    if isinstance(target, Stream):
        char = target.read_char()
    elif target is not None and hasattr(target, 'read'):
        char = target.read(1) or None
    else:
        char = None

    if char is None:
        if _supplied_true(eof_error_p):
            raise lisptype.LispEndOfFileError(target, "READ-CHAR")
        return eof_value
    return lisptype.Character(char)


@_registry.cl_function('READ-BYTE')
def read_byte(stream, eof_error_p=True, eof_value=None):
    """READ-BYTE (CLHS 21.2): read one integer element from a binary stream.

    `OPEN` now honours a binary `:element-type` (streams.py's
    `_classify_element_type` records the per-element byte width and
    signedness on the stream), so this reads exactly that many raw bytes
    per element instead of unconditionally one -- the previous version
    always read a single byte, which was wrong for any element type wider
    than 8 bits (`(unsigned-byte 12)`, `(unsigned-byte 100)`, ...).

    Raises:
        LispTypeError if stream is not a stream
    """
    # stream is required, so it must be a stream (not NIL or T)
    if not isinstance(stream, Stream):
        raise lisptype.LispTypeError(
            f"READ-BYTE: not a stream: {stream!r}",
            expected_type='STREAM', actual_value=stream)

    target = stream
    if isinstance(target, Stream) and target.binary:
        # Use stream's read_byte method if available (e.g., for composite streams)
        if hasattr(target, 'read_byte') and callable(getattr(target, 'read_byte')):
            result = target.read_byte()
            if result is not None:
                return result
        # Fall back to direct file_obj access
        if target.file_obj:
            raw = target.file_obj.read(target.byte_width)
            if raw and len(raw) == target.byte_width:
                return int.from_bytes(raw, 'big', signed=target.byte_signed)
    if _supplied_true(eof_error_p):
        raise lisptype.LispEndOfFileError(target, "READ-BYTE")
    return eof_value


@_registry.cl_function('PEEK-CHAR')
def peek_char(peek_type=None, stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """PEEK-CHAR: look at, without consuming, an upcoming character (CLHS 21.2).

    `peek_type` NIL peeks the very next character; T discards leading
    whitespace first; a character discards input up to and including the
    first occurrence of that character.
    """
    target = resolve_input_stream(stream)

    def _peek():
        if isinstance(target, Stream):
            return target.peek_char()
        return None

    def _consume():
        if isinstance(target, Stream):
            target.read_char()

    def _eof():
        if _supplied_true(eof_error_p):
            raise lisptype.LispEndOfFileError(target, "PEEK-CHAR")
        return eof_value

    skip_char = None
    if isinstance(peek_type, lisptype.Character):
        skip_char = peek_type.char
    elif isinstance(peek_type, str) and len(peek_type) == 1:
        skip_char = peek_type

    if skip_char is not None:
        while True:
            ch = _peek()
            if ch is None:
                return _eof()
            if ch == skip_char:
                return lisptype.Character(ch)
            _consume()
    elif _supplied_true(peek_type):
        # Get current readtable to check syntax types
        readtable = _rt.get_current_readtable()
        while True:
            ch = _peek()
            if ch is None:
                return _eof()
            # Check if character is NOT whitespace according to the readtable's syntax types
            syntax = readtable.syntax_type(ch)
            if syntax != _rt.SYNTAX_WHITESPACE:
                return lisptype.Character(ch)
            _consume()
    else:
        ch = _peek()
        if ch is None:
            return _eof()
        return lisptype.Character(ch)


@_registry.cl_function('UNREAD-CHAR')
def unread_char(character, stream=None):
    """UNREAD-CHAR: push the most recently read character back (CLHS 21.2)."""
    target = resolve_input_stream(stream)
    char = character.char if isinstance(character, lisptype.Character) else str(character)[:1]
    if isinstance(target, Stream):
        target.unread_char(char)
    return lisptype.NIL


@_registry.cl_function('LISTEN')
def listen(stream=None):
    """LISTEN: T if a character is immediately available (CLHS 21.2)."""
    target = resolve_input_stream(stream)
    if isinstance(target, Stream):
        return lisptype.lisp_bool(target.listen())
    return lisptype.T


@_registry.cl_function('CLEAR-INPUT')
def clear_input(stream=None):
    """CLEAR-INPUT: discard any buffered input (CLHS 21.2).

    Raises:
        LispTypeError if stream is not a stream, NIL, or T
    """
    # Accept NIL, T, or a stream; anything else is an error
    if stream is not None and stream is not True and stream is not lisptype.NIL and stream is not lisptype.T:
        if not isinstance(stream, Stream):
            raise lisptype.LispTypeError(
                f"CLEAR-INPUT: not a stream: {stream!r}",
                expected_type='(OR STREAM (MEMBER NIL T))', actual_value=stream)

    target = resolve_input_stream(stream)
    if isinstance(target, Stream):
        target._pending = []
    return lisptype.NIL


@_registry.cl_function('READ')
def read(stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """READ: parse one object from `stream` (CLHS 2.1, 23.1)."""
    return _read_via_reader(stream, eof_error_p, eof_value, "READ")


@_registry.cl_function('READ-CHAR-NO-HANG')
def read_char_no_hang(stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """READ-CHAR-NO-HANG (CLHS 21.2).

    None of fclpy's streams are asynchronous, so "would this block" is
    exactly LISTEN's question: a character available is returned
    immediately, and a genuinely exhausted stream degrades to READ-CHAR's
    ordinary end-of-file handling -- never a NIL for a stream partway
    through, which the CLHS-defined NIL-for-"none ready yet" would be if
    there were real asynchronous input to distinguish it from.
    """
    target = resolve_input_stream(stream)
    if isinstance(target, Stream) and target.listen():
        return lisptype.Character(target.read_char())
    if _supplied_true(eof_error_p):
        raise lisptype.LispEndOfFileError(target, "READ-CHAR-NO-HANG")
    return eof_value


@_registry.cl_function('READ-DELIMITED-LIST')
def read_delimited_list(char, stream=None, recursive_p=None):
    """READ-DELIMITED-LIST: read forms up to and consuming `char` (CLHS 22.4.7)."""
    target = resolve_input_stream(stream)
    delim = char.char if isinstance(char, lisptype.Character) else str(char)[:1]
    readtable = _rt.get_current_readtable()
    forms = []
    while True:
        ch = target.peek_char() if isinstance(target, Stream) else None
        if ch is None:
            raise lisptype.LispEndOfFileError(target, "READ-DELIMITED-LIST")
        if ch == delim:
            target.read_char()
            break
        bridge = _reader_bridge(target)
        reader = _lispreader.LispReader(readtable, bridge)
        forms.append(reader.read_1())
        while bridge.buff:
            target.unread_char(bridge.buff.pop())
    result = lisptype.NIL
    for form in reversed(forms):
        result = lisptype.lispCons(form, result)
    return result


@_registry.cl_function('READ-FROM-STRING')
def read_from_string(string, eof_error_p=True, eof_value=None, *,
                     start=0, end=None, preserve_whitespace=None):
    """READ-FROM-STRING: read one form from `string` (CLHS 23.2).

    CLHS defines this as reading "as if" from a string input stream, so that
    is literally how it is implemented: a `StringInputStream` over the
    bounding indices, read through `_read_via_reader` -- the same body READ
    uses. It had its own copy of that plumbing (build an `io.StringIO`, build
    a reader, call `read_1`), and the copy differed in two ways that mattered:

    * `io.StringIO(substring)` raised ``TypeError: initial_value must be str
      or None, not LispString`` for a Lisp string, because slicing a
      `LispString` yields another `LispString`. Every `(read-from-string s)`
      on a string that had come from the reader was therefore a Python
      exception surfacing as the form's value.
    * it returned one value where CLHS requires two -- the object and the
      index reading stopped at.

    A third defect, found afterward: `string` is typed as STRING (CLHS
    23.2), and CLAUDE.md's array model gives a string three representations
    -- `str`, `LispString`, and a rank-1 character `LispArray` (what a
    displaced, adjustable or fill-pointered string actually is). The
    `isinstance(string, str)` check here saw only the first two and fell
    back to `str(string)` for the third, which is Python's object-repr
    fallback, not the array's characters -- so
    `(read-from-string displaced-string)` read the text
    `"<LISPARRAY ...>"` instead of what the array held.
    `comparison._string_characters` is the one place that already resolves
    all three representations (built for EQUAL/EQUALP, plan.md Finding I);
    this reuses it rather than adding a fourth copy.
    """
    from .streams import StringInputStream
    from .comparison import _string_characters

    text = _string_characters(string)
    if text is None:
        raise lisptype.LispTypeError(
            f"READ-FROM-STRING: {type(string).__name__} is not a string",
            expected_type="STRING", actual_value=string)
    stop = len(text) if end is None or end is lisptype.NIL else int(end)
    begin = 0 if start is None or start is lisptype.NIL else int(start)

    stream = StringInputStream(text, begin, stop)
    result = _read_via_reader(stream, eof_error_p, eof_value, "READ-FROM-STRING",
                              preserve_whitespace=_supplied_true(preserve_whitespace))
    return lisptype.MultipleValues(result, begin + stream.position)


@_registry.cl_function('READ-PRESERVING-WHITESPACE')
def read_preserving_whitespace(stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """READ-PRESERVING-WHITESPACE (CLHS 2.1, 23.1.2).

    Shares READ's reader-bridge plumbing, and now also the one thing that
    distinguishes it from READ: `LispReader.read_8` leaves the single
    whitespace character that terminates a token on the stream instead of
    consuming it, so a subsequent READ-CHAR sees it.
    """
    return _read_via_reader(stream, eof_error_p, eof_value,
                            "READ-PRESERVING-WHITESPACE", preserve_whitespace=True)


# MAKE-STRING-INPUT-STREAM is registered exactly once, in streams.py next to
# the StringInputStream object model. The thin io_read.py delegate that used
# to compete for the name is removed (standing rule 3 -- two registrations
# mean import order, not correctness, decides which runs). Re-exported here so
# `from .io_read import *` importers keep working.
from .streams import make_string_input_stream  # noqa: F401  -- re-export


# Every operator below resolves its readtable argument through the one
# `coerce_to_readtable`, so NIL means the standard readtable in all of them
# (CLHS glossary, "readtable designator") and an omitted argument means the
# current one. `_OMITTED` is what tells those two apart -- a plain `=None`
# default cannot, and that is why every one of these used to raise on NIL.

@_registry.cl_function('COPY-READTABLE')
def copy_readtable(from_readtable=_rt._OMITTED, to_readtable=_rt._OMITTED):
    """COPY-READTABLE: copy `from-readtable` (CLHS 23.2).

    NIL as `from-readtable` designates the *standard* readtable, which is what
    makes `(copy-readtable nil)` -- the basis of every
    `with-standard-io-syntax`-style form, including ansi-test's own
    `my-with-standard-io-syntax` -- answer standard syntax.
    """
    source = _rt.coerce_to_readtable(from_readtable, 'COPY-READTABLE')
    target = to_readtable
    if target is _rt._OMITTED or target is None or target is lisptype.NIL \
            or isinstance(target, lisptype.lispNull):
        return source.copy()
    if not isinstance(target, _rt.Readtable):
        raise lisptype.LispTypeError(
            f"COPY-READTABLE: {type(target).__name__} is not a readtable",
            expected_type="READTABLE", actual_value=target)
    return source.copy_into(target)


@_registry.cl_function('READTABLE-CASE')
def readtable_case(readtable=_rt._OMITTED):
    """READTABLE-CASE: the case sensitivity mode, as a keyword (CLHS 23.2).

    This answered the Python string `'UPCASE'`, which is a Python object
    appearing as a Lisp value (standing rule 2) and is not `EQ` to the
    `:UPCASE` every caller compares it against.
    """
    table = _rt.coerce_to_readtable(readtable, 'READTABLE-CASE')
    return _rt.case_keyword(table.readtable_case())


@_registry.cl_function('SET-READTABLE-CASE')
def set_readtable_case(readtable, mode):
    """`(setf (readtable-case rt) mode)` (CLHS 23.2).

    SETF reaches a place named by a function through a `SET-<name>` function
    when no expander is registered, so this is the writer half of
    READTABLE-CASE rather than a sixth entry in the place ladder (M5).
    """
    table = _rt.coerce_to_readtable(readtable, 'SETF READTABLE-CASE')
    table.set_readtable_case(_rt.case_from_designator(mode, 'SETF READTABLE-CASE'))
    return mode


@_registry.cl_function('GET-MACRO-CHARACTER')
def get_macro_character(char, readtable=_rt._OMITTED):
    """GET-MACRO-CHARACTER: the reader macro function and its terminating flag.

    CLHS 23.2 gives this **two values**: the function (or NIL) and
    non-terminating-p. The readtable stores them as a Python 2-tuple, which
    must not be handed back as the value of the form (standing rule 2).
    """
    table = _rt.coerce_to_readtable(readtable, 'GET-MACRO-CHARACTER')
    entry = table.get_macro_character(_char_of(char, 'GET-MACRO-CHARACTER'))
    if entry is None:
        return lisptype.MultipleValues(lisptype.NIL, lisptype.NIL)
    function, non_terminating_p = entry
    return lisptype.MultipleValues(
        function, lisptype.lisp_bool(non_terminating_p))


@_registry.cl_function('SET-MACRO-CHARACTER')
def set_macro_character(char, function, non_terminating_p=None, readtable=_rt._OMITTED):
    """Set macro character function."""
    table = _rt.coerce_to_readtable(readtable, 'SET-MACRO-CHARACTER')
    table.set_macro_character(
        _char_of(char, 'SET-MACRO-CHARACTER'), function,
        _supplied_true(non_terminating_p))
    return lisptype.T  # CLHS 23.2: SET-MACRO-CHARACTER returns T


@_registry.cl_function('GET-DISPATCH-MACRO-CHARACTER')
def get_dispatch_macro_character(disp_char, sub_char, readtable=_rt._OMITTED):
    """Get dispatch macro character function."""
    table = _rt.coerce_to_readtable(readtable, 'GET-DISPATCH-MACRO-CHARACTER')
    function = table.get_dispatch_macro_character(
        _char_of(disp_char, 'GET-DISPATCH-MACRO-CHARACTER'),
        _char_of(sub_char, 'GET-DISPATCH-MACRO-CHARACTER').upper())
    # "No function" is NIL, not Python None (standing rule 2).
    return lisptype.NIL if function is None else function


@_registry.cl_function('SET-DISPATCH-MACRO-CHARACTER')
def set_dispatch_macro_character(disp_char, sub_char, function, readtable=_rt._OMITTED):
    """Set dispatch macro character."""
    table = _rt.coerce_to_readtable(readtable, 'SET-DISPATCH-MACRO-CHARACTER')
    table.set_dispatch_macro_character(
        _char_of(disp_char, 'SET-DISPATCH-MACRO-CHARACTER'),
        _char_of(sub_char, 'SET-DISPATCH-MACRO-CHARACTER').upper(), function)
    return lisptype.T  # CLHS 23.2: SET-DISPATCH-MACRO-CHARACTER returns T


@_registry.cl_function('MAKE-DISPATCH-MACRO-CHARACTER')
def make_dispatch_macro_character(char, non_terminating_p=None, readtable=_rt._OMITTED):
    """Make character into dispatch macro character."""
    table = _rt.coerce_to_readtable(readtable, 'MAKE-DISPATCH-MACRO-CHARACTER')
    # Our simplified Readtable doesn't expose a dedicated creator; emulate by
    # registering a placeholder sharp reader if needed and marking non-terminating.
    table.set_macro_character(
        _char_of(char, 'MAKE-DISPATCH-MACRO-CHARACTER'),
        lambda c, s: None, _supplied_true(non_terminating_p))
    return lisptype.T


@_registry.cl_function('SET-SYNTAX-FROM-CHAR')
def set_syntax_from_char(to_char, from_char, to_readtable=_rt._OMITTED,
                         from_readtable=_rt._OMITTED):
    """SET-SYNTAX-FROM-CHAR (CLHS 23.2): give `to_char` the syntax type
    `from_char` has, and return T.

    This was a stub that returned T and did nothing, because there was no
    character *syntax type* model for it to write to -- `lispreader` decided
    whitespace/escape/constituent with hardcoded literals. `Readtable.
    syntax_type`/`set_syntax_type` is now that model and the reader reads it,
    so this is a real operation.

    Two details CLHS is explicit about:

    * **The constituent traits of `to_char` are not affected.** They belong to
      the character, not the readtable (`readtable.constituent_trait`), so
      making `#\\Tab` a constituent exposes Tab's *invalid* trait and reading
      it signals READER-ERROR, while the same operation on `#\\\\` yields the
      symbol named "\\". `set-syntax-from-char.lsp` measures exactly that
      difference.
    * **A macro character's function is copied too**, so
      `(set-syntax-from-char c #\\()` makes `c` open a list.

    `from_readtable` defaults to the *standard* readtable, not to the current
    one -- that is what makes the operation reset a character to standard
    syntax, and NIL denotes the standard readtable here as everywhere.
    """
    target = _rt.coerce_to_readtable(to_readtable, 'SET-SYNTAX-FROM-CHAR')
    source = _rt.coerce_to_readtable(
        from_readtable, 'SET-SYNTAX-FROM-CHAR',
        default=_rt.standard_readtable())
    to_c = _char_of(to_char, 'SET-SYNTAX-FROM-CHAR')
    from_c = _char_of(from_char, 'SET-SYNTAX-FROM-CHAR')

    syntax = source.syntax_type(from_c)
    macro = source.get_macro_character(from_c)
    function = macro[0] if macro is not None else None
    target.set_syntax_type(to_c, syntax, function)
    # A dispatch macro character carries its sub-character table with it;
    # otherwise `(set-syntax-from-char c #\#)` would make `c` dispatch and
    # then find no `#\(`/`#\'`/... handlers under it, which is what
    # `set-syntax-from-char.sharp.1` reads through.
    sub_table = source._dispatch_macro_characters.get(from_c)
    if sub_table:
        for sub, fn in sub_table.items():
            target.set_dispatch_macro_character(to_c, sub, fn)
    return lisptype.T


# NOTE: `*READTABLE*` is a *variable*, bound in `lispenv.py` and living in the
# symbol's value cell like every other special. It used to also be registered
# here as a `cl_function` under the variable's own name -- the defect plan.md
# C7 describes for `*PRINT-BASE*`, where registering a function under a
# variable's name is what made the variable evaluate to a Python function
# object. `readtable.get_current_readtable()` now reads that value cell, so
# there is one home and no accessor to keep in step with it.


# NOTE: the real macro expander lives in evaluation_special_forms.py.
# This module-level stub neither evaluated its body nor created a stream,
# and register_module() would auto-register it as a *function* (its Python
# name differs from the expander's, so the decorator dedup misses it),
# clobbering the macro depending on import order -- standing rule 3.


__all__ = [
    'readtablep', 'streamp', 'input_stream_p', 'interactive_stream_p',
    'read_line', 'read_char', 'read_byte', 'peek_char', 'unread_char',
    'listen', 'clear_input', 'read', 'read_char_no_hang',
    'read_delimited_list', 'read_from_string', 'read_preserving_whitespace',
    'make_string_input_stream',
    'copy_readtable', 'readtable_case', 'set_readtable_case',
    'get_macro_character', 'set_macro_character',
    'get_dispatch_macro_character', 'set_dispatch_macro_character',
    'make_dispatch_macro_character', 'set_syntax_from_char',
]
