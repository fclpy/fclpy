"""I/O read operations - stream input and character reading."""

import fclpy.lisptype as lisptype
import fclpy.readtable as _rt
from . import registry as _registry


# === Reader Control Variables ===
# Global read base (default 10)
_read_base = 10


@_registry.cl_function('*READ-BASE*')
def get_read_base():
    """Get the value of *READ-BASE*."""
    return _read_base


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
    """Return True if obj behaves like a Common Lisp stream.

    Criteria (inclusive heuristic):
    - Instance of io.IOBase (covers open file handles, StringIO, BytesIO, etc.)
    - OR has any typical stream method: read / write / readline / readinto / flush.
    This keeps the predicate flexible for user-defined stream-like objects while
    still catching all standard Python I/O objects.
    """
    import io as _io
    if isinstance(obj, _io.IOBase):
        return lisptype.T
    stream_attrs = ("read", "write", "readline", "readinto", "flush")
    return lisptype.lisp_bool(any(hasattr(obj, a) for a in stream_attrs))


@_registry.cl_function('INPUT-STREAM-P')
def input_stream_p(stream):
    """Test if stream is input stream."""
    return lisptype.T  # Simplified


@_registry.cl_function('INTERACTIVE-STREAM-P')
def interactive_stream_p(stream):
    """Test if stream is interactive."""
    return lisptype.T  # Simplified


# I/O read operations
@_registry.cl_function('READ-LINE')
def read_line(stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """Read line from stream."""
    # Simplified implementation
    try:
        line = input()
        return line
    except EOFError:
        if eof_error_p:
            raise lisptype.LispEndOfFileError(stream, "READ-LINE: encountered end of file")
        return eof_value


@_registry.cl_function('READ-CHAR')
def read_char(stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """Read character from stream."""
    try:
        import sys
        char = sys.stdin.read(1)
        return char if char else (eof_value if not eof_error_p else None)
    except:
        if eof_error_p:
            raise lisptype.LispEndOfFileError(stream, "READ-CHAR: encountered end of file")
        return eof_value


@_registry.cl_function('READ-BYTE')
def read_byte(stream, eof_error_p=True, eof_value=None):
    """Read byte from stream."""
    try:
        # Simplified - just return 0
        return 0
    except:
        if eof_error_p:
            raise lisptype.LispEndOfFileError(stream, "READ-BYTE: encountered end of file")
        return eof_value


@_registry.cl_function('PEEK-CHAR')
def peek_char(peek_type=None, stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """Peek at character in stream."""
    # Simplified implementation
    return ' '  # Return space for now


@_registry.cl_function('UNREAD-CHAR')
def unread_char(character, stream=None):
    """Unread character to stream."""
    # Simplified implementation
    return None


@_registry.cl_function('LISTEN')
def listen(stream=None):
    """Test if input is available."""
    return lisptype.T  # Simplified


@_registry.cl_function('CLEAR-INPUT')
def clear_input(stream=None):
    """Clear input from stream."""
    return None


@_registry.cl_function('READ')
def read(stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """Read object from stream."""
    try:
        return input()  # Simplified
    except EOFError:
        if eof_error_p:
            raise lisptype.LispEndOfFileError(stream, "READ: encountered end of file")
        return eof_value


@_registry.cl_function('READ-CHAR-NO-HANG')
def read_char_no_hang(stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """READ-CHAR-NO-HANG: Non-blocking read of a single character or returns eof_value/None."""
    return None  # Simplified placeholder


@_registry.cl_function('READ-DELIMITED-LIST')
def read_delimited_list(char, stream=None, recursive_p=None):
    """READ-DELIMITED-LIST: Read forms until delimiter char (simplified stub)."""
    return []  # Simplified placeholder


@_registry.cl_function('READ-FROM-STRING')
def read_from_string(string, eof_error_p=True, eof_value=None, start=0, end=None, preserve_whitespace=None):
    """READ-FROM-STRING: Parse first form from substring.
    
    Returns the first Lisp object parsed from STRING[START:END].
    Returns two values: the object read and the position where reading stopped.
    """
    import io as _io
    from fclpy import lispreader
    from fclpy.readtable import get_current_readtable
    
    if end is None:
        end = len(string)
    
    substring = string[start:end]
    
    if not substring.strip():
        if eof_error_p:
            raise lisptype.LispEndOfFileError(None, "READ-FROM-STRING: empty string")
        return eof_value
    
    try:
        # Create a stream from the substring
        string_io = _io.StringIO(substring)
        stream = lispreader.LispStream(string_io)
        
        # Create reader using centralized readtable
        readtable = get_current_readtable()
        reader = lispreader.LispReader(readtable.get_macro_character, stream)
        
        # Read one expression
        result = reader.read_1()
        
        if result is None:
            if eof_error_p:
                raise lisptype.LispEndOfFileError(None, "READ-FROM-STRING: unexpected EOF")
            return eof_value
        
        # Return the parsed object
        # Note: In full CL, this returns multiple values (object, position)
        # For now we just return the object
        return result
        
    except EOFError:
        if eof_error_p:
            raise lisptype.LispEndOfFileError(None, "READ-FROM-STRING: unexpected EOF")
        return eof_value


@_registry.cl_function('READ-PRESERVING-WHITESPACE')
def read_preserving_whitespace(stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """READ-PRESERVING-WHITESPACE: Like READ but preserves whitespace (stub)."""
    try:
        return input()  # Simplified
    except EOFError:
        if eof_error_p:
            raise lisptype.LispEndOfFileError(stream, "READ-PRESERVING-WHITESPACE: encountered end of file")
        return eof_value


@_registry.cl_function('MAKE-STRING-INPUT-STREAM')
def make_string_input_stream(string, start=0, end=None):
    """Make string input stream - delegates to streams.py."""
    from .streams import make_string_input_stream as _make_sis
    return _make_sis(string, start, end)


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
def set_syntax_from_char(to_char, from_char, to_readtable=None, from_readtable=None):
    """Set syntax from another character in a readtable."""
    # Placeholder implementation
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
