"""I/O read operations - stream input and character reading."""

import fclpy.lisptype as lisptype
from . import registry as _registry


# === Reader Control Variables ===
# Global read base (default 10)
_read_base = 10


@_registry.cl_function('*READ-BASE*')
def get_read_base():
    """Get the value of *READ-BASE*."""
    return _read_base


@_registry.cl_function('READTABLEP')
def readtablep(obj):
    """Test if object is a readtable."""
    # For now, we don't have readtable objects yet; return NIL
    return lisptype.NIL


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
    """READ-FROM-STRING: Parse first form from substring; simplified returns slice."""
    if end is None:
        end = len(string)
    return string[start:end]  # Simplified placeholder


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


@_registry.cl_function('COPY-READTABLE')
def copy_readtable(from_readtable=None, to_readtable=None):
    """Copy readtable."""
    from ..readtable import get_current_readtable
    if from_readtable is None:
        from_readtable = get_current_readtable()
    # Use the built-in copy method on Readtable
    return from_readtable.copy()


@_registry.cl_function('READTABLE-CASE')
def readtable_case(readtable=None):
    """Get readtable case."""
    from ..readtable import get_current_readtable
    if readtable is None:
        readtable = get_current_readtable()
    return readtable.readtable_case()


@_registry.cl_function('GET-MACRO-CHARACTER')
def get_macro_character(char, readtable=None):
    """Get macro character function."""
    from ..readtable import get_current_readtable
    if readtable is None:
        readtable = get_current_readtable()
    return readtable.get_macro_character(char)


@_registry.cl_function('SET-MACRO-CHARACTER')
def set_macro_character(char, function, non_terminating_p=None, readtable=None):
    """Set macro character function."""
    from ..readtable import get_current_readtable
    if readtable is None:
        readtable = get_current_readtable()
    return readtable.set_macro_character(char, function, non_terminating_p or False)


@_registry.cl_function('GET-DISPATCH-MACRO-CHARACTER')
def get_dispatch_macro_character(disp_char, sub_char, readtable=None):
    """Get dispatch macro character function."""
    from ..readtable import get_current_readtable
    if readtable is None:
        readtable = get_current_readtable()
    return readtable.get_dispatch_macro_character(disp_char, sub_char)


@_registry.cl_function('SET-DISPATCH-MACRO-CHARACTER')
def set_dispatch_macro_character(disp_char, sub_char, function, readtable=None):
    """Set dispatch macro character."""
    from ..readtable import get_current_readtable
    if readtable is None:
        readtable = get_current_readtable()
    return readtable.set_dispatch_macro_character(disp_char, sub_char, function)


@_registry.cl_function('MAKE-DISPATCH-MACRO-CHARACTER')
def make_dispatch_macro_character(char, non_terminating_p=False, readtable=None):
    """Make character into dispatch macro character."""
    from ..readtable import get_current_readtable
    if readtable is None:
        readtable = get_current_readtable()
    # Our simplified Readtable doesn't expose a dedicated creator; emulate by
    # registering a placeholder sharp reader if needed and marking non-terminating.
    readtable.set_macro_character(char, lambda c, s: None, not non_terminating_p)
    return lisptype.T


@_registry.cl_function('SET-SYNTAX-FROM-CHAR')
def set_syntax_from_char(to_char, from_char, to_readtable=None, from_readtable=None):
    """Set syntax from another character in a readtable."""
    # Placeholder implementation
    return lisptype.T


def with_input_from_string(var_string_options, *body):
    """Execute with input from string."""
    # Simplified - just execute body
    result = None
    for form in body:
        result = form
    return result


__all__ = [
    'readtablep', 'streamp', 'input_stream_p', 'interactive_stream_p',
    'read_line', 'read_char', 'read_byte', 'peek_char', 'unread_char',
    'listen', 'clear_input', 'read', 'read_char_no_hang',
    'read_delimited_list', 'read_from_string', 'read_preserving_whitespace',
    'make_string_input_stream',
    'copy_readtable', 'readtable_case',
    'get_macro_character', 'set_macro_character',
    'get_dispatch_macro_character', 'set_dispatch_macro_character',
    'make_dispatch_macro_character', 'set_syntax_from_char',
    'with_input_from_string'
]
