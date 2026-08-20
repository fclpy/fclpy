"""File and stream I/O for Phase 5 Task 5."""

import io as _io
import os
import sys
import fclpy.lisptype as lisptype
from . import registry as _registry


# === Standard Stream Variables ===
#
# The standard streams are *variables*, bound to `Stream` objects in
# `lispenv.setup_standard_environment`. There used to be
# `@cl_function('*STANDARD-OUTPUT*')`-style accessors here returning raw
# `sys.stdout`; they are gone for the same two reasons the printer control
# variable accessors are. Registering a function under a variable's name is
# what the evaluator falls back to when a symbol has no value, so the accessor
# turns "this variable is unbound" into "this variable's value is a Python
# function"; and returning `sys.stdout` puts a Python file object into Lisp as
# a stream, which every stream operation then has to special-case.


class Stream:
    """A Stream object wrapping file handles or string streams."""
    
    def __init__(self, name, file_obj, direction, element_type='character'):
        """Initialize a stream.

        Args:
            name: Name/path of the stream (string)
            file_obj: Python file object or io.IOBase
            direction: 'input', 'output', or 'io'
            element_type: 'character' or 'byte'
        """
        self.name = name
        self.file_obj = file_obj
        self.direction = direction
        self.element_type = element_type
        self.open_p = True
        self.position = 0  # Track current position
        # LIFO pushback buffer for PEEK-CHAR/UNREAD-CHAR/LISTEN. CLHS only
        # requires depth 1, but a stack costs nothing extra and lets a
        # reader-bridge drain more than one character back onto the stream
        # (see io_read.py's READ) without a special case.
        self._pending = []

    def read_char(self):
        """Read a single character."""
        if not self.open_p:
            raise lisptype.LispStreamError(stream=self, message=f"Stream {self.name} is closed")
        if self.direction not in ('input', 'io'):
            raise lisptype.LispStreamError(stream=self, message=f"Stream {self.name} is not open for input")

        if self._pending:
            self.position += 1
            return self._pending.pop()
        char = self.file_obj.read(1)
        if char:
            self.position += 1
            return char
        return None

    def peek_char(self):
        """Look at the next character without consuming it (CLHS 21.2)."""
        if not self.open_p:
            raise lisptype.LispStreamError(stream=self, message=f"Stream {self.name} is closed")
        if self.direction not in ('input', 'io'):
            raise lisptype.LispStreamError(stream=self, message=f"Stream {self.name} is not open for input")

        if self._pending:
            return self._pending[-1]
        char = self.file_obj.read(1)
        if char:
            self._pending.append(char)
            return char
        return None

    def unread_char(self, char):
        """Push `char` back so the next READ-CHAR returns it again (CLHS 21.2)."""
        self._pending.append(char)
        self.position = max(0, self.position - 1)

    def listen(self):
        """T if a character is available without blocking (CLHS 21.2).

        There is no real asynchronous I/O here, so "available" means the
        underlying object can answer a read without blocking -- true for a
        seekable file or a string buffer, which is everything the ANSI
        suite actually drives through LISTEN. A non-seekable stream (e.g. a
        real TTY) has no non-blocking primitive available, so it is assumed
        ready rather than risking a hang.
        """
        if not self.open_p or self.direction not in ('input', 'io'):
            return False
        if self._pending:
            return True
        if not getattr(self.file_obj, 'seekable', lambda: False)():
            return True
        char = self.file_obj.read(1)
        if char:
            self._pending.append(char)
            return True
        return False

    def read_line(self):
        """Read a line, discarding the terminating newline (CLHS 21.2).

        Returns `(text, missing_newline_p)`, or `None` at end of file with
        nothing read at all -- `missing_newline_p` is true exactly when the
        stream ended before a newline was seen, which READ-LINE must return
        as its second value.
        """
        if not self.open_p:
            raise lisptype.LispStreamError(stream=self, message=f"Stream {self.name} is closed")
        if self.direction not in ('input', 'io'):
            raise lisptype.LispStreamError(stream=self, message=f"Stream {self.name} is not open for input")

        chars = []
        if self._pending:
            first = self._pending.pop()
            self.position += 1
            if first == '\n':
                return ('', False)
            chars.append(first)

        while True:
            char = self.file_obj.read(1)
            if not char:
                if not chars:
                    return None
                self.position += len(chars)
                return (''.join(chars), True)
            self.position += 1
            if char == '\n':
                return (''.join(chars), False)
            chars.append(char)
    
    def read_sequence(self, n=None):
        """Read n characters or until EOF."""
        if not self.open_p:
            raise lisptype.LispStreamError(stream=self, message=f"Stream {self.name} is closed")
        if self.direction not in ('input', 'io'):
            raise lisptype.LispStreamError(stream=self, message=f"Stream {self.name} is not open for input")
        
        if n is None:
            text = self.file_obj.read()
        else:
            text = self.file_obj.read(n)
        
        self.position += len(text)
        return text
    
    def write_char(self, char):
        """Write a single character."""
        if not self.open_p:
            raise lisptype.LispStreamError(stream=self, message=f"Stream {self.name} is closed")
        if self.direction not in ('output', 'io'):
            raise lisptype.LispStreamError(stream=self, message=f"Stream {self.name} is not open for output")
        
        if not isinstance(char, str) or len(char) != 1:
            raise ValueError(f"Expected single character, got {char}")
        
        self.file_obj.write(char)
        self.position += 1
        return char
    
    def write_sequence(self, sequence):
        """Write a sequence of characters/bytes."""
        if not self.open_p:
            raise lisptype.LispStreamError(stream=self, message=f"Stream {self.name} is closed")
        if self.direction not in ('output', 'io'):
            raise lisptype.LispStreamError(stream=self, message=f"Stream {self.name} is not open for output")
        
        if isinstance(sequence, str):
            self.file_obj.write(sequence)
            self.position += len(sequence)
        elif isinstance(sequence, (list, tuple)):
            text = ''.join(str(c) for c in sequence)
            self.file_obj.write(text)
            self.position += len(text)
        else:
            raise ValueError(f"Cannot write {type(sequence)}")
        
        return sequence
    
    def write_line(self, line):
        """Write a line with newline."""
        if not self.open_p:
            raise lisptype.LispStreamError(stream=self, message=f"Stream {self.name} is closed")
        if self.direction not in ('output', 'io'):
            raise lisptype.LispStreamError(stream=self, message=f"Stream {self.name} is not open for output")
        
        self.file_obj.write(str(line) + '\n')
        self.position += len(str(line)) + 1
        return lisptype.NIL
    
    def flush(self):
        """Flush the stream."""
        if self.open_p and hasattr(self.file_obj, 'flush'):
            self.file_obj.flush()
        return lisptype.T
    
    def close(self):
        """Close the stream."""
        if self.open_p:
            self.file_obj.close()
            self.open_p = False
        return lisptype.T
    
    def is_open(self):
        """Check if stream is open."""
        return self.open_p
    
    def get_position(self):
        """Get current position in stream."""
        return self.position


def resolve_input_stream(designator):
    """Resolve an input stream designator (CLHS 21.1.3).

    NIL -- what a missing stream argument defaults to -- designates the
    current value of `*STANDARD-INPUT*`, and T designates `*TERMINAL-IO*`.
    Every input operator has to come through here: they used to read from
    Python's `sys.stdin`/`input()` directly, ignoring both an explicit
    stream argument and any `*STANDARD-INPUT*` binding (e.g. from
    WITH-INPUT-FROM-STRING) -- the same defect `write_text`/
    `resolve_output_stream` (io_write.py) replaced on the output side.
    """
    import fclpy.state as state

    if designator is True or designator is lisptype.T:
        name = '*TERMINAL-IO*'
    elif designator is None or designator is lisptype.NIL:
        name = '*STANDARD-INPUT*'
    else:
        return designator

    env = getattr(state, 'current_environment', None)
    symbol = lisptype.COMMON_LISP_PACKAGE.intern_symbol(name)
    if env is not None and env.has_variable(symbol):
        return env.find_variable(symbol)
    return getattr(symbol, 'value', None)


# Keep track of open streams
_open_streams = {}


@_registry.cl_function('OPEN')
def open_file(filename, direction='input', element_type='character', 
              if_exists='error', if_does_not_exist='error'):
    """Open a file and return a stream.
    
    Args:
        filename: Path to file (string)
        direction: 'input', 'output', 'io', 'probe' (default: 'input')
        element_type: 'character' or 'byte' (default: 'character')
        if_exists: 'error', 'new-version', 'rename', 'supersede', 'append', 'overwrite'
        if_does_not_exist: 'error', 'create'
    
    Returns:
        Stream object or NIL for probe mode
    """
    filename = str(filename)
    import fclpy.lisptype as lisptype
    # Normalize Lisp keyword or symbol arguments to Python strings

    if isinstance(direction, (lisptype.lispKeyword, lisptype.LispSymbol)):
        direction = direction.name.lower().replace('-', '_')
    elif isinstance(direction, str):
        direction = direction.lower()



    try:
        if isinstance(if_does_not_exist, (lisptype.lispKeyword, lisptype.LispSymbol)):
            if_does_not_exist = if_does_not_exist.name.lower().replace('-', '_')
        elif isinstance(if_does_not_exist, str):
            if_does_not_exist = if_does_not_exist.lower()
    except Exception:
        pass
    # Resolve the filename through `pathnames.resolve_filespec`, the one place
    # a pathname designator becomes an OS path. OPEN carried the fifth copy of
    # that search, and its copy differed: it took the LISP_CWD candidate
    # *unconditionally*, while every other copy took it only when the candidate
    # existed. So OPEN and PROBE-FILE/DELETE-FILE could resolve the same
    # relative name to two different files -- which is how
    # `files/rename-file.lsp` saw `delete-all-versions` delete one file and
    # then `(with-open-file (s pn1 :direction :output) ...)` refuse because
    # *another* file of that name still existed.
    from fclpy.lispfunc.pathnames import resolve_filespec
    filename = resolve_filespec(filename)


    # Map Lisp direction to Python mode
    if direction == 'input':
        mode = 'r'
    elif direction == 'output':
        mode = 'w'
    elif direction == 'io':
        mode = 'r+'
    elif direction == 'probe':
        # Probe mode: check if file exists without opening
        if os.path.exists(filename):
            return lisptype.T
        return lisptype.NIL
    else:
        raise ValueError(f"Invalid direction: {direction}")
    
    # Handle file existence checks. Every refusal here is a FILE-ERROR naming
    # the file (CLHS OPEN), not a Python `FileExistsError`/`FileNotFoundError`:
    # those match no handler clause, so `(handler-case (open ...) (file-error
    # ...))` could not see them and they surfaced as the *value* of the form.
    from fclpy.lispfunc.pathnames import Pathname
    from fclpy.lispfunc.evaluation_conditions import signal_file_error

    if os.path.exists(filename):
        if direction == 'output' and if_exists == 'error':
            return signal_file_error(
                Pathname(filename), "OPEN: file exists: " + filename)
        elif direction == 'output' and if_exists == 'append':
            mode = 'a'
        elif direction == 'output' and if_exists == 'supersede':
            mode = 'w'
    else:
        # If opening for output/io, default to creating the file when it does not exist
        if if_does_not_exist == 'error' and direction in ('output', 'io'):
            if_does_not_exist = 'create'

        if if_does_not_exist == 'error':
            return signal_file_error(
                Pathname(filename), "OPEN: file not found: " + filename)
        elif if_does_not_exist == 'create' and direction in ('output', 'io'):
            # Create the file (opening in 'w' or 'r+' will handle creation)
            pass

    try:
        file_obj = open(filename, mode, encoding='utf-8')
    except OSError as error:
        return signal_file_error(
            Pathname(filename),
            "OPEN: cannot open " + filename + ": " + str(error))
    stream = Stream(filename, file_obj, direction, element_type)
    _open_streams[id(stream)] = stream
    return stream


@_registry.cl_function('CLOSE')
def close_stream(stream, abort=False):
    """Close a stream.
    
    Args:
        stream: Stream to close
        abort: If True, discard output (for output streams)
    
    Returns:
        T
    """
    if isinstance(stream, Stream):
        stream.close()
        if id(stream) in _open_streams:
            del _open_streams[id(stream)]
    return lisptype.T


# READ-CHAR, READ-LINE, WRITE-CHAR and WRITE-LINE used to be registered
# here too, each ignoring both an explicit stream argument default and any
# `*STANDARD-INPUT*`/`*STANDARD-OUTPUT*` binding in favour of Python's
# `sys.stdin`/`input()`/`sys.stdout`. io_read.py and io_write.py already
# defined the *other* copy of each -- io_write.py's WRITE-CHAR/WRITE-LINE
# went through `resolve_output_stream` correctly, and (because that module
# is imported after this one) silently won the registry, leaving these dead;
# io_read.py's READ-CHAR/READ-LINE were the broken stdin-only copies and
# *they* won instead. Two names, two winners, neither predictable -- standing
# rule 3. There is now one home for each: io_write.py for the writers,
# io_read.py for the readers, both funnelling through `resolve_input_stream`/
# `resolve_output_stream`.


@_registry.cl_function('WRITE-SEQUENCE')
def write_sequence_stream(sequence, stream=None, start=0, end=None):
    """WRITE-SEQUENCE: write elements of `sequence` to `stream` (CLHS 21.2).

    Routed through `write_text` so a NIL/T/omitted `stream` resolves to
    `*STANDARD-OUTPUT*`/`*TERMINAL-IO*` like every other output operator,
    instead of writing to Python's `sys.stdout` unconditionally.
    """
    if isinstance(sequence, str):
        if end is None:
            end = len(sequence)
        text = sequence[start:end]
    elif isinstance(sequence, (list, tuple)):
        if end is None:
            end = len(sequence)
        text = ''.join(str(c) for c in sequence[start:end])
    else:
        raise TypeError(f"Expected string or list, got {type(sequence)}")

    from .io_write import write_text
    write_text(text, stream)
    return sequence


@_registry.cl_function('FLUSH-OUTPUT')
def flush_output(stream=None):
    """Flush output to a stream.
    
    Args:
        stream: Stream to flush (default: *standard-output*)
    
    Returns:
        NIL
    """
    if stream is None:
        import sys
        sys.stdout.flush()
        return lisptype.NIL
    
    if not isinstance(stream, Stream):
        raise TypeError(f"Expected Stream, got {type(stream)}")
    
    stream.flush()
    return lisptype.NIL


@_registry.cl_function('STREAM-POSITION')
def stream_position(stream):
    """Get the current position in a stream.
    
    Args:
        stream: Stream to query
    
    Returns:
        Integer position
    """
    if not isinstance(stream, Stream):
        raise TypeError(f"Expected Stream, got {type(stream)}")
    
    return stream.get_position()


@_registry.cl_function('SET-STREAM-POSITION')
def set_stream_position(stream, position):
    """Set the current position in a stream.
    
    Args:
        stream: Stream to modify
        position: New position (integer, or :START, :END)
    
    Returns:
        New position, or NIL if positioning not supported
    """
    if not isinstance(stream, Stream):
        raise TypeError(f"Expected Stream, got {type(stream)}")
    
    if hasattr(stream, 'file') and stream.file is not None:
        if position == ':START' or (hasattr(position, 'name') and position.name == 'START'):
            stream.file.seek(0)
            return 0
        elif position == ':END' or (hasattr(position, 'name') and position.name == 'END'):
            stream.file.seek(0, 2)
            return stream.file.tell()
        else:
            stream.file.seek(int(position))
            return int(position)
    return lisptype.NIL


@_registry.cl_function('READ-SEQUENCE')
def read_sequence(sequence, stream, start=0, end=None):
    """Read elements from stream into sequence.
    
    Args:
        sequence: Mutable sequence (string or list) to fill
        stream: Stream to read from
        start: Starting index in sequence (default 0)
        end: Ending index (exclusive, default length of sequence)
    
    Returns:
        Index of position where reading stopped
    """
    if end is None:
        if isinstance(sequence, str):
            end = len(sequence)
        elif isinstance(sequence, list):
            end = len(sequence)
        else:
            end = start
    
    if not isinstance(stream, Stream):
        # Try reading from file-like object
        if hasattr(stream, 'read'):
            chars = stream.read(end - start)
            for i, c in enumerate(chars):
                if start + i >= end:
                    break
                if isinstance(sequence, list):
                    sequence[start + i] = c
            return start + len(chars)
        raise TypeError(f"Expected Stream, got {type(stream)}")
    
    position = start
    while position < end:
        char = stream.read_char()
        if char is None:
            break
        if isinstance(sequence, list):
            sequence[position] = char
        position += 1
    
    return position


@_registry.cl_function('OPEN-STREAM-P')
def open_stream_p(stream):
    """Test if a stream is open.
    
    Args:
        stream: Stream to test
    
    Returns:
        T if open, NIL otherwise
    """
    if isinstance(stream, Stream):
        return lisptype.lisp_bool(stream.is_open())
    return lisptype.NIL


# String Stream Classes for Task 8.8

class StringInputStream(Stream):
    """Input stream that reads from a string."""
    
    def __init__(self, string, start=0, end=None):
        """Create a string input stream.
        
        Args:
            string: The string to read from
            start: Starting index (default 0)
            end: Ending index (default len(string))
        """
        if end is None:
            end = len(string)
        self.string = string[start:end]
        self.position = 0
        # Create a StringIO for the underlying file object
        file_obj = _io.StringIO(self.string)
        super().__init__("<string-input-stream>", file_obj, 'input', 'character')
    
    def read_char(self):
        """Read a single character from the string."""
        if self.position >= len(self.string):
            return None
        char = self.string[self.position]
        self.position += 1
        return char
    
    def unread_char(self, char):
        """Unread a character (put it back)."""
        if self.position > 0:
            self.position -= 1
    
    def peek_char(self):
        """Look at next character without consuming it."""
        if self.position >= len(self.string):
            return None
        return self.string[self.position]
    
    def listen(self):
        """Check if characters are available."""
        return self.position < len(self.string)

    def read_line(self):
        """Read a line, discarding the newline (CLHS 21.2).

        Overridden rather than inherited: the base `Stream.read_line`
        reads through `self.file_obj`, a *second* cursor into the same
        text that this class's `read_char`/`peek_char`/`unread_char`
        never touch -- interleaving the two would desynchronize them.
        Returns `(text, missing_newline_p)`, or `None` at end of file.
        """
        if self.position >= len(self.string):
            return None
        newline_at = self.string.find('\n', self.position)
        if newline_at == -1:
            text = self.string[self.position:]
            self.position = len(self.string)
            return (text, True)
        text = self.string[self.position:newline_at]
        self.position = newline_at + 1
        return (text, False)

    def __repr__(self):
        return f"<StringInputStream pos={self.position} len={len(self.string)}>"


class StringOutputStream(Stream):
    """Output stream that writes to a string."""
    
    def __init__(self, element_type='character'):
        """Create a string output stream.
        
        Args:
            element_type: Type of elements ('character' or 'base-char')
        """
        self._buffer = _io.StringIO()
        super().__init__("<string-output-stream>", self._buffer, 'output', element_type)
    
    def write_char(self, char):
        """Write a single character to the stream."""
        if isinstance(char, lisptype.Character):
            char = chr(char.code)
        elif not isinstance(char, str) or len(char) != 1:
            char = str(char)[:1] if str(char) else ''
        self._buffer.write(char)
        self.position += 1
        return char
    
    def write_sequence(self, sequence):
        """Write a string or sequence to the stream."""
        if isinstance(sequence, str):
            self._buffer.write(sequence)
            self.position += len(sequence)
        elif isinstance(sequence, (list, tuple)):
            text = ''.join(str(c) for c in sequence)
            self._buffer.write(text)
            self.position += len(text)
        return sequence
    
    def get_string(self):
        """Get the accumulated string and clear the buffer."""
        value = self._buffer.getvalue()
        # Reset the buffer for continued use
        self._buffer.seek(0)
        self._buffer.truncate(0)
        self.position = 0
        return value
    
    def peek_string(self):
        """Get the accumulated string without clearing the buffer."""
        return self._buffer.getvalue()
    
    def __repr__(self):
        return f"<StringOutputStream len={self.position}>"


class FillPointerOutputStream(Stream):
    r"""An output stream that appends to a fill-pointered string (CLHS 21.2).

    This is the object `(WITH-OUTPUT-TO-STRING (var string) ...)` needs: CLHS
    says output is *appended to* the supplied string, which must have a fill
    pointer, and the form returns the body's values rather than the text. The
    macro used to bind `var` to a plain `MAKE-STRING-OUTPUT-STREAM` and then
    never transfer its contents anywhere, so every byte written in the body
    went into a stream nobody read.

    That was a **measurement gate**, not just a wrong value: the ANSI suite
    captures an operator's output with exactly this form and then asserts
    about it, so any test of what something *prints* compared its expectation
    against the empty string -- `load-file-test` and `compile-file-test` both
    check `(> (length str) 0)` and `(position #\; str)` -- and no amount of
    correct printing could pass.

    Appending as the body runs (rather than copying at the end) is also the
    semantics: the text written so far must already be in the string if the
    body exits non-locally.
    """

    def __init__(self, target, element_type='character'):
        if not isinstance(target, lisptype.LispString):
            raise lisptype.LispTypeError(
                f"WITH-OUTPUT-TO-STRING: {target!r} is not a string",
                expected_type='STRING', actual_value=target)
        if target.fill_pointer is None:
            # CLHS: the string "must be a string with a fill pointer".
            raise lisptype.LispTypeError(
                "WITH-OUTPUT-TO-STRING: the string must have a fill pointer",
                expected_type='STRING', actual_value=target)
        self.target = target
        super().__init__("<fill-pointer-output-stream>", None, 'output',
                         element_type)

    def write_char(self, char):
        if isinstance(char, lisptype.Character):
            char = chr(char.code)
        self.write_sequence(str(char))
        return char

    def write_sequence(self, sequence):
        if isinstance(sequence, lisptype.Character):
            text = chr(sequence.code)
        elif isinstance(sequence, (list, tuple)):
            text = ''.join(
                chr(c.code) if isinstance(c, lisptype.Character) else str(c)
                for c in sequence)
        else:
            text = str(sequence)
        for char in text:
            # Append past the fill pointer, growing the backing store: the
            # string is adjustable in every use the suite makes of it, and
            # `_data`/`fill_pointer` is the same pair VECTOR-PUSH-EXTEND moves.
            if self.target.fill_pointer < len(self.target._data):
                self.target._data[self.target.fill_pointer] = char
            else:
                self.target._data.append(char)
            self.target.fill_pointer += 1
            self.position += 1
        return sequence

    def peek_string(self):
        return str(self.target)

    def __repr__(self):
        return f"<FillPointerOutputStream len={self.position}>"


@_registry.cl_function('%MAKE-FILL-POINTER-OUTPUT-STREAM')
def make_fill_pointer_output_stream(target):
    """The stream `(WITH-OUTPUT-TO-STRING (var string) ...)` expands to.

    Named with a `%` prefix because it is not an ANSI operator -- it is the
    macro's runtime, the same way `%SPECIAL-REF` is a declaration's runtime.
    """
    return FillPointerOutputStream(target)


@_registry.cl_function('MAKE-STRING-INPUT-STREAM')
def make_string_input_stream(string, start=0, end=None):
    """Create a string input stream.
    
    Creates an input stream from which characters can be read.
    The characters are taken from the string between start and end.
    
    Args:
        string: The string to read from
        start: Starting index (default 0)
        end: Ending index (default len(string))
    
    Returns:
        A StringInputStream object
    
    Example:
        (make-string-input-stream \"hello world\")
        (make-string-input-stream \"hello world\" 0 5)
    """
    # Convert LispString to Python string
    import fclpy.lisptype as lisptype
    if isinstance(string, lisptype.LispString):
        string = str(string)
    elif not isinstance(string, str):
        raise TypeError(f"Expected string, got {type(string)}")
    if end is None:
        end = len(string)
    return StringInputStream(string, start, end)


@_registry.cl_function('MAKE-STRING-OUTPUT-STREAM')
def make_string_output_stream(element_type='character'):
    """Create a string output stream.
    
    Creates an output stream that accumulates characters written to it.
    Use GET-OUTPUT-STREAM-STRING to retrieve the accumulated string.
    
    Args:
        element_type: Type of stream elements (default 'character')
    
    Returns:
        A StringOutputStream object
    
    Example:
        (let ((s (make-string-output-stream)))
          (write-char #\\H s)
          (write-string \"ello\" s)
          (get-output-stream-string s))  ; Returns \"Hello\"
    """
    return StringOutputStream(element_type)


@_registry.cl_function('GET-OUTPUT-STREAM-STRING')
def get_output_stream_string(stream):
    """Get the accumulated string from a string output stream.
    
    Returns all characters that have been written to the stream since
    creation or the last call to GET-OUTPUT-STREAM-STRING.
    After this call, the stream's buffer is cleared.
    
    Args:
        stream: A StringOutputStream
    
    Returns:
        String containing all accumulated characters
    
    Example:
        (let ((s (make-string-output-stream)))
          (format s \"Hello ~A\" \"World\")
          (get-output-stream-string s))  ; Returns \"Hello World\"
    """
    if isinstance(stream, StringOutputStream):
        return stream.get_string()
    elif isinstance(stream, str):
        # Legacy compatibility: if stream is a string, just return it
        return stream
    elif hasattr(stream, 'getvalue'):
        # Python StringIO
        return stream.getvalue()
    else:
        raise TypeError(f"Expected StringOutputStream, got {type(stream)}")


# Stream predicate

@_registry.cl_function('STRING-INPUT-STREAM-P')
def string_input_stream_p(obj):
    """Test if object is a string input stream."""
    return lisptype.lisp_bool(isinstance(obj, StringInputStream))


@_registry.cl_function('STRING-OUTPUT-STREAM-P')
def string_output_stream_p(obj):
    """Test if object is a string output stream."""
    return lisptype.lisp_bool(isinstance(obj, StringOutputStream))
