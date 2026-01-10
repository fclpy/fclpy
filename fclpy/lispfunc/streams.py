"""File and stream I/O for Phase 5 Task 5."""

import io as _io
import os
import sys
import fclpy.lisptype as lisptype
from . import registry as _registry


# === Standard Stream Variables ===
# These provide Lisp-level access to standard I/O streams

@_registry.cl_function('*STANDARD-INPUT*')
def get_standard_input():
    """Get the value of *STANDARD-INPUT*."""
    return sys.stdin


@_registry.cl_function('*STANDARD-OUTPUT*')
def get_standard_output():
    """Get the value of *STANDARD-OUTPUT*."""
    return sys.stdout


@_registry.cl_function('*ERROR-OUTPUT*')
def get_error_output():
    """Get the value of *ERROR-OUTPUT*."""
    return sys.stderr


@_registry.cl_function('*TERMINAL-IO*')
def get_terminal_io():
    """Get the value of *TERMINAL-IO* (combined terminal stream)."""
    # Return stdout as a simple approximation
    return sys.stdout


@_registry.cl_function('*QUERY-IO*')
def get_query_io():
    """Get the value of *QUERY-IO* (query/response stream)."""
    return sys.stdout


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
    
    def read_char(self):
        """Read a single character."""
        if not self.open_p:
            raise ValueError(f"Stream {self.name} is closed")
        if self.direction not in ('input', 'io'):
            raise ValueError(f"Stream {self.name} is not open for input")
        
        char = self.file_obj.read(1)
        if char:
            self.position += 1
            return char
        return None
    
    def read_line(self):
        """Read a line (up to newline, not including it)."""
        if not self.open_p:
            raise ValueError(f"Stream {self.name} is closed")
        if self.direction not in ('input', 'io'):
            raise ValueError(f"Stream {self.name} is not open for input")
        
        line = self.file_obj.readline()
        if line:
            # Remove trailing newline if present
            if line.endswith('\n'):
                line = line[:-1]
            self.position += len(line) + 1
            return line
        return None
    
    def read_sequence(self, n=None):
        """Read n characters or until EOF."""
        if not self.open_p:
            raise ValueError(f"Stream {self.name} is closed")
        if self.direction not in ('input', 'io'):
            raise ValueError(f"Stream {self.name} is not open for input")
        
        if n is None:
            text = self.file_obj.read()
        else:
            text = self.file_obj.read(n)
        
        self.position += len(text)
        return text
    
    def write_char(self, char):
        """Write a single character."""
        if not self.open_p:
            raise ValueError(f"Stream {self.name} is closed")
        if self.direction not in ('output', 'io'):
            raise ValueError(f"Stream {self.name} is not open for output")
        
        if not isinstance(char, str) or len(char) != 1:
            raise ValueError(f"Expected single character, got {char}")
        
        self.file_obj.write(char)
        self.position += 1
        return char
    
    def write_sequence(self, sequence):
        """Write a sequence of characters/bytes."""
        if not self.open_p:
            raise ValueError(f"Stream {self.name} is closed")
        if self.direction not in ('output', 'io'):
            raise ValueError(f"Stream {self.name} is not open for output")
        
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
            raise ValueError(f"Stream {self.name} is closed")
        if self.direction not in ('output', 'io'):
            raise ValueError(f"Stream {self.name} is not open for output")
        
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
    
    # Handle file existence checks
    if os.path.exists(filename):
        if direction == 'output' and if_exists == 'error':
            raise FileExistsError(f"File exists: {filename}")
        elif direction == 'output' and if_exists == 'append':
            mode = 'a'
        elif direction == 'output' and if_exists == 'supersede':
            mode = 'w'
    else:
        if if_does_not_exist == 'error':
            raise FileNotFoundError(f"File not found: {filename}")
        elif if_does_not_exist == 'create' and direction in ('output', 'io'):
            # Create the file
            pass
    
    try:
        file_obj = open(filename, mode, encoding='utf-8')
        stream = Stream(filename, file_obj, direction, element_type)
        _open_streams[id(stream)] = stream
        return stream
    except IOError as e:
        raise IOError(f"Cannot open {filename}: {e}")


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


@_registry.cl_function('READ-CHAR')
def read_char_stream(stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """Read a character from a stream.
    
    Args:
        stream: Stream to read from (default: *standard-input*)
        eof_error_p: If True, error on EOF; else return eof_value
        eof_value: Value to return on EOF (default: NIL)
        recursive_p: For recursive read (unused for now)
    
    Returns:
        Character string or eof_value
    """
    if stream is None:
        # Use stdin
        try:
            import sys
            char = sys.stdin.read(1)
            return char if char else (eof_value if not eof_error_p else None)
        except EOFError:
            if eof_error_p:
                raise lisptype.LispEndOfFileError("*standard-input*", "READ-CHAR")
            return eof_value
    
    if not isinstance(stream, Stream):
        raise TypeError(f"Expected Stream, got {type(stream)}")
    
    char = stream.read_char()
    if char is None:
        if eof_error_p:
            raise lisptype.LispEndOfFileError(stream.name, "READ-CHAR")
        return eof_value
    return char


@_registry.cl_function('READ-LINE')
def read_line_stream(stream=None, eof_error_p=True, eof_value=None, recursive_p=None):
    """Read a line from a stream.
    
    Args:
        stream: Stream to read from (default: *standard-input*)
        eof_error_p: If True, error on EOF; else return eof_value
        eof_value: Value to return on EOF (default: NIL)
    
    Returns:
        Line string (without newline) or eof_value
    """
    if stream is None:
        # Use stdin
        try:
            line = input()
            return line
        except EOFError:
            if eof_error_p:
                raise lisptype.LispEndOfFileError("*standard-input*", "READ-LINE")
            return eof_value
    
    if not isinstance(stream, Stream):
        raise TypeError(f"Expected Stream, got {type(stream)}")
    
    line = stream.read_line()
    if line is None:
        if eof_error_p:
            raise lisptype.LispEndOfFileError(stream.name, "READ-LINE")
        return eof_value
    return line


@_registry.cl_function('WRITE-CHAR')
def write_char_stream(character, stream=None):
    """Write a character to a stream.
    
    Args:
        character: Character to write (string of length 1)
        stream: Stream to write to (default: *standard-output*)
    
    Returns:
        character
    """
    if not isinstance(character, str) or len(character) != 1:
        raise TypeError(f"Expected single character, got {character}")
    
    if stream is None:
        # Write to stdout
        import sys
        sys.stdout.write(character)
        return character
    
    if not isinstance(stream, Stream):
        raise TypeError(f"Expected Stream, got {type(stream)}")
    
    return stream.write_char(character)


@_registry.cl_function('WRITE-LINE')
def write_line_stream(line, stream=None):
    """Write a line to a stream with newline.
    
    Args:
        line: Line to write
        stream: Stream to write to (default: *standard-output*)
    
    Returns:
        NIL
    """
    if stream is None:
        # Write to stdout
        print(line)
        return lisptype.NIL
    
    if not isinstance(stream, Stream):
        raise TypeError(f"Expected Stream, got {type(stream)}")
    
    return stream.write_line(line)


@_registry.cl_function('WRITE-SEQUENCE')
def write_sequence_stream(sequence, stream=None, start=0, end=None):
    """Write a sequence to a stream.
    
    Args:
        sequence: String or list of characters
        stream: Stream to write to (default: *standard-output*)
        start: Starting index
        end: Ending index (exclusive)
    
    Returns:
        sequence
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
    
    if stream is None:
        import sys
        sys.stdout.write(text)
        return sequence
    
    if not isinstance(stream, Stream):
        raise TypeError(f"Expected Stream, got {type(stream)}")
    
    stream.write_sequence(text)
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
