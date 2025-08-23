"""I/O and stream operations - file handling, string operations, formatting."""

import fclpy.lisptype as lisptype
from . import registry as _registry


@_registry.cl_function('READTABLEP')
def readtablep(obj):
    """Test if object is a readtable."""
    # For now, return False as we don't have readtable objects yet
    return False


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
        return True
    stream_attrs = ("read", "write", "readline", "readinto", "flush")
    return any(hasattr(obj, a) for a in stream_attrs)


# I/O operations
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


@_registry.cl_function('WRITE-CHAR')
def write_char(character, stream=None):
    """Write character to stream."""
    print(character, end='')
    return character


@_registry.cl_function('WRITE-STRING')
def write_string(string, stream=None, start=0, end=None):
    """Write string to stream."""
    if end is None:
        end = len(string)
    print(string[start:end], end='')
    return string


@_registry.cl_function('WRITE-LINE')
def write_line(string, stream=None):
    """Write line to stream."""
    print(string)
    return string


@_registry.cl_function('WRITE-BYTE')
def write_byte(byte, stream):
    """Write byte to stream."""
    # Simplified implementation
    return byte


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
def listen_fn(stream=None):
    """Test if stream has input available."""
    return True  # Simplified


@_registry.cl_function('LISTEN')
def listen(stream=None):
    """Test if input is available."""
    return True  # Simplified


@_registry.cl_function('CLEAR-INPUT')
def clear_input(stream=None):
    """Clear input from stream."""
    return None


@_registry.cl_function('CLEAR-OUTPUT')
def clear_output(stream=None):
    """Clear output from stream."""
    return None


@_registry.cl_function('WRITE')
def write_fn(object, **kwargs):
    """Write object."""
    print(object, end='')
    return object


@_registry.cl_function('WRITE')
def write(object, stream=None, **kwargs):
    """Write object to stream."""
    print(object, end='')
    return object


@_registry.cl_function('PRIN1-TO-STRING')
def prin1_to_string(object):
    """Print object to string (readable)."""
    return str(object)


@_registry.cl_function('PRINC-TO-STRING')
def princ_to_string(object):
    """Print object to string (not readable)."""
    return str(object)


@_registry.cl_function('WRITE-TO-STRING')
def write_to_string(object, **kwargs):
    """Write object to string."""
    return str(object)


@_registry.cl_function('PRINT')
def print_fn(object, stream=None):
    """Print object."""
    print(object)
    return object


@_registry.cl_function('PRIN1')
def prin1(object, stream=None):
    """Print object readably."""
    print(repr(object))
    return object


@_registry.cl_function('PRINC')
def princ(object, stream=None):
    """Print object for humans."""
    print(object, end='')
    return object


@_registry.cl_function('TERPRI')
def terpri(stream=None):
    """Output newline."""
    print()
    return None


@_registry.cl_function('FRESH-LINE')
def fresh_line(stream=None):
    """Start fresh line if needed."""
    print()
    return None


@_registry.cl_function('FINISH-OUTPUT')
def finish_output(stream=None):
    """Finish output to stream."""
    return None


@_registry.cl_function('FORCE-OUTPUT')
def force_output(stream=None):
    """Force output to stream."""
    return None


# Pathname operations
@_registry.cl_function('PATHNAME')
def pathname(pathspec):
    """Convert to pathname."""
    return str(pathspec)


@_registry.cl_function('PATHNAMEP')
def pathnamep(object):
    """Test if object is pathname."""
    return isinstance(object, str)  # Simplified


@_registry.cl_function('PATHNAME-HOST')
def pathname_host(pathname):
    """Get pathname host."""
    return None  # No host for now


@_registry.cl_function('PATHNAME-DEVICE')
def pathname_device(pathname):
    """Get pathname device."""
    return None  # No device for now


@_registry.cl_function('PATHNAME-DIRECTORY')
def pathname_directory(pathname):
    """Get pathname directory."""
    import os
    return os.path.dirname(str(pathname))


@_registry.cl_function('PATHNAME-NAME')
def pathname_name(pathname):
    """Get pathname name."""
    import os
    return os.path.splitext(os.path.basename(str(pathname)))[0]


@_registry.cl_function('PATHNAME-TYPE')
def pathname_type(pathname):
    """Get pathname type."""
    import os
    ext = os.path.splitext(str(pathname))[1]
    return ext[1:] if ext else None


@_registry.cl_function('PATHNAME-VERSION')
def pathname_version(pathname):
    """Get pathname version."""
    return None  # No versions for now


@_registry.cl_function('MAKE-PATHNAME')
def make_pathname(**kwargs):
    """Make pathname."""
    import os
    parts = []
    if 'directory' in kwargs:
        parts.append(kwargs['directory'])
    if 'name' in kwargs:
        name = kwargs['name']
        if 'type' in kwargs:
            name += '.' + kwargs['type']
        parts.append(name)
    return os.path.join(*parts) if parts else ""


@_registry.cl_function('NAMESTRING')
def namestring(pathname):
    """Get namestring of pathname."""
    return str(pathname)


@_registry.cl_function('DIRECTORY-NAMESTRING')
def directory_namestring(pathname):
    """Get directory namestring."""
    import os
    return os.path.dirname(str(pathname))


@_registry.cl_function('HOST-NAMESTRING')
def host_namestring(pathname):
    """Return host portion of pathname."""
    return ""  # No host for simplified implementation


@_registry.cl_function('FILE-NAMESTRING')
def file_namestring(pathname):
    """Return file portion of pathname."""
    import os
    return os.path.basename(str(pathname))


@_registry.cl_function('ENOUGH-NAMESTRING')
def enough_namestring(pathname, defaults=None):
    """Get enough namestring."""
    return str(pathname)  # Simplified


@_registry.cl_function('PARSE-NAMESTRING')
def parse_namestring(thing, **kwargs):
    """Parse namestring."""
    return str(thing)  # Simplified


@_registry.cl_function('MERGE-PATHNAMES')
def merge_pathnames(pathname, default_pathname=None, default_version=None):
    """Merge pathname with default."""
    return str(pathname)  # Simplified


@_registry.cl_function('WILD-PATHNAME-P')
def wild_pathname_p(pathname, field_key=None):
    """Test if pathname is wild."""
    return False  # No wildcards for now


@_registry.cl_function('PATHNAME-MATCH-P')
def pathname_match_p(pathname, wildname):
    """Test if pathname matches wildname."""
    return str(pathname) == str(wildname)  # Simplified


@_registry.cl_function('TRANSLATE-PATHNAME')
def translate_pathname(source, from_wildname, to_wildname):
    """Translate pathname."""
    return str(source)  # Simplified


@_registry.cl_function('LOGICAL-PATHNAME')
def logical_pathname(pathspec):
    """Convert to logical pathname."""
    return str(pathspec)  # No logical pathnames for now


@_registry.cl_function('TRANSLATE-LOGICAL-PATHNAME')
def translate_logical_pathname(pathname, **kwargs):
    """Translate logical pathname."""
    return str(pathname)


@_registry.cl_function('TRUENAME')
def truename(filespec):
    """Get truename of file."""
    import os
    return os.path.abspath(str(filespec))


# Stream operations
@_registry.cl_function('OPEN')
def open_fn(filespec, **kwargs):
    """Open file."""
    # Simplified - return file name
    return str(filespec)


@_registry.cl_function('CLOSE')
def close_fn(stream, **kwargs):
    """Close stream."""
    return True


@_registry.cl_function('OUTPUT-STREAM-P')
def output_stream_p(stream):
    """Test if stream is output stream."""
    return True  # Simplified


@_registry.cl_function('INPUT-STREAM-P')
def input_stream_p(stream):
    """Test if stream is input stream."""
    return True  # Simplified


@_registry.cl_function('OPEN-STREAM-P')
def open_stream_p(stream):
    """Test if stream is open."""
    return True  # Simplified


@_registry.cl_function('INTERACTIVE-STREAM-P')
def interactive_stream_p(stream):
    """Test if stream is interactive."""
    return True  # Simplified


## Removed duplicate STREAMP definition (merged above)


@_registry.cl_function('STREAM-ELEMENT-TYPE')
def stream_element_type(stream):
    """Get stream element type."""
    return 'CHARACTER'


@_registry.cl_function('STREAM-EXTERNAL-FORMAT')
def stream_external_format(stream):
    """Get stream external format."""
    return 'UTF-8'  # Simplified


@_registry.cl_function('MAKE-STRING-INPUT-STREAM')
def make_string_input_stream(string, start=0, end=None):
    """Make string input stream."""
    if end is None:
        end = len(string)
    return string[start:end]  # Simplified


@_registry.cl_function('MAKE-STRING-OUTPUT-STREAM')
def make_string_output_stream(**kwargs):
    """Make string output stream."""
    return ""  # Simplified


@_registry.cl_function('GET-OUTPUT-STREAM-STRING')
def get_output_stream_string(stream):
    """Get string from output stream."""
    return str(stream)  # Simplified


@_registry.cl_function('MAKE-BROADCAST-STREAM')
def make_broadcast_stream(*streams):
    """Make broadcast stream."""
    return streams[0] if streams else None


@_registry.cl_function('MAKE-CONCATENATED-STREAM')
def make_concatenated_stream(*streams):
    """Make concatenated stream."""
    return streams[0] if streams else None


@_registry.cl_function('MAKE-ECHO-STREAM')
def make_echo_stream(input_stream, output_stream):
    """Make echo stream."""
    return output_stream


@_registry.cl_function('MAKE-SYNONYM-STREAM')
def make_synonym_stream(symbol):
    """Make synonym stream."""
    return str(symbol)


@_registry.cl_function('MAKE-TWO-WAY-STREAM')
def make_two_way_stream(input_stream, output_stream):
    """Make two-way stream."""
    return output_stream


# Pretty printing operations
@_registry.cl_function('COPY-PPRINT-DISPATCH')
def copy_pprint_dispatch(table=None):
    """Copy pretty print dispatch table."""
    return {}  # Simplified


@_registry.cl_function('PPRINT')
def pprint(object, stream=None):
    """Pretty print object."""
    print(object)
    return None


@_registry.cl_function('PPRINT-DISPATCH')
def pprint_dispatch(object, table=None):
    """Get pretty print dispatch function (stub)."""
    return print, False  # Simplified


@_registry.cl_function('PPRINT-EXIT-IF-LIST-EXHAUSTED')
def pprint_exit_if_list_exhausted():
    """Exit if list exhausted (stub)."""
    return None


@_registry.cl_function('PPRINT-INDENT')
def pprint_indent(relative_to, n, stream=None):
    """Set pretty print indent (stub)."""
    return None


@_registry.cl_function('PPRINT-LINEAR')
def pprint_linear(stream, object, prefix=None, per_line_prefix=None, suffix=None):
    """Linear pretty print (stub)."""
    print(object)
    return None


@_registry.cl_function('PPRINT-LOGICAL-BLOCK')
def pprint_logical_block(stream, object, prefix=None, per_line_prefix=None, suffix=None):
    """Logical block pretty print (stub)."""
    print(object)
    return None


@_registry.cl_function('PPRINT-NEWLINE')
def pprint_newline(kind, stream=None):
    """Pretty print newline (stub)."""
    print()
    return None


@_registry.cl_function('PPRINT-POP')
def pprint_pop():
    """Pretty print pop (stub)."""
    return None


@_registry.cl_function('PPRINT-TAB')
def pprint_tab(kind, colnum, colinc, stream=None):
    """Pretty print tab (stub)."""
    return None


@_registry.cl_function('PPRINT-TABULAR')
def pprint_tabular(stream, object, prefix=None, per_line_prefix=None, suffix=None):
    """Tabular pretty print (stub)."""
    print(object)
    return None


@_registry.cl_function('PPRINT-FILL')
def pprint_fill(stream, list_obj, colon_p=None, at_sign_p=None):
    """Pretty print fill (stub)."""
    print(list_obj)
    return None



@_registry.cl_function('SET-PPRINT-DISPATCH')
def set_pprint_dispatch(type_specifier, function, priority=0, table=None):
    """Set pretty print dispatch."""
    return None


# File operations
@_registry.cl_function('PROBE-FILE')
def probe_file(pathspec):
    """Test if file exists."""
    import os
    return os.path.exists(str(pathspec))


@_registry.cl_function('DELETE-FILE')
def delete_file(filespec):
    """Delete file."""
    import os
    os.remove(str(filespec))
    return True


@_registry.cl_function('RENAME-FILE')
def rename_file(filespec, new_name):
    """Rename file."""
    import os
    os.rename(str(filespec), str(new_name))
    return str(new_name)


@_registry.cl_function('FILE-AUTHOR')
def file_author(pathspec):
    """Get file author."""
    return "unknown"  # Simplified


@_registry.cl_function('FILE-LENGTH')
def file_length(stream):
    """Get file length."""
    return 0  # Simplified


@_registry.cl_function('FILE-POSITION')
def file_position(stream, position=None):
    """Get or set file position."""
    if position is None:
        return 0  # Get position
    else:
        return position  # Set position


@_registry.cl_function('FILE-STRING-LENGTH')
def file_string_length(stream, string):
    """Length of string in file."""
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


@_registry.cl_function('COMPILE-FILE')
def compile_file(input_file, output_file=None, **kwargs):
    """Compile file."""
    return str(input_file), [], []  # Simplified


@_registry.cl_function('COMPILE-FILE-PATHNAME')
def compile_file_pathname(input_file, output_file=None, **kwargs):
    """Get compiled file pathname."""
    import os
    base = os.path.splitext(str(input_file))[0]
    return base + ".fasl"  # Simplified


# Additional I/O functions
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


# Format operations
@_registry.cl_function('FORMAT')
def format_fn(destination, control_string, *args):
    """Format output."""
    try:
        formatted = control_string.format(*args)
        if destination is True:
            print(formatted, end='')
            return None
        elif destination is None:
            return formatted
        else:
            # Assume destination is a stream
            print(formatted, end='')
            return None
    except:
        return str(control_string)  # Fallback


@_registry.cl_function('FORMATTER')
def formatter(control_string):
    """Create formatter function."""
    def format_func(stream, *args):
        return format_fn(stream, control_string, *args)
    return format_func


# Condition operations
@_registry.cl_function('SIMPLE-CONDITION-FORMAT-ARGUMENTS')
def simple_condition_format_arguments(condition):
    """Get format arguments from condition."""
    return []  # Simplified


@_registry.cl_function('SIMPLE-CONDITION-FORMAT-CONTROL')
def simple_condition_format_control(condition):
    """Get format control from condition."""
    return str(condition)  # Simplified


def end_of_file():
    """End of file condition."""
    return EOFError()


def file_error():
    """File error condition."""
    return FileNotFoundError()


def file_error_pathname(condition):
    """Get pathname from file error."""
    return str(condition)  # Simplified


# Interactive I/O
def y_or_n_p(control_string=None, *args):
    """Ask yes/no question."""
    if control_string:
        print(control_string.format(*args), end=' ')
    response = input("(y or n) ").strip().lower()
    return response in ('y', 'yes')


def yes_or_no_p(control_string=None, *args):
    """Ask yes/no question with full words."""
    if control_string:
        print(control_string.format(*args), end=' ')
    response = input("(yes or no) ").strip().lower()
    return response == 'yes'


# Error handling
@_registry.cl_function('ERROR')
def error(format_control, *args):
    """Signal error."""
    msg = format_control.format(*args) if args else str(format_control)
    raise Exception(msg)


# Symbol generation
@_registry.cl_function('GENSYM')
def gensym(x="G"):
    """Generate unique symbol."""
    import random
    return lisptype.LispSymbol(f"{x}{random.randint(1000, 9999)}")


# Special print function to avoid conflict with Python's print
@_registry.cl_function('PRINT')
def _s_print_(object, stream=None):
    """Print function with encoded name to avoid Python print conflict."""
    if stream is None:
        print(object)
    else:
        stream.write(str(object))
        stream.write('\n')
    return object


# Macro character operations - use centralized readtable
from ..readtable import get_current_readtable


@_registry.cl_function('GET-MACRO-CHARACTER')
def get_macro_character(char, readtable=None):
    """Get macro character function."""
    if readtable is None:
        readtable = get_current_readtable()
    return readtable.get_macro_character(char)


@_registry.cl_function('SET-MACRO-CHARACTER')
def set_macro_character(char, function, non_terminating_p=None, readtable=None):
    """Set macro character function."""
    if readtable is None:
        readtable = get_current_readtable()
    return readtable.set_macro_character(char, function, non_terminating_p or False)


@_registry.cl_function('SET-DISPATCH-MACRO-CHARACTER')
def set_dispatch_macro_character(disp_char, sub_char, function, readtable=None):
    """Set dispatch macro character."""
    if readtable is None:
        readtable = get_current_readtable()
    return readtable.set_dispatch_macro_character(disp_char, sub_char, function)


@_registry.cl_function('GET-DISPATCH-MACRO-CHARACTER')
def get_dispatch_macro_character(disp_char, sub_char, readtable=None):
    """Get dispatch macro character function."""
    if readtable is None:
        readtable = get_current_readtable()
    return readtable.get_dispatch_macro_character(disp_char, sub_char)


@_registry.cl_function('MAKE-DISPATCH-MACRO-CHARACTER')
def make_dispatch_macro_character(char, non_terminating_p=False, readtable=None):
    """Make character into dispatch macro character."""
    if readtable is None:
        readtable = get_current_readtable()
    # Our simplified Readtable doesn't expose a dedicated creator; emulate by
    # registering a placeholder sharp reader if needed and marking non-terminating.
    # We re-use set_macro_character to ensure the dispatch starter exists.
    # In CL, MAKEDISPATCHMACROCHARACTER installs a dispatch macro; here just ensure entry.
    readtable.set_macro_character(char, lambda c, s: None, not non_terminating_p)
    return True


@_registry.cl_function('COPY-READTABLE')
def copy_readtable(from_readtable=None, to_readtable=None):
    """Copy readtable."""
    if from_readtable is None:
        from_readtable = get_current_readtable()
    # Our simplified Readtable lacks copy method; perform shallow structure copy.
    from fclpy.readtable import Readtable as _RT
    new_rt = _RT()
    # Replace its tables with copies of source (shallow copy suffices for function refs)
    new_rt._macro_characters = dict(from_readtable._macro_characters)
    new_rt._dispatch_macro_characters = {
        k: dict(v) for k, v in from_readtable._dispatch_macro_characters.items()
    }
    new_rt.set_readtable_case(from_readtable.readtable_case())
    return new_rt


@_registry.cl_function('READTABLE-CASE')
def readtable_case(readtable=None):
    """Get readtable case."""
    if readtable is None:
        readtable = get_current_readtable()
    return readtable.readtable_case()


@_registry.cl_function('SET-SYNTAX-FROM-CHAR')
def set_syntax_from_char(to_char, from_char, to_readtable=None, from_readtable=None):
    """Set syntax from another character in a readtable."""
    # Placeholder implementation
    return True


# WITH- macros (simplified implementations)
def with_open_file(var_filespec_options, *body):
    """Execute with open file."""
    # Simplified - just execute body
    result = None
    for form in body:
        result = form
    return result


def with_input_from_string(var_string_options, *body):
    """Execute with input from string."""
    # Simplified - just execute body
    result = None
    for form in body:
        result = form
    return result


def with_open_stream(stream_var_stream, *body):
    """Execute with open stream."""
    # Simplified - just execute body
    result = None
    for form in body:
        result = form
    return result


def with_output_to_string(stream_var_options, *body):
    """Execute with output to string."""
    # Simplified - just execute body and return empty string
    result = None
    for form in body:
        result = form
    return ""
