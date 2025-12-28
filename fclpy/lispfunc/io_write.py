"""I/O write operations - stream output, printing, pathnames, and file operations."""

import fclpy.lisptype as lisptype
from . import registry as _registry

# Re-export pathname functions from pathnames module for backward compatibility
# Note: make_pathname (registered as 'PATHNAME') and make_pathname_function
# (registered as 'MAKE-PATHNAME') are different functions!
from .pathnames import (
    make_pathname,  # PATHNAME function - converts string to Pathname
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

# Alias for backward compatibility - some code may use 'pathname' instead of 'make_pathname'
pathname = make_pathname


@_registry.cl_function('CLEAR-OUTPUT')
def clear_output(stream=None):
    """Clear output from stream."""
    return None


@_registry.cl_function('OUTPUT-STREAM-P')
def output_stream_p(stream):
    """Test if stream is output stream."""
    return lisptype.T  # Simplified


@_registry.cl_function('OPEN-STREAM-P')
def open_stream_p(stream):
    """Test if stream is open."""
    return lisptype.T  # Simplified


# I/O write operations
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


@_registry.cl_function('WRITE')
def write(object, stream=None, **kwargs):
    """Write object to stream."""
    print(lisptype.lisp_str(object), end='')
    return object


@_registry.cl_function('PRIN1-TO-STRING')
def prin1_to_string(object):
    """Print object to string (readable)."""
    return lisptype.lisp_repr(object)


@_registry.cl_function('PRINC-TO-STRING')
def princ_to_string(object):
    """Print object to string (not readable)."""
    return lisptype.lisp_str(object)


@_registry.cl_function('WRITE-TO-STRING')
def write_to_string(object, **kwargs):
    """Write object to string."""
    return lisptype.lisp_str(object)


@_registry.cl_function('PRINT')
def print_fn(object, stream=None):
    """Print object."""
    print(lisptype.lisp_str(object))
    return object


@_registry.cl_function('PRIN1')
def prin1(object, stream=None):
    """Print object readably."""
    print(lisptype.lisp_repr(object))
    return object


@_registry.cl_function('PRINC')
def princ(object, stream=None):
    """Print object for humans."""
    print(lisptype.lisp_str(object), end='')
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
    return print, lisptype.NIL  # Simplified


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


# NOTE: Pathname operations are defined in pathnames.py with proper Pathname class support
# Functions like PATHNAME, PATHNAMEP, PATHNAME-DIRECTORY, etc. are all in pathnames.py


# Stream operations
@_registry.cl_function('OPEN')
def open_fn(filespec, **kwargs):
    """Open file."""
    # Simplified - return file name
    return str(filespec)


@_registry.cl_function('CLOSE')
def close_fn(stream, **kwargs):
    """Close stream."""
    return lisptype.T


@_registry.cl_function('STREAM-ELEMENT-TYPE')
def stream_element_type(stream):
    """Get stream element type."""
    return 'CHARACTER'


@_registry.cl_function('STREAM-EXTERNAL-FORMAT')
def stream_external_format(stream):
    """Get stream external format."""
    return 'UTF-8'  # Simplified


# File operations
# NOTE: PROBE-FILE is defined in pathnames.py and imported above


@_registry.cl_function('DELETE-FILE')
def delete_file(filespec):
    """Delete file."""
    import os
    os.remove(str(filespec))
    return lisptype.T


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
    'make_broadcast_stream', 'make_concatenated_stream',
    'make_echo_stream', 'make_synonym_stream', 'make_two_way_stream',
    # Pretty printing
    'copy_pprint_dispatch', 'pprint', 'pprint_dispatch',
    'pprint_exit_if_list_exhausted', 'pprint_indent', 'pprint_linear',
    'pprint_logical_block', 'pprint_newline', 'pprint_pop', 'pprint_tab',
    'pprint_tabular', 'pprint_fill', 'set_pprint_dispatch',
    # Format operations
    'format_fn', 'formatter',
    # Pathname operations
    'pathname', 'pathnamep', 'pathname_host', 'pathname_device',
    'pathname_directory', 'pathname_name', 'pathname_type',
    'pathname_version', 'make_pathname', 'namestring',
    'directory_namestring', 'host_namestring', 'file_namestring',
    'enough_namestring', 'parse_namestring', 'merge_pathnames',
    'wild_pathname_p', 'pathname_match_p', 'translate_pathname',
    'logical_pathname', 'translate_logical_pathname', 'truename',
    # File/Stream operations
    'open_fn', 'close_fn', 'stream_element_type', 'stream_external_format',
    # File operations
    'probe_file', 'delete_file', 'rename_file', 'file_author',
    'file_length', 'file_position', 'file_string_length',
    'file_write_date', 'compile_file', 'compile_file_pathname',
    # Condition operations
    'simple_condition_format_arguments', 'simple_condition_format_control',
    'end_of_file', 'file_error', 'file_error_pathname',
    # Error handling
    'error',
    # Interactive I/O
    'y_or_n_p', 'yes_or_no_p',
    # WITH- macros
    'with_open_file', 'with_open_stream', 'with_output_to_string'
]
