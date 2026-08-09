"""I/O write operations - stream output, printing, pathnames, and file operations."""

import fclpy.lisptype as lisptype
from . import registry as _registry
from .streams import open_file as open_fn, close_stream as close_fn


# === Printer Control Variables ===
# These control how objects are printed by PRINT, PRIN1, PRINC, FORMAT, etc.

class PrinterSettings:
    """Container for printer control variables.
    
    This class manages the printer control variables that affect
    how Lisp objects are printed. Variables can be dynamically bound
    for local control of printing behavior.
    """
    
    def __init__(self):
        # *PRINT-LEVEL* - Maximum depth to print nested structures
        # NIL means no limit
        self.print_level = None
        
        # *PRINT-LENGTH* - Maximum number of elements to print in a list/vector
        # NIL means no limit
        self.print_length = None
        
        # *PRINT-CASE* - Case to use when printing symbols
        # :UPCASE (default), :DOWNCASE, :CAPITALIZE
        self.print_case = 'UPCASE'
        
        # *PRINT-CIRCLE* - Whether to detect and print circular structures
        self.print_circle = False
        
        # *PRINT-GENSYM* - Whether to print #: prefix for uninterned symbols
        self.print_gensym = True
        
        # *PRINT-ARRAY* - Whether to print arrays readably
        self.print_array = True
        
        # *PRINT-READABLY* - Whether to print in a readable format
        self.print_readably = False
        
        # *PRINT-ESCAPE* - Whether to print escape characters (for PRIN1 vs PRINC)
        self.print_escape = True
        
        # *PRINT-BASE* - Radix for printing integers (default 10)
        self.print_base = 10
        
        # *PRINT-RADIX* - Whether to print radix prefix
        self.print_radix = False
        
        # *PRINT-PRETTY* - Whether to use pretty printing
        self.print_pretty = False
        
        # *PRINT-LINES* - Max lines for pretty printing (NIL = no limit)
        self.print_lines = None
        
        # *PRINT-MISER-WIDTH* - Column at which miser style kicks in
        self.print_miser_width = 40
        
        # *PRINT-RIGHT-MARGIN* - Right margin for pretty printing
        self.print_right_margin = 80
    
    def copy(self):
        """Create a copy of current settings."""
        new = PrinterSettings()
        new.print_level = self.print_level
        new.print_length = self.print_length
        new.print_case = self.print_case
        new.print_circle = self.print_circle
        new.print_gensym = self.print_gensym
        new.print_array = self.print_array
        new.print_readably = self.print_readably
        new.print_escape = self.print_escape
        new.print_base = self.print_base
        new.print_radix = self.print_radix
        new.print_pretty = self.print_pretty
        new.print_lines = self.print_lines
        new.print_miser_width = self.print_miser_width
        new.print_right_margin = self.print_right_margin
        return new


def stream_element_type(stream):
    """Get stream element type (simple fallback)."""
    return 'CHARACTER'


def stream_external_format(stream):
    """Get stream external format (simple fallback)."""
    return 'UTF-8'


# Global printer settings (corresponds to *PRINT-...* variables)
_printer_settings = PrinterSettings()


def get_printer_settings():
    """Get the current printer settings object."""
    return _printer_settings


def set_printer_setting(name, value):
    """Set a printer control variable.
    
    Args:
        name: Variable name (e.g., 'PRINT-LEVEL', 'PRINT-LENGTH')
        value: New value
    """
    settings = _printer_settings
    name_lower = name.lower().replace('-', '_').replace('*', '')
    if hasattr(settings, name_lower):
        setattr(settings, name_lower, value)
    else:
        raise lisptype.LispError(f"Unknown printer variable: {name}")


def get_printer_setting(name):
    """Get a printer control variable.
    
    Args:
        name: Variable name (e.g., 'PRINT-LEVEL', 'PRINT-LENGTH')
    
    Returns:
        Current value of the variable
    """
    settings = _printer_settings
    name_lower = name.lower().replace('-', '_').replace('*', '')
    if hasattr(settings, name_lower):
        return getattr(settings, name_lower)
    else:
        raise lisptype.LispError(f"Unknown printer variable: {name}")


# === Registered Printer Control Variable Accessors ===
# These provide Lisp-level access to *PRINT-...* variables

@_registry.cl_function('*PRINT-LEVEL*')
def get_print_level():
    """Get the value of *PRINT-LEVEL*."""
    v = _printer_settings.print_level
    return lisptype.NIL if v is None else v


@_registry.cl_function('*PRINT-LENGTH*')
def get_print_length():
    """Get the value of *PRINT-LENGTH*."""
    v = _printer_settings.print_length
    return lisptype.NIL if v is None else v


@_registry.cl_function('*PRINT-BASE*')
def get_print_base():
    """Get the value of *PRINT-BASE*."""
    return _printer_settings.print_base


@_registry.cl_function('*PRINT-RADIX*')
def get_print_radix():
    """Get the value of *PRINT-RADIX*."""
    return lisptype.T if _printer_settings.print_radix else lisptype.NIL


@_registry.cl_function('*PRINT-CASE*')
def get_print_case():
    """Get the value of *PRINT-CASE*."""
    case_val = _printer_settings.print_case
    from .core import intern_keyword
    return intern_keyword(case_val)


@_registry.cl_function('*PRINT-CIRCLE*')
def get_print_circle():
    """Get the value of *PRINT-CIRCLE*."""
    return lisptype.T if _printer_settings.print_circle else lisptype.NIL


@_registry.cl_function('*PRINT-GENSYM*')
def get_print_gensym():
    """Get the value of *PRINT-GENSYM*."""
    return lisptype.T if _printer_settings.print_gensym else lisptype.NIL


@_registry.cl_function('*PRINT-ARRAY*')
def get_print_array():
    """Get the value of *PRINT-ARRAY*."""
    return lisptype.T if _printer_settings.print_array else lisptype.NIL


@_registry.cl_function('*PRINT-READABLY*')
def get_print_readably():
    """Get the value of *PRINT-READABLY*."""
    return lisptype.T if _printer_settings.print_readably else lisptype.NIL


@_registry.cl_function('*PRINT-ESCAPE*')
def get_print_escape():
    """Get the value of *PRINT-ESCAPE*."""
    return lisptype.T if _printer_settings.print_escape else lisptype.NIL


@_registry.cl_function('*PRINT-PRETTY*')
def get_print_pretty():
    """Get the value of *PRINT-PRETTY*."""
    return lisptype.T if _printer_settings.print_pretty else lisptype.NIL


def _print_with_limits(obj, current_level=0, current_length_tracker=None):
    """Print object respecting *PRINT-LEVEL* and *PRINT-LENGTH*.
    
    Args:
        obj: Object to print
        current_level: Current nesting depth
        current_length_tracker: Dict tracking element counts per level
    
    Returns:
        String representation of object
    """
    settings = _printer_settings
    
    # Check level limit
    if settings.print_level is not None:
        if current_level >= settings.print_level:
            return '#'
    
    if isinstance(obj, (list, tuple)):
        if current_length_tracker is None:
            current_length_tracker = {}
        
        parts = []
        length_key = current_level
        count = 0
        
        for item in obj:
            if settings.print_length is not None and count >= settings.print_length:
                parts.append('...')
                break
            parts.append(_print_with_limits(item, current_level + 1, current_length_tracker))
            count += 1
        
        return '(' + ' '.join(parts) + ')'
    
    # Check T and NIL before general LispSymbol check
    elif obj is lisptype.T:
        return 'T'
    
    elif obj is None or obj is lisptype.NIL:
        return 'NIL'
    
    elif isinstance(obj, lisptype.LispSymbol):
        name = obj.name
        if settings.print_case == 'DOWNCASE':
            name = name.lower()
        elif settings.print_case == 'CAPITALIZE':
            name = name.capitalize()
        # UPCASE is default, name already uppercase
        
        # Handle uninterned symbols
        if settings.print_gensym and obj.package is None:
            return '#:' + name
        return name
    
    elif isinstance(obj, str):
        if settings.print_escape:
            # Escape quotes and backslashes
            escaped = obj.replace('\\', '\\\\').replace('"', '\\"')
            return '"' + escaped + '"'
        else:
            return obj
    
    elif isinstance(obj, int):
        if settings.print_radix:
            if settings.print_base == 16:
                return '#x' + hex(obj)[2:].upper()
            elif settings.print_base == 8:
                return '#o' + oct(obj)[2:]
            elif settings.print_base == 2:
                return '#b' + bin(obj)[2:]
            elif settings.print_base != 10:
                return f'#.R{settings.print_base} ' + str(obj)
        return str(obj)
    
    else:
        return lisptype.lisp_str(obj)


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
    # If a stream object is provided, write to it; otherwise default to stdout
    if stream is None:
        print(lisptype.lisp_repr(object))
        return object

    # Lazy import to avoid cycles
    from .streams import Stream
    if isinstance(stream, Stream):
        stream.write_sequence(lisptype.lisp_repr(object))
        return object

    # Fallback: attempt to write via Python file-like object
    try:
        stream.write(lisptype.lisp_repr(object))
        return object
    except Exception:
        print(lisptype.lisp_repr(object))
        return object
    return object


@_registry.cl_function('PRINC')
def princ(object, stream=None):
    """Print object for humans."""
    if stream is None:
        print(lisptype.lisp_str(object), end='')
        return object

    from .streams import Stream
    if isinstance(stream, Stream):
        stream.write_sequence(lisptype.lisp_str(object))
        return object

    try:
        stream.write(lisptype.lisp_str(object))
        return object
    except Exception:
        print(lisptype.lisp_str(object), end='')
        return object


@_registry.cl_function('TERPRI')
def terpri(stream=None):
    """Output newline."""
    if stream is None:
        print()
        return None

    from .streams import Stream
    if isinstance(stream, Stream):
        stream.write_line('')
        return None

    try:
        stream.write('\n')
        return None
    except Exception:
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
    """Make string output stream - delegates to streams.py."""
    from .streams import make_string_output_stream as _make_sos
    element_type = kwargs.get('element_type', 'character')
    return _make_sos(element_type)


@_registry.cl_function('GET-OUTPUT-STREAM-STRING')
def get_output_stream_string(stream):
    """Get string from output stream - delegates to streams.py."""
    from .streams import get_output_stream_string as _get_oss
    return _get_oss(stream)


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


def _format_directive(control_string, cursor, pos):
    """Process a single format directive starting at pos (after ~).

    Consumes arguments from `cursor` (a _FormatCursor), mutating it in
    place. Returns (output_string, new_pos).
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
        # ~A - Aesthetic (princ-style, no escapes)
        val = get_arg()
        if val is None:
            result = "()" if colon_flag else "NIL"
        elif val is lisptype.NIL:
            result = "()" if colon_flag else "NIL"
        elif isinstance(val, str):
            result = val
        else:
            result = lisptype.lisp_str(val)
        # Apply mincol padding if specified
        if params:
            mincol = params[0] if params[0] is not None else 0
            if len(result) < mincol:
                padding = ' ' * (mincol - len(result))
                result = result + padding if not at_flag else padding + result
        return (result, pos)
    
    elif directive == 'S':
        # ~S - Standard (prin1-style, with escapes)
        val = get_arg()
        if val is None:
            result = "()" if colon_flag else "NIL"
        elif val is lisptype.NIL:
            result = "()" if colon_flag else "NIL"
        else:
            result = lisptype.lisp_repr(val)
        # Apply mincol padding if specified
        if params:
            mincol = params[0] if params[0] is not None else 0
            if len(result) < mincol:
                padding = ' ' * (mincol - len(result))
                result = result + padding if not at_flag else padding + result
        return (result, pos)
    
    elif directive == 'D':
        # ~D - Decimal integer
        val = get_arg()
        try:
            num = int(val) if val is not None else 0
            if at_flag and num >= 0:
                result = '+' + str(num)
            else:
                result = str(num)
            # Apply mincol padding
            if params:
                mincol = params[0] if params[0] is not None else 0
                padchar = params[1] if len(params) > 1 and params[1] else ' '
                if len(result) < mincol:
                    padding = str(padchar) * (mincol - len(result))
                    result = padding + result
            # Add commas for :
            if colon_flag:
                # Insert commas every 3 digits from right
                sign = ''
                if result[0] in '+-':
                    sign = result[0]
                    result = result[1:]
                parts = []
                while len(result) > 3:
                    parts.append(result[-3:])
                    result = result[:-3]
                parts.append(result)
                result = sign + ','.join(reversed(parts))
        except (TypeError, ValueError):
            result = str(val)
        return (result, pos)
    
    elif directive == 'X':
        # ~X - Hexadecimal
        val = get_arg()
        try:
            num = int(val) if val is not None else 0
            if num < 0:
                result = '-' + hex(-num)[2:].upper()
            else:
                result = hex(num)[2:].upper()
            if at_flag and num >= 0:
                result = '+' + result
        except (TypeError, ValueError):
            result = str(val)
        return (result, pos)
    
    elif directive == 'O':
        # ~O - Octal
        val = get_arg()
        try:
            num = int(val) if val is not None else 0
            if num < 0:
                result = '-' + oct(-num)[2:]
            else:
                result = oct(num)[2:]
            if at_flag and num >= 0:
                result = '+' + result
        except (TypeError, ValueError):
            result = str(val)
        return (result, pos)
    
    elif directive == 'B':
        # ~B - Binary
        val = get_arg()
        try:
            num = int(val) if val is not None else 0
            if num < 0:
                result = '-' + bin(-num)[2:]
            else:
                result = bin(num)[2:]
            if at_flag and num >= 0:
                result = '+' + result
        except (TypeError, ValueError):
            result = str(val)
        return (result, pos)
    
    elif directive == 'R':
        # ~R - Radix (with param) or English (without)
        if params and params[0] is not None:
            radix = params[0]
            val = get_arg()
            try:
                num = int(val) if val is not None else 0
                if radix == 10:
                    result = str(num)
                elif radix == 16:
                    result = hex(num)[2:].upper() if num >= 0 else '-' + hex(-num)[2:].upper()
                elif radix == 8:
                    result = oct(num)[2:] if num >= 0 else '-' + oct(-num)[2:]
                elif radix == 2:
                    result = bin(num)[2:] if num >= 0 else '-' + bin(-num)[2:]
                else:
                    # General radix conversion
                    if num == 0:
                        result = '0'
                    else:
                        digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                        neg = num < 0
                        num = abs(num)
                        chars = []
                        while num:
                            chars.append(digits[num % radix])
                            num //= radix
                        result = ('-' if neg else '') + ''.join(reversed(chars))
            except (TypeError, ValueError):
                result = str(val)
        else:
            # English representation
            val = get_arg()
            try:
                num = int(val) if val is not None else 0
                # Simple English for small numbers
                if colon_flag:
                    # Ordinal: 1st, 2nd, 3rd, etc.
                    ordinals = {1: 'first', 2: 'second', 3: 'third', 4: 'fourth',
                               5: 'fifth', 6: 'sixth', 7: 'seventh', 8: 'eighth',
                               9: 'ninth', 10: 'tenth', 11: 'eleventh', 12: 'twelfth'}
                    result = ordinals.get(num, str(num) + ('th' if num not in (1,2,3) or (11 <= num <= 13) else
                                                          'st' if num % 10 == 1 else
                                                          'nd' if num % 10 == 2 else
                                                          'rd' if num % 10 == 3 else 'th'))
                else:
                    # Cardinal: one, two, three, etc.
                    cardinals = {0: 'zero', 1: 'one', 2: 'two', 3: 'three', 4: 'four',
                                5: 'five', 6: 'six', 7: 'seven', 8: 'eight', 9: 'nine',
                                10: 'ten', 11: 'eleven', 12: 'twelve'}
                    result = cardinals.get(num, str(num))
            except (TypeError, ValueError):
                result = str(val)
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
        # ~& - Fresh line (newline only if not at start of line)
        count = params[0] if params and params[0] else 1
        # We don't track column, so just emit newline
        return ('\n' * count, pos)

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
            if isinstance(fmt_args, (list, tuple)):
                sub_cursor = _FormatCursor(fmt_args)
            else:
                sub_cursor = _FormatCursor([fmt_args] if fmt_args is not None else [])
            result = _format_process_cursor(str(fmt_str) if fmt_str else '', sub_cursor)
        return (result, pos)
    
    elif directive == '<':
        # ~< ... ~> - Justification/Logical block
        # This is a complex directive for text justification and pretty printing
        # For now, implement a simplified version that processes content between separators
        # Find matching ~>
        nesting = 1
        end_pos = pos
        segments = []
        segment_start = pos
        
        while end_pos < len(control_string) and nesting > 0:
            if control_string[end_pos] == '~':
                if end_pos + 1 < len(control_string):
                    # Skip any modifiers to find directive char
                    j = end_pos + 1
                    while j < len(control_string) and control_string[j] in '0123456789,:#@':
                        j += 1
                    if j < len(control_string):
                        next_char = control_string[j].upper()
                        has_colon = ':' in control_string[end_pos+1:j]
                        
                        if next_char == '<':
                            nesting += 1
                            end_pos = j + 1
                        elif next_char == '>':
                            nesting -= 1
                            if nesting == 0:
                                # Found the closing ~>
                                segments.append(control_string[segment_start:end_pos])
                                end_pos = j + 1  # Position after the closing >
                                break
                            end_pos = j + 1
                        elif next_char == ';' and nesting == 1:
                            # Separator within the justification block
                            segments.append(control_string[segment_start:end_pos])
                            segment_start = j + 1
                            end_pos = j + 1
                        else:
                            end_pos = j + 1
                    else:
                        end_pos += 1
                else:
                    end_pos += 1
            else:
                end_pos += 1
        else:
            # If we exited the loop without finding closing ~>
            segments.append(control_string[segment_start:])
            end_pos = len(control_string)
        
        # For simplified implementation:
        # - If there are multiple segments separated by ~:;, use the last non-empty one
        # - Process it with remaining args
        result = ''
        if segments:
            # Look for the last segment (after the final ~:; separator)
            # This handles the pattern ~<prefix~:;main content~> where we want the main content
            segment_to_use = segments[-1] if segments else ''
            # Shares the outer cursor: arguments the segment consumes must
            # not be re-offered to directives that follow the ~<...~>.
            result = _format_process_cursor(segment_to_use, cursor)

        return (result, end_pos)

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
            # ~[ clause0 ~; clause1 ~; ... ~] - select by index
            val = get_arg()
            try:
                idx = int(val) if val is not None else 0
                if 0 <= idx < len(clauses):
                    result = _format_process_cursor(clauses[idx], cursor)
                elif default_clause is not None and default_clause < len(clauses):
                    result = _format_process_cursor(clauses[default_clause], cursor)
                else:
                    result = ''
            except (TypeError, ValueError):
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

        if at_flag:
            # ~@{...~} - use the rest of the outer arguments as the items,
            # directly from the outer cursor: they belong to the same
            # argument stream, not a separate list argument.
            items = cursor.remaining()
            cursor.idx = len(cursor.args)
        else:
            # ~{...~} / ~:{...~} - the next argument is the list of items,
            # a scope of its own (per CLHS 22.3.7): only the single "list"
            # argument itself (already taken by get_arg()) is removed from
            # the outer cursor; what the iteration body does with its
            # elements never touches the outer cursor further.
            items = get_arg()
            if items is None or items is lisptype.NIL:
                items = []
            elif not isinstance(items, (list, tuple)):
                items = [items]

        result_parts = []
        # Special marker for iteration escape (from ~^)
        ESCAPE_MARKER = '\u0000'

        if colon_flag:
            # Each item is a sublist, processed with its own fresh cursor.
            for item in items:
                item_list = list(item) if isinstance(item, (list, tuple)) else [item]
                sub_cursor = _FormatCursor(item_list)
                part = _format_process_cursor(inner, sub_cursor)
                part = part.replace(ESCAPE_MARKER, '')
                result_parts.append(part)
        else:
            # Items are consumed one at a time from the provided items,
            # each pass over `inner` getting its own fresh cursor scoped to
            # the remaining items.
            item_list = list(items)
            while item_list:
                sub_cursor = _FormatCursor(item_list)
                part = _format_process_cursor(inner, sub_cursor)
                consumed = sub_cursor.idx
                # If inner indicates escape, stop iteration
                if '\u0000' in part:
                    part = part.replace('\u0000', '')
                    result_parts.append(part)
                    break
                result_parts.append(part)
                if consumed <= 0:
                    # Prevent infinite loop; consume one element
                    consumed = 1
                item_list = item_list[consumed:]

        return (''.join(result_parts), end_pos)
    
    elif directive == '}':
        return ('', pos)
    
    elif directive == '^':
        # ~^ - Escape from iteration (only in ~{ ~})
        # Emit a special marker that iteration handling will detect
        ESCAPE_MARKER = '\u0000'
        return (ESCAPE_MARKER, pos)

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
    while pos < len(control_string):
        c = control_string[pos]
        if c == '~':
            pos += 1
            output, pos = _format_directive(control_string, cursor, pos)
            result.append(output)
        else:
            result.append(c)
            pos += 1
    return ''.join(result)


def _format_process(control_string, args):
    """Process a format control string with arguments (fresh cursor)."""
    return _format_process_cursor(control_string, _FormatCursor(args))


def _format_process_with_tail(control_string, args):
    """Like _format_process but also return the number of arguments consumed
    (i.e. the index of the first remaining argument)."""
    cursor = _FormatCursor(args)
    result = _format_process_cursor(control_string, cursor)
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
        print(formatted, end='')
        return lisptype.NIL
    elif destination is None or destination is lisptype.NIL:
        return formatted
    else:
        _write_stream_output(destination, formatted)
        return lisptype.NIL


def _write_stream_output(destination, text):
    """Write `text` to a FORMAT/FORMATTER stream destination.

    Mirrors the isinstance(stream, Stream) -> write_sequence(...) convention
    PRIN1/PRINC/TERPRI already use (see above) -- fclpy's Stream classes
    expose write_sequence/write_char, not Python's file-like .write(), so
    checking hasattr(destination, 'write') is never true for them and used
    to silently fall through to printing at stdout regardless of which
    stream was actually requested.
    """
    if destination is True or destination is lisptype.T:
        print(text, end='')
        return
    from .streams import Stream
    if isinstance(destination, Stream):
        destination.write_sequence(text)
        return
    try:
        destination.write(text)
    except Exception:
        print(text, end='')


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
        _write_stream_output(stream, formatted)
        # Return the tail (remaining args) as a list
        return list(args[consumed:])

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
    """Delete file."""
    import os
    # Resolve similar to LOAD/COMPILE-FILE so relative pathnames are found
    from fclpy.lispfunc.pathnames import Pathname
    import fclpy.state as state
    env = state.current_environment

    if isinstance(filespec, Pathname):
        path_str = filespec.original
    else:
        path_str = str(filespec)

    if not os.path.isabs(path_str):
        resolved = False
        lisp_cwd = os.environ.get('LISP_CWD')
        if lisp_cwd:
            candidate = os.path.normpath(os.path.join(lisp_cwd, path_str))
            if os.path.exists(candidate):
                path_str = candidate
                resolved = True

        if not resolved and env is not None:
            load_truename_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-TRUENAME*')
            load_truename = env.find_variable(load_truename_sym)
            if load_truename and load_truename is not lisptype.NIL and isinstance(load_truename, Pathname):
                current_file_path = load_truename.original
                current_dir = os.path.dirname(current_file_path)
                if current_dir:
                    candidate = os.path.normpath(os.path.join(current_dir, path_str))
                    if os.path.exists(candidate):
                        path_str = candidate
                        resolved = True

        if not resolved and env is not None:
            default_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*DEFAULT-PATHNAME-DEFAULTS*')
            default_pathname = env.find_variable(default_sym)
            if default_pathname and default_pathname is not lisptype.NIL and isinstance(default_pathname, Pathname):
                default_path = default_pathname.original
                if os.path.isdir(default_path):
                    default_dir = default_path
                else:
                    default_dir = os.path.dirname(default_path)
                if default_dir:
                    candidate = os.path.normpath(os.path.join(default_dir, path_str))
                    if os.path.exists(candidate):
                        path_str = candidate

    try:
        os.remove(path_str)
        return lisptype.T
    except FileNotFoundError:
        return lisptype.NIL


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
    """Compile file.
    
    In FCLpy, we don't actually compile to bytecode - we copy the source file
    to a .fasl file which will be interpreted when loaded. This allows FCLpy
    to work with Common Lisp build systems that expect compile-and-load workflows.
    
    Returns: MultipleValues(output-truename, warnings-p, failure-p)
      - output-truename: The pathname of the output file
      - warnings-p: NIL (no warnings)
      - failure-p: NIL (no failure)
    """
    import os
    import shutil
    from fclpy.lispfunc.pathnames import Pathname
    
    # Get the input path (resolve relative names similarly to LOAD)
    import fclpy.state as state
    env = state.current_environment

    if isinstance(input_file, Pathname):
        input_path = input_file.original
    else:
        input_path = str(input_file)

    # If input_path is not absolute, try to resolve it using LISP_CWD,
    # *LOAD-TRUENAME* directory, or *DEFAULT-PATHNAME-DEFAULTS* (like LOAD)
    import os
    if not os.path.isabs(input_path):
        resolved = False
        lisp_cwd = os.environ.get('LISP_CWD')
        if lisp_cwd:
            candidate = os.path.normpath(os.path.join(lisp_cwd, input_path))
            if os.path.exists(candidate):
                input_path = candidate
                resolved = True

        if not resolved and env is not None:
            # Try *LOAD-TRUENAME*
            load_truename_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-TRUENAME*')
            load_truename = env.find_variable(load_truename_sym)
            try:
                from fclpy.lispfunc.pathnames import Pathname as PN
            except Exception:
                PN = None
            if load_truename and load_truename is not lisptype.NIL and PN is not None and isinstance(load_truename, PN):
                current_file_path = load_truename.original
                current_dir = os.path.dirname(current_file_path)
                if current_dir:
                    candidate = os.path.normpath(os.path.join(current_dir, input_path))
                    if os.path.exists(candidate):
                        input_path = candidate
                        resolved = True

        if not resolved and env is not None:
            default_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*DEFAULT-PATHNAME-DEFAULTS*')
            default_pathname = env.find_variable(default_sym)
            if default_pathname and default_pathname is not lisptype.NIL and PN is not None and isinstance(default_pathname, PN):
                default_path = default_pathname.original
                if os.path.isdir(default_path):
                    default_dir = default_path
                else:
                    default_dir = os.path.dirname(default_path)
                if default_dir:
                    candidate = os.path.normpath(os.path.join(default_dir, input_path))
                    if os.path.exists(candidate):
                        input_path = candidate
                        resolved = True
    
    # Determine output path
    if output_file is not None:
        if isinstance(output_file, Pathname):
            out_path = output_file.original
        else:
            out_path = str(output_file)
    else:
        # Default: replace extension with .fasl
        base = os.path.splitext(input_path)[0]
        out_path = base + ".fasl"
    
    # "Compile" by copying the source file to the output path
    # This allows LOAD to find and interpret the .fasl file
    try:
        if os.path.exists(input_path):
            shutil.copy2(input_path, out_path)
            output_pathname = Pathname(out_path)
            return lisptype.MultipleValues(output_pathname, lisptype.NIL, lisptype.NIL)
        else:
            # File doesn't exist - return failure
            return lisptype.MultipleValues(lisptype.NIL, lisptype.NIL, lisptype.T)
    except Exception as e:
        # Compilation failed
        return lisptype.MultipleValues(lisptype.NIL, lisptype.NIL, lisptype.T)


@_registry.cl_function('COMPILE-FILE-PATHNAME')
def compile_file_pathname(input_file, output_file=None, **kwargs):
    """Get compiled file pathname.
    
    Returns the pathname that COMPILE-FILE would produce for the given input file.
    Returns a .fasl extension version of the input file. The load function
    will handle loading the source if the .fasl doesn't exist.
    """
    from fclpy.lispfunc.pathnames import Pathname
    import os
    
    # Resolve input path similar to compile_file so pathname reflects real location
    import fclpy.state as state
    env = state.current_environment

    if isinstance(input_file, Pathname):
        input_str = input_file.original
    else:
        input_str = str(input_file)

    import os
    if not os.path.isabs(input_str):
        # Try LISP_CWD
        lisp_cwd = os.environ.get('LISP_CWD')
        if lisp_cwd:
            candidate = os.path.normpath(os.path.join(lisp_cwd, input_str))
            if os.path.exists(candidate):
                input_str = candidate

        if env is not None:
            load_truename_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-TRUENAME*')
            load_truename = env.find_variable(load_truename_sym)
            try:
                from fclpy.lispfunc.pathnames import Pathname as PN
            except Exception:
                PN = None
            if load_truename and load_truename is not lisptype.NIL and PN is not None and isinstance(load_truename, PN):
                current_file_path = load_truename.original
                current_dir = os.path.dirname(current_file_path)
                if current_dir:
                    candidate = os.path.normpath(os.path.join(current_dir, input_str))
                    if os.path.exists(candidate):
                        input_str = candidate

        if env is not None:
            try:
                from fclpy.lispfunc.pathnames import Pathname as PN
            except Exception:
                PN = None
            default_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*DEFAULT-PATHNAME-DEFAULTS*')
            default_pathname = env.find_variable(default_sym)
            if default_pathname and default_pathname is not lisptype.NIL and PN is not None and isinstance(default_pathname, PN):
                default_path = default_pathname.original
                if os.path.isdir(default_path):
                    default_dir = default_path
                else:
                    default_dir = os.path.dirname(default_path)
                if default_dir:
                    candidate = os.path.normpath(os.path.join(default_dir, input_str))
                    if os.path.exists(candidate):
                        input_str = candidate

    base = os.path.splitext(input_str)[0]
    result = base + ".fasl"
    return Pathname(result)


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
