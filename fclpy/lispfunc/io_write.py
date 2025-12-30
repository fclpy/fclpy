"""I/O write operations - stream output, printing, pathnames, and file operations."""

import fclpy.lisptype as lisptype
from . import registry as _registry


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

def _format_directive(control_string, args, pos, arg_idx):
    """Process a single format directive starting at pos (after ~).
    
    Returns (output_string, new_pos, new_arg_idx, consumed_arg).
    """
    if pos >= len(control_string):
        return ('~', pos, arg_idx, False)
    
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
            if arg_idx < len(args):
                params.append(args[arg_idx])
                arg_idx += 1
            pos += 1
        elif c == '#':
            # Number of remaining arguments
            params.append(len(args) - arg_idx)
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
        return ('~', pos, arg_idx, False)
    
    directive = control_string[pos].upper()
    pos += 1
    
    # Helper to get next arg
    def get_arg():
        nonlocal arg_idx
        if arg_idx < len(args):
            val = args[arg_idx]
            arg_idx += 1
            return val
        return None
    
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
        return (result, pos, arg_idx, True)
    
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
        return (result, pos, arg_idx, True)
    
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
        return (result, pos, arg_idx, True)
    
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
        return (result, pos, arg_idx, True)
    
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
        return (result, pos, arg_idx, True)
    
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
        return (result, pos, arg_idx, True)
    
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
        return (result, pos, arg_idx, True)
    
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
        return (result, pos, arg_idx, True)
    
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
        return (result, pos, arg_idx, True)
    
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
        return (result, pos, arg_idx, True)
    
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
        return (result, pos, arg_idx, True)
    
    elif directive == '%':
        # ~% - Newline
        count = params[0] if params and params[0] else 1
        return ('\n' * count, pos, arg_idx, False)
    
    elif directive == '&':
        # ~& - Fresh line (newline only if not at start of line)
        count = params[0] if params and params[0] else 1
        # We don't track column, so just emit newline
        return ('\n' * count, pos, arg_idx, False)
    
    elif directive == '~':
        # ~~ - Literal tilde
        count = params[0] if params and params[0] else 1
        return ('~' * count, pos, arg_idx, False)
    
    elif directive == '|':
        # ~| - Page separator (form feed)
        count = params[0] if params and params[0] else 1
        return ('\f' * count, pos, arg_idx, False)
    
    elif directive == 'T':
        # ~T - Tabulation
        colnum = params[0] if params else 1
        colinc = params[1] if len(params) > 1 else 1
        # We don't track column, so just emit spaces
        return (' ' * (colnum if colnum else 1), pos, arg_idx, False)
    
    elif directive == '*':
        # ~* - Go to argument
        if at_flag:
            # Go to absolute argument position
            new_idx = params[0] if params and params[0] is not None else 0
            arg_idx = new_idx
        elif colon_flag:
            # Go backwards
            count = params[0] if params and params[0] is not None else 1
            arg_idx = max(0, arg_idx - count)
        else:
            # Go forwards
            count = params[0] if params and params[0] is not None else 1
            arg_idx = min(len(args), arg_idx + count)
        return ('', pos, arg_idx, False)
    
    elif directive == '?':
        # ~? - Recursive processing
        # The next arg is a format string, and the one after is args for it
        fmt_str = get_arg()
        if at_flag:
            # Use remaining args
            result = _format_process(str(fmt_str) if fmt_str else '', args[arg_idx:])
            arg_idx = len(args)  # All remaining args consumed
        else:
            fmt_args = get_arg()
            if isinstance(fmt_args, (list, tuple)):
                result = _format_process(str(fmt_str) if fmt_str else '', fmt_args)
            else:
                result = _format_process(str(fmt_str) if fmt_str else '', [fmt_args] if fmt_args else [])
        return (result, pos, arg_idx, True)
    
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
        
        inner_result = _format_process(inner, args[arg_idx:])
        
        # Count args consumed in inner
        # Simple approximation: count ~A, ~S, ~D etc.
        consumed = inner.count('~')  # rough
        arg_idx = min(len(args), arg_idx + consumed)
        
        if colon_flag and at_flag:
            result = inner_result.upper()
        elif colon_flag:
            result = inner_result.capitalize()
        elif at_flag:
            # Capitalize each word
            result = ' '.join(w.capitalize() for w in inner_result.split())
        else:
            result = inner_result.lower()
        
        return (result, end_pos, arg_idx, False)
    
    elif directive == ')':
        # End of case conversion - should not be reached directly
        return ('', pos, arg_idx, False)
    
    elif directive == '[':
        # ~[ ... ~] - Conditional
        # Find the clauses and closing ~]
        nesting = 1
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
                        elif d == ';' and nesting == 1:
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
        
        if colon_flag:
            # ~:[ test ~; else ~]
            val = get_arg()
            # T is truthy, NIL/False/None are falsy
            is_true = val is not None and val is not lisptype.NIL and val is not False
            # Also check for T symbol
            if val is lisptype.T:
                is_true = True
            if is_true:
                result = _format_process(clauses[1] if len(clauses) > 1 else '', args[arg_idx:])
            else:
                result = _format_process(clauses[0] if clauses else '', args[arg_idx:])
        elif at_flag:
            # ~@[ test ~] - if arg is non-nil, process with arg, else skip
            val = get_arg()
            if val is not None and val is not lisptype.NIL and val is not False:
                # Process clause with this arg (don't consume it)
                arg_idx -= 1
                result = _format_process(clauses[0] if clauses else '', args[arg_idx:])
            else:
                result = ''
        else:
            # ~[ clause0 ~; clause1 ~; ... ~] - select by index
            val = get_arg()
            try:
                idx = int(val) if val is not None else 0
                if 0 <= idx < len(clauses):
                    result = _format_process(clauses[idx], args[arg_idx:])
                elif default_clause is not None and default_clause < len(clauses):
                    result = _format_process(clauses[default_clause], args[arg_idx:])
                else:
                    result = ''
            except (TypeError, ValueError):
                result = ''
        
        return (result, end_pos, arg_idx, True)
    
    elif directive == ']':
        return ('', pos, arg_idx, False)
    
    elif directive == '{':
        # ~{ ... ~} - Iteration
        # Find matching ~}
        nesting = 1
        end_pos = pos
        while end_pos < len(control_string) and nesting > 0:
            if control_string[end_pos] == '~':
                if end_pos + 1 < len(control_string):
                    next_char = control_string[end_pos + 1].upper()
                    if next_char == '{':
                        nesting += 1
                    elif next_char == '}':
                        nesting -= 1
            end_pos += 1
        
        inner = control_string[pos:end_pos-2] if end_pos >= 2 else ''
        
        if at_flag:
            # Use remaining args as list
            items = list(args[arg_idx:])
            arg_idx = len(args)
        else:
            # Next arg is a list
            items = get_arg()
            if items is None or items is lisptype.NIL:
                items = []
            elif not isinstance(items, (list, tuple)):
                items = [items]
        
        result_parts = []
        if colon_flag:
            # Each item is a sublist
            for item in items:
                if isinstance(item, (list, tuple)):
                    result_parts.append(_format_process(inner, list(item)))
                else:
                    result_parts.append(_format_process(inner, [item]))
        else:
            # Items are consumed one at a time
            item_list = list(items)
            while item_list:
                # Process inner, consuming as many args as needed
                result_parts.append(_format_process(inner, item_list))
                # Rough estimate: inner consumes some items
                consumed = max(1, inner.count('~A') + inner.count('~a') + 
                              inner.count('~S') + inner.count('~s') +
                              inner.count('~D') + inner.count('~d'))
                item_list = item_list[consumed:]
        
        return (''.join(result_parts), end_pos, arg_idx, True)
    
    elif directive == '}':
        return ('', pos, arg_idx, False)
    
    elif directive == '^':
        # ~^ - Escape from iteration (only in ~{ ~})
        # This should cause iteration to stop
        # For now, return empty (caller should handle)
        return ('', pos, arg_idx, False)
    
    elif directive == '\n':
        # ~<newline> - Ignored newline
        if at_flag:
            # Keep the newline
            return ('\n', pos, arg_idx, False)
        else:
            # Ignore newline and following whitespace
            while pos < len(control_string) and control_string[pos] in ' \t':
                pos += 1
            return ('', pos, arg_idx, False)
    
    elif directive == 'P':
        # ~P - Plural
        if colon_flag:
            # Back up one arg
            arg_idx = max(0, arg_idx - 1)
        val = get_arg() if not colon_flag else (args[arg_idx - 1] if arg_idx > 0 else 1)
        try:
            num = int(val) if val is not None else 1
            if at_flag:
                result = 'y' if num == 1 else 'ies'
            else:
                result = '' if num == 1 else 's'
        except (TypeError, ValueError):
            result = 's'
        return (result, pos, arg_idx, False)
    
    else:
        # Unknown directive - just output the tilde and char
        return ('~' + directive, pos, arg_idx, False)


def _format_process(control_string, args):
    """Process a format control string with arguments."""
    result = []
    pos = 0
    arg_idx = 0
    args = list(args) if args else []
    
    while pos < len(control_string):
        c = control_string[pos]
        if c == '~':
            pos += 1
            output, pos, arg_idx, _ = _format_directive(control_string, args, pos, arg_idx)
            result.append(output)
        else:
            result.append(c)
            pos += 1
    
    return ''.join(result)


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
        # Assume destination is a stream
        if hasattr(destination, 'write'):
            destination.write(formatted)
        else:
            print(formatted, end='')
        return lisptype.NIL


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
    
    # Get the input path
    if isinstance(input_file, Pathname):
        input_path = input_file.original
    else:
        input_path = str(input_file)
    
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
    
    # Convert to string if needed
    if isinstance(input_file, Pathname):
        input_str = str(input_file)
    else:
        input_str = str(input_file)
    
    base = os.path.splitext(input_str)[0]
    result = base + ".fasl"
    return Pathname(result)


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
