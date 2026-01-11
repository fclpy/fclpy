"""
Lisp Printer - converts Lisp objects back to text.

This module implements prin1 and princ functions, with proper formatting
for different types of Lisp objects.
"""

import fclpy.lisptype as lisptype
from fclpy.lisptype import (
    LispSymbol, lispKeyword, Character, lispCons, lispNull, NIL,
    lispNull as LispNull, LispString
)


def prin1(obj, stream=None):
    """Print object in readable form (can be read back with reader).
    
    Args:
        obj: Object to print
        stream: Output stream (not used yet, returns string)
        
    Returns:
        String representation of object
    """
    return _print_object(obj, escape=True)


def princ(obj, stream=None):
    """Print object in canonical form (human-readable).
    
    Similar to prin1 but with minimal escaping for human readability.
    
    Args:
        obj: Object to print
        stream: Output stream (not used yet, returns string)
        
    Returns:
        String representation of object
    """
    return _print_object(obj, escape=False)


def _print_object(obj, escape=True):
    """Internal function to print objects with optional escaping.
    
    Args:
        obj: Object to print
        escape: Whether to include escaping for readability
        
    Returns:
        String representation
    """
    if obj is None or isinstance(obj, lispNull):
        return "NIL"
    elif isinstance(obj, bool):
        return "T" if obj else "NIL"
    elif isinstance(obj, int):
        return str(obj)
    elif isinstance(obj, float):
        return str(obj)
    elif isinstance(obj, (str, LispString)):
        return _print_string(str(obj), escape)
    elif isinstance(obj, lispKeyword):
        return _print_keyword(obj)
    elif isinstance(obj, LispSymbol):
        return _print_symbol(obj)
    elif isinstance(obj, Character):
        return _print_character(obj)
    elif isinstance(obj, lispCons):
        return _print_list(obj, escape)
    else:
        # Fallback for unknown types
        return str(obj)


def _print_string(s, escape=True):
    """Print a string literal with proper escaping.
    
    Args:
        s: String to print
        escape: Whether to escape special characters
        
    Returns:
        Quoted string with escapes
    """
    if not escape:
        # For princ, just return the string content
        return s
    
    # For prin1, quote and escape
    result = ['"']
    for char in s:
        if char == '"':
            result.append('\\"')
        elif char == '\\':
            result.append('\\\\')
        elif char == '\n':
            result.append('\\n')
        elif char == '\t':
            result.append('\\t')
        elif char == '\r':
            result.append('\\r')
        else:
            result.append(char)
    result.append('"')
    return ''.join(result)


def _print_keyword(kw):
    """Print a keyword.
    
    Args:
        kw: lispKeyword object
        
    Returns:
        String like ":FOO"
    """
    return f":{kw.name}"


def _print_symbol(sym):
    """Print a symbol.
    
    Args:
        sym: LispSymbol object
        
    Returns:
        String representation of symbol (in uppercase)
    """
    name = sym.name
    
    # Check if symbol needs quoting (contains special characters)
    if _needs_quoting(name):
        # Return quoted symbol |...|
        return f"|{name}|"
    
    return name


def _needs_quoting(name):
    """Check if a symbol name needs quoting.
    
    Symbols need quoting if they contain special characters that would
    cause them to be parsed differently.
    
    Args:
        name: Symbol name string
        
    Returns:
        True if quoting is needed
    """
    if not name:
        return True
    
    # Check for special characters that require quoting
    special_chars = set('()\'";,#`\\|')
    
    if any(c in special_chars for c in name):
        return True
    
    # Check if it looks like a number
    try:
        int(name)
        return True  # Looks like number, needs quoting
    except ValueError:
        pass
    
    try:
        float(name)
        return True  # Looks like float, needs quoting
    except ValueError:
        pass
    
    # Check for whitespace
    if any(c.isspace() for c in name):
        return True
    
    return False


def _print_character(char):
    r"""Print a character literal.
    
    Args:
        char: Character object
        
    Returns:
        String like "#\A" or "#\Space"
    """
    c = char.char
    
    # Check for named characters
    named_chars = {
        ' ': 'Space',
        '\n': 'Newline',
        '\t': 'Tab',
        '\r': 'Return',
        '\b': 'Backspace',
        '\f': 'Form-Feed',
        '\x7f': 'Rubout',
    }
    
    if c in named_chars:
        return f"#\\{named_chars[c]}"
    
    # For printable ASCII, use single character
    if 32 <= ord(c) < 127:
        return f"#\\{c}"
    
    # For other characters, use Unicode escape
    return f"#\\U{ord(c):04x}"


def _print_list(lst, escape=True):
    """Print a list.
    
    Args:
        lst: lispCons object
        escape: Whether to escape content
        
    Returns:
        String representation like "(a b c)"
    """
    if lst is None or isinstance(lst, lispNull):
        return "NIL"
    
    # Check for special list forms
    if isinstance(lst, lispCons):
        # Check for dotted notation
        elements = []
        current = lst
        has_dot = False
        dot_tail = None
        
        while isinstance(current, lispCons):
            elements.append(current.car)
            current = current.cdr
            
            if current is not None and not isinstance(current, lispNull) and not isinstance(current, lispCons):
                # Dotted list
                has_dot = True
                dot_tail = current
                break
        
        # Build result string
        result = ["("]
        for i, elem in enumerate(elements):
            if i > 0:
                result.append(" ")
            result.append(_print_object(elem, escape))
        
        if has_dot:
            result.append(" . ")
            result.append(_print_object(dot_tail, escape))
        
        result.append(")")
        return ''.join(result)
    
    return "NIL"


def print_object(obj, escape=True):
    """Public function to print objects.
    
    This is an alias for _print_object that can be used directly.
    
    Args:
        obj: Object to print
        escape: Whether to escape for readability
        
    Returns:
        String representation
    """
    return _print_object(obj, escape)
