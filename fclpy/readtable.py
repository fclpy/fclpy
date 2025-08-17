#!/usr/bin/env python3
"""
Simplified centralized readtable implementation for FCLpy.
This module provides a single centralized location for all macro character handling.
"""

from typing import Dict, Tuple, Callable, Optional, Any

# Global readtable instance
_current_readtable = None

class Readtable:
    """
    Centralized readtable for managing macro characters and reader macros.
    This replaces the scattered macro character implementations across multiple modules.
    """
    
    def __init__(self):
        self._macro_characters: Dict[str, Tuple[Callable, bool]] = {}
        self._dispatch_macro_characters: Dict[str, Dict[str, Callable]] = {}
        self._case = 'UPCASE'  # :UPCASE, :DOWNCASE, :PRESERVE, :INVERT
        
        # Initialize with standard Common Lisp macro characters
        self._setup_standard_macros()
    
    def _setup_standard_macros(self):
        """Set up the standard Common Lisp macro characters."""
        # Standard terminating macro characters
        self.set_macro_character('(', self._left_paren_reader, False)
        self.set_macro_character(')', self._right_paren_reader, True)
        self.set_macro_character('"', self._string_reader, True)
        self.set_macro_character("'", self._quote_reader, True)
        self.set_macro_character(';', self._semicolon_reader, True)
        
        # Standard non-terminating macro characters
        self.set_macro_character('`', self._backquote_reader, True)
        self.set_macro_character(',', self._comma_reader, True)
        
        # Dispatch macro character
        self.set_macro_character('#', self._sharp_reader, False)
        
    def get_macro_character(self, char: str) -> Optional[Tuple[Callable, bool]]:
        """
        Get the macro character function and terminating flag for a character.
        Returns (function, non_terminating_p) or None if not a macro character.
        """
        return self._macro_characters.get(char)
    
    def set_macro_character(self, char: str, function: Callable, non_terminating_p: bool = False):
        """
        Set a macro character function.
        
        Args:
            char: The character to set as a macro character
            function: The reader function to call
            non_terminating_p: True if this is a non-terminating macro character
        """
        self._macro_characters[char] = (function, non_terminating_p)
    
    def get_dispatch_macro_character(self, dispatch_char: str, sub_char: str) -> Optional[Callable]:
        """Get a dispatch macro character function."""
        dispatch_table = self._dispatch_macro_characters.get(dispatch_char)
        if dispatch_table:
            return dispatch_table.get(sub_char)
        return None
    
    def set_dispatch_macro_character(self, dispatch_char: str, sub_char: str, function: Callable):
        """Set a dispatch macro character function."""
        if dispatch_char not in self._dispatch_macro_characters:
            self._dispatch_macro_characters[dispatch_char] = {}
        self._dispatch_macro_characters[dispatch_char][sub_char] = function
    
    def readtable_case(self):
        """Get the current readtable case setting."""
        return self._case
    
    def set_readtable_case(self, case: str):
        """Set the readtable case (:UPCASE, :DOWNCASE, :PRESERVE, :INVERT)."""
        self._case = case
    
    # Simple macro character implementations that don't create circular dependencies
    def _left_paren_reader(self, char, stream):
        """Read a list starting with ("""
        result = []
        
        while True:
            # Skip whitespace
            c = stream.read_char()
            if not c:
                raise ValueError("EOF during list read")
            if c.isspace():
                continue
            if c == ')':
                break
            else:
                # Put the character back and read the next item
                stream.unread_char(c)
                item = self._read_item(stream)
                if item is not None:
                    result.append(item)
        
        # Convert to Lisp cons structure
        from . import lisptype
        lisp_list = lisptype.NIL
        for item in reversed(result):
            lisp_list = lisptype.lispCons(item, lisp_list)
        return lisp_list
    
    def _read_item(self, stream):
        """Read a single item from the stream"""
        # Skip whitespace
        c = stream.read_char()
        while c and c.isspace():
            c = stream.read_char()
            
        if not c:
            return None
            
        # Handle different token types
        if c.isdigit() or c == '-' or c == '+':
            # Read number (might be negative or positive)
            return self._read_number(c, stream)
        elif c == '"':
            # Read string
            return self._read_string_literal(stream)
        elif c == "'":
            # Read quoted expression
            return self._read_quote(stream)
        elif c == '(':
            # Read nested list
            return self._left_paren_reader(c, stream)
        elif c == ')':
            raise ValueError("Unexpected closing parenthesis")
        elif c == ';':
            # Skip comment and read next item
            self._skip_comment(stream)
            return self._read_item(stream)
        else:
            # Read symbol
            return self._read_symbol(c, stream)
    
    def _read_number(self, first_char, stream):
        """Read a numeric token"""
        token = first_char
        while True:
            c = stream.read_char()
            if not c or c.isspace() or c in '()':
                if c:
                    stream.unread_char(c)
                break
            token += c
        try:
            return int(token)
        except ValueError:
            try:
                return float(token)
            except ValueError:
                # Not a number, treat as symbol
                from . import lisptype
                return lisptype.LispSymbol(token.upper())
    
    def _read_string_literal(self, stream):
        """Read a string literal (already consumed opening quote)"""
        result = ""
        while True:
            c = stream.read_char()
            if not c:
                raise ValueError("EOF in string literal")
            if c == '"':
                break
            if c == '\\':
                # Handle escape sequences
                next_c = stream.read_char()
                if not next_c:
                    raise ValueError("EOF after escape in string")
                if next_c == 'n':
                    result += '\n'
                elif next_c == 't':
                    result += '\t'
                elif next_c == 'r':
                    result += '\r'
                elif next_c == '\\':
                    result += '\\'
                elif next_c == '"':
                    result += '"'
                else:
                    result += next_c
            else:
                result += c
        return result
    
    def _read_quote(self, stream):
        """Read a quoted expression"""
        expr = self._read_item(stream)
        from . import lisptype
        quote_sym = lisptype.LispSymbol("QUOTE")
        return lisptype.lispCons(quote_sym, lisptype.lispCons(expr, lisptype.NIL))
    
    def _read_symbol(self, first_char, stream):
        """Read a symbol token"""
        token = first_char
        while True:
            c = stream.read_char()
            if not c or c.isspace() or c in '()':
                if c:
                    stream.unread_char(c)
                break
            token += c
        from . import lisptype
        return lisptype.LispSymbol(token.upper())
    
    def _skip_comment(self, stream):
        """Skip a comment to end of line"""
        while True:
            c = stream.read_char()
            if not c or c == '\n':
                break
    
    def _right_paren_reader(self, char, stream):
        """Handle unmatched right parenthesis."""
        raise ValueError("Unmatched closing parenthesis")
    
    def _string_reader(self, char, stream):
        """Read a string literal."""
        return self._read_string_literal(stream)
    
    def _quote_reader(self, char, stream):
        """Read a quoted expression."""
        return self._read_quote(stream)
    
    def _semicolon_reader(self, char, stream):
        """Read a comment - skip to end of line and return next item."""
        self._skip_comment(stream)
        # Comments don't return a value, so we need to read the next item
        return None  # This will cause LispReader to continue reading
    
    def _backquote_reader(self, char, stream):
        """Read a backquoted expression."""
        raise RuntimeError("_backquote_reader should not be called directly")
    
    def _comma_reader(self, char, stream):
        """Read a comma expression (unquote)."""
        raise RuntimeError("_comma_reader should not be called directly")
    
    def _sharp_reader(self, char, stream):
        """Handle dispatch macro characters starting with #."""
        raise RuntimeError("_sharp_reader should not be called directly")


def get_current_readtable() -> Readtable:
    """Get the current global readtable."""
    global _current_readtable
    if _current_readtable is None:
        _current_readtable = Readtable()
    return _current_readtable

def set_current_readtable(readtable: Readtable):
    """Set the current global readtable."""
    global _current_readtable
    _current_readtable = readtable

# Convenience functions for backward compatibility
def get_macro_character(char: str) -> Optional[Callable]:
    """Get just the macro character function (for backward compatibility)."""
    result = get_current_readtable().get_macro_character(char)
    if result:
        return result[0]  # Return just the function, not the tuple
    return None

def set_macro_character(char: str, function: Callable, non_terminating_p: bool = False):
    """Set a macro character function (for backward compatibility)."""
    get_current_readtable().set_macro_character(char, function, non_terminating_p)

def get_dispatch_macro_character(dispatch_char: str, sub_char: str) -> Optional[Callable]:
    """Get a dispatch macro character function (for backward compatibility)."""
    return get_current_readtable().get_dispatch_macro_character(dispatch_char, sub_char)

def set_dispatch_macro_character(dispatch_char: str, sub_char: str, function: Callable):
    """Set a dispatch macro character function (for backward compatibility)."""
    get_current_readtable().set_dispatch_macro_character(dispatch_char, sub_char, function)
