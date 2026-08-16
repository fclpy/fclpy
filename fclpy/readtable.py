#!/usr/bin/env python3
"""
Simplified centralized readtable implementation for FCLpy.
This module provides a single centralized location for all macro character handling.
"""

from typing import Dict, Tuple, Callable, Optional, Any

# The standard readtable (CLHS 23.1.1). Built once, on demand.
_standard_readtable = None

# The four values `readtable-case` can take (CLHS 23.1.2).
READTABLE_CASES = ('UPCASE', 'DOWNCASE', 'PRESERVE', 'INVERT')


class Readtable:
    """
    Centralized readtable for managing macro characters and reader macros.
    This replaces the scattered macro character implementations across multiple modules.
    """

    def __init__(self):
        self._macro_characters: Dict[str, Tuple[Callable, bool]] = {}
        self._dispatch_macro_characters: Dict[str, Dict[str, Callable]] = {}
        self._case = 'UPCASE'  # :UPCASE, :DOWNCASE, :PRESERVE, :INVERT
        # True only for the one object `standard_readtable()` returns.
        self._standard = False

        # Initialize with standard Common Lisp macro characters
        self._setup_standard_macros()

    def _check_mutable(self, what: str):
        """The standard readtable is immutable (CLHS 23.1.1).

        It is shared, and NIL denotes it wherever a readtable designator is
        accepted, so a form that mutated it would silently redefine what
        "standard syntax" means for the rest of the session -- including for
        every later `(copy-readtable nil)`.
        """
        if self._standard:
            from . import lisptype
            raise lisptype.LispError(
                f"{what}: the standard readtable may not be modified "
                "(CLHS 23.1.1); copy it with (copy-readtable nil) first")

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
        self._check_mutable('SET-MACRO-CHARACTER')
        self._macro_characters[char] = (function, non_terminating_p)
    
    def get_dispatch_macro_character(self, dispatch_char: str, sub_char: str) -> Optional[Callable]:
        """Get a dispatch macro character function."""
        dispatch_table = self._dispatch_macro_characters.get(dispatch_char)
        if dispatch_table:
            return dispatch_table.get(sub_char)
        return None
    
    def set_dispatch_macro_character(self, dispatch_char: str, sub_char: str, function: Callable):
        """Set a dispatch macro character function."""
        self._check_mutable('SET-DISPATCH-MACRO-CHARACTER')
        if dispatch_char not in self._dispatch_macro_characters:
            self._dispatch_macro_characters[dispatch_char] = {}
        self._dispatch_macro_characters[dispatch_char][sub_char] = function
    
    def readtable_case(self):
        """The readtable case, as one of `READTABLE_CASES`.

        This is the *internal* spelling, which the reader and the printer both
        consult directly. `READTABLE-CASE` the Lisp function answers the
        corresponding keyword -- see `case_keyword`.
        """
        return self._case

    def set_readtable_case(self, case: str):
        """Set the readtable case (:UPCASE, :DOWNCASE, :PRESERVE, :INVERT)."""
        self._check_mutable('SETF READTABLE-CASE')
        if case not in READTABLE_CASES:
            from . import lisptype
            raise lisptype.LispTypeError(
                f"SETF READTABLE-CASE: {case!r} is not one of "
                f"{', '.join(READTABLE_CASES)} (CLHS 23.1.2)",
                expected_type="(MEMBER :UPCASE :DOWNCASE :PRESERVE :INVERT)",
                actual_value=case)
        self._case = case

    def copy(self) -> 'Readtable':
        """Create a copy of this readtable.
        
        The copy has the same macro characters and dispatch characters,
        but modifying the copy does not affect the original.
        
        Returns:
            A new Readtable instance with copied settings.
        """
        new_rt = Readtable.__new__(Readtable)
        # Create shallow copies of the dictionaries
        new_rt._macro_characters = dict(self._macro_characters)
        new_rt._dispatch_macro_characters = {
            k: dict(v) for k, v in self._dispatch_macro_characters.items()
        }
        new_rt._case = self._case
        # A copy of the standard readtable is an ordinary, mutable readtable --
        # that is the whole point of `(copy-readtable nil)`.
        new_rt._standard = False
        return new_rt

    def copy_into(self, target: 'Readtable') -> 'Readtable':
        """Overwrite `target` with this readtable's syntax and return it.

        `COPY-READTABLE`'s `to-readtable` argument (CLHS): when supplied, the
        readtable it names is *modified* and returned rather than a fresh one
        being made, which is how `copy-readtable.6` observes that the result is
        EQL to the table it passed in.
        """
        target._check_mutable('COPY-READTABLE')
        target._macro_characters = dict(self._macro_characters)
        target._dispatch_macro_characters = {
            k: dict(v) for k, v in self._dispatch_macro_characters.items()
        }
        target._case = self._case
        return target
    
    # Simple macro character implementations that don't create circular dependencies
    def _left_paren_reader(self, char, stream):
        """Read a list starting with ("""
        result = []
        dotted_tail = None
        
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
                    # Check if this is the dot notation for a dotted pair
                    # The dot is a LispSymbol with name '.'
                    from . import lisptype
                    if isinstance(item, lisptype.LispSymbol) and item.name == '.':
                        # Next item becomes the cdr (tail) of the list
                        # Skip whitespace after the dot
                        c = stream.read_char()
                        while c and c.isspace():
                            c = stream.read_char()
                        if c:
                            stream.unread_char(c)
                        # Read the tail
                        dotted_tail = self._read_item(stream)
                        # After the tail, we expect either whitespace and ) or just )
                        c = stream.read_char()
                        while c and c.isspace():
                            c = stream.read_char()
                        if c != ')':
                            raise ValueError(f"Expected ) after dotted tail, got {c}")
                        break
                    else:
                        result.append(item)
        
        # Convert to Lisp cons structure
        from . import lisptype
        if dotted_tail is not None:
            # Build list with dotted tail
            lisp_list = dotted_tail
            for item in reversed(result):
                lisp_list = lisptype.lispCons(item, lisp_list)
        else:
            # Regular proper list
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
        # If this character is a macro character, dispatch to its handler
        mc = self.get_macro_character(c)
        if mc is not None:
            # mc may be (function, non_terminating_p) or a raw function
            func = mc[0] if isinstance(mc, tuple) else mc
            return func(c, stream)
            
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
        elif c == '`':
            # Backquote/quasiquote
            return self._backquote_reader(c, stream)
        elif c == ',':
            # Unquote or unquote-splicing
            return self._comma_reader(c, stream)
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
        
        # Try to parse as ratio (e.g., 1/2, -3/4)
        if '/' in token:
            parts = token.split('/')
            if len(parts) == 2:
                try:
                    from fractions import Fraction
                    numerator = int(parts[0])
                    denominator = int(parts[1])
                    return Fraction(numerator, denominator)
                except (ValueError, ZeroDivisionError):
                    pass  # Not a valid ratio, fall through
        
        try:
            return int(token)
        except ValueError:
            try:
                # Try standard Python float first
                return float(token)
            except ValueError:
                # Try to handle Common Lisp exponent markers (D, F, S, L)
                # Normalize exponent markers to E for Python
                normalized = token.upper()
                for marker in 'DFSL':
                    normalized = normalized.replace(marker, 'E')
                try:
                    return float(normalized)
                except ValueError:
                    # Not a number, treat as symbol (intern into user package)
                    from . import lisptype
                    return lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol(token)
    
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
        from . import lisptype
        return lisptype.LispString(result)
    
    def _read_quote(self, stream):
        """Read a quoted expression"""
        expr = self._read_item(stream)
        from . import lisptype
        quote_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol("QUOTE")
        return lisptype.lispCons(quote_sym, lisptype.lispCons(expr, lisptype.NIL))
    
    def _read_symbol(self, first_char, stream):
        """Read a symbol token with package awareness.
        
        When reading an unqualified symbol:
        1. Check current *PACKAGE* (from state.current_package)
        2. Look for existing symbol in current package
        3. Look for exported symbol in USE'd packages
        4. If not found, intern in current package
        """
        token = first_char
        while True:
            c = stream.read_char()
            if not c or c.isspace() or c in '()':
                if c:
                    stream.unread_char(c)
                break
            # Handle backslash escape
            if c == '\\':
                escaped = stream.read_char()
                if not escaped:
                    raise Exception("reader-error: EOF after escape")
                # Use placeholder for escaped colons
                if escaped == ':':
                    token += '\x00'
                else:
                    token += escaped
            else:
                token += c
        from . import lisptype
        from . import state
        
        # If symbol starts with a leading colon, treat it as a keyword
        if token.startswith(':'):
            # Intern keyword with the name after the colon
            name = token[1:]
            return lisptype.intern_keyword(name.upper())
        
        # Handle package-qualified symbols (PKG:SYM or PKG::SYM)
        # Only treat as package-qualified if contains a real colon (not escaped placeholder \x00)
        token_check = token.replace('\x00', '')
        if ':' in token_check and not token.startswith(':'):
            return self._read_package_qualified_symbol(token)
        
        # Get current package
        current_pkg = getattr(state, 'current_package', None)
        if current_pkg is None:
            current_pkg = lisptype.COMMON_LISP_USER_PACKAGE
        
        # Restore escaped colons before interning
        token_restored = token.replace('\x00', ':')
        name_upper = token_restored.upper()
        
        # CRITICAL: In Common Lisp, NIL is both the symbol and the empty list; the
        # reader should return the canonical NIL object rather than a
        # fresh symbol. Similarly, T should return the global T symbol.
        if name_upper == 'NIL':
            return lisptype.NIL
        if name_upper == 'T':
            return lisptype.T
        
        # First check if symbol exists in current package
        sym, status = current_pkg.find_symbol(name_upper)
        if sym is not None:
            return sym
        
        # Check USE'd packages for exported symbols
        for used_pkg in getattr(current_pkg, 'use_packages', []):
            # Handle both Package objects and package names
            if isinstance(used_pkg, str):
                used_pkg = lisptype.find_package(used_pkg)
            if used_pkg is not None:
                # Only look for external symbols in USE'd packages
                if name_upper in getattr(used_pkg, 'external_symbols', set()):
                    sym = used_pkg.symbols.get(name_upper)
                    if sym is not None:
                        return sym
        
        # Not found - intern in current package
        return current_pkg.intern_symbol(token_restored)
    
    def _read_package_qualified_symbol(self, token):
        """Read a package-qualified symbol like PKG:SYM or PKG::SYM.
        
        Single colon (PKG:SYM) means external symbol access.
        Double colon (PKG::SYM) means internal symbol access.
        """
        from . import lisptype
        
        if '::' in token:
            # Internal symbol access
            parts = token.split('::', 1)
            pkg_name = parts[0].upper()
            sym_name = parts[1].upper() if len(parts) > 1 else ''
            # Restore escaped colons in symbol name
            sym_name = sym_name.replace('\x00', ':')
        else:
            # External symbol access
            parts = token.split(':', 1)
            pkg_name = parts[0].upper()
            sym_name = parts[1].upper() if len(parts) > 1 else ''
            # Restore escaped colons in symbol name
            sym_name = sym_name.replace('\x00', ':')
        
        # Find the package
        pkg = lisptype.find_package(pkg_name)
        if pkg is None:
            # Package not found - create it as a fallback
            pkg = lisptype.make_package(pkg_name)
        
        # Intern the symbol in that package
        return pkg.intern_symbol(sym_name)
    
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
        """Read a backquoted (quasiquote) expression.

        `x  => (QUASIQUOTE x)
        This implements a simple quasiquote reader: it wraps the next form with
        the symbol QUASIQUOTE. For more complete quasiquote/unquote behavior
        a future enhancement should perform nested processing.
        """
        expr = self._read_item(stream)
        from . import lisptype
        qq_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol("QUASIQUOTE")
        return lisptype.lispCons(qq_sym, lisptype.lispCons(expr, lisptype.NIL))
    
    def _comma_reader(self, char, stream):
        """Read a comma expression (unquote / unquote-splicing).

        ,x  => (UNQUOTE x)
        ,@x => (UNQUOTE-SPLICING x)
        """
        # Check for @ for unquote-splicing
        next_c = stream.read_char()
        if next_c == '@':
            expr = self._read_item(stream)
            from . import lisptype
            sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol("UNQUOTE-SPLICING")
            return lisptype.lispCons(sym, lisptype.lispCons(expr, lisptype.NIL))
        else:
            if next_c:
                stream.unread_char(next_c)
            expr = self._read_item(stream)
            from . import lisptype
            sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol("UNQUOTE")
            return lisptype.lispCons(sym, lisptype.lispCons(expr, lisptype.NIL))
    
    def _sharp_reader(self, char, stream):
        """Handle dispatch macro characters starting with #.
        
        This reads the next character and dispatches to the appropriate
        sub-character handler, or handles built-in # constructs.
        """
        sub_char = stream.read_char()
        if not sub_char:
            raise ValueError("EOF after #")
        
        sub_char_upper = sub_char.upper()
        
        # Check for registered dispatch macro character
        dispatch_table = self._dispatch_macro_characters.get('#', {})
        if sub_char_upper in dispatch_table:
            return dispatch_table[sub_char_upper](sub_char, stream)
        
        # Handle built-in # constructs
        if sub_char == '|':
            # Block comment #| ... |#
            self._skip_block_comment(stream)
            # Block comments don't return a value, continue reading
            return None
        elif sub_char == "'":
            # Function shorthand: #'x -> (FUNCTION x)
            expr = self._read_item(stream)
            from . import lisptype
            func_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol("FUNCTION")
            return lisptype.lispCons(func_sym, lisptype.lispCons(expr, lisptype.NIL))
        elif sub_char == '\\':
            # Character literal: #\x
            return self._read_character_literal(stream)
        elif sub_char == '(':
            # Vector literal: #(...)
            return self._read_vector(stream)
        elif sub_char == '+':
            # Feature expression #+feature form
            return self._read_feature_plus(stream)
        elif sub_char == '-':
            # Feature expression #-feature form
            return self._read_feature_minus(stream)
        elif sub_char == '.':
            # Read-time evaluation: #.(form)
            expr = self._read_item(stream)
            # Actually evaluate at read time
            import fclpy.state as state
            from fclpy.lispfunc.evaluation_core import eval
            env = state.current_environment
            if env is not None:
                return eval(expr, env)
            else:
                # Fall back to returning the form if no environment
                return expr
        elif sub_char.upper() == 'P':
            # Pathname literal: #P"path" or #p"path"
            return self._read_pathname_literal(stream)
        elif sub_char.upper() == 'X':
            # Hexadecimal number: #xFF -> 255
            return self._read_radix_number(stream, 16)
        elif sub_char.upper() == 'B':
            # Binary number: #b1010 -> 10
            return self._read_radix_number(stream, 2)
        elif sub_char.upper() == 'O':
            # Octal number: #o17 -> 15
            return self._read_radix_number(stream, 8)
        elif sub_char == ':':
            # Uninterned symbol: #:foo -> symbol not interned in any package
            return self._read_uninterned_symbol(stream)
        elif sub_char.upper() == 'C':
            # Complex number: #C(real imag) or #c(real imag)
            return self._read_complex_number(stream)
        elif sub_char == '*':
            # Bit vector: #*101 -> bit vector with elements 1, 0, 1
            return self._read_bit_vector(stream)
        elif sub_char in '0123456789':
            # Could be array rank, reader label, etc.
            # For now, read the number and check what follows
            num = sub_char
            while True:
                c = stream.read_char()
                if c and c.isdigit():
                    num += c
                elif c == '=':
                    # Reader label: #n=expr
                    expr = self._read_item(stream)
                    return expr
                elif c == '#':
                    # Reader reference: #n#
                    return None  # Placeholder
                elif c == 'A' or c == 'a':
                    # Array: #nA(nested sequences) -- CLHS 2.4.8.12. It used
                    # to return the nested *list* structure verbatim, so
                    # `#2a((1 2) (3 4))` read as a list of lists rather than
                    # as a rank-2 array.
                    return self._read_array(stream, int(num))
                elif c == '(':
                    # `#n(...)`: a vector of exactly n elements.
                    return self._read_vector(stream, size=int(num))
                elif c == 'R' or c == 'r':
                    # Radix literal: #nR -> integer read in base n (CLHS 2.4.8.7),
                    # e.g. #3r1021101. Previously unhandled: the digit branch
                    # above only recognized '=', '#' and 'A' as followers, so
                    # anything else (including 'R') fell through to `break`
                    # with the accumulated digits discarded and `r1021101...`
                    # left on the stream to be read as a bare symbol token --
                    # `Unbound variable: R1021101`, not a reader error, which
                    # is what made this defect look evaluator-side.
                    radix = int(num)
                    if radix < 2 or radix > 36:
                        raise ValueError(f"Invalid radix: {radix}")
                    return self._read_radix_number(stream, radix)
                else:
                    if c:
                        stream.unread_char(c)
                    break
            return None
        else:
            raise ValueError(f"Unknown # dispatch character: #{sub_char}")
    
    def _skip_block_comment(self, stream):
        """Skip a block comment #| ... |# with nesting support."""
        depth = 1
        prev_char = None
        
        while depth > 0:
            c = stream.read_char()
            if not c:
                raise ValueError("EOF in block comment")
            
            if prev_char == '|' and c == '#':
                depth -= 1
                prev_char = None
            elif prev_char == '#' and c == '|':
                depth += 1
                prev_char = None
            else:
                prev_char = c
    
    def _read_character_literal(self, stream):
        r"""Read a character literal like #\A or #\Space."""
        from . import character
        
        c = stream.read_char()
        if not c:
            raise ValueError("EOF in character literal")
        
        # Check for named characters
        if c.isalpha():
            # Might be a named character like #\Space or #\Newline
            name = c
            while True:
                next_c = stream.read_char()
                if next_c and (next_c.isalnum() or next_c == '-'):
                    name += next_c
                else:
                    if next_c:
                        stream.unread_char(next_c)
                    break
            
            # Check for named characters
            name_upper = name.upper()
            if len(name) == 1:
                # Single character
                return character.Character(name)
            elif name_upper == 'SPACE':
                return character.Character(' ')
            elif name_upper == 'NEWLINE' or name_upper == 'LINEFEED':
                return character.Character('\n')
            elif name_upper == 'TAB':
                return character.Character('\t')
            elif name_upper == 'RETURN':
                return character.Character('\r')
            elif name_upper == 'PAGE':
                return character.Character('\f')
            elif name_upper == 'BACKSPACE':
                return character.Character('\b')
            elif name_upper == 'RUBOUT' or name_upper == 'DELETE':
                return character.Character('\x7f')
            elif name_upper == 'NULL' or name_upper == 'NUL':
                return character.Character('\x00')
            else:
                # Unknown named char, use as-is if single
                raise ValueError(f"Unknown character name: {name}")
        else:
            return character.Character(c)
    
    def _read_vector(self, stream, size=None):
        """Read a vector literal ``#(...)`` or ``#n(...)``.

        A ``#(...)`` literal is a *simple* vector (CLHS 2.4.8.3), so it reads
        as this implementation's simple-vector representation, a Python list.
        It used to read as an `AdjustableVector`, which made every literal
        vector claim to be adjustable and to have a fill pointer.
        """
        result = []
        while True:
            # Skip whitespace
            c = stream.read_char()
            if not c:
                raise ValueError("EOF in vector literal")
            if c.isspace():
                continue
            if c == ')':
                break
            else:
                stream.unread_char(c)
                item = self._read_item(stream)
                if item is not None:
                    result.append(item)
        
        if size is not None:
            # `#n(...)` is a vector of length n; a shorter element list is
            # padded with its last element (CLHS 2.4.8.3).
            if len(result) > size:
                raise ValueError(
                    f"#{size}(...) given {len(result)} elements")
            if len(result) < size:
                if not result:
                    raise ValueError(f"#{size}() has no element to replicate")
                result.extend([result[-1]] * (size - len(result)))
        return result
    
    def _check_feature(self, feature):
        """Check if a feature expression is satisfied.
        
        Feature can be:
        - A symbol: check if it's in *FEATURES*
        - (AND feature1 feature2 ...): all features must be present
        - (OR feature1 feature2 ...): any feature must be present
        - (NOT feature): feature must be absent
        """
        import fclpy.state as state
        import fclpy.lisptype as lisptype
        
        # Get *FEATURES* list
        features_list = []
        if state.current_environment:
            features_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*FEATURES*')
            features = state.current_environment.find_variable(features_sym)
            if features and features is not lisptype.NIL:
                # Convert to list of uppercase names
                current = features
                while hasattr(current, 'car') and hasattr(current, 'cdr'):
                    item = current.car
                    if isinstance(item, lisptype.lispKeyword):
                        features_list.append(item.name.upper())
                    elif isinstance(item, lisptype.LispSymbol):
                        features_list.append(item.name.upper())
                    current = current.cdr
                    if current is lisptype.NIL:
                        break
        
        # Handle different feature expression types
        if isinstance(feature, lisptype.lispKeyword):
            return feature.name.upper() in features_list
        elif isinstance(feature, lisptype.LispSymbol):
            return feature.name.upper() in features_list
        elif hasattr(feature, 'car') and hasattr(feature, 'cdr'):
            # It's a cons - check for AND, OR, NOT
            operator = feature.car
            if isinstance(operator, lisptype.LispSymbol):
                op_name = operator.name.upper()
                if op_name == 'AND':
                    # All sub-features must be present
                    current = feature.cdr
                    while hasattr(current, 'car') and hasattr(current, 'cdr') and current is not lisptype.NIL:
                        if not self._check_feature(current.car):
                            return False
                        current = current.cdr
                    return True
                elif op_name == 'OR':
                    # Any sub-feature must be present
                    current = feature.cdr
                    while hasattr(current, 'car') and hasattr(current, 'cdr') and current is not lisptype.NIL:
                        if self._check_feature(current.car):
                            return True
                        current = current.cdr
                    return False
                elif op_name == 'NOT':
                    # Feature must be absent
                    sub_feature = feature.cdr
                    if hasattr(sub_feature, 'car'):
                        sub_feature = sub_feature.car
                    return not self._check_feature(sub_feature)
        
        # Unknown feature expression - default to absent
        return False
    
    def _read_feature_plus(self, stream):
        """Read #+feature expr.
        
        Includes the expression only if feature is present in *FEATURES*.
        """
        feature = self._read_item(stream)
        expr = self._read_item(stream)
        
        if self._check_feature(feature):
            return expr
        else:
            # Feature not present - skip the expression
            return None
    
    def _read_feature_minus(self, stream):
        """Read #-feature expr.
        
        Includes the expression only if feature is NOT present in *FEATURES*.
        """
        feature = self._read_item(stream)
        expr = self._read_item(stream)
        
        if not self._check_feature(feature):
            return expr
        else:
            # Feature is present - skip the expression
            return None
    
    def _read_pathname_literal(self, stream):
        """Read a pathname literal like #P\"path/to/file\"."""
        from fclpy.lispfunc.pathnames import Pathname
        
        # Expect a string next
        c = stream.read_char()
        while c and c.isspace():
            c = stream.read_char()
        
        if c != '"':
            raise ValueError(f"Expected string after #P, got: {c}")
        
        # Read the string
        path_str = self._read_string_literal(stream)
        return Pathname(path_str)
    
    _RADIX_DIGIT_CHARS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    def _read_radix_number(self, stream, radix):
        """Read a number in the specified radix (base), CLHS 2.4.8.

        Examples:
            #xFF -> 255 (radix 16)
            #b1010 -> 10 (radix 2)
            #o17 -> 15 (radix 8)
            #3r1021101 -> a base-3 integer (radix 3, via #nR)

        One digit-set formula for any radix 2-36 rather than three
        hand-picked sets plus a `0123456789`-only fallback that silently
        broke every other explicit radix `#nR` can name.

        Args:
            stream: Input stream
            radix: The base (2-36)

        Returns:
            Integer value
        """
        valid_chars = self._RADIX_DIGIT_CHARS[:radix]

        # Read the number token
        token = ''
        negative = False

        # Check for sign
        c = stream.read_char()
        if c == '-':
            negative = True
            c = stream.read_char()
        elif c == '+':
            c = stream.read_char()

        # Read digits
        while c and c.upper() in valid_chars:
            token += c
            c = stream.read_char()

        # Put back the last character if it's not EOF
        if c:
            stream.unread_char(c)

        if not token:
            raise ValueError(f"No digits found for radix-{radix} number")

        # Parse the number
        try:
            value = int(token, radix)
            return -value if negative else value
        except ValueError:
            raise ValueError(f"Invalid radix-{radix} number: {token}")

    def _read_uninterned_symbol(self, stream):
        """Read an uninterned symbol like #:foo.
        
        Uninterned symbols are not part of any package. Each time #:foo is read,
        a fresh symbol with name "FOO" is created that has no home package.
        
        Args:
            stream: Input stream
            
        Returns:
            A new uninterned LispSymbol
        """
        from . import lisptype
        
        # Read the symbol name
        token = ''
        while True:
            c = stream.read_char()
            if not c or c.isspace() or c in '()':
                if c:
                    stream.unread_char(c)
                break
            token += c
        
        if not token:
            raise ValueError("Empty symbol name after #:")
        
        # Create an uninterned symbol (not in any package)
        # Use uppercase for consistency with CL standard
        name = token.upper()
        # Special case: T and NIL should return the canonical symbols
        if name == 'T':
            return lisptype.T
        elif name == 'NIL':
            return lisptype.NIL
        return lisptype.LispSymbol(name, package=None)

    def _read_complex_number(self, stream):
        """Read a complex number literal #C(real imag).
        
        The syntax is #C(real imag) or #c(real imag) where real and imag
        are real numbers (integers, ratios, or floats).
        
        Args:
            stream: Input stream
            
        Returns:
            A Python complex number
        """
        from fractions import Fraction
        
        # Skip whitespace
        while True:
            c = stream.read_char()
            if not c:
                raise ValueError("EOF after #C")
            if not c.isspace():
                break
        
        # Expect opening paren
        if c != '(':
            raise ValueError(f"Expected ( after #C, got {c!r}")
        
        # Read real part
        real_part = self._read_item(stream)
        if real_part is None:
            raise ValueError("Expected real part in #C(...)")
        
        # Read imaginary part
        imag_part = self._read_item(stream)
        if imag_part is None:
            raise ValueError("Expected imaginary part in #C(...)")
        
        # Skip whitespace and find closing paren
        while True:
            c = stream.read_char()
            if not c:
                raise ValueError("EOF in #C(...)")
            if c.isspace():
                continue
            if c == ')':
                break
            else:
                raise ValueError(f"Expected ) in #C(...), got {c!r}")
        
        # Convert Fraction to float for complex construction
        if isinstance(real_part, Fraction):
            real_part = float(real_part)
        if isinstance(imag_part, Fraction):
            imag_part = float(imag_part)
        
        # Ensure both parts are numeric
        if not isinstance(real_part, (int, float)):
            raise ValueError(f"Real part must be a number, got {type(real_part).__name__}")
        if not isinstance(imag_part, (int, float)):
            raise ValueError(f"Imaginary part must be a number, got {type(imag_part).__name__}")
        
        return complex(real_part, imag_part)

    def _read_array(self, stream, rank):
        """Read a ``#nA(...)`` array literal (CLHS 2.4.8.12).

        The dimensions are those of the nested sequences that follow, which
        is why this cannot simply hand the nested structure back: an array of
        rank 2 and a list of lists are different objects.
        """
        contents = self._read_item(stream)
        from fclpy.lispfunc.arrays import make_array

        if rank == 0:
            return make_array(None, initial_contents=contents)
        dimensions = _nested_dimensions(contents, rank)
        return make_array(dimensions, initial_contents=contents)

    def _read_bit_vector(self, stream):
        """Read a bit vector literal #*101.
        
        The syntax is #*bits where bits is a sequence of 0 and 1 characters.
        #*101 creates a bit vector with elements [1, 0, 1].
        #* creates an empty bit vector.
        
        Args:
            stream: Input stream
            
        Returns:
            A list representing a bit vector (to be enhanced with proper bit-vector type)
        """
        bits = []
        while True:
            c = stream.read_char()
            if not c:
                break
            if c == '0':
                bits.append(0)
            elif c == '1':
                bits.append(1)
            elif c.isspace() or c in '()':
                # End of bit vector
                if c:
                    stream.unread_char(c)
                break
            else:
                # Any non-0/1 character ends the bit vector
                stream.unread_char(c)
                break
        
        # A bit vector is an array whose *element type* is BIT (CLHS 15.1.2.2),
        # not a general vector that happens to hold zeroes and ones: the
        # latter is a different type, prints as `#(1 0 1)`, and answers NIL to
        # BIT-VECTOR-P. Returning a bare list conflated the two.
        from fclpy.lispfunc.arrays import make_bit_vector

        return make_bit_vector(bits)


def _nested_dimensions(contents, rank):
    """The dimensions a `#nA` literal's nested sequences describe."""
    dimensions = []
    current = contents
    for _ in range(rank):
        from fclpy.lispfunc.sequence_protocol import seq_elements

        items = seq_elements(current, '#nA')
        dimensions.append(len(items))
        current = items[0] if items else None
    return dimensions


def standard_readtable() -> Readtable:
    """The **standard readtable** (CLHS 23.1.1).

    CLHS makes this a distinct object from the current one, and NIL denotes
    *it* -- not the current readtable -- wherever a readtable designator is
    accepted. There was no such object at all, which is why
    `(copy-readtable nil)` raised instead of answering standard syntax; see
    `coerce_to_readtable`.
    """
    global _standard_readtable
    if _standard_readtable is None:
        rt = Readtable()
        # Set last: `__init__` installs the standard macro characters through
        # `set_macro_character`, which the immutability guard would reject.
        rt._standard = True
        _standard_readtable = rt
    return _standard_readtable


# `*READTABLE*` is interned once; caching it keeps `get_current_readtable`
# cheap enough for the per-symbol calls the printer makes.
_readtable_sym = None


def _readtable_symbol():
    global _readtable_sym
    if _readtable_sym is None:
        from . import lisptype
        _readtable_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*READTABLE*')
    return _readtable_sym


def get_current_readtable() -> Readtable:
    """The current readtable: the value of `*READTABLE*`, its one home.

    This used to read a module-global `_current_readtable` while `*READTABLE*`
    was a separate variable that nothing consulted, so
    `(let ((*readtable* rt)) (read ...))` bound the variable and then read with
    the old table anyway. That is the same defect the printer's control
    variables had (plan.md C7): a control variable not connected to the
    mechanism it names. Every reader entry point already funnels through this
    function, so giving the variable one home fixes all of them at once.
    """
    from . import lisptype
    sym = _readtable_symbol()
    rt = getattr(sym, 'value', None)
    if isinstance(rt, Readtable):
        return rt
    if rt is None or rt is lisptype.NIL or isinstance(rt, lisptype.lispNull):
        # Not yet initialized (bootstrap, or a fresh environment). The current
        # readtable starts as a *copy* of the standard one so that mutating it
        # cannot corrupt standard syntax.
        rt = standard_readtable().copy()
        sym.value = rt
        return rt
    raise lisptype.LispTypeError(
        f"*READTABLE* is bound to {type(rt).__name__}, which is not a readtable",
        expected_type="READTABLE", actual_value=rt)


def set_current_readtable(readtable: Readtable):
    """Set the current readtable by assigning `*READTABLE*`."""
    _readtable_symbol().value = readtable


def coerce_to_readtable(designator, what: str, default=None) -> Readtable:
    """Resolve a **readtable designator** (CLHS glossary).

    NIL denotes the **standard** readtable -- not the current one, and not an
    error. A readtable denotes itself. `default` is the readtable an *omitted*
    argument denotes, which for every operator that takes one is the current
    readtable.

    This is the one resolver. Eight operators in `io_read.py` each carried
    their own `if readtable is None: readtable = get_current_readtable()`,
    which handled an omitted argument and nothing else -- so every one of them
    broke on exactly the NIL the designator rule exists for, and
    `(copy-readtable nil)` raised `'lispNull' object has no attribute 'copy'`
    as the value of the form (standing rule 2).
    """
    from . import lisptype
    if designator is _OMITTED:
        return default if default is not None else get_current_readtable()
    if isinstance(designator, Readtable):
        return designator
    if designator is None or designator is lisptype.NIL or isinstance(
            designator, lisptype.lispNull):
        return standard_readtable()
    raise lisptype.LispTypeError(
        f"{what}: {type(designator).__name__} is not a readtable designator",
        expected_type="READTABLE", actual_value=designator)


class _Omitted:
    """Marks an argument that was not supplied at all.

    Needed because NIL is a *meaningful* readtable designator here, so the
    usual `=None` default cannot tell "omitted" (the current readtable) from
    "given NIL" (the standard readtable).
    """

    def __repr__(self):
        return '<omitted>'


_OMITTED = _Omitted()


def case_keyword(case_name: str):
    """The `READTABLE-CASE` keyword for an internal case name."""
    from . import lisptype
    return lisptype.intern_keyword(case_name)


def case_from_designator(value, what: str) -> str:
    """The internal case name a `(setf readtable-case)` value names."""
    from . import lisptype
    name = getattr(value, 'name', None)
    if name is None and isinstance(value, str):
        name = value
    if name is not None and name.upper() in READTABLE_CASES:
        return name.upper()
    raise lisptype.LispTypeError(
        f"{what}: {value!r} is not one of :UPCASE :DOWNCASE :PRESERVE :INVERT",
        expected_type="(MEMBER :UPCASE :DOWNCASE :PRESERVE :INVERT)",
        actual_value=value)

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
