

import sys
import re as _re
import fclpy.lisptype as lisptype

class LispStream():
    def __init__(self, fh):
        self.fh = fh
        self.tokens = []
        self.buff = []
        self._eof = False
    def unread_char(self, y):
        if y:  # Don't unread EOF
            self.buff.append(y)
    def push_token(self, token):
        self.tokens.append(token)
    def has_token(self,token):
        return token in self.tokens
    def pop_token(self):
        return self.tokens.pop()
    def read_char(self):
        if len(self.buff) > 0:
            return self.buff.pop()
        char = self.fh.read(1)
        if char == '':
            self._eof = True
            return None
        return char
    def eof(self):
        return self._eof

STDIN = LispStream(sys.stdin)

class LispReader():
    
    def __init__(self, get_macro_character, stream = STDIN):
        self.stream = stream
        self.get_macro_character = get_macro_character
    
    def read_1(self):
        toss = True
        while(toss):
            toss = False
            x = self.stream.read_char()
            if x is None or self.stream.eof():
                return None
            elif (not self.valid_char(x)):
                raise Exception("reader-error")
            elif self.whitespace_char(x):
                toss = True
            elif self.macro_character(x):
                macro_func = self.get_macro_character(x)
                # get_macro_character returns (function, non_terminating_p)
                if isinstance(macro_func, tuple):
                    macro_func = macro_func[0]
                result = macro_func(x, self.stream)
                # If macro returns None (e.g., comments), continue reading
                if result is None:
                    toss = True
                else:
                    return result
            elif self.single_escape_character(x):
                y = self.stream.read_char()
                if y is None or self.stream.eof():
                    raise Exception("reader-error")
                # Use placeholder for escaped colons
                if y == ':':
                    return self.read_8('\x00')  # Placeholder for escaped colon
                else:
                    return self.read_8(y.upper())
            elif self.multiple_escape_character(x):
                return self.read_9("")
            else:
                return self.read_8(x.upper())
    def read_8(self, token):
        more = True
        while(more):
            y = self.stream.read_char()
            if y is None:
                more = False
            elif self.single_escape_character(y):
                # Handle backslash escape within token
                escaped = self.stream.read_char()
                if escaped is None:
                    raise Exception("reader-error: EOF after escape")
                # Use placeholder for escaped colons
                if escaped == ':':
                    token = token + '\x00'
                else:
                    token = token + escaped.upper()
            elif self.terminating_macro_character(y):
                self.stream.unread_char(y)
                more = False
            elif self.whitespace_char(y):
                more = False
            else:
                token = token + y.upper()
        return self.read_10(token)
    
    def read_9(self, token):
        """Read a string literal (between double quotes)."""
        while True:
            c = self.stream.read_char()
            if not c:
                raise Exception("Unexpected EOF in string")
            elif c == '"':
                break
            elif c == '\\':
                # Handle escape sequences
                next_c = self.stream.read_char()
                if not next_c:
                    raise Exception("Unexpected EOF after escape")
                # Simple escape handling
                if next_c == 'n':
                    token += '\n'
                elif next_c == 't':
                    token += '\t'
                elif next_c == 'r':
                    token += '\r'
                elif next_c == '\\':
                    token += '\\'
                elif next_c == '"':
                    token += '"'
                else:
                    token += next_c
            else:
                token += c
        return token
    
    
    def read_10(self, token):
        # Try to parse as integer
        if _re.match(r"^[+-]?\d+$", token):
            return int(token)
        # Try to parse as float (including exponent markers D, E, F, S, L)
        # Patterns: 1.5, 1.5E10, 1.5D2, 1E10, 1D2, etc.
        float_pattern = r"^[+-]?(\d+\.?\d*|\d*\.\d+)([DEFSLdefsl][+-]?\d+)?$"
        if _re.match(float_pattern, token):
            # Normalize exponent markers (D, F, S, L) to E for Python
            normalized = token.upper()
            for marker in 'DFSL':
                normalized = normalized.replace(marker, 'E')
            return float(normalized)
        # Otherwise it's a symbol
        # Keywords start with ':' and should be interned in KEYWORD package
        if token.startswith(":"):
            # strip leading ':' and return an interned keyword (keywords are self-evaluating)
            name = token[1:]
            return lisptype.intern_keyword(name.upper())
        
        # Handle package-qualified symbols (PKG:SYM or PKG::SYM)
        # Only treat as package-qualified if contains a real colon (not escaped placeholder \x00)
        # Create a temporary version without placeholders to check
        token_check = token.replace('\x00', '')
        if ':' in token_check and not token.startswith(':'):
            return self._read_package_qualified_symbol(token)
        
        # Get current package from state
        from . import state
        current_pkg = getattr(state, 'current_package', None)
        if current_pkg is None:
            current_pkg = lisptype.COMMON_LISP_USER_PACKAGE
        
        # Restore escaped colons in the token before interning
        token_restored = token.replace('\x00', ':')
        name_upper = token_restored.upper()

        # Special-case the canonical Lisp booleans/empty-list: NIL and T
        # In Common Lisp, NIL is both the symbol and the empty list; the
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
        
        # Not found - intern in current package (with restored colons)
        return current_pkg.intern_symbol(token_restored)
    
    def _read_package_qualified_symbol(self, token):
        """Read a package-qualified symbol like PKG:SYM or PKG::SYM."""
        # Restore escaped colons (\x00 placeholder) before processing
        # but only after determining if this is package-qualified
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
    
    def valid_char(self,c):
        return c is not None
    
    def whitespace_char(self,c):
        # Include form feed (\x0c) and vertical tab (\x0b) as whitespace per CL standard
        return c is not None and c in [" ","\t","\n","\r","\x0c","\x0b"]
       
    def eof(self,c):
        return c != c
    def macro_character(self,c ):
        # Consult the supplied readtable macro-character resolver if available
        try:
            mc = self.get_macro_character(c)
            return mc is not None
        except Exception:
            # Fallback to a conservative built-in set for safety
            return c in ["(",")","'",";"]
    def terminating_macro_character(self,c):
        # A macro character is terminating if the readtable marks it as terminating
        try:
            mc = self.get_macro_character(c)
            if mc is None:
                return False
            # mc may be a tuple (function, non_terminating_p)
            if isinstance(mc, tuple) and len(mc) > 1:
                non_terminating = bool(mc[1])
            else:
                # Default: treat as terminating
                non_terminating = False
            return not non_terminating
        except Exception:
            return c in [")"]
    
    def non_terminating_macro_character(self,c):
        return c != c
    def single_escape_character(self,c):
        return c == "\\"
    def multiple_escape_character(self,c):
        return c == "\""
    

