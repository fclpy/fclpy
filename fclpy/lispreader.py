

import sys
import re as _re
import fclpy.lisptype as lisptype


class ReaderErrorSignal(Exception):
    """Malformed input, raised from inside the reader's token loop.

    Deliberately a plain Python marker rather than `lisptype.ReaderError`:
    the inner loop does not know the outer Lisp stream (it reads through a
    bridge), and READER-ERROR carries a STREAM slot that ansi-test reads back
    with `stream-error-stream`. Every reader entry point converts this into a
    real `lisptype.ReaderError` attached to the actual stream -- the same
    convention the existing `EOFError` sites already use for end-of-file.
    """


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

    def __init__(self, readtable, stream = STDIN):
        """`readtable` is the `readtable.Readtable` whose syntax to read with.

        It used to be only that table's `get_macro_character` *method*, which
        is why every other syntax decision in here was a hardcoded literal --
        the reader simply had no way to ask. A bare callable is still accepted
        (many tests pass `rt.get_macro_character`) and then standard syntax
        types apply, which is what those callers mean anyway.
        """
        self.stream = stream
        if callable(readtable) and not hasattr(readtable, 'syntax_type'):
            self.readtable = None
            self.get_macro_character = readtable
        else:
            self.readtable = readtable
            self.get_macro_character = readtable.get_macro_character

    # --- CLHS 2.1.4 syntax types, asked of the readtable ---
    #
    # One resolver per question, each deferring to the readtable so
    # SET-SYNTAX-FROM-CHAR is observable here. When no readtable was supplied
    # these fall back to `readtable`'s module-level standard tables rather than
    # to a private copy of them (standing rule 3).

    def _syntax_type(self, c):
        from . import readtable as _rt
        if self.readtable is not None:
            return self.readtable.syntax_type(c)
        mc = None
        try:
            mc = self.get_macro_character(c)
        except Exception:
            mc = None
        if mc is not None:
            non_terminating = bool(mc[1]) if isinstance(mc, tuple) and len(mc) > 1 else False
            return (_rt.SYNTAX_NON_TERMINATING_MACRO if non_terminating
                    else _rt.SYNTAX_TERMINATING_MACRO)
        standard = _rt.STANDARD_SYNTAX_TYPES.get(c)
        if standard is not None:
            return standard
        if c in _rt.STANDARD_WHITESPACE:
            return _rt.SYNTAX_WHITESPACE
        return _rt.SYNTAX_CONSTITUENT

    def _readtable_case(self):
        if self.readtable is not None:
            return self.readtable.readtable_case()
        return 'UPCASE'

    def _convert_case(self, chars, escaped):
        """Apply `readtable-case` to the unescaped characters of a token.

        CLHS 23.1.2: an escaped character "is not affected by the readtable
        case", so the conversion is per-character and driven by `escaped`.
        This whole function is what the reader was missing: it upcased every
        character with a bare `.upper()`, so `:PRESERVE`/`:DOWNCASE`/`:INVERT`
        had no effect on reading at all and an escaped `|abc|` came out "ABC".
        """
        from . import readtable as _rt
        case = self._readtable_case()
        if case == 'PRESERVE':
            return ''.join(chars)
        if case == 'UPCASE':
            return ''.join(c.upper() if not e else c
                           for c, e in zip(chars, escaped))
        if case == 'DOWNCASE':
            return ''.join(c.lower() if not e else c
                           for c, e in zip(chars, escaped))
        # :INVERT -- if the unescaped cased characters are all the same case,
        # invert them; a mixed-case token is left alone (CLHS 23.1.2).
        unescaped = [c for c, e in zip(chars, escaped) if not e and c.isalpha()]
        has_upper = any(c.isupper() for c in unescaped)
        has_lower = any(c.islower() for c in unescaped)
        if has_upper and has_lower:
            return ''.join(chars)
        if has_upper:
            return ''.join(c.lower() if not e else c
                           for c, e in zip(chars, escaped))
        return ''.join(c.upper() if not e else c
                       for c, e in zip(chars, escaped))

    def _check_constituent_valid(self, c):
        """A constituent whose constituent trait is *invalid* is a reader
        error (CLHS 2.1.4.2).

        In the standard readtable this fires only for Backspace and Rubout;
        it becomes reachable for Tab/Newline/Space/... exactly when
        SET-SYNTAX-FROM-CHAR turns one of them into a constituent, which is
        what `set-syntax-from-char.lsp`'s invalid-trait tests check.
        """
        from . import readtable as _rt
        if _rt.constituent_trait(c) == _rt.CONSTITUENT_TRAIT_INVALID:
            raise ReaderErrorSignal(
                f"{c!r} has the invalid constituent trait (CLHS 2.1.4.2)")

    def read_1(self, preserve_whitespace=False):
        from . import readtable as _rt
        toss = True
        while(toss):
            toss = False
            x = self.stream.read_char()
            if x is None or self.stream.eof():
                return None
            syntax = self._syntax_type(x)
            if syntax == _rt.SYNTAX_WHITESPACE:
                toss = True
            elif syntax in (_rt.SYNTAX_TERMINATING_MACRO,
                            _rt.SYNTAX_NON_TERMINATING_MACRO):
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
            elif syntax == _rt.SYNTAX_SINGLE_ESCAPE:
                y = self.stream.read_char()
                if y is None or self.stream.eof():
                    # A single escape (`\`) with nothing after it is a
                    # truncated token, not a malformed one -- the same
                    # unconditional END-OF-FILE readtable.py's own
                    # mid-form handlers now raise (io_read.py's
                    # `_read_via_reader` converts either into the real
                    # condition).
                    raise EOFError("EOF after single escape")
                return self.read_8(['\x00' if y == ':' else y], [True],
                                   preserve_whitespace)
            elif syntax == _rt.SYNTAX_MULTIPLE_ESCAPE:
                return self.read_9([], [], preserve_whitespace)
            else:
                self._check_constituent_valid(x)
                return self.read_8([x], [False], preserve_whitespace)

    def read_8(self, chars, escaped=None, preserve_whitespace=False):
        """CLHS 2.2 step 8: accumulate an unescaped token.

        `chars`/`escaped` are parallel lists -- the token's characters and
        whether each was escaped -- because `readtable-case` applies only to
        the unescaped ones and that cannot be recovered from a finished
        string. A `str` is still accepted for `chars` so an external caller
        passing a partial token keeps working.
        """
        from . import readtable as _rt
        if isinstance(chars, str):
            chars = list(chars)
            escaped = [False] * len(chars) if escaped is None else escaped
        if escaped is None:
            escaped = [False] * len(chars)
        more = True
        while(more):
            y = self.stream.read_char()
            if y is None:
                more = False
                continue
            syntax = self._syntax_type(y)
            if syntax == _rt.SYNTAX_SINGLE_ESCAPE:
                escaped_char = self.stream.read_char()
                if escaped_char is None:
                    raise EOFError("EOF after single escape in token")
                chars.append('\x00' if escaped_char == ':' else escaped_char)
                escaped.append(True)
            elif syntax == _rt.SYNTAX_MULTIPLE_ESCAPE:
                return self.read_9(chars, escaped, preserve_whitespace)
            elif syntax == _rt.SYNTAX_TERMINATING_MACRO:
                self.stream.unread_char(y)
                more = False
            elif syntax == _rt.SYNTAX_WHITESPACE:
                # CLHS 23.1.2: ordinary READ consumes the single whitespace
                # character that terminates a token; READ-PRESERVING-
                # WHITESPACE must not, so the character stays available for
                # whatever reads the stream next (READ-PRESERVING-
                # WHITESPACE.16/READ-FROM-STRING's :PRESERVE-WHITESPACE).
                if preserve_whitespace:
                    self.stream.unread_char(y)
                more = False
            else:
                # A constituent, or a *non-terminating* macro character --
                # CLHS 2.2 step 8 accumulates the latter into the token
                # rather than dispatching it, which is why `a#b` is one
                # symbol.
                self._check_constituent_valid(y)
                chars.append(y)
                escaped.append(False)
        return self.read_10(self._convert_case(chars, escaped))

    def read_9(self, chars, escaped=None, preserve_whitespace=False):
        """CLHS 2.2 step 9: accumulate inside a multiple-escape (`|...|`).

        Every character is taken as-is -- escaped, so `readtable-case` leaves
        it alone -- until the matching multiple-escape character. This used to
        be a *string literal* reader keyed on `"`, which is not what step 9
        is: `"` is a terminating macro character whose function reads a
        string, while `|` is the multiple-escape character. Nothing reached
        this function for `"` (the macro table wins first), so `|abc|` fell
        through to the plain-token path and read as a symbol *named* `|ABC|`,
        pipes and all.
        """
        from . import readtable as _rt
        if isinstance(chars, str):
            chars = list(chars)
            escaped = [False] * len(chars) if escaped is None else escaped
        if escaped is None:
            escaped = [False] * len(chars)
        while True:
            c = self.stream.read_char()
            if c is None:
                raise EOFError("EOF inside multiple escape")
            syntax = self._syntax_type(c)
            if syntax == _rt.SYNTAX_MULTIPLE_ESCAPE:
                return self.read_8(chars, escaped, preserve_whitespace)
            if syntax == _rt.SYNTAX_SINGLE_ESCAPE:
                c = self.stream.read_char()
                if c is None:
                    raise EOFError("EOF after single escape inside multiple escape")
            chars.append('\x00' if c == ':' else c)
            escaped.append(True)


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
        # Otherwise it's a symbol.
        #
        # `token` arrives already case-converted per `readtable-case`, with
        # escaped characters left verbatim (see `_convert_case`). Nothing below
        # may re-case it: the `.upper()` calls that used to be here made every
        # symbol name upper case regardless of the readtable, which is the
        # other half of why `:PRESERVE` had no observable effect.
        # Keywords start with ':' and should be interned in KEYWORD package
        if token.startswith(":"):
            # strip leading ':' and return an interned keyword (keywords are self-evaluating)
            name = token[1:]
            return lisptype.intern_keyword(name.replace('\x00', ':'),
                                           exact_case=True)

        # Handle package-qualified symbols (PKG:SYM or PKG::SYM)
        # Only treat as package-qualified if contains a real colon (not escaped placeholder \x00)
        # Create a temporary version without placeholders to check
        token_check = token.replace('\x00', '')
        if ':' in token_check and not token.startswith(':'):
            return self._read_package_qualified_symbol(token)

        # The current package is the value of `*PACKAGE*`; `state`'s resolver
        # is the one place that decides (see state.current_package_value).
        from . import state
        current_pkg = state.current_package_value()

        # Restore escaped colons in the token before interning
        name = token.replace('\x00', ':')

        # Special-case the canonical Lisp booleans/empty-list: NIL and T
        # In Common Lisp, NIL is both the symbol and the empty list; the
        # reader should return the canonical NIL object rather than a
        # fresh symbol. Similarly, T should return the global T symbol.
        if name == 'NIL':
            return lisptype.NIL
        if name == 'T':
            return lisptype.T

        # First check if symbol exists in current package
        sym, status = current_pkg.find_symbol(name)
        if sym is not None:
            return sym

        # Check USE'd packages for exported symbols
        for used_pkg in getattr(current_pkg, 'use_packages', []):
            # Handle both Package objects and package names
            if isinstance(used_pkg, str):
                used_pkg = lisptype.find_package(used_pkg)
            if used_pkg is not None:
                # Only look for external symbols in USE'd packages
                if name in getattr(used_pkg, 'external_symbols', set()):
                    sym = used_pkg.symbols.get(name)
                    if sym is not None:
                        return sym

        # Not found - intern in current package (with restored colons)
        return current_pkg.intern_symbol(name, exact_case=True)
    
    def _read_package_qualified_symbol(self, token):
        """Read a package-qualified symbol like PKG:SYM or PKG::SYM.

        `token` is already case-converted (see `read_10`), so neither half is
        re-cased here.
        """
        separator = '::' if '::' in token else ':'
        parts = token.split(separator, 1)
        pkg_name = parts[0]
        sym_name = parts[1] if len(parts) > 1 else ''
        # Restore escaped colons in both halves
        pkg_name = pkg_name.replace('\x00', ':')
        sym_name = sym_name.replace('\x00', ':')

        # Find the package
        pkg = lisptype.find_package(pkg_name)
        if pkg is None:
            # Package not found - create it as a fallback
            pkg = lisptype.make_package(pkg_name)

        # Intern the symbol in that package
        return pkg.intern_symbol(sym_name, exact_case=True)
    
    # The seven hardcoded syntax-type predicates that used to live here --
    # `whitespace_char` (a literal list), `single_escape_character` (`c ==
    # "\\"`), `multiple_escape_character` (`c == '"'`, not even the right
    # character), `valid_char` (always True, so no character was ever
    # invalid), and the macro-character pair -- are gone. They were a second,
    # readtable-blind copy of CLHS 2.1.4, which is why SET-SYNTAX-FROM-CHAR
    # had nothing to act on; `_syntax_type` above is the one resolver and it
    # asks the readtable.

    

