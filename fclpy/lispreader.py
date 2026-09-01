

import sys
import re as _re
import fclpy.lisptype as lisptype
import fclpy.numtoken as numtoken


#: The reader control variables of CLHS Figure 23-1, with their ANSI initial
#: values -- the one table the bootstrap builds them from, so that a variable
#: cannot be *proclaimed* special (`lispenv.STANDARD_SPECIAL_VARIABLES`) and
#: then left with no value, which is how all four of these came to be
#: **unbound**: `(boundp '*read-base*)` was NIL, and `*read-eval*` signalled
#: UNBOUND-VARIABLE. `*READTABLE*` is deliberately absent -- its initial value
#: is an object built during bootstrap, and `readtable.py` owns it.
#:
#: A value of `True`/`False` here means the Lisp T/NIL, and a `str` means the
#: interned COMMON-LISP symbol of that name (`*READ-DEFAULT-FLOAT-FORMAT*`
#: holds the *type name* SINGLE-FLOAT, not a keyword).
READER_VARIABLES = {
    '*READ-BASE*': 10,
    '*READ-DEFAULT-FLOAT-FORMAT*': 'SINGLE-FLOAT',
    '*READ-EVAL*': True,
    '*READ-SUPPRESS*': False,
}


def _reader_variable_value(name: str):
    """The value of one of CLHS Figure 23-1's reader control variables.

    Resolution order matches `printer.resolve_control`'s and, through it,
    `evaluation_core.eval`'s order for a variable reference: a binding in the
    current environment chain first, then the symbol's value cell, then the
    ANSI initial value. Falling back to the initial value rather than to the
    evaluator's next step matters for the same reason it does in the printer:
    that next step is the *function* registry, and these names were once
    registered there as `cl_function`s (plan.md C7), so a reference resolved
    to a Python function object.
    """
    import fclpy.state as state

    symbol = lisptype.COMMON_LISP_PACKAGE.intern_symbol(name)
    env = getattr(state, 'current_environment', None)
    if env is not None and env.has_variable(symbol):
        return env.find_variable(symbol)
    return getattr(symbol, 'value', None)


def resolve_read_base():
    """The current input radix -- the one place `*READ-BASE*` is read."""
    value = _reader_variable_value('*READ-BASE*')
    if value is None:
        return READER_VARIABLES['*READ-BASE*']
    try:
        return numtoken.check_radix(value, '*READ-BASE*')
    except numtoken.NumericTokenError as exc:
        raise lisptype.LispTypeError(str(exc))


def _is_nil(value):
    return value is None or value is lisptype.NIL or (
        isinstance(value, lisptype.LispSymbol) and value.name == 'NIL')


def resolve_read_suppress():
    """Whether `*READ-SUPPRESS*` is true (CLHS 23.1.2).

    One resolver, like `resolve_read_base`: when it is true every reader macro
    must consume its syntactic input and return NIL without constructing the
    object (or without signalling the errors construction would signal, beyond
    what consumption itself requires). It lives here beside the other reader
    control variables because the macro-character functions in `readtable.py`
    and the token constructors in this module must ask the *same* question --
    two answers is how suppressed reads would still intern symbols.
    """
    return not _is_nil(_reader_variable_value('*READ-SUPPRESS*'))


def resolve_read_eval():
    """Whether `*READ-EVAL*` is true (CLHS 23.1.2) -- the `#.` gate."""
    return not _is_nil(_reader_variable_value('*READ-EVAL*'))


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
        The rule itself lives once, in `readtable.convert_case_chars`, which
        the readtable's own token path uses too -- two copies of it is how
        `:PRESERVE` could work in one path and not the other.
        """
        from . import readtable as _rt
        return _rt.convert_case_chars(chars, escaped, self._readtable_case())

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
        # The `#n=` label table is per *form*, not per readtable or per
        # session: `#1=` inside one READ must not resolve `#1#` inside the
        # next. read_1 is the one entry point every Lisp-level read funnels
        # through (READ, READ-FROM-STRING, READ-DELIMITED-LIST, LOAD's
        # per-form loop), so the frame lives and dies with this call.
        _rt.label_frame_push(preserve_whitespace)
        try:
            return self._read_1_body(preserve_whitespace)
        finally:
            _rt.label_frame_pop()

    def _read_1_body(self, preserve_whitespace=False):
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
                continue
            result = self._read_given_syntax(x, syntax, preserve_whitespace)
            # If macro returns None (e.g., comments), continue reading
            if result is None:
                toss = True
            else:
                return self._check_result(result)

    def _read_given_syntax(self, x, syntax, preserve_whitespace=False):
        """CLHS 2.2's dispatch on one *already consumed* character's syntax
        type -- the whole of step 6-10 for that character: macro dispatch,
        single escape, multiple escape, or plain token accumulation.

        `_read_1_body` is the top-level caller (whitespace skipping and the
        "no object, keep reading" loop live there); `readtable._read_item`
        is the nested one, reading list elements and quote operands through
        the same dispatch rather than a second copy of it. Unlike
        `_read_1_body` this returns `readtable.DOT_MARKER` unchecked -- the
        list reader consumes it as the dotted-pair dot -- and never skips a
        `None` result itself.
        """
        from . import readtable as _rt
        if syntax in (_rt.SYNTAX_TERMINATING_MACRO,
                      _rt.SYNTAX_NON_TERMINATING_MACRO):
            # The reader calls the *internal* (char, stream) convention:
            # a user macro function arrives adapted, unlike the
            # user-facing function GET-MACRO-CHARACTER returns.
            macro_func = (self.readtable.macro_char_callable(x)
                          if self.readtable is not None
                          else self.get_macro_character(x))
            # get_macro_character returns (function, non_terminating_p)
            if isinstance(macro_func, tuple):
                macro_func = macro_func[0]
            return macro_func(x, self.stream)
        if syntax == _rt.SYNTAX_SINGLE_ESCAPE:
            y = self.stream.read_char()
            if y is None or self.stream.eof():
                # A single escape (`\`) with nothing after it is a
                # truncated token, not a malformed one -- the same
                # unconditional END-OF-FILE readtable.py's own
                # mid-form handlers now raise (io_read.py's
                # `_read_via_reader` converts either into the real
                # condition).
                raise EOFError("EOF after single escape")
            return self.read_8([y], [True], preserve_whitespace)
        if syntax == _rt.SYNTAX_MULTIPLE_ESCAPE:
            return self.read_9([], [], preserve_whitespace)
        self._check_constituent_valid(x)
        return self.read_8([x], [False], preserve_whitespace)

    def _check_result(self, result):
        """A lone unescaped dot token is not an object (CLHS 2.3.3).

        The token constructors answer `readtable.DOT_MARKER` for one -- the
        marker is what the list reader consumes as the dotted-pair dot -- and
        anywhere *outside* a list it is a reader error, at top level or as an
        object of any other construct.
        """
        from . import readtable as _rt
        if result is _rt.DOT_MARKER:
            raise ReaderErrorSignal(
                "the single dot token is valid only as the dot in a dotted list")
        return result

    def read_8(self, chars, escaped=None, preserve_whitespace=False, saw_escape=False):
        """CLHS 2.2 step 8: accumulate an unescaped token.

        `chars`/`escaped` are parallel lists -- the token's characters and
        whether each was escaped -- because `readtable-case` applies only to
        the unescaped ones and that cannot be recovered from a finished
        string. `saw_escape` says whether any escape *syntax* was used, even
        an empty `||` that adds no characters but still makes a token of dots
        an ordinary symbol (CLHS 2.3.3). A `str` is still accepted for
        `chars` so an external caller passing a partial token keeps working.
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
                chars.append(escaped_char)
                escaped.append(True)
                saw_escape = True
            elif syntax == _rt.SYNTAX_MULTIPLE_ESCAPE:
                return self.read_9(chars, escaped, preserve_whitespace,
                                   saw_escape)
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
        return self.read_10(chars, escaped, saw_escape)

    def read_9(self, chars, escaped=None, preserve_whitespace=False,
               saw_escape=False):
        """CLHS 2.2 step 9: accumulate inside a multiple-escape (`|...|`).

        Every character is taken as-is -- escaped, so `readtable-case` leaves
        it alone -- until the matching multiple-escape character. This used to
        be a *string literal* reader keyed on `"`, which is not what step 9
        is: `"` is a terminating macro character whose function reads a
        string, while `|` is the multiple-escape character. Nothing reached
        this function for `"` (the macro table wins first), so `|abc|` fell
        through to the plain-token path and read as a symbol *named* `|ABC|`,
        pipes and all. Entering the multiple escape sets `saw_escape` even if
        it encloses nothing (`||`).
        """
        from . import readtable as _rt
        if isinstance(chars, str):
            chars = list(chars)
            escaped = [False] * len(chars) if escaped is None else escaped
        if escaped is None:
            escaped = [False] * len(chars)
        saw_escape = True
        while True:
            c = self.stream.read_char()
            if c is None:
                raise EOFError("EOF inside multiple escape")
            syntax = self._syntax_type(c)
            if syntax == _rt.SYNTAX_MULTIPLE_ESCAPE:
                return self.read_8(chars, escaped, preserve_whitespace,
                                   saw_escape)
            if syntax == _rt.SYNTAX_SINGLE_ESCAPE:
                c = self.stream.read_char()
                if c is None:
                    raise EOFError("EOF after single escape inside multiple escape")
            chars.append(c)
            escaped.append(True)

    def read_10(self, chars, escaped=None, saw_escape=False):
        """CLHS 2.2 step 10: the accumulated token becomes a number or a symbol.

        `chars` and `escaped` are parallel lists -- the token's characters and
        whether each was escaped -- because three decisions below are
        per-character: `readtable-case` applies only to the unescaped ones
        (applied by `read_8` before this point), an escaped character is never
        a package marker (CLHS 2.4.5), and a token of unescaped dots is not an
        object (CLHS 2.3.3). The `\\x00` placeholder this used to substitute
        for an escaped colon could not tell `\\:` from a *literal NUL
        character* -- which `syntax.escaped.2` reads -- so the colon analysis
        now runs on the real characters with the escape flags beside them. A
        `str` is still accepted for `chars` so an external caller passing a
        partial token keeps working (then nothing was escaped).

        The three hardcoded regexes that used to be inlined here are gone --
        `numtoken` is the one place CLHS 2.3.1 is applied, shared with the
        `#B`/`#O`/`#X`/`#nR` readers, which had their own partial copy.
        """
        from . import readtable as _rt
        if isinstance(chars, str):
            chars = list(chars)
            escaped = [False] * len(chars) if escaped is None else escaped
        if escaped is None:
            escaped = [False] * len(chars)

        # `*READ-SUPPRESS*` (CLHS 23.1.2): consume the token, construct
        # nothing. Checked before number parsing (a malformed number must not
        # signal under suppression), before the dot-token rule (suppressed
        # dots are consumed, not errors), and before interning -- a
        # suppressed symbol must not enter any package, which is why
        # `NONEXISTENT-PACKAGE::FOO` must not create a package here.
        if resolve_read_suppress():
            return lisptype.NIL

        # A token consisting only of *unescaped* dots: one dot is the
        # dotted-list marker -- `DOT_MARKER`, consumed by the list reader and
        # a reader error anywhere else -- and two or more are an error
        # wherever they appear (CLHS 2.3.3). Any escape syntax, even the
        # empty `||`, makes it an ordinary symbol instead (`\.` reads as
        # `|.|`).
        if chars and all(c == '.' for c in chars) and \
                not (saw_escape or any(escaped)):
            if len(chars) == 1:
                return _rt.DOT_MARKER
            raise ReaderErrorSignal(
                "a token consisting only of dots is not a valid object")

        # `readtable-case` applies per character, unescaped ones only; every
        # string below -- the package/symbol halves of a qualified name, the
        # number token, the name interned -- is cut from the converted
        # characters, while the colon analysis runs on the raw ones (case
        # conversion never moves a colon).
        converted = self._convert_case(chars, escaped)
        colons = [i for i, (c, e) in enumerate(zip(chars, escaped))
                  if c == ':' and not e]
        if colons and colons[0] == 0:
            # Keywords start with an unescaped ':' and are interned in the
            # KEYWORD package (self-evaluating).
            return lisptype.intern_keyword(''.join(converted[1:]),
                                           exact_case=True)
        if colons:
            return self._read_package_qualified_symbol(converted, colons)

        token = ''.join(converted)
        try:
            number = numtoken.parse_numeric_token(
                token, radix=resolve_read_base(),
                escaped=(saw_escape or any(escaped)))
        except numtoken.NumericTokenError as exc:
            raise ReaderErrorSignal(str(exc))
        if number is not None:
            return number

        # The current package is the value of `*PACKAGE*`; `state`'s resolver
        # is the one place that decides (see state.current_package_value).
        from . import state
        current_pkg = state.current_package_value()

        # Special-case the canonical Lisp booleans/empty-list: NIL and T
        # In Common Lisp, NIL is both the symbol and the empty list; the
        # reader should return the canonical NIL object rather than a
        # fresh symbol. Similarly, T should return the global T symbol.
        if token == 'NIL':
            return lisptype.NIL
        if token == 'T':
            return lisptype.T

        # First check if symbol exists in current package
        sym, status = current_pkg.find_symbol(token)
        if sym is not None:
            return sym

        # Check USE'd packages for exported symbols
        for used_pkg in getattr(current_pkg, 'use_packages', []):
            # Handle both Package objects and package names
            if isinstance(used_pkg, str):
                used_pkg = lisptype.find_package(used_pkg)
            if used_pkg is not None:
                # Only look for external symbols in USE'd packages
                if token in getattr(used_pkg, 'external_symbols', set()):
                    sym = used_pkg.symbols.get(token)
                    if sym is not None:
                        return sym

        # Not found - intern in current package
        from .readtable import intern_token_symbol
        return intern_token_symbol(token, current_pkg)

    def _read_package_qualified_symbol(self, chars, colons):
        """Read a package-qualified symbol like PKG:SYM or PKG::SYM.

        `chars` is the token's characters (already case-converted per
        `readtable-case`, see `read_10`) and `colons` the indices of its
        *unescaped* colons; `::` is internal access, `:` external. Neither
        half is re-cased here.
        """
        first = colons[0]
        internal = len(colons) > 1 and colons[1] == first + 1
        pkg_name = ''.join(chars[:first])
        sym_name = ''.join(chars[first + (2 if internal else 1):])

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

    

