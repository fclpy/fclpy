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

# --- Character syntax types (CLHS 2.1.4) ---
#
# Every character has exactly one *syntax type* in a given readtable, and the
# reader's algorithm (CLHS 2.2) is written entirely in terms of it. This is
# the model `SET-SYNTAX-FROM-CHAR` sets and `lispreader.LispReader` reads;
# before it existed each of those decisions was a hardcoded literal in the
# reader (`c in [" ","\t",...]` for whitespace, `c == "\\"` for single escape,
# `c == '"'` for *multiple* escape -- which is not even the right character,
# multiple escape is `|`), so there was nothing for SET-SYNTAX-FROM-CHAR to
# act on and it was a stub returning T.
SYNTAX_CONSTITUENT = 'constituent'
SYNTAX_WHITESPACE = 'whitespace'
SYNTAX_TERMINATING_MACRO = 'terminating-macro'
SYNTAX_NON_TERMINATING_MACRO = 'non-terminating-macro'
SYNTAX_SINGLE_ESCAPE = 'single-escape'
SYNTAX_MULTIPLE_ESCAPE = 'multiple-escape'

# CLHS 2.1.4.1's whitespace characters. Backspace and Rubout are *not* here:
# they are constituents whose constituent trait is invalid (below), which is a
# different thing and is what `set-syntax-from-char.lsp`'s invalid-trait tests
# distinguish.
STANDARD_WHITESPACE = frozenset(' \t\n\r\f\v')

# The standard syntax types of CLHS 2.1.4.1, for every character that is not a
# plain constituent. Macro characters live in `_macro_characters` (they need a
# function as well as a type), so this table carries only the two escapes --
# the standard readtable's macro characters are installed by
# `_setup_standard_macros` and `syntax_type` derives their type from that one
# place rather than duplicating the list here.
STANDARD_SYNTAX_TYPES = {
    '\\': SYNTAX_SINGLE_ESCAPE,
    '|': SYNTAX_MULTIPLE_ESCAPE,
}

# CLHS 2.1.4.2's *constituent traits*. These belong to the character itself,
# not to the readtable, and `SET-SYNTAX-FROM-CHAR` explicitly does **not**
# copy them: "the constituent traits of to-char are not affected". That is
# exactly what the trait tests measure -- `(set-syntax-from-char #\\ #\X)`
# makes `\` a constituent, and reading it then yields the symbol named "\"
# because `\`'s own trait is alphabetic; doing the same to Tab yields a
# READER-ERROR because Tab's trait is invalid.
CONSTITUENT_TRAIT_INVALID = 'invalid'
CONSTITUENT_TRAIT_ALPHABETIC = 'alphabetic'

# Backspace, Tab, Newline, Linefeed, Page, Return, Space and Rubout are the
# characters CLHS 2.1.4.2 gives the *invalid* constituent trait.
INVALID_CONSTITUENTS = frozenset('\b\t\n\x0b\x0c\r \x7f')

# What a token of one unescaped dot (CLHS 2.3.3) reads as. It is not an
# object: the list reader consumes it as the dotted-pair dot, and every other
# position -- top level, a vector element, a quoted form's tail -- must turn
# it into a reader error (`_check_result` in `lispreader.py`, `_read_item`
# below). It is deliberately not a Lisp object, so it cannot leak as a value;
# an *escaped* dot reads as the ordinary symbol `|.|` and never becomes this.
DOT_MARKER = object()

# Whether the token characters that follow were escaped, shared with
# `lispreader.read_10`'s per-character analysis. It is a module-level function
# rather than a method on either reader so the readtable's token path and the
# LispReader token path cannot grow second copies of the CLHS 23.1.2 rule.
def convert_case_chars(chars, escaped, case='UPCASE'):
    """Apply `readtable-case` to a token's characters (CLHS 23.1.2).

    An escaped character is used *as is* -- "is not affected by the readtable
    case" -- so the conversion is per-character, driven by the parallel
    `escaped` list. `:INVERT` inverts only when every unescaped cased
    character has the same case; a mixed-case token is left alone.
    """
    if case == 'PRESERVE':
        return ''.join(chars)
    if case == 'UPCASE':
        return ''.join(c.upper() if not e else c
                       for c, e in zip(chars, escaped))
    if case == 'DOWNCASE':
        return ''.join(c.lower() if not e else c
                       for c, e in zip(chars, escaped))
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


def read_suppressed():
    """Whether `*READ-SUPPRESS*` is true right now (CLHS 23.1.2).

    `lispreader.resolve_read_suppress` is the one resolver -- this is the
    readtable-side alias every macro-character function asks, so the macro
    path and the token path cannot disagree about the same dynamic variable.
    """
    from .lispreader import resolve_read_suppress
    return resolve_read_suppress()


# --- The per-READ frame stack ---
#
# Two pieces of read state must live for the extent of one *form*, not on the
# readtable object (the current readtable is shared by every read in the
# process):
#
# * the `#n=` label table -- `#1=` inside one READ must not resolve a `#1#`
#   inside the next (CLHS 2.4.8.5/.6);
# * whether this READ is `READ-PRESERVING-WHITESPACE` -- CLHS 2.2 step 8
#   says ordinary READ *consumes* the one whitespace character that
#   terminates a token and READ-PRESERVING-WHITESPACE must not, and that
#   applies to tokens read *inside* macro-character forms too, not only to
#   top-level tokens. The readtable's token path consults the frame for the
#   answer, the way it consults `*READTABLE*` itself.
#
# `read_1` -- the one entry point every Lisp-level read funnels through
# (READ, READ-FROM-STRING, READ-DELIMITED-LIST, LOAD's per-form loop) --
# pushes and pops. Direct `_read_item` calls outside any READ default to
# ordinary READ semantics.
_read_frames = []


def label_frame_push(preserve_whitespace=False):
    _read_frames.append({'labels': {}, 'preserve_ws': bool(preserve_whitespace)})


def label_frame_pop():
    if _read_frames:
        _read_frames.pop()


def current_label_frame():
    """The innermost open label table, or None outside any READ."""
    return _read_frames[-1]['labels'] if _read_frames else None


def preserving_whitespace():
    """Whether the innermost READ is READ-PRESERVING-WHITESPACE."""
    return bool(_read_frames[-1]['preserve_ws']) if _read_frames else False


class _LabelPlaceholder:
    """What `#n#` yields while its `#n=` is still being read.

    A unique Python object, patched out of the finished structure by identity
    the moment `#n=` completes (`_patch_label_placeholders`), so `#1=(17
    . #1#)` comes out an actual cycle and no placeholder survives as part of
    the value.
    """

    __slots__ = ('label',)

    def __init__(self, label):
        self.label = label

    def __repr__(self):
        return f'#<label-{self.label}-placeholder>'


def constituent_trait(char: str) -> str:
    """The constituent trait of `char` (CLHS 2.1.4.2).

    A property of the character, not of any readtable -- see the note above
    `CONSTITUENT_TRAIT_INVALID`.
    """
    if char in INVALID_CONSTITUENTS:
        return CONSTITUENT_TRAIT_INVALID
    return CONSTITUENT_TRAIT_ALPHABETIC


def _reader_error(message: str) -> Exception:
    """The exception to raise for malformed input -- **one place, one class.**

    Every one of these sites used to raise a bare `ValueError`, and a Python
    exception is not a condition: it matches no `handler-case` clause, so it
    surfaced as the *value* of the form
    (`#<ERROR Python error in function call: ValueError: Unknown # dispatch
    character: #<>`). That is the defect prompt.txt names outright, and here it
    also *hid* a printer bug: `randomly-check-readability` handles
    `reader-error` and reports the offending output, so a `ValueError` instead
    turned "the printer wrote something unreadable" into an unexplained crash
    in eight `print.backquote.random` tests.

    `ReaderErrorSignal` is the marker every reader entry point converts into a
    real READER-ERROR carrying the stream; imported lazily because this module
    keeps no top-level `lisptype`/`lispreader` import.
    """
    from .lispreader import ReaderErrorSignal
    return ReaderErrorSignal(message)


class _UserMacroCharacterFunction:
    """A Lisp-level SET-MACRO-CHARACTER function in the reader's calling
    convention.

    The reader dispatches a macro character with ``(char, stream)`` -- the
    convention of `Readtable`'s own built-in readers. A *user* macro function
    is specified as ``(stream char)`` (CLHS 23.2), so SET-MACRO-CHARACTER
    wraps the function it is given in one of these and the reader's call
    arrives with the arguments the standard names. `get_macro_character`
    unwraps, so the function GET-MACRO-CHARACTER returns is EQL to the one
    SET-MACRO-CHARACTER was given.

    The user function receives the **Lisp stream**, not the reader's internal
    bridge: whatever characters the reader has looked ahead past are pushed
    back onto the stream before the call, so `(read stream)` inside a macro
    function continues from the right character.
    """

    __slots__ = ('function',)

    def __init__(self, function):
        self.function = function

    def __call__(self, char, stream):
        from . import lisptype
        fn = _resolve_user_reader_function(self.function, stream,
                                           'SET-MACRO-CHARACTER function')
        return fn(lisp_stream_of(stream), lisptype.Character(char))


def lisp_stream_of(stream):
    """The Lisp-side stream designator behind the reader's internal bridge.

    `lispreader.LispStream` is a character bridge, not a Lisp stream; the
    stream it wraps (`streams.Stream`, via `_StreamFileAdapter`) is what a
    user macro-character function's `(read stream)` expects. A bridge whose
    source is a raw Python file object has no Lisp stream to offer and gets
    itself back -- user functions that ignore the stream (all of
    ansi-test's) are unaffected either way.
    """
    fh = getattr(stream, 'fh', None)
    target = getattr(fh, '_stream', None)
    if target is not None:
        # Characters the reader has already consumed into its pushback must
        # not be skipped by the user function's own reads.
        buff = getattr(stream, 'buff', None)
        if buff:
            while buff:
                target.unread_char(buff.pop())
        return target
    return stream


def _resolve_user_reader_function(function, stream, what):
    """A SET-MACRO-CHARACTER/SET-DISPATCH-MACRO-CHARACTER function object,
    resolved from a function designator (a symbol names its `fboundp`)."""
    from .lispfunc.evaluation_core import coerce_to_function
    try:
        return coerce_to_function(function, what)
    except Exception:
        # No environment available (bootstrap-time reading): the designator
        # cannot be resolved yet, so leave the callable as given.
        if callable(function):
            return function
        raise


def _as_internal_caller(function):
    """`function` in the reader's internal ``(char, stream)`` convention.

    A user function (anything that is not a `Readtable`'s own built-in
    reader) is stored behind the `(stream char)` adapter; an adapter or a
    built-in reader passes through unchanged. SET-MACRO-CHARACTER and
    SET-SYNTAX-FROM-CHAR's function copy both go through here, so the two
    ways a user function can enter a readtable's macro table cannot
    disagree about the calling convention.
    """
    if isinstance(function, _UserMacroCharacterFunction) or \
            isinstance(getattr(function, '__self__', None), Readtable):
        return function
    return _UserMacroCharacterFunction(function)


def intern_token_symbol(name, package, exact_case=True):
    """Intern a plain token symbol in `package` -- the one place both token
    paths (this module's `_read_symbol` and `lispreader.read_10`) agree on.

    The KEYWORD package's invariant -- every symbol in it is *external*
    (CLHS 11.1.2) -- is applied here, so reading a feature expression's
    un-colon'd symbol (`#-ecl`, `#-(or)`) or otherwise interning into
    KEYWORD yields an external keyword. These used to come out `:INTERNAL`,
    which is exactly what `keyword.2` and `do-external-symbols.5` walk the
    package to find.
    """
    from . import lisptype
    if package is getattr(lisptype, 'KEYWORD_PACKAGE', None):
        return lisptype.intern_keyword(name, exact_case=exact_case)
    return package.intern_symbol(name, exact_case=exact_case)


def _character_of(text):
    """The Lisp CHARACTER for a one-character reader token."""
    from . import lisptype
    return text if isinstance(text, lisptype.Character) else lisptype.Character(text)


class Readtable:
    """
    Centralized readtable for managing macro characters and reader macros.
    This replaces the scattered macro character implementations across multiple modules.
    """

    # Class-level default for the syntax-type overrides, so *every* Readtable
    # answers `syntax_type` correctly however it was constructed -- `copy()`
    # builds one through `__new__` and so do white-box tests. Never mutated in
    # place: `set_syntax_type` installs a per-instance dict first, so this
    # shared empty mapping stays empty.
    _syntax_types: Dict[str, str] = {}

    def __init__(self):
        self._macro_characters: Dict[str, Tuple[Callable, bool]] = {}
        self._dispatch_macro_characters: Dict[str, Dict[str, Callable]] = {}
        self._case = 'UPCASE'  # :UPCASE, :DOWNCASE, :PRESERVE, :INVERT
        # Per-character syntax-type *overrides* (CLHS 2.1.4). Only characters
        # whose type differs from `STANDARD_SYNTAX_TYPES`/the macro table are
        # recorded, so `syntax_type` stays the one resolver and there is no
        # second copy of the standard table to drift from it.
        self._syntax_types: Dict[str, str] = {}
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
        """Set up the standard Common Lisp macro characters.

        The second argument is `non_terminating_p`, and CLHS 2.1.4.1 fixes it
        exactly: `"`, `'`, `(`, `)`, `,`, `;` and `` ` `` are **terminating**
        macro characters and `#` is the only **non-terminating** one. Every
        entry here except `(` used to say the opposite, which nothing noticed
        because the flag had only one reader asking -- and that reader
        (`lispreader`) decided token termination from a hardcoded literal list
        instead. Now that `syntax_type` derives the syntax type from this
        table, the flag is load-bearing: a non-terminating macro character is
        *accumulated into a token* by CLHS 2.2 step 8, so `;` marked
        non-terminating made `(read-from-string "0;2")` answer the symbol
        `|0;2|` rather than 0.
        """
        # Standard terminating macro characters (CLHS 2.1.4.1)
        self.set_macro_character('(', self._left_paren_reader, False)
        self.set_macro_character(')', self._right_paren_reader, False)
        self.set_macro_character('"', self._string_reader, False)
        self.set_macro_character("'", self._quote_reader, False)
        self.set_macro_character(';', self._semicolon_reader, False)
        self.set_macro_character('`', self._backquote_reader, False)
        self.set_macro_character(',', self._comma_reader, False)

        # The one standard non-terminating macro character, and the dispatch
        # character (CLHS 2.4.8): `#` may appear inside a token, so `a#b` is
        # one symbol. `#` is also *the* standard dispatch macro character, so
        # its (empty) sub-character table exists from the start --
        # GET-DISPATCH-MACRO-CHARACTER answers NIL for its undefined
        # sub-characters rather than "not a dispatch macro character".
        self.set_macro_character('#', self._sharp_reader, True)
        self._dispatch_macro_characters['#'] = {}
        
    def get_macro_character(self, char: str) -> Optional[Tuple[Callable, bool]]:
        """
        Get the macro character function and terminating flag for a character.
        Returns (function, non_terminating_p) or None if not a macro character.

        The function returned is the one SET-MACRO-CHARACTER was given -- a
        user function, not the ``(char, stream)`` adapter this table stores
        for the reader -- so `(get-macro-character c)` returns something EQL
        to what was installed.
        """
        entry = self._macro_characters.get(char)
        if entry is None:
            return None
        function, non_terminating = entry
        if isinstance(function, _UserMacroCharacterFunction):
            function = function.function
        return (function, non_terminating)

    def macro_char_callable(self, char: str) -> Optional[Callable]:
        """The callable the *reader* should invoke: the internal
        ``(char, stream)`` convention, user functions adapted. This is
        `get_macro_character`'s unwrapped opposite and exists so the two
        callers -- the reader and the Lisp GET-MACRO-CHARACTER -- cannot
        disagree about which one sees the adapter."""
        entry = self._macro_characters.get(char)
        return entry[0] if entry is not None else None

    def set_macro_character(self, char: str, function: Callable, non_terminating_p: bool = False):
        """
        Set a macro character function.

        Args:
            char: The character to set as a macro character
            function: The reader function to call
            non_terminating_p: True if this is a non-terminating macro character
        """
        self._check_mutable('SET-MACRO-CHARACTER')
        function = _as_internal_caller(function)
        self._macro_characters[char] = (function, bool(non_terminating_p))
        # Becoming a macro character *is* a change of syntax type, so any
        # explicit non-macro override for this character no longer holds --
        # leaving it would make `syntax_type` and `get_macro_character`
        # disagree about the same character.
        self._syntax_types.pop(char, None)

    def syntax_type(self, char: str) -> str:
        """The syntax type of `char` in this readtable (CLHS 2.1.4).

        The one resolver, and the reason `SET-SYNTAX-FROM-CHAR` can work at
        all. Order matters: the macro table wins, because `set_macro_character`
        is how a character *becomes* a macro character and that table already
        records whether it terminates; then an explicit override set by
        `set_syntax_type`; then the standard table; then whitespace; then
        constituent, which is what the overwhelming majority of characters are.
        """
        macro = self._macro_characters.get(char)
        if macro is not None:
            return (SYNTAX_NON_TERMINATING_MACRO if macro[1]
                    else SYNTAX_TERMINATING_MACRO)
        if char in self._syntax_types:
            return self._syntax_types[char]
        standard = STANDARD_SYNTAX_TYPES.get(char)
        if standard is not None:
            return standard
        if char in STANDARD_WHITESPACE:
            return SYNTAX_WHITESPACE
        return SYNTAX_CONSTITUENT

    def set_syntax_type(self, char: str, syntax: str, function: Callable = None):
        """Give `char` the syntax type `syntax` in this readtable.

        `function` is required for the two macro types and ignored otherwise;
        `SET-SYNTAX-FROM-CHAR` supplies it when the character it copies from is
        a macro character, because CLHS says the macro function is copied along
        with the type.

        A character that stops being a macro character must be removed from the
        macro table, or `syntax_type` above would keep answering "macro" for it
        -- that is the whole reason both tables are read through one resolver.
        """
        self._check_mutable('SET-SYNTAX-FROM-CHAR')
        if syntax in (SYNTAX_TERMINATING_MACRO, SYNTAX_NON_TERMINATING_MACRO):
            if function is None:
                from . import lisptype
                raise lisptype.LispError(
                    "SET-SYNTAX-FROM-CHAR: a macro syntax type needs a macro function")
            self._syntax_types.pop(char, None)
            self._macro_characters[char] = (
                function, syntax == SYNTAX_NON_TERMINATING_MACRO)
            return
        self._macro_characters.pop(char, None)
        self._dispatch_macro_characters.pop(char, None)
        # Install a per-instance dict before writing, so the class-level
        # default above is never mutated into a table shared by every readtable.
        if '_syntax_types' not in self.__dict__:
            self._syntax_types = {}
        self._syntax_types[char] = syntax
    
    def get_dispatch_macro_character(self, dispatch_char: str, sub_char: str) -> Optional[Callable]:
        """Get a dispatch macro character function."""
        dispatch_table = self._dispatch_macro_characters.get(dispatch_char)
        if dispatch_table:
            return dispatch_table.get(sub_char)
        return None

    def has_dispatch_table(self, dispatch_char: str) -> bool:
        """Whether `dispatch_char` is a dispatch macro character here --
        `GET-DISPATCH-MACRO-CHARACTER`'s error condition (CLHS 23.2)."""
        return dispatch_char in self._dispatch_macro_characters

    def make_dispatch_macro_character(self, char: str,
                                      non_terminating_p: bool = False):
        """MAKE-DISPATCH-MACRO-CHARACTER (CLHS 23.2): make `char` into a
        dispatch macro character with an empty sub-character table.

        This is the same shape `#` has: reading `char`, an optional decimal
        integer, and a sub-character dispatches into the table; a
        sub-character with no function is a READER-ERROR
        (`make-dispatch-macro-character.3`). The previous implementation
        registered a placeholder that returned None -- "no object, keep
        reading" -- so `!x` read as the symbol `x` and every unknown
        sub-character was silently swallowed.
        """
        self._check_mutable('MAKE-DISPATCH-MACRO-CHARACTER')
        self._macro_characters[char] = (
            self._generic_dispatch_reader, bool(non_terminating_p))
        self._syntax_types.pop(char, None)
        self._dispatch_macro_characters[char] = {}

    def _generic_dispatch_reader(self, char, stream):
        """The dispatch body of a user dispatch macro character (CLHS 2.4.8).

        Reads the optional decimal `n` -- whatever `*READ-BASE*` says about
        tokens, the dispatch parameter is *decimal* -- and the sub-character,
        then calls the registered function as `(stream sub-char n)` (CLHS
        2.4.8's argument order for user functions). No `*READ-SUPPRESS*`
        handling of its own: a registered function receives the call either
        way, and an unknown sub-character is a reader error even suppressed
        (the same rule `#`'s unknown-dispatch path applies).
        """
        sub_char = stream.read_char()
        if not sub_char:
            raise EOFError("EOF after dispatch macro character")
        n = None
        if sub_char.isdigit():
            digits = [sub_char]
            while True:
                c = stream.read_char()
                if c and c.isdigit():
                    digits.append(c)
                else:
                    break
            n = int(''.join(digits))
            sub_char = c
            if not sub_char:
                raise EOFError("EOF after dispatch parameter")

        table = self._dispatch_macro_characters.get(char, {})
        function = table.get(sub_char.upper())
        if function is None:
            raise _reader_error(
                f"unknown dispatch sub-character: {char}{sub_char}")
        lisp_stream = lisp_stream_of(stream)
        from . import lisptype
        return function(lisp_stream, lisptype.Character(sub_char), n)
    
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

    def _rebind(self, function, target: 'Readtable'):
        r"""`function`, but reading through `target` if it is one of *this*
        readtable's own built-in reader methods.

        This is what makes a copied readtable a readtable in its own right.
        The built-in macro functions (`_left_paren_reader`, `_sharp_reader`,
        ...) are **bound methods**, and each reads its sub-expressions through
        `self._read_item`, i.e. through the macro characters of the readtable
        it is bound to. Copying the dictionary alone therefore handed the copy
        a set of readers that still consulted the *original*::

            (let ((*readtable* (copy-readtable nil)))
              (set-macro-character #\! (get-macro-character #'))
              (read-from-string "(list 1 !good)"))   ; => (LIST 1 !GOOD)

        -- the `!` worked at top level, where `read_1` looks the character up
        in the current readtable, and was invisible inside the list, whose
        elements were read by the standard readtable's `_left_paren_reader`.
        `(copy-readtable nil)` followed by `set-macro-character` is the
        standard idiom for altering syntax, and every use of it inside an
        aggregate was silently ignored.

        A function that is *not* one of this readtable's methods -- a user
        function, or a reader borrowed from another table with
        `(get-macro-character #')` -- is carried across untouched, because
        then the function really is the value and not a piece of this table.
        """
        owner = getattr(function, '__self__', None)
        if owner is not self:
            return function
        return getattr(target, function.__name__)

    def _copied_tables(self, target: 'Readtable'):
        """This readtable's two syntax tables, rebound for `target`."""
        macro_characters = {
            char: (self._rebind(fn, target), non_terminating)
            for char, (fn, non_terminating) in self._macro_characters.items()
        }
        dispatch = {
            char: {sub: self._rebind(fn, target) for sub, fn in table.items()}
            for char, table in self._dispatch_macro_characters.items()
        }
        return macro_characters, dispatch

    def copy(self) -> 'Readtable':
        """Create a copy of this readtable.
        
        The copy has the same macro characters and dispatch characters,
        but modifying the copy does not affect the original.
        
        Returns:
            A new Readtable instance with copied settings.
        """
        new_rt = Readtable.__new__(Readtable)
        # A copy of the standard readtable is an ordinary, mutable readtable --
        # that is the whole point of `(copy-readtable nil)`.
        new_rt._standard = False
        new_rt._macro_characters, new_rt._dispatch_macro_characters =             self._copied_tables(new_rt)
        new_rt._case = self._case
        # Syntax-type overrides are part of a readtable's syntax and must be
        # copied with it, or `(copy-readtable rt)` would silently answer a
        # table that reads differently from `rt`.
        new_rt._syntax_types = dict(self._syntax_types)
        return new_rt

    def copy_into(self, target: 'Readtable') -> 'Readtable':
        """Overwrite `target` with this readtable's syntax and return it.

        `COPY-READTABLE`'s `to-readtable` argument (CLHS): when supplied, the
        readtable it names is *modified* and returned rather than a fresh one
        being made, which is how `copy-readtable.6` observes that the result is
        EQL to the table it passed in.
        """
        target._check_mutable('COPY-READTABLE')
        target._macro_characters, target._dispatch_macro_characters =             self._copied_tables(target)
        target._case = self._case
        target._syntax_types = dict(self._syntax_types)
        return target
    
    # Simple macro character implementations that don't create circular dependencies
    #
    # Every "ran out of input mid-form" site below raises the builtin
    # `EOFError`, not `ValueError` -- this module deliberately avoids
    # importing `lisptype` (it would be circular), so it cannot raise
    # `lisptype.LispEndOfFileError` directly. `EOFError` is already the
    # convention every reader entry point catches and converts (READ,
    # READ-FROM-STRING, LOAD's per-form loop, the REPL reader in runtime.py)
    # -- attaching the real outer stream there, which these inner handlers
    # never see. A plain `ValueError` here used to fall through those
    # `except EOFError` clauses uncaught, past the reader's own dispatcher,
    # into a generic Python-exception-to-condition fallback that reported it
    # as a bare `ERROR`, not `END-OF-FILE` -- so `(signals-error (read ...)
    # end-of-file)` failed for every compound form truncated mid-read.
    def _left_paren_reader(self, char, stream):
        """Read a list starting with ("""
        from . import lisptype
        result = []
        dotted_tail = None
        # `*READ-SUPPRESS*`: the loop still runs (it is what consumes the
        # elements), but every token it reads answers NIL and no dot marker
        # can appear, so the built chain is discarded at the end.
        suppressed = read_suppressed()

        while True:
            # Skip whitespace
            c = stream.read_char()
            if not c:
                raise EOFError("EOF during list read")
            if c.isspace():
                continue
            if c == ')':
                break
            else:
                # Put the character back and read the next item
                stream.unread_char(c)
                item = self._read_item(stream)
                if item is DOT_MARKER:
                    # The dotted-pair dot (CLHS 2.3.3): exactly one element
                    # must precede it (it cannot come first), exactly one
                    # object follows it, and only the closing paren after
                    # that. A two-dot token is rejected by the token
                    # constructor itself, never reaching this branch.
                    if not result:
                        raise _reader_error(
                            "the dot token may not appear before any element")
                    if dotted_tail is not None:
                        raise _reader_error(
                            "a second dot token after the dotted tail")
                    # Skip whitespace after the dot
                    c = stream.read_char()
                    while c and c.isspace():
                        c = stream.read_char()
                    if not c:
                        raise _reader_error("EOF after the dot token")
                    stream.unread_char(c)
                    # Read the tail
                    dotted_tail = self._read_subform(stream)
                    # After the tail, we expect either whitespace and ) or just )
                    c = stream.read_char()
                    while c and c.isspace():
                        c = stream.read_char()
                    if c != ')':
                        raise _reader_error(f"Expected ) after dotted tail, got {c}")
                    break
                elif item is not None:
                    if dotted_tail is not None:
                        raise _reader_error(
                            "an element may not follow the dotted tail")
                    result.append(item)

        if suppressed:
            return lisptype.NIL

        # Convert to Lisp cons structure
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
        """Read a single item from the stream.

        Two decisions, once each: a *macro character* dispatches to the raw
        entry in this table (the internal ``(char, stream)`` convention), and
        everything else is a **token** read through `lispreader.LispReader`'s
        CLHS 2.2 steps 8-10 -- the one token path. This used to carry its own
        `_read_number`/`_read_symbol` copies here, and the number copy's
        fallback interned every digit-led non-number **into COMMON-LISP-USER
        unconditionally** -- so `(quote 123!)` inside a loaded file read to
        `CL-USER::123!` no matter what `*PACKAGE*` said, which is exactly the
        shape ansi-test's own `deftest` expected-value literals have.

        The dot token is deliberately *not* checked here: it comes back as
        `DOT_MARKER` for the list reader to consume as the dotted-pair dot.
        """
        # Skip whitespace
        c = stream.read_char()
        while c and c.isspace():
            c = stream.read_char()

        if not c:
            return None
        # If this character is a macro character, dispatch to its handler.
        # The *raw* entry is what the reader calls: a user function arrives
        # wrapped in its (char, stream) adapter.
        mc = self._macro_characters.get(c)
        if mc is not None:
            # mc may be (function, non_terminating_p) or a raw function
            func = mc[0] if isinstance(mc, tuple) else mc
            return func(c, stream)

        from .lispreader import LispReader
        reader = LispReader(self, stream)
        return reader._read_given_syntax(c, self.syntax_type(c),
                                         preserving_whitespace())

    def _read_subform(self, stream):
        """`_read_item`, where the result must be an actual object.

        Every construct that reads *one sub-form* -- `'`, `` ` ``, `,#`,
        `#.`, `#S`, `#C`, `#P`, ... -- goes through here, so the dotted-pair
        dot token (`readtable.DOT_MARKER`), which is not an object, is a
        reader error in all of those positions (CLHS 2.3.3). The list reader
        is the one caller that must *not* use this: it consumes the marker
        itself.
        """
        item = self._read_item(stream)
        if item is DOT_MARKER:
            raise _reader_error(
                "the single dot token is valid only as the dot in a dotted list")
        return item

    def _read_string_literal(self, stream, terminator='"'):
        """Read a string literal (already consumed the opening delimiter).

        `terminator` is the delimiter to stop at -- see `_string_reader`.
        Under `*READ-SUPPRESS*` the characters are still consumed to the
        delimiter (consumption is what determines the form's extent) but no
        string is constructed (CLHS 23.1.2).

        The escape rule is the readtable's, not a hardcoded backslash
        (CLHS 2.4.5): **any** character with *single-escape* syntax discards
        itself and takes the next character literally, which is what makes
        `(set-syntax-from-char c #\\)` change how strings containing `c` read
        (`set-syntax-from-char.single-escape.2`). A multiple-escape character
        is *not* special inside a string -- CLHS 2.4.5's `"|x| = |-x|"` example
        is a ten-character string, pipes included.
        """
        suppressed = read_suppressed()
        result = ""
        while True:
            c = stream.read_char()
            if not c:
                raise EOFError("EOF in string literal")
            if c == terminator:
                break
            if self.syntax_type(c) == SYNTAX_SINGLE_ESCAPE:
                # The single escape is discarded; the next character is taken
                # as itself (CLHS 2.4.5).
                next_c = stream.read_char()
                if not next_c:
                    raise EOFError("EOF after escape in string")
                if suppressed:
                    continue
                if c == '\\':
                    # The standard single-escape character keeps its historic
                    # `\n`/`\t`/`\r` interpretations. CLHS 2.4.5 says the next
                    # character is accumulated *literally*, but the unit test
                    # tests/test_roundtrip.py pins the C-style reading, so the
                    # mapping stays until that test is amended.
                    if next_c == 'n':
                        next_c = '\n'
                    elif next_c == 't':
                        next_c = '\t'
                    elif next_c == 'r':
                        next_c = '\r'
                result += next_c
            else:
                if suppressed:
                    continue
                result += c
        if suppressed:
            from . import lisptype
            return lisptype.NIL
        from . import lisptype
        return lisptype.LispString(result)

    def _read_token(self, stream, first_char=None):
        """Read a symbol-like token, honoring CLHS 2.4.5's escape characters.

        A character is either a plain constituent or escaped via `\\` (single
        escape) or `|...|` (multiple escape); an escaped character is used
        *as is*, so case conversion must not touch it (CLHS 23.1.2) and an
        escaped colon is never a package marker.

        Returns `(chars, escaped, consumed, saw_escape)`: `chars` are the
        token's real characters -- case conversion is the *caller's* decision,
        via `convert_case_chars`, because only it knows whether the name is
        being interned or compared -- and `escaped` the parallel list saying
        which were escaped. `saw_escape` is whether any escape *syntax* was
        used at all, even the empty `||`, which contributes no characters but
        still makes a token of dots an ordinary symbol (CLHS 2.3.3:
        `syntax.dot-token.7`, `.||` reads as `|.|`). The `\\x00` placeholder an
        earlier version substituted for an escaped colon could not tell `\\:`
        from a *literal NUL character* (which `syntax.escaped.2` reads), so the
        analysis that needs to tell them apart now runs on the flags instead
        of a substituted string. `consumed` is False only when nothing at all
        followed -- `chars` empty is not the same thing, because `||` (CLHS
        2.4.5) is a *valid*, explicitly-escaped empty name (`universe.lsp`'s
        `'#:||`), not the absence of one.

        Used by the `#:` uninterned-symbol reader; the general token path is
        `lispreader.LispReader.read_8`'s.
        """
        chars = []
        escaped = []
        saw_escape = False

        def consume(c, is_escaped):
            chars.append(c)
            escaped.append(is_escaped)

        consumed = first_char is not None
        if consumed:
            saw_escape = self._consume_token_char(stream, first_char, consume)
        while True:
            c = stream.read_char()
            if not c:
                break
            if c.isspace():
                # CLHS 2.2 step 8: ordinary READ consumes the one whitespace
                # character that terminates the token; READ-PRESERVING-
                # WHITESPACE leaves it for whatever reads the stream next.
                # The distinction follows the innermost READ, through macro-
                # character forms as much as at top level.
                if preserving_whitespace():
                    stream.unread_char(c)
                break
            if c in '()':
                stream.unread_char(c)
                break
            consumed = True
            if self._consume_token_char(stream, c, consume):
                saw_escape = True

        return chars, escaped, consumed, saw_escape

    def _consume_token_char(self, stream, c, consume):
        """One token character: plain, single-escaped, or `|...|`-wrapped.

        `consume(char, was_escaped)` is called once per character the token
        keeps; the return says whether escape *syntax* was seen, which is
        meaningful even when the escape enclosed nothing (`||`).
        """
        if c == '\\':
            escaped = stream.read_char()
            if escaped is None:
                raise EOFError("EOF after single escape in token")
            consume(escaped, True)
            return True
        if c == '|':
            while True:
                p = stream.read_char()
                if p is None:
                    raise EOFError("EOF inside multiple escape")
                if p == '|':
                    return True
                if p == '\\':
                    p = stream.read_char()
                    if p is None:
                        raise EOFError(
                            "EOF after single escape inside multiple escape")
                consume(p, True)
        consume(c, False)
        return False

    def _skip_comment(self, stream):
        """Skip a comment to end of line"""
        while True:
            c = stream.read_char()
            if not c or c == '\n':
                break
    
    def _right_paren_reader(self, char, stream):
        """An unmatched close parenthesis is a READER-ERROR (CLHS 2.4.2)."""
        raise _reader_error(f"unmatched close parenthesis {char!r}")

    def _string_reader(self, char, stream):
        """Read a string literal delimited by `char`.

        The delimiter is the character that *started* the string, not a
        hardcoded `"`. That matters once SET-SYNTAX-FROM-CHAR can copy this
        function onto another character: `(set-syntax-from-char #\\a #\\")`
        then makes `a0a` read as the string "0", where a hardcoded `"` would
        scan to end of input and signal END-OF-FILE instead.
        """
        return self._read_string_literal(stream, terminator=char)
    
    def _quote_reader(self, char, stream):
        """Read a quoted expression."""
        from . import lisptype
        expr = self._read_subform(stream)
        if expr is None:
            raise EOFError("EOF after quote")
        quote_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol("QUOTE")
        return lisptype.lispCons(quote_sym, lisptype.lispCons(expr, lisptype.NIL))
    
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
        from . import lisptype
        # While the backquote's sub-form is being read, a comma is legal
        # (CLHS 2.4.3); the depth on this readtable instance is what
        # `_comma_reader` checks. The counter sits on the instance rather
        # than a module global so that two readtables cannot see each other's
        # nesting -- and a fresh copy starts at zero.
        self._backquote_depth = getattr(self, '_backquote_depth', 0) + 1
        try:
            expr = self._read_subform(stream)
            if expr is None:
                raise EOFError("EOF after backquote")
        finally:
            self._backquote_depth -= 1
        qq_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol("QUASIQUOTE")
        return lisptype.lispCons(qq_sym, lisptype.lispCons(expr, lisptype.NIL))

    def _comma_reader(self, char, stream):
        """Read a comma expression (unquote / unquote-splicing).

        ,x  => (UNQUOTE x)
        ,@x => (UNQUOTE-SPLICING x)

        A comma outside a backquoted form is a reader error (CLHS 2.4.3) --
        constructing `(UNQUOTE x)` there silently built a form that is not
        valid syntax at all.
        """
        from . import lisptype
        if getattr(self, '_backquote_depth', 0) <= 0:
            raise _reader_error("comma outside a backquoted form")
        # Check for @ for unquote-splicing
        next_c = stream.read_char()
        if next_c == '@':
            expr = self._read_subform(stream)
            if expr is None:
                raise EOFError("EOF after comma-splice")
            sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol("UNQUOTE-SPLICING")
            return lisptype.lispCons(sym, lisptype.lispCons(expr, lisptype.NIL))
        else:
            if next_c:
                stream.unread_char(next_c)
            expr = self._read_subform(stream)
            if expr is None:
                raise EOFError("EOF after comma")
            sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol("UNQUOTE")
            return lisptype.lispCons(sym, lisptype.lispCons(expr, lisptype.NIL))
    
    def _sharp_reader(self, char, stream):
        """Handle dispatch macro characters starting with # (CLHS 2.4.8).

        An optional `n` -- read here as a *decimal* integer, whatever
        `*READ-BASE*` says (`syntax.sharp-asterisk.10`: with `*read-base*` 3,
        `#10*` is a ten-bit vector) -- may precede the dispatch character, and
        every dispatch character accepts one syntactically; only the ones
        CLHS gives a parameter to use it. A registered dispatch-macro
        character (SET-DISPATCH-MACRO-CHARACTER) is consulted first and
        receives `(sub-char, stream, n)`, per CLHS 2.4.8.1's argument order;
        an unknown dispatch character is a reader error even under
        `*READ-SUPPRESS*` (the tests that want that: `#<`, `# `, `#)`).
        """
        sub_char = stream.read_char()
        if not sub_char:
            raise EOFError("EOF after #")

        n = None
        if sub_char.isdigit():
            digits = [sub_char]
            while True:
                c = stream.read_char()
                if c and c.isdigit():
                    digits.append(c)
                else:
                    break
            n = int(''.join(digits))
            sub_char = c
            if not sub_char:
                raise EOFError("EOF after #<number>")

        sub_char_upper = sub_char.upper()

        # Check for registered dispatch macro character. A function
        # registered with SET-DISPATCH-MACRO-CHARACTER is a *user* function:
        # CLHS 2.4.8 calls it as (stream sub-char n), unlike this table's
        # built-in `(stream, n)` methods below.
        dispatch_table = self._dispatch_macro_characters.get('#', {})
        if sub_char_upper in dispatch_table:
            return dispatch_table[sub_char_upper](
                lisp_stream_of(stream), _character_of(sub_char), n)

        handler = _SHARP_HANDLERS.get(sub_char) or _SHARP_HANDLERS.get(
            sub_char_upper)
        if handler is None:
            raise _reader_error(f"Unknown # dispatch character: #{sub_char}")
        return handler(self, stream, n)

    # --- `#` dispatch handlers (CLHS 2.4.8) ---
    #
    # Each takes `(stream, n)` and is responsible for its own
    # `*READ-SUPPRESS*` behavior (CLHS 23.1.2): consume the syntactic input
    # the unsuppressed read would consume, construct nothing, and answer NIL.
    # Errors that determine *consumption* still signal under suppression
    # (an unmatched `)` must be hit to know the form ended); errors that
    # only matter to *construction* -- an unknown character name, an invalid
    # radix, a label that was never defined -- do not.

    def _consume_suppressed_form(self, stream):
        """Consume one form under `*READ-SUPPRESS*` and answer NIL."""
        self._read_subform(stream)
        from . import lisptype
        return lisptype.NIL

    def _sharp_function(self, stream, n):
        """`#'x` -> (FUNCTION x) (CLHS 2.4.8.2)."""
        from . import lisptype
        expr = self._read_subform(stream)
        if expr is None:
            raise EOFError("EOF after #'")
        if read_suppressed():
            return lisptype.NIL
        func_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol("FUNCTION")
        return lisptype.lispCons(func_sym, lisptype.lispCons(expr, lisptype.NIL))

    def _sharp_block_comment(self, stream, n):
        """`#|...|#` -- a block comment, nested (CLHS 2.4.8.19)."""
        self._skip_block_comment(stream)
        return None

    def _sharp_character(self, stream, n):
        """`#\\x` -- a character literal (CLHS 2.4.8.1).

        The first character after `#\\` is taken literally -- even a macro
        character or whitespace is the character itself (`#\\(` is `(`). Any
        other character begins a *token*: `#\\ab` is the name "AB", and so is
        `#\\:x` (`:` is a plain constituent, not a macro character), while a
        one-character token is that character with its case preserved
        (`#\\a` is the *lowercase* letter). An escaped name character is used
        as-is, per CLHS 23.1.2.
        """
        from . import lisptype
        c = stream.read_char()
        if not c:
            raise EOFError("EOF in character literal")
        syntax = self.syntax_type(c)
        if syntax in (SYNTAX_WHITESPACE, SYNTAX_TERMINATING_MACRO,
                      SYNTAX_SINGLE_ESCAPE, SYNTAX_MULTIPLE_ESCAPE):
            # Nothing more can belong to the character: it stands for itself.
            if read_suppressed():
                return lisptype.NIL
            return lisptype.Character(c)
        # A constituent: the *token* is the character's name (CLHS 2.4.8.2).
        chars, escaped, _consumed, _saw_escape = self._read_token(stream, c)
        if read_suppressed():
            return lisptype.NIL
        if len(chars) == 1:
            # One character, case preserved.
            return lisptype.Character(chars[0])
        name = ''.join(convert_case_chars(chars, escaped, 'UPCASE'))
        # CLHS 22.1.3.2's #\U+XXXX notation (the printer's output for a
        # character with no name/graphic form) is the reader's job too:
        # name-char answers it, so PRINT.CHAR.8/.9's write/read round-trip
        # needs READ to accept it back. `u+hex` in any case is a code, not
        # a name; the character is built exactly as CODE-CHAR builds one
        # so printer, NAME-CHAR and reader agree on the object.
        if len(name) > 2 and name[:2] == 'U+' \
                and all(d in '0123456789ABCDEF' for d in name[2:]):
            try:
                return lisptype.Character(chr(int(name[2:], 16)))
            except ValueError:
                raise _reader_error(f"#\\U+{name[2:]}: not a valid character code")
        from .lispfunc import characters as _chars
        try:
            return _chars.character(name)
        except lisptype.LispTypeError:
            raise _reader_error(f"Unknown character name: {name}")

    def _sharp_vector(self, stream, n):
        """`#(...)` / `#n(...)` -- a vector literal (CLHS 2.4.8.3)."""
        return self._read_vector(stream, size=n)

    def _sharp_bit_vector(self, stream, n):
        """`#*` / `#n*` -- a bit vector (CLHS 2.4.8.4)."""
        from . import lisptype
        # The bit token ends at whitespace, a terminating macro character,
        # or end of file -- the same boundary CLHS 2.2 step 8 uses -- so
        # `#*012` reads one token "012" and *then* finds the non-bit.
        token = []
        while True:
            c = stream.read_char()
            if c is None:
                break
            syntax = self.syntax_type(c)
            if syntax in (SYNTAX_WHITESPACE, SYNTAX_TERMINATING_MACRO):
                stream.unread_char(c)
                break
            token.append(c)
        if read_suppressed():
            return lisptype.NIL
        bits = ''.join(token)
        if any(b not in '01' for b in bits):
            raise _reader_error(f"#*: {bits!r} is not a sequence of bits")
        if n is None:
            values = [int(b) for b in bits]
        else:
            if len(bits) > n:
                raise _reader_error(
                    f"#{n}* given {len(bits)} bits")
            if not bits:
                if n > 0:
                    raise _reader_error(
                        f"#{n}* has no bit to fill with")
                values = []
            else:
                values = [int(b) for b in bits]
                values.extend([values[-1]] * (n - len(values)))
        from fclpy.lispfunc.arrays import make_bit_vector
        return make_bit_vector(values)

    def _sharp_pathname(self, stream, n):
        """`#P...` -- a pathname designator (CLHS 2.4.8.15)."""
        return self._read_pathname_literal(stream)

    def _sharp_radix_b(self, stream, n):
        return self._read_radix_literal(stream, 2, n)

    def _sharp_radix_o(self, stream, n):
        return self._read_radix_literal(stream, 8, n)

    def _sharp_radix_x(self, stream, n):
        return self._read_radix_literal(stream, 16, n)

    def _sharp_radix_n(self, stream, n):
        return self._read_radix_literal(stream, n, n)

    def _sharp_uninterned(self, stream, n):
        """`#:name` -- an uninterned symbol (CLHS 2.4.8.5)."""
        return self._read_uninterned_symbol(stream)

    def _sharp_struct(self, stream, n):
        """`#S(name ...)` -- a structure instance (CLHS 2.4.8.14)."""
        return self._read_structure(stream)

    def _sharp_eval(self, stream, n):
        """`#.(form)` -- read-time evaluation (CLHS 2.4.8.10)."""
        from . import lisptype
        from .lispreader import resolve_read_eval
        import fclpy.state as state
        if read_suppressed():
            # The form is consumed but never evaluated -- not even a THROW
            # inside it can fire (`read-suppress.sharp-dot.3`).
            return self._consume_suppressed_form(stream)
        if not resolve_read_eval():
            raise _reader_error(
                "#. may not evaluate: *READ-EVAL* is false (CLHS 2.4.8.10)")
        expr = self._read_subform(stream)
        if expr is None:
            raise EOFError("EOF after #.")
        env = state.current_environment
        if env is not None:
            from fclpy.lispfunc.evaluation_core import eval
            return eval(expr, env)
        return expr

    def _sharp_feature_plus(self, stream, n):
        """`#+feature form` (CLHS 2.4.8.16)."""
        return self._read_feature(stream, negate=False)

    def _sharp_feature_minus(self, stream, n):
        """`#-feature form` (CLHS 2.4.8.17)."""
        return self._read_feature(stream, negate=True)

    def _sharp_array(self, stream, n):
        """`#nA(...)` -- an array literal (CLHS 2.4.8.12)."""
        if read_suppressed():
            return self._consume_suppressed_form(stream)
        if n is None:
            raise _reader_error("#A requires a rank")
        return self._read_array(stream, n)

    def _sharp_complex(self, stream, n):
        """`#C(r i)` -- a complex number (CLHS 2.4.8.11).

        Suppressed, the construct consumes *one form* -- `#c1` and `#cFOO`
        are valid suppressed syntax -- rather than demanding a paren the
        unsuppressed read would insist on.
        """
        if read_suppressed():
            return self._consume_suppressed_form(stream)
        return self._read_complex_number(stream)

    def _sharp_label(self, stream, n):
        """`#n=form` -- define a label (CLHS 2.4.8.5/.6 circular syntax)."""
        return self._read_sharp_equal(stream, n)

    def _sharp_label_ref(self, stream, n):
        """`#n#` -- reference a label (CLHS 2.4.8.6)."""
        return self._read_sharp_sharp(stream, n)

    def _read_feature(self, stream, negate):
        """`#+`/`#-`'s shared body.

        The feature expression is read with `*PACKAGE*` bound to the KEYWORD
        package (CLHS 2.4.8.1), so `#+x` names the keyword `:X` regardless of
        the reading package, while a package-qualified name
        (`#+cl-test::x`) still denotes the symbol it writes. When the
        feature-expression fails, the following form is *skipped* -- read
        with `*READ-SUPPRESS*` true, which is exactly the machinery CLHS
        23.1.2 gives the reader for consuming a form without constructing
        it -- and the handler answers None so the outer reader carries on
        with the form after it.
        """
        from . import lisptype
        from . import state
        from .lispfunc.binding import BindingFrame

        feature = self._read_feature_expression(stream)
        present = self._check_feature(feature)
        if negate:
            present = not present
        if present:
            form = self._read_subform(stream)
            if read_suppressed():
                return lisptype.NIL
            return form
        self._skip_form(stream)
        return None

    def _read_feature_expression(self, stream):
        """Read a feature expression with `*PACKAGE*` bound to KEYWORD."""
        from . import lisptype
        from . import state
        from .lispfunc.binding import BindingFrame

        package_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*PACKAGE*')
        keyword_pkg = getattr(lisptype, 'KEYWORD_PACKAGE', None)
        if keyword_pkg is None:
            keyword_pkg = lisptype.find_package('KEYWORD')
        env = state.current_environment
        if env is None or keyword_pkg is None:
            return self._read_subform(stream)
        frame = BindingFrame(env, bound_vars=(package_sym,))
        with frame:
            frame.bind(package_sym, keyword_pkg)
            return self._read_subform(stream)

    def _skip_form(self, stream):
        """Consume one form with `*READ-SUPPRESS*` bound true (CLHS 2.4.8.1).

        This is how a failing `#+` feature-expression skips exactly one form
        -- the suppressed readers consume the form's extent without
        constructing it, which is the one way to know where it ends.
        """
        from . import lisptype
        from . import state
        from .lispfunc.binding import BindingFrame

        suppress_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol(
            '*READ-SUPPRESS*')
        env = state.current_environment
        if env is None:
            self._read_item(stream)
            return
        frame = BindingFrame(env, bound_vars=(suppress_sym,))
        with frame:
            frame.bind(suppress_sym, lisptype.T)
            self._read_item(stream)

    def _skip_block_comment(self, stream):
        """Skip a block comment #| ... |# with nesting support."""
        depth = 1
        prev_char = None
        
        while depth > 0:
            c = stream.read_char()
            if not c:
                raise EOFError("EOF in block comment")
            
            if prev_char == '|' and c == '#':
                depth -= 1
                prev_char = None
            elif prev_char == '#' and c == '|':
                depth += 1
                prev_char = None
            else:
                prev_char = c
    
    def _read_vector(self, stream, size=None):
        """Read a vector literal ``#(...)`` or ``#n(...)``.

        A ``#(...)`` literal is a *simple* vector (CLHS 2.4.8.3), so it reads
        as this implementation's simple-vector representation, a Python list.
        It used to read as an `AdjustableVector`, which made every literal
        vector claim to be adjustable and to have a fill pointer.
        """
        from . import lisptype
        suppressed = read_suppressed()
        result = []
        while True:
            # Skip whitespace
            c = stream.read_char()
            if not c:
                raise EOFError("EOF in vector literal")
            if c.isspace():
                continue
            if c == ')':
                break
            else:
                stream.unread_char(c)
                item = self._read_item(stream)
                if item is DOT_MARKER:
                    # A vector has no dotted tail (CLHS 2.3.3 via
                    # `syntax.dot-error.7`), and the marker must never end up
                    # stored as an element.
                    raise _reader_error(
                        "the dot token may not appear in a vector literal")
                if item is not None:
                    result.append(item)

        if suppressed:
            return lisptype.NIL
        if size is not None:
            # `#n(...)` is a vector of length n; a shorter element list is
            # padded with its last element (CLHS 2.4.8.3).
            if len(result) > size:
                raise _reader_error(
                    f"#{size}(...) given {len(result)} elements")
            if len(result) < size:
                if not result:
                    raise _reader_error(f"#{size}() has no element to replicate")
                result.extend([result[-1]] * (size - len(result)))
        return result
    
    def _check_feature(self, feature):
        """Check if a feature expression is satisfied (CLHS 24.1.2.1).

        A feature expression is a symbol, or a list whose head names the
        operator `AND`/`OR`/`NOT` (the operator may be a keyword -- `(:and)`
        and `(:not :x)` are feature expressions). A feature *symbol* matches
        by EQ against the elements of `*FEATURES*`: the expression was read
        with `*PACKAGE*` bound to KEYWORD, so `#+x` denotes `:X` and matches
        the keyword in the list, while a package-qualified name
        (`#+cl-test::x`) only matches that exact symbol
        (`syntax.sharp-plus.8` vs `.9` pins the difference). `(AND)` with no
        sub-features is satisfied; `(OR)` with none is not.
        """
        from . import state
        from . import lisptype

        def features():
            from .lispfunc.binding import dynamic_value
            return dynamic_value(
                lisptype.COMMON_LISP_PACKAGE.intern_symbol('*FEATURES*'))

        if hasattr(feature, 'name') and not hasattr(feature, 'car'):
            current = features()
            while hasattr(current, 'car') and hasattr(current, 'cdr') and \
                    current is not lisptype.NIL and current is not None:
                if current.car is feature:
                    return True
                current = current.cdr
            return False
        if hasattr(feature, 'car') and hasattr(feature, 'cdr'):
            operator = feature.car
            op_name = getattr(operator, 'name', None)
            if op_name is not None:
                op_name = op_name.upper()
                if op_name == 'AND':
                    current = feature.cdr
                    while hasattr(current, 'car') and hasattr(current, 'cdr') and current is not lisptype.NIL:
                        if not self._check_feature(current.car):
                            return False
                        current = current.cdr
                    return True
                if op_name == 'OR':
                    current = feature.cdr
                    while hasattr(current, 'car') and hasattr(current, 'cdr') and current is not lisptype.NIL:
                        if self._check_feature(current.car):
                            return True
                        current = current.cdr
                    return False
                if op_name == 'NOT':
                    sub_feature = feature.cdr
                    if hasattr(sub_feature, 'car'):
                        sub_feature = sub_feature.car
                    return not self._check_feature(sub_feature)

        # Unknown feature expression - default to absent
        return False

    def _read_pathname_literal(self, stream):
        """`#P` -- read a form and coerce it as a string designator to a
        pathname (CLHS 2.4.8.15).

        The form, not a quotation mark: `#P` followed by any expression
        evaluating-to-a-string-designator is valid syntax
        (`syntax.sharp-p.5` writes `#P#.(make-array ... 'base-char)`), so
        requiring the next *character* to be `"` made every other spelling a
        reader error.
        """
        from fclpy.lispfunc.pathnames import pathname_from_namestring
        from . import lisptype

        form = self._read_subform(stream)
        if read_suppressed():
            return lisptype.NIL
        if isinstance(form, (str, lisptype.LispString)):
            return pathname_from_namestring(form)
        if isinstance(form, lisptype.LispSymbol):
            return pathname_from_namestring(form.name)
        if isinstance(form, lisptype.Character):
            return pathname_from_namestring(form.char)
        raise _reader_error(
            f"#P: {type(form).__name__} does not designate a string")

    def _read_radix_literal(self, stream, radix, n):
        """`#B`/`#O`/`#X`/`#nR` -- a rational in `radix` (CLHS 2.4.8.7).

        `radix` is 2/8/16 for the letter forms and `n` itself for `#nR`;
        `#r` with no prefix has no radix and is an error. Under
        `*READ-SUPPRESS*` the token is consumed without any parsing or
        validation -- `#0b0` must answer NIL, not complain about radix 0.
        """
        from . import lisptype

        if read_suppressed():
            while True:
                c = stream.read_char()
                if c is None:
                    break
                syntax = self.syntax_type(c)
                if syntax in (SYNTAX_WHITESPACE, SYNTAX_TERMINATING_MACRO):
                    stream.unread_char(c)
                    break
            return lisptype.NIL

        if radix is None or radix < 2 or radix > 36:
            raise _reader_error(f"Invalid radix: {radix}")
        return self._read_radix_number(stream, radix)
    
    def _read_radix_number(self, stream, radix):
        """Read a **rational** in `radix` -- `#B`/`#O`/`#X`/`#nR`, CLHS 2.4.8.

        Rational, not integer: `#x1A/B` is a ratio, and this used to stop at
        the first character that was not a digit of its radix, so it answered
        the numerator alone and left `/B` on the stream to be read as the next
        form. `print.ratios.random` prints under a random `*print-base*` and
        `*print-radix*`, so `#x951115BA/AC02A5F7` is output the printer itself
        produces -- the reader answering `2500924858` for it is a round-trip
        failure of the printer's own writing.

        The token is accumulated up to the next terminating macro character or
        whitespace (the same boundary CLHS 2.2 step 8 uses) and then handed to
        `numtoken`, so the radix syntax and the `*READ-BASE*` syntax are one
        implementation. A digit-by-digit scan cannot be, because the set of
        characters that belong to the token is not the set of digits.
        """
        from . import numtoken as _numtoken

        try:
            _numtoken.check_radix(radix, 'radix')
        except _numtoken.NumericTokenError as exc:
            raise _reader_error(str(exc))

        token = ''
        while True:
            c = stream.read_char()
            if c is None:
                break
            syntax = self.syntax_type(c)
            if syntax in (SYNTAX_WHITESPACE, SYNTAX_TERMINATING_MACRO):
                stream.unread_char(c)
                break
            token += c

        if not token:
            raise _reader_error(f"No digits found for radix-{radix} number")

        try:
            value = _numtoken.parse_numeric_token(token, radix=radix)
        except _numtoken.NumericTokenError as exc:
            raise _reader_error(str(exc))
        if value is None or isinstance(value, float):
            # `#x1.5` is not "a float in base 16" -- CLHS 2.4.8.7 admits only a
            # rational after a radix prefix, so a token that parses as a float
            # is an error rather than a value read in some other radix.
            raise _reader_error(f"{token!r} is not a rational in radix {radix}")
        return value

    def _read_uninterned_symbol(self, stream):
        """Read an uninterned symbol like #:foo (CLHS 2.4.8.5).

        Uninterned symbols are not part of any package. Each time #:foo is
        read, a fresh symbol with name "FOO" is created that has no home
        package -- *every* time, and for *every* name: `#:t` is an uninterned
        symbol named "T", not the global constant, and `#:. ` names ".". It
        is an error for the name to contain an unescaped package marker
        (CLHS 2.4.8.5; `syntax.sharp-colon.error.1`), while `#:|a:b|`'s
        escaped colon is fine.
        """
        from . import lisptype

        # Read the symbol name, honoring `\`/`|...|` escapes (CLHS 2.4.5)
        # exactly as `_read_symbol` does -- this used to read raw characters
        # with no escape handling at all, so `#:|abc|` kept its literal pipe
        # characters as part of the name and then upcased them too.
        chars, escaped, consumed, _saw_escape = self._read_token(stream)

        if read_suppressed():
            return lisptype.NIL

        if not consumed:
            raise _reader_error("Empty symbol name after #:")

        if any(c == ':' and not e for c, e in zip(chars, escaped)):
            raise _reader_error(
                "an uninterned symbol's name may not contain a package marker")

        name = ''.join(convert_case_chars(chars, escaped, self._case))
        return lisptype.LispSymbol(name, package=None)

    def _read_complex_number(self, stream):
        """Read a complex number literal #C(real imag) (CLHS 2.4.8.11).

        The syntax is #C(real imag) where real and imag are real numbers.
        The reader's rule is the COMPLEX function's coalescing rule (CLHS
        12.1.5.3): a rational real part with a zero imaginary part *is* the
        real part, so `#C(1 0)` reads as the integer 1 and only
        `#c(0 1)` builds a complex.
        """
        from fractions import Fraction
        from . import lisptype

        # Skip whitespace
        while True:
            c = stream.read_char()
            if not c:
                raise EOFError("EOF after #C")
            if not c.isspace():
                break

        # Expect opening paren
        if c != '(':
            raise _reader_error(f"Expected ( after #C, got {c!r}")

        # Read real part
        real_part = self._read_subform(stream)
        if real_part is None:
            raise _reader_error("Expected real part in #C(...)")

        # Read imaginary part
        imag_part = self._read_subform(stream)
        if imag_part is None:
            raise _reader_error("Expected imaginary part in #C(...)")

        # Skip whitespace and find closing paren
        while True:
            c = stream.read_char()
            if not c:
                raise EOFError("EOF in #C(...)")
            if c.isspace():
                continue
            if c == ')':
                break
            else:
                raise _reader_error(f"Expected ) in #C(...), got {c!r}")

        if read_suppressed():
            return lisptype.NIL

        # Both parts must be *real* numbers -- a rational (int/Fraction) or
        # a float. A complex part is not a real, so `#c(#c(1 2) 3)` is an
        # error, as is any other non-number.
        if not isinstance(real_part, (int, float, Fraction)):
            raise _reader_error(f"Real part must be a number, got {type(real_part).__name__}")
        if not isinstance(imag_part, (int, float, Fraction)):
            raise _reader_error(f"Imaginary part must be a number, got {type(imag_part).__name__}")

        # The reader applies COMPLEX's rules (CLHS 12.1.5.3), mirroring
        # `misc_macros.complex_fn`: a *rational* real part with a *rational*
        # zero imag part coalesces to the real part (`syntax.sharp-c.2`:
        # `#C(1 0)` is 1; `(complex 1/2 0)` is 1/2). A float zero does not
        # coalesce -- `(complex 1 0.0)` is `#c(1.0 0.0)` -- and neither
        # does a float real part (`(complex 1.0 0)` is `#c(1.0 0.0)`).
        if imag_part == 0 and not isinstance(imag_part, float) and \
                isinstance(real_part, (int, Fraction)):
            return real_part

        # Both parts rational: keep the literal exact (CLHS 2.3.2 -- the
        # ratio in the source denotes a ratio). Converting the parts to
        # float here is what made `#c(1/2 1/3)` read as a float complex
        # and left `expt.16` comparing a float answer against the exact
        # `#c(-1/24 23/108)` the source wrote.
        if isinstance(real_part, (int, Fraction)) and \
                isinstance(imag_part, (int, Fraction)):
            from fclpy.lispfunc.math_arithmetic import LispComplex
            return LispComplex(real_part, imag_part)

        # At least one float: a float-parts complex, as `complex_fn` builds
        # it (`complex.5`'s rule that a complex with a float part has float
        # parts throughout).
        return complex(real_part, imag_part)

    def _read_array(self, stream, rank):
        """Read a ``#nA(...)`` array literal (CLHS 2.4.8.12).

        The dimensions are those of the nested sequences that follow, which
        is why this cannot simply hand the nested structure back: an array of
        rank 2 and a list of lists are different objects.
        """
        contents = self._read_item(stream)
        from fclpy.lispfunc.arrays import make_array

        if read_suppressed():
            from . import lisptype
            return lisptype.NIL
        if rank == 0:
            return make_array(None, initial_contents=contents)
        dimensions = _nested_dimensions(contents, rank)
        return make_array(dimensions, initial_contents=contents)

    def _read_structure(self, stream):
        """`#S(name slot-name value ...)` -- a structure instance (CLHS 2.4.8.14).

        The operand is read as *data*, then interpreted: `name` must name a
        defined structure class, each `slot-name` -- a symbol, keyword,
        string, or character, compared by name -- must name one of its slots
        (`:allow-other-keys` is consumed and ignored), a slot named twice
        keeps its *first* value, and an unspecified slot gets its initform.
        The instance is built through the one class/instance model
        (`classes.LispInstance`), so `#s` and the DEFSTRUCT constructors
        produce indistinguishable objects.
        """
        from . import lisptype

        form = self._read_subform(stream)
        if form is None:
            raise EOFError("EOF after #S")
        if read_suppressed():
            return lisptype.NIL

        import fclpy.classes as classes
        from .lispfunc.misc_clos import _eval_initform

        if not isinstance(form, lisptype.lispCons):
            raise _reader_error("#S expects a (name slot-name value ...) list")
        name_obj = form.car
        name = getattr(name_obj, 'name', None)
        if not name:
            raise _reader_error(f"#S: {type(name_obj).__name__} does not name a structure")
        struct_class = classes.find_class(name)
        if struct_class is None:
            raise _reader_error(f"#S: no structure named {name}")
        if getattr(struct_class, 'metaclass_name', '') != 'STRUCTURE-CLASS':
            raise _reader_error(f"#S: {name} is not a structure class")

        slots = struct_class.get_all_slots()
        slot_values = {name_str: _eval_initform(slot_def)
                       for name_str, slot_def in slots.items()}
        seen = set()

        # `:allow-other-keys` may appear anywhere among the pairs and (with
        # a non-NIL value) permits slot names the structure does not define
        # (`syntax.sharp-s.8`: `:b z :allow-other-keys t :a x :foo bar`),
        # so its presence is settled before the pairs are processed.
        allow_other = False
        scan = form.cdr
        while scan is not None and scan is not lisptype.NIL and \
                isinstance(scan, lisptype.lispCons):
            key = getattr(scan.car, 'name', None)
            if key and key.upper() == 'ALLOW-OTHER-KEYS':
                allow_other = True
                break
            scan = scan.cdr
            if not isinstance(scan, lisptype.lispCons):
                break
            scan = scan.cdr

        cur = form.cdr
        while cur is not None and cur is not lisptype.NIL and \
                isinstance(cur, lisptype.lispCons):
            key_obj = cur.car
            cur = cur.cdr
            if not isinstance(cur, lisptype.lispCons):
                raise _reader_error("#S: odd number of slot-name/value pairs")
            value = cur.car
            cur = cur.cdr

            if isinstance(key_obj, lisptype.Character):
                key = key_obj.char
            elif isinstance(key_obj, (lisptype.LispSymbol, lisptype.lispKeyword)):
                key = key_obj.name
            elif isinstance(key_obj, (str, lisptype.LispString)):
                key = str(key_obj)
            else:
                raise _reader_error(
                    f"#S: {type(key_obj).__name__} does not name a slot")
            if key.upper() == 'ALLOW-OTHER-KEYS':
                continue
            if key not in slots:
                if allow_other:
                    continue
                raise _reader_error(f"#S: {key} is not a slot of {name}")
            if key in seen:
                continue
            seen.add(key)
            slot_values[key] = value

        return classes.LispInstance(lisp_class=struct_class,
                                    slot_values=slot_values)

    def _read_sharp_equal(self, stream, n):
        """`#n=form` -- define a label (CLHS 2.4.8.5).

        A placeholder is registered *before* the form is read, so a `#n#`
        inside the form yields the placeholder; when the form is complete
        every occurrence of the placeholder in it is patched to the form
        itself, by identity, which is what makes `#1=(A B . #1#)` an actual
        cycle and `(#1=(17) #1#)` one shared list.
        """
        from . import lisptype

        if read_suppressed():
            return self._consume_suppressed_form(stream)
        if n is None:
            raise _reader_error("#= requires a label number")
        frame = current_label_frame()
        if frame is None:
            raise _reader_error("#= read outside of a read")
        if n in frame:
            raise _reader_error(f"label #{n}= is already defined")
        placeholder = _LabelPlaceholder(n)
        frame[n] = placeholder
        form = self._read_subform(stream)
        if form is None:
            raise EOFError("EOF after #=")
        if form is placeholder:
            raise _reader_error(f"#{n}= may not reference itself")
        frame[n] = form
        _patch_label_placeholders(form, {id(placeholder): form})
        return form

    def _read_sharp_sharp(self, stream, n):
        """`#n#` -- reference the object an earlier `#n=` read (CLHS 2.4.8.6)."""
        from . import lisptype

        if read_suppressed():
            # The label need not exist -- or even have been read: the
            # reference answers NIL and no error (`##`, `#1#`).
            return lisptype.NIL
        if n is None:
            raise _reader_error("## requires a label number")
        frame = current_label_frame()
        if frame is None or n not in frame:
            raise _reader_error(f"reference to undefined label #{n}#")
        return frame[n]


def _patch_label_placeholders(obj, mapping):
    """Replace every label placeholder in `obj` with the object its `#n=`
    read, in place, by identity.

    `mapping` is `{id(placeholder): value}` -- ids, not the placeholders
    themselves, because Lisp objects define structural `__eq__`s that would
    let one placeholder "be" another. The walk covers cons cells (car by
    recursion, the cdr chain by iteration), Python lists (the simple-vector
    representation), array storage, and instance slot values -- everything a
    literal can build -- and is cycle-safe, because a circular structure is
    exactly the thing it exists to produce.
    """
    from . import lisptype

    seen = set()

    def walk(value):
        if isinstance(value, lisptype.lispCons):
            cur = value
            while isinstance(cur, lisptype.lispCons):
                cid = id(cur)
                if cid in seen:
                    return
                seen.add(cid)
                car = cur.car
                if id(car) in mapping:
                    car = mapping[id(car)]
                    cur.car = car
                walk(car)
                nxt = cur.cdr
                if id(nxt) in mapping:
                    nxt = mapping[id(nxt)]
                    cur.cdr = nxt
                cur = nxt
            return
        if isinstance(value, list):
            vid = id(value)
            if vid in seen:
                return
            seen.add(vid)
            for i, el in enumerate(value):
                if id(el) in mapping:
                    el = mapping[id(el)]
                    value[i] = el
                walk(el)
            return
        # Array storage (a non-displaced LispArray's `_data`) and instance
        # slot values are the remaining containers a literal can build.
        storage = getattr(value, '_data', None)
        if isinstance(storage, list):
            walk(storage)
            return
        slots = getattr(value, 'slot_values', None)
        if isinstance(slots, dict):
            sid = id(value)
            if sid in seen:
                return
            seen.add(sid)
            for key, el in slots.items():
                if id(el) in mapping:
                    el = mapping[id(el)]
                    slots[key] = el
                walk(el)

    walk(obj)


# The `#` dispatch table (CLHS 2.4.8). Symbol-valued keys are matched
# case-sensitively, the letter keys case-insensitively (`_sharp_reader` tries
# the sub-character as given, then upper-cased). Registered user dispatch
# functions take precedence over all of these.
_SHARP_HANDLERS = {
    "'": Readtable._sharp_function,
    '|': Readtable._sharp_block_comment,
    '\\': Readtable._sharp_character,
    '(': Readtable._sharp_vector,
    '*': Readtable._sharp_bit_vector,
    ':': Readtable._sharp_uninterned,
    '.': Readtable._sharp_eval,
    '+': Readtable._sharp_feature_plus,
    '-': Readtable._sharp_feature_minus,
    '=': Readtable._sharp_label,
    '#': Readtable._sharp_label_ref,
    'B': Readtable._sharp_radix_b,
    'O': Readtable._sharp_radix_o,
    'X': Readtable._sharp_radix_x,
    'R': Readtable._sharp_radix_n,
    'C': Readtable._sharp_complex,
    'A': Readtable._sharp_array,
    'S': Readtable._sharp_struct,
    'P': Readtable._sharp_pathname,
}


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


# The "argument not supplied" marker. NIL is a *meaningful* readtable
# designator here, so a `=None` default cannot tell "omitted" (the current
# readtable) from "given NIL" (the standard readtable). One sentinel object
# for the whole implementation -- this module used to define its own, and
# every other operator with the same problem had no sentinel at all.
from fclpy.lisptype_basic import OMITTED as _OMITTED


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
