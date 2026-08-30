"""Character operations - character predicates, manipulation, and comparison."""

import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry
from . import arrays as _arrays


# CLHS 13.1.7's standard character names. `NAME-CHAR` parses a name
# case-insensitively (several names, e.g. NEWLINE/LINEFEED, may share a
# character); `CHAR-NAME` must answer with the exact spelling ansi-test
# checks via `(string= (char-name #\Space) "Space")` -- capitalized, not
# upper-case -- so it is not simply the other table's key.
_NAMED_CHAR_TEXT = {
    "SPACE": " ", "NEWLINE": "\n", "LINEFEED": "\n", "TAB": "\t",
    "RETURN": "\r", "PAGE": "\f", "BACKSPACE": "\b", "RUBOUT": chr(127),
    "NULL": chr(0), "NUL": chr(0), "BELL": chr(7), "BEL": chr(7),
    "ESCAPE": chr(27), "ESC": chr(27),
}
_CHAR_DISPLAY_NAME = {
    " ": "Space", "\n": "Newline", "\t": "Tab", "\r": "Return",
    "\f": "Page", "\b": "Backspace", chr(127): "Rubout",
    chr(0): "Null", chr(7): "Bell", chr(27): "Escape",
}


def _char_text(character):
    """The one-character text of a Lisp CHARACTER argument, or None.

    A character here is `lisptype.Character` -- what the reader's `#\\`
    syntax and CODE-CHAR/DIGIT-CHAR/CHAR-UPCASE/... construct -- or a bare
    length-1 Python `str`, which is what a string index yielded before
    `arrays.string_element` existed and what a few callers still pass
    directly. Every predicate/comparison below has to accept both or "the
    same" character disagrees with itself depending on where it came from:
    before this, every function here tested `isinstance(character, str)`
    only, so a `Character` argument -- the normal shape once FIND/POSITION/
    LOOP-across on a string wrap their elements via `string_element` --
    silently fell through as "not a character" (a predicate false, an
    accessor unchanged, an ordering comparison a bare Python `TypeError`
    between two `Character`s leaking as the value of the Lisp form).
    """
    if isinstance(character, lisptype.Character):
        return character.char
    if isinstance(character, str) and len(character) == 1:
        return character
    return None


def _require_char_text(character, name):
    """`_char_text`, signalling TYPE-ERROR instead of returning None.

    ansi-test's `char-type-error-check` (used by ALPHA-CHAR-P, BOTH-CASE-P,
    GRAPHIC-CHAR-P, {UPPER,LOWER}-CASE-P, STANDARD-CHAR-P, CHAR-NAME) drives
    every object in `*universe*` through the predicate and asserts a
    TYPE-ERROR for each non-character -- so these can't stay merely
    permissive-false the way a CLHS "consequences are undefined" reading
    would allow.
    """
    text = _char_text(character)
    if text is None:
        raise lisptype.LispTypeError(
            f"{name}: {character!r} is not a character",
            expected_type="CHARACTER", actual_value=character)
    return text


def _char_texts(characters, name):
    """`_char_text` over every argument of a variadic character function.

    CLHS 5.3's comparison functions have the lambda list `(character
    &rest more-characters)` -- at least one argument is required, so zero
    arguments is a PROGRAM-ERROR rather than the vacuous-true these used to
    return for "fewer than two".
    """
    if not characters:
        raise lisptype.LispProgramError(
            f"{name}: at least one argument is required")
    texts = []
    for c in characters:
        text = _char_text(c)
        if text is None:
            raise lisptype.LispTypeError(
                f"{name}: {c!r} is not a character",
                expected_type="CHARACTER", actual_value=c)
        texts.append(text)
    return texts


@_registry.cl_function('ALPHA-CHAR-P')
def alpha_char_p(character):
    """Test if character is alphabetic."""
    text = _require_char_text(character, 'ALPHA-CHAR-P')
    return lisptype.lisp_bool(text.isalpha())


@_registry.cl_function('ALPHANUMERICP')
def alphanumericp(character):
    """Test if character is alphanumeric.

    "Alphanumeric" is *alphabetic or a digit* here -- the same two
    predicates ALPHA-CHAR-P and DIGIT-CHAR-P answer -- and not Python's
    `str.isalnum()`, which is true for the superscript digits (`²`) that
    DIGIT-CHAR-P rejects, so the two answers disagreed
    (`alphanumericp.5.body` walks every code point and requires exactly
    `alphanumericp` ⇔ `(or (alpha-char-p x) (digit-char-p x))`).
    """
    text = _require_char_text(character, 'ALPHANUMERICP')
    return lisptype.lisp_bool(text.isalpha() or digit_char_p(text) is not None)


@_registry.cl_function('BOTH-CASE-P')
def both_case_p(character):
    """Test if character has case.

    This must agree with UPPER-CASE-P and LOWER-CASE-P exactly: a character
    that either of those answers T for has case, and a character BOTH-CASE-P
    answers T for must be reported upper or lower case by them
    (`both-case-p.2.body` requires exactly this over every code point). The
    previous `upper() != lower()` test answered T for the titlecase
    characters (for which neither UPPER-CASE-P nor LOWER-CASE-P is true) and
    disagreed the other way wherever Python's case predicates diverge from
    each other.
    """
    text = _require_char_text(character, 'BOTH-CASE-P')
    return lisptype.lisp_bool(text.isupper() or text.islower())


@_registry.cl_function('CHAR-CODE')
def char_code(character):
    """Get character code."""
    text = _char_text(character)
    if text is not None:
        return ord(text)
    raise lisptype.LispTypeError("CHAR-CODE: argument must be a character",
                                expected_type="CHARACTER",
                                actual_value=character)


def _single_char_case_map(text, mapped):
    """`mapped` if it is still one character, else `text` unchanged.

    CLHS 13.1.1's case mapping is between individual characters; Python's
    `str.lower()`/`.upper()` is a locale mapping and not length-preserving
    (`'ß'.upper()` is the two characters `'SS'`), so a character with no
    single-character case partner has no case conversion to perform.
    """
    return mapped if len(mapped) == 1 else text


@_registry.cl_function('CHAR-DOWNCASE')
def char_downcase(character):
    """Convert character to lowercase.

    CLHS 13.1.1: a character that is not upper case is returned *unchanged*
    -- CHAR-DOWNCASE maps only the upper-case characters. Running
    `str.lower()` on every character changed titlecase and uncased letters
    (`ǅ` became `ǆ`, `İ` became two characters), which
    `char-downcase.2.body`'s `(or (upper-case-p x) (eqlt u x))` rejects.
    """
    text = _char_text(character)
    if text is None:
        raise lisptype.LispTypeError("CHAR-DOWNCASE: argument must be a character",
                                    expected_type="CHARACTER", actual_value=character)
    if text.isupper():
        return lisptype.Character(_single_char_case_map(text, text.lower()))
    return lisptype.Character(text)


@_registry.cl_function('CHAR-UPCASE')
def char_upcase(character):
    """Convert character to uppercase.

    CLHS 13.1.1: a character that is not lower case is returned *unchanged*
    -- CHAR-UPCASE maps only the lower-case characters. Running
    `str.upper()` on every character changed titlecase and uncased letters
    (`ǅ` became `Ǆ` although `(lower-case-p x)` is NIL, which
    `char-upcase.2.body` rejects), and the two-character expansions
    (`'ß'.upper()`) leaked into CHAR-EQUAL's comparisons as `ord()` on a
    two-character string.
    """
    text = _char_text(character)
    if text is None:
        raise lisptype.LispTypeError("CHAR-UPCASE: argument must be a character",
                                    expected_type="CHARACTER", actual_value=character)
    if text.islower():
        return lisptype.Character(_single_char_case_map(text, text.upper()))
    return lisptype.Character(text)


def _fold_case(text):
    """Upper-case one character for a case-insensitive comparison.

    Shares `_single_char_case_map`'s guard against `str.upper()`'s locale
    expansions (`'ß'.upper()` is two characters, `'SS'`): folding must
    never change how many characters are being compared, or a single
    sharp-s would sort as greater than every single-character neighbor
    instead of comparing by code point.
    """
    return _single_char_case_map(text, text.upper())


@_registry.cl_function('CHAR=')
def char_equal(*characters):
    """Test character equality (case sensitive)."""
    texts = _char_texts(characters, 'CHAR=')
    return lisptype.lisp_bool(all(t == texts[0] for t in texts[1:]))


@_registry.cl_function('CHAR-EQUAL')
def char_equal_ignore_case(*characters):
    """Test character equality (case insensitive)."""
    texts = [_fold_case(t) for t in _char_texts(characters, 'CHAR-EQUAL')]
    return lisptype.lisp_bool(all(t == texts[0] for t in texts[1:]))

@_registry.cl_function('CHAR-GREATERP')
def char_greaterp(*characters):
    """Test character greater than (case insensitive)."""
    texts = [_fold_case(t) for t in _char_texts(characters, 'CHAR-GREATERP')]
    return lisptype.lisp_bool(all(texts[i] > texts[i+1] for i in range(len(texts)-1)))

@_registry.cl_function('CHAR-LESSP')
def char_lessp(*characters):
    """Test character less than (case insensitive)."""
    texts = [_fold_case(t) for t in _char_texts(characters, 'CHAR-LESSP')]
    return lisptype.lisp_bool(all(texts[i] < texts[i+1] for i in range(len(texts)-1)))

@_registry.cl_function('CHAR-NOT-EQUAL')
def char_not_equal_ignore_case(*characters):
    """Test that no two characters are the same (case insensitive)."""
    texts = [_fold_case(t) for t in _char_texts(characters, 'CHAR-NOT-EQUAL')]
    return lisptype.lisp_bool(len(set(texts)) == len(texts))

@_registry.cl_function('CHAR-NOT-GREATERP')
def char_not_greaterp(*characters):
    """Test characters are monotonically nondecreasing (case insensitive)."""
    texts = [_fold_case(t) for t in _char_texts(characters, 'CHAR-NOT-GREATERP')]
    return lisptype.lisp_bool(all(texts[i] <= texts[i+1] for i in range(len(texts)-1)))

@_registry.cl_function('CHAR-NOT-LESSP')
def char_not_lessp(*characters):
    """Test characters are monotonically nonincreasing (case insensitive)."""
    texts = [_fold_case(t) for t in _char_texts(characters, 'CHAR-NOT-LESSP')]
    return lisptype.lisp_bool(all(texts[i] >= texts[i+1] for i in range(len(texts)-1)))

@_registry.cl_function('CHAR-INT')
def char_int(character):
    """Get character integer value."""
    return char_code(character)

@_registry.cl_function('CHAR-NAME')
def char_name(character):
    """Get character name (CLHS 13.1.7's names, spelled as ansi-test's
    `(string= (char-name #\\Space) "Space")` expects -- capitalized, not
    upper-case).

    Only a character with a name in `_CHAR_DISPLAY_NAME` gets one back: a
    made-up name for every other unprintable character (this used to
    return `f"CHAR-{code}"`) cannot round-trip through NAME-CHAR, and
    `char-name.1.fn` checks exactly that round trip for every character
    in the implementation's code range.
    """
    text = _require_char_text(character, 'CHAR-NAME')
    return _CHAR_DISPLAY_NAME.get(text)


# Case sensitive character comparisons
@_registry.cl_function('CHAR/=')  # case-sensitive inequality
def char_ne(*characters):
    """Test that no two characters are the same (case sensitive)."""
    texts = _char_texts(characters, 'CHAR/=')
    return lisptype.lisp_bool(len(set(texts)) == len(texts))


@_registry.cl_function('CHAR<')
def char_lt(*characters):
    """Test character less than (case sensitive)."""
    texts = _char_texts(characters, 'CHAR<')
    return lisptype.lisp_bool(all(texts[i] < texts[i+1] for i in range(len(texts)-1)))


@_registry.cl_function('CHAR<=')
def char_le(*characters):
    """Test character less than or equal (case sensitive)."""
    texts = _char_texts(characters, 'CHAR<=')
    return lisptype.lisp_bool(all(texts[i] <= texts[i+1] for i in range(len(texts)-1)))


def char_eq(*characters):  # alias (no decorator)
    return char_equal(*characters)


@_registry.cl_function('CHAR>')
def char_gt(*characters):
    """Test character greater than (case sensitive)."""
    texts = _char_texts(characters, 'CHAR>')
    return lisptype.lisp_bool(all(texts[i] > texts[i+1] for i in range(len(texts)-1)))


@_registry.cl_function('CHAR>=')
def char_ge(*characters):
    """Test character greater than or equal (case sensitive)."""
    texts = _char_texts(characters, 'CHAR>=')
    return lisptype.lisp_bool(all(texts[i] >= texts[i+1] for i in range(len(texts)-1)))


def char_less(*characters):  # alias
    return char_lt(*characters)


def char_greater(*characters):  # alias
    return char_gt(*characters)


def char_less_equal(*characters):  # alias
    return char_le(*characters)


def char_greater_equal(*characters):  # alias
    return char_ge(*characters)


@_registry.cl_function('CHARACTER')
def character(designator):
    """Convert a character designator to a CHARACTER (CLHS 5.3)."""
    if isinstance(designator, lisptype.Character):
        return designator
    if isinstance(designator, lisptype.LispString):
        designator = str(designator)

    if isinstance(designator, str):
        if len(designator) == 1:
            return lisptype.Character(designator)
        name_up = designator.upper()
        if name_up in _NAMED_CHAR_TEXT:
            return lisptype.Character(_NAMED_CHAR_TEXT[name_up])
    elif isinstance(designator, lisptype.LispSymbol):
        name_up = designator.name.upper()
        if len(designator.name) == 1:
            return lisptype.Character(designator.name)
        if name_up in _NAMED_CHAR_TEXT:
            return lisptype.Character(_NAMED_CHAR_TEXT[name_up])

    raise lisptype.LispTypeError(f"CHARACTER: cannot convert {designator} to character",
                                expected_type="CHARACTER-DESIGNATOR",
                                actual_value=designator)


@_registry.cl_function('CHARACTERP')
def characterp(object):
    """Test if object is a character.

    Missed the `Character` class entirely, so it disagreed with both TYPEP's
    CHARACTER branch and every function that constructs characters. It also
    returned a raw Python bool rather than T/NIL -- and a Python `False`
    reaching a Lisp conditional reads as *true* under `is_truthy`, so the
    negative answer was the dangerous one.
    """
    return lisptype.lisp_bool(
        isinstance(object, lisptype.Character)
        or (isinstance(object, str) and len(object) == 1)
    )


@_registry.cl_function('CODE-CHAR')
def code_char(code):
    """Convert code to character."""
    try:
        return lisptype.Character(chr(code))
    except ValueError:
        return None


@_registry.cl_function('DIGIT-CHAR')
def digit_char(weight, radix=10):
    """Convert digit weight to character."""
    if 0 <= weight < radix:
        if weight < 10:
            return lisptype.Character(str(weight))
        elif weight < 36:
            return lisptype.Character(chr(ord('A') + weight - 10))
    return None


@_registry.cl_function('DIGIT-CHAR-P')
def digit_char_p(character, radix=10):
    """Test if character is digit and return weight.

    The case fold must be guarded to a *single* character before the range
    test: `'ß'.upper()` is the two characters `'SS'`, and the plain
    string comparison `'A' <= 'SS' <= 'Z'` is TRUE (it compares the first
    characters), which then called `ord()` on a two-character string.
    """
    text = _char_text(character)
    if text is None:
        return None

    if '0' <= text <= '9':
        weight = ord(text) - ord('0')
    elif text.isalpha():
        upper = text.upper()
        if len(upper) != 1 or not ('A' <= upper <= 'Z'):
            return None
        weight = ord(upper) - ord('A') + 10
    else:
        return None

    return weight if weight < radix else None


@_registry.cl_function('GRAPHIC-CHAR-P')
def graphic_char_p(character):
    """Test if character is graphic."""
    text = _require_char_text(character, 'GRAPHIC-CHAR-P')
    return lisptype.lisp_bool(text.isprintable())


@_registry.cl_function('LOWER-CASE-P')
def lower_case_p(character):
    """Test if character is lowercase."""
    text = _require_char_text(character, 'LOWER-CASE-P')
    return lisptype.lisp_bool(text.islower())


@_registry.cl_function('UPPER-CASE-P')
def upper_case_p(character):
    """Test if character is uppercase."""
    text = _require_char_text(character, 'UPPER-CASE-P')
    return lisptype.lisp_bool(text.isupper())


@_registry.cl_function('NAME-CHAR')
def name_char(name):
    """Get character by name.

    `name` is a **string designator** (CLHS 13.1.7): a string, a symbol, or
    a character -- and the string representations of CLAUDE.md's array model
    count, so a *displaced or specialized character array* naming "Newline"
    resolves exactly like the `LispString` spelling of the same text does
    (`name-char.specialized.4` builds every etype × name combination and
    requires both readings to be EQL). The previous implementation accepted
    only `str`/`LispString`/symbol and answered NIL for the array shapes.
    """
    if isinstance(name, lisptype.Character):
        return name
    try:
        text = _string_designator(name)
    except lisptype.LispTypeError:
        return None
    if not isinstance(text, str):
        return None
    # The printer's spelling for a character with no name and no graphic
    # form is #\U+XXXX (printer.character_name); NAME-CHAR must invert it,
    # or print.char.7's `(eql c (name-char (subseq str 2)))` check fails for
    # every character the printer spells that way. CODE-CHAR's construction
    # is reused so the two agree on which codes denote characters (a code
    # outside the implementation's range names nothing).
    if len(text) > 2 and text[0] in 'uU' and text[1] == '+':
        digits = text[2:]
        if digits and all(c in '0123456789abcdefABCDEF' for c in digits):
            try:
                return lisptype.Character(chr(int(digits, 16)))
            except ValueError:
                return None
    try:
        return character(text)
    except lisptype.LispTypeError:
        return None


@_registry.cl_function('INT-CHAR')
def int_char(integer):
    """Convert integer to character."""
    try:
        return lisptype.Character(chr(integer))
    except ValueError:
        return None


@_registry.cl_function('STANDARD-CHAR-P')
def standard_char_p(character):
    """Test if character is standard."""
    text = _require_char_text(character, 'STANDARD-CHAR-P')

    # Standard characters include space, newline, and graphic characters
    # in the basic Latin alphabet
    if text == ' ' or text == '\n':
        return lisptype.T

    code = ord(text)
    result = (33 <= code <= 126)  # Printable ASCII
    return lisptype.lisp_bool(result)


# String functions related to characters
@_registry.cl_function('CHAR')
def char(string, index):
    """Get character at index in string."""
    if isinstance(string, (lisptype.LispString, str)):
        if 0 <= index < len(string):
            return _arrays.string_element(string, string[index])
        raise lisptype.LispError(f"CHAR: index {index} out of bounds for string of length {len(string)}")

    raise lisptype.LispTypeError("CHAR: first argument must be a string",
                                expected_type="STRING",
                                actual_value=string)


def schar(string, index):
    """Get character at index in simple string."""
    return char(string, index)


@_registry.cl_function('STRING')
def string_fn(designator):
    """Convert a string designator to a string (CLHS 16.2: a string,
    a character, or a symbol -- denoting its name).

    A *string* is returned **itself** (CLHS 16.2: "if the designator is a
    string, it is returned"), across every representation this
    implementation gives strings -- `str`, `LispString`, and the
    specialized character `LispArray` (plus the `(array nil 0)` shape
    `string.10`/`string.16` exercise) -- because `check-predicate` runs
    `(eq s (string s))` over `*universe*` and a rebuilt string would break
    the EQ. Only a character and a symbol are coerced to a fresh string.
    """
    if is_string(designator):
        return designator
    elif isinstance(designator, lisptype.Character):
        return lisptype.LispString(designator.char)
    elif isinstance(designator, lisptype.LispSymbol):
        # A symbol denotes its *name* (CLHS 16.2), not its printed
        # representation -- `str()` on a keyword includes the leading colon
        # for PRINC/PRIN1's benefit, so `(string :a)` fell through to the
        # catchall below and answered ":A" instead of "A".
        return lisptype.LispString(designator.name)
    elif isinstance(designator, (list, tuple)):
        return lisptype.LispString(''.join(str(x) for x in designator))
    else:
        return lisptype.LispString(str(designator))


def is_string(value):
    """CLHS 15.1: a string is a specialized array whose element type is a
    subtype of CHARACTER -- not just the two representations that happen to
    print like one.

    `STRINGP`/`SIMPLE-STRING-P` used to test only `isinstance(x, (str,
    LispString))`, which is right for a `LispString` and a plain Python
    `str` but blind to the third representation the array model (CLAUDE.md)
    already tracks: `(make-array n :element-type 'character)` is a
    `LispArray` with `element_type_of(...) is CHARACTER_TYPE`, correctly
    reported by `TYPE-OF`/`ARRAY-ELEMENT-TYPE` -- but invisible to an
    `isinstance` check, exactly the pattern plan.md Finding M names. This is
    not a corner case: ansi-test's own random-string generator (`auxiliary/
    string-aux.lsp`'s `make-random-string`) builds strings this way as often
    as it builds `LispString`s, and the harness's own `(assert (stringp
    ...))` was failing on its own fixture, independent of anything under
    test.

    An `(array nil (*))` counts too (`*.NIL-ARRAY.1`'s ``:nil-vectors-
    are-strings`` tests): NIL is a subtype of every type, CHARACTER
    included, so it satisfies "element type is a subtype of CHARACTER" even
    though it is not CHARACTER itself.
    """
    if isinstance(value, (str, lisptype.LispString)):
        return True
    if not (_arrays.is_array(value) and _arrays.array_rank_of(value) == 1):
        return False
    element_type = _arrays.element_type_of(value)
    return element_type is _arrays.CHARACTER_TYPE or element_type is _arrays.NIL_TYPE


@_registry.cl_function('STRINGP')
def stringp(object):
    """Test if object is a string."""
    return lisptype.lisp_bool(is_string(object))


@_registry.cl_function('SIMPLE-STRING-P')
def simple_string_p(object):
    """Test if object is a simple (non-adjustable, non-displaced, no
    fill-pointer) string."""
    return lisptype.lisp_bool(is_string(object) and _arrays.is_simple_array(object))


@_registry.cl_function('MAKE-STRING')
def make_string(size, *, initial_element=None, element_type=None):
    """Create a string of the given size.

    Args:
        size: Length of the string to create
        initial_element: Character to fill the string with (default is space)
        element_type: Element type (ignored, always CHARACTER)

    Returns:
        A string of length size filled with initial_element

    Examples:
        (make-string 5) => "     "
        (make-string 3 :initial-element #\\x) => "xxx"

    `initial-element` and `element-type` are **keyword-only**: MAKE-STRING's
    ANSI lambda list is `(make-string size &key initial-element
    element-type)`, and a plain defaulted positional parameter is
    indistinguishable from an `&optional` one to the argument checker --
    which is why `(make-string 10 :bad t)` and `(make-string 10 1 1)`
    bound their junk positionally instead of signalling the PROGRAM-ERROR
    CLHS 3.4.1.4 requires (`make-string.error.2`/`.5`).
    """
    if not isinstance(size, int) or size < 0:
        raise lisptype.LispTypeError("MAKE-STRING: size must be a non-negative integer",
                                    expected_type="(INTEGER 0 *)",
                                    actual_value=size)
    
    # Default initial element is space
    if initial_element is None or initial_element is lisptype.NIL:
        fill_char = ' '
    elif isinstance(initial_element, str) and len(initial_element) == 1:
        fill_char = initial_element
    elif isinstance(initial_element, int):
        # Character code - convert to character
        fill_char = chr(initial_element)
    elif isinstance(initial_element, lisptype.Character):
        # Character object
        fill_char = initial_element.char
    else:
        raise lisptype.LispTypeError("MAKE-STRING: initial-element must be a character",
                                    expected_type="CHARACTER",
                                    actual_value=initial_element)
    
    return lisptype.LispString(fill_char * size)


def _capitalize_range(read, write, start, end):
    """CLHS 16.4 STRING-CAPITALIZE over `[start, end)` via `read(i)`/`write(i, ch)`.

    A "word" is a maximal run of alphanumeric characters -- not just
    alphabetic ones, which is what the previous ladder tested. Only the
    word's *first* character is a candidate for uppercasing (and only if it
    is itself alphabetic; a leading digit still consumes the word's one
    capitalization slot, so `"1a"` stays `"1a"` and `"a1a"` becomes `"A1a"`,
    never `"A1A"`), every other alphabetic character in the word is
    lowercased, and a non-alphanumeric character is left untouched and
    starts a new word.
    """
    at_word_start = True
    for i in range(start, end):
        ch = read(i)
        if ch.isalnum():
            if at_word_start:
                if ch.isalpha():
                    write(i, char_upcase(lisptype.Character(ch)).char)
                at_word_start = False
            elif ch.isalpha():
                write(i, char_downcase(lisptype.Character(ch)).char)
        else:
            at_word_start = True


def _upcase_char_reader_writer(read, write, start, end):
    """CHAR-UPCASE-driven uppercasing over [start, end) -- the CLHS 16.4
    `:case :upcase` rule, which maps each character exactly the way
    CHAR-UPCASE does (a character with no single-character upper-case
    partner is left alone; `'ß'` stays one character)."""
    for i in range(start, end):
        write(i, char_upcase(lisptype.Character(read(i))).char)


def _downcase_char_reader_writer(read, write, start, end):
    """The CHAR-DOWNCASE-driven counterpart of `_upcase_char_reader_writer`."""
    for i in range(start, end):
        write(i, char_downcase(lisptype.Character(read(i))).char)


def _mutable_string_bounds(string, start, end, what):
    """Bounding indices for a destructive NSTRING-* argument.

    `len(string)` honors a fill pointer on every representation here
    (`LispString`, `LispArray`, plain `str`) -- see CLAUDE.md on
    `LispString`'s content stopping at its fill pointer.
    """
    from . import sequence_protocol as _seq
    return _seq.bounding_indices(len(string), start, end, what)


def _array_char_reader(string):
    def read(i):
        ch = _arrays.row_major_get(string, i)
        return ch.char if isinstance(ch, lisptype.Character) else ch
    return read


def _array_char_writer(string):
    def write(i, text):
        _arrays.row_major_set(string, i, lisptype.Character(text))
    return write


@_registry.cl_function('STRING-CAPITALIZE')
def string_capitalize(string, *, start=0, end=None):
    """CLHS 16.4: a fresh string, `string`'s designated text with each word capitalized."""
    from . import sequence_protocol as _seq
    text = _string_designator(string)
    start, end = _seq.bounding_indices(len(text), start, end, 'STRING-CAPITALIZE')
    result = list(text)
    _capitalize_range(lambda i: result[i], lambda i, c: result.__setitem__(i, c), start, end)
    return lisptype.LispString(''.join(result))


@_registry.cl_function('STRING-DOWNCASE')
def string_downcase(string, *, start=0, end=None):
    """CLHS 16.4: a fresh string, `string`'s designated text lowercased."""
    from . import sequence_protocol as _seq
    text = _string_designator(string)
    start, end = _seq.bounding_indices(len(text), start, end, 'STRING-DOWNCASE')
    result = list(text)
    _downcase_char_reader_writer(lambda i: result[i], lambda i, c: result.__setitem__(i, c), start, end)
    return lisptype.LispString(''.join(result))


@_registry.cl_function('STRING-UPCASE')
def string_upcase(string, *, start=0, end=None):
    """CLHS 16.4: a fresh string, `string`'s designated text uppercased."""
    from . import sequence_protocol as _seq
    text = _string_designator(string)
    start, end = _seq.bounding_indices(len(text), start, end, 'STRING-UPCASE')
    result = list(text)
    _upcase_char_reader_writer(lambda i: result[i], lambda i, c: result.__setitem__(i, c), start, end)
    return lisptype.LispString(''.join(result))


@_registry.cl_function('NSTRING-CAPITALIZE')
def nstring_capitalize(string, *, start=0, end=None):
    """CLHS 16.4: destructively capitalize `string` in place, returning it."""
    start, end = _mutable_string_bounds(string, start, end, 'NSTRING-CAPITALIZE')
    _capitalize_range(_array_char_reader(string), _array_char_writer(string), start, end)
    return string


@_registry.cl_function('NSTRING-DOWNCASE')
def nstring_downcase(string, *, start=0, end=None):
    """CLHS 16.4: destructively lowercase `string` in place, returning it."""
    start, end = _mutable_string_bounds(string, start, end, 'NSTRING-DOWNCASE')
    _downcase_char_reader_writer(_array_char_reader(string), _array_char_writer(string), start, end)
    return string


@_registry.cl_function('NSTRING-UPCASE')
def nstring_upcase(string, *, start=0, end=None):
    """CLHS 16.4: destructively uppercase `string` in place, returning it."""
    start, end = _mutable_string_bounds(string, start, end, 'NSTRING-UPCASE')
    _upcase_char_reader_writer(_array_char_reader(string), _array_char_writer(string), start, end)
    return string


def _string_designator(x):
    """Resolve `x` as a CLHS "string designator" to plain Python text.

    Delegates to `misc_packages._designator_to_string`, the one designator
    resolver (plan.md standing rule 3). This module used to carry its own
    copy that only handled the symbol and character cases, so a
    pipe-escaped symbol name (`'|abc|`, `STRING=.4`/`.5`) kept its literal
    pipes instead of being unescaped, and a zero-length
    `(make-array '(0) :element-type nil)` string stand-in
    (`*.NIL-ARRAY.1`) fell through to `str(x)` instead of resolving to "".
    """
    from .misc_packages import _designator_to_string
    return _designator_to_string(x)


def _string_relation(s1, s2, start1, end1, start2, end2, fold):
    """Classify the CLHS 16.4 ordering relation between two substrings.

    Returns ``(relation, index)``: `relation` is one of ``'lt'``, ``'eq'``,
    ``'gt'``, and `index` is CLHS's "mismatch index" -- a bounding index of
    `s1` -- which every comparator except STRING=/STRING-EQUAL must return
    on success instead of a bare boolean. `../ansi-test/auxiliary/
    string-aux.lsp`'s `my-string-compare` is the ANSI suite's own reference
    model for this (and what its randomized `random-string-comparison-tests`
    checks every comparator against): a shorter string that matches the
    longer one's prefix is "less", the mismatch index is always relative to
    `s1`, and two substrings that both run out at once are "equal" with the
    index reported as `end1`. `fold` case-folds each character pair, which
    is the entire difference between e.g. STRING-LESSP and STRING<.
    """
    i1, i2 = start1, start2
    while i1 < end1 and i2 < end2:
        c1, c2 = s1[i1], s2[i2]
        if fold:
            # The fold is per *character* and length-preserving -- the same
            # guarded single-character upper-case CHAR-EQUAL/CHAR-LESSP
            # apply. A raw `str.upper()` expands (`'ß'.upper()` is `'SS'`),
            # and a two-character "character" then orders unlike the char
            # predicates do, which `random-string-comparison-tests` catches
            # against the suite's own per-character reference model.
            c1, c2 = _fold_case(c1), _fold_case(c2)
        if c1 != c2:
            return ('lt', i1) if c1 < c2 else ('gt', i1)
        i1 += 1
        i2 += 1
    if i1 == end1 and i2 == end2:
        return ('eq', end1)
    if i1 == end1:
        return ('lt', end1)
    return ('gt', i1)


def _string_bounds(string1, string2, start1, end1, start2, end2):
    """Resolve both string designators and their CLHS bounding indices.

    `sequence_protocol.bounding_indices` is the one place `:end` NIL vs. an
    explicit integer is told apart (CLHS 17.1); the comparators used to test
    `end1 is None`, which is false for an explicitly-passed Lisp NIL and
    made `(string= s1 s2 :end1 nil)` slice with NIL as an index instead of
    treating it as "through the end" (`*.ORDER.2`/`.3`, `STRING=.13`/`.14`).
    """
    from . import sequence_protocol as _seq
    s1 = _string_designator(string1)
    s2 = _string_designator(string2)
    start1, end1 = _seq.bounding_indices(len(s1), start1, end1, 'STRING comparison')
    start2, end2 = _seq.bounding_indices(len(s2), start2, end2, 'STRING comparison')
    return s1, s2, start1, end1, start2, end2


@_registry.cl_function('STRING=')
def string_eq(string1, string2, *, start1=0, end1=None, start2=0, end2=None):
    """CLHS 16.4: true if the substrings' characters match (case sensitive)."""
    s1, s2, start1, end1, start2, end2 = _string_bounds(
        string1, string2, start1, end1, start2, end2)
    relation, _index = _string_relation(s1, s2, start1, end1, start2, end2, fold=False)
    return lisptype.lisp_bool(relation == 'eq')


@_registry.cl_function('STRING-EQUAL')
def string_equal(string1, string2, *, start1=0, end1=None, start2=0, end2=None):
    """CLHS 16.4: true if the substrings' characters match (case insensitive)."""
    s1, s2, start1, end1, start2, end2 = _string_bounds(
        string1, string2, start1, end1, start2, end2)
    relation, _index = _string_relation(s1, s2, start1, end1, start2, end2, fold=True)
    return lisptype.lisp_bool(relation == 'eq')


@_registry.cl_function('STRING<')
def string_lt(string1, string2, *, start1=0, end1=None, start2=0, end2=None):
    """CLHS 16.4: the mismatch index if string1 < string2 (case sensitive), else NIL."""
    s1, s2, start1, end1, start2, end2 = _string_bounds(
        string1, string2, start1, end1, start2, end2)
    relation, index = _string_relation(s1, s2, start1, end1, start2, end2, fold=False)
    return index if relation == 'lt' else lisptype.NIL


@_registry.cl_function('STRING>')
def string_gt(string1, string2, *, start1=0, end1=None, start2=0, end2=None):
    """CLHS 16.4: the mismatch index if string1 > string2 (case sensitive), else NIL."""
    s1, s2, start1, end1, start2, end2 = _string_bounds(
        string1, string2, start1, end1, start2, end2)
    relation, index = _string_relation(s1, s2, start1, end1, start2, end2, fold=False)
    return index if relation == 'gt' else lisptype.NIL


@_registry.cl_function('STRING<=')
def string_le(string1, string2, *, start1=0, end1=None, start2=0, end2=None):
    """CLHS 16.4: the mismatch index if string1 <= string2 (case sensitive), else NIL."""
    s1, s2, start1, end1, start2, end2 = _string_bounds(
        string1, string2, start1, end1, start2, end2)
    relation, index = _string_relation(s1, s2, start1, end1, start2, end2, fold=False)
    return index if relation in ('lt', 'eq') else lisptype.NIL


@_registry.cl_function('STRING>=')
def string_ge(string1, string2, *, start1=0, end1=None, start2=0, end2=None):
    """CLHS 16.4: the mismatch index if string1 >= string2 (case sensitive), else NIL."""
    s1, s2, start1, end1, start2, end2 = _string_bounds(
        string1, string2, start1, end1, start2, end2)
    relation, index = _string_relation(s1, s2, start1, end1, start2, end2, fold=False)
    return index if relation in ('gt', 'eq') else lisptype.NIL


@_registry.cl_function('STRING/=')
def string_ne(string1, string2, *, start1=0, end1=None, start2=0, end2=None):
    """CLHS 16.4: the mismatch index if the substrings differ (case sensitive), else NIL."""
    s1, s2, start1, end1, start2, end2 = _string_bounds(
        string1, string2, start1, end1, start2, end2)
    relation, index = _string_relation(s1, s2, start1, end1, start2, end2, fold=False)
    return index if relation != 'eq' else lisptype.NIL


@_registry.cl_function('STRING-LESSP')
def string_lessp(string1, string2, *, start1=0, end1=None, start2=0, end2=None):
    """CLHS 16.4: the mismatch index if string1 < string2 (case insensitive), else NIL."""
    s1, s2, start1, end1, start2, end2 = _string_bounds(
        string1, string2, start1, end1, start2, end2)
    relation, index = _string_relation(s1, s2, start1, end1, start2, end2, fold=True)
    return index if relation == 'lt' else lisptype.NIL


@_registry.cl_function('STRING-GREATERP')
def string_greaterp(string1, string2, *, start1=0, end1=None, start2=0, end2=None):
    """CLHS 16.4: the mismatch index if string1 > string2 (case insensitive), else NIL."""
    s1, s2, start1, end1, start2, end2 = _string_bounds(
        string1, string2, start1, end1, start2, end2)
    relation, index = _string_relation(s1, s2, start1, end1, start2, end2, fold=True)
    return index if relation == 'gt' else lisptype.NIL


@_registry.cl_function('STRING-NOT-GREATERP')
def string_not_greaterp(string1, string2, *, start1=0, end1=None, start2=0, end2=None):
    """CLHS 16.4: the mismatch index if string1 <= string2 (case insensitive), else NIL."""
    s1, s2, start1, end1, start2, end2 = _string_bounds(
        string1, string2, start1, end1, start2, end2)
    relation, index = _string_relation(s1, s2, start1, end1, start2, end2, fold=True)
    return index if relation in ('lt', 'eq') else lisptype.NIL


@_registry.cl_function('STRING-NOT-LESSP')
def string_not_lessp(string1, string2, *, start1=0, end1=None, start2=0, end2=None):
    """CLHS 16.4: the mismatch index if string1 >= string2 (case insensitive), else NIL."""
    s1, s2, start1, end1, start2, end2 = _string_bounds(
        string1, string2, start1, end1, start2, end2)
    relation, index = _string_relation(s1, s2, start1, end1, start2, end2, fold=True)
    return index if relation in ('gt', 'eq') else lisptype.NIL


@_registry.cl_function('STRING-NOT-EQUAL')
def string_not_equal(string1, string2, *, start1=0, end1=None, start2=0, end2=None):
    """CLHS 16.4: the mismatch index if the substrings differ (case insensitive), else NIL."""
    s1, s2, start1, end1, start2, end2 = _string_bounds(
        string1, string2, start1, end1, start2, end2)
    relation, index = _string_relation(s1, s2, start1, end1, start2, end2, fold=True)
    return index if relation != 'eq' else lisptype.NIL


def _char_bag(character_bag, what):
    """CLHS 16.4 `character-bag`: a sequence of characters, resolved to a
    Python `set` of one-character strings via `sequence_protocol.seq_elements`
    -- the one element-access path for a CLHS 17 sequence, so a bag spelled
    as a string, a list, a general vector or a specialized character array
    are all accepted the same way the trim functions' own tests exercise."""
    from . import sequence_protocol as _seq
    chars = set()
    for e in _seq.seq_elements(character_bag, what):
        if isinstance(e, lisptype.Character):
            chars.add(e.char)
        elif isinstance(e, str) and len(e) == 1:
            chars.add(e)
        else:
            raise lisptype.LispTypeError(
                f"{what}: {e!r} is not a character", expected_type="CHARACTER",
                actual_value=e)
    return chars


@_registry.cl_function('STRING-LEFT-TRIM')
def string_left_trim(character_bag, string):
    """CLHS 16.4: a fresh string with leading `character_bag` members removed."""
    text = _string_designator(string)
    chars = _char_bag(character_bag, 'STRING-LEFT-TRIM')
    i = 0
    while i < len(text) and text[i] in chars:
        i += 1
    return lisptype.LispString(text[i:])


@_registry.cl_function('STRING-RIGHT-TRIM')
def string_right_trim(character_bag, string):
    """CLHS 16.4: a fresh string with trailing `character_bag` members removed."""
    text = _string_designator(string)
    chars = _char_bag(character_bag, 'STRING-RIGHT-TRIM')
    j = len(text)
    while j > 0 and text[j - 1] in chars:
        j -= 1
    return lisptype.LispString(text[:j])


@_registry.cl_function('STRING-TRIM')
def string_trim(character_bag, string):
    """CLHS 16.4: a fresh string with leading and trailing `character_bag` members removed."""
    return string_left_trim(character_bag, string_right_trim(character_bag, string))


def _parse_integer_digit_weight(ch, radix):
    """The digit weight of `ch` in `radix`, or None if it names no digit
    in that radix -- the same rule `DIGIT-CHAR-P` applies, restated here
    over a plain Python character instead of a Lisp one. The upper-case
    fold is guarded to one character the way DIGIT-CHAR-P's is, for the
    same `'ß'.upper()` reason.
    """
    if '0' <= ch <= '9':
        weight = ord(ch) - ord('0')
    elif ch.isalpha():
        upper = ch.upper()
        if len(upper) != 1 or not ('A' <= upper <= 'Z'):
            return None
        weight = ord(upper) - ord('A') + 10
    else:
        return None
    return weight if weight < radix else None


@_registry.cl_function('PARSE-INTEGER')
def parse_integer(string, *, start=0, end=None, radix=10, junk_allowed=None):
    """CLHS PARSE-INTEGER: parse a signed integer, in `radix`, out of the
    bounded substring, skipping surrounding whitespace.

    Returns two values -- the integer (or NIL if :junk-allowed is true and
    none could be parsed) and the index where parsing stopped. Without
    :junk-allowed, anything left in the substring that is not trailing
    whitespace is a PARSE-ERROR rather than a value silently computed from
    a truncated prefix -- the previous implementation was `int(string.strip())`,
    which ignored :start/:end/:radix/:junk-allowed entirely, returned Python
    `None` (not a Lisp value) on failure instead of signaling, and had no
    second return value at all.
    """
    from .comparison import _string_characters
    from .sequence_protocol import bounding_indices
    from .evaluation_conditions import signal_error_object

    text = _string_characters(string)
    if text is None:
        raise lisptype.LispTypeError(
            "PARSE-INTEGER: argument is not a string",
            expected_type="STRING", actual_value=string)
    length = len(text)
    start, end = bounding_indices(length, start, end, 'PARSE-INTEGER')
    radix = int(radix)
    junk_allowed = lisptype.is_truthy(junk_allowed)

    i = start
    while i < end and text[i].isspace():
        i += 1
    ws_end = i

    sign = -1 if (i < end and text[i] == '-') else 1
    if i < end and text[i] in '+-':
        i += 1

    digits_start = i
    value = 0
    while i < end:
        weight = _parse_integer_digit_weight(text[i], radix)
        if weight is None:
            break
        value = value * radix + weight
        i += 1

    if i == digits_start:
        # No digits were parsed -- the sign, if any, was never confirmed.
        if junk_allowed:
            return lisptype.MultipleValues(lisptype.NIL, ws_end)
        return signal_error_object(lisptype.ParseError(
            f"PARSE-INTEGER: no integer found in {text[start:end]!r}"))

    if junk_allowed:
        return lisptype.MultipleValues(sign * value, i)

    j = i
    while j < end and text[j].isspace():
        j += 1
    if j != end:
        return signal_error_object(lisptype.ParseError(
            f"PARSE-INTEGER: junk in string {text[start:end]!r}"))
    return lisptype.MultipleValues(sign * value, end)
