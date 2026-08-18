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
    """Test if character is alphanumeric."""
    text = _require_char_text(character, 'ALPHANUMERICP')
    return lisptype.lisp_bool(text.isalnum())


@_registry.cl_function('BOTH-CASE-P')
def both_case_p(character):
    """Test if character has both cases."""
    text = _require_char_text(character, 'BOTH-CASE-P')
    return lisptype.lisp_bool(text.upper() != text.lower())


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
    """Convert character to lowercase."""
    text = _char_text(character)
    if text is None:
        raise lisptype.LispTypeError("CHAR-DOWNCASE: argument must be a character",
                                    expected_type="CHARACTER", actual_value=character)
    return lisptype.Character(_single_char_case_map(text, text.lower()))


@_registry.cl_function('CHAR-UPCASE')
def char_upcase(character):
    """Convert character to uppercase."""
    text = _char_text(character)
    if text is None:
        raise lisptype.LispTypeError("CHAR-UPCASE: argument must be a character",
                                    expected_type="CHARACTER", actual_value=character)
    return lisptype.Character(_single_char_case_map(text, text.upper()))


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
    """Test if character is digit and return weight."""
    text = _char_text(character)
    if text is None:
        return None

    if '0' <= text <= '9':
        weight = ord(text) - ord('0')
    elif 'A' <= text.upper() <= 'Z':
        weight = ord(text.upper()) - ord('A') + 10
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
    """Get character by name."""
    if isinstance(name, (str, lisptype.LispString, lisptype.LispSymbol)):
        try:
            return character(name)
        except lisptype.LispTypeError:
            return None
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
    a character, or a symbol -- denoting its name)."""
    if isinstance(designator, lisptype.LispString):
        return designator  # Already a mutable string
    elif isinstance(designator, str):
        return lisptype.LispString(designator)
    elif isinstance(designator, lisptype.Character):
        return lisptype.LispString(designator.char)
    elif isinstance(designator, (list, tuple)):
        return lisptype.LispString(''.join(str(x) for x in designator))
    else:
        return lisptype.LispString(str(designator))


@_registry.cl_function('STRINGP')
def stringp(object):
    """Test if object is a string."""
    return lisptype.lisp_bool(isinstance(object, (str, lisptype.LispString)))


@_registry.cl_function('SIMPLE-STRING-P')
def simple_string_p(object):
    """Test if object is a simple string."""
    return lisptype.lisp_bool(isinstance(object, (str, lisptype.LispString)))


@_registry.cl_function('MAKE-STRING')
def make_string(size, initial_element=None, element_type=None):
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


@_registry.cl_function('STRING-CAPITALIZE')
def string_capitalize(string, start=0, end=None):
    """Capitalize string."""
    if end is None:
        end = len(string)
    
    result = list(string)
    capitalize_next = True
    
    for i in range(start, min(end, len(string))):
        if result[i].isalpha():
            if capitalize_next:
                result[i] = result[i].upper()
                capitalize_next = False
            else:
                result[i] = result[i].lower()
        else:
            capitalize_next = True
    
    return ''.join(result)


@_registry.cl_function('STRING-DOWNCASE')
def string_downcase(string, start=0, end=None):
    """Convert string to lowercase."""
    if end is None:
        end = len(string)
    
    result = list(string)
    for i in range(start, min(end, len(string))):
        result[i] = result[i].lower()
    
    return ''.join(result)


@_registry.cl_function('STRING-UPCASE')
def string_upcase(string, start=0, end=None):
    """Convert string to uppercase."""
    if end is None:
        end = len(string)
    
    result = list(string)
    for i in range(start, min(end, len(string))):
        result[i] = result[i].upper()
    
    return ''.join(result)


@_registry.cl_function('NSTRING-CAPITALIZE')
def nstring_capitalize(string, start=0, end=None):
    """Destructively capitalize string."""
    return string_capitalize(string, start, end)


@_registry.cl_function('NSTRING-DOWNCASE')
def nstring_downcase(string, start=0, end=None):
    """Destructively convert to lowercase."""
    return string_downcase(string, start, end)


@_registry.cl_function('NSTRING-UPCASE')
def nstring_upcase(string, start=0, end=None):
    """Destructively convert to uppercase."""
    return string_upcase(string, start, end)


@_registry.cl_function('STRING-EQUAL')
def string_equal(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string equality (case insensitive)."""
    string1 = _string_designator(string1)
    string2 = _string_designator(string2)
    if end1 is None:
        end1 = len(string1)
    if end2 is None:
        end2 = len(string2)

    substr1 = string1[start1:end1].upper()
    substr2 = string2[start2:end2].upper()

    return substr1 == substr2


@_registry.cl_function('STRING-NOT-EQUAL')
def string_not_equal(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string inequality (case insensitive)."""
    return not string_equal(string1, string2, start1, end1, start2, end2)


@_registry.cl_function('STRING-LESSP')
def string_lessp(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string less than (case insensitive)."""
    if end1 is None:
        end1 = len(string1)
    if end2 is None:
        end2 = len(string2)
    
    substr1 = string1[start1:end1].upper()
    substr2 = string2[start2:end2].upper()
    
    return substr1 < substr2


@_registry.cl_function('STRING-GREATERP')
def string_greaterp(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string greater than (case insensitive)."""
    if end1 is None:
        end1 = len(string1)
    if end2 is None:
        end2 = len(string2)
    
    substr1 = string1[start1:end1].upper()
    substr2 = string2[start2:end2].upper()
    
    return substr1 > substr2


@_registry.cl_function('STRING-NOT-GREATERP')
def string_not_greaterp(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string not greater than (case insensitive)."""
    return not string_greaterp(string1, string2, start1, end1, start2, end2)


@_registry.cl_function('STRING-NOT-LESSP')
def string_not_lessp(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string not less than (case insensitive)."""
    return not string_lessp(string1, string2, start1, end1, start2, end2)


def _string_designator(x):
    """Coerce a string designator (string, symbol, or character) to a Python str.

    ANSI STRING=/STRING</STRING> etc. accept any string designator, not just
    literal strings -- e.g. (STRING= "FOO" 'FOO) must be true.
    """
    if isinstance(x, str):
        return x
    if isinstance(x, lisptype.LispSymbol):
        return x.name
    if isinstance(x, lisptype.Character):
        return x.char
    return str(x)


@_registry.cl_function('STRING<')
def string_lt(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string less than (case sensitive)."""
    string1 = _string_designator(string1)
    string2 = _string_designator(string2)
    if end1 is None:
        end1 = len(string1)
    if end2 is None:
        end2 = len(string2)

    return string1[start1:end1] < string2[start2:end2]


@_registry.cl_function('STRING<=')
def string_le(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string less than or equal (case sensitive)."""
    string1 = _string_designator(string1)
    string2 = _string_designator(string2)
    if end1 is None:
        end1 = len(string1)
    if end2 is None:
        end2 = len(string2)

    return string1[start1:end1] <= string2[start2:end2]


@_registry.cl_function('STRING=')
def string_eq(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string equality (case sensitive)."""
    string1 = _string_designator(string1)
    string2 = _string_designator(string2)
    if end1 is None:
        end1 = len(string1)
    if end2 is None:
        end2 = len(string2)

    return string1[start1:end1] == string2[start2:end2]


@_registry.cl_function('STRING/=')
def string_ne(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string inequality (case sensitive)."""
    return not string_eq(string1, string2, start1, end1, start2, end2)


@_registry.cl_function('STRING>')
def string_gt(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string greater than (case sensitive)."""
    string1 = _string_designator(string1)
    string2 = _string_designator(string2)
    if end1 is None:
        end1 = len(string1)
    if end2 is None:
        end2 = len(string2)

    return string1[start1:end1] > string2[start2:end2]


@_registry.cl_function('STRING>=')
def string_ge(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string greater than or equal (case sensitive)."""
    string1 = _string_designator(string1)
    string2 = _string_designator(string2)
    if end1 is None:
        end1 = len(string1)
    if end2 is None:
        end2 = len(string2)

    return string1[start1:end1] >= string2[start2:end2]


@_registry.cl_function('STRING-LEFT-TRIM')
def string_left_trim(character_bag, string):
    """Trim characters from left of string."""
    if isinstance(character_bag, str):
        char_set = set(character_bag)
    else:
        char_set = set(character_bag)
    
    for i, char in enumerate(string):
        if char not in char_set:
            return string[i:]
    
    return ""


@_registry.cl_function('STRING-RIGHT-TRIM')
def string_right_trim(character_bag, string):
    """Trim characters from right of string."""
    if isinstance(character_bag, str):
        char_set = set(character_bag)
    else:
        char_set = set(character_bag)
    
    for i in range(len(string) - 1, -1, -1):
        if string[i] not in char_set:
            return string[:i+1]
    
    return ""


@_registry.cl_function('STRING-TRIM')
def string_trim(character_bag, string):
    """Trim characters from both ends of string."""
    return string_left_trim(character_bag, string_right_trim(character_bag, string))


@_registry.cl_function('PARSE-INTEGER')
def parse_integer(string, **kwargs):
    """Parse integer from string."""
    try:
        return int(string.strip())
    except ValueError:
        return None
