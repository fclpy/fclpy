"""Character operations - character predicates, manipulation, and comparison."""

import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry


@_registry.cl_function('ALPHA-CHAR-P')
def alpha_char_p(character):
    """Test if character is alphabetic."""
    result = isinstance(character, str) and len(character) == 1 and character.isalpha()
    return lisptype.lisp_bool(result)


@_registry.cl_function('ALPHANUMERICP')
def alphanumericp(character):
    """Test if character is alphanumeric."""
    result = isinstance(character, str) and len(character) == 1 and character.isalnum()
    return lisptype.lisp_bool(result)


@_registry.cl_function('BOTH-CASE-P')
def both_case_p(character):
    """Test if character has both cases."""
    if not isinstance(character, str) or len(character) != 1:
        return lisptype.NIL
    result = character.upper() != character.lower()
    return lisptype.lisp_bool(result)


@_registry.cl_function('CHAR-CODE')
def char_code(character):
    """Get character code."""
    if isinstance(character, str) and len(character) == 1:
        return ord(character)
    raise lisptype.LispTypeError("CHAR-CODE: argument must be a character", 
                                expected_type="CHARACTER", 
                                actual_value=character)


@_registry.cl_function('CHAR-DOWNCASE')
def char_downcase(character):
    """Convert character to lowercase."""
    if isinstance(character, str) and len(character) == 1:
        return character.lower()
    return character


@_registry.cl_function('CHAR-UPCASE')
def char_upcase(character):
    """Convert character to uppercase."""
    if isinstance(character, str) and len(character) == 1:
        return character.upper()
    return character


@_registry.cl_function('CHAR=')
def char_equal(*characters):
    """Test character equality (case sensitive)."""
    if len(characters) < 2:
        return lisptype.T
    result = all(c == characters[0] for c in characters[1:])
    return lisptype.lisp_bool(result)


@_registry.cl_function('CHAR-EQUAL')
def char_equal_ignore_case(*characters):
    """Test character equality (case insensitive)."""
    if len(characters) < 2:
        return lisptype.T
    chars = [c.upper() if isinstance(c, str) else c for c in characters]
    return lisptype.lisp_bool(all(c == chars[0] for c in chars[1:]))

@_registry.cl_function('CHAR-GREATERP')
def char_greaterp(*characters):
    """Test character greater than (case insensitive)."""
    if len(characters) < 2:
        return lisptype.T
    chars = [c.upper() if isinstance(c, str) else c for c in characters]
    return lisptype.lisp_bool(all(chars[i] > chars[i+1] for i in range(len(chars)-1)))

@_registry.cl_function('CHAR-LESSP')
def char_lessp(*characters):
    """Test character less than (case insensitive)."""
    if len(characters) < 2:
        return lisptype.T
    chars = [c.upper() if isinstance(c, str) else c for c in characters]
    return lisptype.lisp_bool(all(chars[i] < chars[i+1] for i in range(len(chars)-1)))

def char_not_equal(*characters):
    """Internal helper: inequality (case sensitive)."""
    return lisptype.lisp_bool(not (char_equal(*characters)) is lisptype.T)

@_registry.cl_function('CHAR-NOT-EQUAL')
def char_not_equal_ignore_case(*characters):
    """Test character inequality (case insensitive)."""
    return lisptype.lisp_bool(char_equal_ignore_case(*characters) is lisptype.NIL)

@_registry.cl_function('CHAR-NOT-GREATERP')
def char_not_greaterp(*characters):
    """Test character not greater than (case insensitive)."""
    return lisptype.lisp_bool(char_greaterp(*characters) is lisptype.NIL)

@_registry.cl_function('CHAR-NOT-LESSP')
def char_not_lessp(*characters):
    """Test character not less than (case insensitive)."""
    return lisptype.lisp_bool(char_lessp(*characters) is lisptype.NIL)

@_registry.cl_function('CHAR-INT')
def char_int(character):
    """Get character integer value."""
    return char_code(character)

@_registry.cl_function('CHAR-NAME')
def char_name(character):
    """Get character name."""
    if isinstance(character, str) and len(character) == 1:
        if character == ' ':
            return "SPACE"
        elif character == '\n':
            return "NEWLINE"
        elif character == '\t':
            return "TAB"
        elif character == '\r':
            return "RETURN"
        elif character == '\f':
            return "PAGE"
        elif character == '\b':
            return "BACKSPACE"
        elif character.isprintable():
            return None  # Printable characters don't have names
        else:
            return f"CHAR-{ord(character)}"
    return None


# Case sensitive character comparisons
@_registry.cl_function('CHAR/=')  # case-sensitive inequality
def char_ne(*characters):
    """Test character inequality (case sensitive)."""
    if len(characters) < 2:
        return lisptype.NIL
    result = not all(c == characters[0] for c in characters[1:])
    return lisptype.lisp_bool(result)


@_registry.cl_function('CHAR<')
def char_lt(*characters):
    """Test character less than (case sensitive)."""
    if len(characters) < 2:
        return lisptype.T
    result = all(characters[i] < characters[i+1] for i in range(len(characters)-1))
    return lisptype.lisp_bool(result)


@_registry.cl_function('CHAR<=')
def char_le(*characters):
    """Test character less than or equal (case sensitive)."""
    if len(characters) < 2:
        return lisptype.T
    result = all(characters[i] <= characters[i+1] for i in range(len(characters)-1))
    return lisptype.lisp_bool(result)


def char_eq(*characters):  # alias (no decorator)
    return char_equal(*characters)


@_registry.cl_function('CHAR>')
def char_gt(*characters):
    """Test character greater than (case sensitive)."""
    if len(characters) < 2:
        return lisptype.T
    result = all(characters[i] > characters[i+1] for i in range(len(characters)-1))
    return lisptype.lisp_bool(result)


@_registry.cl_function('CHAR>=')
def char_ge(*characters):
    """Test character greater than or equal (case sensitive)."""
    if len(characters) < 2:
        return lisptype.T
    result = all(characters[i] >= characters[i+1] for i in range(len(characters)-1))
    return lisptype.lisp_bool(result)


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
    """Convert to character."""
    if isinstance(designator, str):
        if len(designator) == 1:
            return designator
        else:
            # Try to convert from character name
            name_up = designator.upper()
            if name_up == "SPACE":
                return " "
            elif name_up == "NEWLINE":
                return "\n"
            elif name_up == "TAB":
                return "\t"
            elif name_up == "RETURN":
                return "\r"
            elif name_up == "PAGE":
                return "\f"
            elif name_up == "BACKSPACE":
                return "\b"
    elif isinstance(designator, int):
        return chr(designator)
    
    raise lisptype.LispTypeError(f"CHARACTER: cannot convert {designator} to character",
                                expected_type="CHARACTER-DESIGNATOR",
                                actual_value=designator)


@_registry.cl_function('CHARACTERP')
def characterp(object):
    """Test if object is a character."""
    return isinstance(object, str) and len(object) == 1


@_registry.cl_function('CODE-CHAR')
def code_char(code):
    """Convert code to character."""
    try:
        return chr(code)
    except ValueError:
        return None


@_registry.cl_function('DIGIT-CHAR')
def digit_char(weight, radix=10):
    """Convert digit weight to character."""
    if 0 <= weight < radix:
        if weight < 10:
            return str(weight)
        elif weight < 36:
            return chr(ord('A') + weight - 10)
    return None


@_registry.cl_function('DIGIT-CHAR-P')
def digit_char_p(character, radix=10):
    """Test if character is digit and return weight."""
    if not isinstance(character, str) or len(character) != 1:
        return None
    
    if '0' <= character <= '9':
        weight = ord(character) - ord('0')
    elif 'A' <= character.upper() <= 'Z':
        weight = ord(character.upper()) - ord('A') + 10
    else:
        return None
    
    return weight if weight < radix else None


@_registry.cl_function('GRAPHIC-CHAR-P')
def graphic_char_p(character):
    """Test if character is graphic."""
    result = isinstance(character, str) and len(character) == 1 and character.isprintable()
    return lisptype.lisp_bool(result)


@_registry.cl_function('LOWER-CASE-P')
def lower_case_p(character):
    """Test if character is lowercase."""
    result = isinstance(character, str) and len(character) == 1 and character.islower()
    return lisptype.lisp_bool(result)


@_registry.cl_function('UPPER-CASE-P')
def upper_case_p(character):
    """Test if character is uppercase."""
    result = isinstance(character, str) and len(character) == 1 and character.isupper()
    return lisptype.lisp_bool(result)


@_registry.cl_function('NAME-CHAR')
def name_char(name):
    """Get character by name."""
    if isinstance(name, str):
        return character(name)
    return None


@_registry.cl_function('INT-CHAR')
def int_char(integer):
    """Convert integer to character."""
    try:
        return chr(integer)
    except ValueError:
        return None


@_registry.cl_function('STANDARD-CHAR-P')
def standard_char_p(character):
    """Test if character is standard."""
    if not isinstance(character, str) or len(character) != 1:
        return lisptype.NIL
    
    # Standard characters include space, newline, and graphic characters
    # in the basic Latin alphabet
    if character == ' ' or character == '\n':
        return lisptype.T
    
    code = ord(character)
    result = (33 <= code <= 126)  # Printable ASCII
    return lisptype.lisp_bool(result)


# String functions related to characters
@_registry.cl_function('CHAR')
def char(string, index):
    """Get character at index in string."""
    if isinstance(string, str) and 0 <= index < len(string):
        return string[index]
    
    if not isinstance(string, str):
        raise lisptype.LispTypeError("CHAR: first argument must be a string",
                                    expected_type="STRING",
                                    actual_value=string)
    else:
        raise lisptype.LispError(f"CHAR: index {index} out of bounds for string of length {len(string)}")


def schar(string, index):
    """Get character at index in simple string."""
    return char(string, index)


@_registry.cl_function('STRING')
def string_fn(designator):
    """Convert to string."""
    if isinstance(designator, str):
        return designator
    elif isinstance(designator, (list, tuple)):
        return ''.join(str(x) for x in designator)
    else:
        return str(designator)


@_registry.cl_function('STRINGP')
def stringp(object):
    """Test if object is a string."""
    return lisptype.lisp_bool(isinstance(object, str))


@_registry.cl_function('SIMPLE-STRING-P')
def simple_string_p(object):
    """Test if object is a simple string."""
    return lisptype.lisp_bool(isinstance(object, str))


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


@_registry.cl_function('STRING<')
def string_lt(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string less than (case sensitive)."""
    if end1 is None:
        end1 = len(string1)
    if end2 is None:
        end2 = len(string2)
    
    return string1[start1:end1] < string2[start2:end2]


@_registry.cl_function('STRING<=')
def string_le(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string less than or equal (case sensitive)."""
    if end1 is None:
        end1 = len(string1)
    if end2 is None:
        end2 = len(string2)
    
    return string1[start1:end1] <= string2[start2:end2]


@_registry.cl_function('STRING=')
def string_eq(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string equality (case sensitive)."""
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
    if end1 is None:
        end1 = len(string1)
    if end2 is None:
        end2 = len(string2)
    
    return string1[start1:end1] > string2[start2:end2]


@_registry.cl_function('STRING>=')
def string_ge(string1, string2, start1=0, end1=None, start2=0, end2=None):
    """Test string greater than or equal (case sensitive)."""
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


# Additional string operations
@_registry.cl_function('STRING=')
def string_equal_fn(*strings):
    """Test string equality (case sensitive)."""
    if len(strings) < 2:
        return lisptype.T
    result = all(s == strings[0] for s in strings[1:])
    return lisptype.lisp_bool(result)


@_registry.cl_function('STRING<')
def string_less(*strings):
    """Test string less than (case sensitive)."""
    if len(strings) < 2:
        return lisptype.T
    result = all(strings[i] < strings[i+1] for i in range(len(strings)-1))
    return lisptype.lisp_bool(result)


@_registry.cl_function('STRING>')
def string_greater(*strings):
    """Test string greater than (case sensitive)."""
    if len(strings) < 2:
        return lisptype.T
    result = all(strings[i] > strings[i+1] for i in range(len(strings)-1))
    return lisptype.lisp_bool(result)


@_registry.cl_function('STRING<=')
def string_less_equal(*strings):
    """Test string less than or equal (case sensitive)."""
    if len(strings) < 2:
        return lisptype.T
    result = all(strings[i] <= strings[i+1] for i in range(len(strings)-1))
    return lisptype.lisp_bool(result)


@_registry.cl_function('STRING>=')
def string_greater_equal(*strings):
    """Test string greater than or equal (case sensitive)."""
    if len(strings) < 2:
        return lisptype.T
    result = all(strings[i] >= strings[i+1] for i in range(len(strings)-1))
    return lisptype.lisp_bool(result)


@_registry.cl_function('STRING-EQUAL')
def string_equal_ignore_case(*strings):
    """Test string equality (case insensitive)."""
    if len(strings) < 2:
        return lisptype.T
    strings_upper = [s.upper() for s in strings]
    result = all(s == strings_upper[0] for s in strings_upper[1:])
    return lisptype.lisp_bool(result)


@_registry.cl_function('STRING-NOT-EQUAL')
def string_not_equal_ignore_case(*strings):
    """Test string inequality (case insensitive)."""
    result = string_equal_ignore_case(*strings)
    return lisptype.T if result == lisptype.NIL else lisptype.NIL


@_registry.cl_function('PARSE-INTEGER')
def parse_integer(string, **kwargs):
    """Parse integer from string."""
    try:
        return int(string.strip())
    except ValueError:
        return None
