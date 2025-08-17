"""Core Lisp data structure functions - cons cells, lists, and basic accessors."""

import fclpy.lisptype as lisptype


def car(seq):
    """Get the first element of a cons cell or sequence."""
    if type(seq) is lisptype.lispCons:
        return seq.car
    if type(seq) is tuple:
        return seq[0]
    return seq


def cdr(seq):
    """Get the rest of a cons cell or sequence."""
    if type(seq) is lisptype.lispCons:
        return seq.cdr
    if type(seq) is tuple:
        return seq[1:]
    return seq


def cons(x, seq):
    """Returns a new cons cell where x is the first element and seq is the rest."""
    return lisptype.lispCons(x, seq)


def consp(obj):
    """Test if object is a cons cell."""
    return type(obj) is lisptype.lispCons


def atom(obj):
    """Test if object is an atom (not a cons cell)."""
    return type(obj) is not lisptype.lispCons


def acons(x, v, seq):
    """Creates a fresh cons, the cdr of which is alist and the car of which is 
    another fresh cons, the car of which is key and the cdr of which is 
    datum."""
    return lisptype.lispCons(lisptype.lispCons(x, v), seq)


def listp(obj):
    """Test if object is a list (either nil or a cons cell)."""
    return obj is None or type(obj) is lisptype.lispCons


def symbolp(obj):
    """Test if object is a symbol."""
    return type(obj) is lisptype.LispSymbol


def keywordp(obj):
    """Test if object is a keyword."""
    return type(obj) is lisptype.lispKeyword


def hash_table_p(obj):
    """Test if object is a hash table."""
    return isinstance(obj, dict)


def packagep(obj):
    """Test if object is a package."""
    return isinstance(obj, lisptype.Package)


def find_package_fn(name):
    """Find package by name or nickname."""
    return lisptype.find_package(name)


def make_package_fn(name, nicknames=None, use_list=None):
    """Create a new package."""
    return lisptype.make_package(name, nicknames or [], use_list or [])


def array_has_fill_pointer_p(array):
    """Test if array has a fill pointer."""
    return hasattr(array, '_fill_pointer')


def copy_tree(tree):
    """Create a copy of a tree structure."""
    if atom(tree):
        return tree
    return cons(copy_tree(tree.car), copy_tree(tree.cdr))


# Car/Cdr combinations
def caar(x):
    return car(car(x))


def cadr(x):
    return car(cdr(x))


def cdar(x):
    return cdr(car(x))


def cddr(x):
    return cdr(cdr(x))


def caaar(x):
    return car(car(car(x)))


def caadr(x):
    return car(car(cdr(x)))


def cadar(x):
    return car(cdr(car(x)))


def caddr(x):
    return car(cdr(cdr(x)))


def cdaar(x):
    return cdr(car(car(x)))


def cdadr(x):
    return cdr(car(cdr(x)))


def cddar(x):
    return cdr(cdr(car(x)))


def cdddr(x):
    return cdr(cdr(cdr(x)))


def caaaar(x):
    return car(car(car(car(x))))


def caaadr(x):
    return car(car(car(cdr(x))))


def caadar(x):
    return car(car(cdr(car(x))))


def caaddr(x):
    return car(car(cdr(cdr(x))))


def cadaar(x):
    return car(cdr(car(car(x))))


def cadadr(x):
    return car(cdr(car(cdr(x))))


def caddar(x):
    return car(cdr(cdr(car(x))))


def cadddr(x):
    return car(cdr(cdr(cdr(x))))


def cdaaar(x):
    return cdr(car(car(car(x))))


def cdaadr(x):
    return cdr(car(car(cdr(x))))


def cdadar(x):
    return cdr(car(cdr(car(x))))


def cdaddr(x):
    return cdr(car(cdr(cdr(x))))


def cddaar(x):
    return cdr(cdr(car(car(x))))


def cddadr(x):
    return cdr(cdr(car(cdr(x))))


def cdddar(x):
    return cdr(cdr(cdr(car(x))))


def cddddr(x):
    return cdr(cdr(cdr(cdr(x))))


# List element accessors
def first(seq):
    """Get the first element of a sequence."""
    return car(seq)


def second(seq):
    """Get the second element of a sequence."""
    return cadr(seq)


def third(seq):
    """Get the third element of a sequence."""
    return caddr(seq)


def fourth(seq):
    """Get the fourth element of a sequence."""
    return car(cdr(cdr(cdr(seq))))


def fifth(seq):
    """Get the fifth element of a sequence."""
    return car(cdr(cdr(cdr(cdr(seq)))))


def sixth(seq):
    """Get the sixth element of a sequence."""
    return car(cdr(cdr(cdr(cdr(cdr(seq))))))


def seventh(seq):
    """Get the seventh element of a sequence.""" 
    return car(cdr(cdr(cdr(cdr(cdr(cdr(seq)))))))


def eighth(seq):
    """Get the eighth element of a sequence."""
    return car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(seq))))))))


def ninth(seq):
    """Get the ninth element of a sequence."""
    return car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(seq)))))))))


def tenth(seq):
    """Get the tenth element of a sequence."""
    return car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(seq))))))))))


def rest(x):
    """Return the rest of list x (same as cdr)"""
    return cdr(x)


def butlast(seq):
    """Return all but the last element of a sequence."""
    return tuple(seq[:-1])


# Property list operations
def getf(plist, indicator, default=None):
    """Get property from property list."""
    current = plist
    while current is not None and current != lisptype.NIL:
        if not consp(current):
            break
        key = car(current)
        if key == indicator:
            return car(cdr(current)) if consp(cdr(current)) else default
        current = cdr(cdr(current))
    return default


def get_properties(plist, indicator_list):
    """Get properties from property list."""
    current = plist
    while current is not None and current != lisptype.NIL:
        if not consp(current):
            break
        key = car(current)
        if key in indicator_list:
            return key, car(cdr(current)) if consp(cdr(current)) else None, current
        current = cdr(cdr(current))
    return None, None, None


def putprop(symbol, value, indicator):
    """Put property on symbol."""
    # For now, just return the value - proper implementation later
    return value


def remprop(symbol, indicator):
    """Remove property from symbol."""
    # For now, just return None - proper implementation later
    return None


def symbol_plist(symbol):
    """Get symbol's property list."""
    if hasattr(symbol, 'plist'):
        return symbol.plist
    return None


def remf(place, indicator):
    """Remove property from place."""
    # For now, just return None - proper implementation later
    return None


# Final batch - type and special forms
def most_negative_long_float():
    """Most negative long float."""
    import sys
    return -sys.float_info.max


def most_positive_long_float():
    """Most positive long float."""
    import sys
    return sys.float_info.max


def most_negative_short_float():
    """Most negative short float."""
    import sys
    return -sys.float_info.max


def most_positive_short_float():
    """Most positive short float."""
    import sys
    return sys.float_info.max


def char_control_bit():
    """Character control bit."""
    return 1


def char_hyper_bit():
    """Character hyper bit."""
    return 2


def char_meta_bit():
    """Character meta bit."""
    return 4


def char_super_bit():
    """Character super bit."""
    return 8


def keyword_package():
    """Keyword package."""
    return 'KEYWORD'


def lisp_package():
    """Lisp package."""
    return 'COMMON-LISP'


def common_lisp_user_package():
    """Common Lisp user package."""
    return 'COMMON-LISP-USER'


def multiple_values_limit():
    """Multiple values limit."""
    return 20


def lambda_parameters_limit():
    """Lambda parameters limit."""
    return 50


def lambda_list_keywords():
    """Lambda list keywords."""
    return ['&optional', '&rest', '&key', '&allow-other-keys', '&aux', '&whole', '&environment', '&body']


def call_arguments_limit():
    """Call arguments limit."""
    return 50


def pi_constant():
    """Pi constant."""
    import math
    return math.pi


def boole_1():
    """Boole constant 1."""
    return 1


def boole_2():
    """Boole constant 2."""
    return 2


def boole_and():
    """Boole and."""
    return 1


def boole_andc1():
    """Boole andc1."""
    return 2


def boole_andc2():
    """Boole andc2."""
    return 3


def boole_c1():
    """Boole c1."""
    return 4


def boole_c2():
    """Boole c2."""
    return 5


def boole_clr():
    """Boole clear."""
    return 0


def boole_eqv():
    """Boole equivalence."""
    return 6


def boole_ior():
    """Boole inclusive or."""
    return 7


def boole_nand():
    """Boole nand."""
    return 8


def boole_nor():
    """Boole nor."""
    return 9


def boole_orc1():
    """Boole orc1."""
    return 10


def boole_orc2():
    """Boole orc2."""
    return 11


def boole_set():
    """Boole set."""
    return 15


def boole_xor():
    """Boole exclusive or."""
    return 12


def internal_time_units_per_second():
    """Internal time units per second."""
    return 1000


def array_element_type(array):
    """Get array element type."""
    return 'T'


def array_rank(array):
    """Get array rank."""
    try:
        return len(array.shape)
    except:
        return 1


def array_total_size(array):
    """Get array total size."""
    try:
        return array.size
    except:
        return len(array) if hasattr(array, '__len__') else 1


def decode_universal_time(universal_time, time_zone=None):
    """Decode universal time."""
    import time
    t = time.gmtime(universal_time - 2208988800)  # Lisp epoch offset
    return t.tm_sec, t.tm_min, t.tm_hour, t.tm_mday, t.tm_mon, t.tm_year, t.tm_wday, False, 0


def encode_universal_time(second, minute, hour, date, month, year, time_zone=None):
    """Encode universal time."""
    import time
    t = (year, month, date, hour, minute, second, 0, 0, 0)
    return int(time.mktime(t)) + 2208988800  # Lisp epoch offset


def get_universal_time():
    """Get current universal time."""
    import time
    return int(time.time()) + 2208988800  # Lisp epoch offset


def get_internal_real_time():
    """Get internal real time."""
    import time
    return int(time.time() * 1000)


def get_internal_run_time():
    """Get internal run time."""
    import time
    return int(time.process_time() * 1000)


def sleep_fn(seconds):
    """Sleep for seconds."""
    import time
    time.sleep(seconds)
    return None


def standard_char_p(char):
    """Test if standard character."""
    return ord(char) < 128


def graphic_char_p(char):
    """Test if graphic character."""
    return char.isprintable()


def digit_char_p(char, radix=10):
    """Digit character test."""
    try:
        val = int(char, radix)
        return val
    except:
        return None


def digit_char(weight, radix=10):
    """Digit character from weight."""
    if 0 <= weight < radix:
        if weight < 10:
            return str(weight)
        else:
            return chr(ord('A') + weight - 10)
    return None


def char_upcase(char):
    """Character upcase."""
    return char.upper()


def char_downcase(char):
    """Character downcase."""
    return char.lower()


def both_case_p(char):
    """Test if character has both cases."""
    return char.isalpha()


def upper_case_p(char):
    """Test if upper case."""
    return char.isupper()


def lower_case_p(char):
    """Test if lower case."""
    return char.islower()


# Special values
def null_value():
    """Null value."""
    return None


def unbound():
    """Unbound value marker."""
    return object()  # Unique object
