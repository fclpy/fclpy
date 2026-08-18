"""Core Lisp data structure functions - cons cells, lists, and basic accessors."""

import fclpy.lisptype as lisptype
from . import registry as _registry


def _null_internal(obj):
    """True for every representation of NIL.

    NIL is spelled three ways here (CLAUDE.md): Python `None`, the
    `lisptype.NIL` singleton, and a `LispSymbol` named "NIL" interned in some
    package.  Code that tests only one of them answers "not NIL" for the other
    two, so the question has one home.
    """
    return (obj is None
            or obj is lisptype.NIL
            or (type(obj) is lisptype.LispSymbol and obj.name == 'NIL'))


def _listp_internal(obj):
    """True if `obj` is a Lisp *list* -- a cons or NIL (CLHS 14.1).

    This is the type CAR/CDR accept, the type LISTP answers for, and the type
    `(typep x 'list)` denotes.  All three must agree: ansi-test's
    `(check-type-error #'car #'listp)` calls CAR on every object in the
    universe that fails LISTP and requires a TYPE-ERROR from each, so a CAR
    that accepts something LISTP rejects is directly observable.
    """
    return type(obj) is lisptype.lispCons or _null_internal(obj)


def _not_a_list(operator, seq):
    """The TYPE-ERROR CAR/CDR owe for a non-list argument (CLHS 14.2)."""
    return lisptype.LispTypeError(
        f"{operator}: {type(seq).__name__} is not a list",
        expected_type="LIST", actual_value=seq)


def _check_list(seq, operator):
    """Signal unless `seq` is a list -- a cons or NIL (CLHS 14.1).

    The entry check every list operator owes its argument, in one place next to
    the predicate that defines the type. `sequence_protocol.check_list` is this
    function, and `list_cells` applies it, so an operator that walks the list
    through the protocol does not repeat it; TAILP and LDIFF call it directly
    because they consume the dotted terminator themselves rather than through
    a walker.
    """
    if not _listp_internal(seq):
        raise _not_a_list(operator, seq)
    return seq


@_registry.cl_function('CAR')
def car(seq):
    """The car of a list: its first element, or NIL for the empty list.

    CLHS 14.2 requires the argument to be a *list*; anything else is a
    TYPE-ERROR.  This used to `return seq` for every other object, so
    `(car 'a)` answered `A` and `(car "ab")` answered `"ab"` -- a silent wrong
    answer (standing rule 4) that also propagated into all 28 compound
    accessors below, since each is a composition of this function.
    """
    if type(seq) is lisptype.lispCons:
        return seq.car
    if _null_internal(seq):
        return lisptype.NIL
    raise _not_a_list('CAR', seq)


@_registry.cl_function('CDR')
def cdr(seq):
    """The cdr of a list: the rest of it, or NIL for the empty list."""
    if type(seq) is lisptype.lispCons:
        return seq.cdr
    if _null_internal(seq):
        return lisptype.NIL
    raise _not_a_list('CDR', seq)


@_registry.cl_function('CONS')
def cons(x, seq):
    """Returns a new cons cell where x is the first element and seq is the rest."""
    return lisptype.lispCons(x, seq)


@_registry.cl_function('CONSP')
def consp(obj):
    """Test if object is a cons cell."""
    return lisptype.lisp_bool(type(obj) is lisptype.lispCons)

def _consp_internal(obj):
    """Internal version for Python code - returns Python boolean."""
    return type(obj) is lisptype.lispCons


@_registry.cl_function('ATOM')
def atom(obj):
    """Test if object is an atom (not a cons cell)."""
    return lisptype.lisp_bool(type(obj) is not lisptype.lispCons)

def _atom_internal(obj):
    """Internal version for Python code - returns Python boolean."""
    return type(obj) is not lisptype.lispCons


def _tail_eq(a, b):
    """True if `a` and `b` are the same tail (CLHS 14.2: EQL).

    Shared by TAILP and LDIFF -- both walk a list's successive cdrs comparing
    each to a target tail. The comparison is EQL, and the Python `==` fallback
    this used to end in is a *different* relation for exactly the objects these
    two are tested with: `tailp.5` hands TAILP a string and a distinct copy of
    that string as the list's tail, and `==` calls them the same tail while EQL
    does not.
    """
    if a is b:
        return True
    if _null_internal(a) and _null_internal(b):
        return True
    from .comparison import eql
    return eql(a, b) is lisptype.T


def _build_list_ending_in(elements, final):
    """Cons a fresh list of `elements` onto `final` (NIL for a proper list,
    an atom for a dotted one).
    """
    result = final
    for e in reversed(elements):
        result = lisptype.lispCons(e, result)
    return result


@_registry.cl_function('TAILP')
def tailp(object_, list_):
    """True if `object_` is the same as some tail of `list_` (CLHS 14.2).

    Genuinely absent operator (plan.md C19): every `tailp.lsp` test was an
    `Undefined function` leak.

    `list_` may be dotted -- its final atom is one of its tails -- but it must
    be a list, so a non-list argument is a TYPE-ERROR rather than a walk that
    terminates immediately with NIL.
    """
    _check_list(list_, 'TAILP')
    current = list_
    while True:
        if _tail_eq(current, object_):
            return lisptype.T
        if not _consp_internal(current):
            return lisptype.NIL
        current = current.cdr


@_registry.cl_function('LDIFF')
def ldiff(list_, sublist):
    """Copy of `list_` up to (not including) the tail `sublist` (CLHS
    14.2); if `sublist` is not a tail of `list_`, a copy of the whole of
    `list_`.

    Genuinely absent operator (plan.md C19): every `ldiff.lsp` test was an
    `Undefined function` leak.

    Like TAILP, `list_` may be dotted but must be a list
    (`ldiff.error.1`-`.5`).
    """
    _check_list(list_, 'LDIFF')
    elements = []
    current = list_
    while _consp_internal(current):
        if _tail_eq(current, sublist):
            return _build_list_ending_in(elements, lisptype.NIL)
        elements.append(current.car)
        current = current.cdr
    final = lisptype.NIL if _tail_eq(current, sublist) else current
    return _build_list_ending_in(elements, final)


def acons(x, v, seq):
    """Creates a fresh cons, the cdr of which is alist and the car of which is 
    another fresh cons, the car of which is key and the cdr of which is 
    datum."""
    return lisptype.lispCons(lisptype.lispCons(x, v), seq)


@_registry.cl_function('LISTP')
def listp(obj):
    """Test if object is a list (either nil or a cons cell)."""
    return lisptype.lisp_bool(_listp_internal(obj))


@_registry.cl_function('SYMBOLP')
def symbolp(*args):
    """Test if object is a symbol."""
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"SYMBOLP: wrong number of arguments (got {len(args)}, expected 1)"
        )
    obj = args[0]
    return lisptype.lisp_bool(type(obj) is lisptype.LispSymbol)


@_registry.cl_function('KEYWORDP')
def keywordp(*args):
    """Test if object is a keyword."""
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"KEYWORDP: wrong number of arguments (got {len(args)}, expected 1)"
        )
    obj = args[0]
    return lisptype.lisp_bool(type(obj) is lisptype.lispKeyword)


@_registry.cl_function('HASH-TABLE-P')
def hash_table_p(*args):
    """Test if object is a hash table."""
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"HASH-TABLE-P: wrong number of arguments (got {len(args)}, expected 1)"
        )
    obj = args[0]
    return lisptype.lisp_bool(isinstance(obj, dict))


from . import registry as _registry  # ensure decorator availability for new predicates

@_registry.cl_function('PACKAGEP')
def packagep(*args):
    """Test if object is a package."""
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"PACKAGEP: wrong number of arguments (got {len(args)}, expected 1)"
        )
    obj = args[0]
    return lisptype.lisp_bool(isinstance(obj, lisptype.Package))


def find_package_fn(name):
    """Find package by name or nickname."""
    return lisptype.find_package(name)


def make_package_fn(name, nicknames=None, use_list=None):
    """Create a new package."""
    return lisptype.make_package(name, nicknames or [], use_list or [])


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


# Property list operations
@_registry.cl_function('GETF')
def getf(plist, indicator, default=None):
    """Get property from property list (CLHS 5.1.2.3, 15.2).

    An improper plist -- a dangling atom where the next indicator or its
    value should be -- used to just `break` the walk and answer `default`,
    silently treating a malformed argument as "property not found" instead
    of the TYPE-ERROR `getf.error.4`/`.5` require (standing rule 4).
    """
    current = plist
    while _consp_internal(current):
        if not _consp_internal(current.cdr):
            raise _not_a_list('GETF', current.cdr)
        if current.car is indicator:
            return current.cdr.car
        current = current.cdr.cdr
    if not _null_internal(current):
        raise _not_a_list('GETF', current)
    return default


@_registry.cl_function('GET-PROPERTIES')
def get_properties(plist, indicator_list):
    """Get properties from property list (CLHS 5.1.2.3, 15.2).

    Returns three values -- CLHS: "returns three values... or three NILs".
    Previously returned a bare Python tuple, which is not a Lisp value at
    all (plan.md standing rule 2/Finding M): a `(get-properties ...)` call
    in a single-value context answered the *tuple object* rather than its
    first element, and `MULTIPLE-VALUE-BIND`/`(values ...)` callers saw
    nothing sensible. `indicator_list` used Python `in` on a `lispCons`,
    which has no `__contains__` (plan.md standing rule 4's class of bug --
    it happened not to raise here only because `lispList.__iter__` made
    `in` fall back to a linear scan, but an improper indicator list was
    never checked at all).
    """
    indicators = []
    cur = indicator_list
    while _consp_internal(cur):
        indicators.append(cur.car)
        cur = cur.cdr
    if not _null_internal(cur):
        raise _not_a_list('GET-PROPERTIES', cur)
    current = plist
    while _consp_internal(current):
        if not _consp_internal(current.cdr):
            raise _not_a_list('GET-PROPERTIES', current.cdr)
        if any(current.car is ind for ind in indicators):
            return lisptype.MultipleValues(current.car, current.cdr.car, current)
        current = current.cdr.cdr
    if not _null_internal(current):
        raise _not_a_list('GET-PROPERTIES', current)
    return lisptype.MultipleValues(lisptype.NIL, lisptype.NIL, lisptype.NIL)


@_registry.cl_function('PUTPROP')
def putprop(*args):
    """Put property on symbol.

    Behavior: (PUTPROP SYMBOL VALUE INDICATOR) stores INDICATOR/VALUE in
    SYMBOL's plist and returns VALUE. Supports `plist` stored as Python
    dict or as a Lisp cons-list (`lispCons`).
    """
    if len(args) != 3:
        raise lisptype.LispProgramError(
            f"PUTPROP: wrong number of arguments (got {len(args)}, expected 3)"
        )
    symbol, value, indicator = args
    plist = getattr(symbol, 'plist', lisptype.NIL)
    # Dict-style storage
    if isinstance(plist, dict):
        plist[indicator] = value
        symbol.plist = plist
        return value
    # Lisp cons-list storage: remove first existing indicator, then cons new pair
    if type(plist) is lisptype.lispCons or plist is lisptype.NIL:
        # Remove existing first occurrence
        prev = None
        curr = plist
        while curr is not lisptype.NIL and _consp_internal(curr):
            key = car(curr)
            if key == indicator:
                # skip this key and its value
                next_pair = cdr(cdr(curr))
                if prev is None:
                    plist = next_pair
                else:
                    prev.cdr = next_pair
                break
            prev = curr
            curr = cdr(cdr(curr))
        # Prepend new indicator/value pair
        new_pair = lisptype.lispCons(indicator, lisptype.lispCons(value, plist))
        symbol.plist = new_pair
        return value
    # Fallback: set as dict
    try:
        d = dict(plist)
        d[indicator] = value
        symbol.plist = d
        return value
    except Exception:
        symbol.plist = {indicator: value}
        return value


@_registry.cl_function('REMPROP')
def remprop(*args):
    """Remove property from symbol.

    Returns T if an occurrence was removed, NIL otherwise. Supports dict
    and lispCons plists.
    """
    if len(args) != 2:
        raise lisptype.LispProgramError(
            f"REMPROP: wrong number of arguments (got {len(args)}, expected 2)"
        )
    symbol, indicator = args
    plist = getattr(symbol, 'plist', lisptype.NIL)
    if plist is None or plist is lisptype.NIL:
        return lisptype.NIL
    # Dict-style
    if isinstance(plist, dict):
        if indicator in plist:
            del plist[indicator]
            symbol.plist = plist
            return lisptype.T
        return lisptype.NIL
    # Lisp cons-list style: remove first occurrence of indicator and its value.
    # prev_value_cell is the cons cell holding the PRECEDING pair's value --
    # its .cdr must be relinked (not the preceding pair's key cell, whose
    # .cdr is that same value cell and must stay intact).
    if type(plist) is lisptype.lispCons:
        prev_value_cell = None
        curr = plist
        found = False
        while curr is not lisptype.NIL and _consp_internal(curr):
            key = car(curr)
            rest = cdr(curr)
            if not _consp_internal(rest):
                break
            next_pair = cdr(rest)
            if key == indicator and not found:
                found = True
                if prev_value_cell is None:
                    plist = next_pair
                else:
                    prev_value_cell.cdr = next_pair
                curr = next_pair
                continue
            prev_value_cell = rest
            curr = next_pair
        symbol.plist = plist if plist is not None else lisptype.NIL
        return lisptype.T if found else lisptype.NIL
    # Fallback: try dict-like removal
    try:
        d = dict(plist)
        if indicator in d:
            del d[indicator]
            symbol.plist = d
            return lisptype.T
        return lisptype.NIL
    except Exception:
        return lisptype.NIL


@_registry.cl_function('SYMBOL-PLIST')
def symbol_plist(*args):
    """Get symbol's property list as a Lisp list.
    
    Returns a proper Lisp plist (indicator value indicator value ...)
    or NIL if the symbol has no properties.
    """
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"SYMBOL-PLIST: wrong number of arguments (got {len(args)}, expected 1)"
        )
    symbol = args[0]
    if hasattr(symbol, 'plist') and symbol.plist:
        plist = symbol.plist
        # If stored as a Python dict, convert to Lisp plist
        if isinstance(plist, dict):
            result = lisptype.NIL
            for key, value in reversed(list(plist.items())):
                result = lisptype.lispCons(value, result)
                result = lisptype.lispCons(key, result)
            return result
        # If already a Lisp cons-list (lispCons), return it directly
        if type(plist) is lisptype.lispCons or plist is lisptype.NIL:
            return plist
        # Fallback: try to treat as iterable of elements and build a Lisp list
        try:
            seq = list(plist)
            result = lisptype.NIL
            for item in reversed(seq):
                result = lisptype.lispCons(item, result)
            return result
        except Exception:
            # Unknown plist format - return NIL
            return lisptype.NIL
    return lisptype.NIL


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


@_registry.cl_function('INTERNAL-TIME-UNITS-PER-SECOND')
def internal_time_units_per_second():
    """Internal time units per second."""
    return 1000


@_registry.cl_function('DECODE-UNIVERSAL-TIME')
def decode_universal_time(universal_time, time_zone=None):
    """Decode universal time."""
    import time
    t = time.gmtime(universal_time - 2208988800)  # Lisp epoch offset
    return t.tm_sec, t.tm_min, t.tm_hour, t.tm_mday, t.tm_mon, t.tm_year, t.tm_wday, False, 0


@_registry.cl_function('ENCODE-UNIVERSAL-TIME')
def encode_universal_time(second, minute, hour, date, month, year, time_zone=None):
    """Encode universal time."""
    import time
    t = (year, month, date, hour, minute, second, 0, 0, 0)
    return int(time.mktime(t)) + 2208988800  # Lisp epoch offset


@_registry.cl_function('GET-UNIVERSAL-TIME')
def get_universal_time():
    """Get current universal time."""
    import time
    return int(time.time()) + 2208988800  # Lisp epoch offset


@_registry.cl_function('GET-INTERNAL-REAL-TIME')
def get_internal_real_time():
    """Get internal real time."""
    import time
    return int(time.time() * 1000)


@_registry.cl_function('GET-INTERNAL-RUN-TIME')
def get_internal_run_time():
    """Get internal run time."""
    import time
    return int(time.process_time() * 1000)


@_registry.cl_function('SLEEP')
def sleep_fn(*args):
    """Sleep for seconds."""
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"SLEEP: wrong number of arguments (got {len(args)}, expected 1)"
        )
    seconds = args[0]
    import time
    time.sleep(seconds)
    return None


def standard_char_p(char):
    """Test if standard character."""
    return ord(char) < 128


@_registry.cl_function('GRAPHIC-CHAR-P')
def graphic_char_p(*args):
    """Test if graphic character."""
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"GRAPHIC-CHAR-P: wrong number of arguments (got {len(args)}, expected 1)"
        )
    char = args[0]
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
