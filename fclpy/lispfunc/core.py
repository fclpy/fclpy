"""Core Lisp data structure functions - cons cells, lists, and basic accessors."""

import fclpy.lisptype as lisptype
from fclpy.system.kernel import kernel
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
            or (type(obj) is lisptype.LispSymbol and obj.name == 'NIL' and obj.package is not None))


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
def symbolp(object):
    """Test if object is a symbol (CLHS SYMBOLP).

    Delegates to `lisptype.is_symbol`, the one predicate for the question, so
    this and TYPEP's SYMBOL branch cannot disagree. The arity check that used
    to be hand-written here is what the Python signature already expresses:
    `LambdaListShape` signals the PROGRAM-ERROR for a wrong argument count.
    """
    return lisptype.lisp_bool(lisptype.is_symbol(object))


@_registry.cl_function('KEYWORDP')
def keywordp(object):
    """Test if object is a keyword (CLHS KEYWORDP)."""
    return lisptype.lisp_bool(lisptype.is_keyword(object))


# HASH-TABLE-P lives with the hash table object model it asks about, in
# `misc_hashtables.py`. It was registered here *and* in the dead
# `hashtables.py`, whose copy won on import order and tested its own unused
# class -- so the predicate answered NIL for every table `MAKE-HASH-TABLE`
# returns. Two registrations, one of them unreachable, is standing rule 3.


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


# COPY-TREE lives once, in misc_macros.copy_tree (the registered COPY-TREE);
# the unregistered recursive copy that used to sit here competed for the name
# and recursed on the cdr spine -- see recursion-plan.md Step 1.


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
    and lispCons plists. Signals a TYPE-ERROR if `symbol` is not a
    symbol (`remprop.error.4` exercises the entire ansi-test
    mini-universe, so every non-symbol must raise).
    """
    if len(args) != 2:
        raise lisptype.LispProgramError(
            f"REMPROP: wrong number of arguments (got {len(args)}, expected 2)"
        )
    symbol, indicator = args
    if not lisptype.is_symbol(symbol):
        raise lisptype.LispTypeError(
            f"REMPROP: {symbol!r} is not a symbol",
            expected_type='SYMBOL', actual_value=symbol)
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
    from .utilities_symbols import _require_symbol
    symbol = _require_symbol(args[0], 'SYMBOL-PLIST')
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
    return -kernel.float_max()


def most_positive_long_float():
    """Most positive long float."""
    return kernel.float_max()


def most_negative_short_float():
    """Most negative short float."""
    return -kernel.float_max()


def most_positive_short_float():
    """Most positive short float."""
    return kernel.float_max()


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


# The sixteen BOOLE-* op codes (CLHS 12.1.4) are *constant variables*, not
# functions -- see `lispenv.STANDARD_CONSTANTS`, their one home. This module
# used to carry a same-named zero-argument function for each (`boole_and`,
# `boole_1`, ...), which `registry.register_module` auto-registered as an
# fbound *function*; since none of the sixteen was ever bound as a variable,
# referencing the bare symbol (`numbers/boole.lsp` builds `*boole-vals*` by
# evaluating each name) fell through `evaluation_core.eval`'s
# unbound-variable-but-fbound fallback and returned the raw Python function
# object as the symbol's value -- and `boole_1`/`boole_and` both happened to
# return the Python int 1, so even a caller that (wrongly) funcalled them
# would have collapsed two distinct BOOLE operations onto one code.

# The universal-time model (CLHS 25.1.4) lives in `utilities_system.py`, which
# is its one home. This module used to carry a second copy of
# DECODE-/ENCODE-/GET-UNIVERSAL-TIME, both internal-time clocks, SLEEP and
# INTERNAL-TIME-UNITS-PER-SECOND; `registry.cl_function` is last-writer-wins,
# so which of the two ran was decided by import order and neither implemented
# the chapter. See that module's docstring.


def standard_char_p(char):
    """Test if standard character."""
    return ord(char) < 128


# GRAPHIC-CHAR-P is registered exactly once, in characters.py next to the rest
# of the character predicates. The core.py version that used to lose by
# import order took its argument as `*args` and called `.isprintable()` on
# whatever showed up -- which is `True` for every Python string and crashes
# on a Lisp `Character` -- and was the same defect the characters.py copy
# fixed. Leaving only the characters.py registration.


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
