"""Arithmetic and basic math operations."""

import math
import functools
from fractions import Fraction
import fclpy.lisptype as lisptype
from . import registry as _registry


def _ensure_number(x, func_name):
    """Ensure argument is a number, raise error if not.

    `Fraction` is a Lisp RATIO, and RATIO is a subtype of RATIONAL/REAL/NUMBER
    (CLHS 12.1) -- omitting it rejected every exact ratio. `(rational
    most-negative-short-float)` produces one, so `numbers/number-comparison.lsp`
    failed to *load* with "<=: Argument is not a REAL", taking its whole file
    out of the run.
    """
    if isinstance(x, (int, float, complex, Fraction)):
        return x
    if x is lisptype.NIL or isinstance(x, lisptype.lispNull):
        raise lisptype.LispTypeError(f"{func_name}: Argument is not a NUMBER: NIL",
                                    expected_type="NUMBER", actual_value=x)
    # `lisptype.Symbol` does not exist -- the class is `LispSymbol` -- so this
    # branch raised AttributeError instead of the TYPE-ERROR it was written to
    # signal, turning "not a number" into a Python error leaking out as the
    # value of the form (standing rule 2). It stayed invisible because nothing
    # reached it until ABS started using this helper.
    if isinstance(x, lisptype.LispSymbol):
        raise lisptype.LispTypeError(f"{func_name}: Argument is not a NUMBER: {x.name}",
                                    expected_type="NUMBER", actual_value=x)
    raise lisptype.LispTypeError(f"{func_name}: Argument is not a NUMBER: {x}",
                                expected_type="NUMBER", actual_value=x)


def _ensure_real(x, func_name):
    """Ensure argument is a real number, raise error if not.

    Includes `Fraction`: a RATIO is a REAL (CLHS 12.1). See `_ensure_number`.
    """
    if isinstance(x, (int, float, Fraction)):
        return x
    if isinstance(x, complex):
        raise lisptype.LispTypeError(f"{func_name}: Argument is not a REAL: {x}",
                                    expected_type="REAL", actual_value=x)
    if x is lisptype.NIL or isinstance(x, lisptype.lispNull):
        raise lisptype.LispTypeError(f"{func_name}: Argument is not a REAL: NIL",
                                    expected_type="REAL", actual_value=x)
    if isinstance(x, lisptype.LispSymbol):
        raise lisptype.LispTypeError(f"{func_name}: Argument is not a REAL: {x.name}",
                                    expected_type="REAL", actual_value=x)
    raise lisptype.LispTypeError(f"{func_name}: Argument is not a REAL: {x}",
                                expected_type="REAL", actual_value=x)


@_registry.cl_function('ABS')
def abs_fn(x):
    """Absolute value (CLHS 12.2): signals a TYPE-ERROR for a non-number.

    This called Python's `abs` on whatever it was handed, so `(abs nil)`
    raised `TypeError: bad operand type for abs(): 'lispNull'` -- a Python
    exception as the value of a Lisp form (standing rule 2), and one that
    aborted a whole *file* load rather than one test. `_ensure_number` is
    the shared check the rest of this module already uses; ABS simply was
    not going through it.
    """
    return abs(_ensure_number(x, 'ABS'))


def _ensure_integer(x, func_name):
    if isinstance(x, bool) or not isinstance(x, int):
        raise lisptype.LispTypeError(
            f"{func_name}: {x!r} is not an INTEGER",
            expected_type="INTEGER", actual_value=x)
    return x


def _ensure_integers(integers, func_name):
    for x in integers:
        _ensure_integer(x, func_name)


@_registry.cl_function('GCD')
def gcd(*integers):
    """Greatest common divisor.

    `math.gcd` (3.9+) already implements CLHS's `(gcd)` => 0, `(gcd i)` =>
    `(abs i)`, and `(gcd ... 0 ...)` correctly for any number of arguments,
    so reducing over it by hand was a second, incomplete copy: `reduce`
    over a single argument returns that argument unchanged rather than its
    absolute value, which `math.gcd(*integers)` does not get wrong.
    """
    _ensure_integers(integers, 'GCD')
    return math.gcd(*integers)


@_registry.cl_function('LCM')
def lcm(*integers):
    """Least common multiple.

    Same reasoning as GCD: `math.lcm` (3.9+) already returns 0 for any
    zero argument and `(abs i)` for a single argument, both of which the
    hand-rolled `abs(a*b) // gcd(a, b)` reduction got wrong -- `gcd(0, 0)
    == 0` made folding in a literal 0 a `ZeroDivisionError` (LCM.9-12), and
    reducing over one argument returned it unmodified instead of its
    absolute value (LCM.2/LCM.3).
    """
    _ensure_integers(integers, 'LCM')
    return math.lcm(*integers)


def _extremum(args, func_name, better):
    """Shared MIN/MAX body (CLHS 12.1.4.1).

    Both take `&rest number+` -- at least one argument is required, so a
    zero-arg call is a PROGRAM-ERROR, and Python's `min()`/`max()` raising
    `ValueError: ... empty sequence` for that case was exactly X1's shape
    (a Python exception standing in for the condition). Every argument must
    be REAL, not just comparable in Python.

    The comparisons are exact (CLHS 12.1.4.1's contagion applies to
    *arithmetic* functions; MIN/MAX just pick the winning argument), and the
    winner is returned unmodified -- coercing a rational winner to float
    whenever a float happened to also be present looked like the ANSI
    "float contagion" rule but is actively wrong: MIN.2 compares a
    near-2**63 exact integer against a MOST-POSITIVE-SINGLE-FLOAT and the
    integer wins, and `float()` of that integer is *not* the same number
    (`9223372036854775802` rounds to `9223372036854775808.0`), so contagion
    silently corrupted the very comparison MIN.2 checks. `(min 1/3 0.8s0)`
    accepts either `1/3` or its float image (MIN.28 asserts an `or` of the
    two), so nothing requires the coercion in the first place.
    """
    if not args:
        raise lisptype.LispProgramError(
            f"{func_name}: requires at least 1 argument")
    for arg in args:
        _ensure_real(arg, func_name)
    winner = args[0]
    for arg in args[1:]:
        if better(arg, winner):
            winner = arg
    return winner


@_registry.cl_function('MAX')
def max_fn(*args):
    """Maximum of numbers."""
    return _extremum(args, 'MAX', lambda a, b: a > b)


@_registry.cl_function('MIN')
def min_fn(*args):
    """Minimum of numbers."""
    return _extremum(args, 'MIN', lambda a, b: a < b)


@_registry.cl_function('SIGNUM')
def signum(x):
    """Sign of number."""
    if x > 0:
        return 1
    elif x < 0:
        return -1
    else:
        return 0


@_registry.cl_function('EVENP')
def evenp(x):
    """Test if number is even."""
    return lisptype.lisp_bool(x % 2 == 0)


@_registry.cl_function('ODDP')
def oddp(x):
    """Test if number is odd."""
    return lisptype.lisp_bool(x % 2 == 1)


@_registry.cl_function('ZEROP')
def zerop(x):
    """Test if number is zero."""
    return lisptype.lisp_bool(x == 0)


@_registry.cl_function('PLUSP')
def plusp(x):
    """Test if number is positive."""
    return lisptype.lisp_bool(x > 0)


@_registry.cl_function('MINUSP')
def minusp(x):
    """Test if number is negative."""
    return lisptype.lisp_bool(x < 0)


@_registry.cl_function('MOD')
def mod(number, divisor):
    """FLOOR's remainder (CLHS 12.2): the sign follows the *divisor*."""
    return number - _rounded_quotient(number, divisor, math.floor) * divisor


@_registry.cl_function('REM')
def rem(number, divisor):
    """TRUNCATE's remainder (CLHS 12.2): the sign follows the *dividend*.

    Both used to be Python's ``%``, which is floor-based -- right for MOD and
    wrong for REM whenever the operands differ in sign: ``(rem -7 2)`` gave 1
    where ANSI requires -1. Defining each as the remainder of the corresponding
    rounding operation is what keeps them consistent with the second value of
    FLOOR and TRUNCATE instead of being a third implementation of "remainder".
    """
    return number - _rounded_quotient(number, divisor, int) * divisor


def _exact_quotient(x, divisor):
    """``x/divisor`` as an exact `Fraction`, or None if either is a float.

    CLHS 12.1.3.3: an operation on two rationals is exact. Python's ``/`` is
    *float* division, so every one of the eight divide-then-round operators
    below used to convert its arguments to double before rounding them, and
    silently lost precision above 2**53::

        (ceiling (+ (expt 2 62) (1+ (expt 2 62))) 2)
        ;; 4611686018427387904, one less than the true 4611686018427387905

    That is not a rounding curiosity. `integer-binary-search`
    (ansi-test `auxiliary/numbers-aux.lsp:46`) steps with
    ``(ceiling (+ lo hi) 2)``, so once ``lo`` passed 2**53 the midpoint rounded
    back to ``lo`` itself, ``(setq lo mid)`` became a no-op, and the search ran
    forever -- 1,335,702 iterations into the 600s LOOP cap, 15% of the whole
    ANSI run's wall time, reached from `numbers/sqrt.lsp`'s
    ``(find-largest-exactly-floatable-integer most-positive-fixnum)``.

    `Fraction` keeps the division exact, and `math.floor`/`math.ceil`/`int`/
    `round` are all exact on a `Fraction` -- including `round`'s round-half-to-
    even, which is the rule CLHS gives ROUND.
    """
    if isinstance(x, float) or isinstance(divisor, float) or \
            isinstance(x, complex) or isinstance(divisor, complex):
        return None
    return Fraction(x) / Fraction(divisor)


def _rounded_quotient(x, divisor, rounder, as_float=False):
    """``x/divisor`` rounded by `rounder`, exactly when both are rational."""
    exact = _exact_quotient(x, divisor)
    quotient = rounder(exact if exact is not None else x / divisor)
    return float(quotient) if as_float else quotient


def _divide_and_round(x, divisor, rounder, as_float=False):
    """The two values CLHS 12.2 requires of the divide-then-round family.

    All eight of FLOOR/CEILING/TRUNCATE/ROUND and their F- variants return
    **quotient and remainder**, where ``remainder = number - quotient*divisor``
    and the F- variants differ only in returning the quotient as a float. They
    used to return the quotient alone, which is why the ansi-test helpers --
    every one of which opens with ``(eql (length vals) 2)`` on
    ``(multiple-value-list (floor n d))`` -- could not pass whatever the
    quotient was.

    `rounder` is the exact rounding operation to apply to the quotient;
    `_exact_quotient` keeps that division exact for rationals.
    """
    quotient = _rounded_quotient(x, divisor, rounder, as_float)
    return lisptype.MultipleValues(quotient, x - quotient * divisor)


@_registry.cl_function('ROUND')
def round_fn(x, divisor=1):
    """Round to nearest, half to even; quotient and remainder (CLHS 12.2)."""
    return _divide_and_round(x, divisor, round)


@_registry.cl_function('TRUNCATE')
def truncate(x, divisor=1):
    """Truncate toward zero; quotient and remainder."""
    return _divide_and_round(x, divisor, int)


@_registry.cl_function('CEILING')
def ceiling(x, divisor=1):
    """Round toward positive infinity; quotient and remainder."""
    return _divide_and_round(x, divisor, math.ceil)


@_registry.cl_function('FLOOR')
def floor(x, divisor=1):
    """Round toward negative infinity; quotient and remainder."""
    return _divide_and_round(x, divisor, math.floor)


@_registry.cl_function('FCEILING')
def fceiling(x, divisor=1):
    """CEILING with the quotient as a float."""
    return _divide_and_round(x, divisor, math.ceil, as_float=True)


@_registry.cl_function('FFLOOR')
def ffloor(x, divisor=1):
    """FLOOR with the quotient as a float."""
    return _divide_and_round(x, divisor, math.floor, as_float=True)


@_registry.cl_function('FROUND')
def fround(x, divisor=1):
    """ROUND with the quotient as a float."""
    return _divide_and_round(x, divisor, round, as_float=True)


@_registry.cl_function('FTRUNCATE')
def ftruncate(x, divisor=1):
    """TRUNCATE with the quotient as a float."""
    return _divide_and_round(x, divisor, int, as_float=True)


@_registry.cl_function('NUMERATOR')
def numerator(rational):
    """Return numerator of rational number"""
    from fractions import Fraction
    if isinstance(rational, Fraction):
        return rational.numerator
    return rational  # For integers, numerator is the number itself


@_registry.cl_function('DENOMINATOR')
def denominator(rational):
    """Return denominator of rational number"""
    from fractions import Fraction
    if isinstance(rational, Fraction):
        return rational.denominator
    return 1  # For integers, denominator is 1


def _canonicalize_rational(value):
    """Collapse a `Fraction` whose denominator reduced to 1 into a plain int.

    CLHS 12.1.1.2: a rational whose denominator is 1 *is* an integer --
    there is no separate ratio representation for it, so `(eql (rational
    -10000.0) -10000)` must hold. `/` already does exactly this
    normalization inline; RATIONAL/RATIONALIZE need the same one, not a
    second copy of the check with its own chance to disagree.
    """
    if isinstance(value, Fraction) and value.denominator == 1:
        return value.numerator
    return value


@_registry.cl_function('RATIONAL')
def rational(number):
    """Convert a real number to a rational (CLHS 12.1.1.2).

    RATIONAL takes exactly one argument. There is no numerator/denominator
    form in ANSI CL -- that was invented here: a second positional
    argument used to be accepted silently instead of the PROGRAM-ERROR
    RATIONAL.ERROR.2/3 require, since `/` (which *does* take a
    denominator) and RATIONAL are different functions.
    """
    _ensure_real(number, 'RATIONAL')
    if isinstance(number, int):
        return number
    if isinstance(number, Fraction):
        return _canonicalize_rational(number)
    # Float -- Fraction(float) is its *exact* binary value, which is what
    # RATIONAL requires (RATIONALIZE, not RATIONAL, approximates).
    return _canonicalize_rational(Fraction(number))


@_registry.cl_function('RATIONALIZE')
def rationalize(x):
    """Convert a float to the simplest rational within its representable
    precision (CLHS 12.1.1.2), approximated here via
    `Fraction.limit_denominator` -- CLHS's exact continued-fraction
    algorithm, keyed to the float's own ulp rather than a fixed
    denominator cap, is not attempted here and remains a known gap for
    values (e.g. irrational transcendentals) where the two disagree.
    """
    _ensure_real(x, 'RATIONALIZE')
    if isinstance(x, int):
        return x
    if isinstance(x, Fraction):
        return _canonicalize_rational(x)
    return _canonicalize_rational(Fraction(x).limit_denominator())


# Type predicates
@_registry.cl_function('NUMBERP')
def numberp(obj):
    """Test if object is a number."""
    from fractions import Fraction
    return lisptype.lisp_bool(isinstance(obj, (int, float, complex, Fraction)))


@_registry.cl_function('INTEGERP')
def integerp(obj):
    """Test if object is an integer."""
    return lisptype.lisp_bool(isinstance(obj, int))


@_registry.cl_function('FLOATP')
def floatp(obj):
    """Test if object is a float."""
    return lisptype.lisp_bool(isinstance(obj, float))


@_registry.cl_function('COMPLEXP')
def complexp(obj):
    """Test if object is a complex number."""
    return isinstance(obj, complex)


@_registry.cl_function('REALP')
def realp(obj):
    """Test if object is a real number.

    A RATIO is a REAL (CLHS 4.2.2: RATIONAL, which includes RATIO, is a
    subtype of REAL) -- `Fraction` was missing here even though `NUMBERP`
    and `RATIONALP` right next to it both already include it, and
    `_ensure_real` (this module's own internal REAL check, used by MIN/MAX
    and the `<`/`>`/`<=`/`>=` family) already treats a `Fraction` as real.
    A representative ratio in ansi-test's `*mini-universe*` therefore made
    `REALP` disagree with the code its own callers use to decide the same
    question -- `check-type-error`'s guard said "not real" while `MIN`
    correctly accepted it as real and returned it, an unresolvable
    contradiction from the test's point of view.
    """
    from fractions import Fraction
    return isinstance(obj, (int, float, Fraction))


@_registry.cl_function('RATIONALP')
def rationalp(obj):
    """Test if object is a rational number (integer or ratio)."""
    from fractions import Fraction
    return isinstance(obj, (int, Fraction))


# Complex number operations
@_registry.cl_function('IMAGPART')
def imagpart(number):
    """Return imaginary part of complex number."""
    if isinstance(number, complex):
        return number.imag
    return 0


@_registry.cl_function('REALPART')
def realpart(number):
    """Return real part of complex number."""
    if isinstance(number, complex):
        return number.real
    return number


@_registry.cl_function('CONJUGATE')
def conjugate(number):
    """Return complex conjugate."""
    if isinstance(number, complex):
        return number.conjugate()
    return number


@_registry.cl_function('PHASE')
def phase(number):
    """Return the phase (angle in radians) of a number.

    CLHS: PHASE always returns a float, even for an exact real -- `(phase
    0)` is `0.0`, not `0`, and PHASE.1-7 check that with `EQLT`, which is
    type-sensitive. Also validates `number` the same way as the other
    functions in this module (`_ensure_number`-style): a non-number must
    signal a TYPE-ERROR carrying that value as its datum, not a bare Python
    `TypeError` from `number >= 0`.
    """
    if not isinstance(number, (int, float, complex, Fraction)):
        raise lisptype.LispTypeError(
            f"PHASE: argument is not a NUMBER: {number!r}",
            expected_type="NUMBER", actual_value=number)
    if isinstance(number, complex):
        return math.atan2(number.imag, number.real)
    return 0.0 if number >= 0 else math.pi


@_registry.cl_function('CIS')
def cis(theta):
    """Return complex number with magnitude 1 and phase theta."""
    return complex(math.cos(theta), math.sin(theta))


# Bitwise operations
@_registry.cl_function('LOGAND')
def logand(*args):
    """Bitwise AND."""
    _ensure_integers(args, 'LOGAND')
    if not args:
        return -1
    return functools.reduce(lambda x, y: x & y, args)


@_registry.cl_function('LOGIOR')
def logior(*args):
    """Bitwise OR."""
    _ensure_integers(args, 'LOGIOR')
    if not args:
        return 0
    return functools.reduce(lambda x, y: x | y, args)


@_registry.cl_function('LOGXOR')
def logxor(*args):
    """Bitwise XOR."""
    _ensure_integers(args, 'LOGXOR')
    if not args:
        return 0
    return functools.reduce(lambda x, y: x ^ y, args)


@_registry.cl_function('LOGNOT')
def lognot(integer):
    """Bitwise NOT."""
    _ensure_integer(integer, 'LOGNOT')
    return ~integer


@_registry.cl_function('LOGEQV')
def logeqv(*args):
    """Bitwise equivalence."""
    _ensure_integers(args, 'LOGEQV')
    if not args:
        return -1
    return functools.reduce(lambda x, y: ~(x ^ y), args)


@_registry.cl_function('ASH')
def ash(i, count):
    """Arithmetic shift left/right."""
    return i << count if count >= 0 else i >> -count


@_registry.cl_function('INTEGER-LENGTH')
def integer_length(integer):
    """Number of bits in integer."""
    _ensure_integer(integer, 'INTEGER-LENGTH')
    if integer < 0:
        integer = ~integer
    return integer.bit_length()


@_registry.cl_function('LOGBITP')
def logbitp(index, integer):
    """Test if bit `index` of `integer`'s two's-complement representation is set.

    `integer & (1 << index)` materializes a bignum with `index` bits just to
    test one of them -- for `index` near MOST-POSITIVE-FIXNUM that is an
    unbounded allocation (a MemoryError, not a slow answer). Shifting
    `integer` right by `index` instead costs work proportional to
    `integer`'s own size: Python's arbitrary-precision shift collapses a
    negative integer's infinite sign-extension to -1 (or a positive one to
    0) once `index` exceeds its bit length, exactly CLHS 12.1.1's two's
    complement rule, without ever building a value the size of `index`.
    """
    from .arrays import nonnegative_integer
    nonnegative_integer(index, 'LOGBITP', expected="UNSIGNED-BYTE")
    _ensure_integer(integer, 'LOGBITP')
    return bool((integer >> index) & 1)


@_registry.cl_function('LOGCOUNT')
def logcount(integer):
    """Number of bits that differ from the sign bit (CLHS 12.1.1.1) --
    the number of 1 bits for a non-negative integer, or of 0 bits for a
    negative one.

    `bin(integer).count('0')` for a negative `integer` doesn't compute
    that: Python's `bin()` of a negative int is a `-` sign followed by the
    binary digits of its *magnitude*, not a two's-complement bit pattern,
    so counting `'0'` characters in it (and subtracting 1 for the `0b`)
    answers a question about the magnitude's zero digits, unrelated to
    LOGCOUNT's actual definition -- `(logcount x)` and `(logcount (lognot
    x))` must always be equal (LOGCOUNT.7), and the old formula didn't
    have that identity. `~integer` for negative `integer` is non-negative,
    so `bin(~integer)` is a clean two's-complement view with no sign to
    special-case.
    """
    _ensure_integer(integer, 'LOGCOUNT')
    return bin(integer if integer >= 0 else ~integer).count('1')


@_registry.cl_function('LOGTEST')
def logtest(integer1, integer2):
    """Test if any bits are set in both integers."""
    _ensure_integer(integer1, 'LOGTEST')
    _ensure_integer(integer2, 'LOGTEST')
    return (integer1 & integer2) != 0


@_registry.cl_function('LOGANDC1')
def logandc1(integer1, integer2):
    """AND with complement of first arg: (logand (lognot integer1) integer2)."""
    _ensure_integer(integer1, 'LOGANDC1')
    _ensure_integer(integer2, 'LOGANDC1')
    return ~integer1 & integer2


@_registry.cl_function('LOGANDC2')
def logandc2(integer1, integer2):
    """AND with complement of second arg: (logand integer1 (lognot integer2))."""
    _ensure_integer(integer1, 'LOGANDC2')
    _ensure_integer(integer2, 'LOGANDC2')
    return integer1 & ~integer2


@_registry.cl_function('LOGNAND')
def lognand(integer1, integer2):
    """NOT of AND: (lognot (logand integer1 integer2))."""
    _ensure_integer(integer1, 'LOGNAND')
    _ensure_integer(integer2, 'LOGNAND')
    return ~(integer1 & integer2)


@_registry.cl_function('LOGNOR')
def lognor(integer1, integer2):
    """NOT of OR: (lognot (logior integer1 integer2))."""
    _ensure_integer(integer1, 'LOGNOR')
    _ensure_integer(integer2, 'LOGNOR')
    return ~(integer1 | integer2)


@_registry.cl_function('LOGORC1')
def logorc1(integer1, integer2):
    """OR with complement of first arg: (logior (lognot integer1) integer2)."""
    _ensure_integer(integer1, 'LOGORC1')
    _ensure_integer(integer2, 'LOGORC1')
    return ~integer1 | integer2


@_registry.cl_function('LOGORC2')
def logorc2(integer1, integer2):
    """OR with complement of second arg: (logior integer1 (lognot integer2))."""
    _ensure_integer(integer1, 'LOGORC2')
    _ensure_integer(integer2, 'LOGORC2')
    return integer1 | ~integer2


@_registry.cl_function('BYTE')
def byte_fn(size, position):
    """Create byte specifier."""
    return (size, position)


@_registry.cl_function('BYTE-SIZE')
def byte_size(bytespec):
    """Size of byte specifier."""
    return bytespec[0]


@_registry.cl_function('BYTE-POSITION')
def byte_position(bytespec):
    """Position of byte specifier."""
    return bytespec[1]


@_registry.cl_function('LDB')
def ldb(bytespec, integer):
    """Load byte."""
    size, position = bytespec
    mask = (1 << size) - 1
    return (integer >> position) & mask


@_registry.cl_function('LDB-TEST')
def ldb_test(bytespec, integer):
    """Test byte."""
    return ldb(bytespec, integer) != 0


@_registry.cl_function('DPB')
def dpb(newbyte, bytespec, integer):
    """Deposit byte."""
    return deposit_field(newbyte, bytespec, integer)


@_registry.cl_function('DEPOSIT-FIELD')
def deposit_field(newbyte, bytespec, integer):
    """Deposit field in integer."""
    size, position = bytespec
    mask = (1 << size) - 1
    # Clear the field and insert new value
    cleared = integer & ~(mask << position)
    return cleared | ((newbyte & mask) << position)


@_registry.cl_function('MASK-FIELD')
def mask_field(bytespec, integer):
    """Mask field in integer."""
    size, position = bytespec
    mask = (1 << size) - 1
    return integer & (mask << position)


# Bit array operations


# Comparison operators
@_registry.cl_function('=')
def _s_eq_(*args):
    """Numeric equality operator (=)."""
    if len(args) < 2:
        return lisptype.T
    # Validate all args are numbers
    for arg in args:
        _ensure_number(arg, '=')
    first = args[0]
    return lisptype.lisp_bool(all(x == first for x in args[1:]))


@_registry.cl_function('<')
def _s_lt_(*args):
    """Less than operator (<)."""
    if len(args) < 2:
        return lisptype.T
    # Validate all args are real numbers
    for arg in args:
        _ensure_real(arg, '<')
    for i in range(len(args) - 1):
        if not (args[i] < args[i + 1]):
            return lisptype.NIL
    return lisptype.T


@_registry.cl_function('>')
def _s_gt_(*args):
    """Greater than operator (>)."""
    if len(args) < 2:
        return lisptype.T
    # Validate all args are real numbers
    for arg in args:
        _ensure_real(arg, '>')
    for i in range(len(args) - 1):
        if not (args[i] > args[i + 1]):
            return lisptype.NIL
    return lisptype.T


@_registry.cl_function('<=')
def _s_lt__s_eq_(*args):
    """Less than or equal operator (<=)."""
    if len(args) < 2:
        return lisptype.T
    # Validate all args are real numbers
    for arg in args:
        _ensure_real(arg, '<=')
    for i in range(len(args) - 1):
        if not (args[i] <= args[i + 1]):
            return lisptype.NIL
    return lisptype.T


@_registry.cl_function('>=')
def _s_gt__s_eq_(*args):
    """Greater than or equal operator (>=)."""
    if len(args) < 2:
        return lisptype.T
    # Validate all args are real numbers
    for arg in args:
        _ensure_real(arg, '>=')
    for i in range(len(args) - 1):
        if not (args[i] >= args[i + 1]):
            return lisptype.NIL
    return lisptype.T


@_registry.cl_function('/=')
def _s_slash__s_eq_(*args):
    """Not equal operator (/=)."""
    if len(args) < 2:
        return lisptype.T
    for i in range(len(args)):
        for j in range(i + 1, len(args)):
            if args[i] == args[j]:
                return lisptype.NIL
    return lisptype.T


# Arithmetic operators
@_registry.cl_function('+')
def _s_plus_(*args):
    """Addition operator (+)."""
    if not args:
        return 0
    return sum(args)


@_registry.cl_function('-')
def _s_minus_(*args):
    """Subtraction operator (-)."""
    if not args:
        raise ValueError("- requires at least one argument")
    if len(args) == 1:
        return -args[0]
    result = args[0]
    for x in args[1:]:
        result -= x
    return result


@_registry.cl_function('*')
def _s_star_(*args):
    """Multiplication operator (*)."""
    if not args:
        return 1
    result = args[0]
    for x in args[1:]:
        result *= x
    return result


@_registry.cl_function('/')
def _s_slash_(*args):
    """Division operator (/).
    
    When dividing integers, returns an exact ratio (Fraction) if result is not exact.
    Automatically reduces fractions and normalizes signs.
    """
    from fractions import Fraction
    
    if not args:
        raise ValueError("/ requires at least one argument")
    if len(args) == 1:
        # Reciprocal: (/ x) = 1/x
        x = args[0]
        if isinstance(x, int) and x != 0:
            return _canonicalize_rational(Fraction(1, x))
        return 1 / x
    
    result = args[0]
    for x in args[1:]:
        # If both are integers and division is not exact, return a Fraction
        if isinstance(result, int) and isinstance(x, int) and x != 0:
            # Use Fraction for exact rational arithmetic
            result = Fraction(result, x)
        elif isinstance(result, Fraction) and isinstance(x, int) and x != 0:
            result = result / x
        elif isinstance(result, Fraction) and isinstance(x, Fraction):
            result = result / x
        else:
            result = result / x
    
    return _canonicalize_rational(result)


@_registry.cl_function('1+')
def _s_one_s_plus_(x):
    """Increment by one operator (1+)."""
    return x + 1


@_registry.cl_function('1-')
def _s_one_s_minus_(x):
    """Decrement by one operator (1-)."""
    return x - 1


# Fixed arithmetic limits
@_registry.cl_function('MOST-POSITIVE-FIXNUM')
def most_positive_fixnum():
    """Most positive fixnum."""
    return 2**63 - 1


@_registry.cl_function('MOST-NEGATIVE-FIXNUM')
def most_negative_fixnum():
    """Most negative fixnum."""
    return -2**63


@_registry.cl_function('BOOLE')
def boole(op, integer1, integer2):
    """Boolean operation on integers."""
    if op == 1:  # BOOLE-AND
        return integer1 & integer2
    elif op == 2:  # BOOLE-IOR
        return integer1 | integer2
    elif op == 6:  # BOOLE-XOR
        return integer1 ^ integer2
    else:
        return 0  # Simplified


@_registry.cl_function('ROTATE')
def rotate(integer, count, size=32):
    """Rotate bits in integer.
    
    Rotates COUNT bits left (positive) or right (negative) in SIZE-bit field.
    Note: ROTATE is not standard ANSI Common Lisp. Use ASH for shifts.
    This is provided for compatibility with the target list.
    """
    if size <= 0:
        return integer
    
    # Normalize count to be within size
    count = count % size
    if count == 0:
        return integer
    
    # Mask to size bits
    mask = (1 << size) - 1
    integer = integer & mask
    
    # Rotate left (positive count)
    if count > 0:
        return ((integer << count) | (integer >> (size - count))) & mask
    else:
        # Rotate right (negative count)
        count = -count
        return ((integer >> count) | (integer << (size - count))) & mask


__all__ = [
    'abs_fn', 'gcd', 'lcm', 'max_fn', 'min_fn', 'signum',
    'evenp', 'oddp', 'zerop', 'plusp', 'minusp',
    'mod', 'rem', 'round_fn', 'truncate', 'ceiling', 'floor',
    'fceiling', 'ffloor', 'fround', 'ftruncate',
    'numerator', 'denominator', 'rational', 'rationalize',
    'numberp', 'integerp', 'floatp', 'complexp', 'realp', 'rationalp',
    'imagpart', 'realpart', 'conjugate', 'phase', 'cis',
    'logand', 'logior', 'logxor', 'lognot', 'logeqv', 'ash',
    'logandc1', 'logandc2', 'lognand', 'lognor', 'logorc1', 'logorc2',
    'integer_length', 'logbitp', 'logcount', 'logtest',
    'byte_fn', 'byte_size', 'byte_position', 'ldb', 'ldb_test', 'dpb',
    'deposit_field', 'mask_field',
    '_s_eq_', '_s_lt_', '_s_gt_', '_s_lt__s_eq_', '_s_gt__s_eq_', '_s_slash__s_eq_',
    '_s_plus_', '_s_minus_', '_s_star_', '_s_slash_',
    '_s_one_s_plus_', '_s_one_s_minus_',
    'most_positive_fixnum', 'most_negative_fixnum', 'boole',
]
