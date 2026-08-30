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
    """Sign of number.

    CLHS 12.1.2: For a real number, returns a number of the same type as x
    with value -1, 0, or 1 based on sign. For a complex number, returns the
    unit vector in the direction of x, i.e., x / |x| (or 0 if x is zero).
    """
    import cmath
    _ensure_number(x, 'SIGNUM')

    if isinstance(x, complex):
        # For complex: return x / |x| (unit vector)
        magnitude = abs(x)
        if magnitude == 0:
            # For complex zero, return x itself (preserves +0/-0 distinction)
            return x
        else:
            return x / magnitude
    elif isinstance(x, float):
        # For float: return -1.0, 0.0, or 1.0 (preserving float type)
        if x > 0:
            return 1.0
        elif x < 0:
            return -1.0
        else:
            return 0.0 if x == 0.0 else x  # Preserve -0.0 vs +0.0
    else:
        # For integer/rational: return -1, 0, or 1
        if x > 0:
            return 1
        elif x < 0:
            return -1
        else:
            return 0


@_registry.cl_function('EVENP')
def evenp(x):
    """Test if number is even."""
    _ensure_integer(x, 'EVENP')
    return lisptype.lisp_bool(x % 2 == 0)


@_registry.cl_function('ODDP')
def oddp(x):
    """Test if number is odd."""
    _ensure_integer(x, 'ODDP')
    return lisptype.lisp_bool(x % 2 == 1)


@_registry.cl_function('ZEROP')
def zerop(x):
    """Test if number is zero."""
    _ensure_number(x, 'ZEROP')
    return lisptype.lisp_bool(x == 0)


@_registry.cl_function('PLUSP')
def plusp(x):
    """Test if number is positive."""
    _ensure_real(x, 'PLUSP')
    return lisptype.lisp_bool(x > 0)


@_registry.cl_function('MINUSP')
def minusp(x):
    """Test if number is negative."""
    _ensure_real(x, 'MINUSP')
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

    The F- variants return the quotient as a float but the remainder must
    remain an integer (or the numeric type of the original inputs).
    """
    # Compute quotient without float conversion first, for accurate remainder
    quotient_int = _rounded_quotient(x, divisor, rounder, as_float=False)
    # Compute remainder using the integer quotient
    remainder = x - quotient_int * divisor
    # Now apply float conversion to quotient if requested
    quotient = float(quotient_int) if as_float else quotient_int
    return lisptype.MultipleValues(quotient, remainder)


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


def _float_rounding_interval(x):
    """The open interval of reals that convert to the float `x`: the
    midpoints between `x` and each of its two neighbors. Everything
    strictly inside is closer to `x` than to any other float, so it
    converts back to `x` under round-to-nearest whatever the tie rule --
    which is exactly the guarantee RATIONALIZE needs: any rational in
    the interval satisfies (= (float r x) x).

    A bound is None when `x` is the extreme finite float of its kind and
    the neighbor does not exist: the interval then runs to that infinity.
    """
    import math
    v = Fraction(x)
    lower = math.nextafter(x, -math.inf)
    upper = math.nextafter(x, math.inf)
    lo = None if math.isinf(lower) else (Fraction(lower) + v) / 2
    hi = None if math.isinf(upper) else (v + Fraction(upper)) / 2
    return lo, hi


def _simplest_rational_in(lo, hi):
    """The simplest rational strictly inside the open interval (lo, hi) --
    smallest denominator, then smallest numerator -- by the Stern-Brocot /
    continued-fraction descent the CLHS RATIONALIZE description implies.

    `lo` and `hi` are exact `Fraction`s with lo < hi. At each step the
    integer part is peeled off: an integer strictly inside the interval is
    simplest; otherwise the same trick applied to the reciprocals of the
    fractional parts finds the fraction of smallest denominator, which is
    the only place a smaller denominator could hide.
    """
    if lo < 0:
        if hi <= 0:
            return -_simplest_rational_in(-hi, -lo)
        return Fraction(0)
    flo = lo.numerator // lo.denominator
    fhi = hi.numerator // hi.denominator
    if flo != fhi and flo + 1 < hi:
        return Fraction(flo + 1)
    # Same integer part (or flo+1 == hi exactly): descend on the
    # fractional parts. p/q in (a, b) with a > 0 is q/p in (1/b, 1/a).
    a = lo - flo
    b = hi - flo
    if a == 0:
        # (flo, hi): the simplest fraction above flo is flo + 1/q with the
        # smallest q such that 1/q < b.
        return flo + 1 / (1 // b + 1)
    return flo + 1 / _simplest_rational_in(1 / b, 1 / a)


@_registry.cl_function('RATIONALIZE')
def rationalize(x):
    """Convert a real to a rational (CLHS rationalize).

    For a float this is the simplest rational that still converts back to
    the same float -- the Stern-Brocot-simplest rational inside the
    float's rounding interval (`_float_rounding_interval`), not the float's
    own exact value, which is what RATIONAL returns. `rationalize.1`/`.3`
    round-trip every result through (float r x) and fail on anything that
    does not come back, which a fixed `limit_denominator` cap cannot
    guarantee (it answered 0 for the subnormals and missed by an ulp on
    doubles); an exact rational, an integer and zero come back unchanged.
    """
    _ensure_real(x, 'RATIONALIZE')
    if isinstance(x, int):
        return x
    if isinstance(x, Fraction):
        return _canonicalize_rational(x)
    if x == 0:
        return 0
    lo, hi = _float_rounding_interval(x)
    if lo is None:
        # (-inf, hi): mirrored from the bounded case -- the simplest
        # integer below hi is -floor(-hi) - 1.
        return _canonicalize_rational(-((-hi).numerator // (-hi).denominator) - 1)
    if hi is None:
        # (lo, +inf): the simplest integer above lo.
        return _canonicalize_rational(lo.numerator // lo.denominator + 1)
    return _canonicalize_rational(_simplest_rational_in(lo, hi))


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

class LispComplex(complex):
    """A complex number that remembers the type of its parts.

    `complex(3, 4)` and `complex(3, 4.0)` both return a Python `complex`
    with `real == 3.0` and `imag == 4.0` -- Python coerces int to float
    on construction. CLHS 12.1.4.1 keeps the parts in the type they
    arrived in: `(complex 3 4)` is a complex with int parts, `(complex
    1/3 0)` collapses to a ratio, `(complex 0 0)` collapses to an
    integer. `imagpart.1`/`2`/`3`, `realpart.1`/`2`/`3`, `complex.1`/`3`
    and `*.9`/`*.10`/`/.6`/`/.9`/`/.10`/`/.11` all demand this directly,
    by EQL against a number that the test wrote in source -- `(eql
    (realpart (complex 3 4)) 3)` must be T, not NIL.

    The class subclasses `complex` so the rest of the codebase -- the
    printer (`printer._write_complex`), `isinstance(x, complex)` checks in
    `comparison.py` and the hash tables, and the existing `+`/`-`/`*`/`/`
    Python arithmetic -- keeps treating the result as a complex. The
    original parts live in `_real`/`_imag`; `.real` and `.imag` are
    overridden to return those, so anything that asks `x.real` or
    `x.imag` (the printer, the existing `realpart`/`imagpart`, the
    existing `*`/`/` complex arithmetic) gets the type-preserved value
    back, not the float that Python's `complex` would have given it.
    """
    __slots__ = ('_real', '_imag')

    def __new__(cls, realpart, imagpart):
        instance = super().__new__(cls, realpart, imagpart)
        instance._real = realpart
        instance._imag = imagpart
        return instance

    @property
    def real(self):
        return self._real

    @property
    def imag(self):
        return self._imag

    def __repr__(self):
        return f'#C({self._real} {self._imag})'


def _is_lisp_complex(x):
    """True only for `LispComplex`, not for plain Python `complex`."""
    return isinstance(x, LispComplex)


def _complex_parts(x):
    """The real and imag parts of `x` as a (real, imag) pair.

    Plain Python `complex` and `LispComplex` both have `.real`/`.imag`;
    for anything else, the imag part is 0 and the real is `x` itself.
    """
    if isinstance(x, (LispComplex, complex)):
        return x.real, x.imag
    return x, 0


def _make_lisp_complex(real, imag):
    """Build a `LispComplex` unless `imag` is 0 and `real` is not a complex."""
    if imag == 0 and not isinstance(real, (LispComplex, complex)):
        return real
    return LispComplex(real, imag)


def _lisp_complex_wrap(real, imag):
    """Build the result of `a OP b` over complex parts, honoring CLHS
    12.1.5.3's canonical representation: a *rational* complex with a zero
    imaginary part is never constructed -- the result is the real part
    itself (`(* #c(1 2) #c(1 -2))` is the integer 5, `(* 0 #c(2 2))` is
    the integer 0). A complex with float parts is never canonicalized
    away: a float zero imag stays (`(* #c(1.0 2.0) 0)` is `#c(0.0 0.0)`),
    and one float operand makes the result's parts float -- which the
    test on the *result's* part type expresses directly, and which is
    what keeps the float rows of `expt.29` and `conjugate`'s signed-zero
    tests passing.

    The old criterion keyed on whether an *operand* was complex, so
    `(* 0 #c(2 2))` answered `#c(0 0)` -- a complex the canonical
    representation rule forbids -- while `(expt 0 #c(2 2))` answered a
    float complex; `expt.29` compares the two with EQL and could only
    pass while EQL conflated complex part types numerically.
    """
    from fractions import Fraction
    if imag == 0 and isinstance(real, (int, Fraction)):
        return real
    return LispComplex(real, imag)


def _exact_div(a, b):
    """`a/b` as an exact rational (Fraction) when both are integers/Fractions,
    otherwise fall back to Python division.
    """
    from fractions import Fraction
    if b == 0:
        from fclpy.lispfunc.evaluation_conditions import signal_condition
        signal_condition(lisptype.DivisionByZero("DIVISION-BY-ZERO"))
        return
    if isinstance(a, int) and isinstance(b, int):
        if a == 0:
            return 0
        if b == 1:
            return a
        return Fraction(a, b)
    if isinstance(a, Fraction) or isinstance(b, Fraction):
        return Fraction(a) / Fraction(b)
    return a / b


def _lisp_complex_add(a, b):
    ar, ai = _complex_parts(a)
    br, bi = _complex_parts(b)
    new_real = ar + br
    new_imag = ai + bi
    return _lisp_complex_wrap(new_real, new_imag)


def _lisp_complex_sub(a, b):
    ar, ai = _complex_parts(a)
    br, bi = _complex_parts(b)
    new_real = ar - br
    new_imag = ai - bi
    return _lisp_complex_wrap(new_real, new_imag)


def _lisp_complex_mul(a, b):
    ar, ai = _complex_parts(a)
    br, bi = _complex_parts(b)
    new_real = ar * br - ai * bi
    new_imag = ar * bi + ai * br
    return _lisp_complex_wrap(new_real, new_imag)


def _lisp_complex_div(a, b):
    ar, ai = _complex_parts(a)
    br, bi = _complex_parts(b)
    if br == 0 and bi == 0:
        from fclpy.lispfunc.evaluation_conditions import signal_condition
        signal_condition(lisptype.DivisionByZero(f"DIVISION-BY-ZERO on complex /"))
        return
    both_complex = isinstance(a, (LispComplex, complex)) and \
                   isinstance(b, (LispComplex, complex))
    # Every result below goes through `_lisp_complex_wrap`, not a bare
    # `LispComplex`: CLHS 12.1.5.3 forbids a rational complex with zero
    # imag, so `(/ #c(1 2) #c(1 2))` is the integer 1, while float-parts
    # results stay complex.
    if bi == 0:
        if not both_complex and ai == 0:
            return _exact_div(ar, br)
        if ai == 0:
            return _lisp_complex_wrap(_exact_div(ar, br), 0)
        return _lisp_complex_wrap(_exact_div(ar, br), _exact_div(ai, br))
    if ai == 0 and not both_complex:
        denom = br * br + bi * bi
        return _lisp_complex_wrap(_exact_div(ar * br, denom),
                                  _exact_div(-ar * bi, denom))
    denom = br * br + bi * bi
    new_real = (ar * br + ai * bi)
    new_imag = (ai * br - ar * bi)
    return _lisp_complex_wrap(_exact_div(new_real, denom),
                              _exact_div(new_imag, denom))


def _lisp_complex_neg(a):
    ar, ai = _complex_parts(a)
    new_real = -ar
    new_imag = -ai
    # `(- #c(0 0))` returns a complex even when the result has imag 0:
    # `(- (- #c(0 0)))` (minus.1) has to EQL `#c(0 0)`, not 0. The
    # complex-coalescing rule (CLHS 12.1.5.1) is about COMPLEX's
    # two-arg constructor, not about negation. For *plain* Python
    # complex (the reader's `#c(...)` still returns those), the result
    # is also a complex so `(- (- #c(0.0 0.0)))` round-trips.
    if isinstance(a, (LispComplex, complex)):
        if _is_lisp_complex(a):
            return LispComplex(new_real, new_imag)
        return complex(new_real, new_imag)
    # Plain real
    if new_imag == 0:
        return new_real
    return LispComplex(new_real, new_imag)


@_registry.cl_function('IMAGPART')
def imagpart(number):
    """Return imaginary part of complex number (CLHS 12.1.5.3).

    For a real, the imag part is 0; the *type* of that 0 must match the
    product `(* 0 x)` for the same `x`, otherwise `imagpart.4` (which
    EQLs `(imagpart x)` against `(* 0 x)` for every `x` in `*reals*`)
    fails. For a float, `(* 0 x)` is `0.0`; for a Fraction, it is
    `Fraction(0, 1)`; for an int, it is `0`.

    A non-number signals a TYPE-ERROR. The previous `imagpart` returned
    `0` for everything that was neither a Python `complex` nor a float,
    so `(imagpart 'foo)` silently answered `0` and `imagpart.error.3`
    (a `check-type-error` probe) saw it as a success.
    """
    if isinstance(number, LispComplex):
        return number.imag
    if isinstance(number, complex):
        return number.imag
    if isinstance(number, float):
        return 0.0
    if isinstance(number, int):
        return 0
    if isinstance(number, Fraction):
        return Fraction(0, 1)
    _ensure_number(number, 'IMAGPART')


@_registry.cl_function('REALPART')
def realpart(number):
    """Return real part of complex number (CLHS 12.1.5.3).

    A non-number signals a TYPE-ERROR. The previous `realpart` returned
    the value itself for anything that was not a Python `complex`, so
    `(realpart 'foo)` silently answered `FOO` and `realpart.error.3`
    (a `check-type-error` probe) saw it as a success. The `complex`
    branch is split between Python's `complex` and our own `LispComplex`
    so a part that was held as an int/Fraction by the constructor is
    returned as an int/Fraction, not as a float (`realpart.1`/`2`/`3`
    EQL `(realpart (complex x 0))` against `x` for every `x` in `*reals*`).
    """
    if isinstance(number, LispComplex):
        return number.real
    if isinstance(number, complex):
        return number.real
    if isinstance(number, (int, float, Fraction)):
        return number
    _ensure_number(number, 'REALPART')


@_registry.cl_function('CONJUGATE')
def conjugate(number):
    """Return complex conjugate, preserving the type of parts."""
    if isinstance(number, LispComplex):
        # LispComplex preserves part types; conjugate without converting to float
        return LispComplex(number.real, -number.imag)
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
    """Arithmetic shift left/right.

    Shifts i left by count bits (or right if count is negative).
    Both i and count must be integers.
    """
    _ensure_integer(i, 'ASH')
    _ensure_integer(count, 'ASH')
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
    """Deposit byte: newbyte's low `size` bits, shifted to `position`."""
    size, position = bytespec
    mask = (1 << size) - 1
    cleared = integer & ~(mask << position)
    return cleared | ((newbyte & mask) << position)


@_registry.cl_function('DEPOSIT-FIELD')
def deposit_field(newbyte, bytespec, integer):
    """Deposit field: newbyte's bits already *at* the byte's positions
    replace integer's bits there -- unlike DPB there is no shift, so
    bits of newbyte outside [position, position+size) are ignored."""
    size, position = bytespec
    mask = ((1 << size) - 1) << position
    return (integer & ~mask) | (newbyte & mask)


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
    if len(args) == 0:
        raise lisptype.LispProgramError("= requires at least one argument")
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
    if len(args) == 0:
        raise lisptype.LispProgramError("< requires at least one argument")
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
    if len(args) == 0:
        raise lisptype.LispProgramError("> requires at least one argument")
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
    if len(args) == 0:
        raise lisptype.LispProgramError("<= requires at least one argument")
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
    if len(args) == 0:
        raise lisptype.LispProgramError(">= requires at least one argument")
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
    if len(args) == 0:
        raise lisptype.LispProgramError("/= requires at least one argument")
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
    result = args[0]
    for x in args[1:]:
        if isinstance(result, (LispComplex, complex)) or isinstance(x, (LispComplex, complex)):
            result = _lisp_complex_add(result, x)
        else:
            result = result + x
    return result


@_registry.cl_function('-')
def _s_minus_(*args):
    """Subtraction operator (-)."""
    if not args:
        raise lisptype.LispProgramError("- requires at least one argument")
    if len(args) == 1:
        if isinstance(args[0], (LispComplex, complex)):
            return _lisp_complex_neg(args[0])
        return -args[0]
    result = args[0]
    for x in args[1:]:
        if isinstance(result, (LispComplex, complex)) or isinstance(x, (LispComplex, complex)):
            result = _lisp_complex_sub(result, x)
        else:
            result = result - x
    return result


@_registry.cl_function('*')
def _s_star_(*args):
    """Multiplication operator (*)."""
    if not args:
        return 1
    result = args[0]
    for x in args[1:]:
        if isinstance(result, (LispComplex, complex)) or isinstance(x, (LispComplex, complex)):
            result = _lisp_complex_mul(result, x)
        else:
            result = result * x
    return result


@_registry.cl_function('/')
def _s_slash_(*args):
    """Division operator (/).

    When dividing integers, returns an exact ratio (Fraction) if result
    is not exact. Automatically reduces fractions and normalizes signs.
    Signals DIVISION-BY-ZERO when dividing by zero.

    Complex division: CLHS 12.1.5.2's formula `(a+bi)/c = (a+bi)/c` and
    `(/ (complex a b)) = (complex a (-b)) / (a^2 + b^2)`. `/.6`, `/.9`,
    `/.10` and `/.11` compare the result against `(complex (/ a m) (/
    (- b) m))` where `m = a^2 + b^2` -- the parts must stay rational,
    not float. `_lisp_complex_div` computes via the conjugate and
    `_exact_div` so each part is a `Fraction` when the inputs are
    integers and a `float` only when at least one input is a float.
    """
    from fractions import Fraction

    if not args:
        raise lisptype.LispProgramError("/ requires at least one argument")

    try:
        if len(args) == 1:
            # Reciprocal: (/ x) = 1/x
            x = args[0]
            if isinstance(x, (LispComplex, complex)):
                return _lisp_complex_div(1, x)
            if isinstance(x, int) and x != 0:
                return _canonicalize_rational(Fraction(1, x))
            return 1 / x

        result = args[0]
        for x in args[1:]:
            if isinstance(result, (LispComplex, complex)) or isinstance(x, (LispComplex, complex)):
                result = _lisp_complex_div(result, x)
            elif isinstance(result, int) and isinstance(x, int) and x != 0:
                result = Fraction(result, x)
            elif isinstance(result, Fraction) and isinstance(x, int) and x != 0:
                result = result / x
            elif isinstance(result, Fraction) and isinstance(x, Fraction):
                result = result / x
            else:
                result = result / x

        if isinstance(result, (LispComplex, complex)):
            return result
        return _canonicalize_rational(result)
    except ZeroDivisionError:
        # Signal DIVISION-BY-ZERO condition
        from fclpy.lispfunc.evaluation_conditions import signal_condition
        signal_condition(lisptype.DivisionByZero(
            f"Division by zero"))
        return


@_registry.cl_function('1+')
def _s_one_s_plus_(x):
    """Increment by one operator (1+)."""
    if isinstance(x, (LispComplex, complex)):
        return _lisp_complex_add(x, 1)
    return x + 1


@_registry.cl_function('1-')
def _s_one_s_minus_(x):
    """Decrement by one operator (1-)."""
    if isinstance(x, (LispComplex, complex)):
        return _lisp_complex_sub(x, 1)
    return x - 1


# Fixed arithmetic limits.
#
# The bound comes from `typespec.py`, which owns it, rather than from a literal
# here. CLHS 12.1.1.1 ties the constant and the type together --
# `(typep most-positive-fixnum 'fixnum)` must be true -- so a literal in this
# file is a second home for one fact, and the *third* copy (a local
# `2**29 - 1` inside `comparison.typep`) is what made that form answer NIL.
def most_positive_fixnum():
    """Most positive fixnum (CLHS 12.1.1.1)."""
    from fclpy.typespec import MOST_POSITIVE_FIXNUM
    return MOST_POSITIVE_FIXNUM


def most_negative_fixnum():
    """Most negative fixnum (CLHS 12.1.1.1)."""
    from fclpy.typespec import MOST_NEGATIVE_FIXNUM
    return MOST_NEGATIVE_FIXNUM


_BOOLE_OPS = {
    2: lambda x, y: x,               # BOOLE-1
    3: lambda x, y: y,               # BOOLE-2
    6: lambda x, y: x & y,           # BOOLE-AND
    7: lambda x, y: x | y,           # BOOLE-IOR
    8: lambda x, y: x ^ y,           # BOOLE-XOR
    4: lambda x, y: ~x,              # BOOLE-C1
    5: lambda x, y: ~y,              # BOOLE-C2
    0: lambda x, y: 0,               # BOOLE-CLR
    1: lambda x, y: -1,              # BOOLE-SET
    9: lambda x, y: ~(x ^ y),        # BOOLE-EQV
    10: lambda x, y: ~(x & y),       # BOOLE-NAND
    11: lambda x, y: ~(x | y),       # BOOLE-NOR
    12: lambda x, y: (~x) & y,       # BOOLE-ANDC1
    13: lambda x, y: x & (~y),       # BOOLE-ANDC2
    14: lambda x, y: (~x) | y,       # BOOLE-ORC1
    15: lambda x, y: x | (~y),       # BOOLE-ORC2
}


@_registry.cl_function('BOOLE')
def boole(op, integer1, integer2):
    """Bitwise boolean operation selected by `op` (CLHS 12.1.4).

    `op` must be one of the sixteen `BOOLE-*` constant variables
    (`lispenv.STANDARD_CONSTANTS`) -- an out-of-range value is a TYPE-ERROR
    (`boole.error.5`), as is a non-INTEGER `integer1`/`integer2`
    (`boole.error.6`/`.7`). This previously hardcoded three op values (1, 2,
    6 -- none of which are the real BOOLE-AND/BOOLE-IOR/BOOLE-XOR codes 6, 7,
    8) and silently returned 0 for the other thirteen, which is not merely
    incomplete: BOOLE-AND and BOOLE-1 both evaluated (as *variables*, via
    `core.py`'s wrong function-based "constants") to the Python int 1, so
    even a caller that funcalled them could not tell the two operations
    apart.
    """
    op = _ensure_integer_range(op, 'BOOLE', _BOOLE_OPS)
    x = _ensure_integer(integer1, 'BOOLE')
    y = _ensure_integer(integer2, 'BOOLE')
    return _BOOLE_OPS[op](x, y)


def _ensure_integer_range(op, func_name, table):
    if isinstance(op, bool) or not isinstance(op, int) or op not in table:
        raise lisptype.LispTypeError(
            f"{func_name}: {op!r} is not a valid BOOLE op code",
            expected_type="(INTEGER 0 15)", actual_value=op)
    return op


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
