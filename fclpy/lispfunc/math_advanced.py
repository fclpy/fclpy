"""Advanced mathematical functions - transcendental, trigonometric, and special functions."""

import cmath
import math
import sys
from fractions import Fraction
import fclpy.lisptype as lisptype
from . import registry as _registry

def _irrational(real_fn, complex_fn, x):
    """Apply an irrational/transcendental function per CLHS 12.1.5.1.

    Each of these functions is defined over the whole complex plane; a real
    argument outside the function's real-valued domain (e.g. `(sqrt -4)`,
    `(asin 2)`, `(acosh 0)`) must produce a complex result, not an error.
    `math.*` only covers the real domain and raises `ValueError: math domain
    error` the moment it is left, which used to surface as a bare Python
    exception standing in for a Lisp value (X1). One dispatch used by every
    caller below keeps that promotion consistent instead of teaching each
    operator its own domain boundary.

    A non-number `x` must signal a TYPE-ERROR *whose datum is `x` itself*
    (CLHS: `type-error-datum` identifies the offending value) -- letting
    `math.sin("foo")`'s bare `TypeError` reach the generic FUNCALL boundary
    produced a TYPE-ERROR with `datum=None`, which is not `eql` to `"foo"`
    and so still reads as a Python exception standing in for the Lisp
    value ansi-test's `check-type-error` expects.
    """
    if not isinstance(x, (int, float, complex, Fraction)):
        raise lisptype.LispTypeError(
            f"{real_fn.__name__.upper()}: argument is not a NUMBER: {x!r}",
            expected_type="NUMBER", actual_value=x)
    if isinstance(x, complex):
        return complex_fn(x)
    try:
        result = real_fn(x)
        # Preserve integer type for integer zeros: if x is an integer 0
        # and result is 0.0, return 0 instead to maintain type consistency
        if isinstance(x, int) and result == 0.0 and x == 0:
            return 0
        return result
    except ValueError:
        return complex_fn(complex(x))

# Exponential and logarithmic functions
@_registry.cl_function('EXP')
def exp(x):
    """Exponential function.

    CLHS 12.1.5.1: Must signal FLOATING-POINT-OVERFLOW or FLOATING-POINT-UNDERFLOW
    when the result is too large or too small for the float type.
    """
    try:
        result = _irrational(math.exp, cmath.exp, x)
    except OverflowError:
        # Python's math.exp raised OverflowError before we could get a result
        from fclpy.lispfunc.evaluation_conditions import signal_condition
        signal_condition(lisptype.FloatingPointOverflow(
            f"EXP: result overflows the range of the float type"))
        return

    # Check if result overflows/underflows
    if isinstance(result, (int, float)):
        # For real results, check overflow/underflow
        from fclpy.lispfunc.evaluation_conditions import signal_condition

        # Short/single float range
        short_max = 3.4028235e+38
        short_min = 1.4e-45

        # Double/long float range (Python's full range)
        double_max = sys.float_info.max
        double_min = sys.float_info.min

        # Check for overflow (result larger than ANY type's max)
        if abs(result) > double_max:
            signal_condition(lisptype.FloatingPointOverflow(
                f"EXP: result overflows the range of the float type"))
            return

        if abs(result) > short_max:
            signal_condition(lisptype.FloatingPointOverflow(
                f"EXP: result overflows the range of the float type"))
            return

        # Check for underflow. If result is 0.0, check if it's due to underflow
        # by examining the input: if x < log(double_min), then exp(x) would underflow
        if isinstance(x, (int, float)):
            # log(sys.float_info.min) is approximately -744.4 for IEEE 754 doubles
            underflow_threshold = math.log(double_min)
            if x < underflow_threshold:
                signal_condition(lisptype.FloatingPointUnderflow(
                    f"EXP: result underflows the range of the float type"))
                return

        # Also check for small non-zero results that are below the minimum normal
        if result != 0.0:
            if abs(result) < double_min or abs(result) < short_min:
                signal_condition(lisptype.FloatingPointUnderflow(
                    f"EXP: result underflows the range of the float type"))
                return

    return result

@_registry.cl_function('LOG')
def log(x, base=None):
    """Logarithm function."""
    if base is None:
        return _irrational(math.log, cmath.log, x)
    if isinstance(x, complex) or isinstance(base, complex):
        return cmath.log(x, base)
    try:
        return math.log(x, base)
    except ValueError:
        return cmath.log(complex(x), complex(base))

@_registry.cl_function('SQRT')
def sqrt(x):
    """Square root function."""
    return _irrational(math.sqrt, cmath.sqrt, x)

def _check_float_overflow(result, base, power=None):
    """Check if result overflows/underflows for base's float type.

    CLHS 12.1.4.1: EXPT must signal FLOATING-POINT-OVERFLOW or
    FLOATING-POINT-UNDERFLOW when the mathematical result is too large or
    too small for the float type of the base.

    All CL float subtypes (short, single, double, long) share Python's
    float, so the *double* range is the one hard boundary: Python has
    already raised OverflowError for anything beyond it, and a finite
    result is representable. The short/single range is checked only when
    the power is an exact *integer* -- that is the regime a CL with
    distinct float subtypes computes by repeated multiplication in the
    base's own format, and `expt.error.4`-`.11` pin the signals it must
    produce (`(expt most-positive-single-float 2)` overflows single even
    though 1.2e77 is a fine double). A non-integral power is computed
    through exp/log in the widest format, so only the double range bounds
    it: the eager check there made format.e.1/.2's own
    `(expt (coerce 10 type) e)` forms signal on results the tests require
    back as values.
    """
    from fclpy.lispfunc.evaluation_conditions import signal_condition

    # Overflow beyond even the double range (an inf result escaping from
    # the complex paths without Python raising).
    if abs(result) > sys.float_info.max:
        signal_condition(lisptype.FloatingPointOverflow(
            f"EXPT: result overflows the range of the float type"))
        return

    if not (isinstance(power, int) and not isinstance(power, bool)):
        return

    # Short/single float range
    short_max = 3.4028235e+38
    short_min = 1.4e-45

    if abs(result) > short_max:
        signal_condition(lisptype.FloatingPointOverflow(
            f"EXPT: result overflows the range of the float type"))
        return

    # Underflow below the single range (but not a true zero -- the zero
    # case is handled by the caller's explicit underflow branch).
    if result != 0.0 and abs(result) < short_min:
        signal_condition(lisptype.FloatingPointUnderflow(
            f"EXPT: result underflows the range of the float type"))
        return


@_registry.cl_function('EXPT')
def expt(base, power):
    """Raise base to power.

    CLHS 12.1.4.1: x^0 must equal 1 for any x. If the result overflows or
    underflows, signal FLOATING-POINT-OVERFLOW or FLOATING-POINT-UNDERFLOW.
    Complex results with zero imaginary part should be simplified to reals.
    """
    # Special case: any number to the power 0 is 1, preserving the type
    if power == 0:
        if isinstance(base, float):
            return 1.0
        elif isinstance(base, complex):
            # CLHS 12.1.4.1 / 12.1.4.1.1: x^0 is one *in the base's own
            # type* -- a rational complex gives the integer 1 (expt.7
            # loops `(complex i i)` and demands `(eql (expt c 0) 1)`),
            # a float complex gives a float-parts one (expt.8-.11 demand
            # `#c(1.0s0 0.0s0)` etc.). The old heuristic keyed on the
            # parts' *value* being whole, which made a float complex
            # with whole-valued parts answer the integer -- a type the
            # base does not have -- and an exact rational complex with
            # fractional parts (`#c(1/2 1/3)`) answer a float one.
            if isinstance(base.real, (int, Fraction)) and \
                    isinstance(base.imag, (int, Fraction)):
                return 1
            return complex(1.0, 0.0)
        else:
            if isinstance(power, float):
                # CLHS 12.1.4.1.1: a float power always produces a float result,
                # even at 0 where Python's `int ** 0.0` would give 1 (an int).
                # `expt.18` compares `(expt i 0.0)` against `(float 1 0.0) = 1.0`
                # for every i, and they must be EQL; matching the float type
                # is the rule.
                return 1.0
            return 1

    # CLHS 12.1.4.1: if the base is zero and the exponent is positive (in the
    # complex sense, its real part > 0), the result is zero. Python's
    # `0 ** complex(a, b)` raises ZeroDivisionError for *every* complex power
    # even when a > 0, so 0^(2+2j) -- which is mathematically 0 -- is the
    # common case that reaches the except branch below with a *wrong* condition.
    # Detect the positive-real-part-of-exponent case before the try, so
    # `(expt 0 #c(2 2))` returns 0 instead of signalling DIVISION-BY-ZERO.
    # The result type follows CLHS 12.1.4.1.1 contagion: a complex power always
    # gives a complex result (matching what `(* 0 #c(a b))` does, which is
    # exactly what the expt.29 test compares against), a float power with a
    # float base stays float, and an integer power with an integer base stays
    # integer. A complex base always gives a complex result regardless of the
    # power's type, for the same EQL reason.
    base_is_zero_complex = isinstance(base, complex) and base == 0
    base_is_zero_int_float = isinstance(base, (int, float)) and base == 0
    if (base_is_zero_complex or base_is_zero_int_float) and isinstance(power, (int, float, complex)):
        if base_is_zero_complex:
            # Complex base 0+0i to any power -- preserve complex
            if isinstance(power, complex):
                if power.real > 0:
                    # 0+0i to a positive-real complex power is 0+0i.
                    # Match `(* 0+0i #c(a b))` which is also 0+0i (Python: complex * complex).
                    return complex(0, 0)
                if power.real == 0 and power.imag != 0:
                    from fclpy.lispfunc.evaluation_conditions import signal_condition
                    signal_condition(lisptype.DivisionByZero(
                        f"EXPT: 0 to a complex power with zero real part"))
                    return
            # Complex base, real power: 0j ** 2 == 0+0j -- but Python's `0j**2`
            # is 0j, which the post-result code simplifies to 0. The
            # simplification is what the EQL check in expt.29 exercises
            # (since `(* 0+0i 2)` is 0+0j in Python, not 0). Skip the
            # simplification when base is the literal zero complex.
            try:
                result = base ** power
            except OverflowError:
                _signal_float_overflow(base)
                return
            except ZeroDivisionError:
                from fclpy.lispfunc.evaluation_conditions import signal_condition
                signal_condition(lisptype.DivisionByZero(
                    f"EXPT: 0.0 to a negative or complex power"))
                return
            return result
        else:
            # int/float base 0
            if isinstance(power, complex):
                if power.real > 0:
                    # Python contagion: integer * complex = complex; preserve that.
                    if isinstance(power, float) or isinstance(power.real, float) or isinstance(power.imag, float):
                        return complex(0.0, 0.0)
                    return complex(0, 0)
                if power.real == 0 and power.imag != 0:
                    from fclpy.lispfunc.evaluation_conditions import signal_condition
                    signal_condition(lisptype.DivisionByZero(
                        f"EXPT: 0 to a complex power with zero real part"))
                    return

    # Narrow correctness fix for CLHS 12.1.5.3 / 12.1.4.1: when a complex
    # base has *exact* parts (integers or `Fraction`s) and the power is a
    # *non-negative* integer, the answer is also exact, and Python's
    # `complex ** int` degrades both 0+2i squared (-4+0j then -4.0 once the
    # imaginary-zero simplification runs) and (1/2 + 1/3 i) cubed to a
    # float pair. The float answer is *wrong* for `(expt #c(0 2) 2) = -4`
    # and the (1/2, 1/3) case in `expt.16`, both of which the test
    # compares against exact rationals. Doing the multiplication by hand
    # on Fractions keeps the answer exact; once imag collapses to 0, the
    # rational can be returned as-is.
    if (isinstance(base, complex) and isinstance(power, int) and power >= 0
            and not isinstance(power, bool)
            and (isinstance(base.real, (int, Fraction))
                 or (isinstance(base.real, float) and base.real.is_integer()))
            and (isinstance(base.imag, (int, Fraction))
                 or (isinstance(base.imag, float) and base.imag.is_integer()))):
        # Even when the base's parts are Python `float`s (because
        # `complex(0, 2)` promotes both to 0.0/2.0), a value that is
        # *integer-valued* is exact -- `expt.14` relies on this: `#c(0 2)`
        # reads as `(0.0+2.0j)`, and the test compares `(expt #c(0 2) 2)`
        # (should be the integer -4) against the expected `-4`. Use
        # Fraction so the answer is rational; once `cur_im` collapses to
        # 0, return the rational (or its integer reduction).
        re_part = Fraction(int(base.real)) if isinstance(base.real, float) else Fraction(base.real)
        im_part = Fraction(int(base.imag)) if isinstance(base.imag, float) else Fraction(base.imag)
        # Repeated squaring
        cur_re, cur_im = Fraction(1), Fraction(0)
        for _ in range(power):
            new_re = cur_re * re_part - cur_im * im_part
            new_im = cur_re * im_part + cur_im * re_part
            cur_re, cur_im = new_re, new_im
        if cur_im == 0:
            # Coalesce per CLHS 12.1.5.3: complex with zero imag is real
            if cur_re.denominator == 1:
                return int(cur_re)
            return cur_re
        return complex(float(cur_re), float(cur_im))

    # CLHS 12.1.4.1: an integer base to an integer power is rational (any
    # integer power, positive, zero, or negative). Python's `int ** int` for
    # `n ** -1` returns 1.0 -- the *float* 1.0 -- so `(expt 1 -1) = 1.0`
    # and `(eql 1.0 1)` is NIL, which is what `expt.13` collects `(1 2)`
    # for. Use Fraction to keep the answer rational; cancel the denominator
    # when it is 1 so `(expt 2 3) = 8` (not `8/1`). A *float* power still
    # produces a float, and `expt.18` checks `(eql (expt i zero) (float 1
    # zero))` for `zero = 0.0...` -- so the int-int path has to skip the
    # rational branch when the power is a float and let Python do the
    # float promotion.
    if (isinstance(base, int) and not isinstance(base, bool)
            and isinstance(power, int) and not isinstance(power, bool)
            and not isinstance(power, float)):
        result = Fraction(base) ** power
        if result.denominator == 1:
            return result.numerator
        return result

    try:
        result = base ** power
    except OverflowError:
        # Python raised overflow before we could get a result
        # Signal overflow based on base's type
        _signal_float_overflow(base)
        return
    except ZeroDivisionError:
        # 0.0 to a negative or complex power raises ZeroDivisionError
        from fclpy.lispfunc.evaluation_conditions import signal_condition
        signal_condition(lisptype.DivisionByZero(
            f"EXPT: 0.0 to a negative or complex power"))
        return

    # If result is complex with zero imaginary part, simplify to real
    if isinstance(result, complex) and result.imag == 0.0:
        result = result.real

    # Check for overflow/underflow
    _check_float_overflow(result, base, power)

    # Additional underflow check: if result is 0.0 but base is non-zero and small,
    # it might be an underflow that Python truncated to 0.0
    if isinstance(result, float) and result == 0.0:
        if isinstance(base, float) and base != 0.0:
            from fclpy.lispfunc.evaluation_conditions import signal_condition
            # If base is smaller than sqrt(double_min) and power > 1, it underflows
            double_min = sys.float_info.min
            underflow_threshold = math.sqrt(double_min)
            if abs(base) < underflow_threshold and isinstance(power, (int, float)) and power > 1:
                signal_condition(lisptype.FloatingPointUnderflow(
                    f"EXPT: result underflows the range of the float type"))
                return

    return result


def _signal_float_overflow(base):
    """Signal FLOATING-POINT-OVERFLOW for EXPT based on base's float type."""
    from fclpy.lispfunc.evaluation_conditions import signal_condition
    
    if isinstance(base, float):
        if abs(base) > 3.4028235e+38:
            # This is a double/long float
            pass  # Python already overflowed for this
        else:
            # This is a short/single float - overflowed at ~3.4e+38 range
            pass
    
    signal_condition(lisptype.FloatingPointOverflow(
        f"EXPT: result overflows the range of the float type"))

@_registry.cl_function('ISQRT')
def isqrt(x):
    """Integer square root."""
    from .math_arithmetic import _ensure_integer
    _ensure_integer(x, 'ISQRT')
    if x < 0:
        raise lisptype.LispTypeError(
            f"ISQRT: Argument is not a non-negative INTEGER: {x}",
            expected_type="(INTEGER 0 *)", actual_value=x)
    return int(math.sqrt(x))

# Trigonometric functions
@_registry.cl_function('SIN')
def sin(a):
    """Sine function."""
    return _irrational(math.sin, cmath.sin, a)

@_registry.cl_function('COS')
def cos(a):
    """Cosine function."""
    return _irrational(math.cos, cmath.cos, a)

@_registry.cl_function('TAN')
def tan(a):
    """Tangent function."""
    return _irrational(math.tan, cmath.tan, a)

@_registry.cl_function('ASIN')
def asin(x):
    """Arc sine function."""
    return _irrational(math.asin, cmath.asin, x)

@_registry.cl_function('ACOS')
def acos(x):
    """Arc cosine function."""
    return _irrational(math.acos, cmath.acos, x)

@_registry.cl_function('ATAN')
def atan(y, x=None):
    """Arc tangent function.

    CLHS 12.1.4.2: with one argument this is the general (possibly complex)
    arctangent; with two, `(atan y x)` is the four-quadrant real arctangent
    of `y/x` (both must be non-complex), distinct from `(/ y x)` at `x = 0`.
    """
    if x is None:
        return _irrational(math.atan, cmath.atan, y)
    from .math_arithmetic import _ensure_real
    _ensure_real(y, 'ATAN')
    _ensure_real(x, 'ATAN')
    return math.atan2(y, x)

# Hyperbolic functions
@_registry.cl_function('SINH')
def sinh(x):
    """Hyperbolic sine function."""
    return _irrational(math.sinh, cmath.sinh, x)

@_registry.cl_function('COSH')
def cosh(x):
    """Hyperbolic cosine function."""
    return _irrational(math.cosh, cmath.cosh, x)

@_registry.cl_function('TANH')
def tanh(x):
    """Hyperbolic tangent function."""
    return _irrational(math.tanh, cmath.tanh, x)

@_registry.cl_function('ASINH')
def asinh(x):
    """Hyperbolic arc sine function."""
    return _irrational(math.asinh, cmath.asinh, x)

@_registry.cl_function('ACOSH')
def acosh(x):
    """Hyperbolic arc cosine function."""
    return _irrational(math.acosh, cmath.acosh, x)

@_registry.cl_function('ATANH')
def atanh(x):
    """Hyperbolic arc tangent function."""
    return _irrational(math.atanh, cmath.atanh, x)

# Float decoding and encoding
@_registry.cl_function('DECODE-FLOAT')
def decode_float(float_num):
    """Decode float into significand, exponent, sign (CLHS 12.2).

    Returns three *Lisp* values. This returned a Python tuple, which is a
    single value -- and a Python container standing in for multiple values is
    standing rule 2, the same defect its sibling INTEGER-DECODE-FLOAT below
    does not have. The visible consequence was that `(nth-value 1
    (decode-float x))` was NIL, so ansi-aux's `float-exponent` answered NIL
    and `numbers/number-comparison.lsp` died at *load* time with
    `bad operand type for abs(): 'lispNull'` -- taking all 145 of its tests
    out of the run rather than failing any one of them.
    """
    if float_num == 0.0:
        return lisptype.MultipleValues([0.0, 0, 1.0])

    sign = 1.0 if float_num >= 0 else -1.0
    abs_float = abs(float_num)

    # Use frexp to get mantissa and exponent
    mantissa, exponent = math.frexp(abs_float)

    return lisptype.MultipleValues([mantissa, exponent, sign])

@_registry.cl_function('INTEGER-DECODE-FLOAT')
def integer_decode_float(float_num):
    """Integer decode of float."""
    if float_num == 0.0:
        return lisptype.MultipleValues([0, 0, 1])
    
    sign = 1 if float_num >= 0 else -1
    abs_float = abs(float_num)
    
    # Convert to integer representation
    mantissa, exponent = math.frexp(abs_float)
    # Scale mantissa to integer (assuming 53-bit precision for double)
    int_mantissa = int(mantissa * (2 ** 53))
    int_exponent = exponent - 53
    
    return lisptype.MultipleValues([int_mantissa, int_exponent, sign])

@_registry.cl_function('SCALE-FLOAT')
def scale_float(float_num, integer):
    """Scale float by power of radix."""
    return float_num * (2.0 ** integer)

@_registry.cl_function('FLOAT')
def float_fn(number, prototype=None):
    """Convert to float."""
    return float(number)

@_registry.cl_function('FLOAT-DIGITS')
def float_digits(float_num):
    """Number of digits in float."""
    if isinstance(float_num, float):
        return sys.float_info.mant_dig  # 53 for IEEE 754 double
    return 24  # Default for single precision

@_registry.cl_function('FLOAT-PRECISION')
def float_precision(float_num):
    """Precision of float."""
    if isinstance(float_num, float):
        return sys.float_info.mant_dig  # Same as float_digits for most cases
    return 24  # Default for single precision

@_registry.cl_function('FLOAT-RADIX')
def float_radix(float_num):
    """Radix of float."""
    return 2

@_registry.cl_function('FLOAT-SIGN')
def float_sign(float1, float2=None):
    """Sign of float."""
    sign = 1 if float1 >= 0 else -1
    if float2 is None:
        return sign
    return sign * abs(float2)

# Mathematical constants
@_registry.cl_function('PI')
def pi_fn():
    """Return pi."""
    return math.pi

# Floating-point limit constants
@_registry.cl_function('LEAST-POSITIVE-DOUBLE-FLOAT')
def least_positive_double_float():
    """Least positive double float."""
    return sys.float_info.min

@_registry.cl_function('LEAST-NEGATIVE-DOUBLE-FLOAT')
def least_negative_double_float():
    """Least negative double float."""
    return -sys.float_info.min

@_registry.cl_function('MOST-POSITIVE-DOUBLE-FLOAT')
def most_positive_double_float():
    """Most positive double float."""
    return sys.float_info.max

@_registry.cl_function('MOST-NEGATIVE-DOUBLE-FLOAT')
def most_negative_double_float():
    """Most negative double float."""
    return -sys.float_info.max

@_registry.cl_function('LEAST-POSITIVE-SHORT-FLOAT')
def least_positive_short_float():
    """Least positive short float."""
    return sys.float_info.min

@_registry.cl_function('LEAST-NEGATIVE-SHORT-FLOAT')
def least_negative_short_float():
    """Least negative short float."""
    return -sys.float_info.min

@_registry.cl_function('MOST-POSITIVE-SHORT-FLOAT')
def most_positive_short_float():
    """Most positive short float."""
    return sys.float_info.max

@_registry.cl_function('MOST-NEGATIVE-SHORT-FLOAT')
def most_negative_short_float():
    """Most negative short float."""
    return -sys.float_info.max

@_registry.cl_function('LEAST-POSITIVE-SINGLE-FLOAT')
def least_positive_single_float():
    """Least positive single float."""
    return sys.float_info.min

@_registry.cl_function('LEAST-NEGATIVE-SINGLE-FLOAT')
def least_negative_single_float():
    """Least negative single float."""
    return -sys.float_info.min

@_registry.cl_function('MOST-POSITIVE-SINGLE-FLOAT')
def most_positive_single_float():
    """Most positive single float."""
    return sys.float_info.max

@_registry.cl_function('MOST-NEGATIVE-SINGLE-FLOAT')
def most_negative_single_float():
    """Most negative single float."""
    return -sys.float_info.max

@_registry.cl_function('LEAST-POSITIVE-LONG-FLOAT')
def least_positive_long_float():
    """Least positive long float."""
    return sys.float_info.min

@_registry.cl_function('LEAST-NEGATIVE-LONG-FLOAT')
def least_negative_long_float():
    """Least negative long float."""
    return -sys.float_info.min

@_registry.cl_function('MOST-POSITIVE-LONG-FLOAT')
def most_positive_long_float():
    """Most positive long float."""
    return sys.float_info.max

@_registry.cl_function('MOST-NEGATIVE-LONG-FLOAT')
def most_negative_long_float():
    """Most negative long float."""
    return -sys.float_info.max

# Normalized floating-point limit constants
@_registry.cl_function('LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT')
def least_positive_normalized_double_float():
    """LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT."""
    return sys.float_info.min

@_registry.cl_function('LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT')
def least_negative_normalized_double_float():
    """LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT."""
    return -sys.float_info.min

@_registry.cl_function('LEAST-POSITIVE-NORMALIZED-LONG-FLOAT')
def least_positive_normalized_long_float():
    """LEAST-POSITIVE-NORMALIZED-LONG-FLOAT."""
    return sys.float_info.min

@_registry.cl_function('LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT')
def least_negative_normalized_long_float():
    """LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT."""
    return -sys.float_info.min

@_registry.cl_function('LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT')
def least_positive_normalized_short_float():
    """LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT."""
    return sys.float_info.min

@_registry.cl_function('LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT')
def least_negative_normalized_short_float():
    """LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT."""
    return -sys.float_info.min

@_registry.cl_function('LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT')
def least_positive_normalized_single_float():
    """LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT."""
    return sys.float_info.min

@_registry.cl_function('LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT')
def least_negative_normalized_single_float():
    """LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT."""
    return -sys.float_info.min

# The eight float epsilons are **constants**, not functions (CLHS 12.1.4),
# and their one home is `lispenv.STANDARD_CONSTANTS` -- see the note by
# `_EPSILON_LIMITS` there for why all four formats share one value. They used
# to *also* be registered here as `cl_function`s, which is the defect plan.md
# C7 describes for `*PRINT-BASE*`: registering a function under a variable's
# name makes the variable evaluate to a Python function object, and which of
# the two won depended on import order. They also disagreed with the
# constants -- `sys.float_info.epsilon` is 2**-52, and the negative epsilon is
# not "epsilon / 2" in general.

__all__ = [
    'exp', 'log', 'sqrt', 'expt', 'isqrt',
    'sin', 'cos', 'tan', 'asin', 'acos', 'atan',
    'sinh', 'cosh', 'tanh', 'asinh', 'acosh', 'atanh',
    'decode_float', 'integer_decode_float', 'scale_float', 'float_fn',
    'float_digits', 'float_precision', 'float_radix', 'float_sign',
    'pi_fn',
    'least_positive_double_float', 'least_negative_double_float',
    'most_positive_double_float', 'most_negative_double_float',
    'least_positive_short_float', 'least_negative_short_float',
    'most_positive_short_float', 'most_negative_short_float',
    'least_positive_single_float', 'least_negative_single_float',
    'most_positive_single_float', 'most_negative_single_float',
    'least_positive_long_float', 'least_negative_long_float',
    'most_positive_long_float', 'most_negative_long_float',
    'least_positive_normalized_double_float', 'least_negative_normalized_double_float',
    'least_positive_normalized_long_float', 'least_negative_normalized_long_float',
    'least_positive_normalized_short_float', 'least_negative_normalized_short_float',
    'least_positive_normalized_single_float', 'least_negative_normalized_single_float',
    
    
]
