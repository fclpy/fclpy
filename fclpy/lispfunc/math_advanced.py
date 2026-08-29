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

def _check_float_overflow(result, base):
    """Check if result overflows/underflows for base's float type.
    
    CLHS 12.1.4.1: EXPT must signal FLOATING-POINT-OVERFLOW or 
    FLOATING-UNDERFLOW when the mathematical result is too large or too small
    for the float type of the base.
    
    All CL float subtypes (short, single, double, long) share Python's float,
    but they have different ranges. Since fclpy doesn't preserve float subtype
    info at runtime, we check against all ranges.
    
    Ranges (approximately):
    - short/single: max ~3.4028235e+38, min ~1.4e-45
    - double/long: max ~1.7976931348623157e+308, min ~5e-324
    """
    import math
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
            f"EXPT: result overflows the range of the float type"))
        return
    
    if abs(result) > short_max:
        signal_condition(lisptype.FloatingPointOverflow(
            f"EXPT: result overflows the range of the float type"))
        return
    
    # Check for underflow (result smaller than ANY type's min, but not zero)
    if result != 0.0:
        if abs(result) < double_min or abs(result) < short_min:
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
            # Check if the base is an exact complex (created from exact inputs)
            # In Python, all complex parts are floats, but we can heuristically detect
            # exact complex if both real and imaginary parts are whole numbers.
            # Exact complex (like #C(1 1) in Lisp) => return exact 1
            # Float complex (like #C(1.5f0 2.3f0)) => return #C(1.0 0.0)
            if (isinstance(base.real, int) or base.real == int(base.real)) and \
               (isinstance(base.imag, int) or base.imag == int(base.imag)):
                # Both parts look like whole numbers: treat as exact
                return 1
            else:
                # At least one part has fractional component: return float complex
                return complex(1.0, 0.0)
        else:
            return 1

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
    _check_float_overflow(result, base)

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
