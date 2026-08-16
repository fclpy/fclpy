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
        return real_fn(x)
    except ValueError:
        return complex_fn(complex(x))


# Exponential and logarithmic functions
@_registry.cl_function('EXP')
def exp(x):
    """Exponential function."""
    return _irrational(math.exp, cmath.exp, x)


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


@_registry.cl_function('EXPT')
def expt(base, power):
    """Raise base to power."""
    return base ** power


@_registry.cl_function('ISQRT')
def isqrt(x):
    """Integer square root."""
    if x < 0:
        raise ValueError("isqrt requires non-negative input")
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


# Floating-point epsilon constants
@_registry.cl_function('SHORT-FLOAT-EPSILON')
def short_float_epsilon():
    """Short float epsilon."""
    return sys.float_info.epsilon


@_registry.cl_function('SINGLE-FLOAT-EPSILON')
def single_float_epsilon():
    """Single float epsilon."""
    return sys.float_info.epsilon


@_registry.cl_function('DOUBLE-FLOAT-EPSILON')
def double_float_epsilon():
    """Double float epsilon."""
    return sys.float_info.epsilon


@_registry.cl_function('LONG-FLOAT-EPSILON')
def long_float_epsilon():
    """Long float epsilon."""
    return sys.float_info.epsilon


@_registry.cl_function('SHORT-FLOAT-NEGATIVE-EPSILON')
def short_float_negative_epsilon():
    """Short float negative epsilon."""
    return sys.float_info.epsilon / 2


@_registry.cl_function('SINGLE-FLOAT-NEGATIVE-EPSILON')
def single_float_negative_epsilon():
    """Single float negative epsilon."""
    return sys.float_info.epsilon / 2


@_registry.cl_function('DOUBLE-FLOAT-NEGATIVE-EPSILON')
def double_float_negative_epsilon():
    """Double float negative epsilon."""
    return sys.float_info.epsilon / 2


@_registry.cl_function('LONG-FLOAT-NEGATIVE-EPSILON')
def long_float_negative_epsilon():
    """Long float negative epsilon."""
    return sys.float_info.epsilon / 2


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
    'short_float_epsilon', 'single_float_epsilon',
    'double_float_epsilon', 'long_float_epsilon',
    'short_float_negative_epsilon', 'single_float_negative_epsilon',
    'double_float_negative_epsilon', 'long_float_negative_epsilon',
]
