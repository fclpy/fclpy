"""Mathematical functions - arithmetic, trigonometry, and numeric operations."""

import math
import functools
import fclpy.lisptype as lisptype
from . import registry as _registry


@_registry.cl_function('ACOS')
def acos(x):
    """Arc cosine function."""
    return math.acos(x)


@_registry.cl_function('ACOSH')
def acosh(x):
    """Hyperbolic arc cosine function."""
    return math.acosh(x)


@_registry.cl_function('ASIN')
def asin(x):
    """Arc sine function."""
    return math.asin(x)


@_registry.cl_function('ASINH')
def asinh(x):
    """Hyperbolic arc sine function."""
    return math.asinh(x)


@_registry.cl_function('ATAN')
def atan(x):
    """Arc tangent function."""
    return math.atan(x)


@_registry.cl_function('ATANH')
def atanh(x):
    """Hyperbolic arc tangent function."""
    return math.atanh(x)


@_registry.cl_function('COS')
def cos(a):
    """Cosine function."""
    return math.cos(a)


@_registry.cl_function('SIN')
def sin(a):
    """Sine function."""
    return math.sin(a)


@_registry.cl_function('TAN')
def tan(a):
    """Tangent function."""
    return math.tan(a)


@_registry.cl_function('EXP')
def exp(x):
    """Exponential function."""
    return math.exp(x)


@_registry.cl_function('EXPT')
def expt(base, power):
    """Raise base to power."""
    return base ** power


@_registry.cl_function('CEILING')
def ceiling(x, divisor=1):
    """Return ceiling as integer."""
    return math.ceil(x / divisor)


@_registry.cl_function('FLOOR')
def floor(x, divisor=1):
    """Return floor as integer."""
    return math.floor(x / divisor)


@_registry.cl_function('FCEILING')
def fceiling(x, divisor=1):
    """Return ceiling as float."""
    return float(math.ceil(x / divisor))


@_registry.cl_function('FFLOOR')
def ffloor(x, divisor=1):
    """Return floor as float."""
    return float(math.floor(x / divisor))


@_registry.cl_function('FROUND')
def fround(x, divisor=1):
    """Round to nearest float."""
    return float(round(x / divisor))


@_registry.cl_function('FTRUNCATE')
def ftruncate(x, divisor=1):
    """Truncate to float."""
    return float(int(x / divisor))


@_registry.cl_function('ASH')
def ash(i, count):
    """Arithmetic shift left/right."""
    return i << count if count >= 0 else i >> -count


@_registry.cl_function('GCD')
def gcd(*integers):
    """Greatest common divisor."""
    if not integers:
        return 0
    return functools.reduce(math.gcd, integers)


@_registry.cl_function('LCM')
def lcm(*integers):
    """Least common multiple."""
    if not integers:
        return 1
    def lcm2(a, b):
        return abs(a * b) // math.gcd(a, b)
    return functools.reduce(lcm2, integers)


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


@_registry.cl_function('NUMBERP')
def numberp(obj):
    """Test if object is a number."""
    return lisptype.lisp_bool(isinstance(obj, (int, float, complex)))


@_registry.cl_function('INTEGERP')
def integerp(obj):
    """Test if object is an integer."""
    return lisptype.lisp_bool(isinstance(obj, int))


@_registry.cl_function('FLOATP')
def floatp(obj):
    """Test if object is a float."""
    return lisptype.lisp_bool(isinstance(obj, float))
    return isinstance(obj, float)


@_registry.cl_function('COMPLEXP')
def complexp(obj):
    """Test if object is a complex number."""
    return isinstance(obj, complex)


@_registry.cl_function('REALP')
def realp(obj):
    """Test if object is a real number."""
    return isinstance(obj, (int, float))


@_registry.cl_function('RATIONALP')
def rationalp(obj):
    """Test if object is a rational number."""
    return isinstance(obj, (int, float))  # In Python, floats are used for rationals


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
    """Return phase of complex number."""
    if isinstance(number, complex):
        return math.atan2(number.imag, number.real)
    return 0 if number >= 0 else math.pi


@_registry.cl_function('CIS')
def cis(theta):
    """Return complex number with magnitude 1 and phase theta."""
    return complex(math.cos(theta), math.sin(theta))


@_registry.cl_function('ABS')
def abs_fn(x):
    """Absolute value."""
    return abs(x)


@_registry.cl_function('SIGNUM')
def signum(x):
    """Sign of number."""
    if x > 0:
        return 1
    elif x < 0:
        return -1
    else:
        return 0


@_registry.cl_function('MAX')
def max_fn(*args):
    """Maximum of numbers."""
    return max(args)


@_registry.cl_function('MIN')
def min_fn(*args):
    """Minimum of numbers."""
    return min(args)


def one_plus(x):
    """Add one to number."""
    return x + 1


def one_minus(x):
    """Subtract one from number."""
    return x - 1


def plus(*args):
    """Addition."""
    return sum(args)


def minus(x, *args):
    """Subtraction."""
    if not args:
        return -x
    return x - sum(args)


def times(*args):
    """Multiplication."""
    result = 1
    for x in args:
        result *= x
    return result


def divide(x, *args):
    """Division."""
    if not args:
        return 1 / x
    result = x
    for y in args:
        result /= y
    return result


@_registry.cl_function('MOD')
def mod(number, divisor):
    """Modulo operation."""
    return number % divisor


@_registry.cl_function('REM')
def rem(number, divisor):
    """Remainder operation."""
    return number % divisor


@_registry.cl_function('ROUND')
def round_fn(x, divisor=1):
    """Round to nearest integer."""
    return round(x / divisor)


@_registry.cl_function('TRUNCATE')
def truncate(x, divisor=1):
    """Truncate to integer."""
    return int(x / divisor)


@_registry.cl_function('MOST-POSITIVE-FIXNUM')
def most_positive_fixnum():
    """Most positive fixnum."""
    return 2**63 - 1


@_registry.cl_function('MOST-NEGATIVE-FIXNUM')
def most_negative_fixnum():
    """Most negative fixnum."""
    return -2**63


# Float operations
@_registry.cl_function('DECODE-FLOAT')
def decode_float(float_num):
    """Decode float into significand, exponent, sign."""
    import struct
    if float_num == 0.0:
        return 0.0, 0, 1.0
    
    sign = 1.0 if float_num >= 0 else -1.0
    abs_float = abs(float_num)
    
    # Use frexp to get mantissa and exponent
    mantissa, exponent = math.frexp(abs_float)
    
    return mantissa, exponent, sign


@_registry.cl_function('INTEGER-DECODE-FLOAT')
def integer_decode_float(float_num):
    """Integer decode of float."""
    if float_num == 0.0:
        return 0, 0, 1
    
    sign = 1 if float_num >= 0 else -1
    abs_float = abs(float_num)
    
    # Convert to integer representation
    mantissa, exponent = math.frexp(abs_float)
    # Scale mantissa to integer (assuming 53-bit precision for double)
    int_mantissa = int(mantissa * (2 ** 53))
    int_exponent = exponent - 53
    
    return int_mantissa, int_exponent, sign


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
    import sys
    if isinstance(float_num, float):
        return sys.float_info.mant_dig  # 53 for IEEE 754 double
    return 24  # Default for single precision


@_registry.cl_function('FLOAT-PRECISION')
def float_precision(float_num):
    """Precision of float."""
    import sys
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


# Bitwise operations
@_registry.cl_function('LOGAND')
def logand(*args):
    """Bitwise AND."""
    if not args:
        return -1
    return functools.reduce(lambda x, y: x & y, args)


@_registry.cl_function('LOGIOR')
def logior(*args):
    """Bitwise OR."""
    if not args:
        return 0
    return functools.reduce(lambda x, y: x | y, args)


@_registry.cl_function('LOGXOR')
def logxor(*args):
    """Bitwise XOR."""
    if not args:
        return 0
    return functools.reduce(lambda x, y: x ^ y, args)


@_registry.cl_function('LOGNOT')
def lognot(integer):
    """Bitwise NOT."""
    return ~integer


@_registry.cl_function('LOGEQV')
def logeqv(*args):
    """Bitwise equivalence."""
    if not args:
        return -1
    return functools.reduce(lambda x, y: ~(x ^ y), args)


@_registry.cl_function('INTEGER-LENGTH')
def integer_length(integer):
    """Number of bits in integer."""
    if integer < 0:
        integer = ~integer
    return integer.bit_length()


@_registry.cl_function('ISQRT')
def isqrt(x):
    """Integer square root."""
    if x < 0:
        raise ValueError("isqrt requires non-negative input")
    return int(math.sqrt(x))


@_registry.cl_function('LOGBITP')
def logbitp(index, integer):
    """Test if bit is set."""
    return bool(integer & (1 << index))


@_registry.cl_function('LOGCOUNT')
def logcount(integer):
    """Number of 1 bits."""
    if integer < 0:
        return bin(integer).count('0') - 1  # Subtract 1 for the '0b' prefix
    return bin(integer).count('1')


@_registry.cl_function('LOGTEST')
def logtest(integer1, integer2):
    """Test if any bits are set in both integers."""
    return (integer1 & integer2) != 0


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


# Special operator functions (with symbol-safe names)
def _s_plus_(*args):
    """Addition operator (+)."""
    if not args:
        return 0
    return sum(args)


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


def _s_star_(*args):
    """Multiplication operator (*)."""
    if not args:
        return 1
    result = args[0]
    for x in args[1:]:
        result *= x
    return result


def _s_slash_(*args):
    """Division operator (/)."""
    if not args:
        raise ValueError("/ requires at least one argument")
    if len(args) == 1:
        return 1 / args[0]
    result = args[0]
    for x in args[1:]:
        result /= x
    return result


def _s_eq_(*args):
    """Numeric equality operator (=)."""
    if len(args) < 2:
        return lisptype.T
    first = args[0]
    return lisptype.lisp_bool(all(x == first for x in args[1:]))


def _s_lt_(*args):
    """Less than operator (<)."""
    if len(args) < 2:
        return lisptype.T
    for i in range(len(args) - 1):
        if not (args[i] < args[i + 1]):
            return lisptype.NIL
    return lisptype.T


def _s_gt_(*args):
    """Greater than operator (>)."""
    if len(args) < 2:
        return lisptype.T
    for i in range(len(args) - 1):
        if not (args[i] > args[i + 1]):
            return lisptype.NIL
    return lisptype.T


def _s_lt__s_eq_(*args):
    """Less than or equal operator (<=)."""
    if len(args) < 2:
        return lisptype.T
    for i in range(len(args) - 1):
        if not (args[i] <= args[i + 1]):
            return lisptype.NIL
    return lisptype.T


def _s_gt__s_eq_(*args):
    """Greater than or equal operator (>=)."""
    if len(args) < 2:
        return lisptype.T
    for i in range(len(args) - 1):
        if not (args[i] >= args[i + 1]):
            return lisptype.NIL
    return lisptype.T


def _s_slash__s_eq_(*args):
    """Not equal operator (/=)."""
    if len(args) < 2:
        return lisptype.T
    for i in range(len(args)):
        for j in range(i + 1, len(args)):
            if args[i] == args[j]:
                return lisptype.NIL
    return lisptype.T


def _s_one_s_plus_(x):
    """Increment by one operator (1+)."""
    return x + 1


def _s_one_s_minus_(x):
    """Decrement by one operator (1-)."""
    return x - 1


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

@_registry.cl_function('RATIONAL')
def rational(n, d=1):
    """Create rational number from numerator and denominator"""
    from fractions import Fraction
    return Fraction(n, d)

@_registry.cl_function('RATIONALIZE')
def rationalize(x):
    """Convert number to rational"""
    from fractions import Fraction
    return Fraction(x).limit_denominator()


@_registry.cl_function('SQRT')
def sqrt(x):
    """Square root function."""
    return math.sqrt(x)


@_registry.cl_function('LOG')
def log(x, base=None):
    """Logarithm function."""
    if base is None:
        return math.log(x)
    return math.log(x, base)


@_registry.cl_function('SINH')
def sinh(x):
    """Hyperbolic sine function."""
    return math.sinh(x)


@_registry.cl_function('COSH')
def cosh(x):
    """Hyperbolic cosine function."""
    return math.cosh(x)


@_registry.cl_function('TANH')
def tanh(x):
    """Hyperbolic tangent function."""
    return math.tanh(x)


# Bit operations
@_registry.cl_function('BIT')
def bit_fn(bit_array, *indices):
    """Access bit in bit array."""
    # For now, assume bit_array is a list of 0s and 1s
    if len(indices) == 1:
        return bit_array[indices[0]]
    # Multi-dimensional - not implemented yet
    return 0


@_registry.cl_function('SBIT')
def sbit(bit_array, *indices):
    """Setf-able bit access."""
    return bit_fn(bit_array, *indices)


@_registry.cl_function('BIT-AND')
def bit_and(bit_array1, bit_array2, result_array=None):
    """Bitwise AND of bit arrays."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(bit_array1[i] & bit_array2[i])
    return result


@_registry.cl_function('BIT-IOR')
def bit_ior(bit_array1, bit_array2, result_array=None):
    """Bitwise inclusive OR of bit arrays."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(bit_array1[i] | bit_array2[i])
    return result


@_registry.cl_function('BIT-XOR')
def bit_xor(bit_array1, bit_array2, result_array=None):
    """Bitwise exclusive OR of bit arrays."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(bit_array1[i] ^ bit_array2[i])
    return result


@_registry.cl_function('BIT-EQV')
def bit_eqv(bit_array1, bit_array2, result_array=None):
    """Bitwise equivalence of bit arrays."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(1 if bit_array1[i] == bit_array2[i] else 0)
    return result


@_registry.cl_function('BIT-NAND')
def bit_nand(bit_array1, bit_array2, result_array=None):
    """Bitwise NAND of bit arrays."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(1 - (bit_array1[i] & bit_array2[i]))
    return result


@_registry.cl_function('BIT-NOR')
def bit_nor(bit_array1, bit_array2, result_array=None):
    """Bitwise NOR of bit arrays."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(1 - (bit_array1[i] | bit_array2[i]))
    return result


@_registry.cl_function('BIT-ANDC1')
def bit_andc1(bit_array1, bit_array2, result_array=None):
    """Bitwise AND complement of first array."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append((1 - bit_array1[i]) & bit_array2[i])
    return result


@_registry.cl_function('BIT-ANDC2')
def bit_andc2(bit_array1, bit_array2, result_array=None):
    """Bitwise AND complement of second array."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(bit_array1[i] & (1 - bit_array2[i]))
    return result


@_registry.cl_function('BIT-ORC1')
def bit_orc1(bit_array1, bit_array2, result_array=None):
    """Bitwise OR complement of first array."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append((1 - bit_array1[i]) | bit_array2[i])
    return result


@_registry.cl_function('BIT-ORC2')
def bit_orc2(bit_array1, bit_array2, result_array=None):
    """Bitwise OR complement of second array."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(bit_array1[i] | (1 - bit_array2[i]))
    return result


@_registry.cl_function('BIT-NOT')
def bit_not(bit_array, result_array=None):
    """Bitwise NOT of bit array."""
    result = []
    for bit in bit_array:
        result.append(1 - bit)
    return result


@_registry.cl_function('BIT-VECTOR-P')
def bit_vector_p(obj):
    """Test if object is a bit vector."""
    return isinstance(obj, list) and all(b in (0, 1) for b in obj)


@_registry.cl_function('SIMPLE-BIT-VECTOR-P')
def simple_bit_vector_p(obj):
    """Test if object is a simple bit vector."""
    return bit_vector_p(obj)  # For now, same as bit_vector_p


# Mathematical constants
@_registry.cl_function('PI')
def pi_fn():
    """Return pi."""
    import math
    return math.pi


# Additional math functions for completeness
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


# Floating-point constants
@_registry.cl_function('LEAST-POSITIVE-DOUBLE-FLOAT')
def least_positive_double_float():
    """Least positive double float."""
    import sys
    return sys.float_info.min


@_registry.cl_function('LEAST-NEGATIVE-DOUBLE-FLOAT')
def least_negative_double_float():
    """Least negative double float."""
    import sys
    return -sys.float_info.min


@_registry.cl_function('MOST-POSITIVE-DOUBLE-FLOAT')
def most_positive_double_float():
    """Most positive double float."""
    import sys
    return sys.float_info.max


@_registry.cl_function('MOST-NEGATIVE-DOUBLE-FLOAT')
def most_negative_double_float():
    """Most negative double float."""
    import sys
    return -sys.float_info.max


@_registry.cl_function('LEAST-POSITIVE-SHORT-FLOAT')
def least_positive_short_float():
    """Least positive short float."""
    import sys
    return sys.float_info.min


@_registry.cl_function('LEAST-NEGATIVE-SHORT-FLOAT')
def least_negative_short_float():
    """Least negative short float."""
    import sys
    return -sys.float_info.min


@_registry.cl_function('MOST-POSITIVE-SHORT-FLOAT')
def most_positive_short_float():
    """Most positive short float."""
    import sys
    return sys.float_info.max


@_registry.cl_function('MOST-NEGATIVE-SHORT-FLOAT')
def most_negative_short_float():
    """Most negative short float."""
    import sys
    return -sys.float_info.max


@_registry.cl_function('LEAST-POSITIVE-SINGLE-FLOAT')
def least_positive_single_float():
    """Least positive single float."""
    import sys
    return sys.float_info.min


@_registry.cl_function('LEAST-NEGATIVE-SINGLE-FLOAT')
def least_negative_single_float():
    """Least negative single float."""
    import sys
    return -sys.float_info.min


@_registry.cl_function('MOST-POSITIVE-SINGLE-FLOAT')
def most_positive_single_float():
    """Most positive single float."""
    import sys
    return sys.float_info.max


@_registry.cl_function('MOST-NEGATIVE-SINGLE-FLOAT')
def most_negative_single_float():
    """Most negative single float."""
    import sys
    return -sys.float_info.max


@_registry.cl_function('LEAST-POSITIVE-LONG-FLOAT')
def least_positive_long_float():
    """Least positive long float."""
    import sys
    return sys.float_info.min


@_registry.cl_function('LEAST-NEGATIVE-LONG-FLOAT')
def least_negative_long_float():
    """Least negative long float."""
    import sys
    return -sys.float_info.min


@_registry.cl_function('MOST-POSITIVE-LONG-FLOAT')
def most_positive_long_float():
    """Most positive long float."""
    import sys
    return sys.float_info.max


@_registry.cl_function('MOST-NEGATIVE-LONG-FLOAT')
def most_negative_long_float():
    """Most negative long float."""
    import sys
    return -sys.float_info.max


@_registry.cl_function('SHORT-FLOAT-EPSILON')
def short_float_epsilon():
    """Short float epsilon."""
    import sys
    return sys.float_info.epsilon


@_registry.cl_function('SINGLE-FLOAT-EPSILON')
def single_float_epsilon():
    """Single float epsilon."""
    import sys
    return sys.float_info.epsilon


@_registry.cl_function('DOUBLE-FLOAT-EPSILON')
def double_float_epsilon():
    """Double float epsilon."""
    import sys
    return sys.float_info.epsilon


@_registry.cl_function('LONG-FLOAT-EPSILON')
def long_float_epsilon():
    """Long float epsilon."""
    import sys
    return sys.float_info.epsilon


@_registry.cl_function('SHORT-FLOAT-NEGATIVE-EPSILON')
def short_float_negative_epsilon():
    """Short float negative epsilon."""
    import sys
    return sys.float_info.epsilon / 2


@_registry.cl_function('SINGLE-FLOAT-NEGATIVE-EPSILON')
def single_float_negative_epsilon():
    """Single float negative epsilon."""
    import sys
    return sys.float_info.epsilon / 2


@_registry.cl_function('DOUBLE-FLOAT-NEGATIVE-EPSILON')
def double_float_negative_epsilon():
    """Double float negative epsilon."""
    import sys
    return sys.float_info.epsilon / 2


@_registry.cl_function('LONG-FLOAT-NEGATIVE-EPSILON')
def long_float_negative_epsilon():
    """Long float negative epsilon."""
    import sys
    return sys.float_info.epsilon / 2
