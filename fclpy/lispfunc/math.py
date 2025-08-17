"""Mathematical functions - arithmetic, trigonometry, and numeric operations."""

import math
import functools
import fclpy.lisptype as lisptype


def acos(x):
    """Arc cosine function."""
    return math.acos(x)


def acosh(x):
    """Hyperbolic arc cosine function."""
    return math.acosh(x)


def asin(x):
    """Arc sine function."""
    return math.asin(x)


def asinh(x):
    """Hyperbolic arc sine function."""
    return math.asinh(x)


def atan(x):
    """Arc tangent function."""
    return math.atan(x)


def atanh(x):
    """Hyperbolic arc tangent function."""
    return math.atanh(x)


def cos(a):
    """Cosine function."""
    return math.cos(a)


def sin(a):
    """Sine function."""
    return math.sin(a)


def tan(a):
    """Tangent function."""
    return math.tan(a)


def exp(x):
    """Exponential function."""
    return math.exp(x)


def expt(base, power):
    """Raise base to power."""
    return base ** power


def ceiling(x, divisor=1):
    """Return ceiling as integer."""
    return math.ceil(x / divisor)


def floor(x, divisor=1):
    """Return floor as integer."""
    return math.floor(x / divisor)


def fceiling(x, divisor=1):
    """Return ceiling as float."""
    raise lisptype.LispNotImplementedError("FCEILING")


def ffloor(x, divisor=1):
    """Return floor as float."""
    raise lisptype.LispNotImplementedError("FFLOOR")


def fround(x, divisor=1):
    """Round to nearest float."""
    raise lisptype.LispNotImplementedError("FROUND")


def ftruncate(x, divisor=1):
    """Truncate to float."""
    raise lisptype.LispNotImplementedError("FTRUNCATE")


def ash(i, count):
    """Arithmetic shift left/right."""
    return i << count if count >= 0 else i >> -count


def gcd(*integers):
    """Greatest common divisor."""
    if not integers:
        return 0
    return functools.reduce(math.gcd, integers)


def lcm(*integers):
    """Least common multiple."""
    if not integers:
        return 1
    def lcm2(a, b):
        return abs(a * b) // math.gcd(a, b)
    return functools.reduce(lcm2, integers)


def evenp(x):
    """Test if number is even."""
    return x % 2 == 0


def oddp(x):
    """Test if number is odd."""
    return x % 2 == 1


def zerop(x):
    """Test if number is zero."""
    return x == 0


def plusp(x):
    """Test if number is positive."""
    return x > 0


def minusp(x):
    """Test if number is negative."""
    return x < 0


def numberp(obj):
    """Test if object is a number."""
    return isinstance(obj, (int, float, complex))


def integerp(obj):
    """Test if object is an integer."""
    return isinstance(obj, int)


def floatp(obj):
    """Test if object is a float."""
    return isinstance(obj, float)


def complexp(obj):
    """Test if object is a complex number."""
    return isinstance(obj, complex)


def realp(obj):
    """Test if object is a real number."""
    return isinstance(obj, (int, float))


def rationalp(obj):
    """Test if object is a rational number."""
    return isinstance(obj, (int, float))  # In Python, floats are used for rationals


def imagpart(number):
    """Return imaginary part of complex number."""
    if isinstance(number, complex):
        return number.imag
    return 0


def realpart(number):
    """Return real part of complex number."""
    if isinstance(number, complex):
        return number.real
    return number


def conjugate(number):
    """Return complex conjugate."""
    if isinstance(number, complex):
        return number.conjugate()
    return number


def phase(number):
    """Return phase of complex number."""
    if isinstance(number, complex):
        return math.atan2(number.imag, number.real)
    return 0 if number >= 0 else math.pi


def cis(theta):
    """Return complex number with magnitude 1 and phase theta."""
    return complex(math.cos(theta), math.sin(theta))


def abs_fn(x):
    """Absolute value."""
    return abs(x)


def signum(x):
    """Sign of number."""
    if x > 0:
        return 1
    elif x < 0:
        return -1
    else:
        return 0


def max_fn(*args):
    """Maximum of numbers."""
    return max(args)


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


def mod(number, divisor):
    """Modulo operation."""
    return number % divisor


def rem(number, divisor):
    """Remainder operation."""
    return number % divisor


def round_fn(x, divisor=1):
    """Round to nearest integer."""
    return round(x / divisor)


def truncate(x, divisor=1):
    """Truncate to integer."""
    return int(x / divisor)


def most_positive_fixnum():
    """Most positive fixnum."""
    return 2**63 - 1


def most_negative_fixnum():
    """Most negative fixnum."""
    return -2**63


# Float operations
def decode_float(float_num):
    """Decode float into significand, exponent, sign."""
    raise lisptype.LispNotImplementedError("DECODE-FLOAT")


def integer_decode_float(float_num):
    """Integer decode of float."""
    raise lisptype.LispNotImplementedError("INTEGER-DECODE-FLOAT")


def scale_float(float_num, integer):
    """Scale float by power of radix."""
    raise lisptype.LispNotImplementedError("SCALE-FLOAT")


def float_fn(number, prototype=None):
    """Convert to float."""
    return float(number)


def float_digits(float_num):
    """Number of digits in float."""
    raise lisptype.LispNotImplementedError("FLOAT-DIGITS")


def float_precision(float_num):
    """Precision of float."""
    raise lisptype.LispNotImplementedError("FLOAT-PRECISION")


def float_radix(float_num):
    """Radix of float."""
    return 2


def float_sign(float1, float2=None):
    """Sign of float."""
    sign = 1 if float1 >= 0 else -1
    if float2 is None:
        return sign
    return sign * abs(float2)


# Bitwise operations
def logand(*args):
    """Bitwise AND."""
    if not args:
        return -1
    return functools.reduce(lambda x, y: x & y, args)


def logior(*args):
    """Bitwise OR."""
    if not args:
        return 0
    return functools.reduce(lambda x, y: x | y, args)


def logxor(*args):
    """Bitwise XOR."""
    if not args:
        return 0
    return functools.reduce(lambda x, y: x ^ y, args)


def lognot(integer):
    """Bitwise NOT."""
    return ~integer


def logeqv(*args):
    """Bitwise equivalence."""
    if not args:
        return -1
    return functools.reduce(lambda x, y: ~(x ^ y), args)


def integer_length(integer):
    """Number of bits in integer."""
    if integer < 0:
        integer = ~integer
    return integer.bit_length()


def isqrt(x):
    """Integer square root."""
    if x < 0:
        raise ValueError("isqrt requires non-negative input")
    return int(math.sqrt(x))


def logbitp(index, integer):
    """Test if bit is set."""
    return bool(integer & (1 << index))


def logcount(integer):
    """Number of 1 bits."""
    if integer < 0:
        return bin(integer).count('0') - 1  # Subtract 1 for the '0b' prefix
    return bin(integer).count('1')


def logtest(integer1, integer2):
    """Test if any bits are set in both integers."""
    return (integer1 & integer2) != 0


def deposit_field(newbyte, bytespec, integer):
    """Deposit field in integer."""
    raise lisptype.LispNotImplementedError("DEPOSIT-FIELD")


def mask_field(bytespec, integer):
    """Mask field in integer."""
    raise lisptype.LispNotImplementedError("MASK-FIELD")


def byte_fn(size, position):
    """Create byte specifier."""
    raise lisptype.LispNotImplementedError("BYTE")


def byte_size(bytespec):
    """Size of byte specifier."""
    raise lisptype.LispNotImplementedError("BYTE-SIZE")


def byte_position(bytespec):
    """Position of byte specifier."""
    raise lisptype.LispNotImplementedError("BYTE-POSITION")


def ldb(bytespec, integer):
    """Load byte."""
    raise lisptype.LispNotImplementedError("LDB")


def ldb_test(bytespec, integer):
    """Test byte."""
    raise lisptype.LispNotImplementedError("LDB-TEST")


def dpb(newbyte, bytespec, integer):
    """Deposit byte."""
    raise lisptype.LispNotImplementedError("DPB")


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
        return True
    first = args[0]
    return all(x == first for x in args[1:])


def _s_lt_(*args):
    """Less than operator (<)."""
    if len(args) < 2:
        return True
    for i in range(len(args) - 1):
        if not (args[i] < args[i + 1]):
            return False
    return True


def _s_gt_(*args):
    """Greater than operator (>)."""
    if len(args) < 2:
        return True
    for i in range(len(args) - 1):
        if not (args[i] > args[i + 1]):
            return False
    return True


def _s_lt__s_eq_(*args):
    """Less than or equal operator (<=)."""
    if len(args) < 2:
        return True
    for i in range(len(args) - 1):
        if not (args[i] <= args[i + 1]):
            return False
    return True


def _s_gt__s_eq_(*args):
    """Greater than or equal operator (>=)."""
    if len(args) < 2:
        return True
    for i in range(len(args) - 1):
        if not (args[i] >= args[i + 1]):
            return False
    return True


def _s_slash__s_eq_(*args):
    """Not equal operator (/=)."""
    if len(args) < 2:
        return True
    for i in range(len(args)):
        for j in range(i + 1, len(args)):
            if args[i] == args[j]:
                return False
    return True


def _s_one_s_plus_(x):
    """Increment by one operator (1+)."""
    return x + 1


def _s_one_s_minus_(x):
    """Decrement by one operator (1-)."""
    return x - 1


def numerator(rational):
    """Return numerator of rational number"""
    from fractions import Fraction
    if isinstance(rational, Fraction):
        return rational.numerator
    return rational  # For integers, numerator is the number itself

def denominator(rational):
    """Return denominator of rational number"""
    from fractions import Fraction
    if isinstance(rational, Fraction):
        return rational.denominator
    return 1  # For integers, denominator is 1

def rational(n, d=1):
    """Create rational number from numerator and denominator"""
    from fractions import Fraction
    return Fraction(n, d)

def rationalize(x):
    """Convert number to rational"""
    from fractions import Fraction
    return Fraction(x).limit_denominator()


def sqrt(x):
    """Square root function."""
    return math.sqrt(x)


def log(x, base=None):
    """Logarithm function."""
    if base is None:
        return math.log(x)
    return math.log(x, base)


def sinh(x):
    """Hyperbolic sine function."""
    return math.sinh(x)


def cosh(x):
    """Hyperbolic cosine function."""
    return math.cosh(x)


def tanh(x):
    """Hyperbolic tangent function."""
    return math.tanh(x)


# Bit operations
def bit_fn(bit_array, *indices):
    """Access bit in bit array."""
    # For now, assume bit_array is a list of 0s and 1s
    if len(indices) == 1:
        return bit_array[indices[0]]
    # Multi-dimensional - not implemented yet
    return 0


def sbit(bit_array, *indices):
    """Setf-able bit access."""
    return bit_fn(bit_array, *indices)


def bit_and(bit_array1, bit_array2, result_array=None):
    """Bitwise AND of bit arrays."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(bit_array1[i] & bit_array2[i])
    return result


def bit_ior(bit_array1, bit_array2, result_array=None):
    """Bitwise inclusive OR of bit arrays."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(bit_array1[i] | bit_array2[i])
    return result


def bit_xor(bit_array1, bit_array2, result_array=None):
    """Bitwise exclusive OR of bit arrays."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(bit_array1[i] ^ bit_array2[i])
    return result


def bit_eqv(bit_array1, bit_array2, result_array=None):
    """Bitwise equivalence of bit arrays."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(1 if bit_array1[i] == bit_array2[i] else 0)
    return result


def bit_nand(bit_array1, bit_array2, result_array=None):
    """Bitwise NAND of bit arrays."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(1 - (bit_array1[i] & bit_array2[i]))
    return result


def bit_nor(bit_array1, bit_array2, result_array=None):
    """Bitwise NOR of bit arrays."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(1 - (bit_array1[i] | bit_array2[i]))
    return result


def bit_andc1(bit_array1, bit_array2, result_array=None):
    """Bitwise AND complement of first array."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append((1 - bit_array1[i]) & bit_array2[i])
    return result


def bit_andc2(bit_array1, bit_array2, result_array=None):
    """Bitwise AND complement of second array."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(bit_array1[i] & (1 - bit_array2[i]))
    return result


def bit_orc1(bit_array1, bit_array2, result_array=None):
    """Bitwise OR complement of first array."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append((1 - bit_array1[i]) | bit_array2[i])
    return result


def bit_orc2(bit_array1, bit_array2, result_array=None):
    """Bitwise OR complement of second array."""
    result = []
    for i in range(min(len(bit_array1), len(bit_array2))):
        result.append(bit_array1[i] | (1 - bit_array2[i]))
    return result


def bit_not(bit_array, result_array=None):
    """Bitwise NOT of bit array."""
    result = []
    for bit in bit_array:
        result.append(1 - bit)
    return result


def bit_vector_p(obj):
    """Test if object is a bit vector."""
    return isinstance(obj, list) and all(b in (0, 1) for b in obj)


def simple_bit_vector_p(obj):
    """Test if object is a simple bit vector."""
    return bit_vector_p(obj)  # For now, same as bit_vector_p


# Mathematical constants
def pi_fn():
    """Return pi."""
    import math
    return math.pi


# Additional math functions for completeness
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
def least_positive_double_float():
    """Least positive double float."""
    import sys
    return sys.float_info.min


def least_negative_double_float():
    """Least negative double float."""
    import sys
    return -sys.float_info.min


def most_positive_double_float():
    """Most positive double float."""
    import sys
    return sys.float_info.max


def most_negative_double_float():
    """Most negative double float."""
    import sys
    return -sys.float_info.max


def least_positive_short_float():
    """Least positive short float."""
    import sys
    return sys.float_info.min


def least_negative_short_float():
    """Least negative short float."""
    import sys
    return -sys.float_info.min


def most_positive_short_float():
    """Most positive short float."""
    import sys
    return sys.float_info.max


def most_negative_short_float():
    """Most negative short float."""
    import sys
    return -sys.float_info.max


def least_positive_single_float():
    """Least positive single float."""
    import sys
    return sys.float_info.min


def least_negative_single_float():
    """Least negative single float."""
    import sys
    return -sys.float_info.min


def most_positive_single_float():
    """Most positive single float."""
    import sys
    return sys.float_info.max


def most_negative_single_float():
    """Most negative single float."""
    import sys
    return -sys.float_info.max


def least_positive_long_float():
    """Least positive long float."""
    import sys
    return sys.float_info.min


def least_negative_long_float():
    """Least negative long float."""
    import sys
    return -sys.float_info.min


def most_positive_long_float():
    """Most positive long float."""
    import sys
    return sys.float_info.max


def most_negative_long_float():
    """Most negative long float."""
    import sys
    return -sys.float_info.max


def short_float_epsilon():
    """Short float epsilon."""
    import sys
    return sys.float_info.epsilon


def single_float_epsilon():
    """Single float epsilon."""
    import sys
    return sys.float_info.epsilon


def double_float_epsilon():
    """Double float epsilon."""
    import sys
    return sys.float_info.epsilon


def long_float_epsilon():
    """Long float epsilon."""
    import sys
    return sys.float_info.epsilon


def short_float_negative_epsilon():
    """Short float negative epsilon."""
    import sys
    return sys.float_info.epsilon / 2


def single_float_negative_epsilon():
    """Single float negative epsilon."""
    import sys
    return sys.float_info.epsilon / 2


def double_float_negative_epsilon():
    """Double float negative epsilon."""
    import sys
    return sys.float_info.epsilon / 2


def long_float_negative_epsilon():
    """Long float negative epsilon."""
    import sys
    return sys.float_info.epsilon / 2
