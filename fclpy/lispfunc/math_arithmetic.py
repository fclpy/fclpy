"""Arithmetic and basic math operations."""

import math
import functools
import fclpy.lisptype as lisptype
from . import registry as _registry


@_registry.cl_function('ABS')
def abs_fn(x):
    """Absolute value."""
    return abs(x)


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


@_registry.cl_function('MAX')
def max_fn(*args):
    """Maximum of numbers."""
    return max(args)


@_registry.cl_function('MIN')
def min_fn(*args):
    """Minimum of numbers."""
    return min(args)


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
    """Test if object is a real number."""
    return isinstance(obj, (int, float))


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
    """Return phase of complex number."""
    if isinstance(number, complex):
        return math.atan2(number.imag, number.real)
    return 0 if number >= 0 else math.pi


@_registry.cl_function('CIS')
def cis(theta):
    """Return complex number with magnitude 1 and phase theta."""
    return complex(math.cos(theta), math.sin(theta))


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


@_registry.cl_function('ASH')
def ash(i, count):
    """Arithmetic shift left/right."""
    return i << count if count >= 0 else i >> -count


@_registry.cl_function('INTEGER-LENGTH')
def integer_length(integer):
    """Number of bits in integer."""
    if integer < 0:
        integer = ~integer
    return integer.bit_length()


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


# Comparison operators
@_registry.cl_function('=')
def _s_eq_(*args):
    """Numeric equality operator (=)."""
    if len(args) < 2:
        return lisptype.T
    first = args[0]
    return lisptype.lisp_bool(all(x == first for x in args[1:]))


@_registry.cl_function('<')
def _s_lt_(*args):
    """Less than operator (<)."""
    if len(args) < 2:
        return lisptype.T
    for i in range(len(args) - 1):
        if not (args[i] < args[i + 1]):
            return lisptype.NIL
    return lisptype.T


@_registry.cl_function('>')
def _s_gt_(*args):
    """Greater than operator (>)."""
    if len(args) < 2:
        return lisptype.T
    for i in range(len(args) - 1):
        if not (args[i] > args[i + 1]):
            return lisptype.NIL
    return lisptype.T


@_registry.cl_function('<=')
def _s_lt__s_eq_(*args):
    """Less than or equal operator (<=)."""
    if len(args) < 2:
        return lisptype.T
    for i in range(len(args) - 1):
        if not (args[i] <= args[i + 1]):
            return lisptype.NIL
    return lisptype.T


@_registry.cl_function('>=')
def _s_gt__s_eq_(*args):
    """Greater than or equal operator (>=)."""
    if len(args) < 2:
        return lisptype.T
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
            return Fraction(1, x)
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
    
    # If result is a Fraction with denominator 1, return as integer
    if isinstance(result, Fraction) and result.denominator == 1:
        return result.numerator
    
    return result


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


__all__ = [
    'abs_fn', 'gcd', 'lcm', 'max_fn', 'min_fn', 'signum',
    'evenp', 'oddp', 'zerop', 'plusp', 'minusp',
    'mod', 'rem', 'round_fn', 'truncate', 'ceiling', 'floor',
    'fceiling', 'ffloor', 'fround', 'ftruncate',
    'numerator', 'denominator', 'rational', 'rationalize',
    'numberp', 'integerp', 'floatp', 'complexp', 'realp', 'rationalp',
    'imagpart', 'realpart', 'conjugate', 'phase', 'cis',
    'logand', 'logior', 'logxor', 'lognot', 'logeqv', 'ash',
    'integer_length', 'logbitp', 'logcount', 'logtest',
    'byte_fn', 'byte_size', 'byte_position', 'ldb', 'ldb_test', 'dpb',
    'deposit_field', 'mask_field',
    'bit_fn', 'sbit', 'bit_and', 'bit_ior', 'bit_xor', 'bit_eqv',
    'bit_nand', 'bit_nor', 'bit_andc1', 'bit_andc2', 'bit_orc1', 'bit_orc2',
    'bit_not', 'bit_vector_p', 'simple_bit_vector_p',
    '_s_eq_', '_s_lt_', '_s_gt_', '_s_lt__s_eq_', '_s_gt__s_eq_', '_s_slash__s_eq_',
    '_s_plus_', '_s_minus_', '_s_star_', '_s_slash_',
    '_s_one_s_plus_', '_s_one_s_minus_',
    'most_positive_fixnum', 'most_negative_fixnum', 'boole',
]
