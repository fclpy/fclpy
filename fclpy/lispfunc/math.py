"""Mathematical functions - arithmetic, trigonometry, and numeric operations.

This module re-exports mathematical functions from specialized submodules:
- math_arithmetic: Basic arithmetic, bitwise operations, comparisons, and predicates
- math_advanced: Transcendental, trigonometric, and floating-point operations
"""

from .math_arithmetic import *
from .math_advanced import *

# Comprehensive exports for backward compatibility
__all__ = [
    # From math_arithmetic
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
    # From math_advanced
    'exp', 'log', 'sqrt', 'expt', 'isqrt',
    'sin', 'cos', 'tan', 'asin', 'acos', 'atan',
    'sinh', 'cosh', 'tanh', 'asinh', 'acosh', 'atanh',
    'decode_float', 'integer_decode_float', 'scale_float', 'float_fn',
    'float_digits', 'float_precision', 'float_radix', 'float_sign',
    'pi_fn',
    # The most/least-positive/negative-*-float family (normalized included)
    # is deliberately absent: they are constant *variables* whose one home is
    # `lispenv.STANDARD_CONSTANTS`, not functions -- see the note by `pi_fn`
    # in math_advanced.py.
]
