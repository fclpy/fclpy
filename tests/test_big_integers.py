"""Tests for big integer support (Phase 8 Task 2)."""

import pytest
from fclpy.lisptype import T, NIL
from fclpy.lispfunc.comparison import typep
from fclpy.lispfunc.math_arithmetic import most_positive_fixnum, most_negative_fixnum


# Fixnum boundary constants (matching the implementation)
FIXNUM_MAX = 2**29 - 1
FIXNUM_MIN = -2**29


class TestFixnumBignum:
    """Test FIXNUM and BIGNUM type classification."""
    
    def test_small_integer_is_fixnum(self):
        """Small integers are classified as FIXNUM."""
        assert typep(0, 'FIXNUM') == T
        assert typep(1, 'FIXNUM') == T
        assert typep(-1, 'FIXNUM') == T
        assert typep(42, 'FIXNUM') == T
        assert typep(1000000, 'FIXNUM') == T
        
    def test_boundary_integers_are_fixnum(self):
        """Integers at fixnum boundaries are FIXNUM."""
        assert typep(FIXNUM_MAX, 'FIXNUM') == T
        assert typep(FIXNUM_MIN, 'FIXNUM') == T
        
    def test_large_positive_integer_is_bignum(self):
        """Large positive integers are BIGNUM."""
        assert typep(FIXNUM_MAX + 1, 'BIGNUM') == T
        assert typep(2**30, 'BIGNUM') == T
        assert typep(2**64, 'BIGNUM') == T
        
    def test_large_negative_integer_is_bignum(self):
        """Large negative integers are BIGNUM."""
        assert typep(FIXNUM_MIN - 1, 'BIGNUM') == T
        assert typep(-2**30, 'BIGNUM') == T
        assert typep(-2**64, 'BIGNUM') == T
        
    def test_fixnum_is_not_bignum(self):
        """Fixnums should not be classified as BIGNUM."""
        assert typep(0, 'BIGNUM') == NIL
        assert typep(42, 'BIGNUM') == NIL
        assert typep(FIXNUM_MAX, 'BIGNUM') == NIL
        
    def test_bignum_is_not_fixnum(self):
        """Bignums should not be classified as FIXNUM."""
        assert typep(FIXNUM_MAX + 1, 'FIXNUM') == NIL
        assert typep(2**64, 'FIXNUM') == NIL
        
    def test_both_are_integers(self):
        """Both fixnums and bignums are INTEGER."""
        assert typep(42, 'INTEGER') == T
        assert typep(2**64, 'INTEGER') == T
        assert typep(FIXNUM_MAX + 1, 'INTEGER') == T
        
    def test_non_integer_is_neither(self):
        """Non-integers are neither FIXNUM nor BIGNUM."""
        assert typep(3.14, 'FIXNUM') == NIL
        assert typep(3.14, 'BIGNUM') == NIL
        assert typep("hello", 'FIXNUM') == NIL
        assert typep("hello", 'BIGNUM') == NIL


class TestBigIntegerArithmetic:
    """Test arithmetic with big integers."""
    
    def test_big_integer_addition(self):
        """Addition of big integers works correctly."""
        big1 = 2**100
        big2 = 2**100
        result = big1 + big2
        assert result == 2**101
        
    def test_big_integer_multiplication(self):
        """Multiplication of big integers works correctly."""
        big = 2**100
        result = big * big
        assert result == 2**200
        
    def test_big_integer_division(self):
        """Division of big integers works correctly."""
        big = 2**100
        result = big // 2
        assert result == 2**99
        
    def test_arithmetic_preserves_type(self):
        """Arithmetic results maintain correct type classification."""
        # Small numbers that fit in fixnum
        small = 1000
        assert typep(small * 2, 'FIXNUM') == T
        
        # Large numbers that overflow to bignum
        big = FIXNUM_MAX
        result = big * 2
        assert typep(result, 'BIGNUM') == T


class TestMostPositiveNegativeFixnum:
    """Test MOST-POSITIVE-FIXNUM and MOST-NEGATIVE-FIXNUM constants."""
    
    def test_most_positive_fixnum_exists(self):
        """MOST-POSITIVE-FIXNUM returns a positive integer."""
        result = most_positive_fixnum()
        assert isinstance(result, int)
        assert result > 0
        
    def test_most_negative_fixnum_exists(self):
        """MOST-NEGATIVE-FIXNUM returns a negative integer."""
        result = most_negative_fixnum()
        assert isinstance(result, int)
        assert result < 0
        
    def test_symmetry(self):
        """Fixnum range is roughly symmetric."""
        pos = most_positive_fixnum()
        neg = most_negative_fixnum()
        # Should be approximately symmetric (neg is one more negative)
        assert abs(neg) >= pos


class TestRatioType:
    """Test RATIO type classification."""
    
    def test_fraction_is_ratio(self):
        """Fractions are classified as RATIO."""
        from fractions import Fraction
        assert typep(Fraction(1, 2), 'RATIO') == T
        assert typep(Fraction(3, 4), 'RATIO') == T
        
    def test_integer_is_not_ratio(self):
        """Integers are not RATIO."""
        assert typep(42, 'RATIO') == NIL
        
    def test_float_is_not_ratio(self):
        """Floats are not RATIO."""
        assert typep(3.14, 'RATIO') == NIL


class TestRationalType:
    """Test RATIONAL type classification."""
    
    def test_integer_is_rational(self):
        """Integers are RATIONAL."""
        assert typep(42, 'RATIONAL') == T
        assert typep(2**64, 'RATIONAL') == T
        
    def test_fraction_is_rational(self):
        """Fractions are RATIONAL."""
        from fractions import Fraction
        assert typep(Fraction(1, 2), 'RATIONAL') == T
        
    def test_float_is_not_rational(self):
        """Floats are not RATIONAL."""
        assert typep(3.14, 'RATIONAL') == NIL


class TestRealType:
    """Test REAL type classification."""
    
    def test_integer_is_real(self):
        """Integers are REAL."""
        assert typep(42, 'REAL') == T
        
    def test_float_is_real(self):
        """Floats are REAL."""
        assert typep(3.14, 'REAL') == T
        
    def test_fraction_is_real(self):
        """Fractions are REAL."""
        from fractions import Fraction
        assert typep(Fraction(1, 2), 'REAL') == T
        
    def test_complex_is_not_real(self):
        """Complex numbers are not REAL."""
        assert typep(1+2j, 'REAL') == NIL


class TestNumberType:
    """Test NUMBER type classification with all numeric types."""
    
    def test_integer_is_number(self):
        """Integers are NUMBER."""
        assert typep(42, 'NUMBER') == T
        
    def test_bignum_is_number(self):
        """Bignums are NUMBER."""
        assert typep(2**100, 'NUMBER') == T
        
    def test_float_is_number(self):
        """Floats are NUMBER."""
        assert typep(3.14, 'NUMBER') == T
        
    def test_complex_is_number(self):
        """Complex numbers are NUMBER."""
        assert typep(1+2j, 'NUMBER') == T
        
    def test_fraction_is_number(self):
        """Fractions are NUMBER."""
        from fractions import Fraction
        assert typep(Fraction(1, 2), 'NUMBER') == T
