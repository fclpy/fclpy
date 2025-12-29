"""Tests for rational number arithmetic (Phase 8 Task 1)."""

import pytest
from fractions import Fraction
from fclpy.lispfunc.math_arithmetic import (
    _s_slash_, gcd, lcm, numerator, denominator, rational, rationalize,
    numberp, rationalp, integerp
)


class TestGCD:
    """Test GCD (Greatest Common Divisor) function."""
    
    def test_gcd_two_numbers(self):
        """GCD of two numbers."""
        assert gcd(12, 8) == 4
        assert gcd(48, 18) == 6
        
    def test_gcd_multiple_numbers(self):
        """GCD of multiple numbers."""
        assert gcd(12, 18, 24) == 6
        assert gcd(100, 25, 50) == 25
        
    def test_gcd_no_args(self):
        """GCD with no arguments returns 0."""
        assert gcd() == 0
        
    def test_gcd_single_arg(self):
        """GCD of single number is itself."""
        assert gcd(42) == 42


class TestLCM:
    """Test LCM (Least Common Multiple) function."""
    
    def test_lcm_two_numbers(self):
        """LCM of two numbers."""
        assert lcm(4, 6) == 12
        assert lcm(3, 5) == 15
        
    def test_lcm_no_args(self):
        """LCM with no arguments returns 1."""
        assert lcm() == 1


class TestDivision:
    """Test division operator with rational results."""
    
    def test_exact_division_returns_integer(self):
        """Division that is exact returns an integer."""
        result = _s_slash_(6, 2)
        assert result == 3
        assert isinstance(result, int)
        
    def test_inexact_division_returns_fraction(self):
        """Division that is not exact returns a Fraction."""
        result = _s_slash_(1, 2)
        assert result == Fraction(1, 2)
        assert isinstance(result, Fraction)
        
    def test_fraction_reduction(self):
        """Fractions are automatically reduced."""
        result = _s_slash_(6, 8)
        assert result == Fraction(3, 4)
        assert result.numerator == 3
        assert result.denominator == 4
        
    def test_sign_normalization(self):
        """Signs are normalized in fractions."""
        result = _s_slash_(-6, 8)
        assert result == Fraction(-3, 4)
        assert result.numerator == -3
        assert result.denominator == 4
        
        result2 = _s_slash_(6, -8)
        assert result2 == Fraction(-3, 4)
        
    def test_reciprocal(self):
        """Reciprocal of integer returns Fraction."""
        result = _s_slash_(3)
        assert result == Fraction(1, 3)
        
    def test_multiple_division(self):
        """Division with multiple arguments."""
        result = _s_slash_(24, 2, 3)  # 24 / 2 / 3 = 4
        assert result == 4
        
    def test_multiple_division_fraction_result(self):
        """Multiple divisions resulting in fraction."""
        result = _s_slash_(1, 2, 3)  # 1 / 2 / 3 = 1/6
        assert result == Fraction(1, 6)
        
    def test_division_with_float_returns_float(self):
        """Division involving floats returns float."""
        result = _s_slash_(1.0, 2)
        assert result == 0.5
        assert isinstance(result, float)


class TestNumeratorDenominator:
    """Test numerator and denominator functions."""
    
    def test_fraction_numerator(self):
        """Numerator of Fraction."""
        f = Fraction(3, 4)
        assert numerator(f) == 3
        
    def test_fraction_denominator(self):
        """Denominator of Fraction."""
        f = Fraction(3, 4)
        assert denominator(f) == 4
        
    def test_integer_numerator(self):
        """Numerator of integer is itself."""
        assert numerator(5) == 5
        
    def test_integer_denominator(self):
        """Denominator of integer is 1."""
        assert denominator(5) == 1


class TestRationalConstructor:
    """Test rational number constructor."""
    
    def test_create_fraction(self):
        """Create fraction from numerator and denominator."""
        r = rational(3, 4)
        assert r == Fraction(3, 4)
        
    def test_fraction_auto_reduced(self):
        """Fractions are automatically reduced."""
        r = rational(6, 8)
        assert r == Fraction(3, 4)
        
    def test_rationalize(self):
        """Rationalize a float."""
        r = rationalize(0.5)
        assert r == Fraction(1, 2)


class TestPredicates:
    """Test numeric type predicates."""
    
    def test_numberp_integer(self):
        """NUMBERP returns T for integers."""
        from fclpy.lisptype import T
        assert numberp(42) == T
        
    def test_numberp_float(self):
        """NUMBERP returns T for floats."""
        from fclpy.lisptype import T
        assert numberp(3.14) == T
        
    def test_numberp_fraction(self):
        """NUMBERP returns T for Fractions."""
        from fclpy.lisptype import T
        assert numberp(Fraction(1, 2)) == T
        
    def test_rationalp_integer(self):
        """RATIONALP returns True for integers."""
        assert rationalp(42) == True
        
    def test_rationalp_fraction(self):
        """RATIONALP returns True for Fractions."""
        assert rationalp(Fraction(1, 2)) == True
        
    def test_rationalp_float(self):
        """RATIONALP returns False for floats."""
        assert rationalp(3.14) == False
        
    def test_integerp_integer(self):
        """INTEGERP returns T for integers."""
        from fclpy.lisptype import T
        assert integerp(42) == T
        
    def test_integerp_fraction(self):
        """INTEGERP returns NIL for Fractions."""
        from fclpy.lisptype import NIL
        assert integerp(Fraction(1, 2)) == NIL


class TestArithmeticWithFractions:
    """Test arithmetic operations preserve Fraction types."""
    
    def test_fraction_addition(self):
        """Adding fractions produces a fraction."""
        f1 = Fraction(1, 4)
        f2 = Fraction(1, 4)
        result = f1 + f2
        assert result == Fraction(1, 2)
        
    def test_division_result_arithmetic(self):
        """Arithmetic on division results."""
        half = _s_slash_(1, 2)
        quarter = _s_slash_(1, 4)
        result = half + quarter
        assert result == Fraction(3, 4)
        
    def test_mixed_arithmetic(self):
        """Mixed integer and fraction arithmetic."""
        half = _s_slash_(1, 2)
        result = half + 1
        assert result == Fraction(3, 2)
