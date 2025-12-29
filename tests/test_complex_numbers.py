"""Tests for complex number support (Phase 8 Task 3)."""

import pytest
import io
from fclpy.lisptype import T, NIL
from fclpy.readtable import Readtable, get_current_readtable
from fclpy.lispreader import LispReader, LispStream
from fclpy.lispfunc.math_arithmetic import (
    imagpart, realpart, conjugate, phase, cis, complexp
)
from fclpy.lispfunc.misc_macros import complex_fn
from fclpy.lispfunc.comparison import typep


def _parse_code(code):
    """Helper to parse Lisp code from a string."""
    rt = get_current_readtable()
    s = io.StringIO(code)
    reader = LispReader(rt.get_macro_character, LispStream(s))
    return reader.read_1()


class TestComplexReader:
    """Test #C reader syntax for complex numbers."""
    
    def test_read_simple_complex(self):
        """Read simple complex number #C(1 2)."""
        result = _parse_code('#C(1 2)')
        assert result == complex(1, 2)
        assert isinstance(result, complex)
        
    def test_read_complex_lowercase(self):
        """Read complex with lowercase #c(1 2)."""
        result = _parse_code('#c(3 4)')
        assert result == complex(3, 4)
        
    def test_read_complex_with_floats(self):
        """Read complex with float components."""
        result = _parse_code('#C(1.5 2.5)')
        assert result == complex(1.5, 2.5)
        
    def test_read_complex_with_negative(self):
        """Read complex with negative components."""
        result = _parse_code('#C(-1 -2)')
        assert result == complex(-1, -2)
        
    def test_read_complex_zero_imaginary(self):
        """Read complex with zero imaginary part."""
        result = _parse_code('#C(5 0)')
        assert result == complex(5, 0)
        
    def test_read_complex_zero_real(self):
        """Read complex with zero real part."""
        result = _parse_code('#C(0 7)')
        assert result == complex(0, 7)
        
    def test_read_complex_with_whitespace(self):
        """Read complex with extra whitespace."""
        result = _parse_code('#C(  3   4  )')
        assert result == complex(3, 4)


class TestComplexConstructor:
    """Test COMPLEX function for constructing complex numbers."""
    
    def test_complex_two_args(self):
        """COMPLEX with two arguments."""
        result = complex_fn(3, 4)
        assert result == complex(3, 4)
        
    def test_complex_one_arg(self):
        """COMPLEX with one argument (imaginary defaults to 0)."""
        result = complex_fn(5)
        assert result == complex(5, 0)
        
    def test_complex_floats(self):
        """COMPLEX with float arguments."""
        result = complex_fn(1.5, 2.5)
        assert result == complex(1.5, 2.5)
        
    def test_complex_negative(self):
        """COMPLEX with negative arguments."""
        result = complex_fn(-3, -4)
        assert result == complex(-3, -4)


class TestComplexParts:
    """Test REALPART and IMAGPART functions."""
    
    def test_realpart_complex(self):
        """REALPART of complex number."""
        assert realpart(complex(3, 4)) == 3
        
    def test_imagpart_complex(self):
        """IMAGPART of complex number."""
        assert imagpart(complex(3, 4)) == 4
        
    def test_realpart_real_number(self):
        """REALPART of real number is itself."""
        assert realpart(5) == 5
        assert realpart(3.14) == 3.14
        
    def test_imagpart_real_number(self):
        """IMAGPART of real number is 0."""
        assert imagpart(5) == 0
        assert imagpart(3.14) == 0


class TestComplexOperations:
    """Test complex number operations."""
    
    def test_conjugate(self):
        """CONJUGATE of complex number."""
        result = conjugate(complex(3, 4))
        assert result == complex(3, -4)
        
    def test_conjugate_real(self):
        """CONJUGATE of real number is itself."""
        assert conjugate(5) == 5
        
    def test_phase(self):
        """PHASE of complex number."""
        import math
        # phase of 1+i is pi/4
        result = phase(complex(1, 1))
        assert abs(result - math.pi/4) < 0.0001
        
    def test_phase_real_positive(self):
        """PHASE of positive real is 0."""
        assert phase(5) == 0
        
    def test_phase_real_negative(self):
        """PHASE of negative real is pi."""
        import math
        assert abs(phase(-5) - math.pi) < 0.0001
        
    def test_cis(self):
        """CIS returns cos(x) + i*sin(x)."""
        import math
        result = cis(math.pi/4)
        expected = complex(math.cos(math.pi/4), math.sin(math.pi/4))
        assert abs(result.real - expected.real) < 0.0001
        assert abs(result.imag - expected.imag) < 0.0001


class TestComplexArithmetic:
    """Test arithmetic with complex numbers."""
    
    def test_complex_addition(self):
        """Adding complex numbers."""
        c1 = complex(1, 2)
        c2 = complex(3, 4)
        assert c1 + c2 == complex(4, 6)
        
    def test_complex_subtraction(self):
        """Subtracting complex numbers."""
        c1 = complex(5, 7)
        c2 = complex(2, 3)
        assert c1 - c2 == complex(3, 4)
        
    def test_complex_multiplication(self):
        """Multiplying complex numbers."""
        c1 = complex(1, 2)
        c2 = complex(3, 4)
        # (1+2i)(3+4i) = 3 + 4i + 6i + 8i² = 3 + 10i - 8 = -5 + 10i
        assert c1 * c2 == complex(-5, 10)
        
    def test_complex_division(self):
        """Dividing complex numbers."""
        c1 = complex(4, 2)
        c2 = complex(2, 0)
        assert c1 / c2 == complex(2, 1)
        
    def test_complex_abs(self):
        """Absolute value (magnitude) of complex number."""
        import math
        c = complex(3, 4)
        assert abs(c) == 5  # 3-4-5 triangle


class TestComplexTypep:
    """Test TYPEP with complex numbers."""
    
    def test_complex_is_complex(self):
        """Complex numbers are COMPLEX type."""
        assert typep(complex(1, 2), 'COMPLEX') == T
        
    def test_complex_is_number(self):
        """Complex numbers are NUMBER type."""
        assert typep(complex(1, 2), 'NUMBER') == T
        
    def test_complex_is_not_real(self):
        """Complex numbers are not REAL type."""
        assert typep(complex(1, 2), 'REAL') == NIL
        
    def test_real_is_not_complex(self):
        """Real numbers are not COMPLEX type."""
        assert typep(5, 'COMPLEX') == NIL
        assert typep(3.14, 'COMPLEX') == NIL


class TestComplexp:
    """Test COMPLEXP predicate."""
    
    def test_complexp_true(self):
        """COMPLEXP returns True for complex numbers."""
        assert complexp(complex(1, 2)) == True
        assert complexp(1+2j) == True
        
    def test_complexp_false(self):
        """COMPLEXP returns False for non-complex."""
        assert complexp(5) == False
        assert complexp(3.14) == False
        assert complexp("hello") == False
