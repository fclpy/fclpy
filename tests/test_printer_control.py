"""Tests for printer control variables (Phase 8 Task 6)."""

import pytest
from fclpy.lisptype import T, NIL, LispSymbol
from fclpy.lispfunc.io_write import (
    PrinterSettings, get_printer_settings, set_printer_setting,
    get_printer_setting, _print_with_limits
)


class TestPrinterSettings:
    """Test PrinterSettings class."""
    
    def test_create_settings(self):
        """Can create PrinterSettings."""
        settings = PrinterSettings()
        assert settings is not None
        
    def test_default_values(self):
        """Default values are correct."""
        settings = PrinterSettings()
        assert settings.print_level is None
        assert settings.print_length is None
        assert settings.print_case == 'UPCASE'
        assert settings.print_circle is False
        assert settings.print_gensym is True
        assert settings.print_array is True
        assert settings.print_readably is False
        assert settings.print_escape is True
        assert settings.print_base == 10
        assert settings.print_radix is False
        
    def test_copy_settings(self):
        """Can copy settings."""
        settings = PrinterSettings()
        settings.print_level = 5
        settings.print_case = 'DOWNCASE'
        
        copy = settings.copy()
        assert copy.print_level == 5
        assert copy.print_case == 'DOWNCASE'
        
        # Changes to copy don't affect original
        copy.print_level = 10
        assert settings.print_level == 5


class TestGetSetPrinterSetting:
    """Test get/set printer setting functions."""
    
    def test_set_print_level(self):
        """Can set *PRINT-LEVEL*."""
        original = get_printer_setting('PRINT-LEVEL')
        try:
            set_printer_setting('PRINT-LEVEL', 3)
            assert get_printer_setting('PRINT-LEVEL') == 3
        finally:
            set_printer_setting('PRINT-LEVEL', original)
            
    def test_set_print_length(self):
        """Can set *PRINT-LENGTH*."""
        original = get_printer_setting('PRINT-LENGTH')
        try:
            set_printer_setting('PRINT-LENGTH', 10)
            assert get_printer_setting('PRINT-LENGTH') == 10
        finally:
            set_printer_setting('PRINT-LENGTH', original)
            
    def test_set_print_case(self):
        """Can set *PRINT-CASE*."""
        original = get_printer_setting('PRINT-CASE')
        try:
            set_printer_setting('PRINT-CASE', 'DOWNCASE')
            assert get_printer_setting('PRINT-CASE') == 'DOWNCASE'
        finally:
            set_printer_setting('PRINT-CASE', original)
            
    def test_get_printer_settings(self):
        """get_printer_settings returns settings object."""
        settings = get_printer_settings()
        assert isinstance(settings, PrinterSettings)


class TestPrintWithLimits:
    """Test _print_with_limits function."""
    
    def test_print_simple_values(self):
        """Print simple values."""
        assert _print_with_limits(42) == '42'
        assert _print_with_limits(NIL) == 'NIL'
        assert _print_with_limits(T) == 'T'
        
    def test_print_list(self):
        """Print list."""
        result = _print_with_limits([1, 2, 3])
        assert '1' in result and '2' in result and '3' in result
        
    def test_print_level_limit(self):
        """*PRINT-LEVEL* limits nesting depth."""
        original = get_printer_setting('PRINT-LEVEL')
        try:
            set_printer_setting('PRINT-LEVEL', 2)
            
            # Level 0: outer list
            # Level 1: inner list
            # Level 2: innermost - should show #
            nested = [[['deep']]]
            result = _print_with_limits(nested)
            assert '#' in result
        finally:
            set_printer_setting('PRINT-LEVEL', original)
            
    def test_print_length_limit(self):
        """*PRINT-LENGTH* limits list length."""
        original = get_printer_setting('PRINT-LENGTH')
        try:
            set_printer_setting('PRINT-LENGTH', 3)
            
            long_list = [1, 2, 3, 4, 5, 6, 7, 8]
            result = _print_with_limits(long_list)
            assert '...' in result
            assert '1' in result and '2' in result and '3' in result
        finally:
            set_printer_setting('PRINT-LENGTH', original)
            
    def test_print_no_limits(self):
        """Without limits, print everything."""
        original_level = get_printer_setting('PRINT-LEVEL')
        original_length = get_printer_setting('PRINT-LENGTH')
        try:
            set_printer_setting('PRINT-LEVEL', None)
            set_printer_setting('PRINT-LENGTH', None)
            
            nested = [[[['very', 'deep']]]]
            result = _print_with_limits(nested)
            assert '#' not in result
            assert 'very' in result
        finally:
            set_printer_setting('PRINT-LEVEL', original_level)
            set_printer_setting('PRINT-LENGTH', original_length)


class TestPrintCase:
    """Test *PRINT-CASE* behavior."""
    
    def test_print_case_upcase(self):
        """UPCASE prints symbols in uppercase."""
        original = get_printer_setting('PRINT-CASE')
        original_gensym = get_printer_setting('PRINT-GENSYM')
        try:
            set_printer_setting('PRINT-CASE', 'UPCASE')
            set_printer_setting('PRINT-GENSYM', False)  # Don't prefix uninterned
            sym = LispSymbol('HELLO')
            result = _print_with_limits(sym)
            assert result == 'HELLO'
        finally:
            set_printer_setting('PRINT-CASE', original)
            set_printer_setting('PRINT-GENSYM', original_gensym)
            
    def test_print_case_downcase(self):
        """DOWNCASE prints symbols in lowercase."""
        original = get_printer_setting('PRINT-CASE')
        original_gensym = get_printer_setting('PRINT-GENSYM')
        try:
            set_printer_setting('PRINT-CASE', 'DOWNCASE')
            set_printer_setting('PRINT-GENSYM', False)  # Don't prefix uninterned
            sym = LispSymbol('HELLO')
            result = _print_with_limits(sym)
            assert result == 'hello'
        finally:
            set_printer_setting('PRINT-CASE', original)
            set_printer_setting('PRINT-GENSYM', original_gensym)
            
    def test_print_case_capitalize(self):
        """CAPITALIZE capitalizes symbols."""
        original = get_printer_setting('PRINT-CASE')
        original_gensym = get_printer_setting('PRINT-GENSYM')
        try:
            set_printer_setting('PRINT-CASE', 'CAPITALIZE')
            set_printer_setting('PRINT-GENSYM', False)  # Don't prefix uninterned
            sym = LispSymbol('HELLO')
            result = _print_with_limits(sym)
            assert result == 'Hello'
        finally:
            set_printer_setting('PRINT-CASE', original)
            set_printer_setting('PRINT-GENSYM', original_gensym)


class TestPrintGensym:
    """Test *PRINT-GENSYM* behavior."""
    
    def test_print_gensym_true(self):
        """With *PRINT-GENSYM* true, uninterned symbols have #: prefix."""
        original = get_printer_setting('PRINT-GENSYM')
        try:
            set_printer_setting('PRINT-GENSYM', True)
            sym = LispSymbol('G123')
            sym.package = None  # Uninterned
            result = _print_with_limits(sym)
            assert result.startswith('#:')
        finally:
            set_printer_setting('PRINT-GENSYM', original)
            
    def test_print_gensym_false(self):
        """With *PRINT-GENSYM* false, no #: prefix."""
        original = get_printer_setting('PRINT-GENSYM')
        try:
            set_printer_setting('PRINT-GENSYM', False)
            sym = LispSymbol('G123')
            sym.package = None  # Uninterned
            result = _print_with_limits(sym)
            assert not result.startswith('#:')
        finally:
            set_printer_setting('PRINT-GENSYM', original)


class TestPrintEscape:
    """Test *PRINT-ESCAPE* behavior."""
    
    def test_print_escape_true(self):
        """With *PRINT-ESCAPE* true, strings are quoted."""
        original = get_printer_setting('PRINT-ESCAPE')
        try:
            set_printer_setting('PRINT-ESCAPE', True)
            result = _print_with_limits("hello")
            assert result.startswith('"') and result.endswith('"')
        finally:
            set_printer_setting('PRINT-ESCAPE', original)
            
    def test_print_escape_false(self):
        """With *PRINT-ESCAPE* false, strings are not quoted."""
        original = get_printer_setting('PRINT-ESCAPE')
        try:
            set_printer_setting('PRINT-ESCAPE', False)
            result = _print_with_limits("hello")
            assert not result.startswith('"')
        finally:
            set_printer_setting('PRINT-ESCAPE', original)


class TestPrintRadix:
    """Test *PRINT-RADIX* and *PRINT-BASE* behavior."""
    
    def test_print_base_default(self):
        """Default base 10 prints normally."""
        result = _print_with_limits(42)
        assert result == '42'
        
    def test_print_radix_hex(self):
        """With radix and base 16, print hex."""
        original_radix = get_printer_setting('PRINT-RADIX')
        original_base = get_printer_setting('PRINT-BASE')
        try:
            set_printer_setting('PRINT-RADIX', True)
            set_printer_setting('PRINT-BASE', 16)
            result = _print_with_limits(255)
            assert '#x' in result.lower() or 'FF' in result.upper()
        finally:
            set_printer_setting('PRINT-RADIX', original_radix)
            set_printer_setting('PRINT-BASE', original_base)
            
    def test_print_radix_octal(self):
        """With radix and base 8, print octal."""
        original_radix = get_printer_setting('PRINT-RADIX')
        original_base = get_printer_setting('PRINT-BASE')
        try:
            set_printer_setting('PRINT-RADIX', True)
            set_printer_setting('PRINT-BASE', 8)
            result = _print_with_limits(64)
            assert '#o' in result.lower() or '100' in result
        finally:
            set_printer_setting('PRINT-RADIX', original_radix)
            set_printer_setting('PRINT-BASE', original_base)
