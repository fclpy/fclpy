"""
Comprehensive tests for Readtable class functionality.
Tests that the Readtable class properly manages macro characters and dispatch macros.
"""

import pytest
import fclpy.lisptype as lisptype
from fclpy.readtable import Readtable, get_current_readtable, set_current_readtable
from fclpy.lispfunc.io import (
    set_macro_character,
    get_macro_character,
    set_dispatch_macro_character,
    get_dispatch_macro_character,
    copy_readtable,
    readtable_case,
)


class TestReadtableBasics:
    """Test basic Readtable instantiation and standard macros."""
    
    def test_readtable_creation(self):
        """Test that a Readtable can be created."""
        rt = Readtable()
        assert rt is not None
        assert isinstance(rt, Readtable)
    
    def test_standard_macro_characters(self):
        """Test that standard macro characters are installed."""
        rt = Readtable()
        
        # Check standard terminating macro characters
        assert rt.get_macro_character('(') is not None
        assert rt.get_macro_character(')') is not None
        assert rt.get_macro_character('"') is not None
        assert rt.get_macro_character("'") is not None
        assert rt.get_macro_character(';') is not None
        
        # Check standard non-terminating macro characters
        assert rt.get_macro_character('`') is not None
        assert rt.get_macro_character(',') is not None
        
        # Check dispatch macro character
        assert rt.get_macro_character('#') is not None
    
    def test_readtable_case_upcase(self):
        """Test readtable case is UPCASE by default."""
        rt = Readtable()
        assert rt.readtable_case() == 'UPCASE'
    
    def test_set_readtable_case(self):
        """Test setting different readtable case modes."""
        rt = Readtable()
        
        rt.set_readtable_case('DOWNCASE')
        assert rt.readtable_case() == 'DOWNCASE'
        
        rt.set_readtable_case('PRESERVE')
        assert rt.readtable_case() == 'PRESERVE'
        
        rt.set_readtable_case('INVERT')
        assert rt.readtable_case() == 'INVERT'
        
        rt.set_readtable_case('UPCASE')
        assert rt.readtable_case() == 'UPCASE'


class TestMacroCharacterManagement:
    """Test setting and getting macro characters."""
    
    def test_set_and_get_macro_character(self):
        """Test setting and retrieving a custom macro character."""
        rt = Readtable()
        
        def custom_reader(char, stream):
            return "custom"
        
        rt.set_macro_character('~', custom_reader, False)
        
        result = rt.get_macro_character('~')
        assert result is not None
        assert result[0] is custom_reader
        assert result[1] == False  # non_terminating_p
    
    def test_set_macro_character_terminating_vs_non_terminating(self):
        """Test difference between terminating and non-terminating macro chars."""
        rt = Readtable()
        
        def dummy_reader(char, stream):
            return None
        
        # Set terminating
        rt.set_macro_character('!', dummy_reader, False)
        result = rt.get_macro_character('!')
        assert result[1] == False
        
        # Set non-terminating
        rt.set_macro_character('@', dummy_reader, True)
        result = rt.get_macro_character('@')
        assert result[1] == True
    
    def test_overwrite_macro_character(self):
        """Test that setting a macro character twice overwrites the first."""
        rt = Readtable()
        
        def reader1(char, stream):
            return "first"
        
        def reader2(char, stream):
            return "second"
        
        rt.set_macro_character('&', reader1, False)
        rt.set_macro_character('&', reader2, True)
        
        result = rt.get_macro_character('&')
        assert result[0] is reader2
        assert result[1] == True
    
    def test_macro_character_not_found(self):
        """Test getting a macro character that doesn't exist."""
        rt = Readtable()
        
        # Use a character that's definitely not set
        result = rt.get_macro_character('\x00')
        assert result is None


class TestDispatchMacroCharacters:
    """Test dispatch macro character management."""
    
    def test_set_and_get_dispatch_macro(self):
        """Test setting and retrieving dispatch macro characters."""
        rt = Readtable()
        
        def dispatch_reader(char, stream):
            return "dispatch"
        
        rt.set_dispatch_macro_character('#', 'x', dispatch_reader)
        
        result = rt.get_dispatch_macro_character('#', 'x')
        assert result is dispatch_reader
    
    def test_multiple_dispatch_subchars(self):
        """Test that multiple dispatch sub-characters can exist for the same dispatch char."""
        rt = Readtable()
        
        def reader_a(char, stream):
            return "a"
        
        def reader_b(char, stream):
            return "b"
        
        rt.set_dispatch_macro_character('#', 'a', reader_a)
        rt.set_dispatch_macro_character('#', 'b', reader_b)
        
        assert rt.get_dispatch_macro_character('#', 'a') is reader_a
        assert rt.get_dispatch_macro_character('#', 'b') is reader_b
    
    def test_multiple_dispatch_chars(self):
        """Test dispatch macros on different dispatch characters."""
        rt = Readtable()
        
        def reader1(char, stream):
            return "reader1"
        
        def reader2(char, stream):
            return "reader2"
        
        # Both # and $ as dispatch characters
        rt.set_dispatch_macro_character('#', 'x', reader1)
        rt.set_dispatch_macro_character('$', 'x', reader2)
        
        assert rt.get_dispatch_macro_character('#', 'x') is reader1
        assert rt.get_dispatch_macro_character('$', 'x') is reader2
    
    def test_dispatch_not_found(self):
        """Test getting a dispatch macro that doesn't exist."""
        rt = Readtable()
        
        result = rt.get_dispatch_macro_character('#', 'z')
        assert result is None
        
        # Even if dispatch char exists, if sub-char doesn't
        result = rt.get_dispatch_macro_character('#', '\x00')
        assert result is None


class TestReadtableCopy:
    """Test copying readtables."""
    
    def test_copy_creates_new_instance(self):
        """Test that copy creates a new instance."""
        rt1 = Readtable()
        rt2 = rt1.copy()
        
        assert rt1 is not rt2
        assert isinstance(rt2, Readtable)
    
    def test_copy_preserves_macro_characters(self):
        """Test that copying preserves macro character definitions."""
        rt1 = Readtable()
        
        def custom_reader(char, stream):
            return "custom"
        
        rt1.set_macro_character('~', custom_reader, False)
        rt2 = rt1.copy()
        
        # Both should have the custom macro character
        result1 = rt1.get_macro_character('~')
        result2 = rt2.get_macro_character('~')
        
        assert result1 is not None
        assert result2 is not None
        assert result1[0] is result2[0]
        assert result1[1] == result2[1]
    
    def test_copy_preserves_dispatch_macros(self):
        """Test that copying preserves dispatch macro definitions."""
        rt1 = Readtable()
        
        def dispatch_reader(char, stream):
            return "dispatch"
        
        rt1.set_dispatch_macro_character('#', 'x', dispatch_reader)
        rt2 = rt1.copy()
        
        # Both should have the dispatch macro
        result1 = rt1.get_dispatch_macro_character('#', 'x')
        result2 = rt2.get_dispatch_macro_character('#', 'x')
        
        assert result1 is dispatch_reader
        assert result2 is dispatch_reader
    
    def test_copy_preserves_case_mode(self):
        """Test that copying preserves case mode setting."""
        rt1 = Readtable()
        rt1.set_readtable_case('PRESERVE')
        
        rt2 = rt1.copy()
        
        assert rt2.readtable_case() == 'PRESERVE'
    
    def test_copy_is_independent(self):
        """Test that modifying a copy doesn't affect the original."""
        rt1 = Readtable()
        rt2 = rt1.copy()
        
        def new_reader(char, stream):
            return "new"
        
        # Add a macro to the copy
        rt2.set_macro_character('~', new_reader, False)
        
        # Original should not have it
        assert rt1.get_macro_character('~') is None
        
        # Copy should have it
        assert rt2.get_macro_character('~') is not None
    
    def test_copy_dispatch_independence(self):
        """Test that modifying dispatch macros in copy doesn't affect original."""
        rt1 = Readtable()
        rt2 = rt1.copy()
        
        def reader1(char, stream):
            return "reader1"
        
        def reader2(char, stream):
            return "reader2"
        
        # Add different dispatch macros to each
        rt1.set_dispatch_macro_character('#', 'a', reader1)
        rt2.set_dispatch_macro_character('#', 'a', reader2)
        
        # Each should have its own
        assert rt1.get_dispatch_macro_character('#', 'a') is reader1
        assert rt2.get_dispatch_macro_character('#', 'a') is reader2


class TestGlobalReadtable:
    """Test global readtable operations."""
    
    def test_get_current_readtable(self):
        """Test getting the current global readtable."""
        rt = get_current_readtable()
        assert rt is not None
        assert isinstance(rt, Readtable)
    
    def test_readtable_consistency(self):
        """Test that repeated gets return the same instance."""
        rt1 = get_current_readtable()
        rt2 = get_current_readtable()
        assert rt1 is rt2
    
    def test_set_current_readtable(self):
        """Test setting a new current readtable."""
        old_rt = get_current_readtable()
        new_rt = Readtable()
        
        set_current_readtable(new_rt)
        
        current = get_current_readtable()
        assert current is new_rt
        
        # Restore
        set_current_readtable(old_rt)
        assert get_current_readtable() is old_rt


class TestReadtableLispFunctions:
    """Test Lisp functions that work with readtables."""
    
    def test_copy_readtable_via_lispfunc(self):
        """Test COPY-READTABLE Lisp function."""
        rt = copy_readtable()
        
        assert rt is not None
        assert isinstance(rt, Readtable)
    
    def test_copy_readtable_creates_independent_copy(self):
        """Test that COPY-READTABLE creates independent copy."""
        rt1 = copy_readtable()
        
        def custom_reader(char, stream):
            return "custom"
        
        rt1.set_macro_character('~', custom_reader, False)
        
        # Get a copy of the modified readtable
        rt2 = rt1.copy()
        
        # rt2 should have the custom macro
        assert rt2.get_macro_character('~') is not None
    
    def test_readtable_case_function(self):
        """READTABLE-CASE answers a *keyword*, not a Python string (CLHS 23.2).

        This asserted `== 'DOWNCASE'`, i.e. it pinned a Python object appearing
        as a Lisp value (standing rule 2): the string is not EQ to the
        `:DOWNCASE` every caller compares the result against.
        """
        import fclpy.lisptype as lisptype

        rt = Readtable()
        rt.set_readtable_case('DOWNCASE')

        set_current_readtable(rt)
        # CLHS 23.2: READTABLE-CASE takes one required readtable argument
        # (zero arguments is a PROGRAM-ERROR, `readtable-case.error.1`).
        case = readtable_case(rt)

        assert case is lisptype.intern_keyword('DOWNCASE')

    def test_readtable_case_rejects_a_non_case(self):
        """The four values of CLHS 23.1.2 are the only ones accepted."""
        import pytest

        import fclpy.lisptype as lisptype

        rt = Readtable()
        with pytest.raises(lisptype.LispTypeError):
            rt.set_readtable_case('SIDEWAYS')
    
    def test_macro_character_functions(self):
        """Test SET-MACRO-CHARACTER and GET-MACRO-CHARACTER functions."""
        rt = Readtable()
        set_current_readtable(rt)
        
        def dummy_reader(char, stream):
            return None
        
        assert set_macro_character('~', dummy_reader, True) is lisptype.T

        # GET-MACRO-CHARACTER answers *two values* (CLHS 23.2): the function
        # and non-terminating-p. This asserted a Python 2-tuple with a Python
        # `True` in it -- the readtable's internal storage handed back as the
        # value of the form (standing rule 2).
        result = get_macro_character('~')
        assert isinstance(result, lisptype.MultipleValues)
        assert result[0] is dummy_reader
        assert result[1] is lisptype.T
    
    def test_dispatch_macro_character_functions(self):
        """Test SET-DISPATCH-MACRO-CHARACTER and GET-DISPATCH-MACRO-CHARACTER."""
        rt = Readtable()
        set_current_readtable(rt)
        
        def dummy_reader(char, stream):
            return None
        
        set_dispatch_macro_character('#', 'x', dummy_reader)
        
        result = get_dispatch_macro_character('#', 'x')
        assert result is dummy_reader


class TestReadtableEdgeCases:
    """Test edge cases and special scenarios."""
    
    def test_empty_char_handling(self):
        """Test handling of empty string char."""
        rt = Readtable()
        result = rt.get_macro_character('')
        assert result is None
    
    def test_unicode_macro_characters(self):
        """Test that unicode characters can be macro characters."""
        rt = Readtable()
        
        def unicode_reader(char, stream):
            return "unicode"
        
        rt.set_macro_character('λ', unicode_reader, False)
        
        result = rt.get_macro_character('λ')
        assert result is not None
        assert result[0] is unicode_reader
    
    def test_copy_empty_readtable(self):
        """Test copying a readtable that had standard macros removed."""
        rt1 = Readtable.__new__(Readtable)
        rt1._macro_characters = {}
        rt1._dispatch_macro_characters = {}
        rt1._case = 'UPCASE'
        
        rt2 = rt1.copy()
        
        assert len(rt2._macro_characters) == 0
        assert len(rt2._dispatch_macro_characters) == 0
        assert rt2.readtable_case() == 'UPCASE'
    
    def test_multiple_copies_independence(self):
        """Test that multiple copies are all independent and don't share changes."""
        rt1 = Readtable()
        
        # Add a macro to rt1
        def reader1(c, s): return "1"
        rt1.set_macro_character('a', reader1, False)
        
        # Now make copies - they will have 'a'
        rt2 = rt1.copy()
        rt3 = rt1.copy()
        
        def reader2(c, s): return "2"
        def reader3(c, s): return "3"
        def reader4(c, s): return "4"
        
        # Add different macros to each copy
        rt2.set_macro_character('b', reader2, False)
        rt3.set_macro_character('c', reader3, False)
        
        # rt1 should only have 'a'
        assert rt1.get_macro_character('a') is not None
        assert rt1.get_macro_character('b') is None
        assert rt1.get_macro_character('c') is None
        
        # rt2 should have both 'a' (from copy) and 'b' (newly added)
        assert rt2.get_macro_character('a') is not None
        assert rt2.get_macro_character('b') is not None
        assert rt2.get_macro_character('c') is None
        
        # rt3 should have both 'a' (from copy) and 'c' (newly added)
        assert rt3.get_macro_character('a') is not None
        assert rt3.get_macro_character('b') is None
        assert rt3.get_macro_character('c') is not None
