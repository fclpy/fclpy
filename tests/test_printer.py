"""
Tests for the Lisp printer (prin1 and princ).

Tests that objects are printed in proper format and can be read back.
"""

import pytest
from fclpy import lispenv
from fclpy.printer import prin1, princ, print_object
from fclpy.reader import read
from fclpy.lisptype import (
    LispSymbol, lispKeyword, Character, lispCons, NIL,
    COMMON_LISP_USER_PACKAGE, intern_symbol, intern_keyword
)


@pytest.fixture(autouse=True)
def standard_packages():
    """Bootstrap the standard environment before each test in this file.

    Whether a symbol prints with a package prefix depends on package state
    (CLHS 22.1.3.3): `QUOTE` prints bare only because it is external in
    COMMON-LISP and COMMON-LISP-USER uses it. Those exports are established by
    `setup_standard_environment`, so without this the expected output depends on
    whether some earlier test in the session happened to bootstrap first.
    """
    lispenv.setup_standard_environment()


class TestPrintIntegers:
    """Test printing integers."""
    
    def test_print_positive_integer(self):
        """Test printing positive integer."""
        assert prin1(42) == "42"
    
    def test_print_negative_integer(self):
        """Test printing negative integer."""
        assert prin1(-42) == "-42"
    
    def test_print_zero(self):
        """Test printing zero."""
        assert prin1(0) == "0"
    
    def test_prin1_princ_same_for_integers(self):
        """Test that prin1 and princ are same for integers."""
        assert prin1(42) == princ(42)


class TestPrintFloats:
    """Test printing floating-point numbers."""
    
    def test_print_float(self):
        """Test printing float."""
        assert prin1(3.14) == "3.14"
    
    def test_print_negative_float(self):
        """Test printing negative float."""
        assert prin1(-3.14) == "-3.14"
    
    def test_prin1_princ_same_for_floats(self):
        """Test that prin1 and princ are same for floats."""
        assert prin1(3.14) == princ(3.14)


class TestPrintStrings:
    """Test printing string literals."""
    
    def test_print_simple_string(self):
        """Test printing simple string."""
        result = prin1("hello")
        assert result == '"hello"'
    
    def test_print_string_with_spaces(self):
        """Test printing string with spaces."""
        result = prin1("hello world")
        assert result == '"hello world"'
    
    def test_print_string_with_escaped_quotes(self):
        """Test printing string with quotes."""
        result = prin1('say "hi"')
        assert result == '"say \\"hi\\""'
    
    def test_print_string_with_newline(self):
        r"""A newline in a string prints as a newline, not as `\n`.

        CLHS 22.1.3.4 escapes only `"` and `\` inside a string, because CLHS
        2.4.5 makes backslash a *single escape* character that is included
        without interpretation. Emitting `\n` would read back as the two
        characters `\` and `n`, so this test previously asserted a
        representation that does not round-trip.
        """
        result = prin1("line1\nline2")
        assert result == '"line1\nline2"'

    def test_print_string_with_tab(self):
        r"""A tab prints as a tab, for the same reason as the newline above."""
        result = prin1("col1\tcol2")
        assert result == '"col1\tcol2"'
    
    def test_princ_string_no_quotes(self):
        """Test that princ doesn't quote strings."""
        result = princ("hello")
        assert result == "hello"
    
    def test_princ_string_no_escaping(self):
        """Test that princ doesn't escape strings."""
        result = princ("hello\nworld")
        assert result == "hello\nworld"


class TestPrintSymbols:
    """Test printing symbols."""
    
    def test_print_simple_symbol(self):
        """Test printing simple symbol."""
        sym = intern_symbol("foo")
        assert prin1(sym) == "FOO"
    
    def test_print_symbol_lowercase(self):
        """Test that symbols print in uppercase."""
        sym = intern_symbol("foo")
        result = prin1(sym)
        assert result == "FOO"
    
    def test_print_symbol_with_hyphen(self):
        """Test printing symbol with hyphens."""
        sym = intern_symbol("my-symbol")
        assert prin1(sym) == "MY-SYMBOL"
    
    def test_print_symbol_with_numbers(self):
        """Test printing symbol with numbers."""
        sym = intern_symbol("foo42")
        assert prin1(sym) == "FOO42"
    
    def test_print_symbol_needs_quoting(self):
        """A name containing a space is printed inside `|...|`.

        `LispSymbol(...)` constructs an *uninterned* symbol, so the printed
        representation also carries the `#:` prefix `*PRINT-GENSYM*` calls for
        (CLHS 22.1.3.3).
        """
        sym = LispSymbol("FOO BAR")  # Create directly with space
        result = prin1(sym)
        assert result == "#:|FOO BAR|"
    
    def test_prin1_princ_same_for_symbols(self):
        """Test that prin1 and princ are same for symbols."""
        sym = intern_symbol("test")
        assert prin1(sym) == princ(sym)


class TestPrintKeywords:
    """Test printing keywords."""
    
    def test_print_keyword(self):
        """Test printing keyword."""
        kw = intern_keyword("foo")
        assert prin1(kw) == ":FOO"
    
    def test_print_keyword_with_hyphen(self):
        """Test printing keyword with hyphens."""
        kw = intern_keyword("my-key")
        assert prin1(kw) == ":MY-KEY"
    
    def test_princ_drops_the_keyword_colon(self):
        """PRINC prints a keyword without its package marker.

        The colon is part of the *escaped* representation, so PRIN1 keeps it and
        PRINC does not (CLHS 22.1.3.3). This test used to assert the two were
        equal, which pinned the bug that PRINC and PRIN1 were two unrelated
        representations rather than one printer with `*PRINT-ESCAPE*` bound
        differently.
        """
        kw = intern_keyword("TEST")
        assert prin1(kw) == ":TEST"
        assert princ(kw) == "TEST"


class TestPrintCharacters:
    """Test printing character literals."""
    
    def test_print_character_a(self):
        """Test printing character 'A'."""
        char = Character("A")
        result = prin1(char)
        assert result == "#\\A"
    
    def test_print_character_space(self):
        """Space prints bare under PRIN1, not as ``#\\Space`` (CLHS 22.1.3.2,
        ansi-test printer/print-characters.lsp PRINT.CHAR.3/.4)."""
        char = Character(" ")
        result = prin1(char)
        assert result == "#\\ "
    
    def test_print_character_newline(self):
        """Test printing newline character."""
        char = Character("\n")
        result = prin1(char)
        assert result == "#\\Newline"
    
    def test_print_character_tab(self):
        """Test printing tab character."""
        char = Character("\t")
        result = prin1(char)
        assert result == "#\\Tab"
    
    def test_princ_prints_a_character_without_the_reader_macro(self):
        r"""PRINC prints the character itself, PRIN1 prints `#\X`.

        CLHS 22.1.3.2. `Character.__str__` also produced `#\X`, which is what
        made `(princ #\X)` print escaped; this test asserted that equality and
        so pinned it.
        """
        char = Character("X")
        assert prin1(char) == "#\\X"
        assert princ(char) == "X"


class TestPrintLists:
    """Test printing list structures."""
    
    def test_print_empty_list(self):
        """Test printing empty list."""
        assert prin1(NIL) == "NIL"
    
    def test_print_simple_list(self):
        """Test printing simple list."""
        lst = read("(a b c)")
        result = prin1(lst)
        assert result == "(A B C)"
    
    def test_print_list_with_numbers(self):
        """Test printing list with numbers."""
        lst = read("(1 2 3)")
        result = prin1(lst)
        assert result == "(1 2 3)"
    
    def test_print_list_with_strings(self):
        """Test printing list with strings."""
        lst = read('(\"hello\" \"world\")')
        result = prin1(lst)
        assert result == '("hello" "world")'
    
    def test_print_nested_list(self):
        """Test printing nested lists."""
        lst = read("(a (b c) d)")
        result = prin1(lst)
        assert result == "(A (B C) D)"
    
    def test_print_dotted_list(self):
        """Test printing dotted list."""
        lst = read("(a . b)")
        result = prin1(lst)
        assert result == "(A . B)"
    
    def test_print_dotted_list_multiple(self):
        """Test printing dotted list with multiple elements."""
        lst = read("(a b c . d)")
        result = prin1(lst)
        assert result == "(A B C . D)"
    
    def test_prin1_princ_same_for_lists(self):
        """Test that prin1 and princ are same for lists."""
        lst = read("(a b c)")
        assert prin1(lst) == princ(lst)


class TestRoundTrip:
    """Test reading and printing round-trip."""
    
    def test_roundtrip_integer(self):
        """Test read-print-read roundtrip for integer."""
        original = 42
        printed = prin1(original)
        result = read(printed)
        assert result == original
    
    def test_roundtrip_float(self):
        """Test read-print-read roundtrip for float."""
        original = 3.14
        printed = prin1(original)
        result = read(printed)
        assert result == original
    
    def test_roundtrip_symbol(self):
        """Test read-print-read roundtrip for symbol."""
        original = read("foo")
        printed = prin1(original)
        result = read(printed)
        assert result is original  # Should be same object
    
    def test_roundtrip_keyword(self):
        """Test read-print-read roundtrip for keyword."""
        original = read(":foo")
        printed = prin1(original)
        result = read(printed)
        assert result is original  # Should be same object
    
    def test_roundtrip_simple_list(self):
        """Test read-print-read roundtrip for simple list."""
        original = read("(a b c)")
        printed = prin1(original)
        result = read(printed)
        
        # Extract elements for comparison
        def list_elements(lst):
            elements = []
            current = lst
            while isinstance(current, lispCons):
                elements.append(current.car)
                current = current.cdr
            return elements
        
        orig_elements = list_elements(original)
        result_elements = list_elements(result)
        
        assert len(orig_elements) == len(result_elements)
        for o, r in zip(orig_elements, result_elements):
            assert o is r  # Same symbol objects
    
    def test_roundtrip_nested_list(self):
        """Test read-print-read roundtrip for nested list."""
        original_text = "(a (b c) d)"
        original = read(original_text)
        printed = prin1(original)
        result = read(printed)
        
        # Verify structure
        assert isinstance(result, lispCons)
        assert isinstance(result.cdr.car, lispCons)
    
    def test_roundtrip_dotted_list(self):
        """Test read-print-read roundtrip for dotted list."""
        original = read("(a . b)")
        printed = prin1(original)
        result = read(printed)
        
        assert result.car.name == "A"
        assert result.cdr.name == "B"


class TestQuoteForms:
    """Test printing quoted forms."""
    
    def test_print_quoted_symbol(self):
        """Test printing quoted symbol."""
        lst = read("'foo")
        result = prin1(lst)
        assert result == "(QUOTE FOO)"
    
    def test_print_quoted_list(self):
        """Test printing quoted list."""
        lst = read("'(a b c)")
        result = prin1(lst)
        assert result == "(QUOTE (A B C))"
    
    def test_print_function_quote(self):
        """Test printing function quote."""
        lst = read("#'foo")
        result = prin1(lst)
        assert result == "(FUNCTION FOO)"


class TestNilAndBool:
    """Test printing NIL and boolean values."""
    
    def test_print_nil(self):
        """Test printing NIL."""
        assert prin1(NIL) == "NIL"
    
    def test_print_none(self):
        """Test printing None (equivalent to NIL)."""
        assert prin1(None) == "NIL"
    
    def test_print_true_bool(self):
        """Test printing True (Python bool)."""
        # Note: True is not typically used in Lisp, but test for compatibility
        assert prin1(True) == "T"
    
    def test_print_false_bool(self):
        """Test printing False (Python bool)."""
        assert prin1(False) == "NIL"


class TestEdgeCases:
    """Test edge cases and special situations."""
    
    def test_print_empty_symbol(self):
        """An empty name needs `|...|`; uninterned adds `#:` (CLHS 22.1.3.3)."""
        sym = LispSymbol("")  # Create directly, so it has no home package
        result = prin1(sym)
        assert result == "#:||"
    
    def test_print_symbol_that_looks_like_number(self):
        """Test printing symbol that looks like a number."""
        sym = LispSymbol("123ABC")  # Starts with number
        result = prin1(sym)
        # "123ABC" is not a number in base 10, so no `|...|` is needed -- only a
        # name that would actually *read* as a number has to be escaped. The
        # `#:` is because the symbol is uninterned.
        assert result == "#:123ABC"  # Depends on impl
    
    def test_print_string_with_backslash(self):
        """Test printing string with backslash."""
        result = prin1("path\\to\\file")
        assert result == '"path\\\\to\\\\file"'
    
    def test_print_mixed_list(self):
        """Test printing list with mixed types."""
        lst = read('(1 "hello" :key foo)')
        result = prin1(lst)
        assert "1" in result
        assert '"hello"' in result
        assert ":KEY" in result
        assert "FOO" in result
