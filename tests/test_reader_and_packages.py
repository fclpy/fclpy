"""
Tests for reader and package system integration.

Tests that symbols are properly interned into the current package,
keywords self-evaluate, and symbol identity is preserved.
"""

import pytest
from conftest import read, read_all, read_in_package
from fclpy.lispreader import ReaderErrorSignal
from fclpy.lisptype import (
    COMMON_LISP_USER_PACKAGE, KEYWORD_PACKAGE, 
    intern_symbol, intern_keyword, LispSymbol, lispKeyword
)
import fclpy.state as state


class TestReaderBasics:
    """Test basic reader functionality."""
    
    def test_read_integer(self):
        """Test reading integer literals."""
        result = read("42")
        assert result == 42
    
    def test_read_float(self):
        """Test reading float literals."""
        result = read("3.14")
        assert result == 3.14
    
    def test_read_string(self):
        """Test reading string literals."""
        result = read('"hello"')
        assert result == "hello"
    
    def test_read_symbol(self):
        """Test reading symbols."""
        result = read("foo")
        assert isinstance(result, LispSymbol)
        assert result.name == "FOO"
    
    def test_read_keyword(self):
        """Test reading keyword literals."""
        result = read(":foo")
        assert isinstance(result, lispKeyword)
        assert result.name == "FOO"


class TestSymbolInterning:
    """Test that symbols are properly interned into packages."""
    
    def test_symbol_identity_same_name(self):
        """Test that reading the same symbol twice gives the same object."""
        result1 = read("foo")
        result2 = read("foo")
        assert result1 is result2  # Same Python object
    
    def test_symbol_identity_different_text(self):
        """Test that symbols with same uppercase name are identical."""
        result1 = read("FOO")
        result2 = read("foo")
        assert result1 is result2
    
    def test_symbol_in_list(self):
        """Test symbol interning in list context."""
        result = read("(foo bar foo)")
        # Extract symbols from list
        foo1 = result.car
        foo2 = result.cdr.cdr.car
        assert foo1 is foo2  # Should be same object
    
    def test_different_symbols(self):
        """Test that different symbols are different objects."""
        result1 = read("foo")
        result2 = read("bar")
        assert result1 is not result2
    
    def test_symbol_package_location(self):
        """Test that symbols are interned in the correct package."""
        result = read_in_package("test-symbol", COMMON_LISP_USER_PACKAGE)
        assert result.package == COMMON_LISP_USER_PACKAGE
    
    def test_read_same_symbol_multiple_times(self):
        """Test reading the same symbol in different calls."""
        sym1 = read("my-symbol")
        sym2 = read("my-symbol")
        sym3 = read("MY-SYMBOL")
        
        assert sym1 is sym2
        assert sym2 is sym3
        assert id(sym1) == id(sym2) == id(sym3)


class TestKeywordHandling:
    """Test keyword handling and self-evaluation."""
    
    def test_keyword_created(self):
        """Test that keywords are created."""
        result = read(":foo")
        assert isinstance(result, lispKeyword)
    
    def test_keyword_name(self):
        """Test keyword name is uppercase."""
        result = read(":foo")
        assert result.name == "FOO"
    
    def test_keyword_identity(self):
        """Test that same keyword read twice is same object."""
        kw1 = read(":foo")
        kw2 = read(":foo")
        assert kw1 is kw2
    
    def test_keyword_different(self):
        """Test that different keywords are different objects."""
        kw1 = read(":foo")
        kw2 = read(":bar")
        assert kw1 is not kw2
    
    def test_keyword_in_keyword_package(self):
        """Test that keywords are in the KEYWORD package."""
        result = read(":foo")
        assert result.package == KEYWORD_PACKAGE
    
    def test_keyword_self_evaluating(self):
        """Test that keyword evaluates to itself (conceptual test)."""
        kw = read(":test")
        assert isinstance(kw, lispKeyword)
        # Keywords should self-evaluate when evaluated
        assert kw.name == "TEST"


class TestListReading:
    """Test reading list structures."""
    
    def test_read_empty_list(self):
        """Test reading empty list."""
        result = read("()")
        from fclpy.lisptype import NIL
        assert result == NIL
    
    def test_read_list_with_symbols(self):
        """Test reading list of symbols."""
        result = read("(a b c)")
        symbols = []
        current = result
        from fclpy.lisptype import lispCons, NIL
        while isinstance(current, lispCons):
            symbols.append(current.car)
            current = current.cdr
        
        assert len(symbols) == 3
        assert all(isinstance(s, LispSymbol) for s in symbols)
    
    def test_read_nested_list(self):
        """Test reading nested lists."""
        result = read("(a (b c) d)")
        from fclpy.lisptype import lispCons
        
        # First element is 'a'
        assert result.car.name == "A"
        
        # Second element is (b c)
        nested = result.cdr.car
        assert isinstance(nested, lispCons)
        assert nested.car.name == "B"
    
    def test_read_list_with_numbers(self):
        """Test reading list with mixed types."""
        result = read("(1 foo 3.14)")
        from fclpy.lisptype import lispCons
        
        assert result.car == 1
        assert isinstance(result.cdr.car, LispSymbol)
        assert result.cdr.cdr.car == 3.14
    
    def test_unbalanced_paren_unclosed(self):
        """Test that unclosed paren raises error."""
        with pytest.raises(EOFError):
            read("(a b c")
    
    def test_unbalanced_paren_extra_close(self):
        """Test that extra close paren raises error."""
        with pytest.raises(ReaderErrorSignal):
            read(")")
    
    def test_dotted_list(self):
        """Test reading dotted list."""
        result = read("(a . b)")
        from fclpy.lisptype import lispCons
        
        assert isinstance(result, lispCons)
        assert result.car.name == "A"
        assert isinstance(result.cdr, LispSymbol)
        assert result.cdr.name == "B"
    
    def test_quoted_list(self):
        """Test reading quoted list."""
        result = read("'(a b)")
        from fclpy.lisptype import lispCons
        
        # Should be (quote (a b))
        assert isinstance(result, lispCons)
        assert result.car.name == "QUOTE"


class TestVectorReading:
    """Test reading vector literals."""
    
    def test_read_empty_vector(self):
        """Test reading empty vector #()."""
        result = read("#()")
        # CLHS 2.4.8.3: #(...) denotes a simple vector. The dead `fclpy.reader`
        # turned the literal into a *call* to VECTOR, which is what this
        # asserted; a Python list is this implementation's simple general
        # vector (CLAUDE.md's array model).
        assert isinstance(result, list)
        assert len(result) == 0

    def test_read_vector_with_elements(self):
        """#(1 2 3) reads as a simple vector of the three elements."""
        result = read("#(1 2 3)")
        # Not a (VECTOR 1 2 3) call form, which is what the dead reader built.
        assert isinstance(result, list)
        assert result == [1, 2, 3]


    def test_read_vector_with_symbols(self):
        """#(a b c) reads as a simple vector of the three symbols.

        This walked `result.cdr` expecting the dead reader's (VECTOR a b c)
        form -- with a `while` loop that never advanced `current`, so it could
        only ever spin or fail.
        """
        result = read("#(a b c)")
        assert isinstance(result, list)
        assert [s.name for s in result] == ["A", "B", "C"]


class TestQuoteVariants:
    """Test reading the quote reader macros."""

    def test_function_quote(self):
        """Test reading #' function quote."""
        result = read("#'foo")
        from fclpy.lisptype import lispCons
        
        # Should be (FUNCTION foo)
        assert isinstance(result, lispCons)
        assert result.car.name == "FUNCTION"
        assert result.cdr.car.name == "FOO"
    
    def test_quote(self):
        """Test reading ' quote."""
        result = read("'foo")
        from fclpy.lisptype import lispCons
        
        # Should be (QUOTE foo)
        assert isinstance(result, lispCons)
        assert result.car.name == "QUOTE"
    
    def test_backquote(self):
        """Test reading ` backquote."""
        result = read("`foo")
        from fclpy.lisptype import lispCons
        
        # Should be (QUASIQUOTE foo)
        assert isinstance(result, lispCons)
        assert result.car.name == "QUASIQUOTE"
    
    def test_comma(self):
        """Test reading , unquote."""
        # CLHS 2.4.6: a comma is only meaningful inside a backquote, and it is
        # an error for one to appear outside. The live reader signals; the dead
        # reader built an (UNQUOTE foo) form, which this asserted.
        with pytest.raises(ReaderErrorSignal):
            read(",foo")
        # inside a backquote it is read as the unquote
        assert str(read("`(a ,foo)")) == "(QUASIQUOTE (A (UNQUOTE FOO)))"


class TestReadAll:
    """Test read_all function for multiple objects."""
    
    def test_read_all_integers(self):
        """Test reading multiple integers."""
        results = read_all("1 2 3")
        assert results == [1, 2, 3]
    
    def test_read_all_mixed(self):
        """Test reading mixed object types."""
        results = read_all("1 foo 3.14")
        from fclpy.lisptype import LispSymbol
        
        assert len(results) == 3
        assert results[0] == 1
        assert isinstance(results[1], LispSymbol)
        assert results[2] == 3.14
    
    def test_read_all_lists(self):
        """Test reading multiple lists."""
        results = read_all("(a b) (c d)")
        from fclpy.lisptype import lispCons
        
        assert len(results) == 2
        assert all(isinstance(r, lispCons) for r in results)
    
    def test_read_all_empty(self):
        """Test read_all on empty input."""
        results = read_all("")
        assert results == []


class TestPackageContext:
    """Test reader in different package contexts."""
    
    def test_reader_with_explicit_package(self):
        """Test reader with explicit package."""
        result = read_in_package("test-symbol", COMMON_LISP_USER_PACKAGE)
        
        assert result.package == COMMON_LISP_USER_PACKAGE
    
    def test_keyword_always_in_keyword_package(self):
        """Test that keywords always go to KEYWORD package regardless of reader package."""
        result = read_in_package(":test", COMMON_LISP_USER_PACKAGE)
        
        assert result.package == KEYWORD_PACKAGE


class TestCharacterReading:
    """Test reading character literals."""
    
    def test_read_character_single(self):
        """Test reading single character literal."""
        result = read("#\\A")
        from fclpy.lisptype import Character
        
        assert isinstance(result, Character)
        assert result.char == "A"
    
    def test_read_character_space(self):
        """Test reading Space character literal."""
        result = read("#\\Space")
        from fclpy.lisptype import Character
        
        assert isinstance(result, Character)
        assert result.char == " "
    
    def test_read_character_identity(self):
        """Test that same character read twice might be same object."""
        result1 = read("#\\A")
        result2 = read("#\\A")
        
        from fclpy.lisptype import Character
        assert isinstance(result1, Character)
        assert isinstance(result2, Character)
        assert result1.char == result2.char


class TestErrorHandling:
    """Test error conditions."""
    
    def test_unexpected_eof(self):
        """Test that EOF in list raises error."""
        with pytest.raises(EOFError):
            read("(a b")
    
    def test_unbalanced_paren(self):
        """Test that bare closing paren raises error."""
        with pytest.raises(ReaderErrorSignal):
            read(")")
    
    def test_empty_input(self):
        """Empty input ends cleanly rather than mid-object.

        `read_1` answers None at a clean end of input, which is what lets
        `read_all` and the LOAD/COMPILE-FILE form loops terminate. The ANSI
        boundary -- `(read s)` signalling END-OF-FILE -- is at the Lisp `READ`.
        """
        assert read("") is None
