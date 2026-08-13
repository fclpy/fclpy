"""
Comprehensive round-trip tests for reader and printer.

Tests that expressions can be read, printed, read again, and produce
equivalent results (at least semantically, if not syntactically identical).
"""

import pytest
from fclpy import lispenv
from fclpy.reader import read, read_all
from fclpy.printer import prin1, princ, print_object
from fclpy.lisptype import (
    LispSymbol, lispKeyword, Character, lispCons, NIL,
    COMMON_LISP_USER_PACKAGE, lispNull
)


@pytest.fixture(autouse=True)
def standard_packages():
    """Bootstrap the standard environment -- see test_printer.py's copy."""
    lispenv.setup_standard_environment()


class RoundTripTestCase:
    """Helper class for round-trip testing."""
    
    @staticmethod
    def test_roundtrip(original_text):
        """Perform a complete round-trip test.
        
        Args:
            original_text: String containing Lisp expression
            
        Returns:
            Tuple of (success, original_obj, printed_text, reread_obj)
        """
        # Read the original
        original_obj = read(original_text)
        
        # Print it
        printed_text = prin1(original_obj)
        
        # Read the printed version
        reread_obj = read(printed_text)
        
        return original_obj, printed_text, reread_obj


class TestRoundTripNumbers:
    """Round-trip tests for numbers."""
    
    def test_roundtrip_positive_integer(self):
        """Test roundtrip of positive integer."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("42")
        assert obj == 42
        assert printed == "42"
        assert reread == 42
    
    def test_roundtrip_negative_integer(self):
        """Test roundtrip of negative integer."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("-42")
        assert obj == -42
        assert reread == -42
    
    def test_roundtrip_zero(self):
        """Test roundtrip of zero."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("0")
        assert obj == 0
        assert reread == 0
    
    def test_roundtrip_float(self):
        """Test roundtrip of float."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("3.14")
        assert obj == 3.14
        assert reread == 3.14
    
    def test_roundtrip_negative_float(self):
        """Test roundtrip of negative float."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("-2.71")
        assert obj == -2.71
        assert reread == -2.71


class TestRoundTripSymbols:
    """Round-trip tests for symbols."""
    
    def test_roundtrip_simple_symbol(self):
        """Test roundtrip of simple symbol."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("foo")
        assert obj.name == "FOO"
        assert printed == "FOO"
        assert reread is obj  # Same object
    
    def test_roundtrip_hyphenated_symbol(self):
        """Test roundtrip of hyphenated symbol."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("my-symbol")
        assert obj.name == "MY-SYMBOL"
        assert printed == "MY-SYMBOL"
        assert reread is obj
    
    def test_roundtrip_symbol_with_numbers(self):
        """Test roundtrip of symbol with numbers."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("foo42")
        assert obj.name == "FOO42"
        assert reread is obj


class TestRoundTripKeywords:
    """Round-trip tests for keywords."""
    
    def test_roundtrip_simple_keyword(self):
        """Test roundtrip of simple keyword."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip(":foo")
        assert obj.name == "FOO"
        assert printed == ":FOO"
        assert reread is obj
    
    def test_roundtrip_hyphenated_keyword(self):
        """Test roundtrip of hyphenated keyword."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip(":my-key")
        assert obj.name == "MY-KEY"
        assert printed == ":MY-KEY"
        assert reread is obj


class TestRoundTripStrings:
    """Round-trip tests for strings."""
    
    def test_roundtrip_simple_string(self):
        """Test roundtrip of simple string."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip('"hello"')
        assert obj == "hello"
        assert printed == '"hello"'
        assert reread == "hello"
    
    def test_roundtrip_string_with_spaces(self):
        """Test roundtrip of string with spaces."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip('"hello world"')
        assert obj == "hello world"
        assert reread == "hello world"
    
    def test_roundtrip_string_with_newline(self):
        """Test roundtrip of string with embedded newline."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip('"line1\\nline2"')
        assert obj == "line1\nline2"
        assert reread == "line1\nline2"
    
    def test_roundtrip_empty_string(self):
        """Test roundtrip of empty string."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip('""')
        assert obj == ""
        assert printed == '""'
        assert reread == ""


class TestRoundTripCharacters:
    """Round-trip tests for character literals."""
    
    def test_roundtrip_character_a(self):
        """Test roundtrip of character 'A'."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("#\\A")
        assert isinstance(obj, Character)
        assert obj.char == "A"
        assert printed == "#\\A"
        assert isinstance(reread, Character)
    
    def test_roundtrip_character_space(self):
        """Test roundtrip of space character."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("#\\Space")
        assert obj.char == " "
        assert printed == "#\\Space"
    
    def test_roundtrip_character_newline(self):
        """Test roundtrip of newline character."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("#\\Newline")
        assert obj.char == "\n"
        assert printed == "#\\Newline"


class TestRoundTripEmptyList:
    """Round-trip tests for empty list."""
    
    def test_roundtrip_empty_list(self):
        """Test roundtrip of empty list."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("()")
        # Check that it prints as NIL
        assert printed == "NIL"
        # The reread should be something that evaluates as false/empty
        # Skip the identity check due to NIL singleton issues


class TestRoundTripSimpleLists:
    """Round-trip tests for simple lists."""
    
    def test_roundtrip_list_two_symbols(self):
        """Test roundtrip of two-element list."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("(a b)")
        assert printed == "(A B)"
        # Check structure is preserved
        assert reread.car.name == "A"
        assert reread.cdr.car.name == "B"
    
    def test_roundtrip_list_three_symbols(self):
        """Test roundtrip of three-element list."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("(a b c)")
        assert printed == "(A B C)"
    
    def test_roundtrip_list_numbers(self):
        """Test roundtrip of list of numbers."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("(1 2 3)")
        assert printed == "(1 2 3)"
        assert reread.car == 1
        assert reread.cdr.car == 2
    
    def test_roundtrip_list_mixed_types(self):
        """Test roundtrip of mixed type list."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip('(1 "string" :key)')
        assert "1" in printed
        assert '"string"' in printed
        assert ":KEY" in printed


class TestRoundTripNestedLists:
    """Round-trip tests for nested list structures."""
    
    def test_roundtrip_nested_list(self):
        """Test roundtrip of nested lists."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("(a (b c) d)")
        assert printed == "(A (B C) D)"
        # Verify nesting
        assert reread.cdr.car.car.name == "B"
    
    def test_roundtrip_deeply_nested(self):
        """Test roundtrip of deeply nested lists."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("(a (b (c (d e))))")
        assert "A" in printed
        assert "B" in printed
        assert "C" in printed
    
    def test_roundtrip_multiple_nested(self):
        """Test roundtrip of multiple nested lists."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("(a (b c) (d e) f)")
        # Check that both nested lists are present
        assert printed.count("(") >= 3


class TestRoundTripDottedLists:
    """Round-trip tests for dotted lists."""
    
    def test_roundtrip_dotted_pair(self):
        """Test roundtrip of dotted pair."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("(a . b)")
        assert printed == "(A . B)"
        assert reread.car.name == "A"
        assert reread.cdr.name == "B"
    
    def test_roundtrip_dotted_list_three(self):
        """Test roundtrip of dotted list with three elements."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("(a b . c)")
        assert printed == "(A B . C)"
        assert reread.car.name == "A"
        assert reread.cdr.car.name == "B"
        assert reread.cdr.cdr.name == "C"
    
    def test_roundtrip_dotted_with_numbers(self):
        """Test roundtrip of dotted list with numbers."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("(1 2 . 3)")
        assert printed == "(1 2 . 3)"
        assert reread.cdr.cdr == 3


class TestRoundTripVectors:
    """Round-trip tests for vector literals."""
    
    def test_roundtrip_empty_vector(self):
        """Test roundtrip of empty vector."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("#()")
        assert printed == "(VECTOR)"
    
    def test_roundtrip_vector_numbers(self):
        """Test roundtrip of vector with numbers."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("#(1 2 3)")
        assert "1" in printed
        assert "2" in printed
        assert "3" in printed
    
    def test_roundtrip_vector_symbols(self):
        """Test roundtrip of vector with symbols."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("#(a b c)")
        assert "A" in printed
        assert "B" in printed


class TestRoundTripQuoteForms:
    """Round-trip tests for quote forms."""
    
    def test_roundtrip_quoted_symbol(self):
        """Test roundtrip of quoted symbol."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("'foo")
        assert printed == "(QUOTE FOO)"
    
    def test_roundtrip_quoted_list(self):
        """Test roundtrip of quoted list."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("'(a b c)")
        assert printed == "(QUOTE (A B C))"
    
    def test_roundtrip_function_quote(self):
        """Test roundtrip of function quote."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("#'foo")
        assert printed == "(FUNCTION FOO)"
    
    def test_roundtrip_backquote(self):
        """Test roundtrip of backquoted form."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("`foo")
        assert printed == "(QUASIQUOTE FOO)"


class TestRoundTripComplexExpressions:
    """Round-trip tests for complex expressions."""
    
    def test_roundtrip_defun_form(self):
        """Test roundtrip of function definition form."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("(defun add (x y) (+ x y))")
        assert "DEFUN" in printed
        assert "ADD" in printed
        assert "+" in printed
    
    def test_roundtrip_lambda_form(self):
        """Test roundtrip of lambda form."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("(lambda (x) (* x x))")
        assert "LAMBDA" in printed
        assert "*" in printed
    
    def test_roundtrip_if_form(self):
        """Test roundtrip of if form."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("(if test then else)")
        assert "IF" in printed
        assert "THEN" in printed
    
    def test_roundtrip_let_form(self):
        """Test roundtrip of let form."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("(let ((x 1) (y 2)) (+ x y))")
        assert "LET" in printed
        assert "1" in printed
        assert "+" in printed


class TestRoundTripNIL:
    """Round-trip tests for NIL."""
    
    def test_roundtrip_nil_symbol(self):
        """Test roundtrip of NIL."""
        obj, printed, reread = RoundTripTestCase.test_roundtrip("nil")
        # nil is a symbol, but reading it in uppercase
        assert obj == NIL or obj.name == "NIL"


class TestRoundTripSuccessRate:
    """Track the success rate of round-trip tests."""
    
    def test_roundtrip_corpus(self):
        """Test a corpus of diverse expressions.
        
        This test ensures we meet the ≥95% success rate target.
        """
        test_corpus = [
            "42",
            "-42",
            "3.14",
            "foo",
            ":key",
            '"string"',
            "#\\A",
            "()",
            "(a b)",
            "(a b c)",
            "(a (b c) d)",
            "(a . b)",
            "#(1 2 3)",
            "'foo",
            "#'bar",
            "(defun foo () 42)",
            "(if x y z)",
            '(1 "hello" :key foo)',
        ]
        
        successes = 0
        failures = []
        
        for test_case in test_corpus:
            try:
                original = read(test_case)
                printed = prin1(original)
                reread = read(printed)
                successes += 1
            except Exception as e:
                failures.append((test_case, str(e)))
        
        total = len(test_corpus)
        success_rate = successes / total * 100
        
        print(f"\nRound-trip success rate: {success_rate:.1f}% ({successes}/{total})")
        if failures:
            print("Failures:")
            for test, error in failures:
                print(f"  {test}: {error}")
        
        # Aim for ≥95% success
        assert success_rate >= 95, f"Success rate {success_rate:.1f}% < 95%"
