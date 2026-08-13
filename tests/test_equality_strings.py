"""EQUAL/EQUALP over the two string representations (plan.md Finding I).

A Lisp string is a `LispString`, which is not a `str` subclass, so an
`isinstance(obj, str)` type test is false for every string the reader
produces. EQUAL's string branch was written that way and was therefore
dead: `(equal "abc" "abc")` returned NIL. The ANSI harness compares every
test result against its expected value with EQUAL, so this failed every
test with a string-valued expectation regardless of the code under test.
"""

import pytest

from fclpy import lisptype
from fclpy.lispfunc.comparison import equal, equalp

T, NIL = lisptype.T, lisptype.NIL
LispString = lisptype.LispString
Character = lisptype.Character


def both_forms(text):
    """The same string in each representation that reaches EQUAL."""
    return [text, LispString(text)]


class TestEqualOnStrings:

    @pytest.mark.parametrize("a", both_forms("abc"))
    @pytest.mark.parametrize("b", both_forms("abc"))
    def test_equal_strings_are_equal_in_every_representation(self, a, b):
        assert equal(a, b) is T

    @pytest.mark.parametrize("a", both_forms("abc"))
    @pytest.mark.parametrize("b", both_forms("abd"))
    def test_differing_strings_are_not_equal(self, a, b):
        assert equal(a, b) is NIL

    @pytest.mark.parametrize("a", both_forms("abc"))
    @pytest.mark.parametrize("b", both_forms("ABC"))
    def test_equal_is_case_sensitive(self, a, b):
        assert equal(a, b) is NIL

    def test_string_is_not_equal_to_a_symbol(self):
        assert equal(LispString("ABC"), lisptype.LispSymbol("ABC")) is NIL

    def test_string_is_not_equal_to_a_list(self):
        assert equal(LispString("ab"), [Character('a'), Character('b')]) is NIL

    def test_empty_strings_are_equal(self):
        assert equal(LispString(""), "") is T


class TestEqualpOnStrings:

    @pytest.mark.parametrize("a", both_forms("abc"))
    @pytest.mark.parametrize("b", both_forms("ABC"))
    def test_equalp_ignores_case(self, a, b):
        assert equalp(a, b) is T

    @pytest.mark.parametrize("a", both_forms("abc"))
    @pytest.mark.parametrize("b", both_forms("abd"))
    def test_equalp_still_compares_content(self, a, b):
        assert equalp(a, b) is NIL


class TestEqualpOnCharacters:
    """EQUALP compares characters with CHAR-EQUAL (case-insensitive); EQUAL
    and EQL remain case-sensitive."""

    def test_equalp_characters_ignore_case(self):
        assert equalp(Character('a'), Character('A')) is T

    def test_equal_characters_are_case_sensitive(self):
        assert equal(Character('a'), Character('A')) is NIL

    def test_same_character_is_equal(self):
        assert equal(Character('a'), Character('a')) is T

    def test_character_is_not_equalp_to_a_string(self):
        """A string is an array and a character is not, so the types are
        disjoint even where a character is still held as a length-1 str."""
        assert equalp(LispString("a"), Character('A')) is NIL
        assert equalp(Character('A'), LispString("a")) is NIL
