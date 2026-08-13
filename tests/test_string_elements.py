"""The elements of a string are CHARACTERs (CLHS 15.1).

Both string representations index to a bare length-1 Python `str`, so every
site that reads an element out of a string has to convert it. Getting this
wrong is not cosmetic: a string is also a vector, so a "character" that is
itself a one-element string is a one-element vector, and anything walking a
sequence element-wise follows it down forever. The ANSI harness's own
`equalp-with-case` does exactly that, so this defect made comparing two
strings exhaust the stack and abort an entire test run.
"""

import pytest

from fclpy import lisptype
from fclpy.lispfunc.comparison import equal, eql, typep
from fclpy.lispfunc.sequences_higher import aref, string_element
from fclpy.lispfunc.characters import characterp

T, NIL = lisptype.T, lisptype.NIL
Character = lisptype.Character
LispString = lisptype.LispString


def sym(name):
    return lisptype.LispSymbol(name)


class TestStringElementsAreCharacters:

    @pytest.mark.parametrize("s", ["abc", LispString("abc")])
    def test_aref_of_a_string_is_a_character(self, s):
        assert isinstance(aref(s, 0), Character)

    @pytest.mark.parametrize("s", ["abc", LispString("abc")])
    def test_aref_element_is_eql_to_the_character(self, s):
        assert eql(aref(s, 0), Character('a')) is T

    @pytest.mark.parametrize("s", ["abc", LispString("abc")])
    def test_a_string_element_is_not_itself_a_string(self, s):
        """The property that stops element-wise traversal recursing."""
        assert typep(aref(s, 0), sym('STRING')) is NIL
        assert typep(aref(s, 0), sym('VECTOR')) is NIL

    def test_aref_of_a_non_string_vector_is_unchanged(self):
        assert aref([1, 2, 3], 0) == 1

    def test_string_element_leaves_non_string_containers_alone(self):
        assert string_element([1, 2], 1) == 1
        assert string_element(["a"], "a") == "a"


class TestStringsAreVectors:
    """CLHS 15.1: a string is a vector and every vector is an array. TYPEP
    said otherwise, which is what stopped the harness comparing strings."""

    @pytest.mark.parametrize("s", ["abc", LispString("abc")])
    @pytest.mark.parametrize("type_name", ['STRING', 'VECTOR', 'ARRAY'])
    def test_a_string_is_a_string_vector_and_array(self, s, type_name):
        assert typep(s, sym(type_name)) is T

    @pytest.mark.parametrize("s", ["abc", LispString("abc")])
    def test_a_string_is_not_a_simple_vector(self, s):
        """SIMPLE-VECTOR holds elements of type T specifically."""
        assert typep(s, sym('SIMPLE-VECTOR')) is NIL

    def test_a_list_is_not_a_vector(self):
        assert typep(lisptype.lispCons(1, NIL), sym('VECTOR')) is NIL


class TestCharacterp:
    """CHARACTERP missed the Character class and returned a raw Python bool
    -- and a Python False reaching a Lisp conditional reads as true."""

    def test_character_object_is_a_character(self):
        assert characterp(Character('a')) is T

    def test_a_multi_character_string_is_not_a_character(self):
        assert characterp(LispString("ab")) is NIL

    def test_returns_lisp_booleans_not_python_bools(self):
        for value in (Character('a'), LispString("ab"), 5):
            assert characterp(value) in (T, NIL)


class TestElementWiseTraversalTerminates:
    """The regression that motivated all of the above."""

    def test_comparing_strings_element_wise_terminates(self):
        s1, s2 = LispString("abcd"), LispString("abcd")
        assert equal(s1, s2) is T
        # Walking element-wise must bottom out at characters, not recurse.
        for i in range(4):
            element = aref(s1, i)
            assert isinstance(element, Character)
            assert typep(element, sym('VECTOR')) is NIL


class TestMakeArrayFromString:
    """MAKE-ARRAY with a string :initial-contents builds an array of
    CHARACTERs, not of length-1 strings (CLHS 15.1)."""

    def test_initial_contents_from_string_yields_characters(self):
        from fclpy.lispfunc.vectors import make_array
        arr = make_array(4, initial_contents=LispString("abcd"))
        assert all(isinstance(e, Character) for e in arr)
        assert eql(arr[0], Character('a')) is T
