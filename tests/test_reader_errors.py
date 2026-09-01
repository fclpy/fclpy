"""
Tests for reader error handling -- against the reader the implementation uses.

These tests read through `fclpy.tokenizer` -> `fclpy.lispreader` ->
`fclpy.readtable`, the path every form the interpreter evaluates goes through
(CLAUDE.md's architecture map).

They previously imported `fclpy.reader`, a separate ~480-line reader that no
module under `fclpy/` imported (deleted 2026-09-01), so they measured dead code
while the real reader went untested at the unit level -- and the two disagree on conformance: the dead
one splits "123abc" into the integer 123 and "1.2.3" into the float 1.2, while
the live reader correctly answers the symbols |123ABC| and |1.2.3|. Two tests
were therefore parked as `xfail(strict=True)` describing a defect the shipping
reader does not have.

The live reader reports failure two ways, and the distinction is the ANSI one
(CLHS 23.1 / `READ`):

* input that ends in the middle of an object -> `EOFError`
* input that is malformed -> `ReaderErrorSignal`
"""

import pytest

from conftest import read, read_all
from fclpy.lispreader import ReaderErrorSignal
from fclpy.lisptype import LispSymbol


class TestEndOfFileErrors:
    """Input that ends in the middle of an object signals EOF."""

    def test_eof_in_list(self):
        with pytest.raises(EOFError):
            read("(a b c")

    def test_eof_in_nested_list(self):
        with pytest.raises(EOFError):
            read("(a (b c)")

    def test_eof_in_string(self):
        with pytest.raises(EOFError):
            read('"unclosed string')

    def test_eof_in_vector(self):
        with pytest.raises(EOFError):
            read("#(1 2 3")

    def test_eof_in_block_comment(self):
        with pytest.raises(EOFError):
            read("#|unclosed comment")

    def test_empty_input_is_not_a_partial_object(self):
        """Empty input ends cleanly rather than mid-object.

        `read_1` answers None, which is what lets `read_all` and the LOAD/
        COMPILE-FILE form loops stop; it is not the ANSI `READ` boundary, where
        `(read s)` on empty input signals END-OF-FILE and `(read s nil :eof)`
        answers :eof. That distinction is tested at the Lisp level.
        """
        assert read("") is None

    def test_whitespace_only_input_is_not_a_partial_object(self):
        assert read("   \n  \t  ") is None


class TestMalformedSyntaxErrors:
    """Malformed input signals a reader error."""

    def test_extra_closing_paren(self):
        with pytest.raises(ReaderErrorSignal):
            read(")")

    def test_closing_paren_before_any_object(self):
        with pytest.raises(ReaderErrorSignal):
            read(") (a b c")

    def test_reads_one_object_and_leaves_the_rest(self):
        """`read` consumes one object; a later stray `)` is not its problem."""
        assert str(read("(a b c)")) == "(A B C)"

    def test_stops_at_the_first_complete_object(self):
        assert str(read("(a ) b)")) == "(A)"


class TestDottedListErrors:
    """Errors in dotted-list syntax."""

    def test_dot_at_start(self):
        with pytest.raises(ReaderErrorSignal):
            read("(. a)")

    def test_dot_missing_tail(self):
        with pytest.raises(ReaderErrorSignal):
            read("(a b .)")

    def test_dot_outside_list(self):
        with pytest.raises(ReaderErrorSignal):
            read(".")

    def test_two_dots_in_one_list(self):
        with pytest.raises(ReaderErrorSignal):
            read("(a . b . c)")

    def test_valid_dotted_list(self):
        assert str(read("(a b c . d)")) == "(A B C . D)"


class TestTokenAccumulation:
    """A token ends only at whitespace or a terminating macro character.

    CLHS 2.3.1: token accumulation does not stop at a digit/letter boundary, so
    "123abc" and "1.2.3" are each ONE token and a reader must never split one
    into a number plus a remainder.
    """

    def test_letters_after_digits_read_as_one_symbol(self):
        """"123abc" is not a potential number, so it is a symbol.

        CLHS 2.3.1.1: a letter may act as a number marker, but "no letter that
        is adjacent to another letter may ever be treated as a number marker" --
        a, b and c are mutually adjacent, so the token contains letters that are
        not number markers and is not a potential number. |123ABC| is required.
        """
        result = read("123abc")
        assert isinstance(result, LispSymbol)
        assert result.name == "123ABC"

    def test_multiple_decimal_points_read_as_one_token(self):
        """"1.2.3" must not be split into the float 1.2 plus ".3".

        Unlike "123abc" this token *is* a potential number (digits and decimal
        points only -- CLHS 2.3.1.1 lists `3.1.2.6` as an example), so it is a
        *reserved token* whose interpretation is implementation-dependent and
        for which CLHS explicitly permits signalling a reader-error. A symbol is
        this implementation's choice; what is not permitted, and what is
        asserted here, is consuming only part of the token.
        """
        result = read("1.2.3")
        assert isinstance(result, LispSymbol)
        assert result.name == "1.2.3"

    def test_potential_numbers_that_are_not_numbers(self):
        """CLHS 2.3.1.1's own examples of potential numbers that are not
        numbers: each is one token, never split."""
        for text, name in (("1b5000", "1B5000"),
                           ("12/25/83", "12/25/83"),
                           ("3.1.2.6", "3.1.2.6")):
            result = read(text)
            assert isinstance(result, LispSymbol), text
            assert result.name == name


class TestErrorMessages:
    """A failure carries a message."""

    def test_eof_error_has_message(self):
        with pytest.raises(EOFError) as info:
            read("(a b c")
        assert str(info.value)

    def test_reader_error_has_message(self):
        with pytest.raises(ReaderErrorSignal) as info:
            read("(. a)")
        assert str(info.value)


class TestRecoveryScenarios:
    """The reader is usable after a failure."""

    def test_read_after_error(self):
        with pytest.raises(EOFError):
            read("(a b c")
        assert read("42") == 42

    def test_read_all_propagates_a_partial_final_object(self):
        with pytest.raises(EOFError):
            read_all("1 2 (a b c")

    def test_read_all_reads_every_object(self):
        assert [str(x) for x in read_all("1 2 3")] == ["1", "2", "3"]


class TestComplexErrorScenarios:
    """Failures inside nested and dispatched syntax."""

    def test_nested_list_with_unclosed_inner(self):
        with pytest.raises(EOFError):
            read("(a (b c d) (e f g")

    def test_quoted_form_with_eof(self):
        with pytest.raises(EOFError):
            read("'(a b c")

    def test_function_quote_with_eof(self):
        with pytest.raises(EOFError):
            read("#'(lambda (x)")

    def test_deeply_nested_with_unclosed(self):
        with pytest.raises(EOFError):
            read("(a (b (c (d (e f")

    def test_comment_in_unclosed_list(self):
        with pytest.raises(EOFError):
            read("(a b ; comment\nc d")

    def test_string_in_unclosed_list(self):
        with pytest.raises(EOFError):
            read('(a "string" b')


class TestValidInputNotErroring:
    """Valid input reads to the expected object."""

    def test_complex_valid_expression(self):
        assert str(read("(defun foo (x y) (+ x y))")) == "(DEFUN FOO (X Y) (+ X Y))"

    def test_deeply_nested_valid(self):
        assert str(read("(a (b (c (d (e f)))))")) == "(A (B (C (D (E F)))))"

    def test_mixed_valid(self):
        result = read('(1 "string" :keyword foo #\\A #(1 2 3))')
        assert result is not None
        assert str(result).startswith("(1 string :KEYWORD FOO ")

    def test_quote_reader_macro(self):
        assert str(read("'foo")) == "(QUOTE FOO)"

    def test_backquote_reader_macro(self):
        assert str(read("`(a ,b)")) == "(QUASIQUOTE (A (UNQUOTE B)))"

    def test_function_reader_macro(self):
        assert str(read("#'foo")) == "(FUNCTION FOO)"
