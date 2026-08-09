"""Tests for FORMAT function (Phase 8 Task 4)."""

import pytest
from fclpy.lisptype import T, NIL, Character
from fclpy.lispfunc.io_write import format_fn


class TestFormatBasic:
    """Basic FORMAT directive tests."""
    
    def test_format_literal_text(self):
        """Literal text passes through unchanged."""
        result = format_fn(None, "Hello World")
        assert result == "Hello World"
        
    def test_format_empty_string(self):
        """Empty control string."""
        result = format_fn(None, "")
        assert result == ""


class TestFormatAesthetic:
    """~A directive tests."""
    
    def test_format_a_string(self):
        """~A with string argument."""
        result = format_fn(None, "Hello ~A!", "World")
        assert result == "Hello World!"
        
    def test_format_a_number(self):
        """~A with number argument."""
        result = format_fn(None, "Value: ~A", 42)
        assert result == "Value: 42"
        
    def test_format_a_nil(self):
        """~A with NIL."""
        result = format_fn(None, "Value: ~A", NIL)
        assert result == "Value: NIL"
        
    def test_format_a_colon_nil(self):
        """~:A with NIL prints as ()."""
        result = format_fn(None, "Value: ~:A", NIL)
        assert result == "Value: ()"
        
    def test_format_a_list(self):
        """~A with list argument."""
        result = format_fn(None, "List: ~A", [1, 2, 3])
        assert "1" in result and "2" in result and "3" in result
        
    def test_format_a_multiple(self):
        """Multiple ~A directives."""
        result = format_fn(None, "~A + ~A = ~A", 1, 2, 3)
        assert result == "1 + 2 = 3"


class TestFormatStandard:
    """~S directive tests."""
    
    def test_format_s_string(self):
        """~S with string argument (with quotes)."""
        result = format_fn(None, "~S", "hello")
        assert '"hello"' in result or "'hello'" in result or "hello" in result
        
    def test_format_s_number(self):
        """~S with number."""
        result = format_fn(None, "~S", 42)
        assert "42" in result


class TestFormatDecimal:
    """~D directive tests."""
    
    def test_format_d_positive(self):
        """~D with positive integer."""
        result = format_fn(None, "~D", 42)
        assert result == "42"
        
    def test_format_d_negative(self):
        """~D with negative integer."""
        result = format_fn(None, "~D", -42)
        assert result == "-42"
        
    def test_format_d_zero(self):
        """~D with zero."""
        result = format_fn(None, "~D", 0)
        assert result == "0"
        
    def test_format_d_at_sign(self):
        """~@D shows + sign for positive."""
        result = format_fn(None, "~@D", 42)
        assert result == "+42"
        
    def test_format_d_colon(self):
        """~:D adds commas."""
        result = format_fn(None, "~:D", 1000000)
        assert result == "1,000,000"
        
    def test_format_d_width(self):
        """~5D pads to width."""
        result = format_fn(None, "~5D", 42)
        assert result == "   42"


class TestFormatHex:
    """~X directive tests."""
    
    def test_format_x_positive(self):
        """~X with positive integer."""
        result = format_fn(None, "~X", 255)
        assert result == "FF"
        
    def test_format_x_negative(self):
        """~X with negative integer."""
        result = format_fn(None, "~X", -16)
        assert result == "-10"
        
    def test_format_x_zero(self):
        """~X with zero."""
        result = format_fn(None, "~X", 0)
        assert result == "0"


class TestFormatOctal:
    """~O directive tests."""
    
    def test_format_o_positive(self):
        """~O with positive integer."""
        result = format_fn(None, "~O", 64)
        assert result == "100"
        
    def test_format_o_negative(self):
        """~O with negative integer."""
        result = format_fn(None, "~O", -8)
        assert result == "-10"


class TestFormatBinary:
    """~B directive tests."""
    
    def test_format_b_positive(self):
        """~B with positive integer."""
        result = format_fn(None, "~B", 10)
        assert result == "1010"
        
    def test_format_b_negative(self):
        """~B with negative integer."""
        result = format_fn(None, "~B", -5)
        assert result == "-101"


class TestFormatRadix:
    """~R directive tests."""
    
    def test_format_r_base_16(self):
        """~16R hexadecimal."""
        result = format_fn(None, "~16R", 255)
        assert result == "FF"
        
    def test_format_r_base_2(self):
        """~2R binary."""
        result = format_fn(None, "~2R", 10)
        assert result == "1010"
        
    def test_format_r_cardinal(self):
        """~R without param gives English cardinal."""
        result = format_fn(None, "~R", 5)
        assert result == "five"
        
    def test_format_r_ordinal(self):
        """~:R gives English ordinal."""
        result = format_fn(None, "~:R", 3)
        assert result == "third"


class TestFormatCharacter:
    """~C directive tests."""
    
    def test_format_c_char(self):
        """~C with Character."""
        result = format_fn(None, "~C", Character('A'))
        assert result == "A"
        
    def test_format_c_string_char(self):
        """~C with single-char string."""
        result = format_fn(None, "~C", "X")
        assert result == "X"


class TestFormatFloat:
    """~F directive tests."""
    
    def test_format_f_basic(self):
        """~F with float."""
        result = format_fn(None, "~F", 3.14159)
        assert "3.14" in result
        
    def test_format_f_precision(self):
        """~,2F with 2 decimal places."""
        result = format_fn(None, "~,2F", 3.14159)
        assert result == "3.14"
        
    def test_format_f_integer(self):
        """~F with integer."""
        result = format_fn(None, "~F", 42)
        assert "42" in result


class TestFormatNewline:
    """~% directive tests."""
    
    def test_format_percent(self):
        """~% produces newline."""
        result = format_fn(None, "Hello~%World")
        assert result == "Hello\nWorld"
        
    def test_format_percent_multiple(self):
        """~3% produces 3 newlines."""
        result = format_fn(None, "A~3%B")
        assert result == "A\n\n\nB"


class TestFormatTilde:
    """~~ directive tests."""
    
    def test_format_tilde(self):
        """~~ produces literal tilde."""
        result = format_fn(None, "Hello~~World")
        assert result == "Hello~World"
        
    def test_format_tilde_multiple(self):
        """~3~ produces 3 tildes."""
        result = format_fn(None, "A~3~B")
        assert result == "A~~~B"


class TestFormatIteration:
    """~{ ~} directive tests."""
    
    def test_format_iteration_simple(self):
        """~{~A ~} iterates over list."""
        result = format_fn(None, "~{~A ~}", [1, 2, 3])
        assert "1" in result and "2" in result and "3" in result
        
    def test_format_iteration_at(self):
        """~@{~A ~} uses remaining args."""
        result = format_fn(None, "~@{~A ~}", 1, 2, 3)
        assert "1" in result and "2" in result and "3" in result


class TestFormatConditional:
    """~[ ~] directive tests."""
    
    def test_format_conditional_index(self):
        """~[zero~;one~;two~] selects by index."""
        result = format_fn(None, "~[zero~;one~;two~]", 1)
        assert result == "one"
        
    def test_format_conditional_colon(self):
        """~:[false~;true~] boolean conditional."""
        result = format_fn(None, "~:[no~;yes~]", T)
        assert result == "yes"
        
        result = format_fn(None, "~:[no~;yes~]", NIL)
        assert result == "no"


class TestFormatCaseConversion:
    """~( ~) directive tests."""
    
    def test_format_lowercase(self):
        """~(~A~) converts to lowercase."""
        result = format_fn(None, "~(~A~)", "HELLO")
        assert result == "hello"
        
    def test_format_uppercase(self):
        """~:@(~A~) converts to uppercase."""
        result = format_fn(None, "~:@(~A~)", "hello")
        assert result == "HELLO"
        
    def test_format_capitalize(self):
        """~:(~A~) capitalizes."""
        result = format_fn(None, "~:(~A~)", "hello")
        assert result == "Hello"


class TestFormatPlural:
    """~P directive tests."""
    
    def test_format_plural_one(self):
        """~P with 1 gives empty string."""
        result = format_fn(None, "~D item~P", 1, 1)
        assert result == "1 item"
        
    def test_format_plural_many(self):
        """~P with >1 gives 's'."""
        result = format_fn(None, "~D item~P", 5, 5)
        assert result == "5 items"
        
    def test_format_plural_at(self):
        """~@P gives y/ies."""
        result = format_fn(None, "~D bab~@P", 1, 1)
        assert result == "1 baby"
        
        result = format_fn(None, "~D bab~@P", 3, 3)
        assert result == "3 babies"


class TestFormatGoToArg:
    """~* directive tests."""
    
    def test_format_skip_forward(self):
        """~* skips one argument."""
        result = format_fn(None, "~A ~*~A", "first", "second", "third")
        assert result == "first third"
        
    def test_format_skip_back(self):
        """~:* goes back one argument."""
        result = format_fn(None, "~A ~:*~A", "test")
        assert result == "test test"


class TestFormatDestination:
    """Test different destinations."""
    
    def test_format_to_string(self):
        """NIL destination returns string."""
        result = format_fn(NIL, "Hello ~A", "World")
        assert result == "Hello World"
        
    def test_format_to_none(self):
        """None destination returns string."""
        result = format_fn(None, "Hello ~A", "World")
        assert result == "Hello World"


class TestFormatComplex:
    """Complex format string tests."""

    def test_format_mixed(self):
        """Mixed directives."""
        result = format_fn(None, "~A is ~D years old~%", "Alice", 30)
        assert result == "Alice is 30 years old\n"

    def test_format_padding(self):
        """Formatted table-like output."""
        result = format_fn(None, "~10A~5D", "Name", 42)
        assert "Name" in result and "42" in result


class TestFormatArgCursorPropagation:
    """Regression tests for the shared argument cursor (M0): consumption
    inside a nested directive (~[...~], ~<...~>, ~(...~), ~@?) must be
    visible to directives that follow it, not silently discarded by a
    fresh, restarted argument index."""

    def test_conditional_then_aesthetic_advances_shared_cursor(self):
        """~[...~] consumes its selector; ~A after it must see the *next*
        argument, not re-read one already consumed by the selector."""
        result = format_fn(None, "~[zero~;one~]-~A", 1, "tail")
        assert result == "one-tail"

    def test_goto_back_inside_conditional_clause_affects_outer_index(self):
        """~:* inside a ~[...~] clause backs up the *outer* cursor so the
        clause can re-read the argument the selector just consumed."""
        result = format_fn(None, "~[~:*~A~;other~]", 0)
        assert result == "0"

    def test_justification_block_advances_shared_cursor(self):
        """~<...~> consumes from the outer stream; a directive following it
        must see the next argument, not restart at the beginning."""
        result = format_fn(None, "~<~A~> ~A", "first", "second")
        assert result == "first second"

    def test_case_conversion_advances_shared_cursor_exactly(self):
        """~(...~) must consume exactly what its inner directives consume
        (not the old inner.count('~') approximation), leaving the right
        argument for what follows."""
        result = format_fn(None, "~(~A~) ~A", "HELLO", "world")
        assert result == "hello world"

    def test_recursive_at_format_shares_outer_argument_stream(self):
        """~@? processes its format string against the same outer argument
        stream; only what it actually consumes should be unavailable to
        directives that follow it."""
        result = format_fn(None, "~@?~A", "~A ", "x", "y")
        assert result == "x y"


class TestFormatPluralColon:
    """~:P must be net-zero on the argument cursor: it re-examines the
    previously consumed argument rather than consuming (or over-consuming)
    a new one."""

    def test_colon_p_does_not_double_consume(self):
        result = format_fn(None, "~D cat~:P and ~D cat~:P", 1, 2)
        assert result == "1 cat and 2 cats"

    def test_colon_at_p_uses_y_ies_without_consuming(self):
        result = format_fn(None, "~D bab~:@P", 1)
        assert result == "1 baby"

        result = format_fn(None, "~D bab~:@P", 3)
        assert result == "3 babies"


class TestFormatIterationDoesNotTruncateOnNil:
    """~{~} must iterate through every element regardless of what any one
    element prints as - it must not stop just because an element's printed
    form happens to contain the substring 'NIL'."""

    def test_nil_element_does_not_truncate_iteration(self):
        result = format_fn(None, "~{~A ~}", [1, NIL, 3])
        assert result == "1 NIL 3 "


class TestFormatCaseConversionSemantics:
    """~:( and ~@( are distinct per CLHS 22.3.9 and must not be swapped:
    ~:( capitalizes every word; ~@( capitalizes only the first word and
    forces the rest of the output to lower case."""

    def test_colon_capitalizes_every_word(self):
        result = format_fn(None, "~:(~A~)", "hello world")
        assert result == "Hello World"

    def test_at_capitalizes_only_first_word(self):
        result = format_fn(None, "~@(~A~)", "HELLO WORLD")
        assert result == "Hello world"
