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


def _cons_list(*items):
    """Build a real Lisp list (chain of lispCons) from Python values.

    The distinction matters: a Lisp list arriving at FORMAT is a lispCons,
    never a Python list, so tests that pass a Python list exercise a path
    the interpreter never actually takes.
    """
    from fclpy.lisptype import lispCons, NIL as _NIL
    result = _NIL
    for item in reversed(items):
        result = lispCons(item, result)
    return result


class TestFormatIterationOverConsLists:
    """~{...~} and ~? take a *list* argument and iterate over its elements.

    These previously tested `isinstance(arg, (list, tuple))`, which is false
    for every Lisp list, so the whole list was formatted as one opaque
    element: (format nil "~{~A ~}" '(1 2 3)) returned "(1 2 3) ".
    """

    def test_iteration_over_cons_list(self):
        assert format_fn(None, "~{~A ~}", _cons_list(1, 2, 3)) == "1 2 3 "

    def test_iteration_consumes_multiple_args_per_pass(self):
        assert format_fn(None, "~{~A=~A ~}", _cons_list(1, 2, 3, 4)) == "1=2 3=4 "

    def test_iteration_over_empty_cons_list(self):
        assert format_fn(None, "~{~A ~}", NIL) == ""

    def test_colon_iteration_over_sublists(self):
        arg = _cons_list(_cons_list(1, 2), _cons_list(3, 4))
        assert format_fn(None, "~:{~A-~A ~}", arg) == "1-2 3-4 "

    def test_iteration_count_parameter_bounds_passes(self):
        assert format_fn(None, "~2{~A ~}", _cons_list(1, 2, 3, 4)) == "1 2 "

    def test_recursive_directive_takes_cons_arg_list(self):
        assert format_fn(None, "~?", "~A+~A", _cons_list(1, 2)) == "1+2"


class TestFormatEscapeDirective:
    """~^ terminates the enclosing iteration only when its condition holds
    (CLHS 22.3.9.2). It previously escaped unconditionally, so every ~{~^~}
    loop stopped after its first pass."""

    def test_escape_separates_rather_than_truncating(self):
        assert format_fn(None, "~{~A~^,~}", _cons_list(1, 2, 3)) == "1,2,3"

    def test_escape_on_single_element_emits_no_separator(self):
        assert format_fn(None, "~{~A~^,~}", _cons_list(1)) == "1"

    def test_escape_on_empty_list_produces_nothing(self):
        assert format_fn(None, "~{~A~^,~}", NIL) == ""

    def test_escape_with_at_iteration_over_remaining_args(self):
        assert format_fn(None, "~@{~A~^,~}", 1, 2, 3) == "1,2,3"

    def test_zero_parameter_forces_escape(self):
        assert format_fn(None, "~{~A~0^X~}", _cons_list(1, 2)) == "1"

    def test_equal_parameters_force_escape(self):
        assert format_fn(None, "~{~A~1,1^X~}", _cons_list(1, 2)) == "1"

    def test_unequal_parameters_do_not_escape(self):
        assert format_fn(None, "~{~A~1,2^X~}", _cons_list(1)) == "1X"

    def test_escape_outside_iteration_ends_control_string(self):
        assert format_fn(None, "a~^b") == "a"

    def test_escape_does_not_leak_a_nul_into_output(self):
        """The old implementation signalled escape with an in-band '\u0000'
        marker; nothing may leave it in the result."""
        assert "\u0000" not in format_fn(None, "~{~A~^,~}", _cons_list(1, 2))


class TestFormatColumnPadding:
    """~mincol,colinc,minpad,padchar for ~A/~S (CLHS 22.3.4.1). Only mincol
    with a hardcoded space was honoured; minpad and padchar were ignored."""

    def test_mincol_pads_on_the_right(self):
        assert format_fn(None, "~10A|", "abc") == "abc       |"

    def test_at_modifier_pads_on_the_left(self):
        assert format_fn(None, "~10@A|", "abc") == "       abc|"

    def test_minpad_applies_even_below_mincol(self):
        assert format_fn(None, "~,,2A|", "abc") == "abc  |"

    def test_padchar_parameter_is_used(self):
        assert format_fn(None, "~4,,,'xA|", "ab") == "abxx|"

    def test_colinc_rounds_padding_up_in_steps(self):
        """minpad 0, then colinc-sized steps until width >= mincol: one
        step of 3 takes "ab" to exactly 5 columns."""
        assert format_fn(None, "~5,3,0A|", "ab") == "ab   |"

    def test_colinc_overshoots_mincol_when_a_step_is_needed(self):
        """A step is taken as a whole, so the result may exceed mincol."""
        assert format_fn(None, "~4,3,0A|", "ab") == "ab   |"

    def test_padding_applies_to_s_directive_too(self):
        assert format_fn(None, "~6S|", 12) == "12    |"


class TestFormatJustification:
    """~<...~> outputs every segment with padding distributed between them
    (CLHS 22.3.6.2). It previously processed only the last segment."""

    def test_all_segments_are_output(self):
        assert format_fn(None, "~<~A~;~A~>", 1, 2) == "12"

    def test_single_segment_is_right_justified(self):
        assert format_fn(None, "~10<abc~>|") == "       abc|"

    def test_padding_goes_between_segments(self):
        assert format_fn(None, "~10<a~;b~>|") == "a        b|"

    def test_at_modifier_pads_after_last_segment(self):
        assert format_fn(None, "~10@<abc~>|") == "abc       |"

    def test_colon_modifier_pads_before_first_segment(self):
        assert format_fn(None, "~10:<abc~>|") == "       abc|"

    def test_plain_escape_inside_colon_iteration_ends_only_that_pass(self):
        """CLHS 22.3.9.2: in ~:{...~} a plain ~^ ends the current sublist's
        pass; later sublists must still be processed."""
        arg = _cons_list(_cons_list(1), _cons_list(2, 3))
        assert format_fn(None, "~:{~A~^-~A~}", arg) == "12-3"

    def test_colon_escape_inside_colon_iteration_ends_everything(self):
        arg = _cons_list(_cons_list(1), _cons_list(2, 3))
        assert format_fn(None, "~:{~A~:^-~A~}", arg) == "1"

    def test_colon_separator_marks_a_line_prefix_that_is_omitted(self):
        """CLHS 22.3.6.2: a first segment ended by ~:; is a line prefix,
        emitted only when the block is broken across lines. Single-line
        output must not include it."""
        assert format_fn(None, "~<pfx~:;body~>") == "body"

    def test_plain_separator_keeps_both_segments(self):
        assert format_fn(None, "~<a~;b~>") == "ab"


class TestFormatIterationControlStringFromArgument:
    """CLHS 22.3.7.4: an empty ~{~} body takes its control string from an
    argument, consumed before the list argument."""

    def test_empty_body_uses_argument_as_control_string(self):
        assert format_fn(None, "~{~}", "~A-", _cons_list(1, 2, 3)) == "1-2-3-"

    def test_empty_body_with_colon_iterates_sublists(self):
        arg = _cons_list(_cons_list(1, 2), _cons_list(3, 4))
        assert format_fn(None, "~:{~}", "~A~A.", arg) == "12.34."


class TestFormatConditionalPrefixParameter:
    """CLHS 22.3.7.2: ~n[ takes its clause index from the prefix parameter
    and consumes no argument. ~#[ is that with n = the number of arguments
    remaining, which is how a control string says "none/one/many"."""

    def test_literal_parameter_selects_clause_without_consuming(self):
        assert format_fn(None, "~1[a~;b~;c~]") == "b"

    def test_hash_parameter_counts_remaining_arguments(self):
        assert format_fn(None, "~#[none~;one~:;many~]") == "none"
        assert format_fn(None, "~#[none~;one~:;many~]", 1) == "one"
        assert format_fn(None, "~#[none~;one~:;many~]", 1, 2, 3) == "many"

    def test_parameter_form_does_not_steal_a_following_argument(self):
        assert format_fn(None, "~#[none~;one~:;many~] ~A", 7) == "one 7"

    def test_hash_inside_iteration_sees_remaining_items(self):
        assert format_fn(None, "~{~A~#[~;and ~]~}", _cons_list(1, 2)) == "1and 2"

    def test_out_of_range_index_falls_to_default_clause(self):
        assert format_fn(None, "~[a~;b~:;other~]", 5) == "other"

    def test_out_of_range_index_without_default_produces_nothing(self):
        assert format_fn(None, "~[a~;b~;c~]", 5) == ""
