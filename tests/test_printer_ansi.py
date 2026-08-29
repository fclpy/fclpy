"""The printer reads its control variables from the dynamic environment.

These tests exist because the printer control variables used to be Python
globals on an `io_write.PrinterSettings` object, reachable only through
`@cl_function('*PRINT-BASE*')` accessors. No Lisp binding form can assign a
Python global, so every one of these assertions failed -- and, worse, nothing
detected that, because the whole printer was `str()`/`repr()` and the ANSI
printer tests could not observe it either (see `TestDefaultStream`).

So each test here is written through the *Lisp* surface -- `(let ((*print-...*
...)) ...)` -- rather than by calling the printer with a keyword argument.
Passing an override directly would still pass if the dynamic-variable lookup
regressed, which is exactly the failure this file is meant to catch.
"""

import io

import pytest

from fclpy import lispenv, lisptype
from fclpy.lispfunc.evaluation_core import eval as lisp_eval
from fclpy.lispreader import LispReader, LispStream
from fclpy.readtable import get_current_readtable
from fclpy.printer import prin1_to_string


@pytest.fixture(autouse=True)
def env():
    """A freshly bootstrapped standard environment."""
    lispenv.setup_standard_environment()
    import fclpy.state as state
    return state.current_environment


def ev(source):
    """Read and evaluate `source`, returning the value as a Python object."""
    import fclpy.state as state
    stream = LispStream(io.StringIO(source))
    readtable = get_current_readtable()
    form = LispReader(readtable.get_macro_character, stream).read_1()
    return lisp_eval(form, state.current_environment)


def evs(source):
    """Read and evaluate `source`, returning the value as a Python string."""
    return str(ev(source))


class TestControlVariablesAreVariables:
    """`*PRINT-...*` must be variables with values, not function registrations."""

    @pytest.mark.parametrize('name,expected', [
        ('*print-base*', '10'),
        ('*print-radix*', 'NIL'),
        ('*print-case*', ':UPCASE'),
        ('*print-circle*', 'NIL'),
        ('*print-escape*', 'T'),
        ('*print-gensym*', 'T'),
        ('*print-level*', 'NIL'),
        ('*print-length*', 'NIL'),
        ('*print-lines*', 'NIL'),
        ('*print-readably*', 'NIL'),
        ('*print-right-margin*', 'NIL'),
        ('*print-array*', 'T'),
        ('*print-pretty*', 'NIL'),
    ])
    def test_initial_value(self, name, expected):
        """The ANSI initial values, as `printer/printer-control-vars.lsp` asserts.

        `*PRINT-RIGHT-MARGIN*` and `*PRINT-MISER-WIDTH*` are NIL, not 80 and 40
        -- the old settings object used numbers, which that file catches.
        """
        assert evs(f'(prin1-to-string {name})') == expected

    def test_reference_is_not_a_python_function(self):
        """Referencing one must not yield a Python object (standing rule 2).

        Registering a *function* named `*PRINT-BASE*` made the evaluator's
        function-registry fallback produce `<function get_print_base ...>` as
        the value of the variable.
        """
        for name in ('*print-base*', '*print-escape*', '*print-case*'):
            assert not callable(ev(name)), f'{name} evaluated to a callable'


class TestPrintBaseAndRadix:
    """`*PRINT-BASE*`/`*PRINT-RADIX*` -- CLHS 22.1.3.1.1."""

    @pytest.mark.parametrize('base,value,expected', [
        (2, 5, '101'), (2, 255, '11111111'), (2, -2, '-10'),
        (3, 80, '2222'), (3, -78, '-2220'),
        (8, 64, '100'),
        (16, 12, 'C'), (16, -14, '-E'), (16, -208, '-D0'),
        (36, 35, 'Z'),
    ])
    def test_base(self, base, value, expected):
        """Digits above 9 are upper case, and the sign leads."""
        assert evs(f'(let ((*print-base* {base})) (prin1-to-string {value}))') == expected

    def test_base_applies_to_bignums(self):
        """Arbitrary precision, so `format(n, "x")` shortcuts are not enough."""
        assert evs('(let ((*print-base* 16)) (prin1-to-string (expt 16 40)))') == \
            '1' + '0' * 40

    @pytest.mark.parametrize('value,expected', [
        (0, '0.'), (1, '1.'), (123456, '123456.'), (-5, '-5.'),
    ])
    def test_radix_in_base_ten_uses_a_trailing_point(self, value, expected):
        assert evs(f'(let ((*print-radix* t)) (prin1-to-string {value}))') == expected

    @pytest.mark.parametrize('base,value,expected', [
        (2, 3, '#b11'), (2, -1, '#b-1'), (2, 256, '#b100000000'),
        (8, 64, '#o100'),
        (16, 255, '#xFF'),
        (3, -4, '#3r-11'), (3, 6561, '#3r100000000'),
    ])
    def test_radix_prefix_precedes_the_sign(self, base, value, expected):
        """`#b-1`, not `-#b1` -- the marker describes the whole numeral."""
        assert evs(
            f'(let ((*print-radix* t) (*print-base* {base})) '
            f'(prin1-to-string {value}))') == expected

    def test_base_does_not_apply_to_floats(self):
        """`*PRINT-BASE*` governs rationals only (CLHS 22.1.3.1.1)."""
        assert evs('(let ((*print-base* 2)) (prin1-to-string 1.5))') == '1.5'


class TestPrintEscape:
    """PRINC is PRIN1 with `*PRINT-ESCAPE*` NIL -- CLHS 22.1.3.2."""

    @pytest.mark.parametrize('form,prin1_out,princ_out', [
        ('"ab"', '"ab"', 'ab'),
        (r'#\a', r'#\a', 'a'),
        # Space prints bare under PRIN1 too (CLHS 22.1.3.2, ansi-test
        # printer/print-characters.lsp PRINT.CHAR.3/.4) -- it is the one
        # standard character name excluded from the "must use the named
        # form" rule.
        (r'#\Space', '#\\ ', ' '),
        (':foo', ':FOO', 'FOO'),
    ])
    def test_escape_distinguishes_prin1_from_princ(self, form, prin1_out, princ_out):
        assert evs(f'(prin1-to-string {form})') == prin1_out
        assert evs(f'(princ-to-string {form})') == princ_out

    def test_a_name_needing_bars_is_escaped_only_under_prin1(self):
        """`|a b|` under PRIN1, `a b` under PRINC (CLHS 22.1.3.3).

        Built with MAKE-SYMBOL rather than read from `'|a b|` source: the
        reader mishandles multiple-escape syntax (it reads `|a b|` as the symbol
        `B|`), which is a reader defect -- plan.md C12 -- and would make this a
        test of the reader instead of the printer.
        """
        assert evs('(prin1-to-string (make-symbol "a b"))') == '#:|a b|'
        assert evs('(princ-to-string (make-symbol "a b"))') == 'a b'

    def test_escape_reaches_nested_components(self):
        """A string inside a list is escaped when the list is."""
        assert evs("""(prin1-to-string '("a" #\\b))""") == r'("a" #\b)'
        assert evs("""(princ-to-string '("a" #\\b))""") == '(a b)'

    def test_only_quote_and_backslash_are_escaped_in_a_string(self):
        r"""CLHS 2.4.5 -- backslash is included without interpretation.

        A newline prints as a newline; emitting `\n` would read back as two
        characters.
        """
        assert evs(r'(prin1-to-string "a\"b")') == r'"a\"b"'
        # A string built from an actual newline character prints containing an
        # actual newline, between the two quotes.
        newline_string = r'(prin1-to-string (coerce (list #\a #\Newline #\b)' \
                         r" 'string))"
        assert evs(newline_string) == '"a\nb"'


class TestPrintCase:
    """`*PRINT-CASE*` -- CLHS 22.1.3.3.2."""

    @pytest.mark.parametrize('case,expected', [
        (':upcase', 'FOO-BAR'),
        (':downcase', 'foo-bar'),
        (':capitalize', 'Foo-Bar'),
    ])
    def test_case_of_a_symbol(self, case, expected):
        """`:capitalize` capitalizes each word, where `-` separates words."""
        assert evs(
            f"(let ((*print-case* {case})) (prin1-to-string 'foo-bar))") == expected

    def test_case_applies_to_princ_too(self):
        assert evs("(let ((*print-case* :downcase)) (princ-to-string 'foo))") == 'foo'

    def test_case_does_not_apply_to_strings(self):
        """Only symbol names are recased, not string contents."""
        assert evs('(let ((*print-case* :downcase)) (princ-to-string "ABC"))') == 'ABC'


class TestReadtableCaseInteraction:
    """`*PRINT-CASE*` is filtered by `READTABLE-CASE` -- CLHS 22.1.3.3.2.

    `*PRINT-CASE*` does not simply recase a symbol name; which characters it
    governs depends on the readtable. This is the matrix
    `printer/print-symbols.lsp`'s PRINT.SYMBOL.1 and .2 check.

    Symbols are built with `LispSymbol(...)` rather than `INTERN`, because
    `INTERN` case-folds its string argument -- case conversion is the *reader's*
    job via `readtable-case` (CLHS 23.1.2) -- so `(intern "xyz")` yields a
    symbol named `XYZ` and every lower-case row would be testing that bug
    instead of this one. See plan.md C10.
    """

    MATRIX = [
        # readtable :upcase -- upper-case chars follow *print-case*
        ('XYZ', 'UPCASE', 'UPCASE', 'XYZ'),
        ('XYZ', 'UPCASE', 'DOWNCASE', 'xyz'),
        ('XYZ', 'UPCASE', 'CAPITALIZE', 'Xyz'),
        # ... and lower-case chars are left alone
        ('xyz', 'UPCASE', 'UPCASE', 'xyz'),
        ('xyz', 'UPCASE', 'CAPITALIZE', 'xyz'),
        # readtable :downcase -- the mirror image
        ('XYZ', 'DOWNCASE', 'UPCASE', 'XYZ'),
        ('XYZ', 'DOWNCASE', 'DOWNCASE', 'XYZ'),
        ('XYZ', 'DOWNCASE', 'CAPITALIZE', 'XYZ'),
        ('xyz', 'DOWNCASE', 'UPCASE', 'XYZ'),
        ('xyz', 'DOWNCASE', 'DOWNCASE', 'xyz'),
        ('xyz', 'DOWNCASE', 'CAPITALIZE', 'Xyz'),
        # readtable :preserve -- *print-case* is ignored entirely
        ('XYZ', 'PRESERVE', 'DOWNCASE', 'XYZ'),
        ('xyz', 'PRESERVE', 'UPCASE', 'xyz'),
        # readtable :invert -- uniform case is inverted, mixed case is not
        ('XYZ', 'INVERT', 'UPCASE', 'xyz'),
        ('XYZ', 'INVERT', 'DOWNCASE', 'xyz'),
        ('xyz', 'INVERT', 'UPCASE', 'XYZ'),
        ('Xyz', 'INVERT', 'UPCASE', 'Xyz'),
    ]

    @pytest.mark.parametrize('name,readtable_case,print_case,expected', MATRIX)
    def test_matrix(self, name, readtable_case, print_case, expected):
        from fclpy import printer
        from fclpy.readtable import get_current_readtable

        readtable = get_current_readtable()
        previous = readtable.readtable_case()
        try:
            readtable.set_readtable_case(readtable_case)
            symbol = lisptype.LispSymbol(name)
            ctx = printer.PrintContext(escape=False, case=print_case)
            assert printer.write_object(symbol, ctx) == expected
        finally:
            readtable.set_readtable_case(previous)


class TestPrintLevelAndLength:
    """`*PRINT-LEVEL*`/`*PRINT-LENGTH*` -- CLHS 22.1.3.4."""

    @pytest.mark.parametrize('level,expected', [
        (0, '#'),
        (1, '(1 #)'),
        (2, '(1 (2 #))'),
        (3, '(1 (2 (3 #)))'),
    ])
    def test_level_abbreviates_aggregates(self, level, expected):
        """An aggregate at depth >= the level prints as `#`.

        Level 0 abbreviates the outermost object itself, which is why the test
        is `>=` and not `>`.
        """
        assert evs(f"(let ((*print-level* {level})) "
                   f"(prin1-to-string '(1 (2 (3 (4))))))") == expected

    def test_level_never_abbreviates_an_atom(self):
        """Atoms print at any depth -- only aggregates become `#`.

        `printer/print-level.lsp`'s PRINT-LEVEL.5 pins this for strings: a
        string is a vector, but `(write-to-string "abcd" :level 0)` is
        `"\\"abcd\\""`, not `"#"`.
        """
        assert evs('(let ((*print-level* 0)) (prin1-to-string 42))') == '42'
        assert evs('(let ((*print-level* 0)) (prin1-to-string "abcd"))') == '"abcd"'
        assert evs("(let ((*print-level* 0)) (prin1-to-string 'foo))") == 'FOO'

    @pytest.mark.parametrize('length,expected', [
        (0, '(...)'),
        (1, '(1 ...)'),
        (2, '(1 2 ...)'),
        (4, '(1 2 3 4)'),
    ])
    def test_length_elides_the_tail(self, length, expected):
        assert evs(f"(let ((*print-length* {length})) "
                   f"(prin1-to-string '(1 2 3 4)))") == expected

    def test_length_applies_to_vectors(self):
        assert evs('(let ((*print-length* 2)) (prin1-to-string (vector 1 2 3 4)))') \
            == '#(1 2 ...)'


class TestAggregateSyntax:
    """Printed representations that must read back as the same object."""

    def test_a_vector_prints_as_a_vector_not_a_list(self):
        """`#(1 2 3)`, not `(1 2 3)`.

        A vector is a Python `list` in this implementation, so `str()` printed
        it with list syntax and every vector read back as a cons.
        """
        assert evs('(prin1-to-string (vector 1 2 3))') == '#(1 2 3)'
        assert evs('(prin1-to-string (vector))') == '#()'

    def test_a_reader_vector_prints_the_same_as_a_constructed_one(self):
        """`#(...)` yields an `AdjustableVector` and `VECTOR` a `list`.

        Two representations of one type; they must not print differently.
        """
        assert evs("(prin1-to-string '#(1 2 3))") == '#(1 2 3)'

    def test_a_multidimensional_array_prints_with_its_rank(self):
        """`#2A((0 0) (0 0))`. `Array.__repr__` produced `#(ARRAY (2, 2))` --
        a Python tuple's repr inside what claimed to be Lisp syntax."""
        assert evs("(prin1-to-string (make-array '(2 2) :initial-element 0))") \
            == '#2A((0 0) (0 0))'

    def test_dotted_and_proper_lists(self):
        assert evs("(prin1-to-string '(1 . 2))") == '(1 . 2)'
        assert evs("(prin1-to-string '(1 2 . 3))") == '(1 2 . 3)'
        assert evs("(prin1-to-string '(1 2 3))") == '(1 2 3)'
        assert evs("(prin1-to-string '())") == 'NIL'

    def test_a_ratio_prints_as_a_ratio(self):
        """Python's repr of a Fraction is `Fraction(1, 2)`, not readable Lisp."""
        assert evs('(prin1-to-string (/ 1 2))') == '1/2'
        assert evs('(let ((*print-base* 2)) (prin1-to-string (/ 1 2)))') == '1/10'


class TestWriteKeywordArguments:
    """WRITE's keyword arguments override the variables -- CLHS 22.3.1."""

    @pytest.mark.parametrize('call,expected', [
        ('(write-to-string 10 :base 2)', '1010'),
        ('(write-to-string 255 :base 16 :radix t)', '#xFF'),
        ('(write-to-string "ab" :escape nil)', 'ab'),
        ('(write-to-string "ab")', '"ab"'),
        ("(write-to-string '(1 (2 (3))) :level 2)", '(1 (2 #))'),
        ("(write-to-string '(1 2 3 4) :length 2)", '(1 2 ...)'),
        ("(write-to-string 'foo :case :downcase)", 'foo'),
    ])
    def test_keyword_overrides(self, call, expected):
        """These were collected into `**kwargs` and silently dropped."""
        assert evs(call) == expected

    def test_write_to_string_defaults_to_escaped(self):
        """It is WRITE, and `*PRINT-ESCAPE*` starts true -- so PRIN1-like.

        It used to call `lisp_str`, i.e. behave as PRINC.
        """
        assert evs('(write-to-string "ab")') == '"ab"'

    def test_a_keyword_argument_beats_an_enclosing_binding(self):
        assert evs('(let ((*print-base* 8)) (write-to-string 10 :base 2))') == '1010'

    def test_an_unknown_keyword_is_an_error(self):
        """Not silently ignored (standing rule 4)."""
        with pytest.raises(Exception):
            ev('(write-to-string 10 :nonsense t)')

    def test_allow_other_keys_permits_unknown_keywords(self):
        """CLHS 3.4.1.4 -- as `printer/write.lsp`'s WRITE.5/.6 check."""
        assert ev('(with-output-to-string (*standard-output*) '
                  "(write 5 :allow-other-keys t :foo 'bar))") == '5'
        assert evs("(write-to-string 5 :allow-other-keys t :foo 'bar)") == '5'


class TestDefaultStream:
    """Output with no stream argument goes to `*STANDARD-OUTPUT*`.

    This is the gate the whole printer sat behind. Every `def-print-test` in
    `printer/` captures output as
    `(with-output-to-string (*standard-output*) (prin1 form))`, and each output
    function defaulted to Python's `print()` instead -- so all of them saw the
    empty string regardless of what the printer produced, and no printer
    behaviour was measurable at all.
    """

    @pytest.mark.parametrize('body,expected', [
        ('(prin1 5)', '5'),
        ('(prin1 "ab")', '"ab"'),
        ('(princ "ab")', 'ab'),
        ('(write 5)', '5'),
        ('(terpri)', '\n'),
        ('(write-string "ab")', 'ab'),
        (r'(write-char #\a)', 'a'),
        ('(prin1 5) (princ " ") (prin1 6)', '5 6'),
    ])
    def test_captured_via_standard_output(self, body, expected):
        assert ev(f'(with-output-to-string (*standard-output*) {body})') == expected

    def test_an_explicit_stream_still_works(self):
        assert ev('(with-output-to-string (s) (prin1 5 s))') == '5'

    def test_control_variables_apply_to_stream_output(self):
        """The binding and the capture compose -- the `def-print-test` shape."""
        assert ev('(let ((*print-base* 2)) '
                  '(with-output-to-string (*standard-output*) (prin1 5)))') == '101'

    def test_print_is_newline_object_space(self):
        """PRINT is TERPRI then PRIN1 then a space (CLHS 22.3.1).

        The order was reversed: object first, then a newline, and PRINC-style
        unescaped.
        """
        assert ev('(with-output-to-string (*standard-output*) (print "ab"))') \
            == '\n"ab" '


class TestFormatDirectivesUseThePrinter:
    """`~A` is PRINC and `~S` is PRIN1 -- CLHS 22.3.4."""

    @pytest.mark.parametrize('call,expected', [
        ('(format nil "~A" "ab")', 'ab'),
        ('(format nil "~S" "ab")', '"ab"'),
        (r'(format nil "~A" #\a)', 'a'),
        (r'(format nil "~S" #\a)', r'#\a'),
        ('(format nil "~A" :foo)', 'FOO'),
        ('(format nil "~S" :foo)', ':FOO'),
        ('(format nil "~A" (vector 1 2))', '#(1 2)'),
        ('(format nil "~A" nil)', 'NIL'),
        ('(format nil "~:A" nil)', '()'),
        ('(format nil "~S" nil)', 'NIL'),
        ('(format nil "~:S" nil)', '()'),
    ])
    def test_directive_output(self, call, expected):
        assert evs(call) == expected

    @pytest.mark.parametrize('control,expected', [
        ('"~0&"', ''),
        ('"~&"', ''),
        ('"~1&"', ''),
        ('"X~&"', 'X\n'),
        ('"X~1&"', 'X\n'),
        ('"X~%~&"', 'X\n'),
        ('"~5&"', '\n' * 4),
        ('"X~5&"', 'X' + '\n' * 5),
    ])
    def test_tilde_ampersand_is_a_fresh_line(self, control, expected):
        """`~n&` is a fresh line then n-1 newlines (CLHS 22.3.1.3).

        It used to emit n newlines unconditionally, with the comment "we don't
        track column" -- the same defect FRESH-LINE had. These are
        `printer/format/format-ampersand.lsp`'s FORMAT.&.1 through .6.
        """
        assert ev(f'(format nil {control})') == expected

    @pytest.mark.xfail(strict=True, reason=(
        "`~&` knows only the column within its own control string, because "
        "FORMAT builds its whole output as a string before writing it to the "
        "stream. So a `~&` that is the first thing in a control string cannot "
        "see that the stream is already mid-line, and emits nothing where "
        "FRESH-LINE correctly emits a newline. Fixing it means threading the "
        "stream's column into FORMAT's directive engine through all eleven "
        "nested `_format_process_cursor` call sites -- FORMAT's engine, plan.md "
        "C2, not the printer. Recorded in plan.md section 5."))
    def test_fresh_line_and_tilde_ampersand_agree(self):
        """`~&` and FRESH-LINE are one operation and must give one answer."""
        via_format = ev('(with-output-to-string (*standard-output*) '
                        '(princ "a") (format t "~&"))')
        via_function = ev('(with-output-to-string (*standard-output*) '
                          '(princ "a") (fresh-line))')
        assert via_format == via_function == 'a\n'

    def test_format_t_writes_to_standard_output(self):
        """FORMAT's `t` means `*STANDARD-OUTPUT*`, not `*TERMINAL-IO*`.

        CLHS 22.3.1 -- FORMAT's `destination` is not a plain stream designator,
        where `t` would mean `*TERMINAL-IO*` (CLHS 21.1.3). It used to print to
        the process's stdout, so `(format t ...)` escaped any enclosing
        `(with-output-to-string (*standard-output*) ...)`.
        """
        assert ev('(with-output-to-string (*standard-output*) '
                  '(format t "x~A" 5))') == 'x5'

    def test_directives_honour_the_control_variables(self):
        """They print through the same printer, so a binding reaches them."""
        assert evs('(let ((*print-base* 8)) (format nil "~A" 8))') == '10'
        assert evs("(let ((*print-case* :downcase)) (format nil \"~A\" 'foo))") == 'foo'
        assert evs("(let ((*print-length* 1)) (format nil \"~A\" '(1 2 3)))") == '(1 ...)'


class TestCircularStructureTerminates:
    """The printer must never be the thing that aborts a run.

    `printer.MAX_DEPTH` was documented as the cutoff standing in for the absent
    `*PRINT-CIRCLE*` -- "it must not recurse forever either: an infinite
    recursion here aborts a whole ANSI run" -- but it bounded only *recursion*,
    and a cons cycle has two other ways to run away:

    * The **cdr chain is walked**, not recursed, so `depth` stays constant and
      `_write_cons` appended to its parts list until the process ran out of
      memory. `(let ((a (list 17 nil))) (setf (cdr a) a) a)` answered
      `MemoryError` *as the value of the form* (standing rule 2).
    * A cycle through an aggregate's **elements** re-enters the same path, and
      since each level re-walks its own cdr chain the work is *exponential* in
      the depth. `print.cons.random.2` wires twenty conses into a random cons
      graph and held a full run at 10GB -- and because the graph is random, the
      same test had completed on earlier runs.

    Cutting cycles is not by itself a termination proof (simple paths through a
    dense graph are exponentially many), so `PRINT_BUDGET` bounds the work too.
    """

    @staticmethod
    def cons(car, cdr):
        return lisptype.lispCons(car, cdr)

    def test_a_self_referential_cdr_terminates(self):
        cell = self.cons(17, lisptype.NIL)
        cell.cdr = cell
        assert prin1_to_string(cell) == '(17 ...)'

    def test_a_cycle_further_down_the_chain_terminates(self):
        third = self.cons(3, lisptype.NIL)
        second = self.cons(2, third)
        first = self.cons(1, second)
        third.cdr = second
        assert prin1_to_string(first) == '(1 2 3 ...)'

    def test_a_cycle_through_a_car_terminates(self):
        # The cons is its own car, so the car is the cycle: one level of
        # parentheses, and the re-entry elides.
        outer = self.cons(lisptype.NIL, lisptype.NIL)
        outer.car = outer
        assert prin1_to_string(outer) == '(...)'

    def test_a_long_proper_list_is_not_truncated(self):
        # The cutoff must not be a cap on element *count*: that would elide
        # legitimate output. 400 elements, all present.
        result = lisptype.NIL
        for i in reversed(range(400)):
            result = self.cons(i, result)
        printed = prin1_to_string(result)
        assert '...' not in printed
        assert printed.startswith('(0 1 2 ') and printed.endswith(' 399)')

    def test_shared_but_acyclic_structure_prints_at_both_occurrences(self):
        # Only structure on the *current path* is a cycle. A shared tail is a
        # DAG, and an implementation without *PRINT-CIRCLE* prints it twice --
        # tracking every object ever seen would wrongly elide the second.
        tail = self.cons(1, self.cons(2, lisptype.NIL))
        assert prin1_to_string(self.cons(tail, self.cons(tail, lisptype.NIL))) \
            == '((1 2) (1 2))'

    def test_a_random_cons_graph_terminates(self):
        # print.cons.random.2's exact shape, over enough draws that a lucky one
        # cannot pass for a fix.
        import random
        for seed in range(60):
            random.seed(seed)
            cells = [self.cons(lisptype.NIL, lisptype.NIL) for _ in range(20)]
            for cell in cells:
                cell.car = cells[random.randrange(20)]
                cell.cdr = cells[random.randrange(20)]
            assert prin1_to_string(cells[0])
