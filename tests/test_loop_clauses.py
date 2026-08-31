"""LOOP's clause vocabulary: WITH, the BEING drivers, and the numeric and
boolean value clauses.

These are additions to the *one* iteration engine, not a second one. What they
have in common -- and the reason they are pinned together here rather than
beside whichever feature each belongs to -- is the failure mode they all shared:
a clause keyword the parser did not recognize was **silently dropped**, so the
loop ran and produced a plausible wrong answer instead of erring.

That is why several of these assert a *value* for a loop that "worked" before:

* ``(loop for x in '(1 5 3) maximize x)`` returned NIL, not 5;
* ``(loop for x in '(1 2 3) never (> x 5))`` returned NIL, and its NIL-returning
  sibling ``never (> x 2)`` therefore "passed" for entirely the wrong reason;
* ``(loop for x being the hash-keys of h ...)`` raised "LOOP FOR clause missing
  iteration spec" for every spelling but one.

Read the assertions as "this clause is executed at all", not merely "this clause
computes the right number".
"""

import io

import pytest

from fclpy import lispenv, lisptype
from fclpy.lispfunc.evaluation_core import eval as lisp_eval
from fclpy.lispreader import LispReader, LispStream
from fclpy.readtable import get_current_readtable


@pytest.fixture(autouse=True)
def env():
    lispenv.setup_standard_environment()


def ev(source):
    import fclpy.state as state
    stream = LispStream(io.StringIO(source))
    form = LispReader(get_current_readtable().get_macro_character, stream).read_1()
    return lisp_eval(form, state.current_environment)


def evs(source):
    """Evaluate and render through the Lisp printer, so a Python object leaking
    out as a Lisp value (standing rule 2) shows up as a wrong string rather than
    comparing equal to the value it is standing in for."""
    from fclpy.printer import prin1_to_string
    return prin1_to_string(ev(source))


def ev_names(source):
    """Evaluate a form yielding a list of symbols and return their names, sorted.

    Sorting happens on the Python side deliberately. Doing it in Lisp with SORT
    would couple these tests to a separate defect -- ``(sort (list 3 1 2) #'<)``
    currently returns a *vector*, so the assertion would fail for a reason that
    has nothing to do with LOOP.
    """
    from fclpy.lispfunc.core import car, cdr
    names = []
    cur = ev(source)
    while isinstance(cur, lisptype.lispCons):
        names.append(car(cur).name)
        cur = cdr(cur)
    return sorted(names)


class TestWith:
    """CLHS 6.1.1.4. WITH was not a recognized keyword at all: the token fell
    into the loop body and evaluated as a free reference, so every one of these
    used to signal ``Unbound variable: WITH``."""

    def test_a_with_variable_is_bound_in_the_body(self):
        assert evs("(loop with x = 1 do (return x))") == '1'

    def test_successive_with_clauses_bind_sequentially(self):
        """Each WITH sees the ones before it -- LET*, not LET."""
        assert evs("(loop with x = 1 with y = (1+ x) do (return (list x y)))") == '(1 2)'

    def test_and_joined_specs_bind_in_parallel(self):
        """AND makes one group, whose init forms all read the *outer* bindings.
        `b` and `c` therefore see the LET's NILs, not the loop's `a` and `b`."""
        assert evs("(let (a b) (loop with a = 1 and b = (list a) and c = (list b)"
                   "               return (list a b c)))") == '(1 (NIL) (NIL))'

    def test_with_initializes_once_not_once_per_iteration(self):
        assert evs("(loop with x = 0 repeat 3 do (incf x) finally (return x))") == '3'

    def test_a_return_in_an_init_form_exits_the_whole_loop(self):
        """The initialization happens inside LOOP's implicit NIL block, so the
        RETURN is this loop's, not an enclosing one's."""
        assert evs("(loop with nil = (return t) return nil)") == 'T'


class TestTypeSpecs:
    """CLHS 6.1.1.7. A type-spec may follow any LOOP variable, and one parser
    handles all three positions -- FOR, WITH and the numeric accumulations --
    because an unconsumed type-spec is read as the next clause's keyword and
    deletes that clause."""

    @pytest.mark.parametrize('source,want', [
        ("(loop with a t = 1 return a)", '1'),
        ("(loop with a fixnum = 2 return a)", '2'),
        ("(loop with a of-type string = \"abc\" return a)", '"abc"'),
        ("(loop for x of-type fixnum in '(1 2) collect x)", '(1 2)'),
        ("(loop for x in '(3 1 2) maximize x fixnum)", '3'),
        ("(loop for x in '(3 1 2) maximize x of-type integer)", '3'),
        ("(loop for x in '(3 1 2) maximize x into m of-type integer finally (return m))", '3'),
    ])
    def test_a_type_spec_does_not_consume_the_next_clause(self, source, want):
        assert evs(source) == want

    @pytest.mark.parametrize('source,want', [
        ("(loop with a of-type fixnum return a)", '0'),
        ("(loop with a of-type float return a)", '0.0'),
        ("(loop with a of-type t return a)", 'NIL'),
        ("(loop with a t return a)", 'NIL'),
    ])
    def test_a_typed_variable_with_no_init_form_starts_at_its_zero(self, source, want):
        assert evs(source) == want

    def test_a_destructured_type_spec_supplies_one_default_per_position(self):
        assert evs("(loop with (a b c) of-type (fixnum float t) return (list a b c))") \
            == '(0 0.0 NIL)'

    def test_collect_takes_no_type_spec(self):
        """`collect` is not one of the numeric accumulations, so a following T
        is the loop's next form and must not be eaten as a type-spec."""
        assert evs("(loop for x in '(1 2) collect x)") == '(1 2)'


class TestDestructuring:
    """One recursive walk, shared by WITH, every FOR driver and USING. It
    replaced three enumerated shapes that between them could not express a
    dotted tail, a NIL hole, or a pattern longer than its value."""

    @pytest.mark.parametrize('source,want', [
        ("(loop with (a b) = '(1 2) return (list b a))", '(2 1)'),
        ("(loop with (nil a) = '(1 2) return a)", '2'),
        ("(loop with (a nil) = '(1 2) return a)", '1'),
        ("(loop with (a b) = '(1) return (list a b))", '(1 NIL)'),
        ("(loop with (a b . rest) = '(1) return (list a b rest))", '(1 NIL NIL)'),
        ("(loop with (a . rest) = '(1 2 3) return (list a rest))", '(1 (2 3))'),
    ])
    def test_patterns(self, source, want):
        assert evs(source) == want


class TestNumericAccumulation:
    """CLHS 6.1.3.2. MAXIMIZE/MINIMIZE were absent from the clause table, so the
    keyword and its form were dropped and the loop returned NIL."""

    @pytest.mark.parametrize('source,want', [
        ("(loop for x in '(1 4 10 5 7 9) maximize x)", '10'),
        ("(loop for x in '(1 4 10 5 7 9) maximizing x)", '10'),
        ("(loop for x in '(4 10 1 5 7 9) minimize x)", '1'),
        ("(loop for x in '(4 10 1 5 7 9) minimizing x)", '1'),
        # No identity element to seed from: all-negative and all-positive runs
        # both have to come from the first value seen.
        ("(loop for x in '(-3 -1 -7) maximize x)", '-1'),
        ("(loop for x in '(3 1 7) minimize x)", '1'),
        ("(loop for x in '(1 4 2) maximize x into m finally (return m))", '4'),
    ])
    def test_extremum(self, source, want):
        assert evs(source) == want


class TestBooleanTerminationClauses:
    """CLHS 6.1.2.2. ALWAYS, NEVER and THEREIS decide the loop's value and end
    it at once, skipping the epilogue -- one shared decision, not three flags.
    NEVER was not in the clause table at all."""

    @pytest.mark.parametrize('source,want', [
        ("(loop for x in '(1 2 3) never (> x 5))", 'T'),
        ("(loop for x in '(1 2 3) never (> x 2))", 'NIL'),
        ("(loop for x in '() never t)", 'T'),
        ("(loop for x in '(1 2 3) always (< x 5))", 'T'),
        ("(loop for x in '(1 2 3) always (< x 2))", 'NIL'),
        ("(loop for x in '() always nil)", 'T'),
        ("(loop for x in '(1 2 3) thereis (and (> x 2) x))", '3'),
        ("(loop for x in '(1 2 3) thereis (> x 9))", 'NIL'),
    ])
    def test_value(self, source, want):
        assert evs(source) == want

    def test_a_decided_clause_skips_the_epilogue(self):
        """FINALLY must not get the chance to override the NIL."""
        assert evs("(loop for x in '(1 2 3) never (> x 2) finally (return 'wrong))") == 'NIL'

    def test_an_undecided_clause_still_runs_the_epilogue(self):
        assert evs("(loop for x in '(1 2) never (> x 9) finally (return 'epilogue))") \
            == 'EPILOGUE'


class TestForBeingHashTable:
    """CLHS 6.1.2.1.6. The parser recognized none of these: it matched only the
    plural SYMBOLS after THE and broke out of the clause otherwise, leaving no
    driver and raising "LOOP FOR clause missing iteration spec"."""

    TABLE = "(let ((h (make-hash-table))) (setf (gethash 'a h) 1) (setf (gethash 'b h) 2) "

    @pytest.mark.parametrize('clause', [
        'being the hash-values of h', 'being the hash-value of h',
        'being each hash-values of h', 'being each hash-value of h',
        'being the hash-values in h', 'being each hash-value in h',
    ])
    def test_every_spelling_of_hash_values_iterates(self, clause):
        assert evs(f"{self.TABLE} (loop for v {clause} sum v))") == '3'

    @pytest.mark.parametrize('clause', [
        'being the hash-keys of h', 'being each hash-key in h',
    ])
    def test_every_spelling_of_hash_keys_iterates(self, clause):
        assert ev_names(f"{self.TABLE} (loop for k {clause} collect k))") == ['A', 'B']

    def test_using_binds_the_other_half_of_the_entry(self):
        assert evs(f"{self.TABLE} (loop for k being the hash-keys of h using (hash-value v)"
                   f"                    sum v))") == '3'

    def test_a_nil_variable_iterates_without_binding(self):
        assert evs(f"{self.TABLE} (loop for nil being the hash-values of h count t))") == '2'

    def test_the_table_carries_no_entries_of_its_own(self):
        """The table's test and sizing are attributes, not entries. They used to
        be three ``__hashmeta__`` *keys*, which this driver collected as Lisp
        values -- Python strings appearing as Lisp data (standing rule 2)."""
        assert evs("(let ((h (make-hash-table))) (loop for k being the hash-keys of h count t))") \
            == '0'
        assert evs("(let ((h (make-hash-table))) (hash-table-count h))") == '0'

    def test_a_non_table_is_a_type_error_not_an_empty_iteration(self):
        with pytest.raises(BaseException):
            ev("(loop for k being the hash-keys of '(1 2 3) count t)")


class TestForBeingPackage:
    """CLHS 6.1.2.1.7, and the three symbol sets shared with DO-SYMBOLS and
    DO-EXTERNAL-SYMBOLS."""

    SETUP = ('(progn (defpackage "LOOP.TEST.PKG" (:use) (:intern "FOO") (:export "BAR")) ')

    @pytest.mark.parametrize('clause,want', [
        ('being the external-symbols of "LOOP.TEST.PKG"', ['BAR']),
        ('being each external-symbol in "LOOP.TEST.PKG"', ['BAR']),
        ('being the present-symbols of "LOOP.TEST.PKG"', ['BAR', 'FOO']),
        ('being the symbols of "LOOP.TEST.PKG"', ['BAR', 'FOO']),
    ])
    def test_symbol_sets(self, clause, want):
        assert ev_names(f'{self.SETUP} (loop for s {clause} collect s))') == want

    def test_a_package_object_is_an_acceptable_designator(self):
        assert evs(f'{self.SETUP} (loop for s being the external-symbols'
                   f'                      of (find-package "LOOP.TEST.PKG") count t))') == '1'

    def test_an_unknown_package_signals_rather_than_iterating_nothing(self):
        """The previous parse swallowed the failed lookup with a bare
        ``except Exception`` and iterated an empty package, so a misspelled name
        produced 0 rather than an error (standing rule 4)."""
        with pytest.raises(BaseException):
            ev('(loop for s being the symbols of "NO.SUCH.PACKAGE" count t)')


class TestDuplicateVariables:
    """CLHS 6.1.1.7: "An error of type program-error is signaled ... if the same
    variable is bound twice in any variable-binding clause of a single loop
    expression", including variables found by destructuring."""

    @pytest.mark.parametrize('source', [
        "(loop with a = 1 and a = 2 return a)",
        "(loop with a = 1 with a = 2 return a)",
        "(loop with (a b) = '(1 2) with a = 3 return a)",
        "(loop for a in '(1) for a in '(2) collect a)",
    ])
    def test_binding_a_name_twice_is_a_program_error(self, source):
        # The signal is a real PROGRAM-ERROR *condition* (CLHS 6.1.1.7), not a
        # bare Python LispProgramError: it is raised at macro-expansion time,
        # and ansi-test's signals-error matches conditions by type -- which is
        # what LOOP.4.7/.4.8 and LOOP.5.ERROR.3/.4 reach for.
        from fclpy.lispfunc.evaluation_core import ConditionException
        with pytest.raises(ConditionException) as exc_info:
            ev(source)
        assert isinstance(exc_info.value.condition, lisptype.ProgramError)

    def test_one_accumulation_destination_may_be_named_repeatedly(self):
        """INTO destinations are not variable *bindings* in that sense -- two
        clauses accumulating into one variable is legal and useful."""
        assert evs("(loop for x in '(1 2) collect x into r collect (- x) into r"
                   "      finally (return r))") == '(1 -1 2 -2)'
