"""The hash table object model (CLHS 18) -- the key-equivalence model.

Replaces `test_phase5_task7_hashtables.py`, which tested
`lispfunc/hashtables.py`'s `HashTable` -- a second, *dead* implementation that
`MAKE-HASH-TABLE` never returned. Those tests passed against an object no Lisp
program could ever obtain, so they covered nothing; that module and they went
together.

The tests below are about the one property that makes a hash table a hash
table: two keys denote the same entry exactly when the table's *test* says
they are equivalent. Every case here answered wrongly while the table was a
Python `dict` whose `test` attribute nothing read.
"""

import pytest

from fclpy import lispenv, lisptype
from fclpy.lispfunc import eval_string


@pytest.fixture(scope='module', autouse=True)
def _environment():
    lispenv.setup_standard_environment()


def value_of(form):
    """The primary value of `form`, as a single-value context would see it."""
    return lisptype.primary_value(eval_string(form))


def values_of(form):
    return value_of(f'(multiple-value-list {form})')


def as_list(lisp_list):
    out = []
    current = lisp_list
    while isinstance(current, lisptype.lispCons):
        out.append(current.car)
        current = current.cdr
    return out


def named(obj):
    """A symbol's name, so a test can compare without interning.

    `LispSymbol.__eq__` is identity -- global lookup is by symbol object, not
    by name -- so ``value == LispSymbol('EQL')`` is false even when the value
    *is* the EQL symbol. Comparing names is what a test wants here; comparing
    identity is what `hash-table-test.2` does with EQ, and the ANSI run covers
    that.
    """
    return getattr(obj, 'name', obj)


class TestTheTestIsHonoured:
    """The declared test decides which keys collide -- nothing else does."""

    def test_equal_table_finds_a_structurally_equal_key(self):
        """An EQUAL table must find a list key built twice.

        A `lispCons` hashes by identity in Python, so this answered NIL: the
        table could not find a key it had just stored.
        """
        assert value_of(
            '(let ((h (make-hash-table :test (quote equal))))'
            '  (setf (gethash (list 1 2) h) 9)'
            '  (gethash (list 1 2) h))') == 9

    def test_eql_table_does_not_conflate_two_equal_strings(self):
        """Two distinct strings are not EQL, so an EQL table keeps them apart.

        Python string equality made an EQL table behave like an EQUAL one.
        """
        assert value_of(
            '(let ((h (make-hash-table)))'
            '  (setf (gethash "ab" h) 9)'
            '  (gethash (copy-seq "ab") h))') is lisptype.NIL

    def test_eql_table_does_not_conflate_an_integer_with_a_float(self):
        """``(eql 1 1.0)`` is false though ``hash(1) == hash(1.0)`` in Python."""
        assert value_of(
            '(let ((h (make-hash-table)))'
            '  (setf (gethash 1.0 h) :float)'
            '  (gethash 1 h))') is lisptype.NIL

    def test_equal_table_distinguishes_an_integer_from_a_float(self):
        """EQUAL falls through to EQL for numbers, so the type still counts."""
        assert value_of(
            '(let ((h (make-hash-table :test (quote equal))))'
            '  (setf (gethash 1 h) :int)'
            '  (setf (gethash 1.0 h) :float)'
            '  (hash-table-count h))') == 2

    def test_equalp_table_ignores_case_and_numeric_type(self):
        """EQUALP is coarser than EQUAL in exactly these two ways."""
        assert value_of(
            '(let ((h (make-hash-table :test (quote equalp))))'
            '  (setf (gethash "AB" h) 1)'
            '  (gethash "ab" h))') == 1
        assert value_of(
            '(let ((h (make-hash-table :test (quote equalp))))'
            '  (setf (gethash 1 h) :one)'
            '  (gethash 1.0 h))') is not lisptype.NIL

    def test_eq_table_keeps_two_equal_lists_apart(self):
        """EQ is identity, so structure must not make two keys collide."""
        assert value_of(
            '(let ((h (make-hash-table :test (quote eq))))'
            '  (setf (gethash (list 1 2) h) 1)'
            '  (setf (gethash (list 1 2) h) 2)'
            '  (hash-table-count h))') == 2


class TestGethashReturnsTwoValues:
    """GETHASH is ``(values value present-p)`` -- CLHS 18.2."""

    def test_absent_key_answers_two_nils(self):
        assert as_list(values_of('(gethash 1 (make-hash-table))')) == [
            lisptype.NIL, lisptype.NIL]

    def test_present_key_answers_present_p_true(self):
        result = as_list(values_of(
            '(let ((h (make-hash-table))) (setf (gethash 1 h) 5) (gethash 1 h))'))
        assert result[0] == 5
        assert result[1] is lisptype.T

    def test_a_stored_nil_is_still_present(self):
        """The reason one value is not enough: NIL is a legitimate value."""
        result = as_list(values_of(
            '(let ((h (make-hash-table))) (setf (gethash 1 h) nil) (gethash 1 h))'))
        assert result[0] is lisptype.NIL
        assert result[1] is lisptype.T

    def test_the_default_is_returned_but_not_stored(self):
        assert named(value_of(
            "(gethash 'x (make-hash-table) 'y)")) == 'Y'


class TestSetfGethashEvaluatesItsSubformsOnce:
    """CLHS 5.1.1.1 -- left to right, exactly once, the default included."""

    def test_the_default_subform_is_evaluated(self):
        """``(setf (gethash k table (incf i)) v)`` must still increment I.

        The SETF expansion took only the first two subforms, so the default
        form was never evaluated.
        """
        result = as_list(values_of(
            '(let ((table (make-hash-table)) (i 0))'
            "  (values (setf (gethash 'x table (incf i)) 'y) i))"))
        assert [named(result[0]), result[1]] == ['Y', 1]

    def test_subforms_are_evaluated_left_to_right(self):
        result = as_list(values_of(
            '(let ((i 0) x y (table (make-hash-table)))'
            "  (setf (gethash 'a table) 'b)"
            "  (values (gethash (progn (setf x (incf i)) 'a)"
            '                   (progn (setf y (incf i)) table))'
            '          i x y))'))
        assert [named(result[0])] + result[1:] == ['B', 2, 1, 2]


class TestAccessors:
    def test_hash_table_p_agrees_with_typep(self):
        """The two used to disagree about the same object."""
        assert value_of('(hash-table-p (make-hash-table))') is lisptype.T
        assert value_of("(typep (make-hash-table) 'hash-table)") is lisptype.T

    def test_hash_table_test_answers_a_symbol(self):
        """Not the Python string ``'EQL'``, and not a function's repr."""
        assert named(value_of('(hash-table-test (make-hash-table))')) == 'EQL'

    @pytest.mark.parametrize('designator,expected',
                             [("'eq", 'EQ'), ("#'eq", 'EQ'),
                              ("'equalp", 'EQUALP'), ("#'equalp", 'EQUALP')])
    def test_a_function_designator_maps_back_to_its_symbol(self, designator,
                                                           expected):
        """``:test #'eq`` and ``:test 'eq`` name the same table."""
        assert named(value_of(
            f'(hash-table-test (make-hash-table :test {designator}))')) == expected

    def test_hash_table_size_is_a_capacity_not_a_count(self):
        """It used to be an alias for HASH-TABLE-COUNT, so this answered 0."""
        assert value_of('(hash-table-size (make-hash-table :size 100))') == 100

    def test_size_stays_a_non_negative_integer_as_the_table_grows(self):
        """`hash-table-aux.lsp` asserts this on every one of 1000 iterations."""
        assert value_of(
            '(let ((h (make-hash-table :size 0)))'
            '  (dotimes (i 200) (setf (gethash i h) i))'
            '  (and (integerp (hash-table-size h))'
            '       (>= (hash-table-size h) (hash-table-count h))))') is lisptype.T

    def test_accessors_signal_a_type_error_for_a_non_table(self):
        """Answering NIL or 0 conflated "not a table" with a real answer."""
        for operator in ('hash-table-count', 'hash-table-size',
                         'hash-table-test', 'hash-table-rehash-size',
                         'hash-table-rehash-threshold', 'clrhash'):
            assert value_of(
                f"(typep (nth-value 1 (ignore-errors ({operator} 17)))"
                "        'type-error)") is lisptype.T


class TestTraversal:
    def test_maphash_returns_exactly_one_nil(self):
        assert as_list(values_of(
            '(maphash (lambda (k v) (declare (ignore k v))) (make-hash-table))')) \
            == [lisptype.NIL]

    def test_remhash_during_maphash_is_allowed(self):
        """CLHS 18.2 permits it; a live view would raise a Python error."""
        assert value_of(
            '(let ((h (make-hash-table)))'
            '  (dotimes (i 50) (setf (gethash i h) i))'
            '  (maphash (lambda (k v) (declare (ignore v)) (remhash k h)) h)'
            '  (hash-table-count h))') == 0

    def test_loop_over_hash_keys_sees_the_real_keys(self):
        """Not the meta-keys, and not an internal surrogate object."""
        assert value_of(
            '(let ((h (make-hash-table)))'
            '  (setf (gethash 7 h) 8)'
            '  (loop for k being the hash-keys of h sum k))') == 7

    def test_with_hash_table_iterator_yields_three_values(self):
        result = as_list(values_of(
            '(let ((h (make-hash-table)))'
            '  (setf (gethash 1 h) 2)'
            '  (with-hash-table-iterator (next h) (next)))'))
        assert result == [lisptype.T, 1, 2]

    def test_with_hash_table_iterator_is_nil_when_exhausted(self):
        assert value_of(
            '(with-hash-table-iterator (next (make-hash-table)) (next))') \
            is lisptype.NIL

    def test_with_hash_table_iterator_passes_the_bodys_values_through(self):
        """`.2` returns *zero* values and `.3` returns four."""
        assert [named(v) for v in as_list(values_of(
            "(with-hash-table-iterator (x (make-hash-table)) (values 'a 'b))"))] \
            == ['A', 'B']
        assert as_list(values_of(
            '(with-hash-table-iterator (x (make-hash-table)) (values))')) == []

    def test_with_hash_table_iterator_accepts_declarations(self):
        assert value_of(
            '(with-hash-table-iterator (x (make-hash-table)) (declare (optimize)))') \
            is lisptype.NIL

    def test_with_hash_table_iterator_visits_every_entry_once(self):
        assert value_of(
            '(let ((h (make-hash-table)) (n 0))'
            '  (dotimes (i 30) (setf (gethash i h) i))'
            '  (with-hash-table-iterator (next h)'
            '    (loop (multiple-value-bind (more k v) (next)'
            '            (declare (ignore k v))'
            '            (unless more (return n))'
            '            (incf n)))))') == 30


class TestSxhash:
    """CLHS 18.2.2: ``(equal x y)`` implies ``(= (sxhash x) (sxhash y))``."""

    def test_sxhash_is_a_non_negative_fixnum(self):
        assert value_of(
            "(typep (sxhash '(a b c)) '(and unsigned-byte fixnum))") is lisptype.T

    @pytest.mark.parametrize('form', [
        '(list 1 2)',
        '(copy-seq "abc")',
        '(copy-seq #*1011)',
        "(cons 'a 'b)",
    ])
    def test_equal_objects_hash_alike(self, form):
        """Each of these answered two different hashes under ``hash(obj)``."""
        assert value_of(f'(let ((a {form}) (b {form}))'
                        '  (and (equal a b) (= (sxhash a) (sxhash b))))') \
            is lisptype.T

    def test_a_symbol_hashes_by_name_only(self):
        """`sxhash.13`: two uninterned symbols named FOO hash alike."""
        assert value_of(
            '(= (sxhash (make-symbol "FOO")) (sxhash (make-symbol "FOO")))') \
            is lisptype.T

    def test_a_general_arrays_hash_survives_mutation(self):
        """`sxhash.7`: EQUAL does not descend into a general array, so its
        hash must not depend on the elements."""
        assert value_of(
            '(let* ((a (make-array 10 :initial-element nil)) (h (sxhash a)))'
            "  (setf (aref a 4) 'x)"
            '  (= h (sxhash a)))') is lisptype.T

    def test_a_circular_key_terminates(self):
        """`sxhash.16`: an unbounded descent cannot even return."""
        assert value_of(
            "(let ((a (list 'a)) (b (list 'a)))"
            '  (setf (cdr a) a) (setf (cdr b) b)'
            '  (= (sxhash a) (sxhash b)))') is lisptype.T

    def test_string_representations_hash_alike(self):
        """`sxhash.5`/`.21`: a fill-pointered string is EQUAL to a plain one."""
        assert value_of(
            '(let ((s1 "abcd")'
            '      (s2 (make-array 10 :element-type (quote character)'
            '                      :initial-contents "abcdefghij"'
            '                      :fill-pointer 4)))'
            '  (and (equal s1 s2) (= (sxhash s1) (sxhash s2))))') is lisptype.T


class TestFixnumBoundaryHasOneHome:
    """TYPEP's FIXNUM and MOST-POSITIVE-FIXNUM must agree (CLHS 12.1.1.1).

    Here because SXHASH is specified to return a fixnum, which is how the
    disagreement surfaced: every `sxhash` test that checked its result's type
    failed on a value that *was* in range.
    """

    def test_most_positive_fixnum_is_a_fixnum(self):
        assert value_of("(typep most-positive-fixnum 'fixnum)") is lisptype.T
        assert value_of("(typep most-negative-fixnum 'fixnum)") is lisptype.T

    def test_typep_and_subtypep_agree_about_a_mid_range_integer(self):
        assert value_of("(typep 1000000000 'fixnum)") is lisptype.T
        assert value_of("(typep 1000000000 'bignum)") is lisptype.NIL
