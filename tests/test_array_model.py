"""The array object model (CLHS 15.1): the five properties an array knows.

These replace `test_phase5_task3_vectors.py` and `test_phase5_task4_arrays.py`,
which certified `vectors.py`'s `AdjustableVector`/`Array` classes -- an
implementation that had *lost* the registry to `sequences_higher.py`'s
copies, so every assertion in them was about code no Lisp form could reach.
(The same shape as plan.md C12's dead `reader.py`: 177 unit tests certifying
a module nothing imports.)

They are grouped by *property* rather than by operator, because that is the
level at which the defect existed: an array that does not record its element
type fails MAKE-ARRAY, AREF, TYPEP, BIT-VECTOR-P, the printer and the reader
at once, and they all come back together.
"""

import pytest

import fclpy.lisptype as lisptype
import fclpy.printer as printer
from fclpy.lispfunc import arrays
from fclpy.lispfunc.arrays import (
    BIT_TYPE, CHARACTER_TYPE, LispArray, T_TYPE, adjust_array,
    array_dimension, array_dimensions, array_element_type,
    array_has_fill_pointer_p, array_in_bounds_p, array_rank, array_total_size,
    adjustable_array_p, aref, aref_set, arrayp, bit_and, bit_not,
    bit_vector_p, fill_pointer, make_array, row_major_aref, set_fill_pointer,
    simple_bit_vector_p, simple_vector_p, vector_pop, vector_push,
    vector_push_extend, vectorp,
)


def lisp_list(*items):
    result = lisptype.NIL
    for item in reversed(items):
        result = lisptype.lispCons(item, result)
    return result


class TestRepresentation:
    """Which Python object MAKE-ARRAY answers -- `_new_array`'s one rule."""

    def test_a_simple_general_vector_is_a_python_list(self):
        assert make_array(3) == [lisptype.NIL] * 3

    def test_a_specialized_or_non_simple_vector_is_a_lisp_array(self):
        assert isinstance(make_array(3, element_type='BIT'), LispArray)
        assert isinstance(make_array(3, fill_pointer=0), LispArray)
        assert isinstance(make_array(3, adjustable=lisptype.T), LispArray)
        assert isinstance(make_array(lisp_list(2, 3)), LispArray)

    def test_a_character_array_is_a_string(self):
        text = make_array(4, element_type='CHARACTER',
                          initial_element=lisptype.Character('x'))
        assert isinstance(text, lisptype.LispString)
        assert str(text) == 'xxxx'


class TestDimensions:
    def test_rank_and_dimensions(self):
        array = make_array(lisp_list(2, 3, 4))
        assert array_rank(array) == 3
        assert arrays.array_dimensions_of(array) == (2, 3, 4)
        assert array_total_size(array) == 24
        assert array_dimension(array, 1) == 3

    def test_a_vector_dimension_is_its_size_not_its_fill_pointer(self):
        # `(array-dimension (make-array 10 :fill-pointer 5) 0)` is 10: the
        # fill pointer bounds the *sequence*, never the array (CLHS 15.1.2).
        # The old implementation answered the fill pointer, and a unit test
        # asserted that wrong answer (plan.md's non-ANSI assertion table).
        array = make_array(10, fill_pointer=5)
        assert array_dimension(array, 0) == 10
        assert array_total_size(array) == 10
        assert len(array) == 5

    def test_dimensions_are_a_lisp_list(self):
        dims = array_dimensions(make_array(lisp_list(2, 3)))
        assert isinstance(dims, lisptype.lispCons)
        assert [dims.car, dims.cdr.car] == [2, 3]

    def test_a_dimension_out_of_range_is_an_error(self):
        with pytest.raises(lisptype.LispTypeError):
            array_dimension(make_array(3), 1)

    def test_array_in_bounds_p(self):
        array = make_array(lisp_list(2, 3))
        assert array_in_bounds_p(array, 1, 2) == lisptype.T
        assert array_in_bounds_p(array, 2, 0) == lisptype.NIL
        assert array_in_bounds_p(array, 0) == lisptype.NIL


class TestElementAccess:
    def test_aref_reaches_every_rank(self):
        array = make_array(lisp_list(2, 3), initial_element=0)
        aref_set(array, (1, 2), 42)
        assert aref(array, 1, 2) == 42
        assert aref(array, 0, 0) == 0

    def test_aref_is_row_major(self):
        array = make_array(lisp_list(2, 3), initial_element=0)
        aref_set(array, (1, 0), 7)
        assert row_major_aref(array, 3) == 7
        assert arrays.array_row_major_index(array, 1, 0) == 3

    def test_the_wrong_number_of_subscripts_is_a_lisp_error(self):
        # It used to raise a Python `IndexError: Expected 2 indices, got 1`,
        # which surfaced as the value of the form (standing rule 2).
        array = make_array(lisp_list(2, 3))
        with pytest.raises(lisptype.LispTypeError):
            aref(array, 1)

    def test_aref_sees_past_the_fill_pointer(self):
        # AREF indexes the array; ELT indexes the sequence (CLHS 15.1.2.1).
        array = make_array(5, fill_pointer=2, initial_element=9)
        assert aref(array, 4) == 9
        assert len(array) == 2

    def test_a_string_element_is_a_character(self):
        text = make_array(3, element_type='CHARACTER',
                          initial_element=lisptype.Character('a'))
        assert isinstance(aref(text, 0), lisptype.Character)


class TestFillPointer:
    def test_fill_pointer_of_a_vector_without_one_is_an_error(self):
        assert array_has_fill_pointer_p(make_array(3)) == lisptype.NIL
        with pytest.raises(lisptype.LispTypeError):
            fill_pointer(make_array(3))

    def test_fill_pointer_t_means_the_whole_vector(self):
        assert fill_pointer(make_array(4, fill_pointer=lisptype.T)) == 4

    def test_vector_push_answers_nil_when_full(self):
        array = make_array(2, fill_pointer=0)
        assert vector_push('a', array) == 0
        assert vector_push('b', array) == 1
        assert vector_push('c', array) is lisptype.NIL
        assert fill_pointer(array) == 2

    def test_vector_push_extend_grows_an_adjustable_vector(self):
        # VECTOR-PUSH used to be `vector.append(...)`, which an array object
        # does not have -- the AttributeError became the value of the form.
        array = make_array(1, fill_pointer=0, adjustable=lisptype.T)
        for index in range(5):
            assert vector_push_extend(index, array) == index
        assert fill_pointer(array) == 5
        assert [aref(array, i) for i in range(5)] == [0, 1, 2, 3, 4]

    def test_vector_push_extend_refuses_a_full_non_adjustable_vector(self):
        array = make_array(1, fill_pointer=0)
        vector_push_extend('a', array)
        with pytest.raises(lisptype.LispTypeError):
            vector_push_extend('b', array)

    def test_vector_pop(self):
        array = make_array(3, fill_pointer=0, adjustable=lisptype.T)
        vector_push_extend('a', array)
        vector_push_extend('b', array)
        assert vector_pop(array) == 'b'
        assert fill_pointer(array) == 1

    def test_setting_the_fill_pointer_is_bounded_by_the_size(self):
        array = make_array(5, fill_pointer=2)
        assert set_fill_pointer(array, 5) == 5
        with pytest.raises(lisptype.LispTypeError):
            set_fill_pointer(array, 6)


class TestAdjustability:
    def test_a_literal_vector_is_not_adjustable(self):
        # `#(...)` is a *simple* vector (CLHS 2.4.8.3). The reader used to
        # build an `AdjustableVector`, so every literal claimed otherwise.
        assert adjustable_array_p(make_array(3)) == lisptype.NIL
        assert adjustable_array_p(make_array(3, adjustable=lisptype.T)) == lisptype.T

    def test_adjust_array_grows_an_adjustable_array_in_place(self):
        array = make_array(2, adjustable=lisptype.T, initial_element=1)
        assert adjust_array(array, 4, initial_element=0) is array
        assert array_total_size(array) == 4
        assert [aref(array, i) for i in range(4)] == [1, 1, 0, 0]

    def test_adjust_array_keeps_elements_at_their_own_subscripts(self):
        # For rank > 1 the elements move: element (i j) stays element (i j),
        # which is not the same as keeping the row-major order (CLHS 15.1.4).
        array = make_array(lisp_list(2, 2), adjustable=lisptype.T, initial_element=0)
        for i in range(2):
            for j in range(2):
                aref_set(array, (i, j), 10 * i + j)
        adjust_array(array, lisp_list(3, 3), initial_element=None)
        assert aref(array, 1, 1) == 11
        assert aref(array, 0, 1) == 1

    def test_adjusting_a_non_adjustable_array_answers_a_new_one(self):
        array = make_array(2, initial_element=1)
        adjusted = adjust_array(array, 3)
        assert adjusted is not array
        assert array_total_size(adjusted) == 3


class TestDisplacement:
    def test_a_displaced_array_shares_storage(self):
        target = make_array(5, initial_element=0)
        view = make_array(2, displaced_to=target, displaced_index_offset=2)
        aref_set(view, (0,), 'x')
        assert aref(target, 2) == 'x'
        aref_set(target, (3,), 'y')
        assert aref(view, 1) == 'y'

    def test_array_displacement_reports_the_target_and_offset(self):
        target = make_array(5)
        view = make_array(2, displaced_to=target, displaced_index_offset=3)
        values = arrays.array_displacement(view).get_all()
        assert values[0] is target and values[1] == 3
        assert arrays.array_displacement(target).get_all()[0] is lisptype.NIL

    def test_displacement_beyond_the_target_is_an_error(self):
        with pytest.raises(lisptype.LispTypeError):
            make_array(4, displaced_to=make_array(5), displaced_index_offset=3)


class TestElementTypes:
    def test_element_type_is_recorded_and_answered_as_a_symbol(self):
        # It used to answer the *Python string* `'T'` for every array.
        assert array_element_type(make_array(3)) is T_TYPE
        assert array_element_type(make_array(3, element_type='BIT')) is BIT_TYPE
        assert array_element_type(
            make_array(3, element_type='CHARACTER')) is CHARACTER_TYPE

    def test_a_bit_array_is_filled_with_bits_not_nil(self):
        array = make_array(3, element_type='BIT')
        assert [aref(array, i) for i in range(3)] == [0, 0, 0]

    def test_storing_a_non_bit_in_a_bit_array_is_a_type_error(self):
        array = make_array(3, element_type='BIT')
        with pytest.raises(lisptype.LispTypeError):
            aref_set(array, (0,), 'x')

    def test_a_bit_vector_is_not_a_general_vector_of_ones_and_zeroes(self):
        assert bit_vector_p(make_array(2, element_type='BIT')) == lisptype.T
        assert bit_vector_p([0, 1]) == lisptype.NIL
        assert simple_vector_p([0, 1]) == lisptype.T
        assert simple_vector_p(make_array(2, element_type='BIT')) == lisptype.NIL
        assert simple_bit_vector_p(
            make_array(2, element_type='BIT', fill_pointer=0)) == lisptype.NIL

    def test_bitwise_operators_answer_bit_arrays(self):
        left = make_array(4, element_type='BIT',
                          initial_contents=lisp_list(1, 0, 1, 0))
        right = make_array(4, element_type='BIT',
                           initial_contents=lisp_list(1, 1, 0, 0))
        assert list(bit_and(left, right)) == [1, 0, 0, 0]
        assert bit_vector_p(bit_and(left, right)) == lisptype.T
        assert list(bit_not(left)) == [0, 1, 0, 1]

    def test_a_bitwise_result_destination_may_be_t_or_an_array(self):
        left = make_array(2, element_type='BIT', initial_contents=lisp_list(1, 1))
        right = make_array(2, element_type='BIT', initial_contents=lisp_list(1, 0))
        assert bit_and(left, right, lisptype.T) is left
        assert list(left) == [1, 0]


class TestInitialContents:
    def test_initial_contents_is_nested_by_dimension(self):
        array = make_array(lisp_list(2, 2),
                           initial_contents=lisp_list(lisp_list(1, 2),
                                                      lisp_list(3, 4)))
        assert aref(array, 1, 0) == 3

    def test_initial_contents_of_the_wrong_shape_is_an_error(self):
        with pytest.raises(lisptype.LispTypeError):
            make_array(3, initial_contents=lisp_list(1, 2))

    def test_initial_contents_and_initial_element_are_exclusive(self):
        with pytest.raises(lisptype.LispProgramError):
            make_array(3, initial_element=0, initial_contents=lisp_list(1, 2, 3))


class TestTypeSpecifiers:
    """`(array element-type dimensions)` and friends -- CLHS 4.2.3."""

    def _typep(self, object, specifier):
        from fclpy.lispfunc.comparison import typep
        return typep(object, specifier)

    def test_every_array_is_arrayp_including_strings_and_high_ranks(self):
        assert arrayp(make_array(lisp_list(2, 2))) == lisptype.T
        assert arrayp(lisptype.LispString('ab')) == lisptype.T
        assert vectorp(make_array(lisp_list(2, 2))) == lisptype.NIL
        assert vectorp(lisptype.LispString('ab')) == lisptype.T

    def test_element_type_and_dimensions_are_both_checked(self):
        array = make_array(lisp_list(2, 3))
        assert self._typep(array, lisp_list(
            lisptype.LispSymbol('ARRAY'), T_TYPE, lisp_list(2, 3))) == lisptype.T
        assert self._typep(array, lisp_list(
            lisptype.LispSymbol('ARRAY'), T_TYPE, lisp_list(3, 2))) == lisptype.NIL
        assert self._typep(array, lisp_list(
            lisptype.LispSymbol('ARRAY'), BIT_TYPE)) == lisptype.NIL

    def test_a_rank_may_be_given_instead_of_dimensions(self):
        array = make_array(lisp_list(2, 3))
        assert self._typep(array, lisp_list(
            lisptype.LispSymbol('ARRAY'), T_TYPE, 2)) == lisptype.T
        assert self._typep(array, lisp_list(
            lisptype.LispSymbol('ARRAY'), T_TYPE, 1)) == lisptype.NIL

    def test_simple_excludes_a_fill_pointer(self):
        plain, pointered = make_array(3), make_array(3, fill_pointer=0)
        assert self._typep(plain, lisptype.LispSymbol('SIMPLE-ARRAY')) == lisptype.T
        assert self._typep(pointered, lisptype.LispSymbol('SIMPLE-ARRAY')) == lisptype.NIL
        assert self._typep(pointered, lisptype.LispSymbol('ARRAY')) == lisptype.T


class TestPrintedRepresentation:
    def test_a_bit_vector_prints_as_a_bit_vector(self):
        array = make_array(4, element_type='BIT',
                           initial_contents=lisp_list(1, 0, 1, 1))
        assert printer.print_object(array) == '#*1011'

    def test_a_multidimensional_array_prints_with_its_rank(self):
        array = make_array(lisp_list(2, 2),
                           initial_contents=lisp_list(lisp_list(1, 2),
                                                      lisp_list(3, 4)))
        assert printer.print_object(array) == '#2A((1 2) (3 4))'

    def test_a_fill_pointer_bounds_what_is_printed(self):
        array = make_array(4, fill_pointer=2, initial_element=0)
        assert printer.print_object(array) == '#(0 0)'
