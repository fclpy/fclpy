"""SUBTYPEP decides type relationships from a type lattice, not a name table.

`SUBTYPEP` used to be a table of hardcoded string pairs in `comparison.py` --
`if t1 == 'INTEGER' and t2 in ['RATIONAL','REAL','NUMBER']` -- with no row for
any compound specifier at all. The important part is *how* it was wrong: a table
miss returned `NIL, T`, i.e. "certainly not a subtype", so
`(subtypep '(integer 0 10) 'integer)` and `(subtypep 'fixnum 'integer)` answered
a confident No. That is why growing the table was not an option and why
`fclpy.typespec` exists: the relation has to be *decided*, as emptiness of
`type1 \\ type2`, over a representation closed under union, intersection and
complement.

The assertions here are the shapes ansi-test's `auxiliary/types-aux.lsp`
actually demands. Its `check-equivalence` asks twelve questions per call and
requires all twelve to be marked **certain**, including

    (subtypep '(and TYPE1 (not TYPE2)) nil)     ; is this difference empty?
    (subtypep t '(or TYPE1 (not TYPE2)))        ; is this union everything?

so the second value is asserted throughout rather than being ignored. A test
that only checked the first value would pass against an implementation that had
merely become vaguer, which is the regression this file is meant to catch.

Everything is driven through the Lisp surface (`(subtypep ...)`), not by calling
`typespec.type_subtypep` directly, so a registry or arity regression is caught
too -- SUBTYPEP takes an optional environment argument and must signal a
PROGRAM-ERROR for any other arity.
"""

import io

import pytest

from fclpy import lispenv, lisptype
from fclpy.lispfunc.evaluation_core import eval as lisp_eval
from fclpy.lispreader import LispReader, LispStream
from fclpy.readtable import get_current_readtable


@pytest.fixture(autouse=True)
def env():
    """A freshly bootstrapped standard environment, built-in classes included.

    The explicit class re-initialisation compensates for a state leak elsewhere
    in the unit suite, not for anything in the type lattice: several tests in
    `test_phase6_classes.py` do `classes._class_registry._classes.clear()` and
    never restore the built-ins, while `_builtin_classes_initialized` stays
    True -- so `(find-class 'array)` raises `NameError: Class not found: ARRAY`
    for every test that runs afterwards. A standard environment has the built-in
    classes by definition, so asking for them here is what this fixture already
    claims to provide. Recorded as a discovered issue rather than fixed in
    place, because the leak belongs to that file and to `classes.py`'s
    duplicated `_init_builtin_classes` (CLAUDE.md, standing rule 3).
    """
    lispenv.setup_standard_environment()
    from fclpy import classes
    classes._builtin_classes_initialized = False
    classes._init_builtin_classes()
    import fclpy.state as state
    return state.current_environment


def ev(source):
    """Read and evaluate `source`."""
    import fclpy.state as state
    stream = LispStream(io.StringIO(source))
    readtable = get_current_readtable()
    form = LispReader(readtable.get_macro_character, stream).read_1()
    return lisp_eval(form, state.current_environment)


def subtypep(type1, type2):
    """`(subtypep type1 type2)` as a Python `(sub, certain)` pair of bools.

    Reads both values, because "certain" is half of what CLHS 4.3.4 specifies
    and the whole of what ansi-test's `check-*` helpers assert.
    """
    result = ev('(multiple-value-list (subtypep %s %s))' % (type1, type2))
    values = []
    while isinstance(result, lisptype.lispCons):
        values.append(result.car)
        result = result.cdr
    assert len(values) == 2, (
        'SUBTYPEP must return exactly two values, got %d -- ansi-test reads them '
        'with MULTIPLE-VALUE-LIST and checks the length' % (len(values),))
    return tuple(v is lisptype.T for v in values)


def assert_equivalent(type1, type2):
    """The core of ansi-test's `check-equivalence`, including its NOT clauses.

    The last four are the ones a name table can never answer: they require
    deciding that a *difference* is empty and that a *union* is everything.
    """
    assert subtypep(type1, type2) == (True, True)
    assert subtypep(type2, type1) == (True, True)
    assert subtypep("'(not %s)" % type1.lstrip("'"),
                    "'(not %s)" % type2.lstrip("'")) == (True, True)
    assert subtypep("'(and %s (not %s))" % (type1.lstrip("'"), type2.lstrip("'")),
                    "'nil") == (True, True)
    assert subtypep("'(and %s (not %s))" % (type2.lstrip("'"), type1.lstrip("'")),
                    "'nil") == (True, True)
    assert subtypep("'t", "'(or %s (not %s))" % (type1.lstrip("'"),
                                                type2.lstrip("'"))) == (True, True)


def assert_disjoint(type1, type2):
    """The certain half of ansi-test's `check-disjointness`."""
    assert subtypep(type1, type2) == (False, True)
    assert subtypep(type2, type1) == (False, True)
    assert subtypep("'(and %s %s)" % (type1.lstrip("'"), type2.lstrip("'")),
                    "'nil") == (True, True)
    assert subtypep(type1, "'(not %s)" % type2.lstrip("'")) == (True, True)


class TestArity:
    """CLHS 4.3.4: two required arguments and an optional environment."""

    def test_optional_environment_is_accepted(self):
        assert subtypep("'bit", "'integer") == (True, True)
        assert ev("(multiple-value-list (subtypep 'bit 'integer nil))") is not None

    @pytest.mark.parametrize('form', [
        '(subtypep)',
        "(subtypep 'integer)",
        "(subtypep 't 't nil nil)",
    ])
    def test_wrong_arity_signals_program_error(self, form):
        """ansi-test `subtypep.error.1`/`.2`/`.3`.

        Asserted through HANDLER-CASE rather than `pytest.raises`, because the
        evaluator turns a raised Lisp error into a *signalled condition* -- so
        the Python exception that escapes is the evaluator's wrapper, and
        matching on it would be testing the wrapper rather than the behaviour a
        Lisp program can observe. A Python `TypeError` from a fixed Python
        signature would not be catchable as PROGRAM-ERROR here at all, which is
        why SUBTYPEP is registered with `*args` and checks the count itself.
        """
        assert ev('(handler-case %s (program-error () 42))' % form) == 42


class TestTheOldTableMisses:
    """The exact relations the string-pair table got confidently wrong."""

    @pytest.mark.parametrize('type1,type2', [
        ("'(integer 0 10)", "'integer"),
        ("'fixnum", "'integer"),
        ("'bignum", "'integer"),
        ("'bit", "'unsigned-byte"),
        ("'(mod 10)", "'integer"),
        ("'(unsigned-byte 8)", "'integer"),
        ("'ratio", "'rational"),
        ("'keyword", "'symbol"),
        ("'null", "'list"),
        ("'null", "'symbol"),
        ("'cons", "'list"),
        ("'standard-char", "'character"),
        ("'logical-pathname", "'pathname"),
        ("'file-stream", "'stream"),
        ("'compiled-function", "'function"),
        ("'simple-base-string", "'sequence"),
    ])
    def test_is_a_certain_subtype(self, type1, type2):
        assert subtypep(type1, type2) == (True, True)


class TestIntegerIntervals:
    """`subtypep-integer.lsp`: bounds, exclusivity and the fixnum split."""

    def test_all_four_spellings_of_unbounded_agree(self):
        for spelling in ["'(integer)", "'(integer *)", "'(integer * *)"]:
            assert_equivalent("'integer", spelling)

    def test_exclusive_integer_bounds_normalize_to_inclusive(self):
        """`(integer (9))` and `(integer 10)` are the *same* type, not merely
        related -- an integer interval has no room between 9 and 10."""
        assert_equivalent("'(integer (9))", "'(integer 10)")
        assert_equivalent("'(integer * (11))", "'(integer * 10)")
        assert subtypep("'(integer 0 10)", "'(integer 0 (10))") == (False, True)

    def test_wider_lower_bound_is_a_supertype(self):
        assert subtypep("'(integer 10)", "'(integer 5)") == (True, True)
        assert subtypep("'(integer 5)", "'(integer 10)") == (False, True)

    def test_fixnum_and_bignum_partition_integer(self):
        """ansi-test `subtypep.fixnum.integer` / `subtypep.bignum.integer`.

        This is also the assertion that pins FIXNUM's range to
        `MOST-POSITIVE-FIXNUM`: TYPEP once used 2**29 while the constant was
        2**63-1, so the two disagreed about the same integer.
        """
        assert_equivalent("'(or fixnum bignum)", "'integer")
        assert_disjoint("'fixnum", "'bignum")

    def test_fixnum_is_its_own_interval(self):
        from fclpy import typespec
        assert_equivalent(
            "'fixnum",
            "'(integer %d %d)" % (typespec.MOST_NEGATIVE_FIXNUM,
                                  typespec.MOST_POSITIVE_FIXNUM))

    def test_adjacent_intervals_merge(self):
        assert_equivalent("'(or (integer 0 5) (integer 6 10))", "'(integer 0 10)")

    def test_interval_union_and_intersection(self):
        assert_equivalent(
            "'(and (or (integer 0 10) (integer 20 30))"
            "      (or (integer 5 15) (integer 25 35)))",
            "'(or (integer 5 10) (integer 25 30))")

    def test_removing_a_point_splits_the_interval(self):
        assert_equivalent("'(and integer (not (eql 10)))",
                          "'(or (integer * 9) (integer 11 *))")
        assert_equivalent("'(and integer (not (integer 1 10)))",
                          "'(or (integer * 0) (integer 11 *))")


class TestNumericTowerCrossings:
    """Intersections *between* numeric types must reduce, not merely relate."""

    def test_integer_intersected_with_real(self):
        assert_equivalent("'(and integer (real 4 10))", "'(integer 4 10)")
        assert_equivalent("'(and (integer 4) (real * (10)))", "'(integer 4 9)")
        assert_equivalent("'(and (integer * 10) (real (4)))", "'(integer 5 10)")

    def test_a_ratio_bound_narrows_an_integer_type(self):
        """ansi-test `subtypep.real.9`: only one integer lies in (-1/2, 1/2)."""
        assert_equivalent("'(and integer (real -1/2 1/2))", "'(integer 0 0)")
        assert_equivalent("'(and integer (real -1/2 1/2))", "'(eql 0)")

    def test_real_bounds_are_indifferent_to_the_bound_type(self):
        assert_equivalent("'(real 0 10)", "'(real 0.0 10.0)")

    def test_interval_intersection_preserves_bracket_kind(self):
        assert_equivalent("'(and (real 0 (10)) (real (5) 15))", "'(real (5) (10))")

    def test_an_exclusive_bound_plus_its_endpoint_is_the_inclusive_one(self):
        assert_equivalent("'(or (real 0 0) (real (0)))", "'(real 0)")

    @pytest.mark.parametrize('type1,type2', [
        ("'integer", "'float"),
        ("'ratio", "'float"),
        ("'integer", "'ratio"),
        ("'complex", "'float"),
        ("'integer", "'complex"),
        ("'(unsigned-byte 8)", "'ratio"),
    ])
    def test_numeric_families_are_disjoint(self, type1, type2):
        assert_disjoint(type1, type2)

    def test_complex_is_a_number_but_not_conversely(self):
        assert subtypep("'complex", "'number") == (True, True)
        assert subtypep("'number", "'complex") == (False, True)


class TestFloats:
    """`subtypep-float.lsp`. fclpy has one float representation, so the four
    float names denote one type -- CLHS 12.1.4.4 permits that, and ansi-test
    discovers the choice rather than assuming otherwise. What must hold either
    way is the interval algebra within a format."""

    def test_float_subtypes_are_floats_and_reals(self):
        for name in ["'short-float", "'single-float", "'double-float", "'long-float"]:
            assert subtypep(name, "'float") == (True, True)
            assert subtypep(name, "'real") == (True, True)

    def test_exclusive_float_bounds(self):
        assert subtypep("'(single-float 0.0 (10.0))",
                        "'(single-float 0.0 10.0)") == (True, True)
        assert subtypep("'(single-float 0.0 10.0)",
                        "'(single-float 0.0 (10.0))") == (False, True)

    def test_float_interval_intersection(self):
        assert_equivalent("'(and (single-float 0.0 2.0) (single-float 1.0 3.0))",
                          "'(single-float 1.0 2.0)")

    def test_signed_zero_bounds_are_interchangeable(self):
        """ansi-test's signed-zero group. fclpy's floats are Python floats, in
        which `0.0 == -0.0`, so the two bounds name the same interval."""
        assert subtypep("'(single-float -0.0)", "'(single-float 0.0)") == (True, True)
        assert subtypep("'(single-float 0.0)", "'(single-float -0.0)") == (True, True)


class TestMemberAndEql:
    """`subtypep-member.lsp` / `subtypep-eql.lsp`."""

    def test_empty_member_is_the_empty_type(self):
        assert_equivalent("'(member)", "'nil")

    def test_member_is_order_insensitive(self):
        assert_equivalent("'(member a b c d)", "'(member c d b a)")

    def test_member_and_integer_intervals_interconvert(self):
        assert_equivalent("'(integer 2 5)", "'(member 2 5 4 3)")
        assert subtypep("'(member 10 20 30)", "'(integer 0 100)") == (True, True)
        assert subtypep("'(integer 3 6)", "'(member 0 1 2 3 4 5 6 7 8 100)") == (True, True)
        assert subtypep("'(integer 3 6)", "'(member 0 1 2 3 5 6 7 8)") == (False, True)

    def test_member_against_symbol(self):
        assert subtypep("'(member a b c d e)", "'symbol") == (True, True)
        assert subtypep("'(member a b 10 d e)", "'symbol") == (False, True)

    def test_boolean_is_exactly_nil_and_t(self):
        assert_equivalent("'boolean", "'(member nil t)")

    def test_null_is_a_member_of_a_symbol_list(self):
        assert subtypep("'null", "'(member a b nil c d e)") == (True, True)

    def test_complement_of_member_is_closed_under_intersection(self):
        assert_equivalent(
            "'(and (not (member b d e f g)) (not (member x y b z d)))",
            "'(not (member b d e f g x y z))")

    def test_removing_members_from_an_interval(self):
        assert_equivalent(
            "'(and (integer 0 30) (not (member 3 4 5 9 10 11 17 18 19)))",
            "'(or (integer 0 2) (integer 6 8) (integer 12 16) (integer 20 30))")

    def test_distinct_eql_types_are_disjoint(self):
        assert subtypep("'(and (eql a) (eql b))", "'nil") == (True, True)


class TestSatisfies:
    """`(satisfies f)` is undecidable in general and must stay so.

    ansi-test checks both directions of this. `subtypep.cons.44` builds a type
    from predicates that are literally `(= 1 (random 2))` and requires
    **NIL NIL**; `subtypep.member.27` requires a **certain T** for
    `(member a b c d)` against `(satisfies symbolp)`. Both follow from the same
    rule: an opaque literal is decided only when what remains of the type is a
    finite set of concrete objects, in which case the predicate is simply called
    on each of them.
    """

    def test_decided_against_a_finite_member_type(self):
        assert subtypep("'(member a b c d)", "'(satisfies symbolp)") == (True, True)
        assert subtypep("'(eql a)", "'(satisfies symbolp)") == (True, True)
        assert subtypep("'(and (member 1 6 10) (satisfies symbolp))", "'nil") == (True, True)

    def test_not_decided_against_an_infinite_type(self):
        assert subtypep("'cons", "'(satisfies my-undecidable-predicate)") == (False, False)

    def test_an_opaque_literal_still_cancels_against_itself(self):
        """`(and X X)` is `X` whatever X means, because the literal is compared
        by specifier rather than being guessed at."""
        assert_equivalent("'(and (satisfies foo) (satisfies foo))", "'(satisfies foo)")


class TestConsProducts:
    """`subtypep-cons.lsp`: cons types are rectangles, closed under difference."""

    def test_all_spellings_of_the_universal_cons_type_agree(self):
        for spelling in ["'(cons)", "'(cons *)", "'(cons * *)", "'(cons t)", "'(cons t t)"]:
            assert_equivalent("'cons", spelling)

    @pytest.mark.parametrize('spec', [
        "'(cons nil)", "'(cons nil *)", "'(cons nil t)",
        "'(cons * nil)", "'(cons t nil)", "'(cons nil nil)",
    ])
    def test_an_empty_component_empties_the_product(self, spec):
        """ansi-test `subtypep.cons.2`: the bottom must propagate out of the
        product rather than yielding a cons type with an impossible car."""
        assert subtypep(spec, "'nil") == (True, True)

    def test_componentwise_intersection(self):
        assert_equivalent("'(and (cons symbol *) (cons * symbol))",
                          "'(cons symbol symbol)")

    def test_de_morgan_on_a_product(self):
        """ansi-test `subtypep.cons.7` -- the rectangle-difference identity."""
        assert_equivalent("'(and cons (not (cons symbol symbol)))",
                          "'(or (cons (not symbol) *) (cons * (not symbol)))")

    def test_a_union_of_rectangles_covers_the_product(self):
        assert_equivalent(
            "'(or (cons integer symbol) (cons integer integer)"
            "     (cons symbol integer) (cons symbol symbol))",
            "'(cons (or integer symbol) (or integer symbol))")

    def test_a_partial_cover_is_certainly_not_a_supertype(self):
        assert subtypep("'(cons (or integer symbol) (or integer symbol))",
                        "'(or (cons integer symbol) (cons symbol integer))") == (False, True)

    def test_fixnum_bignum_split_inside_a_product(self):
        assert subtypep("'(cons integer single-float)",
                        "'(or (cons fixnum single-float)"
                        "     (cons bignum single-float))") == (True, True)

    def test_cons_and_symbol_are_disjoint(self):
        assert_disjoint("'cons", "'symbol")


class TestArrays:
    """`subtypep-array.lsp`. The definitional identities are not special cases:
    every array name reduces to the same (simplicity, upgraded element type,
    dimensions) triple, so they fall out."""

    @pytest.mark.parametrize('name,equivalent', [
        ("'vector", "'(array * (*))"),
        ("'(vector * 17)", "'(array * (17))"),
        ("'simple-vector", "'(simple-array t (*))"),
        ("'(simple-vector 17)", "'(simple-array t (17))"),
        ("'base-string", "'(vector base-char)"),
        ("'(base-string 17)", "'(vector base-char 17)"),
    ])
    def test_definitional_identities(self, name, equivalent):
        assert_equivalent(name, equivalent)

    def test_all_spellings_of_unspecialized_array_agree(self):
        for spelling in ["'(array)", "'(array *)", "'(array * *)"]:
            assert_equivalent("'array", spelling)

    def test_dimension_wise_intersection(self):
        assert_equivalent(
            "'(and (array t (* 10 * * *)) (array t (* * * 29 *)))",
            "'(array t (* 10 * 29 *))")

    def test_nil_dimensions_mean_rank_zero_not_wildcard(self):
        """`(array t nil)` is the rank-0 arrays. Reading NIL as `*` would make
        it a supertype of every array (ansi-test `subtypep.array.10`)."""
        assert subtypep("'(array t nil)", "'(array t (*))") == (False, True)
        assert subtypep("'(array t nil)", "'(array t 1)") == (False, True)
        assert subtypep("'(array t nil)", "'array") == (True, True)

    def test_distinct_ranks_are_never_subtypes(self):
        assert subtypep("'(array t 1)", "'(array t 2)") == (False, True)
        assert subtypep("'(array t 2)", "'(array t 1)") == (False, True)

    def test_distinct_element_types_are_disjoint(self):
        assert_disjoint("'(array bit)", "'(array character)")

    def test_a_vector_is_an_array_but_not_conversely(self):
        assert subtypep("'vector", "'array") == (True, True)
        assert subtypep("'array", "'vector") == (False, True)

    @pytest.mark.parametrize('spec', [
        "'string", "'base-string", "'simple-string",
        "'bit-vector", "'simple-bit-vector",
    ])
    def test_specialized_vectors_are_not_simple_vectors(self, spec):
        """`simple-vector` is `(simple-array t (*))`, so a specialized element
        type excludes it -- ansi-test asserts each of these as `nil t`."""
        assert subtypep(spec, "'simple-vector") == (False, True)


class TestBooleanAlgebra:
    """`subtypep.lsp`'s AND/OR laws, and the mixed-lattice reductions."""

    def test_degenerate_connectives(self):
        assert_equivalent("'(and)", "'t")
        assert_equivalent("'(or)", "'nil")

    @pytest.mark.parametrize('spec', ["'symbol", "'(integer 0 10)", "'cons", "'character"])
    def test_idempotence_and_identity(self, spec):
        bare = spec.lstrip("'")
        assert_equivalent(spec, "'(and %s %s)" % (bare, bare))
        assert_equivalent(spec, "'(or %s %s)" % (bare, bare))
        assert_equivalent(spec, "'(and %s)" % bare)
        assert_equivalent(spec, "'(or %s)" % bare)

    def test_distribution_across_sorts(self):
        assert_equivalent(
            "'(and (or symbol (integer 0 15)) (or symbol (integer 10 25)))",
            "'(or symbol (integer 10 15))")

    def test_a_negated_sort_cancels(self):
        assert_equivalent(
            "'(and (or (not symbol) (integer 0 10)) (or symbol (integer 11 25)))",
            "'(integer 11 25)")

    def test_atom_is_the_complement_of_cons(self):
        assert_equivalent("'atom", "'(not cons)")

    @pytest.mark.parametrize('spec', [
        "'integer", "'symbol", "'cons", "'character", "'bignum",
        "'hash-table", "'(integer 0 100)", "'package", "'function",
    ])
    def test_nothing_but_nil_is_a_subtype_of_nil(self, spec):
        """ansi-test `subtypep.nil.1`: each of these types is nonempty, so a
        decision procedure must say so rather than shrugging."""
        assert subtypep(spec, "'nil") == (False, True)

    @pytest.mark.parametrize('spec', [
        "'integer", "'symbol", "'cons", "'character", "'hash-table", "'t",
    ])
    def test_everything_is_a_subtype_of_t(self, spec):
        assert subtypep(spec, "'t") == (True, True)


class TestClassesAsTypeSpecifiers:
    """A class *object* is a type specifier (ansi-test `subtypep.array.1` passes
    `(find-class 'array)` directly, and every CL class name must be
    type-equivalent to its class object).

    These also pin the fix for `TypeError: unhashable type: 'LispClass'`, which
    a first version of the lattice surfaced as the *value* of the Lisp form
    because class cells went into a `frozenset` as raw objects.
    """

    def test_a_builtin_class_object_equals_its_name(self):
        assert ev("(multiple-value-list (subtypep (find-class 'array) 'array))") is not None
        assert subtypep("(find-class 'array)", "'array") == (True, True)
        assert subtypep("'array", "(find-class 'array)") == (True, True)

    def test_a_class_object_is_a_subtype_of_t(self):
        assert subtypep("(find-class 'array)", "'t") == (True, True)

    def test_user_classes_relate_through_their_superclasses(self):
        ev("(defclass tsp-a () ())")
        ev("(defclass tsp-b (tsp-a) ())")
        assert subtypep("'tsp-b", "'tsp-a") == (True, True)
        assert subtypep("'tsp-a", "'tsp-b") == (False, True)
        assert subtypep("'tsp-b", "'t") == (True, True)

    def test_unrelated_user_classes_are_disjoint(self):
        ev("(defclass tsp-c () ())")
        ev("(defclass tsp-d () ())")
        assert subtypep("'(and tsp-c tsp-d)", "'nil") == (True, True)

    def test_condition_types_relate_through_python_inheritance(self):
        assert subtypep("'simple-error", "'error") == (True, True)
        assert subtypep("'error", "'condition") == (True, True)
        assert subtypep("'error", "'simple-error") == (False, True)


class TestDeftypeIsVisibleToTheTypeSystem:
    """DEFTYPE used to write its expander into `global_env.user_types` and
    *nothing ever read that dict*, so `(deftype foo () '(integer 0 10))`
    succeeded and then `(subtypep 'foo 'integer)` knew nothing about it.

    A deftype lambda list is a macro lambda list except that an omitted
    &OPTIONAL/&KEY parameter defaults to `*` rather than NIL (CLHS 4.2.3), which
    is why DEFTYPE shares the one macro-lambda-list binder instead of getting a
    seventh copy of it.
    """

    def test_a_simple_deftype_is_expanded(self):
        ev("(deftype tsp-small () '(integer 0 10))")
        assert_equivalent("'tsp-small", "'(integer 0 10)")

    def test_an_omitted_optional_parameter_defaults_to_star(self):
        """ansi-test `deftype.9`: bare, this names `(integer 0 *)`."""
        ev("(deftype tsp-ub (&optional x) `(integer 0 ,x))")
        assert_equivalent("'(tsp-ub)", "'unsigned-byte")
        assert_equivalent("'(tsp-ub 4)", "'(integer 0 4)")

    def test_an_optional_parameter_with_a_default(self):
        """ansi-test `deftype.10`."""
        ev("(deftype tsp-def (&optional (x 14)) `(integer 0 ,x))")
        assert_equivalent("'(tsp-def)", "'(integer 0 14)")
        assert_equivalent("'(tsp-def 4)", "'(integer 0 4)")

    def test_keyword_parameters(self):
        """ansi-test `deftype.11`."""
        ev("(deftype tsp-key (&key foo bar) `(integer ,foo ,bar))")
        assert_equivalent("'(tsp-key)", "'integer")
        assert_equivalent("'(tsp-key :foo 3)", "'(integer 3)")
        assert_equivalent("'(tsp-key :bar 10)", "'(integer * 10)")

    def test_rest_parameters_and_the_empty_type(self):
        """ansi-test `deftype.13`."""
        ev("(deftype tsp-rest (&rest args) (if args `(member ,@args) nil))")
        assert_equivalent("'(tsp-rest)", "'nil")
        assert_equivalent("'(tsp-rest a b)", "'(member a b)")

    def test_an_empty_body_denotes_nil(self):
        """ansi-test `deftype.18`."""
        ev("(deftype tsp-empty ())")
        assert_equivalent("'tsp-empty", "'nil")

    def test_return_from_in_the_expander(self):
        """ansi-test `deftype.16`: the body is an implicit BLOCK named for the
        type, which comes free from sharing the macro binder."""
        ev("(deftype tsp-ret () (return-from tsp-ret 'integer))")
        assert_equivalent("'tsp-ret", "'integer")

    def test_only_the_primary_value_of_the_expander_is_the_type(self):
        """ansi-test `deftype.17`."""
        ev("(deftype tsp-mv () (values 'integer t))")
        assert_equivalent("'tsp-mv", "'integer")

    def test_a_declaration_in_the_expander_body_is_tolerated(self):
        """ansi-test `deftype.19`."""
        ev("(deftype tsp-decl () (declare (optimize speed)) 'integer)")
        assert_equivalent("'tsp-decl", "'integer")


class TestUndecidableDegradesRatherThanLying:
    """Standing rule 4, in the one place ANSI explicitly allows a shrug.

    CLHS 4.3.4 lets SUBTYPEP answer `NIL NIL`. What it must never do is answer
    `NIL T` -- a confident No -- for something it simply failed to interpret,
    which is exactly what the old table did on every lookup miss.
    """

    def test_an_unknown_type_name_is_undecided_not_denied(self):
        assert subtypep("'tsp-no-such-type", "'integer") == (False, False)
        assert subtypep("'integer", "'tsp-no-such-type") == (False, False)

    def test_a_malformed_interval_bound_is_undecided(self):
        """The bound is a symbol, not a number. Handing it to `math.floor` would
        surface a Python `TypeError` as the value of the form."""
        assert subtypep("'(integer most-positive-fixnum)", "'integer") == (False, False)
