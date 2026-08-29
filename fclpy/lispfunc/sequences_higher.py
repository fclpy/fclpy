"""Higher-order sequence operations, arrays, and set operations."""

from .core import cons, car, cdr, atom
from . import registry as _registry
import fclpy.lisptype as lisptype
from .arrays import make_array, LispArray
from .sequences_search import (
    _make_matcher, _coerce_function_designator, _lisp_truthy, _call_checked,
)
from .sequence_protocol import (
    seq_elements as _cons_to_list, bounding_indices, make_lisp_list, build_sequence,
    seq_set, seq_length, list_cells, list_elements,
)

# `_cons_to_list` is the protocol's element access under this module's old
# name. The copy it replaced ended in `return [seq]`, so an unrecognized
# sequence -- an `AdjustableVector`, a `LispString` -- became a one-element
# list of itself instead of its elements: REDUCE over a vector "reduced" to
# the vector, and EVERY over a vector tested the vector as a single element
# (plan.md Finding M).


def _matcher_contains(matcher, item, seq):
    """True if `item` matches some element of `seq` under `matcher`."""
    return any(matcher(item, x) for x in seq)


# Association list operations
@_registry.cl_function('ADJOIN')
def adjoin(x, seq, *, test=None, test_not=None, key=None):
    """Tests whether item is the same as an existing element of list.

    Supports :test/:test-not/:key like every other CLHS "two-argument
    test" sequence function (default is eql-like equality); previously
    ignored :key entirely and hardcoded `is` for :test. `key_item=True`
    because CLHS 14.2 says ADJOIN applies :key to `x` as well as to each
    element -- unlike MEMBER/FIND, where :key only ever applies to
    elements. PUSHNEW (`evaluation_special_forms.eval_pushnew`) is defined
    directly in terms of this function. No trailing `**kwargs`: that used
    to silently swallow any other keyword (recognized or not), which is
    the opposite of CLHS 3.4.1.4 -- `split_keyword_args` now owns rejecting
    an unrecognized one unless :allow-other-keys is true.
    """
    matcher = _make_matcher(test=test, test_not=test_not, key=key, key_item=True)
    seq_list = list_elements(seq, 'ADJOIN')
    return seq if _matcher_contains(matcher, x, seq_list) else cons(x, seq)


@_registry.cl_function('PAIRLIS')
def pairlis(keys, data, alist=None):
    """Create an alist from keys and data (CLHS 14.2).

    An association is a *cons*, not a Python tuple, and the alist is a Lisp
    list -- the pairs used to be tuples inside a Python list, so the result
    printed as `#(#(A 1))` and no ASSOC could look anything up in it.
    """
    result = alist if alist is not None else lisptype.NIL
    pairs = list(zip(list_elements(keys, 'PAIRLIS'),
                     list_elements(data, 'PAIRLIS')))
    for key, datum in reversed(pairs):
        result = lisptype.lispCons(lisptype.lispCons(key, datum), result)
    return result


@_registry.cl_function('ACONS')
def acons(key, datum, alist):
    """Add a key/datum association to the front of an alist (CLHS 14.2)."""
    return lisptype.lispCons(lisptype.lispCons(key, datum),
                             alist if alist is not None else lisptype.NIL)


def _parallel_elements(sequences, what):
    """Element lists for the `&rest sequences` of a parallel-mapping operator,
    truncated to the shortest, per CLHS 17.2."""
    columns = [_cons_to_list(seq, what) for seq in sequences]
    if not columns:
        return []
    limit = min(len(column) for column in columns)
    return [[column[i] for column in columns] for i in range(limit)]


def _require_lists(lists, what):
    """The MAP* family's lambda list is `(function &rest lists+)`.

    At least one list is *required*, so `(mapcar #'append)` is a PROGRAM-ERROR
    (`mapcar.error.3`, and the same test in all six of these files). With
    `*lists` there is no arity for Python to check, and returning NIL for no
    lists answered the call instead of rejecting it.
    """
    if not lists:
        raise lisptype.LispProgramError(
            f"{what}: at least one list argument is required")
    return lists


def _parallel_list_elements(lists, what):
    """`_parallel_elements` for the MAP*CAR* family, whose arguments are
    *lists* rather than sequences (CLHS 14.2).

    The distinction is observable: `mapcar.error.1` and its eight siblings
    assert a TYPE-ERROR for every argument failing `listp`, so a string or a
    vector must be refused here even though `seq_elements` would happily read
    it -- MAPCAR is not MAP.
    """
    columns = [list_elements(lst, what) for lst in _require_lists(lists, what)]
    limit = min(len(column) for column in columns)
    return [[column[i] for column in columns] for i in range(limit)]


def _check_function_designator_type(value, what):
    """CLHS 14.2: a function designator is a SYMBOL or a FUNCTION.

    EVERY/SOME accept a function designator and CLHS requires a TYPE-ERROR (not
    a PROGRAM-ERROR) for one of the wrong type, e.g. `(every 1 '(a b c))` --
    1 is an INTEGER, not a function designator. The default ``coerce_to_function``
    raises a PROGRAM-ERROR ("not callable"), which used to fail `every.error.1`
    and `some.error.1` (and the matching `notany`/`notevery` ERROR.1 tests).
    Symbols are still resolved through the function namespace by the caller;
    this check only rejects things that are neither callable nor a symbol.
    """
    if value is None or value is lisptype.NIL:
        # NIL is the "no function" designator for OTHER sequence operators
        # (FIND/SORT :test-not nil), but EVERY/SOME always require a real
        # designator -- they have no "no predicate" default. So a bare NIL
        # here is just the wrong type, not "default", and is signalled.
        pass
    elif isinstance(value, lisptype.LispSymbol):
        return
    elif callable(value):
        return
    raise lisptype.LispTypeError(
        f"{what}: {value!r} is not a function designator",
        expected_type="FUNCTION", actual_value=value)


# Predicate tests on sequences
@_registry.cl_function('EVERY')
def every(predicate, *sequences):
    """True if the predicate holds for every set of corresponding elements (CLHS 17.3).

    The lambda list is ``(function &rest sequences+)``: at least one sequence
    is required, so ``(every #'null)`` is a PROGRAM-ERROR (``every.error.9``)
    rather than vacuously T. The predicate is a function *designator*; testing
    it with a bare `if` made a returned NIL -- a Python-truthy object -- count
    as true. The check is split into a type check (TYPE-ERROR for a non-designator
    like ``(every 1 ...)``) and the callable resolution, so ``every.error.1``/
    ``.10`` and the matching SOME/NOTANY/NOTEVERY tests pass.
    """
    if not sequences:
        raise lisptype.LispProgramError(
            "EVERY: at least one sequence argument is required")
    _check_function_designator_type(predicate, 'EVERY')
    predicate = _coerce_function_designator(predicate)
    for args in _parallel_elements(sequences, 'EVERY'):
        if not _lisp_truthy(predicate(*args)):
            return lisptype.NIL
    return lisptype.T


@_registry.cl_function('SOME')
def some(predicate, *sequences):
    """The first true value the predicate returns, or NIL (CLHS 17.3).

    SOME returns the *value* of the predicate, not T. Like EVERY, the lambda
    list is ``(function &rest sequences+)`` -- ``(some #'null)`` is a
    PROGRAM-ERROR (``some.error.9``), and a non-designator predicate raises
    a TYPE-ERROR (``some.error.1``/``.10``)."""
    if not sequences:
        raise lisptype.LispProgramError(
            "SOME: at least one sequence argument is required")
    _check_function_designator_type(predicate, 'SOME')
    predicate = _coerce_function_designator(predicate)
    for args in _parallel_elements(sequences, 'SOME'):
        value = predicate(*args)
        if _lisp_truthy(value):
            return value
    return lisptype.NIL


@_registry.cl_function('NOTEVERY')
def notevery(predicate, *sequences):
    """True if the predicate is false for some element (CLHS 17.3)."""
    if not sequences:
        raise lisptype.LispProgramError(
            "NOTEVERY: at least one sequence argument is required")
    _check_function_designator_type(predicate, 'NOTEVERY')
    return lisptype.lisp_bool(not _lisp_truthy(every(predicate, *sequences)))


@_registry.cl_function('NOTANY')
def notany(predicate, *sequences):
    """True if the predicate is false for every element (CLHS 17.3)."""
    if not sequences:
        raise lisptype.LispProgramError(
            "NOTANY: at least one sequence argument is required")
    _check_function_designator_type(predicate, 'NOTANY')
    return lisptype.lisp_bool(not _lisp_truthy(some(predicate, *sequences)))


# Mapping operations
@_registry.cl_function('MAP')
def map_fn(result_type, function, *sequences):
    """Map `function` over corresponding elements, building a `result_type`
    sequence (CLHS 17.3).

    `result_type` is a full sequence type specifier, resolved by the shared
    protocol. It used to be compared against the Python string `'LIST'`, so
    `(map 'string #'char-upcase "abc")` fell into an `else` that returned the
    Python list of results -- a vector -- and every non-list request was
    wrong. A NIL specifier means "for effect", and MAP then returns NIL.

    The lambda list is ``(result-type function &rest sequences+)``: at least
    one sequence is required, so ``(map 'list #'null)`` is a PROGRAM-ERROR
    (``map.error.6``). The function is a function *designator*; a
    non-designator raises a TYPE-ERROR (the same rule EVERY/SOME use).
    """
    if not sequences:
        raise lisptype.LispProgramError(
            "MAP: at least one sequence argument is required")
    _check_function_designator_type(function, 'MAP')
    function = _coerce_function_designator(function)
    results = [function(*args) for args in _parallel_elements(sequences, 'MAP')]
    if result_type is None or result_type is lisptype.NIL:
        # CLHS: MAP with a NIL result type calls the function for effect and
        # returns NIL -- unlike MAKE-SEQUENCE/CONCATENATE, for which NIL is
        # the *type* NIL and a non-empty result is an error.
        return lisptype.NIL
    return build_sequence(result_type, results, 'MAP')


@_registry.cl_function('MAP-INTO')
def map_into(result_sequence, function, *sequences):
    """Map `function` into an existing sequence, destructively (CLHS 17.3).

    Writes through the protocol's element store, so the destination may be a
    list, a vector or a string. With no source sequences the function is
    called once per element of the destination.

    For sequences with a fill pointer, updates the fill pointer to the number
    of elements written.
    """
    function = _coerce_function_designator(function)

    # Determine the capacity: for arrays with fill pointers, use the actual
    # capacity, not the fill-pointer-respecting length
    capacity = seq_length(result_sequence, 'MAP-INTO')
    if isinstance(result_sequence, LispArray) and result_sequence.fill_pointer is not None:
        # Use the total_size (capacity) rather than fill-pointer-respecting length
        capacity = result_sequence.total_size
    elif isinstance(result_sequence, lisptype.LispString) and result_sequence.fill_pointer is not None:
        # Use the backing storage length, not the fill-pointer-respecting length
        capacity = len(result_sequence._data)

    # Determine how many elements to map
    if sequences:
        rows = _parallel_elements(sequences, 'MAP-INTO')[:capacity]
    else:
        rows = [[] for _ in range(capacity)]

    # Map elements. For sequences with a fill pointer, we need to write directly
    # to the underlying storage to avoid hitting the fill-pointer-respecting access.
    if isinstance(result_sequence, LispArray) and result_sequence.fill_pointer is not None:
        # Write directly to the array's data storage
        for index, args in enumerate(rows):
            result_sequence.row_major_set(index, function(*args))
    elif isinstance(result_sequence, lisptype.LispString) and result_sequence.fill_pointer is not None:
        # Write directly to the string's underlying data storage, converting Characters to str
        for index, args in enumerate(rows):
            value = function(*args)
            if isinstance(value, lisptype.Character):
                value = value.char
            result_sequence._data[index] = value
    else:
        # Use seq_set for other sequences
        for index, args in enumerate(rows):
            seq_set(result_sequence, index, function(*args), 'MAP-INTO')

    # Update fill pointer if the sequence has one
    if isinstance(result_sequence, LispArray) and result_sequence.fill_pointer is not None:
        result_sequence.fill_pointer = len(rows)
    elif isinstance(result_sequence, lisptype.LispString) and result_sequence.fill_pointer is not None:
        result_sequence.fill_pointer = len(rows)

    return result_sequence


@_registry.cl_function('MAPCAR')
def mapcar(function, *lists):
    """Map over successive elements of lists, collecting results (CLHS 14.2)."""
    function = _coerce_function_designator(function)
    return make_lisp_list([function(*args)
                      for args in _parallel_list_elements(lists, 'MAPCAR')])


def _nconc_results(results, what):
    """Splice the results of MAPCAN/MAPCON together.

    CLHS 14.2 defines these as `(apply #'nconc (mapcar ...))`, so they *are*
    NCONC and must not be a second implementation of it. Folding the results
    into one element list here -- what this used to do -- got the last one
    wrong in both directions: `(mapcan (constantly 1) '(a))` is `1`, not `(1)`
    (`mapcan.11`), because NCONC never traverses its final argument.
    """
    from .sequences_compose import nconc
    return nconc(*results) if results else lisptype.NIL


@_registry.cl_function('MAPCAN')
def mapcan(function, *lists):
    """MAPCAR, with the results spliced together (CLHS 14.2)."""
    function = _coerce_function_designator(function)
    return _nconc_results(
        [function(*args) for args in _parallel_list_elements(lists, 'MAPCAN')],
        'MAPCAN')


@_registry.cl_function('MAPC')
def mapc(function, *lists):
    """Map for side effects, returning the first list (CLHS 14.2)."""
    function = _coerce_function_designator(function)
    for args in _parallel_list_elements(lists, 'MAPC'):
        function(*args)
    return lists[0] if lists else lisptype.NIL


def _successive_tails(lists, what):
    """The successive *tails* (`cdr`s) MAPLIST/MAPCON/MAPL iterate over.

    CLHS 14.2 distinguishes the `-CAR` family, which passes elements, from
    the `-LIST` family, which passes the sublists themselves. MAPLIST/MAPCON/
    MAPL were aliases of MAPCAR/MAPCAN/MAPC here, i.e. the distinction did not
    exist: `(maplist #'list '(1 2))` answered `((1) (2))` instead of
    `(((1 2)) ((2)))`.

    The arguments are lists, so a non-list one is a TYPE-ERROR rather than a
    loop that stops immediately -- `(maplist #'identity 1)` answered NIL, since
    `1` merely failed the `isinstance` guard on the first pass.
    """
    walkers = [list_cells(lst, what) for lst in _require_lists(lists, what)]
    rows = []
    while True:
        row = []
        for walker in walkers:
            cell = next(walker, None)
            if cell is None:
                return rows
            row.append(cell)
        rows.append(row)


@_registry.cl_function('MAPCON')
def mapcon(function, *lists):
    """MAPLIST, with the results spliced together (CLHS 14.2)."""
    function = _coerce_function_designator(function)
    return _nconc_results(
        [function(*args) for args in _successive_tails(lists, 'MAPCON')],
        'MAPCON')


@_registry.cl_function('MAPLIST')
def maplist(function, *lists):
    """Map over successive tails of lists, collecting results (CLHS 14.2)."""
    function = _coerce_function_designator(function)
    return make_lisp_list([function(*args)
                      for args in _successive_tails(lists, 'MAPLIST')])


@_registry.cl_function('MAPL')
def mapl(function, *lists):
    """MAPLIST for side effects, returning the first list (CLHS 14.2)."""
    function = _coerce_function_designator(function)
    for args in _successive_tails(lists, 'MAPL'):
        function(*args)
    return lists[0] if lists else lisptype.NIL


@_registry.cl_function('REDUCE')
def reduce_fn(function, sequence, *, key=None, from_end=None, start=None, end=None,
              initial_value=None, allow_other_keys=None, **other_keys):
    """Reduce sequence using function.

    Args:
        function: The function to apply
        sequence: The sequence to reduce
        key: Optional key function (applied to elements before combining)
        from_end: If true, reduce from right to left
        start: Starting index (default 0)
        end: Ending index (default length)
        initial_value: Initial value for accumulator
    """
    # Validate keywords
    from .arrays import _check_other_keys as _check_reduce_other_keys
    _check_reduce_other_keys(other_keys, allow_other_keys, 'REDUCE')

    has_initial = initial_value is not None

    function = _coerce_function_designator(function)
    key = _coerce_function_designator(key)

    py_seq = _cons_to_list(sequence, 'REDUCE')
    start, end = bounding_indices(len(py_seq), start, end, 'REDUCE')
    py_seq = py_seq[start:end]
    if key is not None:
        # Reduce's :key is called in a single-value context, so reduce the
        # result of each call to its primary value (the same way FIND/COUNT
        # and the other :key sites do via `_call_checked`). Without this a key
        # like `#'(lambda (x) (floor x 2))` returned a `MultipleValues`
        # wrapper and the reduction compared wrappers.
        py_seq = [lisptype.primary_value(_call_checked(key, item, caller_name=':KEY'))
                  for item in py_seq]

    if not py_seq:
        # CLHS 17.3: with no elements and no :initial-value, the function is
        # called with no arguments.
        return initial_value if has_initial else function()

    if from_end is not None and _lisp_truthy(from_end):
        # :from-end folds right, and the accumulated value is the *second*
        # argument: (f e1 (f e2 init)).
        result = initial_value if has_initial else py_seq[-1]
        rest = py_seq if has_initial else py_seq[:-1]
        for item in reversed(rest):
            result = function(item, result)
        return result

    result = initial_value if has_initial else py_seq[0]
    rest = py_seq if has_initial else py_seq[1:]
    for item in rest:
        result = function(result, item)
    return result


def _finish_list(pylist):
    """Return the elements collected by a set operation as a Lisp **list**.

    This used to return the Python list verbatim. A Python list is this
    implementation's *vector*, so every set operation answered a vector:
    `(union '(1 2) '(2 3))` printed as `#(1 2 3)` and `(listp (union ...))`
    was NIL -- eleven operators, one wrong result type (plan.md C5).
    """
    return make_lisp_list(pylist)


# Set operations. Each takes the shared :test/:test-not/:key triple as
# named parameters -- not a `**kwargs` catch-all -- so `split_keyword_args`
# (evaluation_core.py) can validate them itself the same way it does for
# MEMBER/ASSOC/etc: an unrecognized keyword is a PROGRAM-ERROR unless
# :allow-other-keys is true (CLHS 3.4.1.4). A `**kwargs` tail used to
# swallow every keyword silently -- recognized or not, allowed or not --
# which is what made `(union nil nil :bad t)` return an answer instead of
# signalling (plan.md C5/X2/X3, and standing rule 4: no silent accept).
#
# `key_item=True`: CLHS 14.2 applies :key to elements of *both* lists for
# every one of these -- unlike MEMBER/FIND, where a lone search item is
# compared as-is against keyed candidates, a set operation compares two
# list *elements* to each other, so :key must transform both sides.
# `_make_matcher`'s default (`key_item=False`) left the search-item side
# raw, so `(set-exclusive-or '((a . 1) (b . 2)) '((a . 10)) :key #'car)`
# compared a whole `(A . 1)` pair against the keyed `A` and never matched
# anything -- `set-exclusive-or.14` et al.
# The argument *order* of the test call is observable, and it is fixed: an
# element of `list-1` is always the first argument and an element of `list-2`
# always the second. `union.28`-`.31` (and the same four in `nunion.lsp`,
# `set-exclusive-or.lsp` and `nset-exclusive-or.lsp`) pass a test that
# `RETURN-FROM`s out of the whole form the first time it is called the other
# way round, so a reversed call is not a wrong answer but a detected one.
# UNION/NUNION reversed it, because they iterated `list2` and asked whether
# each of its elements was already present in the accumulated `list1` --
# which is the right *algorithm* and the wrong *call*.
def _set_operands(list1, list2, what, test=None, test_not=None, key=None):
    """The `(elements-1, elements-2, contains)` every set operation shares.

    `contains(elements, x, first)` answers whether `x` matches some element of
    `elements`, calling the test with `x` in whichever position it occupies in
    the original call.
    """
    matcher = _make_matcher(test=test, test_not=test_not, key=key, key_item=True)
    elements1 = list_elements(list1, what)
    elements2 = list_elements(list2, what)

    def contains(elements, x, from_list1):
        return any(matcher(x, y) if from_list1 else matcher(y, x)
                   for y in elements)

    return elements1, elements2, contains


@_registry.cl_function('INTERSECTION')
def intersection(list1, list2, *, test=None, test_not=None, key=None):
    """The elements of `list1` that also appear in `list2` (CLHS 14.2)."""
    left, right, contains = _set_operands(
        list1, list2, 'INTERSECTION', test, test_not, key)
    return _finish_list([x for x in left if contains(right, x, True)])


@_registry.cl_function('NINTERSECTION')
def nintersection(list1, list2, *, test=None, test_not=None, key=None):
    """INTERSECTION, permitted to destroy `list1` (CLHS 14.2)."""
    return intersection(list1, list2, test=test, test_not=test_not, key=key)


@_registry.cl_function('UNION')
def union(list1, list2, *, test=None, test_not=None, key=None):
    """Every element of `list1`, plus those of `list2` not already in it."""
    left, right, contains = _set_operands(
        list1, list2, 'UNION', test, test_not, key)
    return _finish_list(
        left + [y for y in right if not contains(left, y, False)])


@_registry.cl_function('NUNION')
def nunion(list1, list2, *, test=None, test_not=None, key=None):
    """UNION, permitted to destroy both arguments (CLHS 14.2)."""
    return union(list1, list2, test=test, test_not=test_not, key=key)


@_registry.cl_function('SET-DIFFERENCE')
def set_difference(list1, list2, *, test=None, test_not=None, key=None):
    """The elements of `list1` that do not appear in `list2` (CLHS 14.2)."""
    left, right, contains = _set_operands(
        list1, list2, 'SET-DIFFERENCE', test, test_not, key)
    return _finish_list([x for x in left if not contains(right, x, True)])


@_registry.cl_function('NSET-DIFFERENCE')
def nset_difference(list1, list2, *, test=None, test_not=None, key=None):
    """SET-DIFFERENCE, permitted to destroy `list1` (CLHS 14.2)."""
    return set_difference(list1, list2, test=test, test_not=test_not, key=key)


@_registry.cl_function('SET-EXCLUSIVE-OR')
def set_exclusive_or(list1, list2, *, test=None, test_not=None, key=None):
    """The elements appearing in exactly one of the two lists (CLHS 14.2)."""
    left, right, contains = _set_operands(
        list1, list2, 'SET-EXCLUSIVE-OR', test, test_not, key)
    return _finish_list(
        [x for x in left if not contains(right, x, True)]
        + [y for y in right if not contains(left, y, False)]
    )


@_registry.cl_function('NSET-EXCLUSIVE-OR')
def nset_exclusive_or(list1, list2, *, test=None, test_not=None, key=None):
    """SET-EXCLUSIVE-OR, permitted to destroy both arguments (CLHS 14.2)."""
    return set_exclusive_or(list1, list2, test=test, test_not=test_not, key=key)


@_registry.cl_function('SUBSETP')
def subsetp(subset, set_arg, *, test=None, test_not=None, key=None):
    """True if every element of `subset` appears in `set_arg` (CLHS 14.2)."""
    left, right, contains = _set_operands(
        subset, set_arg, 'SUBSETP', test, test_not, key)
    return lisptype.lisp_bool(
        all(contains(right, x, True) for x in left))


# PUSH, POP and PUSHNEW are special forms (`evaluation_special_forms.py`'s
# `eval_push`/`eval_pop`/`eval_pushnew`, registered `cl_special` in
# evaluation_special_registrations.py) -- `place` is a place designator,
# not a value, so it must reach them unevaluated. Registering them here too
# as `cl_function`s over Python lists was a second, competing
# implementation (plan.md standing rule 3): dead for PUSH/POP, since a
# special-form registration always wins dispatch, and *live but wrong* for
# PUSHNEW, which had no special-form registration and so actually ran this
# copy -- ignoring :test/:key/:test-not entirely and only ever working when
# `place` was already a plain Python list (plan.md C16).


# The array operators that used to live here -- AREF, SVREF, VECTOR, VECTORP,
# ARRAYP, ARRAY-DIMENSION(S), ARRAY-IN-BOUNDS-P, ARRAY-DISPLACEMENT,
# ADJUST-ARRAY, SIMPLE-VECTOR-P, BIT-VECTOR-P, VECTOR-PUSH/-EXTEND/-POP --
# were a second implementation of the array model, competing with vectors.py
# for the same registry names; import order decided which one ran, and this
# one won while knowing nothing about fill pointers, ranks or element types.
# They live in arrays.py now, once (standing rule 3).


# Lisp symbol for LIST*
@_registry.cl_function('LIST*')
def list_s_star_(*args):
    """LIST* function - creates a dotted list."""
    if not args:
        return None
    if len(args) == 1:
        return args[0]
    return cons(args[0], list_s_star_(*args[1:]))


__all__ = [
    # Association list operations
    'adjoin', 'pairlis', 'acons',
    # Predicate tests
    'every', 'some', 'notevery', 'notany',
    # Mapping operations
    'map_fn', 'map_into', 'mapcar', 'mapcan', 'mapc', 'mapcon', 'maplist', 'mapl',
    'reduce_fn',
    # Set operations
    'intersection', 'union', 'nunion', 'set_difference', 'nset_difference',
    'set_exclusive_or', 'nset_exclusive_or', 'subsetp', 'nintersection',
    # Symbol-safe names
    'list_s_star_',
]
