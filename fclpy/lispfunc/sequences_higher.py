"""Higher-order sequence operations, arrays, and set operations."""

from .core import cons, car, cdr, atom
from . import registry as _registry
import fclpy.lisptype as lisptype
from .arrays import make_array
from .sequences_search import (
    _make_matcher, _coerce_function_designator, _lisp_truthy,
)
from .sequence_protocol import (
    seq_elements as _cons_to_list, bounding_indices, make_lisp_list, build_sequence,
    seq_set, seq_length,
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
def adjoin(x, seq, test=None, test_not=None, key=None):
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
    seq_list = _cons_to_list(seq)
    return seq if _matcher_contains(matcher, x, seq_list) else cons(x, seq)


@_registry.cl_function('PAIRLIS')
def pairlis(keys, data, alist=None):
    """Create an alist from keys and data (CLHS 14.2).

    An association is a *cons*, not a Python tuple, and the alist is a Lisp
    list -- the pairs used to be tuples inside a Python list, so the result
    printed as `#(#(A 1))` and no ASSOC could look anything up in it.
    """
    result = alist if alist is not None else lisptype.NIL
    pairs = list(zip(_cons_to_list(keys, 'PAIRLIS'), _cons_to_list(data, 'PAIRLIS')))
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


# Predicate tests on sequences
@_registry.cl_function('EVERY')
def every(predicate, *sequences):
    """True if the predicate holds for every set of corresponding elements.

    The predicate is a function *designator* and its answer is a Lisp truth
    value; testing it with a bare `if` made a returned NIL -- a Python-truthy
    object -- count as true.
    """
    predicate = _coerce_function_designator(predicate)
    for args in _parallel_elements(sequences, 'EVERY'):
        if not _lisp_truthy(predicate(*args)):
            return lisptype.NIL
    return lisptype.T


@_registry.cl_function('SOME')
def some(predicate, *sequences):
    """The first true value the predicate returns, or NIL (CLHS 17.3).

    SOME returns the *value* of the predicate, not T."""
    predicate = _coerce_function_designator(predicate)
    for args in _parallel_elements(sequences, 'SOME'):
        value = predicate(*args)
        if _lisp_truthy(value):
            return value
    return lisptype.NIL


@_registry.cl_function('NOTEVERY')
def notevery(predicate, *sequences):
    """Test if predicate is false for some element."""
    return lisptype.lisp_bool(not _lisp_truthy(every(predicate, *sequences)))


@_registry.cl_function('NOTANY')
def notany(predicate, *sequences):
    """Test if predicate is false for all elements."""
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
    """
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
    """
    function = _coerce_function_designator(function)
    limit = seq_length(result_sequence, 'MAP-INTO')
    if sequences:
        rows = _parallel_elements(sequences, 'MAP-INTO')[:limit]
    else:
        rows = [[] for _ in range(limit)]
    for index, args in enumerate(rows):
        seq_set(result_sequence, index, function(*args), 'MAP-INTO')
    return result_sequence


@_registry.cl_function('MAPCAR')
def mapcar(function, *lists):
    """Map over successive elements of lists, collecting results (CLHS 14.2)."""
    function = _coerce_function_designator(function)
    return make_lisp_list([function(*args)
                      for args in _parallel_elements(lists, 'MAPCAR')])


def _nconc_results(results, what):
    """Splice the list results of MAPCAN/MAPCON together (CLHS 14.2).

    A non-list result is spliced as itself only when it is the last one; the
    previous version tested `isinstance(result, list)`, which is false for
    every Lisp list, so `(mapcan #'list '(1 2 3))` collected the sublists
    unspliced.
    """
    elements = []
    for result in results:
        if isinstance(result, lisptype.lispCons):
            elements.extend(_cons_to_list(result, what))
        elif result is None or result is lisptype.NIL:
            continue
        else:
            elements.append(result)
    return make_lisp_list(elements)


@_registry.cl_function('MAPCAN')
def mapcan(function, *lists):
    """MAPCAR, with the results spliced together (CLHS 14.2)."""
    function = _coerce_function_designator(function)
    return _nconc_results(
        [function(*args) for args in _parallel_elements(lists, 'MAPCAN')],
        'MAPCAN')


@_registry.cl_function('MAPC')
def mapc(function, *lists):
    """Map for side effects, returning the first list (CLHS 14.2)."""
    function = _coerce_function_designator(function)
    for args in _parallel_elements(lists, 'MAPC'):
        function(*args)
    return lists[0] if lists else lisptype.NIL


def _successive_tails(lists, what):
    """The successive *tails* (`cdr`s) MAPLIST/MAPCON/MAPL iterate over.

    CLHS 14.2 distinguishes the `-CAR` family, which passes elements, from
    the `-LIST` family, which passes the sublists themselves. MAPLIST/MAPCON/
    MAPL were aliases of MAPCAR/MAPCAN/MAPC here, i.e. the distinction did not
    exist: `(maplist #'list '(1 2))` answered `((1) (2))` instead of
    `(((1 2)) ((2)))`.
    """
    tails = [lst for lst in lists]
    rows = []
    while all(isinstance(tail, lisptype.lispCons) for tail in tails) and tails:
        rows.append(list(tails))
        tails = [tail.cdr for tail in tails]
    return rows


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
def reduce_fn(function, sequence, key=None, from_end=None, start=None, end=None, initial_value=None, **kwargs):
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
    # Handle keyword args that might be passed as :initial-value etc.
    if 'initial-value' in kwargs:
        initial_value = kwargs['initial-value']
    has_initial = initial_value is not None

    function = _coerce_function_designator(function)
    key = _coerce_function_designator(key)

    py_seq = _cons_to_list(sequence, 'REDUCE')
    start, end = bounding_indices(len(py_seq), start, end, 'REDUCE')
    py_seq = py_seq[start:end]
    if key is not None:
        py_seq = [key(item) for item in py_seq]

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
@_registry.cl_function('INTERSECTION')
def intersection(list1, list2, test=None, test_not=None, key=None):
    """Set intersection."""
    matcher = _make_matcher(test=test, test_not=test_not, key=key, key_item=True)
    list2 = _cons_to_list(list2, 'INTERSECTION')
    return _finish_list([x for x in _cons_to_list(list1, 'INTERSECTION')
                         if _matcher_contains(matcher, x, list2)])


@_registry.cl_function('UNION')
def union(list1, list2, test=None, test_not=None, key=None):
    """Set union."""
    matcher = _make_matcher(test=test, test_not=test_not, key=key, key_item=True)
    result = _cons_to_list(list1)
    for item in _cons_to_list(list2):
        if not _matcher_contains(matcher, item, result):
            result.append(item)
    return _finish_list(result)


@_registry.cl_function('NUNION')
def nunion(list1, list2, test=None, test_not=None, key=None):
    """Destructive set union."""
    matcher = _make_matcher(test=test, test_not=test_not, key=key, key_item=True)
    list1 = _cons_to_list(list1)
    for item in _cons_to_list(list2):
        if not _matcher_contains(matcher, item, list1):
            list1.append(item)
    return _finish_list(list1)


@_registry.cl_function('SET-DIFFERENCE')
def set_difference(list1, list2, test=None, test_not=None, key=None):
    """Set difference."""
    matcher = _make_matcher(test=test, test_not=test_not, key=key, key_item=True)
    list2 = _cons_to_list(list2, 'SET-DIFFERENCE')
    return _finish_list([x for x in _cons_to_list(list1, 'SET-DIFFERENCE')
                         if not _matcher_contains(matcher, x, list2)])


@_registry.cl_function('NSET-DIFFERENCE')
def nset_difference(list1, list2, test=None, test_not=None, key=None):
    """Destructive set difference."""
    matcher = _make_matcher(test=test, test_not=test_not, key=key, key_item=True)
    list1 = _cons_to_list(list1)
    list2 = _cons_to_list(list2)
    return _finish_list([x for x in list1 if not _matcher_contains(matcher, x, list2)])


@_registry.cl_function('SET-EXCLUSIVE-OR')
def set_exclusive_or(list1, list2, test=None, test_not=None, key=None):
    """Set exclusive or."""
    matcher = _make_matcher(test=test, test_not=test_not, key=key, key_item=True)
    list1 = _cons_to_list(list1)
    list2 = _cons_to_list(list2)
    return _finish_list(
        [x for x in list1 if not _matcher_contains(matcher, x, list2)]
        + [x for x in list2 if not _matcher_contains(matcher, x, list1)]
    )


@_registry.cl_function('NSET-EXCLUSIVE-OR')
def nset_exclusive_or(list1, list2, test=None, test_not=None, key=None):
    """Destructive set exclusive or."""
    return set_exclusive_or(list1, list2, test=test, test_not=test_not, key=key)


@_registry.cl_function('SUBSETP')
def subsetp(subset, set_arg, test=None, test_not=None, key=None):
    """Test if subset is a subset of set_arg."""
    matcher = _make_matcher(test=test, test_not=test_not, key=key, key_item=True)
    set_arg = _cons_to_list(set_arg)
    for item in _cons_to_list(subset):
        if not _matcher_contains(matcher, item, set_arg):
            return lisptype.NIL
    return lisptype.T


@_registry.cl_function('NINTERSECTION')
def nintersection(list1, list2, test=None, test_not=None, key=None):
    """Destructive intersection."""
    return intersection(list1, list2, test=test, test_not=test_not, key=key)


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
