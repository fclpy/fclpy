"""Sequence composition, sorting, and utility operations."""

import functools
from .core import cons, car, cdr, atom, _consp_internal
from . import registry as _registry
from .arrays import LispArray, nonnegative_integer as _nonnegative_integer
from .sequence_protocol import (
    seq_elements, seq_length, bounding_indices, make_lisp_list, rebuild_like,
    build_sequence, seq_set,
)
from .sequences_search import _coerce_function_designator, _lisp_truthy
import fclpy.lisptype as lisptype


# A sequence longer than this cannot be built on any machine this runs on, so
# attempting it is a STORAGE-CONDITION, not a computation. CLHS 4.4 lets an
# implementation refuse a size above ARRAY-DIMENSION-LIMIT, and refusing is the
# only option that stays honest: `(make-list 10000000000000000000000)` -- a
# legitimate `unsigned-byte`, so no type check rejects it -- otherwise builds
# cons cells one at a time until the machine dies. That is exactly how the
# 2026-08-15 full run wedged at 27GB with no diagnostic, and no in-evaluator
# loop watchdog can see it because it is a single call, not an iteration.
CONSTRUCTIBLE_LIMIT = 1 << 30


def _check_constructible(size, what):
    """Signal rather than attempt an allocation that cannot complete."""
    if size > CONSTRUCTIBLE_LIMIT:
        raise lisptype.LispError(
            f"{what}: cannot build a sequence of {size} elements "
            f"(exceeds this implementation's limit of {CONSTRUCTIBLE_LIMIT})")


def endp(x):
    """Test if object is end of list (nil or empty)."""
    return lisptype.lisp_bool(x is None or x == lisptype.NIL)


@_registry.cl_function('LENGTH')
def length(sequence):
    """Get sequence length."""
    return seq_length(sequence, 'LENGTH')


@_registry.cl_function('REVERSE')
def reverse(sequence):
    """Reverse a sequence, returning a fresh sequence of the same type.

    CLHS 17.1: the result is of the same type as the argument. It used to be
    a Python list unconditionally, so reversing a list produced a vector and
    reversing a string produced a vector of characters.
    """
    return rebuild_like(sequence, list(reversed(seq_elements(sequence, 'REVERSE'))))


@_registry.cl_function('NREVERSE')
def nreverse(sequence):
    """Destructively reverse sequence.

    Permitted to be non-destructive (CLHS only allows the argument to be
    destroyed, it does not require it), but the *type* of the result is not
    optional -- that comes from the shared rebuild.
    """
    return rebuild_like(sequence, list(reversed(seq_elements(sequence, 'NREVERSE'))))


def _append_onto(lists, tail, what):
    """Copy every list in `lists` in front of `tail`, sharing `tail` itself.

    CLHS 14.2: APPEND and NCONC copy all their arguments *except* the last,
    whose structure the result shares -- which is why the last argument is
    threaded through rather than flattened. Both used to flatten every
    argument including the last, so the result never shared structure and a
    non-list final argument (`(append '(1) 2)`, a legal dotted result) was
    turned into a one-element list.
    """
    result = tail if tail is not None else lisptype.NIL
    for seq in reversed(lists):
        for item in reversed(seq_elements(seq, what)):
            result = lisptype.lispCons(item, result)
    return result


@_registry.cl_function('APPEND')
def append(*args):
    """Concatenate lists, sharing structure with the last one (CLHS 14.2)."""
    if not args:
        return lisptype.NIL
    return _append_onto(args[:-1], args[-1], 'APPEND')


@_registry.cl_function('NCONC')
def nconc(*lists):
    """APPEND, permitted to destroy all but the last argument (CLHS 14.2).

    Implemented non-destructively; only the *sharing* of the final argument
    is observable in conforming code.
    """
    if not lists:
        return lisptype.NIL
    return _append_onto(lists[:-1], lists[-1], 'NCONC')


@_registry.cl_function('NRECONC')
def nreconc(list1, list2):
    """`(nconc (nreverse list1) list2)` -- CLHS 14.2.

    Both this and REVAPPEND used to test `isinstance(list1, list)`, which is
    false for the `lispCons` every Lisp list actually is, so the whole body
    was dead and both returned `list2` alone: `(revappend '(1 2) '(3))` was
    `(3)`.
    """
    result = list2 if list2 is not None else lisptype.NIL
    for item in seq_elements(list1, 'NRECONC'):
        result = lisptype.lispCons(item, result)
    return result


@_registry.cl_function('REVAPPEND')
def revappend(list1, list2):
    """`(append (reverse list1) list2)` -- CLHS 14.2."""
    return nreconc(list1, list2)


@_registry.cl_function('CONCATENATE')
def concatenate(result_type, *sequences):
    """Concatenate sequences into a fresh sequence of `result_type` (CLHS 17.3).

    Both halves of this used to be open-coded per result type from an
    upper-cased *string* comparison of the designator, and the LIST branch did
    not iterate its arguments at all -- `(concatenate 'list "ab" #(1 2))`
    returned a two-element vector holding the string and the vector. Element
    access and result construction are now the shared protocol's, so a
    compound designator like `(vector character 4)` works for free.
    """
    elements = []
    for seq in sequences:
        elements.extend(seq_elements(seq, 'CONCATENATE'))
    return build_sequence(result_type, elements, 'CONCATENATE')


def _sort_elements(elements, predicate, key, what):
    """Order `elements` by a CLHS sort predicate.

    The predicate and key are function *designators* (X2), and the predicate's
    answer is a Lisp truth value -- a returned Lisp NIL is Python-truthy, so
    it must go through `_lisp_truthy` rather than `if predicate(...)`.
    """
    predicate = _coerce_function_designator(predicate)
    key = _coerce_function_designator(key)

    def compare(a, b):
        a_key = key(a) if key else a
        b_key = key(b) if key else b
        if _lisp_truthy(predicate(a_key, b_key)):
            return -1
        if _lisp_truthy(predicate(b_key, a_key)):
            return 1
        return 0

    return sorted(elements, key=functools.cmp_to_key(compare))


def _sort(sequence, predicate, key, what):
    """SORT/STABLE-SORT: order in place where the argument allows it, and
    return a sequence of the argument's own type (CLHS 17.1).

    It used to return `sorted(...)`, a Python list, so `(sort (list 3 1 2)
    #'<)` answered `#(1 2 3)` -- a vector -- and `(sort (copy-seq "cba")
    #'char<)` answered a vector of characters. That single wrong result type
    is the whole residual of `iteration/loop6.lsp` and `loop7.lsp`, whose
    tests wrap a correct LOOP result in SORT.
    """
    ordered = _sort_elements(seq_elements(sequence, what), predicate, key, what)
    if isinstance(sequence, (list, lisptype.LispString, LispArray)):
        # A vector or string is sorted in place, which is what "destructive"
        # means for these operators; the argument and the result are then EQ.
        for index, item in enumerate(ordered):
            seq_set(sequence, index, item, what)
        return sequence
    return rebuild_like(sequence, ordered)


@_registry.cl_function('SORT')
def sort(sequence, predicate, key=None):
    """Sort a sequence with a two-argument predicate (CLHS 17.3)."""
    return _sort(sequence, predicate, key, 'SORT')


@_registry.cl_function('STABLE-SORT')
def stable_sort(sequence, predicate, key=None):
    """Stable sort -- Python's `sorted` is already stable."""
    return _sort(sequence, predicate, key, 'STABLE-SORT')


@_registry.cl_function('MERGE')
def merge(result_type, sequence1, sequence2, predicate, key=None, **kwargs):
    """Merge two ordered sequences into one of `result_type` (CLHS 17.3).

    CLHS: an element of `sequence2` is taken only when it *precedes* the
    pending element of `sequence1`, so ties keep `sequence1`'s element first
    -- which is why the predicate is applied as `(predicate e2 e1)` and not
    the other way around.
    """
    predicate = _coerce_function_designator(predicate)
    key = _coerce_function_designator(key)
    left = seq_elements(sequence1, 'MERGE')
    right = seq_elements(sequence2, 'MERGE')

    def sort_key(item):
        return key(item) if key else item

    result = []
    i = j = 0
    while i < len(left) and j < len(right):
        if _lisp_truthy(predicate(sort_key(right[j]), sort_key(left[i]))):
            result.append(right[j])
            j += 1
        else:
            result.append(left[i])
            i += 1
    result.extend(left[i:])
    result.extend(right[j:])
    return build_sequence(result_type, result, 'MERGE')


@_registry.cl_function('SUBSEQ')
def subseq(sequence, start, end=None):
    """Get a subsequence, of the same type as the argument (CLHS 17.3)."""
    elements = seq_elements(sequence, 'SUBSEQ')
    start, end = bounding_indices(len(elements), start, end, 'SUBSEQ')
    return rebuild_like(sequence, elements[start:end])


@_registry.cl_function('COPY-SEQ')
def copy_seq(sequence):
    """Copy a sequence, preserving its type (CLHS 17.3)."""
    return rebuild_like(sequence, seq_elements(sequence, 'COPY-SEQ'))


@_registry.cl_function('COPY-LIST')
def copy_list(list_seq):
    """Copy the top-level list structure (cons spine), preserving a
    non-list final cdr for dotted lists, per ANSI COPY-LIST. Must return a
    genuine lispCons chain, not a Python list, so downstream CONSP/EQUAL
    checks (and further destructive list ops) behave correctly.
    """
    if not _consp_internal(list_seq):
        return list_seq
    items = []
    current = list_seq
    while _consp_internal(current):
        items.append(car(current))
        current = cdr(current)
    result = current  # NIL for a proper list, or the dotted tail
    for item in reversed(items):
        result = cons(item, result)
    return result


@_registry.cl_function('COPY-ALIST')
def copy_alist(alist):
    """Copy an association list: the spine *and* each pair are fresh conses
    (CLHS 14.2), while a non-cons element is shared."""
    pairs = []
    for pair in seq_elements(alist, 'COPY-ALIST'):
        if isinstance(pair, lisptype.lispCons):
            pairs.append(lisptype.lispCons(pair.car, pair.cdr))
        else:
            pairs.append(pair)
    return make_lisp_list(pairs)


@_registry.cl_function('FILL')
def fill(sequence, item, start=0, end=None):
    """Store `item` into a sequence, returning that same sequence (CLHS 17.3).

    Destructive, so it writes *through* the argument via the protocol's
    element store rather than rebuilding: this used to require the argument
    to be a Python `list`, so filling a Lisp list signalled a type error.
    """
    start, end = bounding_indices(seq_length(sequence, 'FILL'), start, end, 'FILL')
    for index in range(start, end):
        seq_set(sequence, index, item, 'FILL')
    return sequence


@_registry.cl_function('REPLACE')
def replace(sequence1, sequence2, **kwargs):
    """Copy a subsequence of `sequence2` into `sequence1` (CLHS 17.3).

    Destructive on `sequence1`, which is returned.
    """
    source = seq_elements(sequence2, 'REPLACE')
    start1, end1 = bounding_indices(
        seq_length(sequence1, 'REPLACE'),
        kwargs.get('start1', 0), kwargs.get('end1'), 'REPLACE')
    start2, end2 = bounding_indices(
        len(source), kwargs.get('start2', 0), kwargs.get('end2'), 'REPLACE')

    if sequence1 is sequence2 and start1 > start2:
        # Overlapping copy within one sequence: CLHS says the result is as if
        # the source subsequence were copied first, so materialize it.
        source = list(source)
    for i, j in zip(range(start1, end1), range(start2, end2)):
        seq_set(sequence1, i, source[j], 'REPLACE')
    return sequence1


@_registry.cl_function('NBUTLAST')
def nbutlast(seq, n=1):
    """Return the list without its last `n` conses (CLHS 14.2)."""
    elements = seq_elements(seq, 'NBUTLAST')
    n = int(n)
    return make_lisp_list(elements[:-n] if n else elements) if n <= len(elements) else lisptype.NIL


@_registry.cl_function('BUTLAST')
def butlast(seq, n=1):
    """Return a fresh list without its last `n` conses (CLHS 14.2).

    Previously a `core.py` stub (`tuple(seq[:-1])`) that ignored `n`
    entirely and returned a Python tuple -- a **vector** in this
    architecture, not a Lisp list (plan.md Finding M) -- so `(listp
    (butlast '(a b c)))` was NIL and any `n` other than the implicit 1 was
    silently discarded. Shares NBUTLAST's element/list-building mechanism;
    the two differ only in being permitted vs. required to share structure
    with `seq`, which this implementation (building a fresh list either
    way) satisfies for both.
    """
    return nbutlast(seq, n)


@_registry.cl_function('LAST')
def last(list_seq, n=1):
    """Return the last `n` *conses* of a list (CLHS 14.2).

    The result is a tail of the list, so it is a list -- `(last '(1 2 3))` is
    `(3)`, not the vector `#(3)` this used to build out of a Python slice.
    """
    n = int(n)
    current = list_seq
    if not isinstance(current, lisptype.lispCons):
        return current if current is not None else lisptype.NIL
    length = len(seq_elements(current, 'LAST'))
    for _ in range(max(0, length - n)):
        current = current.cdr
    return current


@_registry.cl_function('NTHCDR')
def nthcdr(n, list_seq):
    """Get nth cdr."""
    current = list_seq
    for _ in range(n):
        if current is None or current == lisptype.NIL:
            break
        if isinstance(current, lisptype.lispCons):
            current = current.cdr
        else:
            break
    return current


@_registry.cl_function('NTH')
def nth(n, list_seq):
    """Get nth element (0-indexed)."""
    current = nthcdr(n, list_seq)
    if current and isinstance(current, lisptype.lispCons):
        return current.car
    elif isinstance(list_seq, (list, tuple)) and n < len(list_seq):
        return list_seq[n]
    return None


@_registry.cl_function('ELT')
def elt(sequence, index):
    """Get element at index (CLHS 17.3).

    An out-of-bounds index is a type error, not NIL: returning NIL made a
    bad index indistinguishable from an element that is legitimately NIL,
    and it silently satisfied the `.ERROR` tests that check for a signal.
    """
    elements = seq_elements(sequence, 'ELT')
    index = int(index)
    if index < 0 or index >= len(elements):
        raise lisptype.LispTypeError(
            f"ELT: index {index} is out of bounds for a sequence of length "
            f"{len(elements)}",
            expected_type=f"index in [0,{len(elements)})", actual_value=index)
    return elements[index]


@_registry.cl_function('MAKE-LIST')
def make_list(size, initial_element=None):
    """Make list of given size (CLHS: make-list size &key initial-element).

    Previously returned a bare Python list, a second incompatible list
    representation (finding M/X2's defect class, same one FORMATTER's
    returned tail had): `(equal (make-list 3) (list nil nil nil))` compared
    a Python list against a real `lispCons` list and depended on EQUAL's
    fallback for two Python lists, which silently broke the moment either
    side became a proper Lisp list -- exactly what happened once FORMATTER's
    tail was fixed to return one.
    """
    if isinstance(size, lisptype.lispCons):
        size = size.car
    # CLHS: size is an `unsigned-byte`. `int(size)` accepted a float and then
    # tried to build the list, so `(make-list 1.0e18)` allocated until the
    # process died -- see arrays.nonnegative_integer.
    size = _nonnegative_integer(size, 'MAKE-LIST')
    _check_constructible(size, 'MAKE-LIST')
    element = initial_element if initial_element is not None else lisptype.NIL
    result = lisptype.NIL
    for _ in range(size):
        result = lisptype.lispCons(element, result)
    return result


@_registry.cl_function('MAKE-SEQUENCE')
def make_sequence(sequence_type, size, **kwargs):
    """Create a sequence of the given type and size (CLHS 17.3).

    The type was previously compared against the *lower-case Python strings*
    `'list'`/`'vector'`/`'string'`, which no Lisp type designator ever is, so
    every branch fell through to the same Python list.
    """
    if isinstance(size, lisptype.lispCons):
        size = size.car
    # Same defect as MAKE-LIST: `[x] * int(size)` is an unbounded allocation
    # for any size that is not an `unsigned-byte`.
    size = _nonnegative_integer(size, 'MAKE-SEQUENCE')
    _check_constructible(size, 'MAKE-SEQUENCE')
    from .sequence_protocol import parse_sequence_type
    kind, _size, _element_type = parse_sequence_type(sequence_type, 'MAKE-SEQUENCE')
    initial_element = kwargs.get('initial_element', None)
    if initial_element is None:
        # CLHS leaves the contents unspecified without :initial-element; a
        # string must still be filled with characters rather than NIL.
        initial_element = lisptype.Character(' ') if kind == 'STRING' else (
            0 if kind == 'BIT-VECTOR' else lisptype.NIL)
    return build_sequence(sequence_type, [initial_element] * size, 'MAKE-SEQUENCE')


@_registry.cl_function('LIST')
def list_fn(*args):
    """Create list from arguments."""
    # Create a Lisp linked-list (lispCons) rather than a native Python list
    if not args:
        return lisptype.NIL
    result = lisptype.NIL
    # Build from the end to preserve order
    for item in reversed(args):
        result = lisptype.lispCons(item, result)
    return result


# LIST* is implemented once, in `sequences_higher.list_s_star_`. The copy that
# used to live here built a *Python list* and so produced a vector rather than
# a dotted list; both registered the same name, and only the last import won
# (standing rule 3).


@_registry.cl_function('TREE-EQUAL')
def tree_equal(tree1, tree2, test=None):
    """Test tree equality."""
    if test is None:
        test = lambda x, y: x == y
    
    if atom(tree1) and atom(tree2):
        return lisptype.lisp_bool(test(tree1, tree2))
    elif atom(tree1) or atom(tree2):
        return lisptype.NIL
    else:
        # Combine sub-results and convert to Lisp boolean
        left = tree_equal(car(tree1), car(tree2), test)
        right = tree_equal(cdr(tree1), cdr(tree2), test)
        return lisptype.lisp_bool(left == lisptype.T and right == lisptype.T)


@_registry.cl_function('LIST-LENGTH')
def list_length(list_seq):
    """Get list length (proper or dotted)."""
    if list_seq is None or list_seq == lisptype.NIL:
        return 0
    
    count = 0
    current = list_seq
    seen = set()
    
    while current is not None and current != lisptype.NIL:
        if id(current) in seen:
            # Circular list
            return None
        
        if not isinstance(current, lisptype.lispCons):
            # Dotted list
            break
        
        seen.add(id(current))
        count += 1
        current = current.cdr
    
    return count


__all__ = [
    'endp', 'length', 'reverse', 'nreverse',
    'append', 'nconc', 'nreconc', 'revappend', 'concatenate',
    'sort', 'stable_sort', 'merge',
    'subseq', 'copy_seq', 'copy_list', 'copy_alist',
    'fill', 'replace', 'nbutlast', 'butlast', 'last',
    'nthcdr', 'nth', 'elt', 'make_list', 'make_sequence',
    'list_fn', 'tree_equal', 'list_length',
]
