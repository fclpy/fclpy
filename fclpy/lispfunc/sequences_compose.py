"""Sequence composition, sorting, and utility operations."""

import functools
from .core import cons, car, cdr, atom, _consp_internal
from . import registry as _registry
from .arrays import (
    LispArray, nonnegative_integer as _nonnegative_integer,
    _check_other_keys as _check_sequence_other_keys
)
from .sequence_protocol import (
    seq_elements, seq_length, bounding_indices, make_lisp_list, rebuild_like,
    build_sequence, seq_set, list_cells, list_elements, list_tail, _type_name,
)
from .sequences_search import _coerce_function_designator, _lisp_truthy
import fclpy.lisptype as lisptype
from .core import _null_internal, _listp_internal


# A sequence longer than this cannot be built on any machine this runs on, so
# attempting it is a STORAGE-CONDITION, not a computation. CLHS 4.4 lets an
# implementation refuse a size above ARRAY-DIMENSION-LIMIT, and refusing is the
# only option that stays honest: `(make-list 10000000000000000000000)` -- a
# legitimate `unsigned-byte`, so no type check rejects it -- otherwise builds
# cons cells one at a time until the machine dies. That is exactly how the
# 2026-08-15 full run wedged at 27GB with no diagnostic, and no in-evaluator
# loop watchdog can see it because it is a single call, not an iteration.
CONSTRUCTIBLE_LIMIT = 1 << 30

# Sentinel for optional arguments
_UNSUPPLIED = object()


def _check_constructible(size, what):
    """Signal rather than attempt an allocation that cannot complete."""
    if size > CONSTRUCTIBLE_LIMIT:
        raise lisptype.LispError(
            f"{what}: cannot build a sequence of {size} elements "
            f"(exceeds this implementation's limit of {CONSTRUCTIBLE_LIMIT})")


@_registry.cl_function('ENDP')
def endp(x):
    """True if `x` is NIL, false if it is a cons, TYPE-ERROR otherwise (CLHS 14.2).

    ENDP is not `(null x)`: its whole point is that walking off the end of a
    *dotted* list is an error rather than a quiet stop, which is why CLHS
    specifies it as the list-iteration terminator. Answering NIL for a
    non-list -- what this did, since it only tested for NIL -- makes
    `(endp 1)` claim that 1 is a list with elements still to come.
    """
    if _null_internal(x):
        return lisptype.T
    if not _consp_internal(x):
        raise lisptype.LispTypeError(
            f"ENDP: {x!r} is not a list", expected_type="LIST", actual_value=x)
    return lisptype.NIL


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

    For a `LispArray` (the representation of a vector with a fill pointer,
    an element type, or some other array property), the reverse is done
    in-place: the array's own storage is rewritten, and the array is
    returned. That preserves its type so `(type-of x) == (type-of (nreverse x))`
    for fill-pointered vectors (`nreverse-vector.5`/`.6`). A non-array
    sequence uses the shared `rebuild_like`.
    """
    if isinstance(sequence, LispArray):
        elements = seq_elements(sequence, 'NREVERSE')
        elements.reverse()
        for index, item in enumerate(elements):
            sequence[index] = item
        return sequence
    return rebuild_like(sequence, list(reversed(seq_elements(sequence, 'NREVERSE'))))


@_registry.cl_function('APPEND')
def append(*args):
    """Concatenate lists, sharing structure with the last one (CLHS 14.2).

    Every argument but the last must be a *proper* list -- `(append '(a . b)
    '(z))` is a TYPE-ERROR (`append.error.1`) -- while the last is threaded
    through untouched, so it need not be a list at all and `(append '(1) 2)`
    is the dotted `(1 . 2)`. Flattening the last argument too, which this used
    to do, lost both the sharing and that dotted result.
    """
    if not args:
        return lisptype.NIL
    result = args[-1] if args[-1] is not None else lisptype.NIL
    for seq in reversed(args[:-1]):
        for item in reversed(list_elements(seq, 'APPEND')):
            result = lisptype.lispCons(item, result)
    return result


@_registry.cl_function('NCONC')
def nconc(*lists):
    """APPEND, destroying all but the last argument (CLHS 14.2).

    Genuinely destructive, and it has to be: `nconc.4` requires
    `(cdddr x)` to *be* the second argument afterwards, and `nconc.5`
    (`(nconc x x)`) requires the result to be circular. Splicing by RPLACD is
    also what makes a dotted non-final argument legal -- its final cdr is
    overwritten, so `(nconc '(a . b) '(c . d) 'foo)` is `(A C . FOO)`
    (`nconc.7`) rather than the TYPE-ERROR the same shape earns from APPEND.

    NIL arguments vanish (they have no cons to splice onto), and the *last*
    argument is never traversed at all, so it may be any object.
    """
    if not lists:
        return lisptype.NIL
    # The last argument is the tail: never walked, never checked.
    spine = [lst for lst in lists[:-1] if not _null_internal(lst)]
    result = lists[-1] if lists[-1] is not None else lisptype.NIL
    for lst in reversed(spine):
        last_cell = None
        for last_cell in list_cells(lst, 'NCONC', dotted='allow'):
            pass
        last_cell.cdr = result
        result = lst
    return result


@_registry.cl_function('NRECONC')
def nreconc(list1, list2):
    """`(nconc (nreverse list1) list2)` -- CLHS 14.2.

    Both this and REVAPPEND used to test `isinstance(list1, list)`, which is
    false for the `lispCons` every Lisp list actually is, so the whole body
    was dead and both returned `list2` alone: `(revappend '(1 2) '(3))` was
    `(3)`.
    """
    result = list2 if list2 is not None else lisptype.NIL
    for item in list_elements(list1, 'NRECONC'):
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
def sort(sequence, predicate, *, key=None):
    """Sort a sequence with a two-argument predicate (CLHS 17.3)."""
    return _sort(sequence, predicate, key, 'SORT')


@_registry.cl_function('STABLE-SORT')
def stable_sort(sequence, predicate, *, key=None):
    """Stable sort -- Python's `sorted` is already stable."""
    return _sort(sequence, predicate, key, 'STABLE-SORT')


@_registry.cl_function('MERGE')
def merge(result_type, sequence1, sequence2, predicate, key=None,
          allow_other_keys=None, **other_keys):
    """Merge two ordered sequences into one of `result_type` (CLHS 17.3).

    CLHS: an element of `sequence2` is taken only when it *precedes* the
    pending element of `sequence1`, so ties keep `sequence1`'s element first
    -- which is why the predicate is applied as `(predicate e2 e1)` and not
    the other way around.
    """
    _check_sequence_other_keys(other_keys, allow_other_keys, 'MERGE')
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
    result = list_tail(list_seq, 'COPY-LIST')
    for cell in reversed(list(list_cells(list_seq, 'COPY-LIST', dotted='allow'))):
        result = cons(cell.car, result)
    return result


@_registry.cl_function('COPY-ALIST')
def copy_alist(alist):
    """Copy an association list: the spine *and* each pair are fresh conses
    (CLHS 14.2), while a non-cons element is shared.

    An alist is a *proper* list, so `(copy-alist '((a . b) . c))` is a
    TYPE-ERROR; it used to copy the tail C in as a third entry.
    """
    pairs = []
    for pair in list_elements(alist, 'COPY-ALIST'):
        if isinstance(pair, lisptype.lispCons):
            pairs.append(lisptype.lispCons(pair.car, pair.cdr))
        else:
            pairs.append(pair)
    return make_lisp_list(pairs)


@_registry.cl_function('FILL')
def fill(sequence, item, *, start=0, end=None):
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


@_registry.cl_function('BUTLAST')
def butlast(seq, n=1):
    """A fresh copy of the list without its last `n` conses (CLHS 14.2).

    A *dotted* list is legal here and its tail is not an element:
    `(butlast '(a b c . d) 1)` is `(A B)`, because the operator counts conses.
    Reading the elements with `seq_elements` folded the tail in as one, so the
    count was one too high and `(butlast '(a b c . d) 1)` answered `(A B C)`.
    """
    n = _nonnegative_integer(n, 'BUTLAST')
    cells = list(list_cells(seq, 'BUTLAST', dotted='allow'))
    kept = cells[:len(cells) - n] if n <= len(cells) else []
    return make_lisp_list([cell.car for cell in kept])


@_registry.cl_function('NBUTLAST')
def nbutlast(seq, n=1):
    """BUTLAST, permitted to destroy the argument (CLHS 14.2).

    Genuinely destructive, because `nbutlast.1`/`.4` require the result to be
    EQ to the argument and its surviving conses to be the argument's own: it
    RPLACDs the last surviving cons to NIL. Building a fresh list -- what
    BUTLAST does and what this used to do as well -- is only correct for
    BUTLAST, whose result must *not* share structure.
    """
    n = _nonnegative_integer(n, 'NBUTLAST')
    cells = list(list_cells(seq, 'NBUTLAST', dotted='allow'))
    if n >= len(cells):
        return lisptype.NIL
    if n:
        cells[len(cells) - n - 1].cdr = lisptype.NIL
    return seq


@_registry.cl_function('LAST')
def last(list_seq, n=1):
    """The last `n` *conses* of a list (CLHS 14.2).

    The result is a tail of the argument, so it is EQ to that tail rather than
    a copy of it, and it inherits the argument's final cdr: `(last '(a b . c))`
    is `(B . C)` and `(last (cons 'a 'b) 0)` is `B` -- the tail beyond the last
    `n` conses, which for `n` = 0 is the dotted terminator itself. Both were
    wrong while the element count came from `seq_elements`, which counted the
    terminator as an element.
    """
    n = _nonnegative_integer(n, 'LAST')
    cells = list(list_cells(list_seq, 'LAST', dotted='allow'))
    if n >= len(cells):
        return list_seq if not _null_internal(list_seq) else lisptype.NIL
    if n == 0:
        return list_tail(list_seq, 'LAST')
    return cells[len(cells) - n]


@_registry.cl_function('NTHCDR')
def nthcdr(n, list_seq):
    """The result of applying CDR to `list_seq` `n` times (CLHS 14.2).

    `n` is an `unsigned-byte`, so a negative or non-integer `n` is a
    TYPE-ERROR rather than a silently clamped loop count. Walking is lazy: a
    dotted list may be entered but not stepped past, so
    `(nthcdr 1 (cons 'a 'b))` is `B` while `(nthcdr 3 (cons 'a 'b))` signals
    (`nthcdr.5` vs `nthcdr.error.10`).
    """
    n = _nonnegative_integer(n, 'NTHCDR')
    current = list_seq
    for _ in range(n):
        if _null_internal(current):
            return lisptype.NIL
        if not _consp_internal(current):
            raise lisptype.LispTypeError(
                f"NTHCDR: {current!r} is not a list",
                expected_type="LIST", actual_value=current)
        current = current.cdr
    return current


@_registry.cl_function('NTH')
def nth(n, list_seq):
    """The `n`th element of a list, zero-indexed (CLHS 14.2).

    `(nth n x)` is `(car (nthcdr n x))`, and sharing NTHCDR's walk is what
    makes the type checks agree; the Python-list fallback this used to carry
    made NTH the one list accessor that also indexed a *vector*.
    """
    current = nthcdr(n, list_seq)
    return current.car if _consp_internal(current) else lisptype.NIL


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
def make_list(size, *, initial_element=None):
    """Make list of given size (CLHS: make-list size &key initial-element).

    Previously returned a bare Python list, a second incompatible list
    representation (finding M/X2's defect class, same one FORMATTER's
    returned tail had): `(equal (make-list 3) (list nil nil nil))` compared
    a Python list against a real `lispCons` list and depended on EQUAL's
    fallback for two Python lists, which silently broke the moment either
    side became a proper Lisp list -- exactly what happened once FORMATTER's
    tail was fixed to return one.
    """
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
def make_sequence(sequence_type, size, *, initial_element=_UNSUPPLIED,
                  allow_other_keys=None, **other_keys):
    """Create a sequence of the given type and size (CLHS 17.3).

    The type was previously compared against the *lower-case Python strings*
    `'list'`/`'vector'`/`'string'`, which no Lisp type designator ever is, so
    every branch fell through to the same Python list.
    """
    # Validate keywords
    _check_sequence_other_keys(other_keys, allow_other_keys, 'MAKE-SEQUENCE')

    if isinstance(size, lisptype.lispCons):
        size = size.car
    # Same defect as MAKE-LIST: `[x] * int(size)` is an unbounded allocation
    # for any size that is not an `unsigned-byte`.
    size = _nonnegative_integer(size, 'MAKE-SEQUENCE')
    _check_constructible(size, 'MAKE-SEQUENCE')
    from .sequence_protocol import parse_sequence_type
    kind, _size, _element_type = parse_sequence_type(sequence_type, 'MAKE-SEQUENCE')

    # Validate type constraints: NULL can only have length 0, CONS must have length > 0
    if kind == 'LIST':
        # Check for NULL type constraint
        if _type_name(sequence_type) == 'NULL' and size > 0:
            raise lisptype.LispTypeError(
                f"MAKE-SEQUENCE: cannot create a sequence of type NULL with size {size}",
                expected_type="NIL", actual_value=size)
        # Check for CONS type constraint
        if _type_name(sequence_type) == 'CONS' and size == 0:
            raise lisptype.LispTypeError(
                f"MAKE-SEQUENCE: cannot create a sequence of type CONS with size 0",
                expected_type="CONS (non-empty)", actual_value=size)

    # Determine initial element
    if initial_element is _UNSUPPLIED:
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
def tree_equal(tree1, tree2, *, test=None, test_not=None):
    """Compare two trees, comparing their *leaves* with :test (CLHS 14.2).

    The default test is EQL, not Python `==`. That distinction is the whole of
    `tree-equal.16`: two separately-consed empty strings are `==` but not EQL,
    and since a string is an atom here, TREE-EQUAL must answer NIL for them.
    Sharing `_make_matcher` is what supplies EQL, :test-not and the
    designator coercion, instead of a bare `lambda x, y: x == y`.

    **The cdr spine is walked iteratively, not recursed into** -- the same
    rule `equal` in comparison.py and COPY-TREE (recursion-plan.md Steps 1-2)
    document: recursing on the cdr costs one Python frame per *element*, so
    two long lists overflowed the default frame limit before either tree's
    nesting was deep. Only the car recurses, so the Python depth is the
    trees' *depth*.

    A circular spine is signalled, not walked: the recursive version this
    replaced died with a RecursionError on one, so raising keeps the
    operator terminating without introducing a hang.
    """
    from .sequences_search import _make_matcher
    matcher = _make_matcher(test=test, test_not=test_not)

    def compare(a, b):
        seen = set()
        while True:
            a_atom, b_atom = _atom(a), _atom(b)
            if a_atom or b_atom:
                return a_atom and b_atom and matcher(a, b)
            key = (id(a), id(b))
            if key in seen:
                raise lisptype.LispError("TREE-EQUAL: the trees are circular")
            seen.add(key)
            if not compare(car(a), car(b)):
                return False
            a, b = cdr(a), cdr(b)

    return lisptype.lisp_bool(compare(tree1, tree2))


def _atom(value):
    """True for everything that is not a cons -- including NIL (CLHS 14.1)."""
    return not _consp_internal(value)


@_registry.cl_function('LIST-LENGTH')
def list_length(list_seq):
    """The length of a proper list, or NIL if it is circular (CLHS 14.2).

    The two answers LIST-LENGTH may give are "a length" and "NIL, it is
    circular" -- a *dotted* list is neither, it is a TYPE-ERROR, which is the
    one shape this used to return a plausible number for
    (`(list-length '(a b c d . e))` answered 4). Circularity is detected here
    rather than in the shared walker because LIST-LENGTH is the only operator
    CLHS requires to terminate on a circular list; every other one is
    explicitly undefined on it, and paying for an identity set on every list
    traversal to serve one operator is the wrong trade.
    """
    count = 0
    seen = set()
    for cell in list_cells(list_seq, 'LIST-LENGTH'):
        if id(cell) in seen:
            return lisptype.NIL
        seen.add(id(cell))
        count += 1
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
