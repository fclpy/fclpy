"""Sequence search and query operations."""

from .core import cons, car, cdr, atom, _consp_internal
from . import registry as _registry
from .sequence_protocol import (
    bounding_indices as _bounding_indices,
    seq_elements as _seq_to_list,
    seq_length as _seq_length,
    rebuild_like as _rebuild_sequence,
)
import fclpy.lisptype as lisptype


# ===== HELPER FUNCTIONS =====
# Element access and result construction live in `sequence_protocol`, which is
# the single implementation of both halves of CLHS 17.1; the aliases above keep
# this module's local spelling without keeping a second copy of the mechanism.


def _lisp_truthy(value):
    """Python-bool-safe truthiness check for a test function's result.

    `lisptype.is_truthy` only excludes NIL/None, so a raw Python `False`
    (e.g. from the default `lambda a, b: a == b` comparator, or from a
    user predicate that returns a plain bool rather than T/NIL) reads as
    true under it -- a known landmine (plan.md). Sidestepped here rather
    than in `is_truthy` itself, which has a much wider blast radius
    (tracked separately under M2).
    """
    return value is not lisptype.NIL and value is not None and value is not False


def _coerce_function_designator(designator):
    """Resolve a :test/:test-not/:key/predicate argument to a callable.

    These arguments are CLHS "function designators": either a function
    object or a symbol naming one (e.g. `:test 'EQL`). A bare `LispSymbol`
    is not itself callable, so treating it as one crashes with a Python
    `TypeError` instead of dispatching through the function namespace --
    plan.md finding X2. Resolution is delegated to `coerce_to_function`,
    the same mechanism APPLY/FUNCALL use, so there is exactly one place
    that knows how to turn a designator into a callable.
    """
    # NIL is a valid designator meaning "no function" (e.g. an explicit
    # `:key nil`, the default); it shows up as Python None *or* as the
    # lisptype.NIL singleton (CLAUDE.md's NIL-has-three-representations
    # gotcha), so both must be normalized to None here or a truthiness
    # check like `if key:` downstream sees a NIL object as truthy.
    if designator is None or designator is lisptype.NIL:
        return None
    if callable(designator):
        return designator
    from .evaluation_core import coerce_to_function
    return coerce_to_function(designator, 'sequence function')


def _matched_positions(start, end, from_end, count, matches_at):
    """Select which indices in `[start, end)` a sequence-modifying function
    should act on, per CLHS 17.2.1's `:count`/`:from-end` protocol.

    `matches_at(i)` is called to test each candidate index and may have
    side effects (a `:test`/predicate function is a full function, not a
    pure comparison -- ANSI's own `nsubstitute-list.20`-style tests rely on
    a stateful lambda). Two things distinguish this from filtering a
    precomputed list and slicing it: `matches_at` is called in the *scan
    order* CLHS specifies -- descending when `:from-end` is true, ascending
    otherwise -- so a stateful predicate observes the same call sequence a
    conforming implementation would; and scanning stops as soon as `count`
    matches have been found, so `matches_at` is never called more times than
    necessary. A NIL/None `count` means "no limit"; CLHS treats a negative
    count as zero.
    """
    if count is not None:
        try:
            count = int(count)
        except (TypeError, ValueError):
            count = None
    chosen = set()
    if count is not None and count <= 0:
        return chosen
    indices = range(end - 1, start - 1, -1) if from_end else range(start, end)
    for i in indices:
        if matches_at(i):
            chosen.add(i)
            if count is not None and len(chosen) >= count:
                break
    return chosen


def _make_matcher(test=None, test_not=None, key=None):
    """Build a `matcher(item, candidate)` implementing CLHS 17.2.1's
    two-argument test protocol in one place.

    CLHS 17.2.1: the test is called as `(funcall test item (funcall key
    element))` -- the item being searched for is always the first
    argument, the (possibly key-transformed) sequence element is always
    the second. `SequenceIterator.matches` previously called it the other
    way around (plan.md finding X3); every caller now shares this one
    matcher instead of re-deriving (and re-reversing) the same logic.
    """
    test = _coerce_function_designator(test)
    test_not = _coerce_function_designator(test_not)
    key = _coerce_function_designator(key)

    if test is not None:
        base, negate = test, False
    elif test_not is not None:
        base, negate = test_not, True
    else:
        # CLHS 17.2.1: the default test is EQL, not Python `==`. The two
        # differ on exactly the values these functions are asked about most:
        # `1 == 1.0` is Python-true but `(eql 1 1.0)` is false, and a
        # `Character`'s `__eq__` refuses a plain 1-character `str`, which is
        # the other half of the `LispString` representation split -- so
        # `(remove #\a "abc")` removed nothing.
        from .comparison import eql as _eql
        base, negate = _eql, False

    def matcher(item, candidate):
        value = key(candidate) if key else candidate
        matched = _lisp_truthy(base(item, value))
        return (not matched) if negate else matched

    return matcher


class SequenceIterator:
    """Universal sequence iterator supporting lists, vectors, strings, and extensible types."""

    def __init__(self, sequence, start=0, end=None, key=None, test=None, test_not=None):
        """Initialize iterator over a sequence.

        Args:
            sequence: The sequence to iterate over (list, str, vector, or custom)
            start: Starting index (inclusive)
            end: Ending index (exclusive), None means end of sequence
            key: Optional function (or function designator) to transform values for testing
            test: Optional comparison function/designator (default is eql-like equality)
            test_not: Optional negated comparison function/designator
        """
        # One element-access path for every sequence representation: the
        # iterator works over the protocol's element list rather than over
        # whichever Python container happened to be passed in.
        self.sequence = _seq_to_list(sequence)
        self.start = 0 if start is None or start is lisptype.NIL else int(start)
        self.end = (len(self.sequence) if end is None or end is lisptype.NIL
                    else int(end))
        self.key = _coerce_function_designator(key)
        self._matcher = _make_matcher(test=test, test_not=test_not, key=key)
        self.index = self.start

    def __iter__(self):
        """Return iterator."""
        return self

    def __next__(self):
        """Get next element."""
        if self.index >= self.end or self.index >= len(self.sequence):
            raise StopIteration
        value = self.sequence[self.index]
        self.index += 1
        return value

    def current_index(self):
        """Get current index in the sequence."""
        return self.index - 1

    def get_value(self, item):
        """Apply key function if provided, otherwise return item unchanged."""
        return self.key(item) if self.key else item

    def matches(self, item, target):
        """Test whether sequence element `item` matches search value `target`."""
        return self._matcher(target, item)

    def reset(self, start=None):
        """Reset iterator to start position."""
        self.index = start if start is not None else self.start


def iterate(sequence, start=0, end=None, key=None, test=None, test_not=None):
    """Create a universal sequence iterator.

    Args:
        sequence: List, string, vector, or other sequence type
        start: Starting index (default 0)
        end: Ending index (default is length of sequence)
        key: Optional function (or function designator) to extract/transform values
        test: Optional comparison function/designator (default is eql-like equality)
        test_not: Optional negated comparison function/designator

    Returns:
        SequenceIterator instance for the sequence.

    A non-sequence argument is rejected by the protocol's element access with
    a Lisp `LispTypeError`. This used to gate on
    `isinstance(sequence, (list, str, tuple))` and raise a *Python* TypeError
    otherwise, so every `#(...)` literal (an `AdjustableVector`) and every
    `"..."` literal (a `LispString`) made FIND/POSITION/COUNT/MISMATCH return
    the text of a Python exception as a Lisp value -- plan.md Finding M
    feeding standing rule 2.
    """
    return SequenceIterator(sequence, start, end, key, test, test_not)


def with_sequence_protocol(sequence, start=0, end=None, key=None, test=None, test_not=None):
    """Helper to wrap sequence operations with protocol support.

    This is a convenience function that ensures consistent handling of:
    - start/end boundaries
    - key transformation functions
    - test comparison functions

    Args:
        sequence: The sequence to process
        start: Starting index
        end: Ending index
        key: Optional transformation function/designator
        test: Optional comparison function/designator
        test_not: Optional negated comparison function/designator

    Returns:
        SequenceIterator configured with all parameters.
    """
    return iterate(sequence, start, end, key, test, test_not)


def _scan(sequence, kwargs, what):
    """The (elements, indices) a CLHS scanning function should visit.

    One place decides what `:start`, `:end` and `:from-end` mean, for all of
    FIND/POSITION/COUNT and their `-IF`/`-IF-NOT` variants. Each of them used
    to re-derive it, and each got a different subset right: `:end` was
    `min(end, len)` in some and unbounded in others, `:from-end` was ignored
    everywhere, and a NIL `:end` -- which CLHS explicitly allows, meaning
    "the end" -- crashed the `min`.
    """
    elements = _seq_to_list(sequence, what)
    start, end = _bounding_indices(
        len(elements), kwargs.get('start', 0), kwargs.get('end'), what)
    from_end = _lisp_truthy(kwargs.get('from_end', None))
    indices = range(end - 1, start - 1, -1) if from_end else range(start, end)
    return elements, indices


def _scan_key(kwargs):
    """The `:key` of a scanning function, as a callable or None."""
    return _coerce_function_designator(kwargs.get('key', None))


def _scan_matcher(kwargs):
    """The shared `:test`/`:test-not`/`:key` matcher of a scanning function."""
    return _make_matcher(test=kwargs.get('test'),
                         test_not=kwargs.get('test_not'),
                         key=kwargs.get('key'))


@_registry.cl_function('FIND')
def find(item, sequence, **kwargs):
    """The first element matching `item` (CLHS 17.3).

    Honours :key, :test, :test-not, :start, :end and :from-end through the
    shared scan and the shared matcher.
    """
    elements, indices = _scan(sequence, kwargs, 'FIND')
    matcher = _scan_matcher(kwargs)
    for i in indices:
        if matcher(item, elements[i]):
            return elements[i]
    return lisptype.NIL


@_registry.cl_function('FIND-IF')
def find_if(predicate, sequence, **kwargs):
    """The first element satisfying `predicate` (CLHS 17.3)."""
    elements, indices = _scan(sequence, kwargs, 'FIND-IF')
    key = _scan_key(kwargs)
    predicate = _coerce_function_designator(predicate)
    for i in indices:
        element = elements[i]
        if _lisp_truthy(predicate(key(element) if key else element)):
            return element
    return lisptype.NIL


@_registry.cl_function('FIND-IF-NOT')
def find_if_not(predicate, sequence, **kwargs):
    """The first element failing `predicate` (CLHS 17.3)."""
    elements, indices = _scan(sequence, kwargs, 'FIND-IF-NOT')
    key = _scan_key(kwargs)
    predicate = _coerce_function_designator(predicate)
    for i in indices:
        element = elements[i]
        if not _lisp_truthy(predicate(key(element) if key else element)):
            return element
    return lisptype.NIL


@_registry.cl_function('POSITION')
def position(item, sequence, **kwargs):
    """The index of the first element matching `item` (CLHS 17.3)."""
    elements, indices = _scan(sequence, kwargs, 'POSITION')
    matcher = _scan_matcher(kwargs)
    for i in indices:
        if matcher(item, elements[i]):
            return i
    return lisptype.NIL


@_registry.cl_function('POSITION-IF')
def position_if(predicate, sequence, **kwargs):
    """The index of the first element satisfying `predicate` (CLHS 17.3)."""
    elements, indices = _scan(sequence, kwargs, 'POSITION-IF')
    key = _scan_key(kwargs)
    predicate = _coerce_function_designator(predicate)
    for i in indices:
        element = elements[i]
        if _lisp_truthy(predicate(key(element) if key else element)):
            return i
    return lisptype.NIL


@_registry.cl_function('POSITION-IF-NOT')
def position_if_not(predicate, sequence, **kwargs):
    """The index of the first element failing `predicate` (CLHS 17.3)."""
    elements, indices = _scan(sequence, kwargs, 'POSITION-IF-NOT')
    key = _scan_key(kwargs)
    predicate = _coerce_function_designator(predicate)
    for i in indices:
        element = elements[i]
        if not _lisp_truthy(predicate(key(element) if key else element)):
            return i
    return lisptype.NIL


@_registry.cl_function('COUNT')
def count(item, sequence, **kwargs):
    """How many elements match `item` (CLHS 17.3)."""
    elements, indices = _scan(sequence, kwargs, 'COUNT')
    matcher = _scan_matcher(kwargs)
    return sum(1 for i in indices if matcher(item, elements[i]))


@_registry.cl_function('COUNT-IF')
def count_if(predicate, sequence, **kwargs):
    """How many elements satisfy `predicate` (CLHS 17.3)."""
    elements, indices = _scan(sequence, kwargs, 'COUNT-IF')
    key = _scan_key(kwargs)
    predicate = _coerce_function_designator(predicate)
    return sum(1 for i in indices
               if _lisp_truthy(predicate(key(elements[i]) if key else elements[i])))


@_registry.cl_function('COUNT-IF-NOT')
def count_if_not(predicate, sequence, **kwargs):
    """How many elements fail `predicate` (CLHS 17.3).

    This one ignored :start/:end entirely, so a bounded count answered for
    the whole sequence.
    """
    elements, indices = _scan(sequence, kwargs, 'COUNT-IF-NOT')
    key = _scan_key(kwargs)
    predicate = _coerce_function_designator(predicate)
    return sum(1 for i in indices
               if not _lisp_truthy(predicate(key(elements[i]) if key else elements[i])))


def _two_sequence_matcher(kwargs):
    """The comparison SEARCH and MISMATCH apply to a pair of elements.

    Both of their arguments are *sequence elements*, so CLHS 17.2.1's `:key`
    applies to **both** -- unlike FIND/POSITION/COUNT, where the item being
    searched for is not keyed. Reusing `_make_matcher` directly would key only
    the second argument, which is why `(mismatch "1010" "1000" :key
    'odddigitp)` needs this wrapper rather than the plain matcher.
    """
    base = _make_matcher(test=kwargs.get('test'), test_not=kwargs.get('test_not'))
    key = _coerce_function_designator(kwargs.get('key', None))
    if key is None:
        return base
    return lambda a, b: base(key(a), key(b))


def _two_sequence_bounds(sequence1, sequence2, kwargs, what):
    """Element lists and bounding indices for the two-sequence operators.

    SEARCH and MISMATCH take *two* bounding-index pairs (`:start1`/`:end1`
    and `:start2`/`:end2`); both used to ignore all four, along with `:test`
    and `:key`, and compare with Python `==` on a slice.
    """
    left = _seq_to_list(sequence1, what)
    right = _seq_to_list(sequence2, what)
    start1, end1 = _bounding_indices(
        len(left), kwargs.get('start1', 0), kwargs.get('end1'), what)
    start2, end2 = _bounding_indices(
        len(right), kwargs.get('start2', 0), kwargs.get('end2'), what)
    return left, right, start1, end1, start2, end2


@_registry.cl_function('SEARCH')
def search(sequence1, sequence2, **kwargs):
    """The index in `sequence2` of a subsequence matching `sequence1`."""
    left, right, start1, end1, start2, end2 = _two_sequence_bounds(
        sequence1, sequence2, kwargs, 'SEARCH')
    matcher = _two_sequence_matcher(kwargs)
    pattern = left[start1:end1]
    width = len(pattern)
    candidates = list(range(start2, end2 - width + 1))
    if _lisp_truthy(kwargs.get('from_end', None)):
        candidates.reverse()
    for offset in candidates:
        if all(matcher(pattern[k], right[offset + k]) for k in range(width)):
            return offset
    return lisptype.NIL


@_registry.cl_function('MISMATCH')
def mismatch(sequence1, sequence2, **kwargs):
    """The index in `sequence1` where the two sequences first differ.

    CLHS 17.3: the index is relative to `sequence1` as a whole, not to its
    bounded subsequence, and under `:from-end` it is *one plus* the index of
    the rightmost difference. NIL means the bounded subsequences match.
    """
    left, right, start1, end1, start2, end2 = _two_sequence_bounds(
        sequence1, sequence2, kwargs, 'MISMATCH')
    matcher = _two_sequence_matcher(kwargs)
    width1, width2 = end1 - start1, end2 - start2
    shared = min(width1, width2)

    if _lisp_truthy(kwargs.get('from_end', None)):
        for offset in range(1, shared + 1):
            if not matcher(left[end1 - offset], right[end2 - offset]):
                return end1 - offset + 1
        return lisptype.NIL if width1 == width2 else start1 + shared
    for offset in range(shared):
        if not matcher(left[start1 + offset], right[start2 + offset]):
            return start1 + offset
    return lisptype.NIL if width1 == width2 else start1 + shared


def _member_tail(list_seq, accepts):
    """The first tail of `list_seq` whose car satisfies `accepts`, else NIL.

    CLHS 14.2: MEMBER returns *the tail itself*, which must be a sublist of
    the argument -- callers rely on it being EQ to that sublist and on being
    able to keep walking or RPLACD it. The three MEMBER functions used to
    flatten the list and return a Python slice, i.e. a fresh vector, located
    with `list.index(x)` -- so a duplicated element returned the tail at the
    *first* equal element rather than the one that matched.
    """
    current = list_seq
    while isinstance(current, lisptype.lispCons):
        if accepts(current.car):
            return current
        current = current.cdr
    return lisptype.NIL


@_registry.cl_function('MEMBER')
def member(item, list_seq, test=None, test_not=None, key=None):
    """The tail of the list beginning with the first element matching `item`."""
    matcher = _make_matcher(test=test, test_not=test_not, key=key)
    return _member_tail(list_seq, lambda element: matcher(item, element))


@_registry.cl_function('MEMBER-IF')
def member_if(predicate, list_seq, key=None):
    """The tail beginning with the first element satisfying `predicate`."""
    key = _coerce_function_designator(key)
    predicate = _coerce_function_designator(predicate)
    return _member_tail(
        list_seq,
        lambda element: _lisp_truthy(predicate(key(element) if key else element)))


@_registry.cl_function('MEMBER-IF-NOT')
def member_if_not(predicate, list_seq, key=None):
    """The tail beginning with the first element failing `predicate`."""
    key = _coerce_function_designator(key)
    predicate = _coerce_function_designator(predicate)
    return _member_tail(
        list_seq,
        lambda element: not _lisp_truthy(predicate(key(element) if key else element)))


def _pair_key(pair, index):
    """Extract the car (index 0) or cdr/second element (index 1) of an
    alist pair, whether the pair is a `lispCons` or a Python list/tuple.

    CLHS requires every element of an alist to itself be a cons (or NIL,
    filtered out by the caller); an atom in that position -- e.g. a stray
    keyword -- is a type error, not a value to quietly skip. Swallowing
    the lookup failure here would trade a loud gap for a silent wrong
    answer (plan.md standing rule 4).
    """
    if hasattr(pair, 'car') and hasattr(pair, 'cdr'):
        return pair.car if index == 0 else pair.cdr
    if isinstance(pair, (list, tuple)) and len(pair) > index:
        return pair[index]
    raise lisptype.LispTypeError(
        f"Malformed alist entry (not a cons): {pair!r}",
        expected_type='CONS',
        actual_value=pair,
    )


def _alist_pairs(alist):
    """Yield the pairs of an association list, whether it is a `lispCons`
    list or a Python list/tuple.
    """
    if hasattr(alist, 'car') and hasattr(alist, 'cdr'):
        current = alist
        while current is not None and current is not lisptype.NIL and hasattr(current, 'car'):
            yield current.car
            current = current.cdr
    elif isinstance(alist, (list, tuple)):
        for pair in alist:
            yield pair


@_registry.cl_function('ASSOC')
def assoc(item, alist, test=None, test_not=None, key=None):
    """Find association with key equal to item.

    Keyword arguments:
    - :test - equality test function/designator (default is EQL-like)
    - :test-not - inequality test function/designator
    - :key - function/designator to apply to each pair's car before testing
    """
    matcher = _make_matcher(test=test, test_not=test_not, key=key)
    for pair in _alist_pairs(alist):
        if pair is None or pair is lisptype.NIL:
            continue
        if matcher(item, _pair_key(pair, 0)):
            return pair
    return lisptype.NIL


@_registry.cl_function('ASSOC-IF')
def assoc_if(predicate, alist, key=None):
    """Find association whose key satisfies predicate."""
    key = _coerce_function_designator(key)
    predicate = _coerce_function_designator(predicate)
    for pair in _alist_pairs(alist):
        if pair is None or pair is lisptype.NIL:
            continue
        value = _pair_key(pair, 0)
        if _lisp_truthy(predicate(key(value) if key else value)):
            return pair
    return lisptype.NIL


@_registry.cl_function('ASSOC-IF-NOT')
def assoc_if_not(predicate, alist, key=None):
    """Find association whose key does not satisfy predicate."""
    key = _coerce_function_designator(key)
    predicate = _coerce_function_designator(predicate)
    for pair in _alist_pairs(alist):
        if pair is None or pair is lisptype.NIL:
            continue
        value = _pair_key(pair, 0)
        if not _lisp_truthy(predicate(key(value) if key else value)):
            return pair
    return lisptype.NIL


@_registry.cl_function('RASSOC')
def rassoc(item, alist, test=None, test_not=None, key=None):
    """Reverse association - find pair whose cdr matches item."""
    matcher = _make_matcher(test=test, test_not=test_not, key=key)
    for pair in _alist_pairs(alist):
        if pair is None or pair is lisptype.NIL:
            continue
        if matcher(item, _pair_key(pair, 1)):
            return pair
    return lisptype.NIL


@_registry.cl_function('RASSOC-IF')
def rassoc_if(predicate, alist, key=None):
    """Reverse association with predicate applied to each pair's cdr."""
    key = _coerce_function_designator(key)
    predicate = _coerce_function_designator(predicate)
    for pair in _alist_pairs(alist):
        if pair is None or pair is lisptype.NIL:
            continue
        value = _pair_key(pair, 1)
        if _lisp_truthy(predicate(key(value) if key else value)):
            return pair
    return lisptype.NIL


@_registry.cl_function('RASSOC-IF-NOT')
def rassoc_if_not(predicate, alist, key=None):
    """Reverse association with negated predicate applied to each pair's cdr."""
    key = _coerce_function_designator(key)
    predicate = _coerce_function_designator(predicate)
    for pair in _alist_pairs(alist):
        if pair is None or pair is lisptype.NIL:
            continue
        value = _pair_key(pair, 1)
        if not _lisp_truthy(predicate(key(value) if key else value)):
            return pair
    return lisptype.NIL


__all__ = [
    # Shared helpers
    '_rebuild_sequence', '_matched_positions',
    # SequenceIterator protocol
    'SequenceIterator', 'iterate', 'with_sequence_protocol',
    # Find operations
    'find', 'find_if', 'find_if_not',
    # Position operations
    'position', 'position_if', 'position_if_not',
    # Count operations
    'count', 'count_if', 'count_if_not',
    # Search operations
    'search', 'mismatch', 'member', 'member_if', 'member_if_not',
    # Association operations
    'assoc', 'assoc_if', 'assoc_if_not', 'rassoc', 'rassoc_if', 'rassoc_if_not',
]
