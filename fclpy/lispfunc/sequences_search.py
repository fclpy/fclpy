"""Sequence search and query operations."""

import functools
from .core import cons, car, cdr, atom, _consp_internal
from . import registry as _registry
import fclpy.lisptype as lisptype


# ===== HELPER FUNCTIONS =====

def _seq_length(sequence):
    """Get the length of any sequence-like object, including lispCons.
    
    Args:
        sequence: List, string, tuple, lispCons, or other sequence type
        
    Returns:
        The length of the sequence as an integer.
    """
    if hasattr(sequence, '__len__'):
        return len(sequence)
    # Handle lispCons by counting elements
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        length = 0
        current = sequence
        while current is not None and current != lisptype.NIL:
            if not hasattr(current, 'cdr'):
                # Dotted pair - add 1 for the car
                length += 1
                break
            length += 1
            current = current.cdr
        return length
    return 0


def _seq_to_list(sequence):
    """Convert any sequence to a Python list for easier processing.

    Args:
        sequence: List, string, tuple, lispCons, AdjustableVector,
            LispString, or any other iterable sequence type

    Returns:
        A Python list containing the elements.
    """
    if isinstance(sequence, (list, tuple)):
        return list(sequence)
    if isinstance(sequence, str):
        return list(sequence)
    # Handle lispCons
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        result = []
        current = sequence
        while current is not None and current != lisptype.NIL:
            if not hasattr(current, 'cdr'):
                # Dotted pair
                result.append(current)
                break
            result.append(current.car)
            current = current.cdr
        return result
    # Everything else that carries the Lisp VECTOR/STRING protocol --
    # `AdjustableVector` (reader-built `#(...)` literals) and `LispString`
    # (`COPY-SEQ` on a string) -- supports plain Python iteration; falling
    # through to `[]` for "unrecognized type" silently dropped every
    # element of both, which is exactly the "Python type test standing in
    # for a Lisp type test" pattern plan.md's Finding M warns about.
    if hasattr(sequence, '__iter__'):
        return list(sequence)
    return []


# ===== SEQUENCE PROTOCOL =====
# Unified interface for sequences: lists, vectors, strings, and extensible for custom types


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


def _char_text(item):
    """Return a 1-character Python str for a Lisp character-like value.

    Elements pulled out of a string sequence are either a `lisptype.Character`
    or, on some paths, an already-plain length-1 Python `str` (plan.md
    Finding I). Rebuilding a string result needs plain text either way.
    """
    if isinstance(item, lisptype.Character):
        return item.char
    return item


def _rebuild_sequence(original, elements):
    """Reconstruct a result in the same kind of sequence as `original`.

    CLHS 17.1: a generic sequence function (one with no `:result-type`
    argument, e.g. REMOVE/SUBSTITUTE and their `N`-destructive counterparts)
    returns a sequence of the *same type* as its `sequence` argument. Every
    caller here has already flattened `original` into a plain Python
    `elements` list via `_seq_to_list` for easy processing; returning that
    list verbatim silently turns a LIST argument into an object that prints
    identically to a proper Lisp list but is not `CONSP`/`EQUAL` to one
    (plan.md Finding M -- a Python container standing in for a Lisp one),
    and turns a STRING argument into a list of one-character pieces instead
    of a string.
    """
    if original is lisptype.NIL or original is None or (
        hasattr(original, 'car') and hasattr(original, 'cdr')
    ):
        result = lisptype.NIL
        for item in reversed(elements):
            result = lisptype.lispCons(item, result)
        return result
    if isinstance(original, lisptype.LispString):
        return lisptype.LispString(''.join(_char_text(e) for e in elements))
    if isinstance(original, str):
        return ''.join(_char_text(e) for e in elements)
    if isinstance(original, tuple):
        return tuple(elements)
    return elements


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
        base, negate = (lambda a, b: a == b), False

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
        # Convert lispCons to list for easier iteration
        if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
            self.sequence = _seq_to_list(sequence)
        else:
            self.sequence = sequence
        self.start = start
        self.end = end if end is not None else _seq_length(self.sequence)
        self.key = _coerce_function_designator(key)
        self._matcher = _make_matcher(test=test, test_not=test_not, key=key)
        self.index = start

    def __iter__(self):
        """Return iterator."""
        return self

    def __next__(self):
        """Get next element."""
        if self.index >= self.end or self.index >= _seq_length(self.sequence):
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
    """
    # Check if it's a lispCons (has car and cdr attributes)
    is_lisp_cons = hasattr(sequence, 'car') and hasattr(sequence, 'cdr')

    if not isinstance(sequence, (list, str, tuple)) and not is_lisp_cons:
        raise TypeError(f"iterate: unsupported sequence type {type(sequence).__name__}")

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


@_registry.cl_function('FIND')
def find(item, sequence, **kwargs):
    """Find item in sequence.

    Supports:
      :key - function (or designator) to apply to each element before comparison
      :test / :test-not - comparison function/designator (default is eql-like)
      :start - start index
      :end - end index
    """
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    test = kwargs.get('test', None)
    test_not = kwargs.get('test_not', None)

    iterator = iterate(sequence, start=start, end=end, key=key, test=test, test_not=test_not)
    for element in iterator:
        if iterator.matches(element, item):
            return element
    return None


@_registry.cl_function('FIND-IF')
def find_if(predicate, sequence, **kwargs):
    """Find item satisfying predicate.

    Supports:
      :key - function (or designator) to apply to each element before testing
      :start - start index
      :end - end index
    """
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    predicate = _coerce_function_designator(predicate)

    iterator = iterate(sequence, start=start, end=end, key=key)
    for element in iterator:
        test_value = iterator.get_value(element)
        if _lisp_truthy(predicate(test_value)):
            return element
    return None


@_registry.cl_function('FIND-IF-NOT')
def find_if_not(predicate, sequence, **kwargs):
    """Find item not satisfying predicate.

    Supports:
      :key - function (or designator) to apply to each element before testing
      :start - start index
      :end - end index
    """
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    predicate = _coerce_function_designator(predicate)

    iterator = iterate(sequence, start=start, end=end, key=key)
    for element in iterator:
        test_value = iterator.get_value(element)
        if not _lisp_truthy(predicate(test_value)):
            return element
    return None


@_registry.cl_function('POSITION')
def position(item, sequence, **kwargs):
    """Find position of item.

    Supports:
      :key - function (or designator) to apply to each element before comparison
      :test / :test-not - comparison function/designator (default is eql-like)
      :start - start index
      :end - end index
    """
    # Convert lispCons to list for indexing
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)

    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    test = kwargs.get('test', None)
    test_not = kwargs.get('test_not', None)
    matcher = _make_matcher(test=test, test_not=test_not, key=key)

    for i in range(start, min(end, _seq_length(sequence))):
        element = sequence[i]
        if matcher(item, element):
            return i
    return None


@_registry.cl_function('POSITION-IF')
def position_if(predicate, sequence, **kwargs):
    """Find position of item satisfying predicate.

    Supports:
      :key - function (or designator) to apply to each element before testing
      :start - start index
      :end - end index
    """
    # Convert lispCons to list for indexing
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)

    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = _coerce_function_designator(kwargs.get('key', None))
    predicate = _coerce_function_designator(predicate)

    for i in range(start, min(end, _seq_length(sequence))):
        element = sequence[i]
        test_val = key(element) if key else element
        if _lisp_truthy(predicate(test_val)):
            return i
    return None


@_registry.cl_function('POSITION-IF-NOT')
def position_if_not(predicate, sequence, **kwargs):
    """Find position of item not satisfying predicate.

    Supports:
      :key - function (or designator) to apply to each element before testing
      :start - start index
      :end - end index
    """
    # Convert lispCons to list for indexing
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)

    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = _coerce_function_designator(kwargs.get('key', None))
    predicate = _coerce_function_designator(predicate)

    for i in range(start, min(end, _seq_length(sequence))):
        element = sequence[i]
        test_val = key(element) if key else element
        if not _lisp_truthy(predicate(test_val)):
            return i
    return None


@_registry.cl_function('COUNT')
def count(item, sequence, **kwargs):
    """Count occurrences of item.

    Supports:
      :key - function (or designator) to apply to each element before comparison
      :test / :test-not - comparison function/designator (default is eql-like)
      :start - start index
      :end - end index
    """
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    test = kwargs.get('test', None)
    test_not = kwargs.get('test_not', None)

    iterator = iterate(sequence, start=start, end=end, key=key, test=test, test_not=test_not)
    count_val = 0
    for element in iterator:
        if iterator.matches(element, item):
            count_val += 1
    return count_val


@_registry.cl_function('COUNT-IF')
def count_if(predicate, sequence, **kwargs):
    """Count items satisfying predicate.

    Supports:
      :key - function (or designator) to apply to each element before testing
      :start - start index
      :end - end index
    """
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    predicate = _coerce_function_designator(predicate)

    iterator = iterate(sequence, start=start, end=end, key=key)
    count_val = 0
    for element in iterator:
        test_value = iterator.get_value(element)
        if _lisp_truthy(predicate(test_value)):
            count_val += 1
    return count_val


@_registry.cl_function('COUNT-IF-NOT')
def count_if_not(predicate, sequence, **kwargs):
    """Count items not satisfying predicate."""
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)
    key = _coerce_function_designator(kwargs.get('key', None))
    predicate = _coerce_function_designator(predicate)
    return sum(1 for x in sequence if not _lisp_truthy(predicate(key(x) if key else x)))


@_registry.cl_function('SEARCH')
def search(sequence1, sequence2, **kwargs):
    """Search for sequence1 in sequence2."""
    # Convert lispCons to lists
    if hasattr(sequence1, 'car') and hasattr(sequence1, 'cdr'):
        sequence1 = _seq_to_list(sequence1)
    if hasattr(sequence2, 'car') and hasattr(sequence2, 'cdr'):
        sequence2 = _seq_to_list(sequence2)
    
    len1 = _seq_length(sequence1)
    len2 = _seq_length(sequence2)
    for i in range(len2 - len1 + 1):
        if sequence2[i:i+len1] == sequence1:
            return i
    return None


@_registry.cl_function('MISMATCH')
def mismatch(sequence1, sequence2, **kwargs):
    """Find first mismatch between sequences."""
    # Convert lispCons to lists
    if hasattr(sequence1, 'car') and hasattr(sequence1, 'cdr'):
        sequence1 = _seq_to_list(sequence1)
    if hasattr(sequence2, 'car') and hasattr(sequence2, 'cdr'):
        sequence2 = _seq_to_list(sequence2)
    
    for i, (x, y) in enumerate(zip(sequence1, sequence2)):
        if x != y:
            return i
    len1 = _seq_length(sequence1)
    len2 = _seq_length(sequence2)
    if len1 != len2:
        return min(len1, len2)
    return None


@_registry.cl_function('MEMBER')
def member(item, list_seq, test=None, test_not=None, key=None):
    """Find member in list.

    Returns the tail of list starting at the first element equal to item,
    or None if item is not found.
    """
    # Handle NIL and None as empty lists
    if list_seq is None or list_seq == lisptype.NIL:
        return None

    # Convert lispCons to list
    if hasattr(list_seq, 'car') and hasattr(list_seq, 'cdr'):
        list_seq = _seq_to_list(list_seq)

    # Handle non-iterable types (defensive - shouldn't happen in correct code)
    if not hasattr(list_seq, '__iter__'):
        return None

    matcher = _make_matcher(test=test, test_not=test_not, key=key)
    for x in list_seq:
        if matcher(item, x):
            return list_seq[list_seq.index(x):]
    return None


@_registry.cl_function('MEMBER-IF')
def member_if(predicate, list_seq, key=None):
    """Find member satisfying predicate."""
    # Convert lispCons to list
    if hasattr(list_seq, 'car') and hasattr(list_seq, 'cdr'):
        list_seq = _seq_to_list(list_seq)

    key = _coerce_function_designator(key)
    predicate = _coerce_function_designator(predicate)
    for x in list_seq:
        if _lisp_truthy(predicate(key(x) if key else x)):
            return list_seq[list_seq.index(x):]
    return None


@_registry.cl_function('MEMBER-IF-NOT')
def member_if_not(predicate, list_seq, key=None):
    """Find member not satisfying predicate."""
    # Convert lispCons to list
    if hasattr(list_seq, 'car') and hasattr(list_seq, 'cdr'):
        list_seq = _seq_to_list(list_seq)

    key = _coerce_function_designator(key)
    predicate = _coerce_function_designator(predicate)
    for x in list_seq:
        if not _lisp_truthy(predicate(key(x) if key else x)):
            return list_seq[list_seq.index(x):]
    return None


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
    '_rebuild_sequence', '_matched_positions', '_char_text',
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
