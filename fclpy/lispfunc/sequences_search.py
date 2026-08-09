"""Sequence search and query operations."""

import functools
from .core import cons, car, cdr, atom
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
        sequence: List, string, tuple, lispCons, or other sequence type
        
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
    return []


# ===== SEQUENCE PROTOCOL =====
# Unified interface for sequences: lists, vectors, strings, and extensible for custom types


class SequenceIterator:
    """Universal sequence iterator supporting lists, vectors, strings, and extensible types."""
    
    def __init__(self, sequence, start=0, end=None, key=None, test=None):
        """Initialize iterator over a sequence.
        
        Args:
            sequence: The sequence to iterate over (list, str, vector, or custom)
            start: Starting index (inclusive)
            end: Ending index (exclusive), None means end of sequence
            key: Optional function to extract/transform values for testing
            test: Optional comparison function (default is equal)
        """
        # Convert lispCons to list for easier iteration
        if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
            self.sequence = _seq_to_list(sequence)
        else:
            self.sequence = sequence
        self.start = start
        self.end = end if end is not None else _seq_length(self.sequence)
        self.key = key
        self.test = test or (lambda x, y: x == y)
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
        """Test if item matches target using test function."""
        return self.test(self.get_value(item), target)
    
    def reset(self, start=None):
        """Reset iterator to start position."""
        self.index = start if start is not None else self.start


def iterate(sequence, start=0, end=None, key=None, test=None):
    """Create a universal sequence iterator.
    
    Args:
        sequence: List, string, vector, or other sequence type
        start: Starting index (default 0)
        end: Ending index (default is length of sequence)
        key: Optional function to extract/transform values
        test: Optional comparison function (default is =)
    
    Returns:
        SequenceIterator instance for the sequence.
    """
    # Check if it's a lispCons (has car and cdr attributes)
    is_lisp_cons = hasattr(sequence, 'car') and hasattr(sequence, 'cdr')
    
    if not isinstance(sequence, (list, str, tuple)) and not is_lisp_cons:
        raise TypeError(f"iterate: unsupported sequence type {type(sequence).__name__}")
    
    return SequenceIterator(sequence, start, end, key, test)


def with_sequence_protocol(sequence, start=0, end=None, key=None, test=None):
    """Helper to wrap sequence operations with protocol support.
    
    This is a convenience function that ensures consistent handling of:
    - start/end boundaries
    - key transformation functions
    - test comparison functions
    
    Args:
        sequence: The sequence to process
        start: Starting index
        end: Ending index
        key: Optional transformation function
        test: Optional comparison function
    
    Returns:
        SequenceIterator configured with all parameters.
    """
    return iterate(sequence, start, end, key, test)


@_registry.cl_function('FIND')
def find(item, sequence, **kwargs):
    """Find item in sequence.
    
    Supports:
      :key - function to apply to each element before comparison
      :test - comparison function (default is eql)
      :start - start index
      :end - end index
    """
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    test = kwargs.get('test', lambda x, y: x == y)
    
    iterator = iterate(sequence, start=start, end=end, key=key, test=test)
    for element in iterator:
        if iterator.matches(element, item):
            return element
    return None


@_registry.cl_function('FIND-IF')
def find_if(predicate, sequence, **kwargs):
    """Find item satisfying predicate.
    
    Supports:
      :key - function to apply to each element before testing
      :start - start index
      :end - end index
    """
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    
    iterator = iterate(sequence, start=start, end=end, key=key)
    for element in iterator:
        test_value = iterator.get_value(element)
        if predicate(test_value):
            return element
    return None


@_registry.cl_function('FIND-IF-NOT')
def find_if_not(predicate, sequence, **kwargs):
    """Find item not satisfying predicate.
    
    Supports:
      :key - function to apply to each element before testing
      :start - start index
      :end - end index
    """
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    
    iterator = iterate(sequence, start=start, end=end, key=key)
    for element in iterator:
        test_value = iterator.get_value(element)
        if not predicate(test_value):
            return element
    return None


@_registry.cl_function('POSITION')
def position(item, sequence, **kwargs):
    """Find position of item.
    
    Supports:
      :key - function to apply to each element before comparison
      :test - comparison function (default is eql)
      :start - start index
      :end - end index
    """
    # Convert lispCons to list for indexing
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)
    
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    test = kwargs.get('test', lambda x, y: x == y)
    
    for i in range(start, min(end, _seq_length(sequence))):
        element = sequence[i]
        test_val = key(element) if key else element
        if test(test_val, item):
            return i
    return None


@_registry.cl_function('POSITION-IF')
def position_if(predicate, sequence, **kwargs):
    """Find position of item satisfying predicate.
    
    Supports:
      :key - function to apply to each element before testing
      :start - start index
      :end - end index
    """
    # Convert lispCons to list for indexing
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)
    
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    
    for i in range(start, min(end, _seq_length(sequence))):
        element = sequence[i]
        test_val = key(element) if key else element
        if predicate(test_val):
            return i
    return None


@_registry.cl_function('POSITION-IF-NOT')
def position_if_not(predicate, sequence, **kwargs):
    """Find position of item not satisfying predicate.
    
    Supports:
      :key - function to apply to each element before testing
      :start - start index
      :end - end index
    """
    # Convert lispCons to list for indexing
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)
    
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    
    for i in range(start, min(end, _seq_length(sequence))):
        element = sequence[i]
        test_val = key(element) if key else element
        if not predicate(test_val):
            return i
    return None


@_registry.cl_function('COUNT')
def count(item, sequence, **kwargs):
    """Count occurrences of item.
    
    Supports:
      :key - function to apply to each element before comparison
      :test - comparison function (default is eql)
      :start - start index
      :end - end index
    """
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    test = kwargs.get('test', lambda x, y: x == y)
    
    iterator = iterate(sequence, start=start, end=end, key=key, test=test)
    count_val = 0
    for element in iterator:
        if iterator.matches(element, item):
            count_val += 1
    return count_val


@_registry.cl_function('COUNT-IF')
def count_if(predicate, sequence, **kwargs):
    """Count items satisfying predicate.
    
    Supports:
      :key - function to apply to each element before testing
      :start - start index
      :end - end index
    """
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    
    iterator = iterate(sequence, start=start, end=end, key=key)
    count_val = 0
    for element in iterator:
        test_value = iterator.get_value(element)
        if predicate(test_value):
            count_val += 1
    return count_val


@_registry.cl_function('COUNT-IF-NOT')
def count_if_not(predicate, sequence, **kwargs):
    """Count items not satisfying predicate."""
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)
    return sum(1 for x in sequence if not predicate(x))


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
    
    for x in list_seq:
        candidate = key(x) if key else x
        if test is not None:
            matched = lisptype.is_truthy(test(item, candidate))
        elif test_not is not None:
            matched = not lisptype.is_truthy(test_not(item, candidate))
        else:
            matched = candidate == item
        if matched:
            return list_seq[list_seq.index(x):]
    return None


@_registry.cl_function('MEMBER-IF')
def member_if(predicate, list_seq, key=None):
    """Find member satisfying predicate."""
    # Convert lispCons to list
    if hasattr(list_seq, 'car') and hasattr(list_seq, 'cdr'):
        list_seq = _seq_to_list(list_seq)
    
    for x in list_seq:
        if predicate(key(x) if key else x):
            return list_seq[list_seq.index(x):]
    return None


@_registry.cl_function('MEMBER-IF-NOT')
def member_if_not(predicate, list_seq, key=None):
    """Find member not satisfying predicate."""
    # Convert lispCons to list
    if hasattr(list_seq, 'car') and hasattr(list_seq, 'cdr'):
        list_seq = _seq_to_list(list_seq)
    
    for x in list_seq:
        if not predicate(key(x) if key else x):
            return list_seq[list_seq.index(x):]
    return None


@_registry.cl_function('ASSOC')
def assoc(item, alist, test=None, test_not=None, key=None):
    """Find association with key equal to item.
    
    Keyword arguments:
    - :test - equality test function (default is EQL)
    - :test-not - inequality test function
    - :key - function to apply to each key before testing
    """
    import fclpy.lisptype as lisptype
    
    # Handle :key parameter
    key_fn = key if callable(key) else None
    
    # Determine comparison function
    if test is not None and callable(test):
        test_fn = test
    elif test_not is not None and callable(test_not):
        test_fn = lambda a, b: not test_not(a, b)
    else:
        # Default to EQL
        test_fn = lambda a, b: a == b
    
    # Iterate through alist
    current = alist
    while current is not None and current is not lisptype.NIL:
        if hasattr(current, 'car') and hasattr(current, 'cdr'):
            pair = current.car
            if pair is not None and pair is not lisptype.NIL:
                if hasattr(pair, 'car'):
                    pair_key = pair.car
                else:
                    pair_key = pair[0] if len(pair) > 0 else None
                
                # Apply key function if provided
                test_key = key_fn(pair_key) if key_fn else pair_key
                
                if test_fn(item, test_key):
                    return pair
            current = current.cdr
        elif isinstance(current, (list, tuple)):
            for pair in current:
                if pair:
                    if hasattr(pair, 'car'):
                        pair_key = pair.car
                    else:
                        pair_key = pair[0] if len(pair) > 0 else None
                    
                    test_key = key_fn(pair_key) if key_fn else pair_key
                    
                    if test_fn(item, test_key):
                        return pair
            break
        else:
            break
    
    return lisptype.NIL


@_registry.cl_function('ASSOC-IF')
def assoc_if(predicate, alist):
    """Find association whose key satisfies predicate."""
    for pair in alist:
        if pair and predicate(pair[0]):
            return pair
    return None


@_registry.cl_function('ASSOC-IF-NOT')
def assoc_if_not(predicate, alist):
    """Find association whose key does not satisfy predicate."""
    for pair in alist:
        if pair and not predicate(pair[0]):
            return pair
    return None


@_registry.cl_function('RASSOC')
def rassoc(item, alist, test=None, test_not=None, key=None):
    """Reverse association - find by value."""
    for pair in alist:
        if pair and len(pair) > 1 and pair[1] == item:
            return pair
    return None


@_registry.cl_function('RASSOC-IF')
def rassoc_if(predicate, alist, key=None):
    """Reverse association with predicate."""
    for pair in alist:
        if pair and len(pair) > 1 and predicate(pair[1]):
            return pair
    return None


@_registry.cl_function('RASSOC-IF-NOT')
def rassoc_if_not(predicate, alist, key=None):
    """Reverse association with negated predicate."""
    for pair in alist:
        if pair and len(pair) > 1 and not predicate(pair[1]):
            return pair
    return None


__all__ = [
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
