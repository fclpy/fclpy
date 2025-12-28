"""Sequence search and query operations."""

import functools
from .core import cons, car, cdr, atom
from . import registry as _registry
import fclpy.lisptype as lisptype


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
        self.sequence = sequence
        self.start = start
        self.end = end if end is not None else len(sequence)
        self.key = key
        self.test = test or (lambda x, y: x == y)
        self.index = start
    
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
    if not isinstance(sequence, (list, str, tuple)):
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
    end = kwargs.get('end', len(sequence))
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
    end = kwargs.get('end', len(sequence))
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
    end = kwargs.get('end', len(sequence))
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
    start = kwargs.get('start', 0)
    end = kwargs.get('end', len(sequence))
    key = kwargs.get('key', None)
    test = kwargs.get('test', lambda x, y: x == y)
    
    for i in range(start, min(end, len(sequence))):
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
    start = kwargs.get('start', 0)
    end = kwargs.get('end', len(sequence))
    key = kwargs.get('key', None)
    
    for i in range(start, min(end, len(sequence))):
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
    start = kwargs.get('start', 0)
    end = kwargs.get('end', len(sequence))
    key = kwargs.get('key', None)
    
    for i in range(start, min(end, len(sequence))):
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
    end = kwargs.get('end', len(sequence))
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
    end = kwargs.get('end', len(sequence))
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
    return sum(1 for x in sequence if not predicate(x))


@_registry.cl_function('SEARCH')
def search(sequence1, sequence2, **kwargs):
    """Search for sequence1 in sequence2."""
    for i in range(len(sequence2) - len(sequence1) + 1):
        if sequence2[i:i+len(sequence1)] == sequence1:
            return i
    return None


@_registry.cl_function('MISMATCH')
def mismatch(sequence1, sequence2, **kwargs):
    """Find first mismatch between sequences."""
    for i, (x, y) in enumerate(zip(sequence1, sequence2)):
        if x != y:
            return i
    if len(sequence1) != len(sequence2):
        return min(len(sequence1), len(sequence2))
    return None


@_registry.cl_function('MEMBER')
def member(item, list_seq, test=None, test_not=None, key=None):
    """Find member in list."""
    for x in list_seq:
        if (key(x) if key else x) == item:
            return list_seq[list_seq.index(x):]
    return None


@_registry.cl_function('MEMBER-IF')
def member_if(predicate, list_seq, key=None):
    """Find member satisfying predicate."""
    for x in list_seq:
        if predicate(key(x) if key else x):
            return list_seq[list_seq.index(x):]
    return None


@_registry.cl_function('MEMBER-IF-NOT')
def member_if_not(predicate, list_seq, key=None):
    """Find member not satisfying predicate."""
    for x in list_seq:
        if not predicate(key(x) if key else x):
            return list_seq[list_seq.index(x):]
    return None


@_registry.cl_function('ASSOC')
def assoc(item, alist):
    """Find association with key equal to item."""
    for pair in alist:
        if pair and pair[0] == item:
            return pair
    return None


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
