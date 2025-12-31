"""Sequence composition, sorting, and utility operations."""

import functools
from .core import cons, car, cdr, atom
from . import registry as _registry
from .vectors import AdjustableVector
import fclpy.lisptype as lisptype


def endp(x):
    """Test if object is end of list (nil or empty)."""
    return lisptype.lisp_bool(x is None or x == lisptype.NIL)


@_registry.cl_function('LENGTH')
def length(sequence):
    """Get sequence length."""
    if sequence is None or sequence == lisptype.NIL:
        return 0
    elif isinstance(sequence, lisptype.lispCons):
        count = 0
        current = sequence
        while current is not None and current != lisptype.NIL:
            if not isinstance(current, lisptype.lispCons):
                break
            count += 1
            current = current.cdr
        return count
    elif isinstance(sequence, (str, list, tuple, AdjustableVector)):
        return len(sequence)
    else:
        raise lisptype.LispTypeError(f"LENGTH: {type(sequence).__name__} is not a sequence",
                                    expected_type="SEQUENCE", actual_value=sequence)


@_registry.cl_function('REVERSE')
def reverse(sequence):
    """Reverse sequence."""
    if sequence is None or sequence == lisptype.NIL:
        return lisptype.NIL
    elif isinstance(sequence, lisptype.lispCons):
        result_list = []
        current = sequence
        while current is not None and current != lisptype.NIL and isinstance(current, lisptype.lispCons):
            result_list.append(current.car)
            current = current.cdr
        return list(reversed(result_list))
    else:
        return list(reversed(sequence))


@_registry.cl_function('NREVERSE')
def nreverse(sequence):
    """Destructively reverse sequence."""
    return reverse(sequence)  # Non-destructive for now


@_registry.cl_function('APPEND')
def append(*args):
    """Append sequences together.
    
    In Common Lisp, APPEND copies all arguments except the last,
    then returns the last argument. The result shares structure 
    with the last argument.
    """
    if len(args) < 1:
        return lisptype.NIL
    
    # Collect all elements
    head_elems = []
    
    # Process all arguments except the last
    for seq in args[:-1]:
        if seq is None or seq == lisptype.NIL:
            continue
        if isinstance(seq, lisptype.lispCons):
            # Convert cons list to Python list
            cur = seq
            while cur is not None and cur != lisptype.NIL and isinstance(cur, lisptype.lispCons):
                head_elems.append(cur.car)
                cur = cur.cdr
        elif isinstance(seq, (list, tuple)):
            head_elems.extend(seq)
        else:
            # Single element
            head_elems.append(seq)
    
    # Handle the last argument - this needs special treatment
    last_part = args[-1]
    
    if not head_elems:
        # No elements from previous args, just return last
        return last_part
    
    # Convert last_part to elements list if it's a Lisp cons list
    if isinstance(last_part, lisptype.lispCons):
        cur = last_part
        while cur is not None and cur != lisptype.NIL and isinstance(cur, lisptype.lispCons):
            head_elems.append(cur.car)
            cur = cur.cdr
    elif isinstance(last_part, (list, tuple)):
        head_elems.extend(last_part)
    elif last_part is None or last_part == lisptype.NIL:
        pass  # NIL at the end is fine
    else:
        head_elems.append(last_part)
    
    # Build proper Lisp cons list from elements
    result = lisptype.NIL
    for elem in reversed(head_elems):
        result = lisptype.lispCons(elem, result)
    
    return result


@_registry.cl_function('NCONC')
def nconc(*lists):
    """Destructive concatenation of lists."""
    if not lists:
        return []
    result = lists[0]
    for lst in lists[1:]:
        if lst:
            result.extend(lst)
    return result


@_registry.cl_function('NRECONC')
def nreconc(list1, list2):
    """Destructively reverse list1 and concatenate with list2."""
    if isinstance(list1, list):
        list1.reverse()
        list1.extend(list2)
        return list1
    return list2


@_registry.cl_function('REVAPPEND')
def revappend(list1, list2):
    """Append reversed list1 to list2."""
    if isinstance(list1, list):
        return list(reversed(list1)) + list(list2)
    return list(list2)


@_registry.cl_function('CONCATENATE')
def concatenate(result_type, *sequences):
    """Concatenate sequences."""
    # Get the type name as a string for comparison
    type_name = result_type
    if isinstance(result_type, lisptype.LispSymbol):
        type_name = result_type.name
    elif hasattr(result_type, '__name__'):
        type_name = result_type.__name__
    
    # Normalize type name to uppercase string
    if isinstance(type_name, str):
        type_name = type_name.upper()
    
    if type_name == 'LIST' or result_type == list:
        result = []
        for seq in sequences:
            result.extend(seq)
        return result
    elif type_name == 'STRING' or result_type == str:
        # For STRING result, concatenate all elements as strings
        result_parts = []
        for seq in sequences:
            if isinstance(seq, str):
                result_parts.append(seq)
            else:
                # For non-string sequences, convert each element to a character
                for elem in seq:
                    if isinstance(elem, str) and len(elem) == 1:
                        result_parts.append(elem)
                    elif isinstance(elem, int):
                        # Could be a character code
                        result_parts.append(chr(elem))
                    else:
                        result_parts.append(str(elem))
        return ''.join(result_parts)
    elif type_name in ('VECTOR', 'SIMPLE-VECTOR'):
        result = []
        for seq in sequences:
            result.extend(seq)
        return result
    else:
        raise lisptype.LispTypeError(f"CONCATENATE: unsupported result type {result_type}",
                                    expected_type="LIST, STRING, or VECTOR",
                                    actual_value=result_type)


@_registry.cl_function('SORT')
def sort(sequence, predicate, key=None):
    """Sort sequence using a two-arg predicate returning truthy when first < second.

    Python 3 removed the cmp parameter, so we translate the predicate into a comparator
    via cmp_to_key. If key is provided we apply it before comparisons.
    """
    from functools import cmp_to_key
    def cmp(a, b):
        a_key = key(a) if key else a
        b_key = key(b) if key else b
        if predicate(a_key, b_key):
            return -1
        if predicate(b_key, a_key):
            return 1
        return 0
    return sorted(sequence, key=cmp_to_key(cmp))


@_registry.cl_function('STABLE-SORT')
def stable_sort(sequence, predicate, key=None):
    """Stable sort sequence."""
    return sort(sequence, predicate, key)  # Python's sort is stable


@_registry.cl_function('MERGE')
def merge(result_type, sequence1, sequence2, predicate, **kwargs):
    """Merge two sorted sequences."""
    result = []
    i, j = 0, 0
    
    while i < len(sequence1) and j < len(sequence2):
        if predicate(sequence1[i], sequence2[j]):
            result.append(sequence1[i])
            i += 1
        else:
            result.append(sequence2[j])
            j += 1
    
    # Add remaining elements
    result.extend(sequence1[i:])
    result.extend(sequence2[j:])
    
    return result


@_registry.cl_function('SUBSEQ')
def subseq(sequence, start, end=None):
    """Get subsequence."""
    if end is None:
        return sequence[start:]
    else:
        return sequence[start:end]


@_registry.cl_function('COPY-SEQ')
def copy_seq(sequence):
    """Copy sequence."""
    if isinstance(sequence, list):
        return list(sequence)
    elif isinstance(sequence, tuple):
        return tuple(sequence)
    elif isinstance(sequence, str):
        return str(sequence)
    else:
        return sequence


@_registry.cl_function('COPY-LIST')
def copy_list(list_seq):
    """Copy list."""
    return list(list_seq) if list_seq else []


@_registry.cl_function('COPY-ALIST')
def copy_alist(alist):
    """Copy association list."""
    return [list(pair) if isinstance(pair, (list, tuple)) else pair for pair in alist]


@_registry.cl_function('FILL')
def fill(sequence, item, start=0, end=None):
    """Fill sequence with item."""
    if isinstance(sequence, list):
        if end is None:
            end = len(sequence)
        for i in range(start, min(end, len(sequence))):
            sequence[i] = item
        return sequence
    else:
        raise lisptype.LispTypeError("FILL: unsupported sequence type",
                                    expected_type="LIST or MUTABLE-SEQUENCE",
                                    actual_value=type(sequence).__name__)


@_registry.cl_function('REPLACE')
def replace(sequence1, sequence2, **kwargs):
    """Replace elements of sequence1 with elements of sequence2."""
    start1 = kwargs.get('start1', 0)
    end1 = kwargs.get('end1', len(sequence1))
    start2 = kwargs.get('start2', 0)
    end2 = kwargs.get('end2', len(sequence2))
    
    for i, j in zip(range(start1, end1), range(start2, end2)):
        if i < len(sequence1) and j < len(sequence2):
            sequence1[i] = sequence2[j]
    
    return sequence1


@_registry.cl_function('NBUTLAST')
def nbutlast(seq, n=1):
    """Destructively remove last n elements."""
    if isinstance(seq, list):
        for _ in range(n):
            if seq:
                seq.pop()
        return seq
    else:
        # For other sequence types, return a new sequence
        return seq[:-n] if len(seq) > n else []


@_registry.cl_function('LAST')
def last(list_seq, n=1):
    """Get last n elements."""
    if not list_seq:
        return None
    
    if isinstance(list_seq, list):
        if n == 1:
            return list_seq[-1:]
        else:
            return list_seq[-n:] if len(list_seq) >= n else list_seq
    else:
        # For other sequences
        return list_seq[-n:] if len(list_seq) >= n else list_seq


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
    """Get element at index."""
    try:
        return sequence[index]
    except (IndexError, TypeError):
        return None


@_registry.cl_function('MAKE-LIST')
def make_list(size, initial_element=None):
    """Make list of given size."""
    # Ensure size is an integer
    if isinstance(size, lisptype.lispCons):
        size = size.car
    size = int(size)
    return [initial_element] * size


@_registry.cl_function('MAKE-SEQUENCE')
def make_sequence(sequence_type, size, **kwargs):
    """Create a sequence of the specified type and size."""
    # Ensure size is an integer
    if isinstance(size, lisptype.lispCons):
        size = size.car
    size = int(size)
    initial_element = kwargs.get('initial_element', None)
    if sequence_type == 'list' or sequence_type == list:
        return [initial_element] * size
    elif sequence_type == 'vector' or sequence_type == 'string':
        if initial_element is None:
            return [None] * size
        return [initial_element] * size
    else:
        return [initial_element] * size


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


@_registry.cl_function('LIST*')
def list_star(*args):
    """Create dotted list."""
    if not args:
        return lisptype.NIL
    if len(args) == 1:
        return args[0]
    
    # Build dotted-like list using Python list ending with final element if not list
    prefix = list(args[:-1])
    last = args[-1]
    if isinstance(last, list):
        return prefix + last
    else:
        prefix.append(last)
        return prefix


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
    'fill', 'replace', 'nbutlast', 'last',
    'nthcdr', 'nth', 'elt', 'make_list', 'make_sequence',
    'list_fn', 'list_star', 'tree_equal', 'list_length',
]
