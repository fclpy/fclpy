"""Sequence operations - lists, vectors, and other sequence manipulation."""

import functools
from .core import cons, car, cdr, atom
from . import registry as _registry
import fclpy.lisptype as lisptype


def endp(x):
    """Test if object is end of list (nil or empty)."""
    return lisptype.lisp_bool(x is None or x == lisptype.NIL)


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


@_registry.cl_function('APPEND')
def append(*args):
    """Append sequences together."""
    if len(args) < 1:
        return lisptype.NIL
    # Build a Lisp proper list for all but last arg, then splice the last
    head_elems = []
    for seq in args[:-1]:
        if seq is None or seq == lisptype.NIL:
            continue
        if isinstance(seq, lisptype.lispCons):
            # convert cons list to Python list
            cur = seq
            while cur is not None and cur != lisptype.NIL and isinstance(cur, lisptype.lispCons):
                head_elems.append(cur.car)
                cur = cur.cdr
        else:
            head_elems.extend(list(seq))
    # If no head elements, just return last
    last_part = args[-1]
    if not head_elems:
        return last_part
    # Build cons chain
    # Represent as Python list for simplicity (avoids cons type checking issues)
    result_list = list(head_elems)
    # If last_part is a Lisp list, attempt to extend, else append
    if isinstance(last_part, list):
        result_list.extend(last_part)
    else:
        result_list.append(last_part)
    return result_list


@_registry.cl_function('ADJOIN')
def adjoin(x, seq, test=lambda x, y: x is y):
    """Tests whether item is the same as an existing element of list."""
    return seq if any(map(functools.partial(test, x), seq)) else cons(x, seq)


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


@_registry.cl_function('PAIRLIS')
def pairlis(keys, data, alist=None):
    """Create alist from keys and data."""
    result = []
    for key, datum in zip(keys, data):
        result.append((key, datum))
    if alist:
        result.extend(alist)
    return result


@_registry.cl_function('ACONS')
def acons(key, datum, alist):
    """Add key-datum pair to alist."""
    return [(key, datum)] + list(alist) if alist else [(key, datum)]


@_registry.cl_function('SUBST')
def subst(new, old, tree, test=None):
    """Substitute old with new in tree."""
    if test is None:
        test = lambda x, y: x == y
    
    if test(tree, old):
        return new
    elif atom(tree):
        return tree
    else:
        return cons(subst(new, old, car(tree), test),
                   subst(new, old, cdr(tree), test))


@_registry.cl_function('SUBST-IF')
def subst_if(new, predicate, tree):
    """Substitute with predicate."""
    if predicate(tree):
        return new
    elif atom(tree):
        return tree
    else:
        return cons(subst_if(new, predicate, car(tree)),
                   subst_if(new, predicate, cdr(tree)))


@_registry.cl_function('SUBST-IF-NOT')
def subst_if_not(new, predicate, tree):
    """Substitute with negated predicate."""
    return subst_if(new, lambda x: not predicate(x), tree)


@_registry.cl_function('SUBLIS')
def sublis(alist, tree, test=None):
    """Substitute using association list."""
    if test is None:
        test = lambda x, y: x == y
    
    if atom(tree):
        for pair in alist:
            if pair and len(pair) > 1 and test(tree, pair[0]):
                return pair[1]
        return tree
    else:
        return cons(sublis(alist, car(tree), test),
                   sublis(alist, cdr(tree), test))


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


@_registry.cl_function('FIND')
def find(item, sequence, **kwargs):
    """Find item in sequence."""
    for x in sequence:
        if x == item:
            return x
    return None


@_registry.cl_function('FIND-IF')
def find_if(predicate, sequence, **kwargs):
    """Find item satisfying predicate."""
    for x in sequence:
        if predicate(x):
            return x
    return None


@_registry.cl_function('FIND-IF-NOT')
def find_if_not(predicate, sequence, **kwargs):
    """Find item not satisfying predicate."""
    for x in sequence:
        if not predicate(x):
            return x
    return None


@_registry.cl_function('POSITION')
def position(item, sequence, **kwargs):
    """Find position of item."""
    try:
        return sequence.index(item)
    except ValueError:
        return None


@_registry.cl_function('POSITION-IF')
def position_if(predicate, sequence, **kwargs):
    """Find position of item satisfying predicate."""
    for i, x in enumerate(sequence):
        if predicate(x):
            return i
    return None


@_registry.cl_function('POSITION-IF-NOT')
def position_if_not(predicate, sequence, **kwargs):
    """Find position of item not satisfying predicate."""
    for i, x in enumerate(sequence):
        if not predicate(x):
            return i
    return None


@_registry.cl_function('COUNT')
def count(item, sequence, **kwargs):
    """Count occurrences of item."""
    return sum(1 for x in sequence if x == item)


@_registry.cl_function('COUNT-IF')
def count_if(predicate, sequence, **kwargs):
    """Count items satisfying predicate."""
    return sum(1 for x in sequence if predicate(x))


@_registry.cl_function('COUNT-IF-NOT')
def count_if_not(predicate, sequence, **kwargs):
    """Count items not satisfying predicate."""
    return sum(1 for x in sequence if not predicate(x))


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
    else:
        return len(sequence)


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
    return [initial_element] * size


@_registry.cl_function('LIST')
def list_fn(*args):
    """Create list from arguments."""
    return list(args)


@_registry.cl_function('LIST')
def lisp_list(*args):
    """Create Lisp list from arguments."""
    return list(args)


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


@_registry.cl_function('CONCATENATE')
def concatenate(result_type, *sequences):
    """Concatenate sequences."""
    if result_type == 'LIST' or result_type == list:
        result = []
        for seq in sequences:
            result.extend(seq)
        return result
    elif result_type == 'STRING' or result_type == str:
        return ''.join(str(seq) for seq in sequences)
    elif result_type == 'VECTOR' or result_type == 'SIMPLE-VECTOR':
        result = []
        for seq in sequences:
            result.extend(seq)
        return result
    else:
        raise lisptype.LispTypeError(f"CONCATENATE: unsupported result type {result_type}",
                                    expected_type="LIST, STRING, or VECTOR",
                                    actual_value=result_type)


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


@_registry.cl_function('REMOVE')
def remove(item, sequence, **kwargs):
    """Remove item from sequence."""
    return [x for x in sequence if x != item]


@_registry.cl_function('REMOVE-IF')
def remove_if(test, sequence, **kwargs):
    """Remove elements satisfying test."""
    return [x for x in sequence if not test(x)]


@_registry.cl_function('REMOVE-IF-NOT')
def remove_if_not(test, sequence, **kwargs):
    """Remove elements not satisfying test."""
    return [x for x in sequence if test(x)]


@_registry.cl_function('DELETE')
def delete_fn(item, sequence, **kwargs):
    """Delete item from sequence."""
    return [x for x in sequence if x != item]


@_registry.cl_function('DELETE-IF')
def delete_if(predicate, sequence, **kwargs):
    """Delete if predicate true."""
    return [x for x in sequence if not predicate(x)]


@_registry.cl_function('DELETE-IF-NOT')
def delete_if_not(predicate, sequence, **kwargs):
    """Delete if predicate false."""
    return [x for x in sequence if predicate(x)]


@_registry.cl_function('REMOVE-DUPLICATES')
def remove_duplicates(sequence, **kwargs):
    """Remove duplicate elements."""
    seen = set()
    result = []
    for item in sequence:
        if item not in seen:
            seen.add(item)
            result.append(item)
    return result


@_registry.cl_function('DELETE-DUPLICATES')
def delete_duplicates(sequence, **kwargs):
    """Delete duplicate elements."""
    return remove_duplicates(sequence, **kwargs)


@_registry.cl_function('SUBSTITUTE')
def substitute(newitem, olditem, sequence, **kwargs):
    """Substitute elements in sequence."""
    return [newitem if x == olditem else x for x in sequence]


@_registry.cl_function('SUBSTITUTE-IF')
def substitute_if(newitem, test, sequence, **kwargs):
    """Substitute using predicate."""
    return [newitem if test(x) else x for x in sequence]


@_registry.cl_function('SUBSTITUTE-IF-NOT')
def substitute_if_not(newitem, test, sequence, **kwargs):
    """Substitute using negated predicate."""
    return [newitem if not test(x) else x for x in sequence]


@_registry.cl_function('NSUBSTITUTE')
def nsubstitute(newitem, olditem, sequence, **kwargs):
    """Destructively substitute."""
    return substitute(newitem, olditem, sequence, **kwargs)  # Non-destructive for now


@_registry.cl_function('NSUBSTITUTE-IF')
def nsubstitute_if(newitem, test, sequence, **kwargs):
    """Destructively substitute using predicate."""
    return substitute_if(newitem, test, sequence, **kwargs)  # Non-destructive for now


@_registry.cl_function('NSUBSTITUTE-IF-NOT')
def nsubstitute_if_not(newitem, test, sequence, **kwargs):
    """Destructively substitute using negated predicate."""
    return substitute_if_not(newitem, test, sequence, **kwargs)  # Non-destructive for now


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


@_registry.cl_function('EVERY')
def every(predicate, *sequences):
    """Test if predicate is true for every element."""
    if not sequences:
        return lisptype.T
    min_len = min(len(seq) for seq in sequences)
    for i in range(min_len):
        args = [seq[i] for seq in sequences]
        if not predicate(*args):
            return lisptype.NIL
    return lisptype.T


@_registry.cl_function('SOME')
def some(predicate, *sequences):
    """Test if predicate is true for some element."""
    if not sequences:
        return lisptype.NIL
    min_len = min(len(seq) for seq in sequences)
    for i in range(min_len):
        args = [seq[i] for seq in sequences]
        if predicate(*args):
            return lisptype.T
    return lisptype.NIL


@_registry.cl_function('NOTEVERY')
def notevery(predicate, *sequences):
    """Test if predicate is false for some element."""
    ev = every(predicate, *sequences)
    return lisptype.NIL if ev == lisptype.T else lisptype.T


@_registry.cl_function('NOTANY')
def notany(predicate, *sequences):
    """Test if predicate is false for all elements."""
    sv = some(predicate, *sequences)
    return lisptype.NIL if sv == lisptype.T else lisptype.T


@_registry.cl_function('MAP')
def map_fn(result_type, function, *sequences):
    """Map function over sequences."""
    if not sequences:
        return []
    
    min_len = min(len(seq) for seq in sequences)
    results = []
    
    for i in range(min_len):
        args = [seq[i] for seq in sequences]
        results.append(function(*args))
    
    if result_type is None:
        return None
    elif result_type == 'LIST':
        return results
    else:
        return results


@_registry.cl_function('MAPCAR')
def mapcar(function, *lists):
    """Map function over lists."""
    return map_fn('LIST', function, *lists)


@_registry.cl_function('MAPCAN')
def mapcan(function, *lists):
    """Map and concatenate results."""
    results = mapcar(function, *lists)
    flattened = []
    for result in results:
        if isinstance(result, list):
            flattened.extend(result)
        else:
            flattened.append(result)
    return flattened


@_registry.cl_function('MAPC')
def mapc(function, *lists):
    """Map for side effects."""
    map_fn(None, function, *lists)
    return lists[0] if lists else None


@_registry.cl_function('MAPCON')
def mapcon(function, *lists):
    """Map over cdrs and concatenate."""
    return mapcan(function, *lists)  # Simplified


@_registry.cl_function('MAPLIST')
def maplist(function, *lists):
    """Map over lists as lists."""
    return mapcar(function, *lists)  # Simplified


@_registry.cl_function('MAPL')
def mapl(function, *lists):
    """Map over lists for side effects."""
    return mapc(function, *lists)


# Set operations
@_registry.cl_function('INTERSECTION')
def intersection(list1, list2, **kwargs):
    """Set intersection."""
    return [x for x in list1 if x in list2]


@_registry.cl_function('UNION')
def union(list1, list2, **kwargs):
    """Set union."""
    result = list(list1)
    for item in list2:
        if item not in result:
            result.append(item)
    return result


@_registry.cl_function('NUNION')
def nunion(list1, list2, **kwargs):
    """Destructive set union."""
    for item in list2:
        if item not in list1:
            list1.append(item)
    return list1


@_registry.cl_function('SET-DIFFERENCE')
def set_difference(list1, list2, **kwargs):
    """Set difference."""
    return [x for x in list1 if x not in list2]


@_registry.cl_function('NSET-DIFFERENCE')
def nset_difference(list1, list2, **kwargs):
    """Destructive set difference."""
    for item in list2:
        while item in list1:
            list1.remove(item)
    return list1


@_registry.cl_function('SET-EXCLUSIVE-OR')
def set_exclusive_or(list1, list2, **kwargs):
    """Set exclusive or."""
    return [x for x in list1 if x not in list2] + [x for x in list2 if x not in list1]


@_registry.cl_function('NSET-EXCLUSIVE-OR')
def nset_exclusive_or(list1, list2, **kwargs):
    """Destructive set exclusive or."""
    # Remove items from list1 that are in list2
    for item in list2:
        while item in list1:
            list1.remove(item)
    # Add items from list2 that are not already in list1
    for item in list2:
        if item not in list1:
            list1.append(item)
    return list1


@_registry.cl_function('SUBSETP')
def subsetp(subset, set_arg, **kwargs):
    """Test if subset is a subset of set_arg."""
    for item in subset:
        if item not in set_arg:
            return lisptype.NIL
    return lisptype.T


@_registry.cl_function('LIST*')
def list_s_star_(*args):
    """LIST* function - creates a dotted list."""
    if not args:
        return None
    if len(args) == 1:
        return args[0]
    return cons(args[0], list_s_star_(*args[1:]))


# Array operations
@_registry.cl_function('MAKE-ARRAY')
def make_array(dimensions, **kwargs):
    """Create array."""
    if isinstance(dimensions, int):
        return [None] * dimensions
    # Multi-dimensional array - for now, nested lists
    def make_nested(dims):
        if len(dims) == 1:
            return [None] * dims[0]
        return [make_nested(dims[1:]) for _ in range(dims[0])]
    return make_nested(dimensions)


@_registry.cl_function('ARRAY-DIMENSIONS')
def array_dimensions(array):
    """Get array dimensions."""
    if isinstance(array, list):
        dims = [len(array)]
        if array and isinstance(array[0], list):
            dims.extend(array_dimensions(array[0]))
        return dims
    return [1]


@_registry.cl_function('ARRAYP')
def arrayp(object):
    """Test if object is array."""
    return lisptype.lisp_bool(isinstance(object, list))


@_registry.cl_function('ARRAY-IN-BOUNDS-P')
def array_in_bounds_p(array, *subscripts):
    """Test if subscripts are valid for array."""
    try:
        dims = array_dimensions(array)
        if len(subscripts) != len(dims):
            return lisptype.NIL
        for i, sub in enumerate(subscripts):
            if sub < 0 or sub >= dims[i]:
                return lisptype.NIL
        return lisptype.T
    except:
        return lisptype.NIL


@_registry.cl_function('ARRAY-DISPLACEMENT')
def array_displacement(array):
    """Return array displacement info."""
    # In Python, arrays are not displaced, so return None and 0
    return None, 0


@_registry.cl_function('ARRAY-DIMENSION')
def array_dimension(array, axis_number):
    """Get specific array dimension."""
    try:
        dimensions = array_dimensions(array)
        if axis_number < 0 or axis_number >= len(dimensions):
            raise IndexError("Invalid axis number")
        return dimensions[axis_number]
    except:
        return 1


@_registry.cl_function('ADJUST-ARRAY')
def adjust_array(array, new_dimensions, **kwargs):
    """Adjust array to new dimensions."""
    # Simple implementation - create new array with new dimensions
    # This is a simplified version
    if isinstance(new_dimensions, int):
        return [None] * new_dimensions
    # For multi-dimensional arrays, delegate to make_array
    return make_array(new_dimensions, **kwargs)


@_registry.cl_function('VECTORP')
def vectorp(object):
    """Test if object is vector."""
    return lisptype.lisp_bool(isinstance(object, list))


@_registry.cl_function('SIMPLE-VECTOR-P')
def simple_vector_p(object):
    """Test if object is simple vector."""
    return lisptype.lisp_bool(isinstance(object, list))


@_registry.cl_function('BIT-VECTOR-P')
def bit_vector_p(object):
    """Test if object is bit vector."""
    return lisptype.lisp_bool(isinstance(object, list) and all(x in (0, 1) for x in object))


@_registry.cl_function('SIMPLE-BIT-VECTOR-P')
def simple_bit_vector_p(object):
    """Test if object is simple bit vector."""
    return bit_vector_p(object)


@_registry.cl_function('AREF')
def aref(array, *subscripts):
    """Array reference."""
    result = array
    for subscript in subscripts:
        result = result[subscript]
    return result


@_registry.cl_function('SVREF')
def svref(vector, index):
    """Simple vector reference."""
    return vector[index]


@_registry.cl_function('VECTOR')
def vector_fn(*elements):
    """Create vector from elements."""
    return list(elements)


@_registry.cl_function('VECTOR-POP')
def vector_pop(vector):
    """Pop from end of vector."""
    if vector:
        return vector.pop()
    return None


@_registry.cl_function('VECTOR-PUSH')
def vector_push(new_element, vector):
    """Push to end of vector."""
    vector.append(new_element)
    return len(vector) - 1


@_registry.cl_function('VECTOR-PUSH-EXTEND')
def vector_push_extend(new_element, vector, extension=None):
    """Push with possible extension."""
    vector.append(new_element)
    return len(vector) - 1


# Additional destructive operations
@_registry.cl_function('NINTERSECTION')
def nintersection(list1, list2, **kwargs):
    """Destructive intersection."""
    return [x for x in list1 if x in list2]


@_registry.cl_function('NSUBST')
def nsubst(new, old, tree, **kwargs):
    """Destructive substitute in tree."""
    return subst(new, old, tree)  # Non-destructive for now


@_registry.cl_function('NSUBST-IF')
def nsubst_if(new, predicate, tree, **kwargs):
    """Destructive substitute if in tree."""
    return subst_if(new, predicate, tree)  # Non-destructive for now


@_registry.cl_function('NSUBST-IF-NOT')
def nsubst_if_not(new, predicate, tree, **kwargs):
    """Destructive substitute if not in tree."""
    return subst_if_not(new, predicate, tree)  # Non-destructive for now


@_registry.cl_function('NSUBLIS')
def nsublis(alist, tree, **kwargs):
    """Destructive substitute using alist."""
    return sublis(alist, tree)  # Non-destructive for now


# Aliased functions for compatibility
@_registry.cl_function('EVERY')
def every_fn(predicate, *sequences):
    """Test if predicate is true for every element."""
    return every(predicate, *sequences)


@_registry.cl_function('SOME')
def some_fn(predicate, *sequences):
    """Test if predicate is true for some element."""
    return some(predicate, *sequences)


# === BATCH: Missing Functions for Complete lispenv.py Integration ===

@_registry.cl_function('MAKE-SEQUENCE')
def make_sequence(sequence_type, size, **kwargs):
    """Create a sequence of the specified type and size."""
    initial_element = kwargs.get('initial_element', None)
    if sequence_type == 'list' or sequence_type == list:
        return [initial_element] * size
    elif sequence_type == 'vector' or sequence_type == 'string':
        if initial_element is None:
            return [None] * size
        return [initial_element] * size
    else:
        return [initial_element] * size


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


@_registry.cl_function('POP')
def pop_fn(place):
    """Remove and return first element of list."""
    if isinstance(place, list) and place:
        return place.pop(0)
    return None


@_registry.cl_function('PUSH')
def push_fn(item, place):
    """Add item to front of list."""
    if isinstance(place, list):
        place.insert(0, item)
        return place
    return [item]


@_registry.cl_function('PUSHNEW')
def pushnew(item, place, **kwargs):
    """Add item to front of list if not already present."""
    if isinstance(place, list):
        if item not in place:
            place.insert(0, item)
        return place
    return [item]


@_registry.cl_function('REDUCE')
def reduce_fn(function, sequence, **kwargs):
    """Reduce sequence using function."""
    if not sequence:
        if 'initial_value' in kwargs:
            return kwargs['initial_value']
        return function()
    
    result = sequence[0] if 'initial_value' not in kwargs else kwargs['initial_value']
    start_idx = 1 if 'initial_value' not in kwargs else 0
    
    for item in sequence[start_idx:]:
        result = function(result, item)
    return result


@_registry.cl_function('REPLACE')
def replace_fn(sequence1, sequence2, **kwargs):
    """Replace elements of sequence1 with elements from sequence2."""
    start1 = kwargs.get('start1', 0)
    end1 = kwargs.get('end1', len(sequence1))
    start2 = kwargs.get('start2', 0)
    end2 = kwargs.get('end2', len(sequence2))
    
    result = list(sequence1)
    src_slice = sequence2[start2:end2]
    
    for i, item in enumerate(src_slice):
        if start1 + i < end1 and start1 + i < len(result):
            result[start1 + i] = item
    
    return result


@_registry.cl_function('REVAPPEND')
def revappend(list1, list2):
    """Append reversed list1 to list2."""
    if isinstance(list1, list):
        return list(reversed(list1)) + list(list2)
    return list(list2)


@_registry.cl_function('SEARCH')
def search_fn(sequence1, sequence2, **kwargs):
    """Search for subsequence in sequence."""
    test = kwargs.get('test', lambda x, y: x == y)
    start2 = kwargs.get('start2', 0)
    end2 = kwargs.get('end2', len(sequence2))
    
    seq1_len = len(sequence1)
    if seq1_len == 0:
        return start2
    
    for i in range(start2, end2 - seq1_len + 1):
        match = True
        for j in range(seq1_len):
            if not test(sequence1[j], sequence2[i + j]):
                match = False
                break
        if match:
            return i
    
    return None


@_registry.cl_function('SORT')
def sort_fn(sequence, predicate=None, **kwargs):
    """Sort sequence using predicate."""
    key_func = kwargs.get('key')
    if predicate is None:
        predicate = lambda x, y: x < y
    
    def compare_func(x, y):
        kx = key_func(x) if key_func else x
        ky = key_func(y) if key_func else y
        if predicate(kx, ky):
            return -1
        elif predicate(ky, kx):
            return 1
        else:
            return 0
    
    from functools import cmp_to_key
    result = list(sequence)
    result.sort(key=cmp_to_key(compare_func))
    return result
