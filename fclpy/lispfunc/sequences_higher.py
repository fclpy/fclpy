"""Higher-order sequence operations, arrays, and set operations."""

from .core import cons, car, cdr, atom
from . import registry as _registry
import fclpy.lisptype as lisptype
# Import make_array from vectors to avoid circular dependency
from .vectors import make_array
from .sequences_search import _make_matcher


def _cons_to_list(seq):
    """Convert a Lisp cons list to a Python list.
    
    If seq is already a Python list/tuple, return it as-is.
    If seq is NIL, return empty list.
    If seq is a cons list, convert to Python list.
    """
    if isinstance(seq, (list, tuple)):
        return list(seq)
    if seq is None or seq == lisptype.NIL:
        return []
    if isinstance(seq, lisptype.lispCons):
        result = []
        cur = seq
        while cur is not None and cur != lisptype.NIL and isinstance(cur, lisptype.lispCons):
            result.append(cur.car)
            cur = cur.cdr
        return result
    # Single element - wrap in list
    return [seq]


def _matcher_contains(matcher, item, seq):
    """True if `item` matches some element of `seq` under `matcher`."""
    return any(matcher(item, x) for x in seq)


# Association list operations
@_registry.cl_function('ADJOIN')
def adjoin(x, seq, test=None, test_not=None, key=None, **kwargs):
    """Tests whether item is the same as an existing element of list.

    Supports :test/:test-not/:key like every other CLHS "two-argument
    test" sequence function (default is eql-like equality); previously
    ignored :key entirely and hardcoded `is` for :test.
    """
    matcher = _make_matcher(test=test, test_not=test_not, key=key)
    seq_list = _cons_to_list(seq)
    return seq if _matcher_contains(matcher, x, seq_list) else cons(x, seq)


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


# Predicate tests on sequences
@_registry.cl_function('EVERY')
def every(predicate, *sequences):
    """Test if predicate is true for every element."""
    if not sequences:
        return lisptype.T
    # Convert all sequences to Python lists to handle lispCons
    py_seqs = [_cons_to_list(seq) for seq in sequences]
    if not py_seqs or not all(py_seqs):
        return lisptype.T
    min_len = min(len(seq) for seq in py_seqs)
    for i in range(min_len):
        args = [seq[i] for seq in py_seqs]
        if not predicate(*args):
            return lisptype.NIL
    return lisptype.T


@_registry.cl_function('SOME')
def some(predicate, *sequences):
    """Test if predicate is true for some element."""
    if not sequences:
        return lisptype.NIL
    # Convert all sequences to Python lists to handle lispCons
    py_seqs = [_cons_to_list(seq) for seq in sequences]
    if not py_seqs or not all(py_seqs):
        return lisptype.NIL
    min_len = min(len(seq) for seq in py_seqs)
    for i in range(min_len):
        args = [seq[i] for seq in py_seqs]
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


# Mapping operations
@_registry.cl_function('MAP')
def map_fn(result_type, function, *sequences):
    """Map function over sequences."""
    if not sequences:
        return []
    
    # Convert all sequences to Python lists to handle lispCons
    py_seqs = [_cons_to_list(seq) for seq in sequences]
    if not py_seqs or not all(py_seqs):
        if result_type is None:
            return None
        return []
    
    min_len = min(len(seq) for seq in py_seqs)
    results = []
    
    for i in range(min_len):
        args = [seq[i] for seq in py_seqs]
        results.append(function(*args))
    
    if result_type is None:
        return None
    elif result_type == 'LIST':
        # Return as Lisp cons list
        result = lisptype.NIL
        for elem in reversed(results):
            result = lisptype.lispCons(elem, result)
        return result
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
    
    # Convert sequence to Python list to handle lispCons
    py_seq = _cons_to_list(sequence)
    
    # Handle start/end
    if start is not None:
        start = int(start)
    else:
        start = 0
    
    if end is not None:
        end = int(end)
    else:
        end = len(py_seq)
    
    py_seq = py_seq[start:end]
    
    # Apply key function if provided
    if key is not None:
        py_seq = [key(item) for item in py_seq]
    
    # Handle from-end
    if from_end:
        py_seq = list(reversed(py_seq))
    
    if not py_seq:
        if initial_value is not None:
            return initial_value
        return function()
    
    result = py_seq[0] if initial_value is None else initial_value
    start_idx = 1 if initial_value is None else 0
    
    for item in py_seq[start_idx:]:
        result = function(result, item)
    return result


def _finish_list(pylist):
    """Return a Python list result as a Lisp value.

    `_cons_to_list` turns NIL into `[]` for iteration; on the way back out
    an empty Python list must become NIL again rather than printing as the
    distinct-looking `()`, even though they are the same Lisp object --
    otherwise `(nunion nil nil)` regresses from `NIL` to `()`.
    """
    return pylist if pylist else lisptype.NIL


def _set_op_matcher(kwargs):
    """Build the shared :test/:test-not/:key matcher for a set operation
    from its **kwargs. All eleven set/list operations below (CLHS calls
    them out as sharing one comparison protocol) previously ignored these
    arguments completely and compared with bare Python `==`/`in`
    (plan.md C5/X2/X3).
    """
    return _make_matcher(
        test=kwargs.get('test'),
        test_not=kwargs.get('test_not'),
        key=kwargs.get('key'),
    )


# Set operations
@_registry.cl_function('INTERSECTION')
def intersection(list1, list2, **kwargs):
    """Set intersection."""
    matcher = _set_op_matcher(kwargs)
    list2 = _cons_to_list(list2)
    return _finish_list([x for x in list1 if _matcher_contains(matcher, x, list2)])


@_registry.cl_function('UNION')
def union(list1, list2, **kwargs):
    """Set union."""
    matcher = _set_op_matcher(kwargs)
    result = _cons_to_list(list1)
    for item in _cons_to_list(list2):
        if not _matcher_contains(matcher, item, result):
            result.append(item)
    return _finish_list(result)


@_registry.cl_function('NUNION')
def nunion(list1, list2, **kwargs):
    """Destructive set union."""
    matcher = _set_op_matcher(kwargs)
    list1 = _cons_to_list(list1)
    for item in _cons_to_list(list2):
        if not _matcher_contains(matcher, item, list1):
            list1.append(item)
    return _finish_list(list1)


@_registry.cl_function('SET-DIFFERENCE')
def set_difference(list1, list2, **kwargs):
    """Set difference."""
    matcher = _set_op_matcher(kwargs)
    list2 = _cons_to_list(list2)
    return _finish_list([x for x in list1 if not _matcher_contains(matcher, x, list2)])


@_registry.cl_function('NSET-DIFFERENCE')
def nset_difference(list1, list2, **kwargs):
    """Destructive set difference."""
    matcher = _set_op_matcher(kwargs)
    list1 = _cons_to_list(list1)
    list2 = _cons_to_list(list2)
    return _finish_list([x for x in list1 if not _matcher_contains(matcher, x, list2)])


@_registry.cl_function('SET-EXCLUSIVE-OR')
def set_exclusive_or(list1, list2, **kwargs):
    """Set exclusive or."""
    matcher = _set_op_matcher(kwargs)
    list1 = _cons_to_list(list1)
    list2 = _cons_to_list(list2)
    return _finish_list(
        [x for x in list1 if not _matcher_contains(matcher, x, list2)]
        + [x for x in list2 if not _matcher_contains(matcher, x, list1)]
    )


@_registry.cl_function('NSET-EXCLUSIVE-OR')
def nset_exclusive_or(list1, list2, **kwargs):
    """Destructive set exclusive or."""
    return set_exclusive_or(list1, list2, **kwargs)


@_registry.cl_function('SUBSETP')
def subsetp(subset, set_arg, **kwargs):
    """Test if subset is a subset of set_arg."""
    matcher = _set_op_matcher(kwargs)
    set_arg = _cons_to_list(set_arg)
    for item in _cons_to_list(subset):
        if not _matcher_contains(matcher, item, set_arg):
            return lisptype.NIL
    return lisptype.T


@_registry.cl_function('NINTERSECTION')
def nintersection(list1, list2, **kwargs):
    """Destructive intersection."""
    return intersection(list1, list2, **kwargs)


# Stack operations
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


# Array operations
    # make_array is now properly implemented in vectors.py

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
    # Helper to convert dimensions to int
    def to_int(val):
        if hasattr(val, '__iter__') and not isinstance(val, (str, bytes)):
            return int(val[0]) if val else 0
        return int(val) if val else 0
    
    # Simple implementation - create new array with new dimensions
    # This is a simplified version
    if isinstance(new_dimensions, int):
        return [None] * new_dimensions
    elif hasattr(new_dimensions, '__iter__') and not isinstance(new_dimensions, (str, bytes)):
        # It's a list - extract first dimension
        dim = to_int(new_dimensions[0] if len(new_dimensions) > 0 else 0)
        return [None] * dim
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
    """Array reference.

    An element of a string is a CHARACTER (CLHS 15.1). Both string
    representations index to a bare length-1 Python `str`, so returning the
    raw element handed back an object that is simultaneously a character and
    a string -- two disjoint types (plan.md C13).

    That conflation is not merely untidy: since a string is also a vector,
    any consumer that walks a vector element-wise sees each character as
    another one-element vector and recurses without end. The ANSI harness's
    own `equalp-with-case` does exactly that, so comparing any two strings
    ran until the stack was exhausted and aborted the whole run.
    """
    result = array
    for subscript in subscripts:
        container = result
        result = string_element(container, container[subscript])
    return result


def _is_string(value):
    """True for either representation of a Lisp string."""
    return isinstance(value, (str, lisptype.LispString))


def string_element(container, element):
    """Normalize an element read out of `container` to a Lisp value.

    The elements of a string are CHARACTERs (CLHS 15.1), but both string
    representations index to a bare length-1 Python `str`. Anything that
    walks a string element-wise -- AREF, LOOP's `across`, sequence
    traversal -- has to apply that conversion, and it must be the *same*
    conversion everywhere or the two halves disagree about what a character
    is. Non-string containers are left alone.
    """
    if isinstance(element, str) and _is_string(container):
        return lisptype.Character(element)
    return element


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
    'map_fn', 'mapcar', 'mapcan', 'mapc', 'mapcon', 'maplist', 'mapl', 'reduce_fn',
    # Set operations
    'intersection', 'union', 'nunion', 'set_difference', 'nset_difference',
    'set_exclusive_or', 'nset_exclusive_or', 'subsetp', 'nintersection',
    # Stack operations
    'pop_fn', 'push_fn', 'pushnew',
    # Array operations (make_array is in vectors.py)
    'array_dimensions', 'arrayp', 'array_in_bounds_p',
    'array_displacement', 'array_dimension', 'adjust_array',
    'vectorp', 'simple_vector_p', 'bit_vector_p', 'simple_bit_vector_p',
    'aref', 'svref', 'vector_fn', 'vector_pop', 'vector_push', 'vector_push_extend',
    # Symbol-safe names
    'list_s_star_',
]
