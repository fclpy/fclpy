"""Higher-order sequence operations, arrays, and set operations."""

import functools
from .core import cons, car, cdr, atom
from . import registry as _registry
import fclpy.lisptype as lisptype


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


# Association list operations
@_registry.cl_function('ADJOIN')
def adjoin(x, seq, test=lambda x, y: x is y):
    """Tests whether item is the same as an existing element of list."""
    seq = _cons_to_list(seq)
    return seq if any(map(functools.partial(test, x), seq)) else cons(x, seq)


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
def reduce_fn(function, sequence, **kwargs):
    """Reduce sequence using function."""
    # Convert sequence to Python list to handle lispCons
    py_seq = _cons_to_list(sequence)
    
    if not py_seq:
        if 'initial_value' in kwargs:
            return kwargs['initial_value']
        return function()
    
    result = py_seq[0] if 'initial_value' not in kwargs else kwargs['initial_value']
    start_idx = 1 if 'initial_value' not in kwargs else 0
    
    for item in py_seq[start_idx:]:
        result = function(result, item)
    return result


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


@_registry.cl_function('NINTERSECTION')
def nintersection(list1, list2, **kwargs):
    """Destructive intersection."""
    return [x for x in list1 if x in list2]


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
@_registry.cl_function('MAKE-ARRAY')
def make_array(dimensions, **kwargs):
    """Create array."""
    # Helper to convert dimensions to int
    def to_int(val):
        if hasattr(val, '__iter__') and not isinstance(val, (str, bytes)):
            return int(val[0]) if val else 0
        return int(val) if val else 0
    
    if isinstance(dimensions, int):
        return [None] * dimensions
    # Multi-dimensional array - for now, nested lists
    def make_nested(dims):
        if len(dims) == 1:
            return [None] * to_int(dims[0])
        return [make_nested(dims[1:]) for _ in range(to_int(dims[0]))]
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
    # Array operations
    'make_array', 'array_dimensions', 'arrayp', 'array_in_bounds_p',
    'array_displacement', 'array_dimension', 'adjust_array',
    'vectorp', 'simple_vector_p', 'bit_vector_p', 'simple_bit_vector_p',
    'aref', 'svref', 'vector_fn', 'vector_pop', 'vector_push', 'vector_push_extend',
    # Symbol-safe names
    'list_s_star_',
]
