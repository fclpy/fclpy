"""Adjustable vectors and multi-dimensional arrays for Phase 5."""

import fclpy.lisptype as lisptype
from . import registry as _registry


class AdjustableVector:
    """A vector that can grow and shrink with a fill-pointer."""
    
    def __init__(self, capacity=0, initial_element=None, fill_pointer=None):
        """Create adjustable vector.
        
        Args:
            capacity: Initial capacity
            initial_element: Element to fill with (default None)
            fill_pointer: How many elements are actually in use (default capacity)
        """
        # Convert capacity to int (might come from lispCons or other types)
        if hasattr(capacity, '__iter__') and not isinstance(capacity, (str, bytes)):
            # If it's a list/tuple, assume single-dimension
            capacity = int(capacity[0]) if capacity else 0
        else:
            capacity = int(capacity) if capacity else 0
        
        self.data = [initial_element] * capacity
        self.capacity = capacity
        
        # Convert fill_pointer similarly
        if fill_pointer is not None:
            if hasattr(fill_pointer, '__iter__') and not isinstance(fill_pointer, (str, bytes)):
                fill_pointer = int(fill_pointer[0]) if fill_pointer else 0
            else:
                fill_pointer = int(fill_pointer)
        
        self.fill_pointer = fill_pointer if fill_pointer is not None else capacity
        self.initial_element = initial_element
    
    def __len__(self):
        """Return the logical length (fill_pointer)."""
        return self.fill_pointer
    
    def __getitem__(self, index):
        """Get element at index."""
        if index < 0 or index >= self.fill_pointer:
            raise IndexError(f"Index {index} out of bounds for fill_pointer={self.fill_pointer}")
        return self.data[index]
    
    def __setitem__(self, index, value):
        """Set element at index."""
        if index < 0 or index >= self.fill_pointer:
            raise IndexError(f"Index {index} out of bounds for fill_pointer={self.fill_pointer}")
        self.data[index] = value
    
    def __iter__(self):
        """Iterate over filled elements."""
        return iter(self.data[:self.fill_pointer])
    
    def __repr__(self):
        """String representation."""
        return f"#(ADJUSTABLE-VECTOR :capacity {self.capacity} :fill-pointer {self.fill_pointer})"
    
    def vector_push(self, element):
        """Add element if there's room, return new fill_pointer or None.
        
        Returns the new value of fill_pointer, or None if no room.
        """
        if self.fill_pointer >= self.capacity:
            return None
        self.data[self.fill_pointer] = element
        self.fill_pointer += 1
        return self.fill_pointer - 1  # Return the index where element was inserted
    
    def vector_push_extend(self, element, extension_factor=1.5):
        """Add element, extending if necessary.
        
        Returns the index where the element was inserted.
        """
        if self.fill_pointer >= self.capacity:
            # Extend capacity
            new_capacity = max(self.capacity + 1, int(self.capacity * extension_factor))
            self.data.extend([self.initial_element] * (new_capacity - self.capacity))
            self.capacity = new_capacity
        
        self.data[self.fill_pointer] = element
        index = self.fill_pointer
        self.fill_pointer += 1
        return index
    
    def to_list(self):
        """Convert to Python list."""
        return list(self.data[:self.fill_pointer])
    
    def is_adjustable(self):
        """Check if this is adjustable."""
        return True


class Array:
    """Multi-dimensional array supporting row-major indexing."""
    
    def __init__(self, dimensions, initial_element=None):
        """Create multi-dimensional array.
        
        Args:
            dimensions: Tuple/list of dimension sizes, e.g., (2, 3) for 2x3
            initial_element: Element to fill with
        """
        if isinstance(dimensions, int):
            dimensions = (dimensions,)
        
        # Ensure all dimensions are integers
        def to_int(val):
            if isinstance(val, int):
                return val
            if isinstance(val, float):
                return int(val)
            if isinstance(val, lisptype.lispCons):
                return to_int(val.car)
            return int(val)
        
        self.dimensions = tuple(to_int(d) for d in dimensions) if isinstance(dimensions, (list, tuple)) else (to_int(dimensions),)
        self.rank = len(self.dimensions)
        
        # Calculate total size
        self.total_size = 1
        for d in self.dimensions:
            self.total_size *= d
        
        # Store data in flat array (row-major order)
        self.data = [initial_element] * self.total_size
    
    def _compute_index(self, indices):
        """Compute flat index from multi-dimensional indices (row-major).
        
        For 2D: index = row * columns + col
        For 3D: index = plane * (rows * cols) + row * cols + col
        """
        if not isinstance(indices, (tuple, list)):
            indices = (indices,)
        
        if len(indices) != self.rank:
            raise IndexError(f"Expected {self.rank} indices, got {len(indices)}")
        
        # Check bounds
        for i, idx in enumerate(indices):
            if idx < 0 or idx >= self.dimensions[i]:
                raise IndexError(f"Index {idx} out of bounds for dimension {i} (size {self.dimensions[i]})")
        
        # Compute row-major index
        flat_index = 0
        multiplier = 1
        for i in range(self.rank - 1, -1, -1):
            flat_index += indices[i] * multiplier
            multiplier *= self.dimensions[i]
        
        return flat_index
    
    def __getitem__(self, indices):
        """Get element at multi-dimensional index."""
        flat_index = self._compute_index(indices)
        return self.data[flat_index]
    
    def __setitem__(self, indices, value):
        """Set element at multi-dimensional index."""
        flat_index = self._compute_index(indices)
        self.data[flat_index] = value
    
    def __repr__(self):
        """String representation."""
        return f"#(ARRAY {self.dimensions})"
    
    def to_list(self):
        """Convert to nested lists (if not 1D)."""
        if self.rank == 1:
            return list(self.data)
        elif self.rank == 2:
            # 2D: return list of lists
            rows, cols = self.dimensions
            result = []
            for r in range(rows):
                row = []
                for c in range(cols):
                    row.append(self.data[r * cols + c])
                result.append(row)
            return result
        else:
            # For higher dimensions, just return flat
            return list(self.data)


@_registry.cl_function('MAKE-ARRAY')
def make_array(dimensions, element_type=None, initial_element=None, initial_contents=None,
               adjustable=False, fill_pointer=None, displaced_to=None, displaced_index_offset=None,
               **kwargs):
    """Make an array (vector for 1D, Array for multi-dimensional).
    
    Args:
        dimensions: Integer for 1D, list/tuple/lispCons for multi-dimensional
        element_type: Element type specifier (ignored in FCLpy, arrays are generic)
        initial_element: Initial value for elements
        initial_contents: Sequence of initial values (mutually exclusive with initial_element)
        adjustable: If True, create adjustable vector (1D only)
        fill_pointer: For adjustable vectors, set fill-pointer
        displaced_to: Another array to share storage with (not fully implemented)
        displaced_index_offset: Offset into displaced array (not fully implemented)
    
    Returns:
        Vector (list), AdjustableVector, or Array
    """
    def to_int(val):
        """Convert a value to int, handling various Lisp types."""
        if isinstance(val, int):
            return val
        if isinstance(val, float):
            return int(val)
        if isinstance(val, lisptype.lispCons):
            # If it's a single-element list, extract the value
            return to_int(val.car)
        return int(val)
    
    def lisp_list_to_python_list(obj):
        """Convert a Lisp list or sequence to Python list.

        A string source contributes CHARACTERs, not the bare length-1
        Python strings it iterates as -- `(make-array 4 :initial-contents
        "abcd")` builds an array *of characters*. Storing the raw strings
        made each element simultaneously a character and a one-element
        string, which is the conflation AREF and LOOP's `across` also had
        to stop propagating (CLHS 15.1).
        """
        from .sequences_higher import string_element

        if obj is None or obj is lisptype.NIL:
            return []
        if isinstance(obj, list):
            return obj
        if isinstance(obj, (str, lisptype.LispString)):
            return [string_element(obj, c) for c in str(obj)]
        if isinstance(obj, lisptype.lispCons):
            result = []
            current = obj
            while isinstance(current, lisptype.lispCons):
                result.append(current.car)
                current = current.cdr
            return result
        return list(obj)
    
    # Convert Lisp list to Python list if needed
    if isinstance(dimensions, lisptype.lispCons):
        dim_list = []
        current = dimensions
        while isinstance(current, lisptype.lispCons):
            dim_list.append(to_int(current.car))
            current = current.cdr
        dimensions = dim_list
    
    # Handle 1D arrays (vectors)
    if isinstance(dimensions, int):
        if initial_contents is not None:
            contents = lisp_list_to_python_list(initial_contents)
            if adjustable or fill_pointer is not None:
                adj_vec = AdjustableVector(capacity=dimensions, fill_pointer=None)
                # Set fill_pointer to capacity temporarily to allow initialization
                old_fp = adj_vec.fill_pointer
                adj_vec.fill_pointer = dimensions
                for i, val in enumerate(contents[:dimensions]):
                    adj_vec[i] = val
                # Now set the actual fill_pointer
                adj_vec.fill_pointer = fill_pointer if fill_pointer is not None else dimensions
                return adj_vec
            else:
                return contents[:dimensions] if len(contents) >= dimensions else contents + [initial_element] * (dimensions - len(contents))
        elif adjustable or fill_pointer is not None:
            adj_vec = AdjustableVector(capacity=dimensions, 
                                      initial_element=initial_element,
                                      fill_pointer=fill_pointer)
            return adj_vec
        else:
            return [initial_element] * dimensions
    
    # Handle multi-dimensional arrays
    if isinstance(dimensions, (list, tuple)):
        # Ensure all dimensions are integers
        dimensions = [to_int(d) for d in dimensions]
        # Single element list is treated as 1D array
        if len(dimensions) == 1:
            dim = dimensions[0]
            if initial_contents is not None:
                contents = lisp_list_to_python_list(initial_contents)
                if adjustable or fill_pointer is not None:
                    adj_vec = AdjustableVector(capacity=dim, fill_pointer=None)
                    # Temporarily allow full initialization
                    adj_vec.fill_pointer = dim
                    for i, val in enumerate(contents[:dim]):
                        adj_vec[i] = val
                    # Set actual fill_pointer
                    adj_vec.fill_pointer = fill_pointer if fill_pointer is not None else dim
                    return adj_vec
                else:
                    return contents[:dim] if len(contents) >= dim else contents + [initial_element] * (dim - len(contents))
            elif adjustable or fill_pointer is not None:
                return AdjustableVector(capacity=dim, 
                                       initial_element=initial_element,
                                       fill_pointer=fill_pointer)
            else:
                return [initial_element] * dim
        return Array(dimensions, initial_element=initial_element)
    
    # Fallback
    return [initial_element]


@_registry.cl_function('AREF')
def aref(array, *indices):
    """Access array element.
    
    For 1D: AREF array index
    For 2D: AREF array row column
    For 3D: AREF array plane row column
    """
    if isinstance(array, Array):
        # Multi-dimensional array
        return array[indices]
    elif isinstance(array, AdjustableVector):
        # 1D adjustable vector
        if len(indices) == 1:
            return array[indices[0]]
        else:
            raise IndexError(f"Adjustable vector is 1D, got {len(indices)} indices")
    else:
        # Regular list (1D)
        if len(indices) == 1:
            try:
                return array[indices[0]]
            except (IndexError, TypeError):
                return None
        else:
            return None


@_registry.cl_function('VECTOR-PUSH')
def vector_push(element, vector):
    """Push element to adjustable vector if room.
    
    Returns the index of insertion, or NIL if no room.
    """
    if isinstance(vector, AdjustableVector):
        result = vector.vector_push(element)
        if result is None:
            return lisptype.NIL
        return result
    else:
        # Not an adjustable vector
        return lisptype.NIL


@_registry.cl_function('VECTOR-PUSH-EXTEND')
def vector_push_extend(element, vector, extension=None):
    """Push element to vector, extending if necessary.
    
    Returns the index of insertion.
    """
    if isinstance(vector, AdjustableVector):
        if extension is None:
            return vector.vector_push_extend(element)
        else:
            return vector.vector_push_extend(element, extension_factor=extension)
    else:
        # Not adjustable - error
        raise lisptype.LispTypeError("VECTOR-PUSH-EXTEND requires adjustable vector",
                                    expected_type="ADJUSTABLE-VECTOR",
                                    actual_value=type(vector).__name__)


@_registry.cl_function('ADJUSTABLE-ARRAY-P')
def adjustable_array_p(array):
    """Test if array is adjustable."""
    return lisptype.lisp_bool(isinstance(array, AdjustableVector))


@_registry.cl_function('ARRAY-DIMENSION')
def array_dimension(array, axis):
    """Get dimension of array along axis."""
    if isinstance(array, Array):
        if axis < 0 or axis >= array.rank:
            raise IndexError(f"Axis {axis} out of range for rank {array.rank}")
        return array.dimensions[axis]
    elif isinstance(array, AdjustableVector):
        if axis == 0:
            return array.fill_pointer
        else:
            raise IndexError(f"Axis {axis} out of range for 1D array")
    else:
        # Regular list
        if axis == 0:
            return len(array)
        else:
            raise IndexError(f"Axis {axis} out of range for 1D array")


@_registry.cl_function('ARRAY-DIMENSIONS')
def array_dimensions(array):
    """Get all dimensions of array as list."""
    if isinstance(array, Array):
        return list(array.dimensions)
    elif isinstance(array, AdjustableVector):
        return [array.fill_pointer]
    else:
        # Regular list
        return [len(array)]


@_registry.cl_function('ARRAY-RANK')
def array_rank(array):
    """Get rank (number of dimensions) of array."""
    if isinstance(array, Array):
        return array.rank
    else:
        # Vectors have rank 1
        return 1


@_registry.cl_function('ARRAY-TOTAL-SIZE')
def array_total_size(array):
    """Get total number of elements in array."""
    if isinstance(array, Array):
        return array.total_size
    elif isinstance(array, AdjustableVector):
        return array.fill_pointer
    else:
        return len(array)


@_registry.cl_function('FILL-POINTER')
def fill_pointer(vector):
    """Get the fill pointer of a vector."""
    if isinstance(vector, AdjustableVector):
        return vector.fill_pointer
    else:
        return len(vector)


@_registry.cl_function('SET-FILL-POINTER')
def set_fill_pointer(vector, new_pointer):
    """Set the fill pointer of a vector."""
    if isinstance(vector, AdjustableVector):
        if new_pointer < 0 or new_pointer > vector.capacity:
            raise ValueError(f"Fill pointer {new_pointer} out of range [0, {vector.capacity}]")
        vector.fill_pointer = new_pointer
        return new_pointer
    else:
        raise lisptype.LispTypeError("SET-FILL-POINTER requires adjustable vector",
                                    expected_type="ADJUSTABLE-VECTOR",
                                    actual_value=type(vector).__name__)
