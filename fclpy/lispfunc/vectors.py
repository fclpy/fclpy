"""Adjustable vectors for Phase 5."""

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
        self.data = [initial_element] * capacity
        self.capacity = capacity
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


@_registry.cl_function('MAKE-ARRAY')
def make_array(dimensions, initial_element=None, adjustable=False, fill_pointer=None, **kwargs):
    """Make an array (vector for now).
    
    Args:
        dimensions: Integer for 1D, tuple for multi-dimensional
        initial_element: Initial value for elements
        adjustable: If True, create adjustable vector
        fill_pointer: For adjustable vectors, set fill-pointer
    
    Returns:
        Vector or AdjustableVector
    """
    if isinstance(dimensions, int):
        if adjustable:
            adj_vec = AdjustableVector(capacity=dimensions, 
                                      initial_element=initial_element,
                                      fill_pointer=fill_pointer)
            return adj_vec
        else:
            return [initial_element] * dimensions
    else:
        # Multi-dimensional - for now just create 1D with product of dimensions
        total_size = 1
        if isinstance(dimensions, (list, tuple)):
            for d in dimensions:
                total_size *= d
        if adjustable:
            return AdjustableVector(capacity=total_size, initial_element=initial_element)
        return [initial_element] * total_size


@_registry.cl_function('AREF')
def aref(array, *indices):
    """Access array element.
    
    For 1D: AREF array index
    For 2D: AREF array row column
    """
    if isinstance(array, AdjustableVector):
        if len(indices) == 1:
            return array[indices[0]]
        else:
            # Multi-dimensional - compute linear index
            # Simplified: assume row-major order
            shape = getattr(array, 'shape', None)
            if shape:
                index = 0
                multiplier = 1
                for i in range(len(indices) - 1, -1, -1):
                    index += indices[i] * multiplier
                    multiplier *= shape[i]
                return array.data[index]
            return None
    else:
        # Regular list
        if len(indices) == 1:
            return array[indices[0]]
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
    if isinstance(array, AdjustableVector):
        if axis == 0:
            return array.fill_pointer
        else:
            return 1
    else:
        if axis == 0:
            return len(array)
        else:
            return 1


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
