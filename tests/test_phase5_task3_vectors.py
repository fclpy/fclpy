"""Tests for Phase 5 Task 3: Adjustable Vectors."""

import pytest
from fclpy.lispfunc.vectors import (
    AdjustableVector, make_array, vector_push, vector_push_extend,
    adjustable_array_p, fill_pointer, set_fill_pointer, aref, array_dimension
)
import fclpy.lisptype as lisptype


class TestAdjustableVector:
    """Test the AdjustableVector class."""
    
    def test_create_empty_vector(self):
        """Test creating an empty vector."""
        vec = AdjustableVector(capacity=0)
        assert len(vec) == 0
        assert vec.fill_pointer == 0
        assert vec.capacity == 0
    
    def test_create_vector_with_capacity(self):
        """Test creating vector with capacity."""
        vec = AdjustableVector(capacity=10)
        assert len(vec) == 10
        assert vec.fill_pointer == 10
        assert vec.capacity == 10
    
    def test_create_vector_with_initial_element(self):
        """Test creating vector with initial element."""
        vec = AdjustableVector(capacity=5, initial_element=0)
        assert len(vec) == 5
        assert vec[0] == 0
        assert vec[4] == 0
    
    def test_create_vector_with_fill_pointer(self):
        """Test creating vector with custom fill pointer."""
        vec = AdjustableVector(capacity=10, initial_element=None, fill_pointer=5)
        assert len(vec) == 5
        assert vec.fill_pointer == 5
        assert vec.capacity == 10
    
    def test_vector_getitem(self):
        """Test getting elements from vector."""
        vec = AdjustableVector(capacity=3, initial_element=0, fill_pointer=3)
        vec[0] = 1
        vec[1] = 2
        vec[2] = 3
        
        assert vec[0] == 1
        assert vec[1] == 2
        assert vec[2] == 3
    
    def test_vector_getitem_out_of_bounds(self):
        """Test out of bounds access raises error."""
        vec = AdjustableVector(capacity=5, fill_pointer=3)
        with pytest.raises(IndexError):
            _ = vec[3]
        with pytest.raises(IndexError):
            _ = vec[10]
    
    def test_vector_setitem(self):
        """Test setting elements in vector."""
        vec = AdjustableVector(capacity=3, fill_pointer=3)
        vec[0] = 'a'
        vec[1] = 'b'
        vec[2] = 'c'
        
        assert vec[0] == 'a'
        assert vec[1] == 'b'
        assert vec[2] == 'c'
    
    def test_vector_iterate(self):
        """Test iterating over vector."""
        vec = AdjustableVector(capacity=5, initial_element=0, fill_pointer=3)
        vec[0] = 1
        vec[1] = 2
        vec[2] = 3
        
        items = list(vec)
        assert items == [1, 2, 3]
    
    def test_vector_push_with_room(self):
        """Test VECTOR-PUSH when there's room."""
        vec = AdjustableVector(capacity=5, fill_pointer=2)
        result = vec.vector_push(42)
        
        assert result == 2  # Return index
        assert vec[2] == 42
        assert vec.fill_pointer == 3
    
    def test_vector_push_without_room(self):
        """Test VECTOR-PUSH when at capacity."""
        vec = AdjustableVector(capacity=3, fill_pointer=3)
        result = vec.vector_push(42)
        
        assert result is None  # No room
        assert vec.fill_pointer == 3
    
    def test_vector_push_extend_with_room(self):
        """Test VECTOR-PUSH-EXTEND with room."""
        vec = AdjustableVector(capacity=10, fill_pointer=5)
        result = vec.vector_push_extend(99)
        
        assert result == 5
        assert vec[5] == 99
        assert vec.fill_pointer == 6
    
    def test_vector_push_extend_without_room(self):
        """Test VECTOR-PUSH-EXTEND extends capacity."""
        vec = AdjustableVector(capacity=3, fill_pointer=3)
        assert vec.capacity == 3
        
        result = vec.vector_push_extend(99)
        
        assert result == 3
        assert vec[3] == 99
        assert vec.fill_pointer == 4
        assert vec.capacity > 3  # Extended
    
    def test_vector_push_extend_factor(self):
        """Test VECTOR-PUSH-EXTEND with custom extension factor."""
        vec = AdjustableVector(capacity=10, fill_pointer=10)
        old_capacity = vec.capacity
        
        vec.vector_push_extend(42, extension_factor=2.0)
        
        assert vec.capacity == old_capacity * 2
    
    def test_vector_to_list(self):
        """Test converting vector to list."""
        vec = AdjustableVector(capacity=5, initial_element=0, fill_pointer=3)
        vec[0] = 1
        vec[1] = 2
        vec[2] = 3
        
        lst = vec.to_list()
        assert lst == [1, 2, 3]
        assert len(lst) == 3


class TestMakeArray:
    """Test MAKE-ARRAY function."""
    
    def test_make_simple_array(self):
        """Test making simple array."""
        arr = make_array(5)
        assert isinstance(arr, list)
        assert len(arr) == 5
    
    def test_make_array_with_initial(self):
        """Test making array with initial element."""
        arr = make_array(5, initial_element=0)
        assert arr == [0, 0, 0, 0, 0]
    
    def test_make_adjustable_array(self):
        """Test making adjustable array."""
        arr = make_array(5, adjustable=True)
        assert isinstance(arr, AdjustableVector)
        assert arr.capacity == 5
    
    def test_make_adjustable_with_fill_pointer(self):
        """Test adjustable array with fill pointer."""
        arr = make_array(10, adjustable=True, fill_pointer=5)
        assert arr.capacity == 10
        assert arr.fill_pointer == 5
        assert len(arr) == 5


class TestVectorPushFunctions:
    """Test VECTOR-PUSH and VECTOR-PUSH-EXTEND."""
    
    def test_vector_push_lisp_interface(self):
        """Test VECTOR-PUSH through Lisp interface."""
        vec = make_array(5, adjustable=True, fill_pointer=0)
        result = vector_push(42, vec)
        
        assert result == 0  # First position
        assert vec[0] == 42
    
    def test_vector_push_returns_nil_no_room(self):
        """Test VECTOR-PUSH returns NIL when no room."""
        vec = make_array(2, adjustable=True, fill_pointer=2)
        result = vector_push(42, vec)
        
        assert result == lisptype.NIL
    
    def test_vector_push_extend_lisp_interface(self):
        """Test VECTOR-PUSH-EXTEND through Lisp interface."""
        vec = make_array(3, adjustable=True, fill_pointer=3)
        result = vector_push_extend(42, vec)
        
        assert result == 3
        assert vec[3] == 42
    
    def test_vector_push_extend_not_adjustable(self):
        """Test VECTOR-PUSH-EXTEND on non-adjustable raises error."""
        vec = make_array(5)  # Regular list
        with pytest.raises(lisptype.LispTypeError):
            vector_push_extend(42, vec)


class TestArrayFunctions:
    """Test array access and query functions."""
    
    def test_adjustable_array_p_true(self):
        """Test ADJUSTABLE-ARRAY-P on adjustable vector."""
        vec = make_array(5, adjustable=True)
        assert adjustable_array_p(vec) == lisptype.T
    
    def test_adjustable_array_p_false(self):
        """Test ADJUSTABLE-ARRAY-P on regular list."""
        arr = make_array(5)
        assert adjustable_array_p(arr) == lisptype.NIL
    
    def test_fill_pointer_value(self):
        """Test getting fill pointer."""
        vec = make_array(10, adjustable=True, fill_pointer=5)
        assert fill_pointer(vec) == 5
    
    def test_fill_pointer_of_list(self):
        """Test fill pointer of regular list."""
        lst = [1, 2, 3, 4, 5]
        assert fill_pointer(lst) == 5
    
    def test_set_fill_pointer(self):
        """Test setting fill pointer."""
        vec = make_array(10, adjustable=True, fill_pointer=5)
        result = set_fill_pointer(vec, 7)
        
        assert result == 7
        assert vec.fill_pointer == 7
        assert len(vec) == 7
    
    def test_set_fill_pointer_out_of_range(self):
        """Test setting fill pointer out of range."""
        vec = make_array(10, adjustable=True)
        with pytest.raises(ValueError):
            set_fill_pointer(vec, 15)
    
    def test_set_fill_pointer_on_list(self):
        """Test set fill pointer on non-adjustable list."""
        lst = [1, 2, 3]
        with pytest.raises(lisptype.LispTypeError):
            set_fill_pointer(lst, 5)
    
    def test_array_dimension(self):
        """Test getting array dimension."""
        vec = make_array(10, adjustable=True, fill_pointer=5)
        assert array_dimension(vec, 0) == 5  # Logical length (fill_pointer)
    
    def test_aref_access(self):
        """Test accessing array elements with AREF."""
        vec = make_array(5, adjustable=True)
        vec[2] = 'test'
        
        assert aref(vec, 2) == 'test'
    
    def test_aref_list(self):
        """Test AREF on regular list."""
        lst = [1, 2, 3, 4, 5]
        assert aref(lst, 2) == 3


class TestVectorIntegration:
    """Integration tests for vector operations."""
    
    def test_build_vector_with_push_extend(self):
        """Test building a vector incrementally."""
        vec = make_array(2, adjustable=True, fill_pointer=0)
        
        # Push elements
        vector_push_extend(1, vec)
        vector_push_extend(2, vec)
        vector_push_extend(3, vec)  # This extends
        vector_push_extend(4, vec)
        
        assert len(vec) == 4
        assert vec.to_list() == [1, 2, 3, 4]
    
    def test_vector_with_set_fill_pointer(self):
        """Test reducing vector size with fill pointer."""
        vec = make_array(5, adjustable=True, initial_element=0, fill_pointer=5)
        for i in range(5):
            vec[i] = i + 1
        
        # Reduce size
        set_fill_pointer(vec, 3)
        
        assert len(vec) == 3
        assert vec.to_list() == [1, 2, 3]
    
    def test_mixed_access_patterns(self):
        """Test mixing different access patterns."""
        vec = make_array(10, adjustable=True, fill_pointer=0)
        
        # Use vector_push_extend to build it up
        vector_push_extend('a', vec)  # Index 0
        vector_push_extend('b', vec)  # Index 1
        vector_push_extend('c', vec)  # Index 2
        
        # Direct access
        vec[2] = 'modified_c'
        
        # Use aref
        assert aref(vec, 0) == 'a'
        assert aref(vec, 1) == 'b'
        assert aref(vec, 2) == 'modified_c'
    
    def test_iteration_respects_fill_pointer(self):
        """Test that iteration respects fill pointer."""
        vec = make_array(10, adjustable=True, fill_pointer=3)
        vec[0] = 'x'
        vec[1] = 'y'
        vec[2] = 'z'
        
        items = list(vec)
        assert items == ['x', 'y', 'z']
        assert len(items) == 3
