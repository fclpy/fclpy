"""Tests for Phase 5 Task 4: Multi-dimensional Arrays."""

import pytest
from fclpy.lispfunc.vectors import Array, make_array, aref, array_dimension, array_dimensions, array_rank, array_total_size
import fclpy.lisptype as lisptype


class TestArrayClass:
    """Test the Array class for multi-dimensional arrays."""
    
    def test_create_2d_array(self):
        """Test creating a 2D array."""
        arr = Array((3, 4))
        assert arr.rank == 2
        assert arr.dimensions == (3, 4)
        assert arr.total_size == 12
    
    def test_create_3d_array(self):
        """Test creating a 3D array."""
        arr = Array((2, 3, 4))
        assert arr.rank == 3
        assert arr.dimensions == (2, 3, 4)
        assert arr.total_size == 24
    
    def test_create_1d_array(self):
        """Test creating a 1D array."""
        arr = Array((5,))
        assert arr.rank == 1
        assert arr.dimensions == (5,)
        assert arr.total_size == 5
    
    def test_create_array_with_initial_element(self):
        """Test creating array with initial element."""
        arr = Array((2, 3), initial_element=0)
        assert arr[0, 0] == 0
        assert arr[1, 2] == 0
    
    def test_2d_array_getitem(self):
        """Test getting 2D array elements."""
        arr = Array((2, 3), initial_element=None)
        arr[0, 0] = 1
        arr[0, 1] = 2
        arr[1, 2] = 6
        
        assert arr[0, 0] == 1
        assert arr[0, 1] == 2
        assert arr[1, 2] == 6
    
    def test_2d_array_setitem(self):
        """Test setting 2D array elements."""
        arr = Array((2, 3))
        arr[0, 0] = 'a'
        arr[1, 1] = 'e'
        
        assert arr[0, 0] == 'a'
        assert arr[1, 1] == 'e'
    
    def test_3d_array_access(self):
        """Test accessing 3D array elements."""
        arr = Array((2, 2, 2))
        arr[0, 0, 0] = 'origin'
        arr[1, 1, 1] = 'opposite'
        
        assert arr[0, 0, 0] == 'origin'
        assert arr[1, 1, 1] == 'opposite'
    
    def test_array_out_of_bounds(self):
        """Test out of bounds access raises error."""
        arr = Array((2, 3))
        
        with pytest.raises(IndexError):
            _ = arr[2, 0]  # Row out of bounds
        
        with pytest.raises(IndexError):
            _ = arr[0, 3]  # Column out of bounds
        
        with pytest.raises(IndexError):
            _ = arr[0]  # Wrong number of indices
    
    def test_array_wrong_index_count(self):
        """Test wrong number of indices raises error."""
        arr = Array((2, 3, 4))
        
        with pytest.raises(IndexError):
            _ = arr[0, 0]  # Need 3 indices
        
        with pytest.raises(IndexError):
            _ = arr[0, 0, 0, 0]  # Too many indices
    
    def test_row_major_indexing(self):
        """Test row-major order (C-style) indexing."""
        # Create 2x3 array
        arr = Array((2, 3))
        
        # Fill with sequential values
        for i in range(2):
            for j in range(3):
                arr[i, j] = i * 3 + j
        
        # Check row-major order
        assert arr[0, 0] == 0
        assert arr[0, 1] == 1
        assert arr[0, 2] == 2
        assert arr[1, 0] == 3
        assert arr[1, 1] == 4
        assert arr[1, 2] == 5
    
    def test_array_to_list_2d(self):
        """Test converting 2D array to nested list."""
        arr = Array((2, 3), initial_element=0)
        arr[0, 0] = 1
        arr[0, 1] = 2
        arr[0, 2] = 3
        arr[1, 0] = 4
        arr[1, 1] = 5
        arr[1, 2] = 6
        
        lst = arr.to_list()
        assert lst == [[1, 2, 3], [4, 5, 6]]


class TestMakeArrayMultiDimensional:
    """Test MAKE-ARRAY with multi-dimensional arrays."""
    
    def test_make_2d_array(self):
        """Test making 2D array."""
        arr = make_array((3, 4))
        assert isinstance(arr, Array)
        assert arr.rank == 2
        assert arr.dimensions == (3, 4)
    
    def test_make_3d_array(self):
        """Test making 3D array."""
        arr = make_array((2, 3, 4))
        assert isinstance(arr, Array)
        assert arr.rank == 3
    
    def test_make_2d_with_initial(self):
        """Test making 2D array with initial element."""
        arr = make_array((2, 2), initial_element=5)
        assert arr[0, 0] == 5
        assert arr[1, 1] == 5
    
    def test_make_array_list_vs_tuple(self):
        """Test MAKE-ARRAY accepts both list and tuple."""
        arr1 = make_array((2, 3))
        arr2 = make_array([2, 3])
        
        assert isinstance(arr1, Array)
        assert isinstance(arr2, Array)
        assert arr1.dimensions == arr2.dimensions


class TestArefMultiDimensional:
    """Test AREF with multi-dimensional arrays."""
    
    def test_aref_2d(self):
        """Test AREF with 2D array."""
        arr = make_array((2, 3))
        arr[0, 0] = 'value'
        
        result = aref(arr, 0, 0)
        assert result == 'value'
    
    def test_aref_3d(self):
        """Test AREF with 3D array."""
        arr = make_array((2, 2, 2))
        arr[1, 1, 1] = 'deep'
        
        result = aref(arr, 1, 1, 1)
        assert result == 'deep'
    
    def test_aref_wrong_indices_2d(self):
        """Test AREF with wrong number of indices."""
        arr = make_array((2, 3))
        
        with pytest.raises(IndexError):
            aref(arr, 0)  # Missing second index


class TestArrayDimensions:
    """Test array dimension query functions."""
    
    def test_array_dimension_2d(self):
        """Test ARRAY-DIMENSION on 2D array."""
        arr = make_array((3, 4))
        assert array_dimension(arr, 0) == 3
        assert array_dimension(arr, 1) == 4
    
    def test_array_dimension_3d(self):
        """Test ARRAY-DIMENSION on 3D array."""
        arr = make_array((2, 3, 4))
        assert array_dimension(arr, 0) == 2
        assert array_dimension(arr, 1) == 3
        assert array_dimension(arr, 2) == 4
    
    def test_array_dimensions(self):
        """Test ARRAY-DIMENSIONS returns all dimensions."""
        arr = make_array((2, 3, 4))
        dims = array_dimensions(arr)
        assert dims == [2, 3, 4]
    
    def test_array_rank(self):
        """Test ARRAY-RANK."""
        arr1d = make_array(5)
        arr2d = make_array((3, 4))
        arr3d = make_array((2, 3, 4))
        
        assert array_rank(arr1d) == 1
        assert array_rank(arr2d) == 2
        assert array_rank(arr3d) == 3
    
    def test_array_total_size(self):
        """Test ARRAY-TOTAL-SIZE."""
        arr2d = make_array((3, 4))
        arr3d = make_array((2, 3, 4))
        
        assert array_total_size(arr2d) == 12
        assert array_total_size(arr3d) == 24
    
    def test_array_dimension_out_of_range(self):
        """Test ARRAY-DIMENSION with invalid axis."""
        arr = make_array((2, 3))
        
        with pytest.raises(IndexError):
            array_dimension(arr, 2)  # Only axes 0, 1 valid


class TestArrayIntegration:
    """Integration tests for multi-dimensional arrays."""
    
    def test_create_and_fill_2d(self):
        """Test creating and filling a 2D array."""
        arr = make_array((3, 3), initial_element=0)
        
        # Create identity-like pattern
        for i in range(3):
            arr[i, i] = 1
        
        assert arr[0, 0] == 1
        assert arr[1, 1] == 1
        assert arr[2, 2] == 1
        assert arr[0, 1] == 0
    
    def test_matrix_operations(self):
        """Test matrix-like operations."""
        # Create 2x2 matrix
        arr = make_array((2, 2))
        
        # Set values
        arr[0, 0] = 1
        arr[0, 1] = 2
        arr[1, 0] = 3
        arr[1, 1] = 4
        
        # Access via AREF
        assert aref(arr, 0, 0) == 1
        assert aref(arr, 1, 1) == 4
        
        # Check dimensions
        assert array_dimensions(arr) == [2, 2]
    
    def test_3d_cube(self):
        """Test 3D array as a cube."""
        cube = make_array((3, 3, 3), initial_element=0)
        
        # Set corner elements
        cube[0, 0, 0] = 'origin'
        cube[2, 2, 2] = 'opposite'
        
        assert aref(cube, 0, 0, 0) == 'origin'
        assert aref(cube, 2, 2, 2) == 'opposite'
        assert array_total_size(cube) == 27
    
    def test_array_persistence(self):
        """Test that array changes persist."""
        arr = make_array((2, 2))
        arr[0, 0] = 'first'
        arr[1, 1] = 'last'
        
        # Access multiple times
        assert arr[0, 0] == 'first'
        assert aref(arr, 1, 1) == 'last'
        assert arr[0, 0] == 'first'  # Still there
    
    def test_different_element_types(self):
        """Test arrays with different element types."""
        # Array of strings
        str_arr = make_array((2, 2))
        str_arr[0, 0] = 'hello'
        str_arr[1, 1] = 'world'
        
        # Array of numbers
        num_arr = make_array((2, 2), initial_element=0)
        num_arr[0, 0] = 42
        num_arr[1, 1] = 3.14
        
        assert str_arr[0, 0] == 'hello'
        assert num_arr[0, 0] == 42
        assert num_arr[1, 1] == 3.14


class TestArrayEdgeCases:
    """Test edge cases for arrays."""
    
    def test_1x1_array(self):
        """Test 1x1 array."""
        arr = make_array((1, 1))
        arr[0, 0] = 'single'
        assert arr[0, 0] == 'single'
        assert array_total_size(arr) == 1
    
    def test_1d_via_array_class(self):
        """Test 1D array created via Array class."""
        arr = Array((5,))
        arr[0, ] = 'zero'
        arr[4, ] = 'four'
        
        assert arr[0] == 'zero'
        assert arr[4] == 'four'
    
    def test_large_2d_array(self):
        """Test larger 2D array."""
        arr = make_array((100, 100), initial_element=None)
        arr[50, 50] = 'center'
        arr[0, 0] = 'start'
        arr[99, 99] = 'end'
        
        assert arr[50, 50] == 'center'
        assert array_total_size(arr) == 10000
