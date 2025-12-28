"""Tests for Phase 5 Task 2: Sequence Functions with Unified Protocol."""

import pytest
from fclpy.lispfunc.sequences import find, find_if, find_if_not, position, position_if, position_if_not, remove, remove_if, remove_if_not, map_fn, reduce_fn, count, count_if


class TestFindFunction:
    """Test FIND with sequence protocol."""
    
    def test_find_in_list(self):
        """Test finding element in list."""
        seq = [1, 2, 3, 4, 5]
        assert find(3, seq) == 3
        assert find(10, seq) is None
    
    def test_find_in_string(self):
        """Test finding character in string."""
        seq = "hello"
        assert find('l', seq) == 'l'
        assert find('x', seq) is None
    
    def test_find_with_key(self):
        """Test FIND with key function."""
        seq = ['a', 'bb', 'ccc']
        result = find(2, seq, key=len)
        assert result == 'bb'
    
    def test_find_with_test(self):
        """Test FIND with custom test."""
        seq = [1, 2, 3, 4, 5]
        result = find(3, seq, test=lambda x, y: x > y)
        # First element where x > 3
        assert result in [4, 5]
    
    def test_find_with_start_end(self):
        """Test FIND with start and end boundaries."""
        seq = [10, 20, 30, 40, 50]
        result = find(30, seq, start=1, end=4)
        assert result == 30
        result = find(30, seq, start=2, end=5)
        assert result == 30
        result = find(30, seq, start=0, end=2)
        assert result is None


class TestFindIfFunction:
    """Test FIND-IF with sequence protocol."""
    
    def test_find_if_basic(self):
        """Test FIND-IF with basic predicate."""
        seq = [1, 2, 3, 4, 5]
        result = find_if(lambda x: x > 3, seq)
        assert result == 4
    
    def test_find_if_not(self):
        """Test FIND-IF-NOT."""
        seq = [1, 2, 3, 4, 5]
        result = find_if_not(lambda x: x > 3, seq)
        assert result == 1
    
    def test_find_if_with_key(self):
        """Test FIND-IF with key function."""
        seq = ['a', 'bb', 'ccc', 'dd']
        result = find_if(lambda x: x > 2, seq, key=len)
        assert result == 'ccc'
    
    def test_find_if_empty(self):
        """Test FIND-IF on empty sequence."""
        assert find_if(lambda x: x > 0, []) is None


class TestPositionFunction:
    """Test POSITION with sequence protocol."""
    
    def test_position_basic(self):
        """Test POSITION in list."""
        seq = [10, 20, 30, 40, 50]
        assert position(30, seq) == 2
        assert position(100, seq) is None
    
    def test_position_in_string(self):
        """Test POSITION in string."""
        seq = "hello"
        assert position('l', seq) == 2
        assert position('x', seq) is None
    
    def test_position_with_key(self):
        """Test POSITION with key function."""
        seq = ['a', 'bb', 'ccc']
        result = position(2, seq, key=len)
        assert result == 1
    
    def test_position_with_test(self):
        """Test POSITION with custom test."""
        seq = [1, 2, 3, 4, 5]
        result = position_if(lambda x: x > 2, seq)
        assert result == 2  # First element > 2 is 3 at index 2
    
    def test_position_if_not(self):
        """Test POSITION-IF-NOT."""
        seq = [2, 4, 6, 7, 8]
        result = position_if_not(lambda x: x % 2 == 0, seq)
        assert result == 3
    
    def test_position_with_start_end(self):
        """Test POSITION with start and end."""
        seq = [1, 2, 3, 2, 5]
        result = position(2, seq, start=0, end=3)
        assert result == 1
        result = position(2, seq, start=2, end=5)
        assert result == 3


class TestRemoveFunction:
    """Test REMOVE with sequence protocol."""
    
    def test_remove_basic(self):
        """Test REMOVE."""
        seq = [1, 2, 3, 2, 5]
        result = remove(2, seq)
        assert result == [1, 3, 5]
    
    def test_remove_if(self):
        """Test REMOVE-IF."""
        seq = [1, 2, 3, 4, 5]
        result = remove_if(lambda x: x > 3, seq)
        assert result == [1, 2, 3]
    
    def test_remove_if_not(self):
        """Test REMOVE-IF-NOT."""
        seq = [1, 2, 3, 4, 5]
        result = remove_if_not(lambda x: x > 2, seq)
        assert result == [3, 4, 5]
    
    def test_remove_with_key(self):
        """Test REMOVE with key function."""
        seq = ['a', 'bb', 'ccc']
        result = remove(2, seq, key=len)
        assert result == ['a', 'ccc']
    
    def test_remove_with_start_end(self):
        """Test REMOVE with boundaries."""
        seq = [1, 2, 3, 2, 5]
        result = remove(2, seq, start=1, end=4)
        # Should only remove from portion [2, 3, 2]
        assert 1 in result
        assert 5 in result


class TestMapFunction:
    """Test MAP with sequence protocol."""
    
    def test_map_basic(self):
        """Test MAP with single sequence."""
        seq = [1, 2, 3]
        result = map_fn('LIST', lambda x: x * 2, seq)
        assert result == [2, 4, 6]
    
    def test_map_multiple_sequences(self):
        """Test MAP with multiple sequences."""
        seq1 = [1, 2, 3]
        seq2 = [10, 20, 30]
        result = map_fn('LIST', lambda x, y: x + y, seq1, seq2)
        assert result == [11, 22, 33]
    
    def test_map_none_result_type(self):
        """Test MAP with None result type (for side effects)."""
        seq = [1, 2, 3]
        result = map_fn(None, lambda x: x * 2, seq)
        assert result is None
    
    def test_map_different_lengths(self):
        """Test MAP with sequences of different lengths."""
        seq1 = [1, 2, 3, 4]
        seq2 = [10, 20]
        result = map_fn('LIST', lambda x, y: x + y, seq1, seq2)
        # Should use minimum length
        assert len(result) == 2
        assert result == [11, 22]


class TestReduceFunction:
    """Test REDUCE with sequence protocol."""
    
    def test_reduce_sum(self):
        """Test REDUCE to sum elements."""
        seq = [1, 2, 3, 4]
        result = reduce_fn(lambda x, y: x + y, seq)
        assert result == 10
    
    def test_reduce_with_initial(self):
        """Test REDUCE with initial value."""
        seq = [1, 2, 3]
        result = reduce_fn(lambda x, y: x + y, seq, initial_value=10)
        assert result == 16
    
    def test_reduce_empty_with_initial(self):
        """Test REDUCE on empty sequence with initial."""
        result = reduce_fn(lambda x, y: x + y, [], initial_value=5)
        assert result == 5
    
    def test_reduce_product(self):
        """Test REDUCE for multiplication."""
        seq = [1, 2, 3, 4]
        result = reduce_fn(lambda x, y: x * y, seq)
        assert result == 24
    
    def test_reduce_string(self):
        """Test REDUCE on strings."""
        seq = ['a', 'b', 'c']
        result = reduce_fn(lambda x, y: x + y, seq)
        assert result == 'abc'


class TestCountFunction:
    """Test COUNT with sequence protocol."""
    
    def test_count_basic(self):
        """Test COUNT."""
        seq = [1, 2, 3, 2, 5, 2]
        assert count(2, seq) == 3
        assert count(10, seq) == 0
    
    def test_count_if(self):
        """Test COUNT-IF."""
        seq = [1, 2, 3, 4, 5]
        assert count_if(lambda x: x > 2, seq) == 3
    
    def test_count_with_key(self):
        """Test COUNT with key function."""
        seq = ['a', 'bb', 'cc', 'ddd']
        result = count(2, seq, key=len)
        assert result == 2
    
    def test_count_strings(self):
        """Test COUNT with strings."""
        seq = "hello"
        assert count('l', seq) == 2


class TestSequenceFunctionsIntegration:
    """Integration tests for rewritten sequence functions."""
    
    def test_find_position_consistency(self):
        """Test that FIND and POSITION work together."""
        seq = [10, 20, 30, 40, 50]
        found = find(30, seq)
        pos = position(30, seq)
        assert found == 30
        assert pos == 2
        assert seq[pos] == found
    
    def test_remove_and_count(self):
        """Test REMOVE and COUNT together."""
        seq = [1, 2, 3, 2, 5]
        before_count = count(2, seq)
        assert before_count == 2
        removed = remove(2, seq)
        after_count = count(2, removed)
        assert after_count == 0
    
    def test_map_and_reduce(self):
        """Test MAP and REDUCE together."""
        seq = [1, 2, 3, 4]
        # Map doubles, then reduce to sum
        doubled = map_fn('LIST', lambda x: x * 2, seq)
        total = reduce_fn(lambda x, y: x + y, doubled)
        assert doubled == [2, 4, 6, 8]
        assert total == 20
    
    def test_find_if_and_remove(self):
        """Test FIND-IF and REMOVE together."""
        seq = [1, 2, 3, 4, 5, 6]
        even_fn = lambda x: x % 2 == 0
        # Find first even
        first_even = find_if(even_fn, seq)
        assert first_even == 2
        # Remove all evens
        odds_only = remove_if(even_fn, seq)
        assert odds_only == [1, 3, 5]
