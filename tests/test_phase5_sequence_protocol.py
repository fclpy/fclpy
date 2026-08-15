"""Tests for Phase 5: Unified Sequence Protocol."""

import pytest
import fclpy.lisptype as lisptype
from fclpy.lispfunc.sequences import (
    iterate, with_sequence_protocol, SequenceIterator
)


class TestSequenceIterator:
    """Test the SequenceIterator class."""
    
    def test_iterate_list_basic(self):
        """Test iterating over a basic list."""
        seq = [1, 2, 3, 4, 5]
        iterator = iterate(seq)
        
        items = list(iterator)
        assert items == [1, 2, 3, 4, 5]
    
    def test_iterate_list_with_start_end(self):
        """Test iterating with start and end boundaries."""
        seq = [10, 20, 30, 40, 50]
        iterator = iterate(seq, start=1, end=4)
        
        items = list(iterator)
        assert items == [20, 30, 40]
    
    def test_iterate_string(self):
        """Test iterating over a string."""
        seq = "hello"
        iterator = iterate(seq)
        
        items = list(iterator)
        assert items == ['h', 'e', 'l', 'l', 'o']
    
    def test_iterate_string_with_boundaries(self):
        """Test iterating over string with start/end."""
        seq = "hello"
        iterator = iterate(seq, start=1, end=4)
        
        items = list(iterator)
        assert items == ['e', 'l', 'l']
    
    def test_iterate_tuple(self):
        """Test iterating over a tuple."""
        seq = (1, 2, 3)
        iterator = iterate(seq)
        
        items = list(iterator)
        assert items == [1, 2, 3]
    
    def test_iterator_with_key_function(self):
        """Test iterator with key transformation function."""
        seq = ['abc', 'de', 'fghij']
        iterator = iterate(seq, key=len)
        
        # Key function transforms to lengths for comparison
        assert iterator.get_value('abc') == 3
        assert iterator.get_value('de') == 2
        assert iterator.get_value('fghij') == 5
    
    def test_iterator_with_custom_test(self):
        """Test iterator with custom test function.

        Per CLHS 17.2.1, `matches(element, target)` calls the test as
        `(funcall test target element)` -- the searched-for value is
        always the test's first argument, the sequence element its second.
        """
        seq = [1, 2, 3, 4, 5]
        test_fn = lambda x, y: x < y  # Custom: less than test
        iterator = iterate(seq, test=test_fn)

        assert iterator.matches(2, 5) == False  # test(5, 2) -> 5 < 2 is False
        assert iterator.matches(5, 2) == True  # test(2, 5) -> 2 < 5 is True
    
    def test_iterator_current_index(self):
        """Test tracking current index during iteration."""
        seq = ['a', 'b', 'c']
        iterator = iterate(seq)
        
        next(iterator)
        assert iterator.current_index() == 0
        next(iterator)
        assert iterator.current_index() == 1
        next(iterator)
        assert iterator.current_index() == 2
    
    def test_iterator_reset(self):
        """Test resetting iterator position."""
        seq = [1, 2, 3, 4, 5]
        iterator = iterate(seq)
        
        # Advance a few steps
        next(iterator)
        next(iterator)
        assert iterator.current_index() == 1
        
        # Reset to start
        iterator.reset()
        assert iterator.index == 0
        
        # Reset to specific position
        iterator.reset(start=2)
        assert iterator.index == 2
    
    def test_iterator_key_and_test_together(self):
        """Test iterator with both key and test functions.

        Per CLHS 17.2.1, key transforms the element (second argument);
        the target passed to `matches` is always the test's first argument.
        """
        seq = ['ab', 'cde', 'f']
        key_fn = len
        test_fn = lambda x, y: x > y
        iterator = iterate(seq, key=key_fn, test=test_fn)

        # 'ab' -> len=2, test(1, 2) -> 1 > 2 = False
        assert iterator.matches('ab', 1) == False
        # 'f' -> len=1, test(2, 1) -> 2 > 1 = True
        assert iterator.matches('f', 2) == True
    
    def test_iterate_empty_sequence(self):
        """Test iterating over empty sequences."""
        # Empty list
        iterator = iterate([])
        items = list(iterator)
        assert items == []
        
        # Empty string
        iterator = iterate("")
        items = list(iterator)
        assert items == []
    
    def test_with_sequence_protocol_helper(self):
        """Test the convenience helper function."""
        seq = [1, 2, 3, 4, 5]
        iterator = with_sequence_protocol(seq, start=1, end=4)
        
        items = list(iterator)
        assert items == [2, 3, 4]
    
    def test_iterator_out_of_bounds(self):
        """Test behavior when end exceeds sequence length."""
        seq = [1, 2, 3]
        iterator = iterate(seq, start=0, end=100)  # end > length
        
        items = list(iterator)
        assert items == [1, 2, 3]  # Should stop at actual end


class TestSequenceProtocolIntegration:
    """Test sequence protocol integration."""
    
    def test_list_sequence_protocol(self):
        """Test protocol with lists."""
        seq = [1, 2, 3, 4, 5]
        iterator = iterate(seq, start=1, end=4)
        
        results = []
        for item in iterator:
            results.append(item)
        
        assert results == [2, 3, 4]
    
    def test_string_sequence_protocol(self):
        """Test protocol with strings."""
        seq = "abcde"
        iterator = iterate(seq, start=1, end=4)
        
        results = []
        for item in iterator:
            results.append(item)
        
        assert ''.join(results) == "bcd"
    
    def test_nested_iteration(self):
        """Test multiple iterators on same sequence."""
        seq = [1, 2, 3, 4, 5]
        iter1 = iterate(seq, start=0, end=2)
        iter2 = iterate(seq, start=3, end=5)
        
        items1 = list(iter1)
        items2 = list(iter2)
        
        assert items1 == [1, 2]
        assert items2 == [4, 5]
    
    def test_sequence_protocol_error_handling(self):
        """A non-sequence is rejected as a *Lisp* type error.

        This asserted a Python `TypeError`, which is what the old
        `iterate()` raised -- and which then surfaced as the value of the
        Lisp form (plan.md standing rule 2). The sequence protocol signals
        `LispTypeError` instead.
        """
        with pytest.raises(lisptype.LispTypeError):
            iterate(123)  # int not supported

        with pytest.raises(lisptype.LispTypeError):
            iterate({'a': 1})  # dict not supported


class TestSequenceProtocolUseCases:
    """Test real-world use cases for the sequence protocol."""
    
    def test_find_use_case(self):
        """Test protocol supports finding elements."""
        seq = ['apple', 'banana', 'cherry']
        iterator = iterate(seq)
        
        for item in iterator:
            if item == 'banana':
                assert iterator.current_index() == 1
                break
    
    def test_position_with_key(self):
        """Test finding by transformed key."""
        seq = ['a', 'bb', 'ccc']
        target_len = 2
        iterator = iterate(seq, key=len)
        
        for item in iterator:
            if iterator.matches(item, target_len):
                # Found 'bb' at position 1
                assert iterator.current_index() == 1
                assert item == 'bb'
                break
    
    def test_subseq_with_boundaries(self):
        """Test subsequence extraction."""
        seq = [10, 20, 30, 40, 50, 60]
        iterator = iterate(seq, start=2, end=5)
        
        subseq = list(iterator)
        assert subseq == [30, 40, 50]
