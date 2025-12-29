"""Tests for random state support (Phase 8 Task 5)."""

import pytest
from fclpy.lisptype import T, NIL
from fclpy.lispfunc.utilities_system import (
    random, make_random_state, random_state_p, RandomState,
    get_random_state, set_random_state
)


class TestRandomState:
    """Test RandomState class."""
    
    def test_create_random_state(self):
        """Can create a RandomState."""
        rs = RandomState()
        assert isinstance(rs, RandomState)
        
    def test_random_state_with_seed(self):
        """RandomState with seed produces reproducible results."""
        rs1 = RandomState(42)
        rs2 = RandomState(42)
        
        # Same seed should produce same sequence
        assert rs1.randrange(100) == rs2.randrange(100)
        assert rs1.randrange(100) == rs2.randrange(100)
        
    def test_random_state_copy(self):
        """Can copy a RandomState."""
        rs1 = RandomState(42)
        rs1.randrange(100)  # Advance state
        rs2 = RandomState(rs1)  # Copy
        
        # Both should produce same sequence from here
        assert rs1.randrange(100) == rs2.randrange(100)
        
    def test_random_state_repr(self):
        """RandomState has proper repr."""
        rs = RandomState()
        assert "#<RANDOM-STATE>" in repr(rs)


class TestRandom:
    """Test RANDOM function."""
    
    def test_random_integer(self):
        """RANDOM with integer limit."""
        for _ in range(10):
            result = random(100)
            assert isinstance(result, int)
            assert 0 <= result < 100
            
    def test_random_float(self):
        """RANDOM with float limit."""
        for _ in range(10):
            result = random(1.0)
            assert isinstance(result, float)
            assert 0.0 <= result < 1.0
            
    def test_random_float_scaled(self):
        """RANDOM with scaled float limit."""
        for _ in range(10):
            result = random(10.0)
            assert isinstance(result, float)
            assert 0.0 <= result < 10.0
            
    def test_random_with_state(self):
        """RANDOM respects provided state."""
        rs = RandomState(42)
        val1 = random(1000, rs)
        
        rs2 = RandomState(42)
        val2 = random(1000, rs2)
        
        assert val1 == val2  # Same seed, same result
        
    def test_random_reproducible(self):
        """RANDOM produces reproducible sequences with same state."""
        rs = RandomState(12345)
        sequence1 = [random(100, rs) for _ in range(5)]
        
        rs = RandomState(12345)
        sequence2 = [random(100, rs) for _ in range(5)]
        
        assert sequence1 == sequence2


class TestMakeRandomState:
    """Test MAKE-RANDOM-STATE function."""
    
    def test_make_random_state_nil(self):
        """MAKE-RANDOM-STATE with NIL copies default state."""
        rs = make_random_state(NIL)
        assert isinstance(rs, RandomState)
        
    def test_make_random_state_none(self):
        """MAKE-RANDOM-STATE with None copies default state."""
        rs = make_random_state(None)
        assert isinstance(rs, RandomState)
        
    def test_make_random_state_t(self):
        """MAKE-RANDOM-STATE with T creates fresh state."""
        rs1 = make_random_state(T)
        rs2 = make_random_state(T)
        assert isinstance(rs1, RandomState)
        assert isinstance(rs2, RandomState)
        # They should produce different sequences (extremely likely)
        # Note: Could theoretically fail but probability is astronomically low
        seq1 = [rs1.randrange(1000000) for _ in range(5)]
        seq2 = [rs2.randrange(1000000) for _ in range(5)]
        assert seq1 != seq2  # Different fresh states
        
    def test_make_random_state_copy(self):
        """MAKE-RANDOM-STATE copies existing state."""
        rs1 = RandomState(42)
        rs1.randrange(100)  # Advance state
        
        rs2 = make_random_state(rs1)
        
        # Both should produce same sequence
        assert rs1.randrange(100) == rs2.randrange(100)


class TestRandomStateP:
    """Test RANDOM-STATE-P predicate."""
    
    def test_random_state_p_true(self):
        """RANDOM-STATE-P returns T for RandomState."""
        rs = RandomState()
        assert random_state_p(rs) == T
        
    def test_random_state_p_false(self):
        """RANDOM-STATE-P returns NIL for non-RandomState."""
        assert random_state_p(42) == NIL
        assert random_state_p("hello") == NIL
        assert random_state_p([1, 2, 3]) == NIL
        assert random_state_p(None) == NIL


class TestGlobalRandomState:
    """Test global random state management."""
    
    def test_get_random_state(self):
        """get_random_state returns a RandomState."""
        rs = get_random_state()
        assert isinstance(rs, RandomState)
        
    def test_set_random_state(self):
        """set_random_state changes the global state."""
        original = get_random_state()
        try:
            new_rs = RandomState(999)
            set_random_state(new_rs)
            assert get_random_state() is new_rs
        finally:
            # Restore original
            set_random_state(original)
            
    def test_set_random_state_affects_random(self):
        """Setting global state affects RANDOM calls."""
        original = get_random_state()
        try:
            rs = RandomState(42)
            set_random_state(rs)
            val1 = random(1000)
            
            rs = RandomState(42)
            set_random_state(rs)
            val2 = random(1000)
            
            assert val1 == val2  # Same seed produces same result
        finally:
            set_random_state(original)
