"""Tests for Phase 5 Task 7: Hash Tables."""

import pytest
from fclpy.lispfunc.hashtables import (
    HashTable, make_hash_table, hash_table_p, gethash, puthash, remhash,
    clrhash, hash_table_count, hash_table_test, hash_table_size,
    maphash, hash_table_keys, hash_table_values, hash_table_items
)
import fclpy.lisptype as lisptype


class TestHashTableClass:
    """Test the HashTable class."""
    
    def test_create_hash_table(self):
        """Test creating a hash table."""
        ht = HashTable()
        assert isinstance(ht, HashTable)
        assert ht.test == 'eql'
        assert ht._size == 16
    
    def test_create_with_test(self):
        """Test creating hash table with specific test."""
        ht = HashTable(test='equal')
        assert ht.test == 'equal'
    
    def test_create_with_size(self):
        """Test creating hash table with specific size."""
        ht = HashTable(size=32)
        assert ht._size == 32
    
    def test_hash_table_get_set(self):
        """Test getting and setting values."""
        ht = HashTable()
        ht.set('key1', 'value1')
        
        assert ht.get('key1') == 'value1'
        assert ht.count() == 1
    
    def test_hash_table_multiple_entries(self):
        """Test multiple entries."""
        ht = HashTable()
        ht.set('a', 1)
        ht.set('b', 2)
        ht.set('c', 3)
        
        assert ht.count() == 3
        assert ht.get('a') == 1
        assert ht.get('b') == 2
        assert ht.get('c') == 3
    
    def test_hash_table_overwrite(self):
        """Test overwriting existing key."""
        ht = HashTable()
        ht.set('key', 'old')
        ht.set('key', 'new')
        
        assert ht.get('key') == 'new'
        assert ht.count() == 1
    
    def test_hash_table_remove(self):
        """Test removing entries."""
        ht = HashTable()
        ht.set('key1', 'value1')
        ht.set('key2', 'value2')
        
        result = ht.remove('key1')
        assert result == lisptype.T
        assert ht.count() == 1
        assert ht.get('key1') is None
        assert ht.get('key2') == 'value2'
    
    def test_hash_table_remove_nonexistent(self):
        """Test removing nonexistent key."""
        ht = HashTable()
        result = ht.remove('nonexistent')
        assert result == lisptype.NIL
    
    def test_hash_table_clear(self):
        """Test clearing hash table."""
        ht = HashTable()
        ht.set('a', 1)
        ht.set('b', 2)
        
        ht.clear()
        assert ht.count() == 0
        assert ht.get('a') is None
    
    def test_hash_table_keys(self):
        """Test getting all keys."""
        ht = HashTable()
        ht.set('a', 1)
        ht.set('b', 2)
        ht.set('c', 3)
        
        keys = ht.keys()
        assert len(keys) == 3
        assert 'a' in keys
        assert 'b' in keys
        assert 'c' in keys
    
    def test_hash_table_values(self):
        """Test getting all values."""
        ht = HashTable()
        ht.set('a', 1)
        ht.set('b', 2)
        ht.set('c', 3)
        
        values = ht.values()
        assert len(values) == 3
        assert 1 in values
        assert 2 in values
        assert 3 in values
    
    def test_hash_table_items(self):
        """Test getting all items."""
        ht = HashTable()
        ht.set('x', 10)
        ht.set('y', 20)
        
        items = ht.items()
        assert len(items) == 2
        assert ('x', 10) in items
        assert ('y', 20) in items


class TestHashTableTest:
    """Test hash table test functions."""
    
    def test_eql_test(self):
        """Test eql comparison."""
        ht = HashTable(test='eql')
        ht.set(1, 'one')
        
        assert ht.get(1) == 'one'
        assert ht.get(1.0) == 'one'  # 1 == 1.0
    
    def test_equal_test(self):
        """Test equal comparison."""
        ht = HashTable(test='equal')
        ht.set('hello', 'world')
        
        assert ht.get('hello') == 'world'
    
    def test_equalp_case_insensitive(self):
        """Test equalp with case insensitivity."""
        ht = HashTable(test='equalp')
        ht.set('KEY', 'value')
        
        # Case insensitive comparison
        assert ht.get('key') == 'value'
        assert ht.get('KEY') == 'value'
        assert ht.get('Key') == 'value'


class TestMakeHashTable:
    """Test MAKE-HASH-TABLE function."""
    
    def test_make_default(self):
        """Test creating with defaults."""
        ht = make_hash_table()
        assert isinstance(ht, HashTable)
        assert ht.test == 'eql'
    
    def test_make_with_test(self):
        """Test creating with test parameter."""
        ht = make_hash_table(test='equal')
        assert ht.test == 'equal'
    
    def test_make_with_size(self):
        """Test creating with size parameter."""
        ht = make_hash_table(size=64)
        assert ht._size == 64


class TestHashTableFunctions:
    """Test hash table Lisp functions."""
    
    def test_hash_table_p(self):
        """Test HASH-TABLE-P."""
        ht = make_hash_table()
        assert hash_table_p(ht) == lisptype.T
        assert hash_table_p("not a table") == lisptype.NIL
        assert hash_table_p([1, 2, 3]) == lisptype.NIL
    
    def test_gethash(self):
        """Test GETHASH."""
        ht = make_hash_table()
        puthash('key', ht, 'value')
        
        result = gethash('key', ht)
        assert result == 'value'
    
    def test_gethash_default(self):
        """Test GETHASH with default."""
        ht = make_hash_table()
        result = gethash('nonexistent', ht, 'default')
        assert result == 'default'
    
    def test_puthash(self):
        """Test PUTHASH."""
        ht = make_hash_table()
        result = puthash('mykey', ht, 'myvalue')
        
        assert result == 'myvalue'
        assert gethash('mykey', ht) == 'myvalue'
    
    def test_remhash(self):
        """Test REMHASH."""
        ht = make_hash_table()
        puthash('key1', ht, 'value1')
        puthash('key2', ht, 'value2')
        
        result = remhash('key1', ht)
        assert result == lisptype.T
        assert hash_table_count(ht) == 1
        assert gethash('key1', ht) is None
    
    def test_remhash_nonexistent(self):
        """Test REMHASH on nonexistent key."""
        ht = make_hash_table()
        result = remhash('nonexistent', ht)
        assert result == lisptype.NIL
    
    def test_clrhash(self):
        """Test CLRHASH."""
        ht = make_hash_table()
        puthash('a', ht, 1)
        puthash('b', ht, 2)
        
        result = clrhash(ht)
        assert result == ht
        assert hash_table_count(ht) == 0
    
    def test_hash_table_count(self):
        """Test HASH-TABLE-COUNT."""
        ht = make_hash_table()
        assert hash_table_count(ht) == 0
        
        puthash('a', ht, 1)
        assert hash_table_count(ht) == 1
        
        puthash('b', ht, 2)
        assert hash_table_count(ht) == 2
    
    def test_hash_table_test(self):
        """Test HASH-TABLE-TEST."""
        ht = make_hash_table(test='equal')
        assert hash_table_test(ht) == 'equal'
    
    def test_hash_table_size(self):
        """Test HASH-TABLE-SIZE."""
        ht = make_hash_table(size=32)
        assert hash_table_size(ht) == 32


class TestHashTableIteration:
    """Test hash table iteration functions."""
    
    def test_hash_table_keys(self):
        """Test HASH-TABLE-KEYS."""
        ht = make_hash_table()
        puthash('a', ht, 1)
        puthash('b', ht, 2)
        puthash('c', ht, 3)
        
        keys = hash_table_keys(ht)
        assert len(keys) == 3
        assert 'a' in keys
        assert 'b' in keys
        assert 'c' in keys
    
    def test_hash_table_values(self):
        """Test HASH-TABLE-VALUES."""
        ht = make_hash_table()
        puthash('x', ht, 10)
        puthash('y', ht, 20)
        
        values = hash_table_values(ht)
        assert len(values) == 2
        assert 10 in values
        assert 20 in values
    
    def test_hash_table_items(self):
        """Test HASH-TABLE-ITEMS."""
        ht = make_hash_table()
        puthash('p', ht, 100)
        puthash('q', ht, 200)
        
        items = hash_table_items(ht)
        assert len(items) == 2
        assert ['p', 100] in items
        assert ['q', 200] in items


class TestHashTableMaphash:
    """Test MAPHASH function."""
    
    def test_maphash(self):
        """Test MAPHASH."""
        ht = make_hash_table()
        puthash('a', ht, 1)
        puthash('b', ht, 2)
        puthash('c', ht, 3)
        
        # Simple function to count entries
        count = [0]
        def counter(key, value):
            count[0] += 1
        
        result = maphash(counter, ht)
        assert result == lisptype.NIL
        assert count[0] == 3


class TestHashTableIntegration:
    """Integration tests for hash tables."""
    
    def test_basic_workflow(self):
        """Test basic hash table workflow."""
        ht = make_hash_table()
        
        # Add entries
        puthash('name', ht, 'Alice')
        puthash('age', ht, 30)
        puthash('city', ht, 'NYC')
        
        # Check count
        assert hash_table_count(ht) == 3
        
        # Retrieve entries
        assert gethash('name', ht) == 'Alice'
        assert gethash('age', ht) == 30
        assert gethash('city', ht) == 'NYC'
        
        # Remove one
        remhash('age', ht)
        assert hash_table_count(ht) == 2
        
        # Check it's gone
        assert gethash('age', ht, lisptype.NIL) == lisptype.NIL
    
    def test_different_types_as_keys(self):
        """Test using different types as keys."""
        ht = make_hash_table()
        
        puthash('string_key', ht, 'value1')
        puthash(42, ht, 'value2')
        puthash(3.14, ht, 'value3')
        puthash(True, ht, 'value4')
        
        assert gethash('string_key', ht) == 'value1'
        assert gethash(42, ht) == 'value2'
        assert gethash(3.14, ht) == 'value3'
        assert gethash(True, ht) == 'value4'
    
    def test_nested_hash_tables(self):
        """Test hash table containing other hash tables."""
        inner = make_hash_table()
        puthash('inner_key', inner, 'inner_value')
        
        outer = make_hash_table()
        puthash('nested', outer, inner)
        
        retrieved = gethash('nested', outer)
        assert isinstance(retrieved, HashTable)
        assert gethash('inner_key', retrieved) == 'inner_value'
    
    def test_large_hash_table(self):
        """Test hash table with many entries."""
        ht = make_hash_table()
        
        # Add 100 entries
        for i in range(100):
            puthash(f'key{i}', ht, f'value{i}')
        
        assert hash_table_count(ht) == 100
        
        # Check random entries
        assert gethash('key0', ht) == 'value0'
        assert gethash('key50', ht) == 'value50'
        assert gethash('key99', ht) == 'value99'
    
    def test_hash_table_updates(self):
        """Test updating existing entries."""
        ht = make_hash_table()
        
        puthash('key', ht, 'original')
        assert gethash('key', ht) == 'original'
        
        puthash('key', ht, 'updated')
        assert gethash('key', ht) == 'updated'
        
        # Count should still be 1
        assert hash_table_count(ht) == 1
