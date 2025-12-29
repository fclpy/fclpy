"""Hash tables for Phase 5 Task 7."""

import fclpy.lisptype as lisptype
from . import registry as _registry


class HashTable:
    """A hash table implementation."""
    
    def __init__(self, test='eql', size=16):
        """Initialize a hash table.
        
        Args:
            test: Test function to use ('eq', 'eql', 'equal', 'equalp')
            size: Initial table size
        """
        self.test = test
        self._data = {}  # Use Python dict
        self._size = size
        self._count = 0
    
    def _hash_key(self, key):
        """Convert key to hashable form."""
        # For now, just use str representation
        # In a real implementation, would use proper hashing
        if isinstance(key, (str, int, float, bool, type(None))):
            return key
        return str(key)
    
    def _compare_keys(self, key1, key2):
        """Compare two keys using test function."""
        if self.test == 'eq':
            return key1 is key2
        elif self.test == 'eql':
            return key1 is key2 or key1 == key2
        elif self.test == 'equal':
            return key1 == key2
        elif self.test == 'equalp':
            # Case-insensitive for strings
            if isinstance(key1, str) and isinstance(key2, str):
                return key1.lower() == key2.lower()
            return key1 == key2
        else:
            return key1 == key2
    
    def get(self, key, default=None):
        """Get value for key."""
        # Simple implementation: search through stored keys
        for stored_key, value in self._data.items():
            if self._compare_keys(stored_key, key):
                return value
        return default
    
    def set(self, key, value):
        """Set value for key."""
        # Check if key exists
        for stored_key in list(self._data.keys()):
            if self._compare_keys(stored_key, key):
                self._data[stored_key] = value
                return value
        
        # New key
        self._data[key] = value
        self._count += 1
        return value
    
    def remove(self, key):
        """Remove key from table."""
        for stored_key in list(self._data.keys()):
            if self._compare_keys(stored_key, key):
                del self._data[stored_key]
                self._count -= 1
                return lisptype.T
        return lisptype.NIL
    
    def clear(self):
        """Clear all entries."""
        self._data.clear()
        self._count = 0
    
    def count(self):
        """Get number of entries."""
        return self._count
    
    def keys(self):
        """Get all keys."""
        return list(self._data.keys())
    
    def values(self):
        """Get all values."""
        return list(self._data.values())
    
    def items(self):
        """Get all key-value pairs."""
        return list(self._data.items())
    
    def to_list(self):
        """Convert to list of [key, value] pairs."""
        return [[k, v] for k, v in self._data.items()]


@_registry.cl_function('MAKE-HASH-TABLE')
def make_hash_table(test='eql', size=16):
    """Create a hash table.
    
    Args:
        test: Test function ('eq', 'eql', 'equal', 'equalp')
        size: Initial size (not enforced)
    
    Returns:
        HashTable object
    """
    return HashTable(test, size)


@_registry.cl_function('HASH-TABLE-P')
def hash_table_p(obj):
    """Test if object is a hash table.
    
    Args:
        obj: Object to test
    
    Returns:
        T if hash table, NIL otherwise
    """
    return lisptype.lisp_bool(isinstance(obj, HashTable))


@_registry.cl_function('GETHASH')
def gethash(key, table, default=None):
    """Get value from hash table.
    
    Args:
        key: Key to look up
        table: Hash table
        default: Default value if key not found
    
    Returns:
        (value, present-p) as multiple values (returns tuple)
        Or just value if default context
    """
    if not isinstance(table, HashTable):
        raise TypeError(f"Expected HashTable, got {type(table)}")
    
    for stored_key, value in table.items():
        if table._compare_keys(stored_key, key):
            return value
    
    return default


@_registry.cl_function('PUTHASH')
def puthash(key, table, value):
    """Set value in hash table.
    
    Args:
        key: Key to set
        table: Hash table
        value: Value to store
    
    Returns:
        value
    """
    if not isinstance(table, HashTable):
        raise TypeError(f"Expected HashTable, got {type(table)}")
    
    return table.set(key, value)


@_registry.cl_function('REMHASH')
def remhash(key, table):
    """Remove entry from hash table.
    
    Args:
        key: Key to remove
        table: Hash table
    
    Returns:
        T if key was present, NIL otherwise
    """
    if not isinstance(table, HashTable):
        raise TypeError(f"Expected HashTable, got {type(table)}")
    
    return table.remove(key)


@_registry.cl_function('CLRHASH')
def clrhash(table):
    """Clear all entries from hash table.
    
    Args:
        table: Hash table to clear
    
    Returns:
        table
    """
    if not isinstance(table, HashTable):
        raise TypeError(f"Expected HashTable, got {type(table)}")
    
    table.clear()
    return table


@_registry.cl_function('HASH-TABLE-COUNT')
def hash_table_count(table):
    """Get number of entries in hash table.
    
    Args:
        table: Hash table
    
    Returns:
        Integer count
    """
    if not isinstance(table, HashTable):
        raise TypeError(f"Expected HashTable, got {type(table)}")
    
    return table.count()


@_registry.cl_function('HASH-TABLE-TEST')
def hash_table_test(table):
    """Get test function of hash table.
    
    Args:
        table: Hash table
    
    Returns:
        Symbol naming test function
    """
    if not isinstance(table, HashTable):
        raise TypeError(f"Expected HashTable, got {type(table)}")
    
    return table.test


@_registry.cl_function('HASH-TABLE-SIZE')
def hash_table_size(table):
    """Get size of hash table.
    
    Args:
        table: Hash table
    
    Returns:
        Integer size
    """
    if not isinstance(table, HashTable):
        raise TypeError(f"Expected HashTable, got {type(table)}")
    
    return table._size


@_registry.cl_function('MAPHASH')
def maphash(function, table):
    """Apply function to each entry in hash table.
    
    Args:
        function: Function taking (key, value)
        table: Hash table
    
    Returns:
        NIL
    """
    if not isinstance(table, HashTable):
        raise TypeError(f"Expected HashTable, got {type(table)}")
    
    for key, value in table.items():
        # Call function with key and value
        # Function should be callable from lispfunc context
        if callable(function):
            function(key, value)
        else:
            # Try to call as Lisp function
            # For now, just call directly
            try:
                function(key, value)
            except TypeError:
                pass
    
    return lisptype.NIL


@_registry.cl_function('WITH-HASH-TABLE-ITERATOR')
def with_hash_table_iterator(table):
    """Create an iterator for hash table.
    
    Args:
        table: Hash table
    
    Returns:
        Iterator over (key, value) pairs
    """
    if not isinstance(table, HashTable):
        raise TypeError(f"Expected HashTable, got {type(table)}")
    
    # Return an iterator
    return iter(table.items())


# Convenience functions for Lisp compatibility

@_registry.cl_function('HASH-TABLE-KEYS')
def hash_table_keys(table):
    """Get all keys from hash table as list.
    
    Args:
        table: Hash table
    
    Returns:
        List of keys
    """
    if not isinstance(table, HashTable):
        raise TypeError(f"Expected HashTable, got {type(table)}")
    
    return table.keys()


@_registry.cl_function('HASH-TABLE-VALUES')
def hash_table_values(table):
    """Get all values from hash table as list.
    
    Args:
        table: Hash table
    
    Returns:
        List of values
    """
    if not isinstance(table, HashTable):
        raise TypeError(f"Expected HashTable, got {type(table)}")
    
    return table.values()


@_registry.cl_function('HASH-TABLE-ITEMS')
def hash_table_items(table):
    """Get all key-value pairs from hash table.
    
    Args:
        table: Hash table
    
    Returns:
        List of [key, value] pairs
    """
    if not isinstance(table, HashTable):
        raise TypeError(f"Expected HashTable, got {type(table)}")
    
    return table.to_list()
