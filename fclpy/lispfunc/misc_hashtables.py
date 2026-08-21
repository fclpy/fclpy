"""Hash table, array, and stream accessor operations."""

import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry


# --- Hash table operations ---
class HashTableDict(dict):
    """A hash table whose own options are attributes, not entries.

    The options used to be stored as three ``'__hashmeta__...'`` string keys
    *in the table itself*, which put them in the key space the table is
    supposed to hold user entries in. Everything that walks a hash table then
    has to know to skip them, and four places did (MAPHASH, CLRHASH,
    HASH-TABLE-COUNT and the printer) while everything else did not -- so
    ``(loop for k being the hash-keys of h ...)`` collected the Python string
    ``"__hashmeta__test"`` as a Lisp value (standing rule 2), and any new
    traversal would have needed a fifth copy of the same filter.

    Keeping them off the keys means a traversal is correct by default rather
    than by remembering, and the four filters are gone rather than five.
    """

    def __init__(self, test='EQL', size=16, rehash_size=1.5, rehash_threshold=0.75):
        super().__init__()
        self.test = str(test).upper()
        self.size = size
        self.rehash_size = rehash_size
        self.rehash_threshold = rehash_threshold


@_registry.cl_function('MAKE-HASH-TABLE')
def make_hash_table(test='EQL', size=16, rehash_size=1.5, rehash_threshold=0.75):
    """Create hash table with specified parameters."""
    return HashTableDict(test, size, rehash_size, rehash_threshold)


@_registry.cl_function('GETHASH')
def gethash(key, hashtable, default=None):
    """Get value from hash table."""
    if isinstance(hashtable, dict) and key in hashtable:
        return hashtable[key]
    return default


@_registry.cl_function('REMHASH')
def remhash(key, hashtable):
    """Remove entry from hash table."""
    if isinstance(hashtable, dict) and key in hashtable:
        del hashtable[key]
        return lisptype.T
    return lisptype.NIL


@_registry.cl_function('MAPHASH')
def maphash(function, hashtable):
    """Apply function to all hash table entries."""
    if isinstance(hashtable, dict):
        for k, v in list(hashtable.items()):
            function(k, v)
    return lisptype.NIL


@_registry.cl_function('CLRHASH')
def clrhash(hashtable):
    """Clear all entries from hash table."""
    if isinstance(hashtable, dict):
        hashtable.clear()
    return hashtable


@_registry.cl_function('SXHASH')
def sxhash(obj):
    """Stable hash for object."""
    try:
        return hash(obj)
    except Exception:
        return hash(str(obj))


@_registry.cl_function('HASH-TABLE-COUNT')
def hash_table_count(table):
    """Count entries in hash table."""
    return len(table) if isinstance(table, dict) else 0


@_registry.cl_function('HASH-TABLE-SIZE')
def hash_table_size(table):
    """Get hash table size."""
    return hash_table_count(table)


@_registry.cl_function('HASH-TABLE-TEST')
def hash_table_test(table):
    """Get hash table test function."""
    return getattr(table, 'test', None) if isinstance(table, dict) else None


@_registry.cl_function('HASH-TABLE-REHASH-SIZE')
def hash_table_rehash_size(table):
    """Get hash table rehash size."""
    return getattr(table, 'rehash_size', None) if isinstance(table, dict) else None


@_registry.cl_function('HASH-TABLE-REHASH-THRESHOLD')
def hash_table_rehash_threshold(table):
    """Get hash table rehash threshold."""
    return getattr(table, 'rehash_threshold', None) if isinstance(table, dict) else None


# --- Array operations ---


@_registry.cl_function('UPGRADED-COMPLEX-PART-TYPE')
def upgraded_complex_part_type(typespec, environment=None):
    """Get upgraded complex part type."""
    return 'REAL'


# --- Stream operations ---
#
# ECHO-STREAM-INPUT-STREAM, ECHO-STREAM-OUTPUT-STREAM, BROADCAST-STREAM-STREAMS,
# CONCATENATED-STREAM-STREAMS, SYNONYM-STREAM-SYMBOL, TWO-WAY-STREAM-INPUT-STREAM
# and TWO-WAY-STREAM-OUTPUT-STREAM now live in streams.py, next to the
# TwoWayStream/EchoStream/ConcatenatedStream/BroadcastStream/SynonymStream
# classes they accessed -- these used to be stubs that returned the composite
# stream object itself (or an empty Python list, i.e. a vector, for the two
# `-STREAMS` accessors) because there was no real composite-stream object to
# accessor into yet.


__all__ = [
    'make_hash_table',
    'gethash',
    'remhash',
    'maphash',
    'clrhash',
    'sxhash',
    'hash_table_count',
    'hash_table_size',
    'hash_table_test',
    'hash_table_rehash_size',
    'hash_table_rehash_threshold',
    'upgraded_complex_part_type',
]
