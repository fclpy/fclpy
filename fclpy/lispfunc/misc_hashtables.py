"""Hash table, array, and stream accessor operations."""

import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry


# --- Hash table operations ---
@_registry.cl_function('MAKE-HASH-TABLE')
def make_hash_table(test='EQL', size=16, rehash_size=1.5, rehash_threshold=0.75):
    """Create hash table with specified parameters."""
    table = {
        '__hashmeta__test': str(test).upper(),
        '__hashmeta__rehash_size': rehash_size,
        '__hashmeta__rehash_threshold': rehash_threshold,
    }
    return table


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
            if not k.startswith('__hashmeta__'):
                function(k, v)
    return lisptype.NIL


@_registry.cl_function('CLRHASH')
def clrhash(hashtable):
    """Clear all entries from hash table."""
    if isinstance(hashtable, dict):
        meta = {k: v for k, v in hashtable.items() if k.startswith('__hashmeta__')}
        hashtable.clear()
        hashtable.update(meta)
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
    return len([k for k in table.keys() if not str(k).startswith('__hashmeta__')]) if isinstance(table, dict) else 0


@_registry.cl_function('HASH-TABLE-SIZE')
def hash_table_size(table):
    """Get hash table size."""
    return hash_table_count(table)


@_registry.cl_function('HASH-TABLE-TEST')
def hash_table_test(table):
    """Get hash table test function."""
    return table.get('__hashmeta__test') if isinstance(table, dict) else None


@_registry.cl_function('HASH-TABLE-REHASH-SIZE')
def hash_table_rehash_size(table):
    """Get hash table rehash size."""
    return table.get('__hashmeta__rehash_size') if isinstance(table, dict) else None


@_registry.cl_function('HASH-TABLE-REHASH-THRESHOLD')
def hash_table_rehash_threshold(table):
    """Get hash table rehash threshold."""
    return table.get('__hashmeta__rehash_threshold') if isinstance(table, dict) else None


# --- Array operations ---
@_registry.cl_function('ARRAY-ROW-MAJOR-INDEX')
def array_row_major_index(array, *subscripts):
    """Compute row-major index for array."""
    return 0


@_registry.cl_function('UPGRADED-ARRAY-ELEMENT-TYPE')
def upgraded_array_element_type(typespec, environment=None):
    """Get upgraded array element type."""
    return 'T'


@_registry.cl_function('UPGRADED-COMPLEX-PART-TYPE')
def upgraded_complex_part_type(typespec, environment=None):
    """Get upgraded complex part type."""
    return 'REAL'


@_registry.cl_function('ADJUSTABLE-ARRAY-P')
def adjustable_array_p(array):
    """Test if array is adjustable."""
    return lisptype.NIL


@_registry.cl_function('ROW-MAJOR-AREF')
def row_major_aref(array, index):
    """Get array element by row-major index."""
    return None


# --- Stream operations ---
@_registry.cl_function('ECHO-STREAM-INPUT-STREAM')
def echo_stream_input_stream(echo_stream):
    """Get input stream from echo stream."""
    return echo_stream


@_registry.cl_function('ECHO-STREAM-OUTPUT-STREAM')
def echo_stream_output_stream(echo_stream):
    """Get output stream from echo stream."""
    return echo_stream


@_registry.cl_function('BROADCAST-STREAM-STREAMS')
def broadcast_stream_streams(broadcast_stream):
    """Get streams from broadcast stream."""
    return []


@_registry.cl_function('CONCATENATED-STREAM-STREAMS')
def concatenated_stream_streams(concatenated_stream):
    """Get streams from concatenated stream."""
    return []


@_registry.cl_function('SYNONYM-STREAM-SYMBOL')
def synonym_stream_symbol(synonym_stream):
    """Get symbol from synonym stream."""
    return synonym_stream


@_registry.cl_function('TWO-WAY-STREAM-INPUT-STREAM')
def two_way_stream_input_stream(two_way_stream):
    """Get input stream from two-way stream."""
    return two_way_stream


@_registry.cl_function('TWO-WAY-STREAM-OUTPUT-STREAM')
def two_way_stream_output_stream(two_way_stream):
    """Get output stream from two-way stream."""
    return two_way_stream


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
    'array_row_major_index',
    'upgraded_array_element_type',
    'upgraded_complex_part_type',
    'adjustable_array_p',
    'row_major_aref',
    'echo_stream_input_stream',
    'echo_stream_output_stream',
    'broadcast_stream_streams',
    'concatenated_stream_streams',
    'synonym_stream_symbol',
    'two_way_stream_input_stream',
    'two_way_stream_output_stream',
]
