"""Sequence operations - lists, vectors, and other sequence manipulation.

This module re-exports sequence operations from specialized submodules:
- sequences_search: Find, position, count, and search operations
- sequences_modify: Remove, delete, and substitute operations  
- sequences_compose: Concatenation, sorting, copying, and composition
- sequences_higher: Higher-order operations, arrays, and sets
"""

from .sequences_search import *
from .sequences_modify import *
from .sequences_compose import *
from .sequences_higher import *

# Comprehensive exports for backward compatibility
__all__ = [
    # From sequences_search
    'SequenceIterator', 'iterate', 'with_sequence_protocol',
    'find', 'find_if', 'find_if_not',
    'position', 'position_if', 'position_if_not',
    'count', 'count_if', 'count_if_not',
    'search', 'mismatch', 'member', 'member_if', 'member_if_not',
    'assoc', 'assoc_if', 'assoc_if_not', 'rassoc', 'rassoc_if', 'rassoc_if_not',
    # From sequences_modify
    'remove', 'remove_if', 'remove_if_not',
    'delete_fn', 'delete_if', 'delete_if_not',
    'remove_duplicates', 'delete_duplicates',
    'substitute', 'substitute_if', 'substitute_if_not',
    'nsubstitute', 'nsubstitute_if', 'nsubstitute_if_not',
    'subst', 'subst_if', 'subst_if_not',
    'sublis', 'nsubst', 'nsubst_if', 'nsubst_if_not', 'nsublis',
    # From sequences_compose
    'endp', 'length', 'reverse', 'nreverse',
    'append', 'nconc', 'nreconc', 'revappend', 'concatenate',
    'sort', 'stable_sort', 'merge',
    'subseq', 'copy_seq', 'copy_list', 'copy_alist',
    'fill', 'replace', 'nbutlast', 'last',
    'nthcdr', 'nth', 'elt', 'make_list', 'make_sequence',
    'list_fn', 'list_star', 'tree_equal', 'list_length',
    # From sequences_higher
    'adjoin', 'pairlis', 'acons',
    'every', 'some', 'notevery', 'notany',
    'map_fn', 'mapcar', 'mapcan', 'mapc', 'mapcon', 'maplist', 'mapl', 'reduce_fn',
    'intersection', 'union', 'nunion', 'set_difference', 'nset_difference',
    'set_exclusive_or', 'nset_exclusive_or', 'subsetp', 'nintersection',
    'pop_fn', 'push_fn', 'pushnew',
    'make_array', 'array_dimensions', 'arrayp', 'array_in_bounds_p',
    'array_displacement', 'array_dimension', 'adjust_array',
    'vectorp', 'simple_vector_p', 'bit_vector_p', 'simple_bit_vector_p',
    'aref', 'svref', 'vector_fn', 'vector_pop', 'vector_push', 'vector_push_extend',
    # Symbol-safe names
    'list_s_star_',
]
