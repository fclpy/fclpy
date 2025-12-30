"""Sequence modification operations - remove, delete, substitute."""

from .core import cons, car, cdr, atom
from . import registry as _registry
import fclpy.lisptype as lisptype
from .sequences_search import iterate, _seq_length, _seq_to_list


@_registry.cl_function('REMOVE')
def remove(item, sequence, **kwargs):
    """Remove item from sequence.
    
    Supports:
      :key - function to apply to each element before comparison
      :test - comparison function (default is eql)
      :start - start index
      :end - end index
    """
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)
    
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    test = kwargs.get('test', lambda x, y: x == y)
    
    result = []
    iterator = iterate(sequence, start=start, end=end, key=key, test=test)
    
    # Add elements before start
    result.extend(sequence[:start])
    
    # Filter and add elements in range
    for element in iterator:
        if not iterator.matches(element, item):
            result.append(element)
    
    # Add elements after end
    if end < _seq_length(sequence):
        result.extend(sequence[end:])
    
    return result


@_registry.cl_function('REMOVE-IF')
def remove_if(test, sequence, **kwargs):
    """Remove elements satisfying test.
    
    Supports:
      :key - function to apply to each element before testing
      :start - start index
      :end - end index
    """
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)
    
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    
    result = []
    iterator = iterate(sequence, start=start, end=end, key=key)
    
    # Add elements before start
    result.extend(sequence[:start])
    
    # Filter and add elements in range
    for element in iterator:
        test_value = iterator.get_value(element)
        if not test(test_value):
            result.append(element)
    
    # Add elements after end
    if end < _seq_length(sequence):
        result.extend(sequence[end:])
    
    return result


@_registry.cl_function('REMOVE-IF-NOT')
def remove_if_not(test, sequence, **kwargs):
    """Remove elements not satisfying test.
    
    Supports:
      :key - function to apply to each element before testing
      :start - start index
      :end - end index
    """
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)
    
    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    
    result = []
    iterator = iterate(sequence, start=start, end=end, key=key)
    
    # Add elements before start
    result.extend(sequence[:start])
    
    # Filter and add elements in range
    for element in iterator:
        test_value = iterator.get_value(element)
        if test(test_value):
            result.append(element)
    
    # Add elements after end
    if end < _seq_length(sequence):
        result.extend(sequence[end:])
    
    return result


@_registry.cl_function('DELETE')
def delete_fn(item, sequence, **kwargs):
    """Delete item from sequence."""
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)
    return [x for x in sequence if x != item]


@_registry.cl_function('DELETE-IF')
def delete_if(predicate, sequence, **kwargs):
    """Delete if predicate true."""
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)
    return [x for x in sequence if not predicate(x)]


@_registry.cl_function('DELETE-IF-NOT')
def delete_if_not(predicate, sequence, **kwargs):
    """Delete if predicate false."""
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)
    return [x for x in sequence if predicate(x)]


@_registry.cl_function('REMOVE-DUPLICATES')
def remove_duplicates(sequence, **kwargs):
    """Remove duplicate elements."""
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)
    
    seen = set()
    result = []
    for item in sequence:
        # Handle unhashable items
        try:
            if item not in seen:
                seen.add(item)
                result.append(item)
        except TypeError:
            # Item is unhashable, use linear search
            if item not in result:
                result.append(item)
    return result


@_registry.cl_function('DELETE-DUPLICATES')
def delete_duplicates(sequence, **kwargs):
    """Delete duplicate elements."""
    return remove_duplicates(sequence, **kwargs)


@_registry.cl_function('SUBSTITUTE')
def substitute(newitem, olditem, sequence, **kwargs):
    """Substitute elements in sequence."""
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)
    return [newitem if x == olditem else x for x in sequence]


@_registry.cl_function('SUBSTITUTE-IF')
def substitute_if(newitem, test, sequence, **kwargs):
    """Substitute using predicate."""
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)
    return [newitem if test(x) else x for x in sequence]


@_registry.cl_function('SUBSTITUTE-IF-NOT')
def substitute_if_not(newitem, test, sequence, **kwargs):
    """Substitute using negated predicate."""
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)
    return [newitem if not test(x) else x for x in sequence]


@_registry.cl_function('NSUBSTITUTE')
def nsubstitute(newitem, olditem, sequence, **kwargs):
    """Destructively substitute."""
    return substitute(newitem, olditem, sequence, **kwargs)  # Non-destructive for now


@_registry.cl_function('NSUBSTITUTE-IF')
def nsubstitute_if(newitem, test, sequence, **kwargs):
    """Destructively substitute using predicate."""
    return substitute_if(newitem, test, sequence, **kwargs)  # Non-destructive for now


@_registry.cl_function('NSUBSTITUTE-IF-NOT')
def nsubstitute_if_not(newitem, test, sequence, **kwargs):
    """Destructively substitute using negated predicate."""
    return substitute_if_not(newitem, test, sequence, **kwargs)  # Non-destructive for now


@_registry.cl_function('SUBST')
def subst(new, old, tree, test=None):
    """Substitute old with new in tree."""
    if test is None:
        test = lambda x, y: x == y
    
    if test(tree, old):
        return new
    elif atom(tree):
        return tree
    else:
        return cons(subst(new, old, car(tree), test),
                   subst(new, old, cdr(tree), test))


@_registry.cl_function('SUBST-IF')
def subst_if(new, predicate, tree):
    """Substitute with predicate."""
    if predicate(tree):
        return new
    elif atom(tree):
        return tree
    else:
        return cons(subst_if(new, predicate, car(tree)),
                   subst_if(new, predicate, cdr(tree)))


@_registry.cl_function('SUBST-IF-NOT')
def subst_if_not(new, predicate, tree):
    """Substitute with negated predicate."""
    return subst_if(new, lambda x: not predicate(x), tree)


@_registry.cl_function('SUBLIS')
def sublis(alist, tree, test=None):
    """Substitute using association list."""
    if test is None:
        test = lambda x, y: x == y
    
    if atom(tree):
        for pair in alist:
            if pair and len(pair) > 1 and test(tree, pair[0]):
                return pair[1]
        return tree
    else:
        return cons(sublis(alist, car(tree), test),
                   sublis(alist, cdr(tree), test))


@_registry.cl_function('NSUBST')
def nsubst(new, old, tree, **kwargs):
    """Destructive substitute in tree."""
    return subst(new, old, tree)  # Non-destructive for now


@_registry.cl_function('NSUBST-IF')
def nsubst_if(new, predicate, tree, **kwargs):
    """Destructive substitute if in tree."""
    return subst_if(new, predicate, tree)  # Non-destructive for now


@_registry.cl_function('NSUBST-IF-NOT')
def nsubst_if_not(new, predicate, tree, **kwargs):
    """Destructive substitute if not in tree."""
    return subst_if_not(new, predicate, tree)  # Non-destructive for now


@_registry.cl_function('NSUBLIS')
def nsublis(alist, tree, **kwargs):
    """Destructive substitute using alist."""
    return sublis(alist, tree)  # Non-destructive for now


__all__ = [
    'remove', 'remove_if', 'remove_if_not',
    'delete_fn', 'delete_if', 'delete_if_not',
    'remove_duplicates', 'delete_duplicates',
    'substitute', 'substitute_if', 'substitute_if_not',
    'nsubstitute', 'nsubstitute_if', 'nsubstitute_if_not',
    'subst', 'subst_if', 'subst_if_not',
    'sublis', 'nsubst', 'nsubst_if', 'nsubst_if_not', 'nsublis',
]
