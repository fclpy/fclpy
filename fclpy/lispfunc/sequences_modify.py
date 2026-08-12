"""Sequence modification operations - remove, delete, substitute."""

from .core import cons, car, cdr, atom
from . import registry as _registry
import fclpy.lisptype as lisptype
from .sequences_search import (
    iterate, _seq_length, _seq_to_list, _make_matcher, _coerce_function_designator,
    _lisp_truthy,
)


@_registry.cl_function('REMOVE')
def remove(item, sequence, **kwargs):
    """Remove item from sequence.

    Supports:
      :key - function (or designator) to apply to each element before comparison
      :test / :test-not - comparison function/designator (default is eql-like)
      :start - start index
      :end - end index
    """
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)

    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    test = kwargs.get('test', None)
    test_not = kwargs.get('test_not', None)

    result = []
    iterator = iterate(sequence, start=start, end=end, key=key, test=test, test_not=test_not)

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
      :key - function (or designator) to apply to each element before testing
      :start - start index
      :end - end index
    """
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)

    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    test = _coerce_function_designator(test)

    result = []
    iterator = iterate(sequence, start=start, end=end, key=key)

    # Add elements before start
    result.extend(sequence[:start])

    # Filter and add elements in range
    for element in iterator:
        test_value = iterator.get_value(element)
        if not _lisp_truthy(test(test_value)):
            result.append(element)

    # Add elements after end
    if end < _seq_length(sequence):
        result.extend(sequence[end:])

    return result


@_registry.cl_function('REMOVE-IF-NOT')
def remove_if_not(test, sequence, **kwargs):
    """Remove elements not satisfying test.

    Supports:
      :key - function (or designator) to apply to each element before testing
      :start - start index
      :end - end index
    """
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)

    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    test = _coerce_function_designator(test)

    result = []
    iterator = iterate(sequence, start=start, end=end, key=key)

    # Add elements before start
    result.extend(sequence[:start])

    # Filter and add elements in range
    for element in iterator:
        test_value = iterator.get_value(element)
        if _lisp_truthy(test(test_value)):
            result.append(element)

    # Add elements after end
    if end < _seq_length(sequence):
        result.extend(sequence[end:])

    return result


@_registry.cl_function('DELETE')
def delete_fn(item, sequence, **kwargs):
    """Delete item from sequence.

    Supports the same :test/:test-not/:key/:start/:end arguments as REMOVE.
    """
    return remove(item, sequence, **kwargs)


@_registry.cl_function('DELETE-IF')
def delete_if(predicate, sequence, **kwargs):
    """Delete if predicate true. Supports :key/:start/:end like REMOVE-IF."""
    return remove_if(predicate, sequence, **kwargs)


@_registry.cl_function('DELETE-IF-NOT')
def delete_if_not(predicate, sequence, **kwargs):
    """Delete if predicate false. Supports :key/:start/:end like REMOVE-IF-NOT."""
    return remove_if_not(predicate, sequence, **kwargs)


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
    """Substitute elements in sequence.

    Supports :key, :test/:test-not, :start, :end like REMOVE.
    """
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)

    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = kwargs.get('key', None)
    test = kwargs.get('test', None)
    test_not = kwargs.get('test_not', None)
    matcher = _make_matcher(test=test, test_not=test_not, key=key)

    return [
        newitem if (start <= i < end and matcher(olditem, x)) else x
        for i, x in enumerate(sequence)
    ]


@_registry.cl_function('SUBSTITUTE-IF')
def substitute_if(newitem, test, sequence, **kwargs):
    """Substitute using predicate. Supports :key, :start, :end."""
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)

    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = _coerce_function_designator(kwargs.get('key', None))
    test = _coerce_function_designator(test)

    return [
        newitem if (start <= i < end and _lisp_truthy(test(key(x) if key else x))) else x
        for i, x in enumerate(sequence)
    ]


@_registry.cl_function('SUBSTITUTE-IF-NOT')
def substitute_if_not(newitem, test, sequence, **kwargs):
    """Substitute using negated predicate. Supports :key, :start, :end."""
    # Convert lispCons to list
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        sequence = _seq_to_list(sequence)

    start = kwargs.get('start', 0)
    end = kwargs.get('end', _seq_length(sequence))
    key = _coerce_function_designator(kwargs.get('key', None))
    test = _coerce_function_designator(test)

    return [
        newitem if (start <= i < end and not _lisp_truthy(test(key(x) if key else x))) else x
        for i, x in enumerate(sequence)
    ]


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
def subst(new, old, tree, test=None, test_not=None, key=None):
    """Substitute old with new in tree.

    Per CLHS 15.4, the test is called with `old` as the first argument and
    the (possibly key-transformed) subexpression as the second -- this
    previously called `test(tree, old)`, the reversed order (plan.md X3).
    """
    matcher = _make_matcher(test=test, test_not=test_not, key=key)

    if matcher(old, tree):
        return new
    elif atom(tree):
        return tree
    else:
        return cons(subst(new, old, car(tree), test, test_not, key),
                   subst(new, old, cdr(tree), test, test_not, key))


@_registry.cl_function('SUBST-IF')
def subst_if(new, predicate, tree):
    """Substitute with predicate."""
    predicate = _coerce_function_designator(predicate)
    if _lisp_truthy(predicate(tree)):
        return new
    elif atom(tree):
        return tree
    else:
        return cons(subst_if(new, predicate, car(tree)),
                   subst_if(new, predicate, cdr(tree)))


@_registry.cl_function('SUBST-IF-NOT')
def subst_if_not(new, predicate, tree):
    """Substitute with negated predicate."""
    predicate = _coerce_function_designator(predicate)
    return subst_if(new, lambda x: not _lisp_truthy(predicate(x)), tree)


@_registry.cl_function('SUBLIS')
def sublis(alist, tree, test=None, test_not=None, key=None):
    """Substitute using association list.

    Per CLHS 15.4, :key is applied to each subexpression of `tree` (the
    candidate), and the test is called with the alist entry's key as the
    first argument -- this previously called `test(tree, pair[0])`, the
    reversed order (plan.md X3), with no :key support at all.
    """
    matcher = _make_matcher(test=test, test_not=test_not, key=key)

    if atom(tree):
        for pair in alist:
            if pair and len(pair) > 1 and matcher(pair[0], tree):
                return pair[1]
        return tree
    else:
        return cons(sublis(alist, car(tree), test, test_not, key),
                   sublis(alist, cdr(tree), test, test_not, key))


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
