"""Sequence modification operations - remove, delete, substitute."""

from .core import cons, car, cdr, atom, _consp_internal
from . import registry as _registry
import fclpy.lisptype as lisptype
from .sequences_search import (
    iterate, _seq_length, _seq_to_list, _make_matcher, _coerce_function_designator,
    _lisp_truthy, _rebuild_sequence, _matched_positions, _two_sequence_matcher,
    _alist_pairs, _pair_key, _call_checked, _apply_key,
)
from .sequence_protocol import bounding_indices as _bounding_indices


@_registry.cl_function('REMOVE')
def remove(item, sequence, **kwargs):
    """Remove item from sequence (CLHS 17.2.1). Non-destructive: `sequence`
    is never mutated, and the result is of the same kind (list/string/
    vector) as `sequence`, not a raw Python list (plan.md Finding M).

    Supports:
      :key - function (or designator) to apply to each element before comparison
      :test / :test-not - comparison function/designator (default is eql-like)
      :count - maximum number of elements to remove
      :from-end - if true, the :count-limited elements are the last matches
      :start - start index
      :end - end index
    """
    original = sequence
    elements = _seq_to_list(sequence)
    start, end = _bounding_indices(
        len(elements), kwargs.get('start', 0), kwargs.get('end'))
    key = kwargs.get('key', None)
    test = kwargs.get('test', None)
    test_not = kwargs.get('test_not', None)
    from_end = _lisp_truthy(kwargs.get('from_end', None))
    matcher = _make_matcher(test=test, test_not=test_not, key=key)

    doomed = _matched_positions(
        start, end, from_end, kwargs.get('count', None),
        lambda i: matcher(item, elements[i]),
    )

    result = [x for i, x in enumerate(elements) if i not in doomed]
    return _rebuild_sequence(original, result)


@_registry.cl_function('REMOVE-IF')
def remove_if(test, sequence, **kwargs):
    """Remove elements satisfying test (CLHS 17.2.1).

    Supports:
      :key - function (or designator) to apply to each element before testing
      :count - maximum number of elements to remove
      :from-end - if true, the :count-limited elements are the last matches
      :start - start index
      :end - end index
    """
    original = sequence
    elements = _seq_to_list(sequence)
    start, end = _bounding_indices(
        len(elements), kwargs.get('start', 0), kwargs.get('end'))
    key = _coerce_function_designator(kwargs.get('key', None))
    test = _coerce_function_designator(test)
    from_end = _lisp_truthy(kwargs.get('from_end', None))

    doomed = _matched_positions(
        start, end, from_end, kwargs.get('count', None),
        lambda i: _lisp_truthy(test(_apply_key(key, elements[i]))),
    )

    result = [x for i, x in enumerate(elements) if i not in doomed]
    return _rebuild_sequence(original, result)


@_registry.cl_function('REMOVE-IF-NOT')
def remove_if_not(test, sequence, **kwargs):
    """Remove elements not satisfying test (CLHS 17.2.1).

    Supports:
      :key - function (or designator) to apply to each element before testing
      :count - maximum number of elements to remove
      :from-end - if true, the :count-limited elements are the last matches
      :start - start index
      :end - end index
    """
    original = sequence
    elements = _seq_to_list(sequence)
    start, end = _bounding_indices(
        len(elements), kwargs.get('start', 0), kwargs.get('end'))
    key = _coerce_function_designator(kwargs.get('key', None))
    test = _coerce_function_designator(test)
    from_end = _lisp_truthy(kwargs.get('from_end', None))

    doomed = _matched_positions(
        start, end, from_end, kwargs.get('count', None),
        lambda i: not _lisp_truthy(test(_apply_key(key, elements[i]))),
    )

    result = [x for i, x in enumerate(elements) if i not in doomed]
    return _rebuild_sequence(original, result)


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
    """Remove duplicate elements (CLHS 17.2.1).

    Duplicates are decided by the shared `:test`/`:test-not`/`:key` matcher,
    not by Python set membership: the old version put elements in a `set`, so
    the comparison was Python hashing and equality -- `:test` and `:key` were
    ignored entirely, `1` and `1.0` collided, and two `EQUAL` lists did not.
    Which of a pair survives is CLHS's rule: the *later* element by default,
    the earlier one under `:from-end`. Both arguments of the test are sequence
    elements, so the `:key` applies to both and the earlier element is always
    passed first -- `_two_sequence_matcher` is the same rule MISMATCH and
    SEARCH need, for the same reason.
    """
    elements = _seq_to_list(sequence)
    start = kwargs.get('start', 0)
    end = kwargs.get('end', None)
    start, end = _bounding_indices(len(elements), start, end, 'REMOVE-DUPLICATES')
    matcher = _two_sequence_matcher(kwargs)
    from_end = _lisp_truthy(kwargs.get('from_end', None))

    keep = []
    for index in range(start, end):
        item = elements[index]
        if from_end:
            duplicate = any(matcher(elements[j], item) for j in keep)
        else:
            duplicate = any(matcher(item, elements[j])
                            for j in range(index + 1, end))
        if not duplicate:
            keep.append(index)
    kept = set(keep)
    result = [item for index, item in enumerate(elements)
              if index < start or index >= end or index in kept]
    return _rebuild_sequence(sequence, result)


@_registry.cl_function('DELETE-DUPLICATES')
def delete_duplicates(sequence, **kwargs):
    """REMOVE-DUPLICATES, permitted to destroy its argument (CLHS 17.2.1)."""
    return remove_duplicates(sequence, **kwargs)


@_registry.cl_function('SUBSTITUTE')
def substitute(newitem, olditem, sequence, **kwargs):
    """Substitute elements in sequence (CLHS 17.2.1). Non-destructive.

    Supports :key, :test/:test-not, :count, :from-end, :start, :end like REMOVE.
    """
    original = sequence
    elements = _seq_to_list(sequence)
    start, end = _bounding_indices(
        len(elements), kwargs.get('start', 0), kwargs.get('end'))
    key = kwargs.get('key', None)
    test = kwargs.get('test', None)
    test_not = kwargs.get('test_not', None)
    from_end = _lisp_truthy(kwargs.get('from_end', None))
    matcher = _make_matcher(test=test, test_not=test_not, key=key)

    chosen = _matched_positions(
        start, end, from_end, kwargs.get('count', None),
        lambda i: matcher(olditem, elements[i]),
    )

    result = [newitem if i in chosen else x for i, x in enumerate(elements)]
    return _rebuild_sequence(original, result)


@_registry.cl_function('SUBSTITUTE-IF')
def substitute_if(newitem, test, sequence, **kwargs):
    """Substitute using predicate (CLHS 17.2.1). Non-destructive.

    Supports :key, :count, :from-end, :start, :end.
    """
    original = sequence
    elements = _seq_to_list(sequence)
    start, end = _bounding_indices(
        len(elements), kwargs.get('start', 0), kwargs.get('end'))
    key = _coerce_function_designator(kwargs.get('key', None))
    test = _coerce_function_designator(test)
    from_end = _lisp_truthy(kwargs.get('from_end', None))

    chosen = _matched_positions(
        start, end, from_end, kwargs.get('count', None),
        lambda i: _lisp_truthy(test(_apply_key(key, elements[i]))),
    )

    result = [newitem if i in chosen else x for i, x in enumerate(elements)]
    return _rebuild_sequence(original, result)


@_registry.cl_function('SUBSTITUTE-IF-NOT')
def substitute_if_not(newitem, test, sequence, **kwargs):
    """Substitute using negated predicate (CLHS 17.2.1). Non-destructive.

    Supports :key, :count, :from-end, :start, :end.
    """
    original = sequence
    elements = _seq_to_list(sequence)
    start, end = _bounding_indices(
        len(elements), kwargs.get('start', 0), kwargs.get('end'))
    key = _coerce_function_designator(kwargs.get('key', None))
    test = _coerce_function_designator(test)
    from_end = _lisp_truthy(kwargs.get('from_end', None))

    chosen = _matched_positions(
        start, end, from_end, kwargs.get('count', None),
        lambda i: not _lisp_truthy(test(_apply_key(key, elements[i]))),
    )

    result = [newitem if i in chosen else x for i, x in enumerate(elements)]
    return _rebuild_sequence(original, result)


def _apply_nsubstitution(sequence, newitem, chosen):
    """Shared engine for NSUBSTITUTE/NSUBSTITUTE-IF/NSUBSTITUTE-IF-NOT once
    the set of positions to replace has been decided.

    CLHS 17.2.1 permits (and ANSI's own `nsubstitute-list.2`-style tests
    require, since they re-read the original binding rather than the
    return value) `sequence` to be modified in place: a cons list is
    mutated cell-by-cell via `.car`; a vector (`AdjustableVector`, a plain
    Python `list`) or `LispString` supports `__setitem__` and is mutated
    element-by-element the same way regardless of which of those concrete
    types it is -- duck-typed rather than an `isinstance` list, so a vector
    representation nothing here has been taught about yet still works. A
    plain Python `str` cannot be mutated in place, but the ANSI string
    tests only ever check the *returned* value, never the original
    binding, so returning a freshly built same-kind result is sufficient
    there.
    """
    if hasattr(sequence, 'car') and hasattr(sequence, 'cdr'):
        cell = sequence
        i = 0
        while _consp_internal(cell):
            if i in chosen:
                cell.car = newitem
            cell = cell.cdr
            i += 1
        return sequence
    if hasattr(sequence, '__setitem__'):
        for i in chosen:
            sequence[i] = newitem
        return sequence
    elements = _seq_to_list(sequence)
    for i in chosen:
        elements[i] = newitem
    return _rebuild_sequence(sequence, elements)


@_registry.cl_function('NSUBSTITUTE')
def nsubstitute(newitem, olditem, sequence, **kwargs):
    """Destructively substitute (CLHS 17.2.1). See `_apply_nsubstitution`
    for what "destructively" means for each sequence representation.
    """
    elements = _seq_to_list(sequence)
    start, end = _bounding_indices(
        len(elements), kwargs.get('start', 0), kwargs.get('end'))
    key = kwargs.get('key', None)
    test = kwargs.get('test', None)
    test_not = kwargs.get('test_not', None)
    from_end = _lisp_truthy(kwargs.get('from_end', None))
    matcher = _make_matcher(test=test, test_not=test_not, key=key)

    chosen = _matched_positions(
        start, end, from_end, kwargs.get('count', None),
        lambda i: matcher(olditem, elements[i]),
    )
    return _apply_nsubstitution(sequence, newitem, chosen)


@_registry.cl_function('NSUBSTITUTE-IF')
def nsubstitute_if(newitem, test, sequence, **kwargs):
    """Destructively substitute using predicate (CLHS 17.2.1)."""
    elements = _seq_to_list(sequence)
    start, end = _bounding_indices(
        len(elements), kwargs.get('start', 0), kwargs.get('end'))
    key = _coerce_function_designator(kwargs.get('key', None))
    test = _coerce_function_designator(test)
    from_end = _lisp_truthy(kwargs.get('from_end', None))

    chosen = _matched_positions(
        start, end, from_end, kwargs.get('count', None),
        lambda i: _lisp_truthy(test(_apply_key(key, elements[i]))),
    )
    return _apply_nsubstitution(sequence, newitem, chosen)


@_registry.cl_function('NSUBSTITUTE-IF-NOT')
def nsubstitute_if_not(newitem, test, sequence, **kwargs):
    """Destructively substitute using negated predicate (CLHS 17.2.1)."""
    elements = _seq_to_list(sequence)
    start, end = _bounding_indices(
        len(elements), kwargs.get('start', 0), kwargs.get('end'))
    key = _coerce_function_designator(kwargs.get('key', None))
    test = _coerce_function_designator(test)
    from_end = _lisp_truthy(kwargs.get('from_end', None))

    chosen = _matched_positions(
        start, end, from_end, kwargs.get('count', None),
        lambda i: not _lisp_truthy(test(_apply_key(key, elements[i]))),
    )
    return _apply_nsubstitution(sequence, newitem, chosen)


@_registry.cl_function('SUBST')
def subst(new, old, tree, *, test=None, test_not=None, key=None):
    """Substitute old with new in tree.

    Per CLHS 15.4, the test is called with `old` as the first argument and
    the (possibly key-transformed) subexpression as the second -- this
    previously called `test(tree, old)`, the reversed order (plan.md X3).
    No trailing `**kwargs`: `split_keyword_args` (evaluation_core.py) itself
    recognizes and consumes `:allow-other-keys` for a named-parameter
    callee now, and a genuinely unrecognized keyword is a PROGRAM-ERROR
    (CLHS 3.4.1.4) rather than the arity TypeError or silent no-op a
    catch-all `**kwargs` produced.
    """
    matcher = _make_matcher(test=test, test_not=test_not, key=key)

    if matcher(old, tree):
        return new
    elif atom(tree):
        return tree
    else:
        return cons(subst(new, old, car(tree), test=test, test_not=test_not, key=key),
                    subst(new, old, cdr(tree), test=test, test_not=test_not, key=key))


@_registry.cl_function('SUBST-IF')
def subst_if(new, predicate, tree, *, key=None):
    """Substitute with predicate (CLHS 15.4).

    `:key` is applied to each subexpression before testing it, previously
    absent entirely. `:key` is this function's *only* keyword, and declaring
    it keyword-only is what lets `split_keyword_args` say so: `(subst-if 'a
    #'null nil :test)` is now the PROGRAM-ERROR `subst-if.error.5` asks for
    (a dangling `:test`, odd count, unrecognized name) instead of a fourth
    positional argument. While the `&key` set was inferred from defaulted
    positional parameters that could not be decided -- the same ambiguity
    `(intern "a" :cl-test)` needs resolved the other way -- and `.error.5`
    was a known deviation.
    """
    predicate = _coerce_function_designator(predicate)
    key = _coerce_function_designator(key)
    target = key(tree) if key else tree
    if _lisp_truthy(predicate(target)):
        return new
    elif atom(tree):
        return tree
    else:
        return cons(subst_if(new, predicate, car(tree), key=key),
                   subst_if(new, predicate, cdr(tree), key=key))


@_registry.cl_function('SUBST-IF-NOT')
def subst_if_not(new, predicate, tree, *, key=None):
    """Substitute with negated predicate."""
    predicate = _coerce_function_designator(predicate)
    return subst_if(new, lambda x: not _lisp_truthy(predicate(x)), tree, key=key)


@_registry.cl_function('SUBLIS')
def sublis(alist, tree, *, test=None, test_not=None, key=None):
    """Replace every subtree that `alist` has an entry for (CLHS 14.2).

    Two things distinguish SUBLIS from SUBST, and this got both wrong:

    * **Every subtree is a candidate, not only every atom.** It substituted
      only in the `atom(tree)` branch, so `sublis.2`'s alist of *list* keys
      (`((f1 a b) . (f2 a b))`) never matched anything. A subtree that *is*
      replaced is not then descended into, which is why that same test does
      not re-substitute its own result back again.
    * **The test's first argument is the subtree, the second is the alist
      key** -- SUBLIS looks each subtree *up* in the alist, exactly as ASSOC
      does, so the subtree is the "item" of CLHS 17.2.1. `sublis.9` pins the
      direction with an asymmetric test (`(< x y)` over `(0 3 8 20)` against
      keys `1 5 10`): under the reverse order nothing matches at all. SUBST is
      the other way round -- there `old` is the item (`subst.10`) -- and that
      is why they cannot share one matcher call.

    `:key` therefore applies to the subtree only, never to the alist key
    (`sublis.7`'s key returns the car of a cons and `*not-present*` for an
    atom, and the alist keys are raw strings), so it is applied here rather
    than through `_make_matcher`, whose `key` transforms the *candidate*.
    """
    # CLHS 14.2: each alist entry is a dotted pair `(old . new)`, not a
    # 2-element list. `_alist_pairs`/`_pair_key` are ASSOC's shared alist-pair
    # accessors (standing rule 3: reuse rather than re-derive).
    pairs = [pair for pair in _alist_pairs(alist, 'SUBLIS')
             if pair is not None and pair is not lisptype.NIL]
    matcher = _make_matcher(test=test, test_not=test_not)
    key = _coerce_function_designator(key)

    def walk(subtree):
        probe = _call_checked(key, subtree, caller_name=':KEY') if key else subtree
        for pair in pairs:
            if matcher(probe, _pair_key(pair, 0)):
                return _pair_key(pair, 1)
        if _consp_internal(subtree):
            return cons(walk(subtree.car), walk(subtree.cdr))
        return subtree

    return walk(tree)


@_registry.cl_function('NSUBST')
def nsubst(new, old, tree, *, test=None, test_not=None, key=None):
    """Destructive substitute in tree (non-destructive for now).

    Previously discarded :test/:test-not/:key entirely by calling `subst`
    with none of `**kwargs` forwarded.
    """
    return subst(new, old, tree, test=test, test_not=test_not, key=key)


@_registry.cl_function('NSUBST-IF')
def nsubst_if(new, predicate, tree, *, key=None):
    """Destructive substitute if in tree (non-destructive for now)."""
    return subst_if(new, predicate, tree, key=key)


@_registry.cl_function('NSUBST-IF-NOT')
def nsubst_if_not(new, predicate, tree, *, key=None):
    """Destructive substitute if not in tree (non-destructive for now)."""
    return subst_if_not(new, predicate, tree, key=key)


@_registry.cl_function('NSUBLIS')
def nsublis(alist, tree, *, test=None, test_not=None, key=None):
    """Destructive substitute using alist (non-destructive for now).

    Previously discarded :test/:test-not/:key entirely by calling `sublis`
    with none of `**kwargs` forwarded.
    """
    return sublis(alist, tree, test=test, test_not=test_not, key=key)


__all__ = [
    'remove', 'remove_if', 'remove_if_not',
    'delete_fn', 'delete_if', 'delete_if_not',
    'remove_duplicates', 'delete_duplicates',
    'substitute', 'substitute_if', 'substitute_if_not',
    'nsubstitute', 'nsubstitute_if', 'nsubstitute_if_not',
    'subst', 'subst_if', 'subst_if_not',
    'sublis', 'nsubst', 'nsubst_if', 'nsubst_if_not', 'nsublis',
]
