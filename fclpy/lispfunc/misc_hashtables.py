"""The hash table object model (CLHS 18) -- one table, one key-equivalence model.

A Common Lisp hash table *is* its test: CLHS 18.1 defines a table as a
mapping from keys to values in which two keys denote the same entry exactly
when the table's test says they are equivalent. Everything else about a hash
table -- size, rehashing, traversal order -- is an implementation detail.

This module used to get that backwards. ``HashTableDict`` was a ``dict``
subclass whose ``test`` was an *attribute nothing ever read*, so keys were
compared by Python's ``__eq__``/``__hash__`` and the declared test was
decoration. The consequences were wrong *values*, not missing errors, and
they were invisible because a Python dict is a convincing hash table:

- ``(let ((h (make-hash-table :test 'equal))) (setf (gethash (list 1 2) h) 9)
  (gethash (list 1 2) h))`` answered **NIL** -- a ``lispCons`` hashes by
  identity, so an EQUAL table could not find a key it had just stored.
- ``(let ((h (make-hash-table))) (setf (gethash "ab" h) 9)
  (gethash (copy-seq "ab") h))`` answered **9**, though two distinct strings
  are not EQL. An EQL table behaved like an EQUAL one.
- ``(let ((h (make-hash-table))) (setf (gethash 1.0 h) 1) (gethash 1 h))``
  answered **1**, because ``1.0 == 1`` and ``hash(1.0) == hash(1)`` in
  Python, though ``(eql 1.0 1)`` is false.

The model here is the fix, and it is deliberately *not* a second
implementation of EQL: a key is bucketed by a coarse surrogate hash and the
candidates in that bucket are compared with **the canonical predicate from
`comparison.py`**. The equivalence relation a table implements is therefore
the Lisp predicate by construction -- there is no separate copy that can
drift from it (standing rule 3). ``hashtables.py``'s ``HashTable`` was that
separate copy: its own ``_compare_keys`` ladder, using Python ``==``, is
what this module replaced.

**When in doubt, collide.** A surrogate must satisfy only one property --
equivalent keys land in the same bucket -- and it is always safe to make it
coarser. An over-collision costs a predicate call; a *missed* collision is a
wrong answer. That is why a `Character` and a one-character Python `str` hash
alike (``comparison.eql`` crosses them), and why anything undecidable hashes
by identity rather than by a guess.

``SXHASH`` is the EQUAL surrogate rather than a function beside it, which is
CLHS 18.2.2's requirement stated as code: "(equal x y) implies (= (sxhash x)
(sxhash y))" is not a property to test for, it is what makes an EQUAL table
work at all.
"""

import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry


# ---------------------------------------------------------------------------
# The test designator
# ---------------------------------------------------------------------------

#: The four tests CLHS 18.1 permits. A hash table's test is one of these
#: *symbols* -- not a function object and not a Python string. HASH-TABLE-TEST
#: used to answer the Python string ``'EQL'`` (standing rule 2) and, when the
#: table was built with ``:test #'eq``, the *repr of a Python function*.
STANDARD_TESTS = ('EQ', 'EQL', 'EQUAL', 'EQUALP')


def _predicate_table():
    """The canonical predicates, by test name.

    Imported lazily: `comparison` imports `arrays` and `core`, and this
    module is pulled in during the `utilities` import chain.
    """
    from .comparison import eq, eql, equal, equalp
    return {'EQ': eq, 'EQL': eql, 'EQUAL': equal, 'EQUALP': equalp}


def resolve_test(designator):
    """The test name a ``:test`` argument designates (CLHS 18.1).

    ``:test`` is a *function designator*, so ``'eq``, ``#'eq`` and
    ``(symbol-function 'eq)`` must all name the same table -- and
    `HASH-TABLE-TEST` must answer the symbol EQ for all three
    (`hash-table-test.2`/`.3`/`.4`). A function object is mapped back to its
    name by identity against the canonical predicates, which is only
    possible *because* there is exactly one implementation of each.
    """
    if designator is lisptype.OMITTED:
        return 'EQL'

    # NIL is *not* omission. `lisptype.OMITTED` exists precisely so the two can
    # be told apart (CLAUDE.md: "`None` cannot tell 'omitted' from 'given
    # NIL'"), and treating a Python `None` as omission here reintroduced the
    # ambiguity on one of NIL's three spellings only -- so `:test nil` was a
    # silent EQL table when NIL arrived as `None` and a type error when it
    # arrived as `lispNull`. One answer for all three, and it is the error:
    # `:test` selects the table's semantics, so guessing at it is the kind of
    # silent wrong answer standing rule 4 forbids.
    name = getattr(designator, 'name', None)
    if name is None and isinstance(designator, str):
        name = str(designator)
    if name is None and callable(designator):
        for test_name, predicate in _predicate_table().items():
            if designator is predicate:
                name = test_name
                break

    if name is not None and name.upper() in STANDARD_TESTS:
        return name.upper()

    raise lisptype.LispTypeError(
        "MAKE-HASH-TABLE: :TEST must designate EQ, EQL, EQUAL or EQUALP, "
        f"not {designator!r}",
        expected_type='(MEMBER EQ EQL EQUAL EQUALP)',
        actual_value=designator)


# ---------------------------------------------------------------------------
# The surrogate hashes -- one per test, each consistent with its predicate
# ---------------------------------------------------------------------------

#: SXHASH must answer an ``(and unsigned-byte fixnum)`` (`sxhash.1`), so every
#: surrogate is folded into this range. `MOST_POSITIVE_FIXNUM` here is 2**63-1,
#: so 62 bits is comfortably inside it and stays positive.
_HASH_MASK = (1 << 62) - 1

#: Structural hashing is depth-bounded because a key may be *circular*:
#: `sxhash.16` builds two self-referential conses and requires their hashes to
#: be EQL, which an unbounded descent cannot even return from. The bound makes
#: a deep structure collide with its truncation, which is safe.
_STRUCTURE_DEPTH = 6


def _is_nil(obj):
    """NIL in any of its three Python spellings (CLAUDE.md's gotcha list)."""
    return obj is None or obj is lisptype.NIL or isinstance(obj, lisptype.lispNull)


def _char_of(obj):
    """The character `obj` is, or None.

    ``comparison.eql`` treats a `Character` and a one-character Python `str`
    as the same character, so they must hash alike or an EQL table could miss
    a key it holds.
    """
    if isinstance(obj, lisptype.Character):
        return obj.char
    if isinstance(obj, str) and not isinstance(obj, lisptype.LispString) and len(obj) == 1:
        return obj
    return None


def _string_of(obj):
    """The characters of `obj` if it is a string, else None.

    Asked of `comparison._string_characters`, the one helper that knows all
    three string representations (`LispString`, Python `str`, and a rank-1
    array of characters -- which is how a *displaced* or fill-pointered
    string is represented). `sxhash.5`/`.10`/`.21` require every one of those
    to hash alike, and reimplementing the recognition here is how they would
    stop agreeing.
    """
    from .comparison import _string_characters
    return _string_characters(obj)


def _bits_of(obj):
    """The active bits of `obj` if it is a bit vector, else None.

    `sxhash.4`/`.6`/`.22` require a bit vector, its copy, and its displaced
    and fill-pointered variants to hash alike; `arrays` owns which is which.
    """
    from . import arrays as _arrays
    if _arrays.is_bit_array(obj) and _arrays.array_rank_of(obj) == 1:
        return tuple(_arrays.array_elements(obj))
    return None


def _number_hash(obj):
    """A hash for a number, or None if `obj` is not one.

    Python's numeric hash is already consistent across `int`, `float`,
    `Fraction` and `complex` for mathematically equal values, which is what
    EQUALP needs (``(equalp 1 1.0)`` is true). It also gives
    ``hash(0.0) == hash(-0.0)``, which is what `sxhash.17`-`.19` require.
    """
    import fractions
    if isinstance(obj, bool):
        return None
    if isinstance(obj, (int, float, complex, fractions.Fraction)):
        return hash(obj)
    return None


def sxhash_key(obj, depth=0):
    """The EQUAL surrogate: a hashable value, equal for EQUAL keys.

    CLHS 5.3 says EQUAL descends into conses, strings, bit vectors and
    pathnames, and into *nothing else* -- so everything else is hashed the way
    EQL would hash it, and a general array is hashed by identity. That last
    point is what `sxhash.7` checks: mutating an element of a general array
    must not change its hash, which follows from EQUAL not looking inside it.
    """
    if _is_nil(obj):
        return ('SYM', 'NIL')

    number = _number_hash(obj)
    if number is not None:
        return ('NUM', number)

    # A symbol hashes by *name only*. `sxhash.13` requires two uninterned
    # `(make-symbol "FOO")`s to hash alike, and `.15`/`.23` require a symbol's
    # hash to survive its package being changed or deleted -- so neither
    # identity nor the package may take part.
    if isinstance(obj, lisptype.LispSymbol):
        return ('SYM', str(getattr(obj, 'name', obj)))

    # A character before a string: a one-character `str` is both to this
    # implementation (plan.md C13), and colliding them is the safe direction.
    char = _char_of(obj)
    if char is not None:
        return ('STR', char)

    chars = _string_of(obj)
    if chars is not None:
        return ('STR', chars)

    bits = _bits_of(obj)
    if bits is not None:
        return ('BV', bits)

    from .pathnames import Pathname
    if isinstance(obj, Pathname):
        # `Pathname.__eq__` compares components, so the namestring it renders
        # from them is the consistent surrogate (`sxhash.20`).
        return ('PATH', str(obj.namestring()))

    from .core import consp, car, cdr
    if consp(obj):
        if depth >= _STRUCTURE_DEPTH:
            return ('CONS', 'DEEP')
        return ('CONS', sxhash_key(car(obj), depth + 1),
                sxhash_key(cdr(obj), depth + 1))

    return ('ID', id(obj))


def _eql_key(obj):
    """The EQL surrogate.

    ``comparison.eql`` is identity, plus same-type-and-value for numbers and
    same-character for characters -- so those two get a value surrogate and
    everything else gets identity. The *type* is part of the number surrogate
    because ``(eql 1 1.0)`` is false while Python's ``hash(1) == hash(1.0)``;
    without it an EQL table conflated an integer with the float beside it.
    """
    if _is_nil(obj):
        return ('SYM', 'NIL')

    char = _char_of(obj)
    if char is not None:
        return ('CHAR', char)

    number = _number_hash(obj)
    if number is not None:
        return ('NUM', type(obj).__name__, number)

    return ('ID', id(obj))


def _eq_key(obj):
    """The EQ surrogate: identity, which is all ``comparison.eq`` tests.

    NIL is normalised because it has three Python spellings and they must
    share a bucket for the *predicate* to get a chance to compare them.
    """
    if _is_nil(obj):
        return ('SYM', 'NIL')
    return ('ID', id(obj))


def _equalp_key(obj, depth=0):
    """The EQUALP surrogate.

    EQUALP is coarser than EQUAL in three ways that matter here: numbers
    compare across types (so the *type* leaves the surrogate), characters and
    strings compare case-insensitively (so the surrogate upcases), and it
    descends into *general* arrays elementwise (so a vector hashes by its
    elements rather than by identity).
    """
    if _is_nil(obj):
        return ('SYM', 'NIL')

    number = _number_hash(obj)
    if number is not None:
        return ('NUM', number)

    if isinstance(obj, lisptype.LispSymbol):
        return ('SYM', str(getattr(obj, 'name', obj)))

    char = _char_of(obj)
    if char is not None:
        return ('STR', char.upper())

    chars = _string_of(obj)
    if chars is not None:
        return ('STR', chars.upper())

    bits = _bits_of(obj)
    if bits is not None:
        return ('BV', bits)

    from .pathnames import Pathname
    if isinstance(obj, Pathname):
        return ('PATH', str(obj.namestring()).upper())

    from .core import consp, car, cdr
    if consp(obj):
        if depth >= _STRUCTURE_DEPTH:
            return ('CONS', 'DEEP')
        return ('CONS', _equalp_key(car(obj), depth + 1),
                _equalp_key(cdr(obj), depth + 1))

    from .sequence_protocol import is_vector, seq_elements
    if is_vector(obj):
        if depth >= _STRUCTURE_DEPTH:
            return ('VEC', 'DEEP')
        return ('VEC', tuple(_equalp_key(e, depth + 1)
                             for e in seq_elements(obj)))

    return ('ID', id(obj))


_SURROGATES = {
    'EQ': _eq_key,
    'EQL': _eql_key,
    'EQUAL': sxhash_key,
    'EQUALP': _equalp_key,
}


# ---------------------------------------------------------------------------
# The table
# ---------------------------------------------------------------------------

class LispHashTable:
    """A hash table: a test, a capacity, and buckets of entries.

    Deliberately **not** a `dict` subclass. Being one is what let the previous
    implementation use Python key equality everywhere by accident: every
    ``table[key]``, ``key in table`` and ``table.items()`` in the rest of the
    tree kept working and kept being wrong. With a distinct class those sites
    fail loudly instead (standing rule 4), which is how the four separate
    ``table[key] = value`` writers behind ``(SETF GETHASH)`` were found.

    Ask `is_hash_table` rather than testing `isinstance`, the same way the
    array model is asked `is_array` -- there is one predicate so that
    `HASH-TABLE-P`, `TYPEP`, `TYPE-OF` and the printer cannot disagree, which
    they previously did: `HASH-TABLE-P` answered NIL for the very object
    `MAKE-HASH-TABLE` returns while `TYPEP` answered T.
    """

    __slots__ = ('test', 'size', 'rehash_size', 'rehash_threshold',
                 '_buckets', '_count', '_predicate_fn', '_surrogate_fn')

    def __init__(self, test='EQL', size=16, rehash_size=1.5,
                 rehash_threshold=1.0):
        self.test = test
        self.size = size
        self.rehash_size = rehash_size
        self.rehash_threshold = rehash_threshold
        self._buckets = {}
        self._count = 0
        # Bound on first use rather than here: `_predicate_table` imports
        # `comparison`, and a table built during the bootstrap could precede
        # it. Cached because these are on the hot path -- rebuilding the
        # predicate dict per lookup costs four imports and a dict per
        # `GETHASH`, and ansi-test's own RT keeps its 22000-entry test
        # registry in an EQUAL table.
        self._predicate_fn = None
        self._surrogate_fn = None

    # -- the key-equivalence model ------------------------------------------

    def _surrogate(self, key):
        fn = self._surrogate_fn
        if fn is None:
            fn = self._surrogate_fn = _SURROGATES[self.test]
        return fn(key)

    def _predicate(self):
        fn = self._predicate_fn
        if fn is None:
            fn = self._predicate_fn = _predicate_table()[self.test]
        return fn

    def _find(self, key):
        """The ``[key, value]`` cell for `key`, or None.

        The bucket narrows the search; **the predicate decides**. That split
        is the whole design: the surrogate may over-collide freely without
        affecting which keys are equivalent.
        """
        bucket = self._buckets.get(self._surrogate(key))
        if bucket is None:
            return None
        predicate = self._predicate()
        for cell in bucket:
            if predicate(cell[0], key) is lisptype.T:
                return cell
        return None

    # -- the operations ----------------------------------------------------

    def lookup(self, key, default=lisptype.NIL):
        """``(value, present-p)`` for `key`."""
        cell = self._find(key)
        if cell is None:
            return default, False
        return cell[1], True

    def put(self, key, value):
        cell = self._find(key)
        if cell is not None:
            cell[1] = value
            return value
        self._buckets.setdefault(self._surrogate(key), []).append([key, value])
        self._count += 1
        self._grow_if_needed()
        return value

    def remove(self, key):
        surrogate = self._surrogate(key)
        bucket = self._buckets.get(surrogate)
        if bucket is None:
            return False
        predicate = self._predicate()
        for index, cell in enumerate(bucket):
            if predicate(cell[0], key) is lisptype.T:
                del bucket[index]
                if not bucket:
                    del self._buckets[surrogate]
                self._count -= 1
                return True
        return False

    def clear(self):
        self._buckets.clear()
        self._count = 0

    def count(self):
        return self._count

    def entries(self):
        """A snapshot of the ``(key, value)`` pairs -- the one traversal.

        A *snapshot*, because CLHS 18.2 explicitly permits the body of
        MAPHASH and WITH-HASH-TABLE-ITERATOR to remove entries (and to assign
        to existing ones) while traversing, which `maphash.4`-`.6` and
        `with-hash-table-iterator.10`/`.11` all do. A live view over the
        buckets would raise Python's "dictionary changed size during
        iteration" *as the value of the Lisp form*.
        """
        return [(cell[0], cell[1])
                for bucket in list(self._buckets.values())
                for cell in list(bucket)]

    def _grow_if_needed(self):
        """Keep `size` a capacity that the count has not exceeded.

        `HASH-TABLE-SIZE` is "the number of entries the table can hold without
        rehashing" (CLHS 18.2), so it has to be a capacity that grows -- it
        used to be an alias for `HASH-TABLE-COUNT`, which made
        ``(hash-table-size (make-hash-table :size 100))`` answer 0.
        `hash-table-aux.lsp` asserts on every one of its 1000 iterations that
        the size is a non-negative integer and `>=` the count.

        **The threshold decides *whether* to grow; the count decides *how
        far*.** Letting the threshold set the target does not terminate, and
        the ANSI suite hands us the two arguments that prove it:
        ``:rehash-threshold 0`` (`make-hash-table.16`) makes
        ``size * threshold < count`` unfalsifiable, and
        ``:rehash-threshold least-positive-short-float``
        (`make-hash-table.26`) makes the size it *would* satisfy about 10**45.
        Neither shows up as a failing test -- both tests insert nothing -- so
        this would have been a hang waiting for the first program to store a
        key in such a table.
        """
        if self._count <= self.size * self.rehash_threshold:
            return
        new_size = self.size
        while new_size < self._count:
            if isinstance(self.rehash_size, float):
                grown = int(new_size * self.rehash_size)
            else:
                grown = new_size + int(self.rehash_size)
            # `max` because a float factor applied to a small size can floor
            # back to the same integer, and `:size 0` starts there.
            new_size = max(grown, new_size + 1)
        self.size = new_size

    def __repr__(self):
        return f'<LispHashTable :TEST {self.test} :COUNT {self._count}>'


def is_hash_table(obj):
    """The one hash-table predicate.

    `HASH-TABLE-P`, `TYPEP`'s HASH-TABLE branch, `typespec`'s class cell and
    the printer all ask this, so they cannot disagree about what a hash table
    is. They did: two of them tested ``isinstance(obj, dict)`` and the one
    that won `HASH-TABLE-P` tested a dead class from `hashtables.py`.
    """
    return isinstance(obj, LispHashTable)


def check_hash_table(obj, operator):
    """Signal a TYPE-ERROR unless `obj` is a hash table.

    Every accessor in CLHS 18.2 is specified on a hash table, and
    `check-type-error` asks each of them for a TYPE-ERROR on all 14 members
    of the standard type universe. Returning NIL or 0 for a non-table instead
    -- which is what ``if isinstance(table, dict) else 0`` did -- conflates
    "not a hash table" with a legitimate answer.
    """
    if not is_hash_table(obj):
        raise lisptype.LispTypeError(
            f"{operator}: {obj!r} is not a hash table",
            expected_type=lisptype.LispSymbol('HASH-TABLE'),
            actual_value=obj)
    return obj


# ---------------------------------------------------------------------------
# The operators (CLHS 18.2)
# ---------------------------------------------------------------------------

def _check_rehash_size(value):
    """CLHS 18.2: ``:rehash-size`` is ``(or (integer 1 *) (float (1.0) *))``."""
    if isinstance(value, bool):
        pass
    elif isinstance(value, int) and value >= 1:
        return value
    elif isinstance(value, float) and value > 1.0:
        return value
    raise lisptype.LispTypeError(
        f"MAKE-HASH-TABLE: :REHASH-SIZE must be an integer >= 1 or a float "
        f"> 1.0, not {value!r}",
        expected_type='(OR (INTEGER 1 *) (FLOAT (1.0) *))', actual_value=value)


def _check_rehash_threshold(value):
    """CLHS 18.2: ``:rehash-threshold`` is a ``(real 0 1)``.

    The value is *stored* as given so `HASH-TABLE-REHASH-THRESHOLD` can
    return it, but 0 (and any value small enough to leave no headroom) would
    make the growth loop above spin, so `_grow_if_needed` is written to make
    progress regardless rather than trusting the threshold to be useful.
    """
    import fractions
    if isinstance(value, bool):
        pass
    elif isinstance(value, (int, float, fractions.Fraction)) and 0 <= value <= 1:
        return value
    raise lisptype.LispTypeError(
        f"MAKE-HASH-TABLE: :REHASH-THRESHOLD must be a real in [0,1], "
        f"not {value!r}",
        expected_type='(REAL 0 1)', actual_value=value)


@_registry.cl_function('MAKE-HASH-TABLE')
def make_hash_table(*, test=lisptype.OMITTED, size=16, rehash_size=1.5,
                    rehash_threshold=1.0):
    """CLHS 18.2 MAKE-HASH-TABLE.

    Every parameter is keyword-*only*, which is what makes them ANSI ``&key``
    parameters rather than ``&optional`` ones (CLAUDE.md: "a builtin's ANSI
    lambda list is its Python signature"). Written as defaulted positionals
    they were indistinguishable from ``&optional``, so ``(make-hash-table
    'eq)`` was accepted in place of ``(make-hash-table :test 'eq)``.

    `test` defaults through `lisptype.OMITTED` rather than through ``'EQL'``
    so that an explicitly supplied NIL is a *type error* rather than a silent
    EQL table.
    """
    resolved = resolve_test(test)
    # `:size` is advisory where `:test` is semantic -- CLHS 18.2 calls it "a
    # hint" the implementation may use as it sees fit, so NIL here is taken as
    # "no hint" rather than signalled. `:test` gets the opposite treatment
    # above because it decides which keys are the same key, and a wrong guess
    # is a wrong answer rather than a slower one.
    if _is_nil(size):
        size = 16
    if not isinstance(size, int) or isinstance(size, bool) or size < 0:
        raise lisptype.LispTypeError(
            f"MAKE-HASH-TABLE: :SIZE must be a non-negative integer, "
            f"not {size!r}",
            expected_type='(INTEGER 0 *)', actual_value=size)
    return LispHashTable(resolved, size,
                         _check_rehash_size(rehash_size),
                         _check_rehash_threshold(rehash_threshold))


@_registry.cl_function('HASH-TABLE-P')
def hash_table_p(object):
    """CLHS 18.2 HASH-TABLE-P.

    The one home of this predicate. `core.py` and the dead `hashtables.py`
    both defined it and `hashtables.py` won on import order, testing
    ``isinstance(obj, HashTable)`` -- its own class, which
    `MAKE-HASH-TABLE` never returned -- so it answered NIL for every hash
    table. All 29 tests in `make-hash-table.lsp` open with
    ``(notnot (hash-table-p ht))``.
    """
    return lisptype.lisp_bool(is_hash_table(object))


@_registry.cl_function('GETHASH')
def gethash(key, hash_table, default=lisptype.NIL):
    """CLHS 18.2 GETHASH -- ``(values value present-p)``.

    Two values, not one. `hash-table-aux.lsp` asserts
    ``(equal (multiple-value-list (gethash k table)) '(nil nil))`` for an
    absent key on every one of its 1000 iterations, and `gethash.4` reads the
    second value directly; with a single value returned, `present-p` was NIL
    for a key that was present and holding NIL.
    """
    check_hash_table(hash_table, 'GETHASH')
    value, present = hash_table.lookup(key, default)
    return lisptype.MultipleValues(value, lisptype.lisp_bool(present))


def puthash(key, hash_table, value):
    """The one place an entry is written -- ``(SETF GETHASH)``'s runtime.

    There were four: `evaluation_core`'s SETF ladder, `_fclpy_setf_gethash`,
    `get_setf_expansion`'s GETHASH branch and the getter/setter pair in
    `evaluation_special_forms`, each doing ``table[key] = value`` on the raw
    dict. All four therefore bypassed the table's test even after the test
    was implemented, which is the defect class standing rule 3 describes --
    a fix that silently fails to apply.
    """
    check_hash_table(hash_table, 'SETF GETHASH')
    return hash_table.put(key, value)


@_registry.cl_function('%PUTHASH')
def puthash_operator(key, hash_table, value):
    """`puthash` as an operator, for the SETF expansion to call."""
    return puthash(key, hash_table, value)


@_registry.cl_function('REMHASH')
def remhash(key, hash_table):
    """CLHS 18.2 REMHASH -- T if there was an entry to remove."""
    check_hash_table(hash_table, 'REMHASH')
    return lisptype.lisp_bool(hash_table.remove(key))


@_registry.cl_function('CLRHASH')
def clrhash(hash_table):
    """CLHS 18.2 CLRHASH -- empty the table, return it."""
    check_hash_table(hash_table, 'CLRHASH')
    hash_table.clear()
    return hash_table


@_registry.cl_function('MAPHASH')
def maphash(function, hash_table):
    """CLHS 18.2 MAPHASH -- call `function` on each entry, return NIL.

    Exactly one value: `maphash.1` checks
    ``(multiple-value-list (maphash ...))`` is ``(nil)``.
    """
    check_hash_table(hash_table, 'MAPHASH')
    from .evaluation_core import funcall
    for key, value in hash_table.entries():
        funcall(function, key, value)
    return lisptype.NIL


@_registry.cl_function('HASH-TABLE-COUNT')
def hash_table_count(hash_table):
    """CLHS 18.2 HASH-TABLE-COUNT."""
    check_hash_table(hash_table, 'HASH-TABLE-COUNT')
    return hash_table.count()


@_registry.cl_function('HASH-TABLE-SIZE')
def hash_table_size(hash_table):
    """CLHS 18.2 HASH-TABLE-SIZE -- the capacity, not the count."""
    check_hash_table(hash_table, 'HASH-TABLE-SIZE')
    return hash_table.size


@_registry.cl_function('HASH-TABLE-TEST')
def hash_table_test(hash_table):
    """CLHS 18.2 HASH-TABLE-TEST -- the test's *symbol*.

    Interned in COMMON-LISP, because the caller compares it with `EQ`
    (`hash-table-test.2`) and global symbol identity is by symbol object, so
    a bare ``LispSymbol('EQL')`` would be a different symbol from the ``EQL``
    the test form names.
    """
    check_hash_table(hash_table, 'HASH-TABLE-TEST')
    return lisptype.COMMON_LISP_PACKAGE.intern_symbol(hash_table.test)


@_registry.cl_function('HASH-TABLE-REHASH-SIZE')
def hash_table_rehash_size(hash_table):
    """CLHS 18.2 HASH-TABLE-REHASH-SIZE."""
    check_hash_table(hash_table, 'HASH-TABLE-REHASH-SIZE')
    return hash_table.rehash_size


@_registry.cl_function('HASH-TABLE-REHASH-THRESHOLD')
def hash_table_rehash_threshold(hash_table):
    """CLHS 18.2 HASH-TABLE-REHASH-THRESHOLD."""
    check_hash_table(hash_table, 'HASH-TABLE-REHASH-THRESHOLD')
    return hash_table.rehash_threshold


@_registry.cl_function('SXHASH')
def sxhash(object):
    """CLHS 18.2 SXHASH -- a hash consistent with EQUAL.

    The same surrogate an EQUAL table buckets by, so the standard's
    "(equal x y) implies (= (sxhash x) (sxhash y))" holds because the two are
    the same function rather than because they were separately made to agree.

    It used to be ``hash(obj)`` with a ``hash(str(obj))`` fallback, which
    failed the contract for every aggregate EQUAL descends into --
    ``(sxhash (list 1 2))`` differed between two EQUAL lists -- and could
    answer a *negative* number, where CLHS requires an
    ``(and unsigned-byte fixnum)``.
    """
    return hash(sxhash_key(object)) & _HASH_MASK


# ---------------------------------------------------------------------------
# WITH-HASH-TABLE-ITERATOR (CLHS 18.2)
# ---------------------------------------------------------------------------

class _HashTableIterator:
    """The state behind one WITH-HASH-TABLE-ITERATOR expansion."""

    __slots__ = ('_entries', '_index')

    def __init__(self, table):
        self._entries = table.entries()
        self._index = 0

    def next(self):
        if self._index >= len(self._entries):
            return lisptype.MultipleValues(lisptype.NIL)
        key, value = self._entries[self._index]
        self._index += 1
        return lisptype.MultipleValues(lisptype.T, key, value)


@_registry.cl_function('%MAKE-HASH-TABLE-ITERATOR')
def make_hash_table_iterator(hash_table):
    """The iterator WITH-HASH-TABLE-ITERATOR binds.

    ``%``-prefixed because it is the macro's runtime rather than an ANSI
    operator, following `%MAKE-FILL-POINTER-OUTPUT-STREAM`.
    """
    check_hash_table(hash_table, 'WITH-HASH-TABLE-ITERATOR')
    return _HashTableIterator(hash_table)


@_registry.cl_function('%HASH-TABLE-ITERATOR-NEXT')
def hash_table_iterator_next(iterator):
    """One step of the iterator: ``(values more-p key value)``."""
    if not isinstance(iterator, _HashTableIterator):
        raise lisptype.LispTypeError(
            f"WITH-HASH-TABLE-ITERATOR: {iterator!r} is not an iterator",
            expected_type='HASH-TABLE-ITERATOR', actual_value=iterator)
    return iterator.next()


# ---------------------------------------------------------------------------
# Not a hash table operator, and it does not belong here
# ---------------------------------------------------------------------------
#
# UPGRADED-COMPLEX-PART-TYPE (CLHS 12.2) is a *type* operator and its home is
# `typespec.py`, which already documents it. It is left registered here only
# because this file is the sole place that registers it, and silently dropping
# an operator while rewriting the module beside it would be an unrelated
# regression. Moving it is a separate change.


@_registry.cl_function('UPGRADED-COMPLEX-PART-TYPE')
def upgraded_complex_part_type(typespec, environment=None):
    """Get upgraded complex part type."""
    return 'REAL'


__all__ = [
    'LispHashTable',
    'is_hash_table',
    'check_hash_table',
    'resolve_test',
    'sxhash_key',
    'puthash',
    'make_hash_table',
    'hash_table_p',
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
