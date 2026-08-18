"""The one sequence protocol: how a Lisp sequence is read, and how one is built.

Every CLHS sequence function has the same two halves. It *reads* elements out
of a sequence argument, and it *builds* a result sequence -- either "of the
same type as the argument" (CLHS 17.1, e.g. REMOVE, SORT, REVERSE) or "of the
type this `result-type` argument designates" (CLHS 15.1.2.2 / 17.1, e.g. MAP,
CONCATENATE, MERGE, MAKE-SEQUENCE). This module owns both halves so that the
~60 sequence operators do not each own a partial copy.

**Why it exists.** Before it, each of `sequences_search`, `sequences_modify`,
`sequences_compose` and `sequences_higher` open-coded both halves with Python
type tests, which is plan.md Finding M ("a Python type test standing in for a
Lisp type test") in its most expensive form:

- *Reading.* `iterate()` gated on `isinstance(sequence, (list, str, tuple))`
  and raised a Python `TypeError` for everything else -- so FIND/POSITION/
  COUNT/MISMATCH on a `LispString` (every `"..."` literal) or an
  `AdjustableVector` (every `#(...)` literal) leaked a Python exception as a
  Lisp value (standing rule 2). `sequences_higher`'s `_cons_to_list` had the
  opposite failure: an unrecognized type fell through to `[seq]`, so REDUCE
  over a vector silently "reduced" a one-element list back to the vector
  itself, and EVERY over a vector answered NIL.
- *Building.* Results were returned as Python `list`s. A Python list is this
  implementation's **vector**, so `(union '(1 2) '(2 3))` returned `#(1 2 3)`,
  `(sort (list 3 1 2) #'<)` returned `#(1 2 3)`, and `(listp (union ...))` was
  NIL. `result-type` was consulted, when at all, by comparing an upper-cased
  string against `'LIST'`, so `(concatenate 'list "ab" #(1 2))` produced
  `#("ab" #(1 2))` -- it did not even iterate its arguments.

The two halves are one mechanism because they are the same question asked in
both directions: *what are the elements of this Lisp sequence, and what Lisp
sequence do these elements belong in?*

Representation note: a **vector** is a Python `list` here, a **list** is a
`lispCons` chain terminated by NIL, and a **string** is a `lisptype.LispString`.
Building a vector therefore returns a plain `list`, which is why `LIST` and
`VECTOR` results must never be conflated -- they are distinct Lisp types that
happen to share a Python spelling for one of them. A vector that has an
element type, a fill pointer, adjustability or displacement to record is an
`arrays.LispArray`; `arrays.py` owns that model and this module asks it rather
than testing for the class, so the two cannot disagree about what a vector is.
"""

import fclpy.lisptype as lisptype
from . import arrays as _arrays
from .arrays import LispArray, string_element
from .core import _check_list as check_list, _consp_internal, _null_internal


# ===== READING =====


def list_cells(value, what='list function', dotted='error'):
    """Yield each cons cell of a Lisp list, one step at a time.

    **The one list-traversal path**, and the reason it yields *cells* rather
    than elements is that the three things a CLHS list operator asks for are
    all derivable from a cell and none of them is derivable from the other
    two: MEMBER/ASSOC need the tail itself (it must be EQ to a sublist of the
    argument), LAST/BUTLAST need to *count* conses, and everything else needs
    the cars.

    Traversal is lazy, and that is a semantic requirement rather than an
    optimization: CLHS defines these operators on a dotted list up to the
    point where they would have to walk past its tail, so the type check has
    to happen at the step, not up front. `(nthcdr 1 (cons 'a 'b))` is `B`
    while `(nthcdr 3 (cons 'a 'b))` is a TYPE-ERROR (`nthcdr.5` vs
    `nthcdr.error.10`), and `(member 'a '(a . b))` finds A and returns before
    the dotted tail is ever reached.

    `dotted` selects the policy for a non-NIL, non-cons tail:

    * ``'error'`` -- a *proper* list is required, so the tail is a TYPE-ERROR.
      This is what CLHS 14.2's LIST arguments (MEMBER, the set operations, the
      MAP* family, PAIRLIS, LIST-LENGTH) and CLHS 17.1's "proper sequence"
      require, and it is what these operators previously did *not* do: the
      dotted tail was read as one more **element**, so `(append '(a . b) '(z))`
      answered `(A B Z)` -- a wrong value, not merely a missing error -- and
      `(pairlis '(a . b) '(c . d))` paired B with D.
    * ``'allow'`` -- stop at the tail. LAST, BUTLAST/NBUTLAST, NTHCDR, LDIFF
      and TAILP are *defined* on dotted lists (`(last '(a b . c))` is
      `(B . C)`), and for them the tail is still not an element.

    A `value` that is not a list at all is a TYPE-ERROR either way, and that
    one is raised *eagerly* rather than on the first `next()`: ansi-test's
    `check-type-error` asserts a TYPE-ERROR for every object failing `listp`,
    and an operator that gives up before consuming its walker (MAPLIST once
    another argument runs out, MEMBER on the empty list) would otherwise never
    reach it.
    """
    check_list(value, what)
    return _walk_cells(value, what, dotted)


def _walk_cells(value, what, dotted):
    if _null_internal(value):
        return
    current = value
    while True:
        yield current
        rest = current.cdr
        if _null_internal(rest):
            return
        if not _consp_internal(rest):
            if dotted == 'allow':
                return
            raise lisptype.LispTypeError(
                f"{what}: {value!r} is not a proper list",
                expected_type="proper LIST", actual_value=value)
        current = rest


def list_elements(value, what='list function', dotted='error'):
    """The elements of a Lisp *list* argument (CLHS 14.2).

    `seq_elements`' counterpart for the operators whose argument must be a
    list rather than any sequence -- a vector handed to MEMBER or UNION is a
    TYPE-ERROR, which is the half `seq_elements` cannot express because a
    vector is a perfectly good sequence.
    """
    return [cell.car for cell in list_cells(value, what, dotted)]


def list_tail(value, what='list function'):
    """The atom a dotted list ends in, or NIL for a proper list.

    Paired with ``list_cells(..., dotted='allow')``: an operator that copies a
    dotted list (COPY-LIST, LDIFF's non-tail result) needs both the cells and
    the terminator, and deriving the terminator by re-walking the chain is how
    the two answers drift apart.
    """
    check_list(value, what)
    if _null_internal(value):
        return lisptype.NIL
    current = value
    while _consp_internal(current.cdr):
        current = current.cdr
    rest = current.cdr
    return lisptype.NIL if _null_internal(rest) else rest


def is_sequence(value):
    """True if `value` is a Lisp sequence (a list or a vector), per CLHS 14.1.

    `TYPEP`'s `SEQUENCE` branch answers with this same predicate, and it has
    to: `seq_elements` accepting an object that `(typep x 'sequence)` denies
    (or the reverse) is directly observable -- ansi-test's `check-type-error`
    asserts that a sequence function signals a TYPE-ERROR for exactly the
    objects that fail `sequencep`.
    """
    if value is None or value is lisptype.NIL:
        return True
    if isinstance(value, lisptype.lispCons):
        return True
    return is_vector(value)


def is_vector(value):
    """True if `value` is a Lisp vector -- a one-dimensional array.

    A vector has several representations here (a Python `list`, a
    `LispString`, and the `LispArray` that carries an element type or a fill
    pointer), and code that tests only `isinstance(x, list)` silently answers
    "not a vector" for most of them. EQUALP did exactly that, so a vector
    built by one path was never EQUALP to the same vector built by another.
    Deferred to the array model so that "is this a vector" has one answer.
    """
    return _arrays.is_vector(value)


def seq_elements(sequence, what='sequence function'):
    """Return the elements of any Lisp sequence as a Python list.

    This is the *only* element-access path for a CLHS 17 `sequence` argument.
    It accepts every representation a Lisp sequence has here -- `lispCons`
    chains, NIL, Python `list`/`tuple` vectors, `LispArray` (respecting its
    fill pointer, via its own `__iter__`), `str` and `LispString`.

    A dotted list is **not a proper sequence** (CLHS 17.1) and signals a
    TYPE-ERROR. It used to have its final atom appended as one more *element*,
    "so callers can detect it" -- which no caller did, so every sequence and
    list operator silently answered with the tail folded in as data:
    `(append '(a . b) '(z))` was `(A B Z)`. The operators that are genuinely
    defined on a dotted list (LAST, BUTLAST, NTHCDR, LDIFF, TAILP) ask
    `list_cells(..., dotted='allow')` instead, which stops at the tail rather
    than consuming it.

    A non-sequence raises a Lisp `LispTypeError` rather than a Python
    `TypeError`: the old `iterate()` raised the latter and it surfaced as the
    value of the form (standing rule 2).
    """
    if sequence is None or sequence is lisptype.NIL:
        return []
    if isinstance(sequence, lisptype.lispCons):
        return list_elements(sequence, what)
    if isinstance(sequence, (list, tuple)):
        return list(sequence)
    if isinstance(sequence, (str, lisptype.LispString)):
        # Elements of a string are CHARACTERs (CLHS 15.1); both string
        # representations index to a bare length-1 Python `str`, so every
        # reader here must apply the same conversion `AREF`/`LOOP across` do
        # (`arrays.string_element`) or FIND/POSITION/REDUCE/... hand back a
        # raw `str` that the printer then shows as a one-character *string*.
        return [string_element(sequence, c) for c in sequence]
    if isinstance(sequence, LispArray):
        # Only a *vector* is a sequence: an array of any other rank is an
        # array but not a sequence (CLHS 14.1), so it must be refused here
        # rather than flattened into its row-major elements.
        if sequence.rank != 1:
            raise lisptype.LispTypeError(
                f"{what}: an array of rank {sequence.rank} is not a sequence",
                expected_type="SEQUENCE", actual_value=sequence)
        # `LispArray.__iter__` already yields through `row_major_get`, which
        # applies `_present_element` -- a character-vector array hands back
        # `Character` objects here without help.
        return list(sequence)
    raise lisptype.LispTypeError(
        f"{what}: {type(sequence).__name__} is not a sequence",
        expected_type="SEQUENCE", actual_value=sequence)


def seq_length(sequence, what='sequence function'):
    """Length of any Lisp sequence, respecting a vector's fill pointer."""
    if sequence is None or sequence is lisptype.NIL:
        return 0
    if isinstance(sequence, (list, tuple, str, lisptype.LispString)):
        return len(sequence)
    if isinstance(sequence, LispArray) and sequence.rank == 1:
        return len(sequence)
    if isinstance(sequence, lisptype.lispCons):
        return len(seq_elements(sequence, what))
    raise lisptype.LispTypeError(
        f"{what}: {type(sequence).__name__} is not a sequence",
        expected_type="SEQUENCE", actual_value=sequence)


def bounding_indices(length, start=0, end=None, what='sequence function'):
    """Normalize a CLHS `:start`/`:end` pair against a sequence of `length`.

    `:end` defaults to the length and accepts NIL as "the end" (CLHS 17.1);
    both arrive as Lisp values, so NIL/None must be distinguished from 0 here
    rather than in each of the twenty callers.
    """
    start = 0 if start is None or start is lisptype.NIL else int(start)
    end = length if end is None or end is lisptype.NIL else int(end)
    if start < 0 or end > length or start > end:
        raise lisptype.LispTypeError(
            f"{what}: bad bounding indices {start}:{end} for length {length}",
            expected_type=f"index in [0,{length}]", actual_value=(start, end))
    return start, end


# ===== BUILDING =====


def _char_text(item):
    """Return a 1-character Python `str` for a Lisp character-like element.

    Elements of a string are a `lisptype.Character` on some paths and an
    already-plain length-1 `str` on others (plan.md Finding I, the
    `LispString`/`str` split that M9 owns); rebuilding a string needs plain
    text either way.
    """
    if isinstance(item, lisptype.Character):
        return item.char
    if isinstance(item, str):
        return item
    raise lisptype.LispTypeError(
        f"cannot store {item!r} in a string", expected_type="CHARACTER",
        actual_value=item)


def make_lisp_list(elements):
    """Build a proper Lisp list (`lispCons` chain, NIL when empty).

    An empty result must be NIL, not `()`/`[]`: NIL is the empty list, and a
    Python `list` is a *vector* here, so returning one is a type error that
    prints convincingly.
    """
    result = lisptype.NIL
    for item in reversed(list(elements)):
        result = lisptype.lispCons(item, result)
    return result


def make_string(elements):
    """Build a Lisp string from character elements."""
    return lisptype.LispString(''.join(_char_text(e) for e in elements))


def make_vector(elements):
    """Build a Lisp vector (a Python `list` in this implementation)."""
    return list(elements)


def make_bit_vector(elements):
    """Build a Lisp bit vector.

    A bit vector is not "a vector holding zeroes and ones" -- it is a vector
    whose *element type* is BIT, which is what makes it print as `#*1011` and
    answer T to `BIT-VECTOR-P`. Building one as a general vector is why
    `(concatenate 'bit-vector ...)` used to answer something that was EQUALP
    to a bit vector but was not one.
    """
    return _arrays.make_bit_vector(elements)


def rebuild_like(original, elements):
    """Build a result of the *same sequence type* as `original` (CLHS 17.1).

    Used by every generic sequence function that has no `result-type`
    argument -- REMOVE, SUBSTITUTE, SORT, REVERSE, SUBSEQ, COPY-SEQ and their
    destructive counterparts. Returning the Python `elements` list verbatim,
    which is what these functions used to do, turns a LIST argument into a
    vector and a STRING argument into a vector of one-character pieces.
    """
    if original is None or original is lisptype.NIL or isinstance(original, lisptype.lispCons):
        return make_lisp_list(elements)
    if isinstance(original, (lisptype.LispString, str)):
        return make_string(elements)
    if isinstance(original, tuple):
        return tuple(elements)
    if isinstance(original, LispArray):
        # "Of the same type" means the same *element type* (CLHS 17.1): the
        # result of REMOVE on a bit vector is a bit vector. It is a simple
        # array either way -- these operators never propagate a fill pointer,
        # adjustability or displacement.
        if original.element_type is _arrays.BIT_TYPE:
            return make_bit_vector(elements)
        if original.element_type is _arrays.CHARACTER_TYPE:
            return make_string(elements)
    return make_vector(elements)


# Sequence type designators, normalized to the kind of sequence they name.
# CLHS 4.2.3/15.1.2.2: LIST and CONS name lists; VECTOR and its subtypes name
# vectors; STRING/BIT-VECTOR are vectors with a constrained element type.
_LIST_TYPES = frozenset(('LIST', 'CONS', 'NULL'))
_STRING_TYPES = frozenset(('STRING', 'SIMPLE-STRING', 'BASE-STRING', 'SIMPLE-BASE-STRING'))
_BIT_VECTOR_TYPES = frozenset(('BIT-VECTOR', 'SIMPLE-BIT-VECTOR'))
_VECTOR_TYPES = frozenset((
    'VECTOR', 'SIMPLE-VECTOR', 'ARRAY', 'SIMPLE-ARRAY', 'SEQUENCE',
)) | _STRING_TYPES | _BIT_VECTOR_TYPES
_CHARACTER_ELEMENT_TYPES = frozenset(('CHARACTER', 'BASE-CHAR', 'STANDARD-CHAR', 'EXTENDED-CHAR'))


def _type_name(designator):
    """The name of a type designator that is a symbol, string, or Python type."""
    if isinstance(designator, lisptype.LispSymbol):
        return designator.name.upper()
    if isinstance(designator, lisptype.LispString):
        return str(designator).upper()
    if isinstance(designator, str):
        return designator.upper()
    if designator is list:
        return 'LIST'
    if designator is str:
        return 'STRING'
    if designator is tuple:
        return 'VECTOR'
    # CLHS 4.2.3: a *class object* is a type specifier, and MAKE-SEQUENCE is
    # routinely handed one (`(make-sequence (class-of v) 1)`). Its name is the
    # type it denotes.
    name = getattr(designator, 'name', None)
    return name.upper() if isinstance(name, str) else None


def parse_sequence_type(result_type, what='sequence function'):
    """Resolve a CLHS sequence type specifier to `(kind, size, element_type)`.

    `kind` is one of `'NIL'`, `'LIST'`, `'STRING'`, `'BIT-VECTOR'`, `'VECTOR'`.
    `size` is the length the specifier constrains the result to, or None.
    `element_type` is the specifier's element-type name, or None.

    Compound specifiers are the reason this is a parser rather than a lookup:
    `(vector t 5)`, `(string 5)`, `(simple-array character (5))` and
    `(vector character)` are all legal `result-type` arguments, and the last
    one names a *string* -- the element type, not the head symbol, decides.
    """
    if result_type is None or result_type is lisptype.NIL:
        return ('NIL', None, None)

    elements = None
    if isinstance(result_type, (lisptype.lispCons, list, tuple)):
        elements = seq_elements(result_type, what)
        if not elements:
            raise lisptype.LispTypeError(
                f"{what}: {result_type!r} is not a sequence type specifier",
                expected_type="sequence type specifier", actual_value=result_type)
        head, rest = elements[0], elements[1:]
    else:
        head, rest = result_type, []

    name = _type_name(head)
    if name is None:
        raise lisptype.LispTypeError(
            f"{what}: {result_type!r} is not a sequence type specifier",
            expected_type="sequence type specifier", actual_value=result_type)
    if name == 'NIL':
        return ('NIL', None, None)

    element_type = None
    size = None
    if rest:
        # (vector element-type size) / (string size) / (array et dimensions)
        if name in _STRING_TYPES or name in _BIT_VECTOR_TYPES:
            size = rest[0]
        else:
            element_type = _type_name(rest[0])
            if len(rest) > 1:
                size = rest[1]
        if isinstance(size, (lisptype.lispCons, list, tuple)):
            # (simple-array character (5)) -- dimensions are a list
            dims = seq_elements(size, what)
            size = dims[0] if len(dims) == 1 else None
    if size is lisptype.NIL or _type_name(size) == '*':
        size = None
    if size is not None:
        size = int(size)

    if name in _LIST_TYPES:
        return ('LIST', size, element_type)
    if name in _STRING_TYPES or element_type in _CHARACTER_ELEMENT_TYPES:
        return ('STRING', size, element_type)
    if name in _BIT_VECTOR_TYPES or element_type == 'BIT':
        return ('BIT-VECTOR', size, element_type)
    if name in _VECTOR_TYPES:
        return ('VECTOR', size, element_type)
    raise lisptype.LispTypeError(
        f"{what}: {result_type!r} does not name a sequence type",
        expected_type="LIST, VECTOR, STRING, or BIT-VECTOR", actual_value=result_type)


def build_sequence(result_type, elements, what='sequence function'):
    """Build a sequence of the type `result_type` designates (CLHS 15.1.2.2).

    The `result-type` half of the protocol, shared by MAP, CONCATENATE, MERGE,
    MAKE-SEQUENCE and COERCE. A NIL designator names the *type* NIL, which
    only the empty sequence inhabits, so a non-empty result under it is an
    error. MAP's `(map nil ...)` is a different rule -- "call for effect" --
    and MAP applies that itself before reaching here.

    A specifier that constrains the length (`(vector t 5)`) is checked, per
    CLHS: a result of the wrong length is an error, not a silent truncation.
    """
    kind, size, _element_type = parse_sequence_type(result_type, what)
    elements = list(elements)
    if size is not None and size != len(elements):
        raise lisptype.LispTypeError(
            f"{what}: result of length {len(elements)} does not match "
            f"the length {size} required by {result_type!r}",
            expected_type=f"sequence of length {size}", actual_value=len(elements))
    if kind == 'NIL':
        if elements:
            raise lisptype.LispTypeError(
                f"{what}: only the empty sequence is of type NIL",
                expected_type="NIL", actual_value=len(elements))
        return lisptype.NIL
    if kind == 'LIST':
        return make_lisp_list(elements)
    if kind == 'STRING':
        return make_string(elements)
    if kind == 'BIT-VECTOR':
        return make_bit_vector(elements)
    return make_vector(elements)


# ===== MUTATION =====


def seq_set(sequence, index, value, what='sequence function'):
    """Store `value` at `index` in a mutable Lisp sequence.

    The destructive operators (FILL, REPLACE, MAP-INTO, the `N`-prefixed
    modifiers) need to write *through* the argument they were given rather
    than rebuild it, and a `lispCons` chain is written by assigning a cell's
    `car` -- which is why this cannot be `sequence[index] = value`.
    """
    if isinstance(sequence, lisptype.lispCons):
        current = sequence
        for _ in range(index):
            current = current.cdr
            if not isinstance(current, lisptype.lispCons):
                raise lisptype.LispTypeError(
                    f"{what}: index {index} is past the end of the list",
                    expected_type="index within the sequence", actual_value=index)
        current.car = value
        return
    if isinstance(sequence, (lisptype.LispString, list, LispArray)):
        sequence[index] = value
        return
    raise lisptype.LispTypeError(
        f"{what}: {type(sequence).__name__} is not a mutable sequence",
        expected_type="mutable SEQUENCE", actual_value=sequence)


__all__ = [
    'is_sequence', 'is_vector', 'check_list', 'list_cells', 'list_elements', 'list_tail',
    'seq_elements', 'seq_length', 'bounding_indices',
    'make_lisp_list', 'make_string', 'make_vector', 'make_bit_vector', 'rebuild_like',
    'parse_sequence_type', 'build_sequence', 'seq_set',
]
