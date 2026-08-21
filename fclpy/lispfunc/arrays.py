"""The one array object model: what an array *is*, and what it knows about itself.

CLHS 15.1 says an array has five properties -- its dimensions, its element
type, whether it is adjustable, whether it has a fill pointer, and whether it
is displaced to another array. Every array operator is a question about one of
those five, so they belong to one object model rather than to the operators.

**Why this module exists.** Before it, the model was three unrelated Python
shapes spread over five modules:

- `vectors.AdjustableVector` -- a 1-D vector with a fill pointer, which is also
  what the reader built for a `#(...)` literal, so every *simple* vector claimed
  to be adjustable.
- `vectors.Array` -- a separate multi-dimensional class that could not have a
  fill pointer, could not be adjustable, and was not `ARRAYP`.
- a bare Python `list` -- everything else.

None of the three recorded an element type, so `MAKE-ARRAY` discarded
`:element-type` outright: `(make-array 5 :element-type 'bit)` answered a vector
of NIL, a bit vector could not be told from a general vector (which is why one
printed as `#(1 0 1 1)` instead of `#*1011`), and `ARRAY-ELEMENT-TYPE` returned
the Python string `'T'` -- a Python object as a Lisp value, standing rule 2.
`:displaced-to` was accepted and ignored.

Worse, the *operators* were duplicated across `vectors.py`,
`sequences_higher.py`, `misc_hashtables.py`, `math_arithmetic.py` and `core.py`,
so which implementation ran was decided by module import order (standing rule
3). `vectors.py`'s fill-pointer-aware `AREF` and `VECTOR-PUSH` lost to
`sequences_higher.py`'s `array[i]` and `vector.append(...)` -- and an
`AdjustableVector` has no `.append`, so `VECTOR-PUSH` leaked an
`AttributeError` as the value of the form.

**Three representations, one protocol.** As with `sequence_protocol.py`, the
answer is not one Python class for every array but one place that answers the
five questions for each representation:

===========================  ==============================================
Python `list`                a *simple* general vector: rank 1, element type
                             T, no fill pointer, not adjustable, not displaced
`lisptype.LispString`        a character vector (CLHS 15.1: a string *is* an
                             array), fill pointer and adjustability included
`LispArray`                  everything else -- any other rank (0 or >= 2),
                             any specialized element type, any fill pointer,
                             adjustability or displacement
===========================  ==============================================

The rule for which one `MAKE-ARRAY` returns is `_new_array` below, and it is
the only place that decides. The point of the rule is that the *simple general
vector*, by far the most common array, needs to record nothing beyond its
elements -- so it stays the Python `list` that the sequence protocol, the
printer and the evaluator already understand -- while every array that has
something to remember is an object that remembers it.

Anything asking an array about itself must go through the protocol functions
here (`array_rank_of`, `array_dimensions_of`, `element_type_of`,
`row_major_get`, ...) rather than through an `isinstance` test, or it will be
right about one representation and wrong about the other two -- plan.md
Finding M.
"""

import fclpy.lisptype as lisptype
from . import registry as _registry


# ===== CANONICAL TYPE SYMBOLS =====

def _cl_symbol(name):
    """The canonical `COMMON-LISP` symbol of that name.

    `ARRAY-ELEMENT-TYPE` must return the *interned* symbol, not a fresh
    `LispSymbol` with the same name: RT compares results with EQL, so a
    freshly built `BIT` is not the `BIT` the test wrote.
    """
    return lisptype.COMMON_LISP_PACKAGE.intern(name)


T_TYPE = lisptype.T
BIT_TYPE = _cl_symbol('BIT')
CHARACTER_TYPE = _cl_symbol('CHARACTER')

# The upgraded element type of a `NIL`-element-type array is NIL itself, not
# T -- CLHS 15.1.2.1's monotonicity requirement forces this: NIL is a
# subtype of both BIT and CHARACTER, so UAET(NIL) must be a subtype of both
# UAET(BIT)=BIT and UAET(CHARACTER)=CHARACTER, and the only type that is a
# subtype of two disjoint types is NIL (`upgraded-array-element-type.nil.1`,
# CLHS's own worked example). NIL is also the one Lisp object that denotes
# the empty type as a type specifier, so it doubles as that marker here.
NIL_TYPE = lisptype.NIL

_CHARACTER_TYPE_NAMES = frozenset((
    'CHARACTER', 'BASE-CHAR', 'STANDARD-CHAR', 'EXTENDED-CHAR'))


def _type_name(designator):
    """The head name of a type designator, or None if it does not name one."""
    if isinstance(designator, lisptype.LispSymbol):
        return designator.name.upper()
    if isinstance(designator, lisptype.LispString):
        return str(designator).upper()
    if isinstance(designator, str):
        return designator.upper()
    if isinstance(designator, lisptype.lispCons):
        return _type_name(designator.car)
    if isinstance(designator, (list, tuple)) and designator:
        return _type_name(designator[0])
    name = getattr(designator, 'name', None)
    if isinstance(name, lisptype.LispSymbol):
        return name.name.upper()
    return name.upper() if isinstance(name, str) else None


def _type_args(designator):
    """The arguments of a compound type specifier, as a Python list."""
    if isinstance(designator, lisptype.lispCons):
        args = []
        current = designator.cdr
        while isinstance(current, lisptype.lispCons):
            args.append(current.car)
            current = current.cdr
        return args
    if isinstance(designator, (list, tuple)):
        return list(designator[1:])
    return []


def upgraded_element_type(spec):
    """The element type an array of `spec` actually holds (CLHS 15.1.2.1).

    fclpy specializes on exactly the three element types the standard forces
    a distinct representation for -- `NIL`, `BIT` and `CHARACTER` -- and
    upgrades everything else to `T`, which CLHS 15.1.2.1 explicitly permits
    ("the upgraded array element type ... is a supertype of the expressed
    type"). `BIT`/`CHARACTER` have to be recorded rather than inferred
    because a bit vector and a general vector holding zeroes and ones are
    different types that print differently and answer `BIT-VECTOR-P`
    differently; `NIL` has to be recorded because upgrading it to `T` fails
    the monotonicity check `upgraded-array-element-type.nil.1` runs (see
    `NIL_TYPE`).

    `spec` denoting NIL is checked before `_type_name(spec)`, because NIL
    (however it arrives -- the singleton, Python `None`, or a `LispSymbol`
    named "NIL") has no `.name` attribute for `_type_name` to read and would
    otherwise fall through to the `name is None` "not a type name at all"
    branch, silently becoming `T` like an unrecognized specifier would.
    """
    if spec is None or spec is lisptype.NIL:
        return NIL_TYPE
    name = _type_name(spec)
    if name is None:
        return T_TYPE
    if name == 'NIL':
        return NIL_TYPE
    if name == 'BIT':
        return BIT_TYPE
    if name in _CHARACTER_TYPE_NAMES:
        return CHARACTER_TYPE
    if name == 'UNSIGNED-BYTE':
        # (unsigned-byte 1) is type-equivalent to BIT (CLHS 12.1.1).
        args = _type_args(spec)
        if len(args) == 1 and args[0] == 1:
            return BIT_TYPE
    return T_TYPE


def default_element(element_type):
    """The element a freshly made array of this type is filled with.

    CLHS leaves the contents of an array made without `:initial-element`
    unspecified, but not *arbitrary*: every element must be of the array's
    element type, so a bit array cannot be filled with NIL.
    """
    if element_type is BIT_TYPE:
        return 0
    if element_type is CHARACTER_TYPE:
        return lisptype.Character(' ')
    return lisptype.NIL


def coerce_element(value, element_type, what='array'):
    """Check `value` against `element_type`, normalizing its representation.

    A bit array holds Python ints and a character array holds plain length-1
    `str` (`LispString`'s storage), so the conversion between a Lisp
    `Character` and its text belongs here -- one place -- rather than in each
    of the operators that store into an array.
    """
    if element_type is BIT_TYPE:
        if value is True:
            value = 1
        if isinstance(value, int) and not isinstance(value, bool) and value in (0, 1):
            return int(value)
        raise lisptype.LispTypeError(
            f"{what}: {value!r} is not of element type BIT",
            expected_type="BIT", actual_value=value)
    if element_type is CHARACTER_TYPE:
        if isinstance(value, lisptype.Character):
            return value.char
        if isinstance(value, str) and len(value) == 1:
            return value
        raise lisptype.LispTypeError(
            f"{what}: {value!r} is not of element type CHARACTER",
            expected_type="CHARACTER", actual_value=value)
    return value


def _present_element(value, element_type):
    """The Lisp value of a stored element (the inverse of `coerce_element`)."""
    if element_type is CHARACTER_TYPE and isinstance(value, str):
        return lisptype.Character(value)
    return value


def string_element(container, element):
    """Normalize an element read out of `container` to a Lisp value.

    The elements of a string are CHARACTERs (CLHS 15.1), but both string
    representations index to a bare length-1 Python `str`. Anything that
    walks a string element-wise -- AREF, LOOP's `across`, sequence traversal
    -- has to apply that conversion, and it must be the *same* conversion
    everywhere or the two halves disagree about what a character is.
    Non-string containers are left alone.

    That conflation is not merely untidy: since a string is also a vector,
    any consumer that walks a vector element-wise sees each character as
    another one-element vector and recurses without end. The ANSI harness's
    own `equalp-with-case` does exactly that, so comparing any two strings
    ran until the stack was exhausted and aborted the whole run.
    """
    if isinstance(element, str) and isinstance(container, (str, lisptype.LispString)):
        return lisptype.Character(element)
    return element


# ===== THE ARRAY OBJECT =====


class LispArray:
    """An array that has something to remember beyond its elements.

    Storage is a flat Python list in row-major order (CLHS 15.1.1's
    row-major ordering is also what `ROW-MAJOR-AREF` and
    `ARRAY-ROW-MAJOR-INDEX` expose), or, for a displaced array, no storage at
    all: every access is forwarded to the target array at a fixed offset, so
    that writes through either one are visible through the other -- which is
    the whole point of displacement, and what an eager copy would silently
    get wrong.
    """

    def __init__(self, dimensions, element_type=None, data=None, fill_pointer=None,
                 adjustable=False, displaced_to=None, displaced_index_offset=0):
        self.dimensions = tuple(int(d) for d in dimensions)
        self.element_type = element_type if element_type is not None else T_TYPE
        self.adjustable = bool(adjustable)
        self.displaced_to = displaced_to
        self.displaced_index_offset = int(displaced_index_offset or 0)
        if displaced_to is None:
            if data is None:
                self._data = [default_element(self.element_type)] * self.total_size
            else:
                self._data = list(data)
        else:
            self._data = None
        self.fill_pointer = fill_pointer

    # --- the five properties ---

    @property
    def rank(self):
        return len(self.dimensions)

    @property
    def total_size(self):
        size = 1
        for d in self.dimensions:
            size *= d
        return size

    @property
    def is_simple(self):
        """CLHS 15.1.1: simple means not displaced, no fill pointer, not adjustable."""
        return (not self.adjustable and self.fill_pointer is None
                and self.displaced_to is None)

    # --- element access, in row-major order ---

    def row_major_get(self, index):
        if index < 0 or index >= self.total_size:
            raise lisptype.LispTypeError(
                f"row-major index {index} is out of bounds for an array of "
                f"total size {self.total_size}",
                expected_type=f"index below {self.total_size}", actual_value=index)
        if self.displaced_to is not None:
            return row_major_get(self.displaced_to, index + self.displaced_index_offset)
        return _present_element(self._data[index], self.element_type)

    def row_major_set(self, index, value):
        if index < 0 or index >= self.total_size:
            raise lisptype.LispTypeError(
                f"row-major index {index} is out of bounds for an array of "
                f"total size {self.total_size}",
                expected_type=f"index below {self.total_size}", actual_value=index)
        if self.displaced_to is not None:
            row_major_set(self.displaced_to, index + self.displaced_index_offset, value)
            return value
        self._data[index] = coerce_element(value, self.element_type)
        return value

    def row_major_index(self, subscripts):
        return row_major_index(self, subscripts)

    # --- Python sequence protocol, for rank-1 arrays ---
    #
    # A vector's *length* is its fill pointer when it has one (CLHS 17.1), so
    # the generic sequence machinery sees exactly the active elements. AREF
    # deliberately does not go through here: it reaches every element of the
    # underlying storage, fill pointer or not.

    def __len__(self):
        if self.fill_pointer is not None:
            return self.fill_pointer
        return self.total_size

    def __getitem__(self, index):
        if isinstance(index, slice):
            return [self.row_major_get(i) for i in range(*index.indices(len(self)))]
        if isinstance(index, tuple):
            return self.row_major_get(row_major_index(self, index))
        if index < 0:
            index += len(self)
        if index < 0 or index >= len(self):
            raise IndexError(f"index {index} out of range for length {len(self)}")
        return self.row_major_get(index)

    def __setitem__(self, index, value):
        if isinstance(index, tuple):
            self.row_major_set(row_major_index(self, index), value)
            return
        if index < 0:
            index += len(self)
        if index < 0 or index >= len(self):
            raise IndexError(f"index {index} out of range for length {len(self)}")
        self.row_major_set(index, value)

    def __iter__(self):
        for i in range(len(self)):
            yield self.row_major_get(i)

    def __repr__(self):
        # The printed representation is the printer's job; this is for Python
        # tracebacks only.
        return (f"<LispArray {self.dimensions} of {self.element_type.name}"
                f"{'' if self.fill_pointer is None else f' fp={self.fill_pointer}'}>")

    # --- growth, for the adjustable operators ---

    @property
    def capacity(self):
        """How many elements the storage holds, ignoring the fill pointer."""
        return self.total_size

    def resize(self, new_dimensions, initial_element=None, keep_contents=True):
        """Grow or shrink in place (ADJUST-ARRAY on an adjustable array).

        Adjusting an array *in place* is required, not an optimization: CLHS
        15.1.4 says an adjustable array's identity is preserved, so every
        other reference to it must see the new dimensions.
        """
        new_dimensions = tuple(int(d) for d in new_dimensions)
        filler = (default_element(self.element_type) if initial_element is None
                  else coerce_element(initial_element, self.element_type))
        old_data = self._data if self._data is not None else [
            self.row_major_get(i) for i in range(self.total_size)]
        old_dimensions = self.dimensions
        size = 1
        for d in new_dimensions:
            size *= d
        if keep_contents and len(new_dimensions) == len(old_dimensions) and len(new_dimensions) > 1:
            # Multi-dimensional adjustment keeps each element at its own
            # subscripts, not at its old row-major index (CLHS 15.1.4).
            data = [filler] * size
            self_dims = old_dimensions
            for old_index in range(len(old_data)):
                subscripts = _subscripts_from_row_major(old_index, self_dims)
                if all(s < new_dimensions[i] for i, s in enumerate(subscripts)):
                    new_index = 0
                    for i, s in enumerate(subscripts):
                        new_index = new_index * new_dimensions[i] + s
                    data[new_index] = old_data[old_index]
        elif keep_contents:
            data = old_data[:size] + [filler] * max(0, size - len(old_data))
        else:
            data = [filler] * size
        self.dimensions = new_dimensions
        self.displaced_to = None
        self.displaced_index_offset = 0
        self._data = data
        if self.fill_pointer is not None:
            self.fill_pointer = min(self.fill_pointer, size)
        return self


def _subscripts_from_row_major(index, dimensions):
    """The subscripts a row-major index denotes in an array of `dimensions`."""
    subscripts = []
    for i in range(len(dimensions) - 1, -1, -1):
        subscripts.append(index % dimensions[i])
        index //= dimensions[i]
    subscripts.reverse()
    return subscripts


# ===== THE PROTOCOL: the five questions, for every representation =====


def is_array(value):
    """True for every array: CLHS 15.1 counts strings and vectors as arrays."""
    return isinstance(value, (LispArray, list, tuple, lisptype.LispString, str))


def is_vector(value):
    """True for a one-dimensional array."""
    if isinstance(value, LispArray):
        return value.rank == 1
    return isinstance(value, (list, tuple, lisptype.LispString, str))


def array_rank_of(value):
    if isinstance(value, LispArray):
        return value.rank
    _require_array(value, 'ARRAY-RANK')
    return 1


def array_dimensions_of(value):
    """The dimensions of an array, as a Python tuple.

    The dimension of a vector is its *total size*, never its fill pointer
    (CLHS 15.1.2) -- reporting the fill pointer is what made
    `(array-dimension (make-array 10 :fill-pointer 5) 0)` answer 5.
    """
    if isinstance(value, LispArray):
        return value.dimensions
    _require_array(value, 'ARRAY-DIMENSIONS')
    return (_storage_size(value),)


def array_total_size_of(value):
    if isinstance(value, LispArray):
        return value.total_size
    _require_array(value, 'ARRAY-TOTAL-SIZE')
    return _storage_size(value)


def element_type_of(value):
    if isinstance(value, LispArray):
        return value.element_type
    if isinstance(value, (lisptype.LispString, str)):
        return CHARACTER_TYPE
    _require_array(value, 'ARRAY-ELEMENT-TYPE')
    return T_TYPE


def fill_pointer_of(value):
    """The array's fill pointer, or None if it has none."""
    if isinstance(value, LispArray):
        return value.fill_pointer
    if isinstance(value, lisptype.LispString):
        return value.fill_pointer
    return None


def is_adjustable(value):
    if isinstance(value, LispArray):
        return value.adjustable
    if isinstance(value, lisptype.LispString):
        return bool(value.adjustable)
    return False


def is_simple_array(value):
    if isinstance(value, LispArray):
        return value.is_simple
    if isinstance(value, lisptype.LispString):
        return value.fill_pointer is None and not value.adjustable
    return is_array(value)


def displacement_of(value):
    """`(target, offset)` for a displaced array, `(NIL, 0)` otherwise."""
    if isinstance(value, LispArray) and value.displaced_to is not None:
        return value.displaced_to, value.displaced_index_offset
    return lisptype.NIL, 0


def _storage_size(value):
    """Total size of a representation that keeps no separate capacity."""
    if isinstance(value, lisptype.LispString):
        return len(value._data)
    return len(value)


def _require_array(value, what):
    if not is_array(value):
        raise lisptype.LispTypeError(
            f"{what}: {type(value).__name__} is not an array",
            expected_type="ARRAY", actual_value=value)


def row_major_index(array, subscripts):
    """The row-major index `subscripts` denotes (CLHS 15.1.1)."""
    dimensions = array_dimensions_of(array)
    subscripts = [int(s) for s in subscripts]
    if len(subscripts) != len(dimensions):
        raise lisptype.LispTypeError(
            f"array of rank {len(dimensions)} indexed with {len(subscripts)} "
            f"subscript(s)",
            expected_type=f"{len(dimensions)} subscripts", actual_value=subscripts)
    index = 0
    for axis, subscript in enumerate(subscripts):
        if subscript < 0 or subscript >= dimensions[axis]:
            raise lisptype.LispTypeError(
                f"subscript {subscript} is out of bounds for axis {axis} "
                f"of size {dimensions[axis]}",
                expected_type=f"index below {dimensions[axis]}", actual_value=subscript)
        index = index * dimensions[axis] + subscript
    return index


def row_major_get(array, index):
    """Read element `index` of any array representation, in row-major order."""
    if isinstance(array, LispArray):
        return array.row_major_get(index)
    _require_array(array, 'ROW-MAJOR-AREF')
    if index < 0 or index >= _storage_size(array):
        raise lisptype.LispTypeError(
            f"row-major index {index} is out of bounds for an array of total "
            f"size {_storage_size(array)}",
            expected_type=f"index below {_storage_size(array)}", actual_value=index)
    if isinstance(array, lisptype.LispString):
        return lisptype.Character(array._data[index])
    if isinstance(array, str):
        return lisptype.Character(array[index])
    return array[index]


def row_major_set(array, index, value):
    """Write element `index` of any array representation."""
    if isinstance(array, LispArray):
        return array.row_major_set(index, value)
    _require_array(array, 'ROW-MAJOR-AREF')
    if index < 0 or index >= _storage_size(array):
        raise lisptype.LispTypeError(
            f"row-major index {index} is out of bounds for an array of total "
            f"size {_storage_size(array)}",
            expected_type=f"index below {_storage_size(array)}", actual_value=index)
    if isinstance(array, lisptype.LispString):
        array._data[index] = coerce_element(value, CHARACTER_TYPE)
        return value
    if isinstance(array, (tuple, str)):
        # A Python `str` is the *other* string representation (plan.md
        # Finding I) and is immutable; assigning to one raises a Python
        # TypeError that would surface as the value of the form.
        raise lisptype.LispTypeError(
            "cannot store into an immutable vector", expected_type="mutable ARRAY",
            actual_value=array)
    array[index] = value
    return value


def array_elements(array):
    """Every active element of a vector, honoring its fill pointer."""
    length = fill_pointer_of(array)
    if length is None:
        length = array_total_size_of(array)
    return [row_major_get(array, i) for i in range(length)]


# ===== CONSTRUCTION =====


def _new_array(dimensions, element_type, data, fill_pointer=None, adjustable=False,
               displaced_to=None, displaced_index_offset=0):
    """Build the representation this array's five properties call for.

    The *only* place that decides between a Python `list`, a `LispString` and
    a `LispArray`; see this module's docstring for the rule.
    """
    dimensions = tuple(int(d) for d in dimensions)
    simple = fill_pointer is None and not adjustable and displaced_to is None

    if displaced_to is not None:
        return LispArray(dimensions, element_type, fill_pointer=fill_pointer,
                         adjustable=adjustable, displaced_to=displaced_to,
                         displaced_index_offset=displaced_index_offset)

    if len(dimensions) == 1 and element_type is CHARACTER_TYPE:
        text = lisptype.LispString(''.join(
            coerce_element(c, CHARACTER_TYPE) for c in data))
        text.element_type = CHARACTER_TYPE
        text.fill_pointer = fill_pointer
        text.adjustable = adjustable
        return text

    if len(dimensions) == 1 and element_type is T_TYPE and simple:
        return list(data)

    return LispArray(dimensions, element_type, data=[
        coerce_element(e, element_type) for e in data],
        fill_pointer=fill_pointer, adjustable=adjustable)


def make_bit_vector(bits):
    """A simple bit vector holding `bits` -- the constructor `#*1011` uses."""
    values = [coerce_element(b, BIT_TYPE, 'bit vector') for b in bits]
    return LispArray((len(values),), BIT_TYPE, data=values)


def _dimensions_argument(dimensions, what='MAKE-ARRAY'):
    """A dimension designator: a non-negative integer, or a list of them."""
    if dimensions is None or dimensions is lisptype.NIL:
        return ()
    if isinstance(dimensions, bool):
        raise lisptype.LispTypeError(
            f"{what}: {dimensions!r} is not a dimension", expected_type="dimension",
            actual_value=dimensions)
    if isinstance(dimensions, int):
        return (_dimension(dimensions, what),)
    if isinstance(dimensions, lisptype.lispCons):
        dims = []
        current = dimensions
        while isinstance(current, lisptype.lispCons):
            dims.append(_dimension(current.car, what))
            current = current.cdr
        return tuple(dims)
    if isinstance(dimensions, (list, tuple, LispArray)):
        return tuple(_dimension(d, what) for d in dimensions)
    raise lisptype.LispTypeError(
        f"{what}: {dimensions!r} is not a valid dimension specification",
        expected_type="a dimension or list of dimensions", actual_value=dimensions)


def nonnegative_integer(value, what, expected="non-negative integer"):
    """Validate a CLHS `unsigned-byte` argument -- a size, count or dimension.

    This is the check that `int(value)` is *not*. `int()` accepts a float, a
    Decimal, a numeric string and anything with `__int__`, so a Lisp form
    whose argument is out of type silently gets a plausible number instead of
    the TYPE-ERROR ANSI requires -- and if that number is large, the caller
    then tries to build it. `(make-list 1.0e18)` reached `range(10**18)` and
    allocated cons cells until the process held 27GB, which is how the
    2026-08-15 full run wedged: `cons/make-list.lsp`'s MAKE-LIST.ERROR.1
    calls `(make-list x)` over `*universe*` precisely to check that every
    non-`unsigned-byte` x signals.

    `bool` is excluded explicitly because it is an `int` subclass in Python,
    so T/NIL would otherwise pass as 1/0 (plan.md Finding M: a Python type
    test standing in for a Lisp one).
    """
    if isinstance(value, bool) or not isinstance(value, int) or value < 0:
        raise lisptype.LispTypeError(
            f"{what}: {value!r} is not a {expected}",
            expected_type=expected, actual_value=value)
    return value


def _dimension(value, what):
    return nonnegative_integer(value, what, expected="valid array dimension")


def _contents_elements(contents, dimensions, element_type, axis=0):
    """Flatten `:initial-contents` in row-major order, checking its shape.

    CLHS 15.1.2: the contents must be nested sequences matching the
    dimensions exactly, and a sequence of the wrong length is an error rather
    than something to pad or truncate.
    """
    from .sequence_protocol import seq_elements

    if axis == len(dimensions):
        return [coerce_element(contents, element_type, 'MAKE-ARRAY')]
    items = [string_element(contents, item)
             for item in seq_elements(contents, 'MAKE-ARRAY :initial-contents')]
    if len(items) != dimensions[axis]:
        raise lisptype.LispTypeError(
            f"MAKE-ARRAY: :initial-contents has {len(items)} element(s) where "
            f"axis {axis} has dimension {dimensions[axis]}",
            expected_type=f"sequence of length {dimensions[axis]}",
            actual_value=contents)
    flat = []
    for item in items:
        flat.extend(_contents_elements(item, dimensions, element_type, axis + 1))
    return flat


_UNSUPPLIED = object()


def _check_other_keys(other_keys, allow_other_keys, what):
    """Apply CLHS 3.4.1.4.1's `:allow-other-keys` rule to a builtin's keywords.

    An unrecognized keyword is a PROGRAM-ERROR unless `:allow-other-keys` is
    true, in which case it is ignored.
    """
    if not other_keys:
        return
    if allow_other_keys not in (None, lisptype.NIL, False):
        return
    names = ', '.join(sorted(k.upper().replace('_', '-') for k in other_keys))
    raise lisptype.LispProgramError(
        f"{what}: unrecognized keyword argument(s): {names}")


@_registry.cl_function('MAKE-ARRAY')
def make_array(dimensions, element_type=_UNSUPPLIED, initial_element=_UNSUPPLIED,
               initial_contents=_UNSUPPLIED, adjustable=None, fill_pointer=None,
               displaced_to=None, displaced_index_offset=None,
               allow_other_keys=None, **other_keys):
    """Create an array (CLHS 15.2.15).

    Every keyword is honored, including the three that used to be discarded:
    `:element-type` (which decides the representation), `:displaced-to` (which
    makes the result share the target's storage rather than copy it) and
    `:displaced-index-offset`.
    """
    _check_other_keys(other_keys, allow_other_keys, 'MAKE-ARRAY')
    dims = _dimensions_argument(dimensions)
    etype = T_TYPE if element_type is _UNSUPPLIED else upgraded_element_type(element_type)

    if initial_element is not _UNSUPPLIED and initial_contents is not _UNSUPPLIED:
        raise lisptype.LispProgramError(
            "MAKE-ARRAY: :initial-element and :initial-contents are mutually exclusive")

    displaced_to = None if displaced_to is lisptype.NIL else displaced_to
    if displaced_to is not None and (initial_element is not _UNSUPPLIED
                                     or initial_contents is not _UNSUPPLIED):
        raise lisptype.LispProgramError(
            "MAKE-ARRAY: :displaced-to may not be combined with :initial-element "
            "or :initial-contents")

    total = 1
    for d in dims:
        total *= d

    if displaced_to is not None:
        offset = 0 if displaced_index_offset in (None, lisptype.NIL) else int(displaced_index_offset)
        if offset + total > array_total_size_of(displaced_to):
            raise lisptype.LispTypeError(
                f"MAKE-ARRAY: :displaced-to array of total size "
                f"{array_total_size_of(displaced_to)} is too small for "
                f"{total} element(s) at offset {offset}",
                expected_type="large enough array", actual_value=displaced_to)
        data = []
    elif initial_contents is not _UNSUPPLIED:
        data = _contents_elements(initial_contents, dims, etype)
        offset = 0
    else:
        fill = (default_element(etype) if initial_element is _UNSUPPLIED
                else coerce_element(initial_element, etype, 'MAKE-ARRAY'))
        data = [fill] * total
        offset = 0

    resolved_fill_pointer = _fill_pointer_argument(fill_pointer, dims)
    adjustable = bool(adjustable) and adjustable is not lisptype.NIL

    return _new_array(dims, etype, data, fill_pointer=resolved_fill_pointer,
                      adjustable=adjustable, displaced_to=displaced_to,
                      displaced_index_offset=offset)


def _fill_pointer_argument(fill_pointer, dimensions):
    """Resolve `:fill-pointer` -- NIL, T, or an index (CLHS 15.2.15)."""
    if fill_pointer is None or fill_pointer is lisptype.NIL:
        return None
    if len(dimensions) != 1:
        raise lisptype.LispTypeError(
            "MAKE-ARRAY: only a vector may have a fill pointer",
            expected_type="array of rank 1", actual_value=dimensions)
    if fill_pointer is lisptype.T or fill_pointer is True:
        return dimensions[0]
    if isinstance(fill_pointer, int) and not isinstance(fill_pointer, bool):
        if fill_pointer < 0 or fill_pointer > dimensions[0]:
            raise lisptype.LispTypeError(
                f"MAKE-ARRAY: fill pointer {fill_pointer} is out of range for a "
                f"vector of size {dimensions[0]}",
                expected_type=f"index in [0,{dimensions[0]}]", actual_value=fill_pointer)
        return fill_pointer
    raise lisptype.LispTypeError(
        f"MAKE-ARRAY: {fill_pointer!r} is not a valid fill pointer",
        expected_type="T, NIL, or an index", actual_value=fill_pointer)


# ===== ACCESS =====


@_registry.cl_function('AREF')
def aref(array, *subscripts):
    """Access an array element (CLHS 15.2.7).

    AREF reaches every element of the array's storage, *including* elements
    beyond a fill pointer -- that is exactly what distinguishes it from ELT
    (CLHS 15.1.2.1). It is also the operator that has to be right about rank:
    the implementation it replaced indexed one subscript at a time, so a
    2-D reference raised `IndexError: Expected 2 indices, got 1` and the
    Python exception surfaced as the value of the form.
    """
    return row_major_get(array, row_major_index(array, subscripts))


def aref_set(array, subscripts, value):
    """`(setf (aref array subscript...) value)`.

    Exposed as a function because the SETF/INCF/DECF/ROTATEF paths each used
    to reimplement it, and each of those copies accepted exactly *one*
    subscript -- so `(setf (aref a i j) v)` silently wrote to element `i`.
    """
    return row_major_set(array, row_major_index(array, subscripts), value)


@_registry.cl_function('ROW-MAJOR-AREF')
def row_major_aref(array, index):
    """Access an array element by row-major index (CLHS 15.2.9)."""
    return row_major_get(array, int(index))


def row_major_aref_set(array, index, value):
    """`(setf (row-major-aref array index) value)`."""
    return row_major_set(array, int(index), value)


@_registry.cl_function('ARRAY-ROW-MAJOR-INDEX')
def array_row_major_index(array, *subscripts):
    """Compute the row-major index of `subscripts` (CLHS 15.2.6)."""
    return row_major_index(array, subscripts)


@_registry.cl_function('SVREF')
def svref(vector, index):
    """Access a simple vector element (CLHS 15.2.13)."""
    if not (isinstance(vector, (list, tuple))
            or (isinstance(vector, LispArray) and vector.rank == 1
                and vector.element_type is T_TYPE and vector.is_simple)):
        raise lisptype.LispTypeError(
            f"SVREF: {type(vector).__name__} is not a simple vector",
            expected_type="SIMPLE-VECTOR", actual_value=vector)
    return row_major_get(vector, int(index))


@_registry.cl_function('BIT')
def bit_fn(bit_array, *subscripts):
    """Access a bit array element (CLHS 15.2.8)."""
    _require_bit_array(bit_array, 'BIT')
    return row_major_get(bit_array, row_major_index(bit_array, subscripts))


@_registry.cl_function('SBIT')
def sbit(bit_array, *subscripts):
    """Access a simple bit array element (CLHS 15.2.8)."""
    _require_bit_array(bit_array, 'SBIT')
    return row_major_get(bit_array, row_major_index(bit_array, subscripts))


# ===== THE FIVE PROPERTIES, AS OPERATORS =====


@_registry.cl_function('ARRAY-RANK')
def array_rank(array):
    """Number of dimensions of an array (CLHS 15.2.5)."""
    return array_rank_of(array)


@_registry.cl_function('ARRAY-DIMENSIONS')
def array_dimensions(array):
    """All dimensions of an array, as a *list* (CLHS 15.2.3)."""
    from .sequence_protocol import make_lisp_list
    return make_lisp_list(array_dimensions_of(array))


@_registry.cl_function('ARRAY-DIMENSION')
def array_dimension(array, axis_number):
    """One dimension of an array (CLHS 15.2.2)."""
    dimensions = array_dimensions_of(array)
    axis = int(axis_number)
    if axis < 0 or axis >= len(dimensions):
        raise lisptype.LispTypeError(
            f"ARRAY-DIMENSION: axis {axis} is out of range for an array of rank "
            f"{len(dimensions)}",
            expected_type=f"axis below {len(dimensions)}", actual_value=axis_number)
    return dimensions[axis]


@_registry.cl_function('ARRAY-TOTAL-SIZE')
def array_total_size(array):
    """Total number of elements, ignoring any fill pointer (CLHS 15.2.10)."""
    return array_total_size_of(array)


@_registry.cl_function('ARRAY-ELEMENT-TYPE')
def array_element_type(array):
    """The element type of an array, as a type *symbol* (CLHS 15.2.1)."""
    return element_type_of(array)


@_registry.cl_function('UPGRADED-ARRAY-ELEMENT-TYPE')
def upgraded_array_element_type(typespec, environment=None):
    """The element type an array of `typespec` will actually have (CLHS 15.2.14)."""
    return upgraded_element_type(typespec)


@_registry.cl_function('ARRAY-IN-BOUNDS-P')
def array_in_bounds_p(array, *subscripts):
    """True if `subscripts` are valid for `array` (CLHS 15.2.4)."""
    dimensions = array_dimensions_of(array)
    if len(subscripts) != len(dimensions):
        return lisptype.NIL
    for axis, subscript in enumerate(subscripts):
        if isinstance(subscript, bool) or not isinstance(subscript, int):
            raise lisptype.LispTypeError(
                f"ARRAY-IN-BOUNDS-P: {subscript!r} is not a subscript",
                expected_type="INTEGER", actual_value=subscript)
        if subscript < 0 or subscript >= dimensions[axis]:
            return lisptype.NIL
    return lisptype.T


@_registry.cl_function('ARRAY-DISPLACEMENT')
def array_displacement(array):
    """The array this one is displaced to, and the offset (CLHS 15.2.11)."""
    _require_array(array, 'ARRAY-DISPLACEMENT')
    target, offset = displacement_of(array)
    return lisptype.MultipleValues([target, offset])


@_registry.cl_function('ADJUSTABLE-ARRAY-P')
def adjustable_array_p(array):
    """True if `array` may be adjusted in place (CLHS 15.2.12)."""
    _require_array(array, 'ADJUSTABLE-ARRAY-P')
    return lisptype.lisp_bool(is_adjustable(array))


@_registry.cl_function('ARRAY-HAS-FILL-POINTER-P')
def array_has_fill_pointer_p(array):
    """True if `array` has a fill pointer (CLHS 15.2.16)."""
    _require_array(array, 'ARRAY-HAS-FILL-POINTER-P')
    return lisptype.lisp_bool(fill_pointer_of(array) is not None)


@_registry.cl_function('FILL-POINTER')
def fill_pointer(vector):
    """The fill pointer of a vector (CLHS 17.3.9).

    A vector *without* a fill pointer is a type error, not length: answering
    the length made `array-has-fill-pointer-p` and `fill-pointer` disagree.
    """
    pointer = fill_pointer_of(vector)
    if pointer is None:
        raise lisptype.LispTypeError(
            f"FILL-POINTER: {type(vector).__name__} has no fill pointer",
            expected_type="vector with a fill pointer", actual_value=vector)
    return pointer


def set_fill_pointer(vector, new_pointer):
    """`(setf (fill-pointer vector) n)`."""
    if fill_pointer_of(vector) is None:
        raise lisptype.LispTypeError(
            f"SETF FILL-POINTER: {type(vector).__name__} has no fill pointer",
            expected_type="vector with a fill pointer", actual_value=vector)
    limit = array_total_size_of(vector)
    new_pointer = int(new_pointer)
    if new_pointer < 0 or new_pointer > limit:
        raise lisptype.LispTypeError(
            f"SETF FILL-POINTER: {new_pointer} is out of range for a vector of "
            f"size {limit}",
            expected_type=f"index in [0,{limit}]", actual_value=new_pointer)
    vector.fill_pointer = new_pointer
    return new_pointer


# ===== GROWTH =====


def _require_fill_pointer(vector, what):
    pointer = fill_pointer_of(vector)
    if pointer is None:
        raise lisptype.LispTypeError(
            f"{what}: {type(vector).__name__} has no fill pointer",
            expected_type="vector with a fill pointer", actual_value=vector)
    return pointer


@_registry.cl_function('VECTOR-PUSH')
def vector_push(new_element, vector):
    """Store `new_element` at the fill pointer, or answer NIL if full (CLHS 17.3.10)."""
    pointer = _require_fill_pointer(vector, 'VECTOR-PUSH')
    if pointer >= array_total_size_of(vector):
        return lisptype.NIL
    row_major_set(vector, pointer, new_element)
    vector.fill_pointer = pointer + 1
    return pointer


@_registry.cl_function('VECTOR-PUSH-EXTEND')
def vector_push_extend(new_element, vector, extension=None):
    """Store `new_element`, growing the vector if it is full (CLHS 17.3.10)."""
    pointer = _require_fill_pointer(vector, 'VECTOR-PUSH-EXTEND')
    if pointer >= array_total_size_of(vector):
        if not is_adjustable(vector):
            raise lisptype.LispTypeError(
                "VECTOR-PUSH-EXTEND: vector is full and not adjustable",
                expected_type="adjustable vector", actual_value=vector)
        size = array_total_size_of(vector)
        growth = int(extension) if extension not in (None, lisptype.NIL) else max(size, 8)
        _grow_vector(vector, size + growth)
    row_major_set(vector, pointer, new_element)
    vector.fill_pointer = pointer + 1
    return pointer


def _grow_vector(vector, new_size):
    """Extend a vector's storage in place, preserving its identity."""
    if isinstance(vector, LispArray):
        vector.resize((new_size,))
        return
    if isinstance(vector, lisptype.LispString):
        vector._data.extend([' '] * (new_size - len(vector._data)))
        return
    raise lisptype.LispTypeError(
        f"cannot extend {type(vector).__name__}", expected_type="adjustable vector",
        actual_value=vector)


@_registry.cl_function('VECTOR-POP')
def vector_pop(vector):
    """Remove and return the element below the fill pointer (CLHS 17.3.11)."""
    pointer = _require_fill_pointer(vector, 'VECTOR-POP')
    if pointer <= 0:
        raise lisptype.LispTypeError(
            "VECTOR-POP: the vector is empty", expected_type="non-empty vector",
            actual_value=vector)
    value = row_major_get(vector, pointer - 1)
    vector.fill_pointer = pointer - 1
    return value


@_registry.cl_function('ADJUST-ARRAY')
def adjust_array(array, new_dimensions, element_type=_UNSUPPLIED,
                 initial_element=_UNSUPPLIED, initial_contents=_UNSUPPLIED,
                 fill_pointer=None, displaced_to=None, displaced_index_offset=None,
                 allow_other_keys=None, **other_keys):
    """Change an array's dimensions (CLHS 15.2.17).

    An *adjustable* array is adjusted in place, so every reference to it sees
    the change; a non-adjustable one yields a fresh array, which is why the
    result must be used rather than assumed to be the argument.
    """
    _check_other_keys(other_keys, allow_other_keys, 'ADJUST-ARRAY')
    _require_array(array, 'ADJUST-ARRAY')
    dims = _dimensions_argument(new_dimensions, 'ADJUST-ARRAY')
    if len(dims) != array_rank_of(array):
        raise lisptype.LispTypeError(
            f"ADJUST-ARRAY: new dimensions of rank {len(dims)} for an array of "
            f"rank {array_rank_of(array)}",
            expected_type=f"{array_rank_of(array)} dimension(s)", actual_value=new_dimensions)
    etype = (element_type_of(array) if element_type is _UNSUPPLIED
             else upgraded_element_type(element_type))
    if etype is not element_type_of(array):
        raise lisptype.LispTypeError(
            "ADJUST-ARRAY: :element-type does not match the array's element type",
            expected_type=str(element_type_of(array)), actual_value=element_type)

    displaced_to = None if displaced_to is lisptype.NIL else displaced_to
    total = 1
    for d in dims:
        total *= d

    if displaced_to is not None:
        offset = 0 if displaced_index_offset in (None, lisptype.NIL) else int(displaced_index_offset)
        if offset + total > array_total_size_of(displaced_to):
            raise lisptype.LispTypeError(
                "ADJUST-ARRAY: :displaced-to array is too small",
                expected_type="large enough array", actual_value=displaced_to)
        new_pointer = _adjusted_fill_pointer(array, fill_pointer, dims)
        adjusted = LispArray(dims, etype, fill_pointer=new_pointer,
                             adjustable=True, displaced_to=displaced_to,
                             displaced_index_offset=offset)
        if is_adjustable(array) and isinstance(array, LispArray):
            array.dimensions = adjusted.dimensions
            array._data = None
            array.displaced_to = displaced_to
            array.displaced_index_offset = offset
            array.fill_pointer = new_pointer
            return array
        return adjusted

    if initial_contents is not _UNSUPPLIED:
        data = _contents_elements(initial_contents, dims, etype)
    else:
        filler = (default_element(etype) if initial_element is _UNSUPPLIED
                  else coerce_element(initial_element, etype, 'ADJUST-ARRAY'))
        data = _adjusted_contents(array, dims, filler)

    new_pointer = _adjusted_fill_pointer(array, fill_pointer, dims)

    if is_adjustable(array):
        if isinstance(array, LispArray):
            array.dimensions = dims
            array.displaced_to = None
            array.displaced_index_offset = 0
            array._data = [coerce_element(e, etype) for e in data]
            array.fill_pointer = new_pointer
            return array
        if isinstance(array, lisptype.LispString):
            array._data = [coerce_element(e, CHARACTER_TYPE) for e in data]
            array.fill_pointer = new_pointer
            return array
    return _new_array(dims, etype, data, fill_pointer=new_pointer,
                      adjustable=is_adjustable(array))


def _adjusted_contents(array, dims, filler):
    """The elements of `array` re-laid-out for `dims`, padded with `filler`.

    Element (i j ...) of the adjusted array is element (i j ...) of the old
    one where both exist (CLHS 15.1.4) -- *not* the element at the same
    row-major index, which is only the same thing for vectors.
    """
    old_dims = array_dimensions_of(array)
    total = 1
    for d in dims:
        total *= d
    data = [filler] * total
    if len(dims) == 1:
        keep = min(total, array_total_size_of(array))
        for i in range(keep):
            data[i] = row_major_get(array, i)
        return data
    for old_index in range(array_total_size_of(array)):
        subscripts = _subscripts_from_row_major(old_index, old_dims)
        if all(s < dims[axis] for axis, s in enumerate(subscripts)):
            new_index = 0
            for axis, s in enumerate(subscripts):
                new_index = new_index * dims[axis] + s
            data[new_index] = row_major_get(array, old_index)
    return data


def _adjusted_fill_pointer(array, fill_pointer, dims):
    """The fill pointer the adjusted array keeps (CLHS 15.2.17)."""
    if fill_pointer in (None, lisptype.NIL):
        existing = fill_pointer_of(array)
        if existing is None:
            return None
        return min(existing, dims[0])
    return _fill_pointer_argument(fill_pointer, dims)


# ===== PREDICATES =====


@_registry.cl_function('ARRAYP')
def arrayp(object):
    """True if `object` is an array -- strings and vectors included (CLHS 15.2.18)."""
    return lisptype.lisp_bool(is_array(object))


@_registry.cl_function('VECTORP')
def vectorp(object):
    """True if `object` is a vector (a one-dimensional array)."""
    return lisptype.lisp_bool(is_vector(object))


@_registry.cl_function('SIMPLE-VECTOR-P')
def simple_vector_p(object):
    """True for a simple vector: rank 1, element type T, nothing else (CLHS 15.1.2.2).

    A string is *not* a simple vector even though it is a simple array, and a
    bit vector is not either -- both have a specialized element type.
    """
    if isinstance(object, (list, tuple)):
        return lisptype.T
    return lisptype.lisp_bool(
        isinstance(object, LispArray) and object.rank == 1
        and object.element_type is T_TYPE and object.is_simple)


def is_bit_array(object):
    return isinstance(object, LispArray) and object.element_type is BIT_TYPE


@_registry.cl_function('BIT-VECTOR-P')
def bit_vector_p(object):
    """True for a vector whose element type is BIT.

    Not "a vector that happens to hold zeroes and ones", which is what the
    implementation this replaced tested -- that made `#(0 1)`, a general
    vector, answer T, and made the two types indistinguishable.
    """
    return lisptype.lisp_bool(is_bit_array(object) and object.rank == 1)


@_registry.cl_function('SIMPLE-BIT-VECTOR-P')
def simple_bit_vector_p(object):
    """True for a bit vector that is simple (CLHS 15.1.2.2)."""
    return lisptype.lisp_bool(
        is_bit_array(object) and object.rank == 1 and object.is_simple)


_ARRAY_TYPE_NAMES = frozenset((
    'ARRAY', 'SIMPLE-ARRAY', 'VECTOR', 'SIMPLE-VECTOR', 'BIT-VECTOR',
    'SIMPLE-BIT-VECTOR', 'STRING', 'SIMPLE-STRING', 'BASE-STRING',
    'SIMPLE-BASE-STRING'))


def is_array_type_name(name):
    """True if `name` heads an array type specifier (CLHS 4.2.3)."""
    return name in _ARRAY_TYPE_NAMES


def _wild(spec):
    """True for the `*` that stands for "any" in an array type specifier.

    Only the *symbol* is wild. Asking `_type_name` would also answer `*` for
    the compound specifier `(* *)`, whose head happens to be `*` -- and that
    made `(typep "abc" '(array * (* *)))` true, i.e. a rank-2 requirement
    satisfied by a rank-1 object.
    """
    if isinstance(spec, (lisptype.lispCons, list, tuple)):
        return False
    return _type_name(spec) == '*'


def array_type_matches(object, type_name, args=()):
    """Does `object` match the array type specifier `(type_name . args)`?

    One matcher for all ten array type specifiers -- `array`, `vector`,
    `bit-vector`, `string`, their `simple-` variants, and every compound form
    of each. TYPEP had a branch per name, each of which tested
    `isinstance(object, (list, tuple, AdjustableVector))` and ignored the
    element type and dimensions entirely, so `(typep #(1 2) 'bit-vector)`,
    `(typep "ab" 'simple-vector)` and `(typep #(1 2) '(array t (5)))` were all
    T. The dimensions and element type only became answerable once the array
    model recorded them, which is why this lives here rather than in TYPEP.
    """
    if not is_array(object):
        return False

    simple_required = type_name.startswith('SIMPLE-')
    base = type_name[len('SIMPLE-'):] if simple_required else type_name
    if simple_required and not is_simple_array(object):
        return False

    if base != 'ARRAY' and array_rank_of(object) != 1:
        return False

    element_type = element_type_of(object)
    if type_name == 'SIMPLE-VECTOR' and element_type is not T_TYPE:
        return False
    if base == 'BIT-VECTOR' and element_type is not BIT_TYPE:
        return False
    if base == 'STRING' and element_type not in (CHARACTER_TYPE, NIL_TYPE):
        # NIL is a subtype of every type, CHARACTER included, so an
        # `(array nil (*))` is a STRING too (CLHS 15.1, `*.NIL-ARRAY.1`'s
        # `:nil-vectors-are-strings` tests) -- matching `characters.is_string`,
        # which STRINGP/SIMPLE-STRING-P go through, and `typespec.py`'s
        # `_array_type` STRING branch, which SUBTYPEP goes through. Without
        # this a mismatch here was directly observable as TYPEP and STRINGP
        # disagreeing on the same object (`check-type-predicate`).
        # BASE-STRING gets no such allowance below: unlike the bare name
        # STRING, it is exactly `(vector base-char)` with nothing extra
        # (`base-string-is-vector-of-base-char.1`/`.2` require the plain
        # equivalence, no NIL-shaped exception).
        return False
    if base == 'BASE-STRING' and element_type is not CHARACTER_TYPE:
        return False

    args = list(args)
    if base in ('ARRAY', 'VECTOR'):
        # (array element-type dimensions) / (vector element-type size)
        if args and not _wild(args[0]):
            if upgraded_element_type(args[0]) is not element_type:
                return False
        dimension_spec = args[1] if len(args) > 1 else None
    else:
        # (string size) / (bit-vector size) / (simple-vector size)
        dimension_spec = args[0] if args else None

    if dimension_spec is None or _wild(dimension_spec):
        return True
    return _dimensions_match(object, dimension_spec, base)


def _dimensions_match(object, spec, base):
    """Check an array type specifier's dimension argument against `object`.

    The argument is a rank (an integer, for `array`), a size (an integer, for
    a vector type) or a list of per-axis dimensions, each of which may be `*`.
    """
    dimensions = array_dimensions_of(object)
    if isinstance(spec, bool):
        return False
    if isinstance(spec, int):
        if base == 'ARRAY':
            return len(dimensions) == spec
        return len(dimensions) == 1 and dimensions[0] == spec
    from .sequence_protocol import seq_elements

    try:
        axes = seq_elements(spec, 'array type specifier')
    except lisptype.LispTypeError:
        return False
    if len(axes) != len(dimensions):
        return False
    for axis, size in zip(axes, dimensions):
        if _wild(axis):
            continue
        if not isinstance(axis, int) or isinstance(axis, bool) or axis != size:
            return False
    return True


def _require_bit_array(array, what):
    if not is_bit_array(array):
        raise lisptype.LispTypeError(
            f"{what}: {type(array).__name__} is not a bit array",
            expected_type="BIT-ARRAY", actual_value=array)


# ===== BIT-WISE OPERATORS =====


def _bit_result(array1, result_array, dimensions):
    """Where a bit-wise operator stores its result (CLHS 15.2.19).

    The third argument is a *destination designator*: NIL (or absent) means a
    fresh array, T means the first argument, and an array means that array.
    """
    if result_array is None or result_array is lisptype.NIL:
        return LispArray(dimensions, BIT_TYPE)
    if result_array is lisptype.T or result_array is True:
        return array1
    _require_bit_array(result_array, 'bit-wise operation')
    if tuple(array_dimensions_of(result_array)) != tuple(dimensions):
        raise lisptype.LispTypeError(
            "bit-wise operation: result array has the wrong dimensions",
            expected_type=str(dimensions), actual_value=result_array)
    return result_array


def _bit_op(name, operation, array1, array2, result_array):
    _require_bit_array(array1, name)
    _require_bit_array(array2, name)
    if array1.dimensions != array2.dimensions:
        raise lisptype.LispTypeError(
            f"{name}: bit arrays have different dimensions",
            expected_type=str(array1.dimensions), actual_value=array2.dimensions)
    bits = [operation(row_major_get(array1, i), row_major_get(array2, i))
            for i in range(array1.total_size)]
    destination = _bit_result(array1, result_array, array1.dimensions)
    for i, bit in enumerate(bits):
        row_major_set(destination, i, bit)
    return destination


@_registry.cl_function('BIT-AND')
def bit_and(bit_array1, bit_array2, result_array=None):
    """Bit-wise AND of two bit arrays (CLHS 15.2.19)."""
    return _bit_op('BIT-AND', lambda a, b: a & b, bit_array1, bit_array2, result_array)


@_registry.cl_function('BIT-IOR')
def bit_ior(bit_array1, bit_array2, result_array=None):
    """Bit-wise inclusive OR of two bit arrays (CLHS 15.2.19)."""
    return _bit_op('BIT-IOR', lambda a, b: a | b, bit_array1, bit_array2, result_array)


@_registry.cl_function('BIT-XOR')
def bit_xor(bit_array1, bit_array2, result_array=None):
    """Bit-wise exclusive OR of two bit arrays (CLHS 15.2.19)."""
    return _bit_op('BIT-XOR', lambda a, b: a ^ b, bit_array1, bit_array2, result_array)


@_registry.cl_function('BIT-EQV')
def bit_eqv(bit_array1, bit_array2, result_array=None):
    """Bit-wise equivalence of two bit arrays (CLHS 15.2.19)."""
    return _bit_op('BIT-EQV', lambda a, b: 1 - (a ^ b), bit_array1, bit_array2, result_array)


@_registry.cl_function('BIT-NAND')
def bit_nand(bit_array1, bit_array2, result_array=None):
    """Bit-wise NAND of two bit arrays (CLHS 15.2.19)."""
    return _bit_op('BIT-NAND', lambda a, b: 1 - (a & b), bit_array1, bit_array2, result_array)


@_registry.cl_function('BIT-NOR')
def bit_nor(bit_array1, bit_array2, result_array=None):
    """Bit-wise NOR of two bit arrays (CLHS 15.2.19)."""
    return _bit_op('BIT-NOR', lambda a, b: 1 - (a | b), bit_array1, bit_array2, result_array)


@_registry.cl_function('BIT-ANDC1')
def bit_andc1(bit_array1, bit_array2, result_array=None):
    """Bit-wise AND of the complement of the first array with the second."""
    return _bit_op('BIT-ANDC1', lambda a, b: (1 - a) & b, bit_array1, bit_array2, result_array)


@_registry.cl_function('BIT-ANDC2')
def bit_andc2(bit_array1, bit_array2, result_array=None):
    """Bit-wise AND of the first array with the complement of the second."""
    return _bit_op('BIT-ANDC2', lambda a, b: a & (1 - b), bit_array1, bit_array2, result_array)


@_registry.cl_function('BIT-ORC1')
def bit_orc1(bit_array1, bit_array2, result_array=None):
    """Bit-wise OR of the complement of the first array with the second."""
    return _bit_op('BIT-ORC1', lambda a, b: (1 - a) | b, bit_array1, bit_array2, result_array)


@_registry.cl_function('BIT-ORC2')
def bit_orc2(bit_array1, bit_array2, result_array=None):
    """Bit-wise OR of the first array with the complement of the second."""
    return _bit_op('BIT-ORC2', lambda a, b: a | (1 - b), bit_array1, bit_array2, result_array)


@_registry.cl_function('BIT-NOT')
def bit_not(bit_array, result_array=None):
    """Bit-wise complement of a bit array (CLHS 15.2.20)."""
    _require_bit_array(bit_array, 'BIT-NOT')
    bits = [1 - row_major_get(bit_array, i) for i in range(bit_array.total_size)]
    destination = _bit_result(bit_array, result_array, bit_array.dimensions)
    for i, bit in enumerate(bits):
        row_major_set(destination, i, bit)
    return destination


# ===== VECTOR CONSTRUCTION =====


@_registry.cl_function('VECTOR')
def vector_fn(*objects):
    """Create a simple general vector of `objects` (CLHS 15.2.21)."""
    return list(objects)


# ===== ARRAY PLACES =====
#
# SETF, PSETF, INCF, DECF and ROTATEF each open-coded the AREF place, and
# every one of those copies read exactly one subscript -- so `(setf (aref a i
# j) v)` silently wrote element `i` of a rank-2 array -- and every one of them
# "helpfully" extended a Python list when the index was out of range, turning
# an error into a longer vector. One reader/writer pair for all of them.

_ARRAY_PLACE_OPERATORS = frozenset((
    'AREF', 'SVREF', 'BIT', 'SBIT', 'ROW-MAJOR-AREF', 'FILL-POINTER'))


def is_array_place(op_name):
    """True if `op_name` heads a place this module knows how to access."""
    return op_name in _ARRAY_PLACE_OPERATORS


def array_place_read(op_name, args):
    """Read the array place `(op_name . args)`, `args` already evaluated."""
    array, subscripts = args[0], args[1:]
    if op_name == 'FILL-POINTER':
        return fill_pointer(array)
    if op_name == 'ROW-MAJOR-AREF':
        return row_major_aref(array, subscripts[0])
    if op_name == 'SVREF':
        return svref(array, subscripts[0])
    if op_name in ('BIT', 'SBIT'):
        _require_bit_array(array, op_name)
    return aref(array, *subscripts)


def array_place_write(op_name, args, value):
    """Write `value` into the array place `(op_name . args)`."""
    array, subscripts = args[0], args[1:]
    if op_name == 'FILL-POINTER':
        return set_fill_pointer(array, value)
    if op_name == 'ROW-MAJOR-AREF':
        return row_major_aref_set(array, subscripts[0], value)
    if op_name == 'SVREF':
        svref(array, subscripts[0])  # the type check SVREF owes its argument
        return row_major_set(array, int(subscripts[0]), value)
    if op_name in ('BIT', 'SBIT'):
        _require_bit_array(array, op_name)
    return aref_set(array, subscripts, value)


__all__ = [
    # The operators, so that `from .arrays import *` gives a caller the same
    # names the registry binds.
    'make_array', 'aref', 'row_major_aref', 'array_row_major_index', 'svref',
    'bit_fn', 'sbit', 'array_rank', 'array_dimensions', 'array_dimension',
    'array_total_size', 'array_element_type', 'upgraded_array_element_type',
    'array_in_bounds_p', 'array_displacement', 'adjustable_array_p',
    'array_has_fill_pointer_p', 'fill_pointer', 'vector_push',
    'vector_push_extend', 'vector_pop', 'adjust_array', 'arrayp', 'vectorp',
    'simple_vector_p', 'bit_vector_p', 'simple_bit_vector_p', 'vector_fn',
    'bit_and', 'bit_ior', 'bit_xor', 'bit_eqv', 'bit_nand', 'bit_nor',
    'bit_andc1', 'bit_andc2', 'bit_orc1', 'bit_orc2', 'bit_not',
    # The model itself.
    'LispArray', 'T_TYPE', 'BIT_TYPE', 'CHARACTER_TYPE',
    'upgraded_element_type', 'default_element', 'coerce_element',
    'is_array', 'is_vector', 'is_bit_array', 'is_simple_array', 'is_adjustable',
    'array_rank_of', 'array_dimensions_of', 'array_total_size_of',
    'element_type_of', 'fill_pointer_of', 'displacement_of',
    'row_major_index', 'row_major_get', 'row_major_set', 'array_elements',
    'make_bit_vector', 'aref_set', 'row_major_aref_set', 'set_fill_pointer',
    'string_element', 'array_type_matches', 'is_array_type_name',
]
