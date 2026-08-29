"""Comparison and equality functions."""

from fractions import Fraction

import fclpy.lisptype as lisptype
from .core import atom, car, cdr, consp
from . import arrays as _arrays
from fclpy.lispfunc import registry as _registry


def _string_characters(obj):
    """Return a Lisp string's characters as a Python str, else None.

    A Lisp string in fclpy is a `LispString`, which is *not* a `str`
    subclass, while string literals from some paths and many internal
    producers are still plain Python `str` (plan.md Finding I). Every site
    that must decide "is this a string, and what is in it" therefore has to
    accept both, exactly as `STRINGP` already does.

    `EQUAL`/`EQUALP` tested only `isinstance(obj, str)`, so the branch was
    dead for every string the reader produces: `(equal "abc" "abc")`
    returned NIL. That is not a niche bug -- the ANSI harness compares each
    test's result against its expected value with `EQUAL`, so *every* test
    whose expected value is a string failed regardless of whether the code
    under test was correct.

    A third representation is a rank-1 array whose element type is a
    subtype of CHARACTER (CLAUDE.md's array model, `characters.is_string`'s
    definition of STRINGP) -- `arrays._new_array` uses this for a
    *displaced* character array, where `LispString`'s own fill-pointer
    support does not reach. Without this branch here, `(equalp
    displaced-string "foo")` was NIL for every one of `make-pathname.2a`'s
    16 special-string variants that happened to be displaced, because one
    side resolved to a string and the other did not, and this function's
    "exactly one side is a string" check treated that as categorically
    unequal rather than comparing the array's own active characters.
    """
    if isinstance(obj, lisptype.LispString):
        return str(obj)
    if isinstance(obj, str):
        return obj
    if _arrays.is_array(obj) and _arrays.array_rank_of(obj) == 1:
        element_type = _arrays.element_type_of(obj)
        # `:allow-nil-arrays` and `:nil-vectors-are-strings` are mutually
        # exclusive flags in ansi-test: some tests treat a rank-1 array
        # with element-type NIL as a vector (and demand `#()`-style
        # vector comparison), others treat it as a string (and demand
        # `""`-style string comparison). Both must pass. The path that
        # both share is the *empty* case -- an array with no elements
        # is equally well an empty vector and an empty string, and a
        # nil array of zero length is what both views have in common.
        # A non-empty nil array is not a string: a string is an array
        # of CHARACTERs and a CHARACTER is disjoint from NIL, so
        # requiring at least one element keeps the two views apart
        # while still serving the empty cases.
        if element_type is _arrays.CHARACTER_TYPE:
            chars = []
            for e in _arrays.array_elements(obj):
                if isinstance(e, lisptype.Character):
                    chars.append(e.char)
                elif isinstance(e, str) and len(e) == 1:
                    chars.append(e)
                else:
                    return None
            return ''.join(chars)
        if element_type is _arrays.NIL_TYPE:
            elements = _arrays.array_elements(obj)
            if not elements:
                return ''
            return None
    return None


@_registry.cl_function('EQ')
def eq(obj1, obj2):
    """Test for object identity."""
    return lisptype.lisp_bool(obj1 is obj2)


def _eql_number_key(x):
    """``(lisp-type, value)`` for a number, or None if `x` is not one.

    CLHS 5.3: two numbers are EQL when they are "of the same type and the same
    value". Both halves need care.

    **"Number" includes RATIO.** This used to read
    ``isinstance(x, (int, float, complex))``, and a Lisp ratio is a
    `fractions.Fraction` here, so it was not a number as far as EQL was
    concerned and *every* ratio fell through to NIL: `(eql 7/2 7/2)` and
    `(equal 7/2 7/2)` were both false. EQUAL delegates to EQL, EQL is the
    default hash-table test, and it is the default `:test` of MEMBER, ASSOC,
    FIND, POSITION, REMOVE, DELETE, SUBST and the rest of CLHS 14/17 -- so one
    missing type made all of them wrong about ratios.

    **"Same type" is the *Lisp* type, not the Python one.** A rational whose
    denominator is 1 *is* an integer (CLHS 12.1.1.2), so it keys as one:
    `_canonicalize_rational` normalizes such a `Fraction` back to `int` at the
    points that produce them, but keying by `type()` alone would make EQL the
    one place that could still disagree with the arithmetic about whether
    `(/ 4 2)` and `2` are the same object type.
    """
    if isinstance(x, bool):
        # Python's bool is a subclass of int; a Lisp boolean is not a number.
        return None
    if isinstance(x, int):
        return ('integer', x)
    if isinstance(x, Fraction):
        return ('integer', x.numerator) if x.denominator == 1 else ('ratio', x)
    if isinstance(x, float):
        return ('float', x)
    if isinstance(x, complex):
        return ('complex', x)
    return None


@_registry.cl_function('EQL')
def eql(obj1, obj2):
    """EQL (CLHS 5.3): EQ, plus numbers by type-and-value and characters by code."""
    if obj1 is obj2:
        return lisptype.T

    key1 = _eql_number_key(obj1)
    if key1 is not None:
        key2 = _eql_number_key(obj2)
        return lisptype.lisp_bool(key2 is not None and key1 == key2)


    # Characters are eql if they are the same character
    if isinstance(obj1, str) and isinstance(obj2, str) and len(obj1) == 1 and len(obj2) == 1:
        return lisptype.lisp_bool(obj1 == obj2)
    if isinstance(obj1, lisptype.Character) and isinstance(obj2, lisptype.Character):
        return lisptype.lisp_bool(obj1.char == obj2.char)
    if isinstance(obj1, lisptype.Character) and isinstance(obj2, str) and len(obj2) == 1:
        return lisptype.lisp_bool(obj1.char == obj2)
    if isinstance(obj2, lisptype.Character) and isinstance(obj1, str) and len(obj1) == 1:
        return lisptype.lisp_bool(obj2.char == obj1)

    return lisptype.NIL


@_registry.cl_function('EQUAL')
def equal(obj1, obj2):
    """EQUAL (CLHS 5.3): EQL, and structural descent into the four aggregates.

    **The cdr spine is walked iteratively, not recursed into.** A Lisp list is
    a chain of conses, so recursing on the cdr costs one Python frame per
    *element*: two 1000-element lists exhausted the default 1000-frame limit
    and `(equal vals vals2)` answered `RecursionError` -- a Python exception as
    the value of a Lisp form (standing rule 2) rather than T. A list's length
    is unbounded in a way its nesting depth is not, so only the `car` may
    recurse; `loop.13.8` compares exactly such a pair and is what exposed it.
    """
    while True:
        if eql(obj1, obj2) == lisptype.T:
            return lisptype.T

        # Cons cells: compare the cars (which may nest, so recurse) and then
        # continue along both cdrs in this frame.
        if consp(obj1) and consp(obj2):
            if equal(car(obj1), car(obj2)) != lisptype.T:
                return lisptype.NIL
            obj1, obj2 = cdr(obj1), cdr(obj2)
            continue
        break


    # Strings - CLHS 5.3: EQUAL compares strings element-wise and is
    # case-sensitive.
    s1 = _string_characters(obj1)
    s2 = _string_characters(obj2)
    if s1 is not None and s2 is not None:
        return lisptype.lisp_bool(s1 == s2)
    # A string is not EQUAL to a non-string, so stop here rather than
    # letting one fall through to the sequence branch below.
    if s1 is not None or s2 is not None:
        return lisptype.NIL


    # Bit vectors -- CLHS 5.3: EQUAL descends into conses, strings, bit
    # vectors and pathnames, and into nothing else. Only the active elements
    # count, so a fill pointer bounds the comparison. A bit vector is not
    # EQUAL to anything that is not one, general vectors included.
    bv1, bv2 = _arrays.is_bit_array(obj1), _arrays.is_bit_array(obj2)
    if bv1 or bv2:
        if not (bv1 and bv2) or obj1.rank != 1 or obj2.rank != 1:
            return lisptype.NIL
        return lisptype.lisp_bool(
            _arrays.array_elements(obj1) == _arrays.array_elements(obj2))

    # Pathnames -- the fourth and last aggregate CLHS 5.3 says EQUAL descends
    # into. `Pathname.__eq__` compares components (and is case-insensitive
    # where the file system is), so this is the one place that decides.
    from .pathnames import Pathname
    p1, p2 = isinstance(obj1, Pathname), isinstance(obj2, Pathname)
    if p1 or p2:
        return lisptype.lisp_bool(p1 and p2 and obj1 == obj2)

    # Lists and tuples -- CLHS 5.3: EQUAL descends into lists, not vectors.
    # Python `list` covers *both* here, so the explicit disambiguation is
    # required: a `#(...)` literal that comes back as a Python `list` is a
    # vector, not a list, and `(vector 1 2 3) = (vector 1 2 3)` is *not*
    # EQUAL (CLHS 5.3; `equal.4` pins this). The character/string check
    # above is what already separated a length-1 Python `str` from a
    # character, and `is_vector` here is the array side of the same line.
    if isinstance(obj1, (list, tuple)) and isinstance(obj2, (list, tuple)):
        from .sequence_protocol import is_vector
        if is_vector(obj1) or is_vector(obj2):
            return lisptype.NIL
        if len(obj1) != len(obj2):
            return lisptype.NIL
        for x, y in zip(obj1, obj2):
            if equal(x, y) != lisptype.T:
                return lisptype.NIL
        return lisptype.T

    return lisptype.NIL


@_registry.cl_function('EQUALP')
def equalp(obj1, obj2):
    """Test for liberal equality."""
    if equal(obj1, obj2) == lisptype.T:
        return lisptype.T

    # CLHS 5.3: EQUALP walks the spine of two conses the way EQUAL does, but
    # with EQUALP on the cars -- `(equalp '(#\a #\b) '(#\A #\B))` is T
    # because EQUALP compares characters case-insensitively, while EQUAL on
    # the cars returns NIL (case-sensitive). EQUALP's aggregate descent must
    # therefore mirror EQUAL's cdr-iteration; the loop below is a copy of
    # `equal`'s structure with the recursive call changed to `equalp`.
    if consp(obj1) and consp(obj2):
        while True:
            if equalp(car(obj1), car(obj2)) != lisptype.T:
                return lisptype.NIL
            obj1, obj2 = cdr(obj1), cdr(obj2)
            if not (consp(obj1) and consp(obj2)):
                break
        # Spines may have terminated together (both NIL) or one may still be a
        # cons; the latter is a length mismatch, which is not EQUALP.
        if consp(obj1) or consp(obj2):
            return lisptype.NIL
        # Fall through: the lists have the same length and per-element EQUALP
        # pairs. Check the atoms (NIL/NIL here) once more with the rest of the
        # EQUALP rules below.
        return equalp(obj1, obj2)

    # Numbers - allow type coercion
    if isinstance(obj1, (int, float, complex)) and isinstance(obj2, (int, float, complex)):
        return lisptype.lisp_bool(obj1 == obj2)
    
    # Characters - CLHS 5.3: EQUALP compares characters with CHAR-EQUAL,
    # which ignores case. EQUAL/EQL above are case-sensitive, so without
    # this branch (which the previous code lacked entirely for Character
    # objects) `(equalp #\a #\A)` was NIL.
    #
    # A character and a *string* are never EQUALP: a string is an array and
    # a character is not, so they are disjoint types even though this
    # implementation still represents some characters as length-1 Python
    # strings (plan.md C13).
    if isinstance(obj1, lisptype.Character) and isinstance(obj2, lisptype.Character):
        return lisptype.lisp_bool(obj1.char.upper() == obj2.char.upper())
    if isinstance(obj1, lisptype.Character) or isinstance(obj2, lisptype.Character):
        return lisptype.NIL

    # Vectors -- CLHS 5.3: two arrays are EQUALP if they have the same
    # dimensions and EQUALP elements. Which *Python* container holds those
    # elements is not part of the question, and testing `isinstance(x, list)`
    # made it part of the question: a `#(...)` literal is an
    # `AdjustableVector` while `(vector ...)` and every rebuilt sequence
    # result is a Python `list`, so the same vector built two ways was never
    # EQUALP to itself (plan.md Finding M). Try the vector branch *before*
    # the string check below so an empty array with element-type NIL
    # (which `_string_characters` would misclassify as the empty string)
    # and an empty `""` (a string, not a vector) each take the branch
    # their actual type names: the array reaches T via element-wise
    # comparison, the string via the string branch.
    from .sequence_protocol import is_vector, seq_elements
    if is_vector(obj1) and is_vector(obj2):
        left, right = seq_elements(obj1), seq_elements(obj2)
        if len(left) != len(right):
            return lisptype.NIL
        for x, y in zip(left, right):
            if equalp(x, y) != lisptype.T:
                return lisptype.NIL
        return lisptype.T
    # One side is a vector, the other is not -- an array and a string
    # are not EQUALP unless the string is a rank-1 character array (then
    # both go through `_string_characters` below). Bail out so the
    # asymmetric string check below can do the right thing.
    if is_vector(obj1) or is_vector(obj2):
        s1 = _string_characters(obj1)
        s2 = _string_characters(obj2)
        if s1 is not None and s2 is not None:
            return lisptype.lisp_bool(s1.upper() == s2.upper())
        return lisptype.NIL

    s1 = _string_characters(obj1)
    s2 = _string_characters(obj2)
    if s1 is not None and s2 is not None:
        return lisptype.lisp_bool(s1.upper() == s2.upper())
    if s1 is not None or s2 is not None:
        return lisptype.NIL


    # Hash tables -- CLHS 5.3: two hash tables are EQUALP if they have the
    # same test, the same number of entries, and for each key in one there is
    # a key in the other whose value is EQUALP.
    #
    # "A key in the other" is decided by *that table's own test*, not by
    # EQUALP, which is the whole subtlety and what `equalp.26`/`.28` measure:
    # two EQ tables holding `#\a` and `#\A` are not EQUALP, because EQ does
    # not equate those keys -- even though EQUALP itself would. So the lookup
    # goes through the table, the one thing that knows its test.
    from .misc_hashtables import is_hash_table
    h1, h2 = is_hash_table(obj1), is_hash_table(obj2)
    if h1 or h2:
        if not (h1 and h2):
            return lisptype.NIL
        if obj1.test != obj2.test or obj1.count() != obj2.count():
            return lisptype.NIL
        for key, value in obj1.entries():
            other, present = obj2.lookup(key)
            if not present or equalp(value, other) != lisptype.T:
                return lisptype.NIL
        return lisptype.T

    # Vectors -- CLHS 5.3: two arrays are EQUALP if they have the same
    # dimensions and EQUALP elements. Which *Python* container holds those
    # elements is not part of the question, and testing `isinstance(x, list)`
    # made it part of the question: a `#(...)` literal is an
    # `AdjustableVector` while `(vector ...)` and every rebuilt sequence
    # result is a Python `list`, so the same vector built two ways was never
    # EQUALP to itself (plan.md Finding M).
    from .sequence_protocol import is_vector, seq_elements
    if is_vector(obj1) and is_vector(obj2):
        left, right = seq_elements(obj1), seq_elements(obj2)
        if len(left) != len(right):
            return lisptype.NIL
        for x, y in zip(left, right):
            if equalp(x, y) != lisptype.T:
                return lisptype.NIL
        return lisptype.T

    return lisptype.NIL


@_registry.cl_function('NOT')
def not_fn(obj):
    """Logical NOT."""
    if obj is None or obj == lisptype.NIL:
        return lisptype.T
    else:
        return lisptype.NIL


@_registry.cl_function('NULL')
def null(obj):
    """Test for null/nil."""
    if obj is None or obj == lisptype.NIL:
        return lisptype.T
    else:
        return lisptype.NIL


@_registry.cl_function('TYPEP')
def typep(object, type_specifier):
    """Test if object is of given type."""
    # Import classes here to avoid circular dependencies
    from fclpy import classes
    from fractions import Fraction
    from .core import _consp_internal
    
    # The fixnum boundary, from its one home. This used to be a local
    # `2**29 - 1`, a second copy of a constant `typespec.py` already owns --
    # and the two disagreed by 34 bits, so `(typep most-positive-fixnum
    # 'fixnum)` was NIL and `(typep 1000000000 'bignum)` was T while SUBTYPEP
    # correctly answered that `(integer 0 1000000000)` is a subtype of FIXNUM.
    # TYPEP contradicting SUBTYPEP about the same integer is standing rule 3;
    # `typespec.py`'s own header records this constant being consolidated, and
    # this was the copy it missed.
    from fclpy.typespec import MOST_POSITIVE_FIXNUM as FIXNUM_MAX
    from fclpy.typespec import MOST_NEGATIVE_FIXNUM as FIXNUM_MIN

    # Helper to convert list to Python list for iteration
    def list_to_pylist(lst):
        """Convert a Lisp list to a Python list."""
        result = []
        current = lst
        while _consp_internal(current):
            result.append(car(current))
            current = cdr(current)
        return result
    
    # Handle compound type specifiers (lists like (or ...), (and ...), (not ...), etc.)
    if _consp_internal(type_specifier):
        first = car(type_specifier)
        if hasattr(first, 'name'):
            compound_type = first.name.upper()
        elif isinstance(first, str):
            compound_type = first.upper()
        else:
            compound_type = str(first).upper()
        
        rest = list_to_pylist(cdr(type_specifier))
        
        if compound_type == 'OR':
            # (OR type1 type2 ...) - true if object matches any type
            for sub_type in rest:
                if typep(object, sub_type) == lisptype.T:
                    return lisptype.T
            return lisptype.NIL
        
        elif compound_type == 'AND':
            # (AND type1 type2 ...) - true if object matches all types
            for sub_type in rest:
                if typep(object, sub_type) == lisptype.NIL:
                    return lisptype.NIL
            return lisptype.T
        
        elif compound_type == 'NOT':
            # (NOT type) - true if object doesn't match type
            if len(rest) >= 1:
                if typep(object, rest[0]) == lisptype.T:
                    return lisptype.NIL
                return lisptype.T
            return lisptype.NIL
        
        elif compound_type == 'MEMBER':
            # (MEMBER item1 item2 ...) - true if object is EQL to any item
            for item in rest:
                if eql(object, item) == lisptype.T:
                    return lisptype.T
            return lisptype.NIL
        
        elif compound_type == 'EQL':
            # (EQL item) - true if object is EQL to item
            if len(rest) >= 1:
                return eql(object, rest[0])
            return lisptype.NIL
        
        elif compound_type == 'SATISFIES':
            # (SATISFIES predicate) - true if (predicate object) is true
            # This requires evaluation - for now return NIL as we can't easily call arbitrary functions
            return lisptype.NIL
        
        elif compound_type == 'INTEGER':
            # (INTEGER [low [high]]) - integer in range
            if not isinstance(object, int):
                return lisptype.NIL
            low = rest[0] if len(rest) > 0 else None
            high = rest[1] if len(rest) > 1 else None
            # Handle * meaning unbounded
            if low is not None and not (hasattr(low, 'name') and low.name == '*'):
                # Handle (low) meaning exclusive
                if _consp_internal(low):
                    if object <= car(low):
                        return lisptype.NIL
                elif object < low:
                    return lisptype.NIL
            if high is not None and not (hasattr(high, 'name') and high.name == '*'):
                # Handle (high) meaning exclusive
                if _consp_internal(high):
                    if object >= car(high):
                        return lisptype.NIL
                elif object > high:
                    return lisptype.NIL
            return lisptype.T
        
        elif compound_type in ('FLOAT', 'SINGLE-FLOAT', 'DOUBLE-FLOAT', 'SHORT-FLOAT', 'LONG-FLOAT'):
            # (FLOAT [low [high]]) - float in range
            if not isinstance(object, float):
                return lisptype.NIL
            low = rest[0] if len(rest) > 0 else None
            high = rest[1] if len(rest) > 1 else None
            if low is not None and not (hasattr(low, 'name') and low.name == '*'):
                if _consp_internal(low):
                    if object <= car(low):
                        return lisptype.NIL
                elif object < low:
                    return lisptype.NIL
            if high is not None and not (hasattr(high, 'name') and high.name == '*'):
                if _consp_internal(high):
                    if object >= car(high):
                        return lisptype.NIL
                elif object > high:
                    return lisptype.NIL
            return lisptype.T
        
        elif compound_type == 'REAL':
            # (REAL [low [high]]) - real number in range
            if not isinstance(object, (int, float, Fraction)):
                return lisptype.NIL
            low = rest[0] if len(rest) > 0 else None
            high = rest[1] if len(rest) > 1 else None
            if low is not None and not (hasattr(low, 'name') and low.name == '*'):
                if _consp_internal(low):
                    if object <= car(low):
                        return lisptype.NIL
                elif object < low:
                    return lisptype.NIL
            if high is not None and not (hasattr(high, 'name') and high.name == '*'):
                if _consp_internal(high):
                    if object >= car(high):
                        return lisptype.NIL
                elif object > high:
                    return lisptype.NIL
            return lisptype.T
        
        elif compound_type == 'RATIONAL':
            # (RATIONAL [low [high]]) - rational in range
            if not isinstance(object, (int, Fraction)):
                return lisptype.NIL
            low = rest[0] if len(rest) > 0 else None
            high = rest[1] if len(rest) > 1 else None
            if low is not None and not (hasattr(low, 'name') and low.name == '*'):
                if _consp_internal(low):
                    if object <= car(low):
                        return lisptype.NIL
                elif object < low:
                    return lisptype.NIL
            if high is not None and not (hasattr(high, 'name') and high.name == '*'):
                if _consp_internal(high):
                    if object >= car(high):
                        return lisptype.NIL
                elif object > high:
                    return lisptype.NIL
            return lisptype.T
        
        elif compound_type in ('MOD', 'UNSIGNED-BYTE', 'SIGNED-BYTE'):
            # (MOD n) = (INTEGER 0 (n)), (UNSIGNED-BYTE n) = integers 0 to 2^n-1
            if not isinstance(object, int):
                return lisptype.NIL
            # A size of `*`, or none at all, means unbounded (CLHS 12.1.2):
            # `(unsigned-byte)` and `(unsigned-byte *)` are both the atomic
            # `unsigned-byte`. Defaulting to 8 instead made
            # `(typep 300 '(unsigned-byte))` false, and `2 ** <the * symbol>`
            # raised a Python TypeError as the value of the form.
            size = rest[0] if len(rest) > 0 else None
            if size is not None and hasattr(size, 'name') and size.name == '*':
                size = None
            if compound_type == 'MOD':
                # (MOD n) requires its n; `*` is not permitted here.
                n = size if size is not None else 1
                return lisptype.lisp_bool(0 <= object < n)
            elif compound_type == 'UNSIGNED-BYTE':
                if size is None:
                    return lisptype.lisp_bool(object >= 0)
                return lisptype.lisp_bool(0 <= object < (2 ** size))
            elif compound_type == 'SIGNED-BYTE':
                if size is None:
                    return lisptype.T
                limit = 2 ** (size - 1)
                return lisptype.lisp_bool(-limit <= object < limit)
        
        elif _arrays.is_array_type_name(compound_type):
            # Every compound array specifier -- (array et dims), (vector et
            # size), (string size), (simple-bit-vector size) -- is one
            # question about the object's element type and dimensions, and
            # the array model is what can answer it. The branches this
            # replaced ignored both: they tested only that the object was one
            # of two Python container types, so `(typep #(1 2) 'bit-vector)`
            # and `(typep #(1 2) '(array t (5)))` were T.
            return lisptype.lisp_bool(
                _arrays.array_type_matches(object, compound_type, rest))

        elif compound_type == 'CONS':
            # (CONS [car-type [cdr-type]]) - cons with specific types
            if not _consp_internal(object):
                return lisptype.NIL
            # With no subtypes, just check it's a cons
            if len(rest) == 0:
                return lisptype.T
            car_type = rest[0] if len(rest) > 0 else None
            cdr_type = rest[1] if len(rest) > 1 else None
            if car_type is not None and not (hasattr(car_type, 'name') and car_type.name == '*'):
                if typep(car(object), car_type) == lisptype.NIL:
                    return lisptype.NIL
            if cdr_type is not None and not (hasattr(cdr_type, 'name') and cdr_type.name == '*'):
                if typep(cdr(object), cdr_type) == lisptype.NIL:
                    return lisptype.NIL
            return lisptype.T
        
        else:
            # Unknown compound type - check if it might be a simple type name followed by parameters
            # Just try the simple type name
            return typep(object, first)
    
    # A condition class (built-in or DEFINE-CONDITION-created) as a type
    # specifier: FIND-CLASS returns the raw Python class for one of these,
    # not a CLOS `LispClass` (see `classes.find_class_fn`), so without this
    # branch it fell through to the generic "no such class" NIL at the
    # bottom -- `(typep condition-instance (find-class 'my-condition-type))`
    # always failed regardless of the actual class relationship.
    if isinstance(type_specifier, type) and issubclass(type_specifier, lisptype.Condition):
        return lisptype.lisp_bool(isinstance(object, type_specifier))

    # Handle LispClass type specifiers (user-defined classes)
    if isinstance(type_specifier, classes.LispClass):
        # Check if object is an instance of this class
        if isinstance(object, classes.LispInstance):
            # Check class hierarchy
            for cls in object.lisp_class.get_linearized_superclasses():
                if cls is type_specifier:
                    return lisptype.T
            return lisptype.NIL
        # Not a CLOS instance -- the class may still name a built-in type
        # (e.g. #<STANDARD-CLASS SYMBOL>), so fall back to checking the
        # class's name as an ordinary type-specifier symbol.
        return typep(object, type_specifier.name)

    # A class *object* itself as the object being tested, e.g.
    # `(typep (find-class 'foo) 'structure-class)`
    # (structures/structure-00.lsp test 14) -- a question about which
    # metaobject class `object` is an instance of, not about `object`'s own
    # instances, so it is answered separately from the LispClass-as-type_specifier
    # branch above.
    if isinstance(object, classes.LispClass):
        obj_type_name = (type_specifier.upper() if isinstance(type_specifier, str)
                          else type_specifier.name.upper() if hasattr(type_specifier, 'name')
                          else str(type_specifier).upper())
        if obj_type_name == 'T':
            return lisptype.T
        from fclpy import typespec
        if obj_type_name in typespec._BUILTIN_CLASS_NAMES:
            return lisptype.lisp_bool(
                getattr(object, 'metaclass_name', 'STANDARD-CLASS') in typespec._class_cone(obj_type_name))
        return lisptype.NIL

    # Handle string or symbol type specifiers
    if isinstance(type_specifier, str):
        type_name = type_specifier.upper()
    elif hasattr(type_specifier, 'name'):
        type_name = type_specifier.name.upper()
    else:
        type_name = str(type_specifier).upper()

    # Conditions (ERROR, SIMPLE-ERROR, WARNING, CONDITION, ...) are plain
    # Python classes in lisptype_extended.py, mirroring the ANSI condition
    # hierarchy via ordinary Python inheritance (SimpleError -> Error ->
    # Condition) -- not CLOS classes, so the LispClass/find_class branches
    # above and below never see them. Previously TYPEP had no branch for
    # them at all, so e.g. (typep c 'simple-error) on a real SimpleError
    # instance fell through to the "no such class" NIL at the bottom: every
    # condition-type-dispatching test (HANDLER-CASE clauses tested via
    # TYPEP, FROB-SIMPLE-ERROR in the ANSI suite's own test helpers, ...)
    # silently took the wrong branch. isinstance() against the mapped class
    # gives correct subtype behavior for free, the same lattice-for-free
    # Finding E calls for in the handler-matching code.
    if isinstance(object, lisptype.Condition):
        if type_name in ('T', 'CONDITION'):
            return lisptype.T
        from fclpy.lispfunc.evaluation_conditions import _condition_class_for_name
        condition_class = _condition_class_for_name(type_name)
        if isinstance(condition_class, type):
            return lisptype.lisp_bool(isinstance(object, condition_class))
        # A non-condition-type specifier (ATOM, STANDARD-OBJECT, TYPES.9A's
        # full cross-product of CL type names): a condition instance is a
        # standard-object, an atom, a t, and every other type the type
        # lattice puts above it. Returning NIL here would silently tell
        # ansi-test's TYPES.9A that no condition is an atom, and the same
        # for the ~70 other names the test iterates over. Fall through to
        # the type-name branches below so a condition gets the same answer
        # `(type-of cond)` would for any other object of the same shape --
        # which is what makes TYPES.9A's "if (subtypep T1 T2) then
        # (typep x T1) implies (typep x T2)" hold.

    # Check for built-in types
    if type_name == 'T':
        return lisptype.T
    elif type_name == 'NULL':
        return null(object)
    elif type_name == 'ATOM':
        return atom(object)
    elif type_name == 'CONS':
        return consp(object)
    elif type_name == 'LIST':
        return lisptype.lisp_bool(null(object) == lisptype.T or consp(object) == lisptype.T)
    elif type_name == 'NUMBER':
        return lisptype.lisp_bool(isinstance(object, (int, float, complex, Fraction)))
    elif type_name == 'INTEGER':
        return lisptype.lisp_bool(isinstance(object, int))
    elif type_name == 'BIT':
        return lisptype.lisp_bool(isinstance(object, int) and object in (0, 1))
    elif type_name == 'UNSIGNED-BYTE':
        # CLHS 12.1.2/4.4: the *atomic* specifier `unsigned-byte` is
        # `(integer 0 *)` -- any non-negative integer -- and `signed-byte` is
        # `integer`. Only the compound `(unsigned-byte n)` had a branch (see
        # below), so the bare symbol fell through to NIL: `(typep 5
        # 'unsigned-byte)` was false.
        #
        # That is not a cosmetic gap. ansi-test's `check-type-error` takes a
        # *guard* predicate and calls the function under test on every element
        # of `*mini-universe*` the guard rejects, expecting a TYPE-ERROR. With
        # this guard answering NIL for everything, MAKE-LIST.ERROR.1 handed
        # `(make-list 10000000000000000000000)` to a MAKE-LIST that builds its
        # result one cons at a time -- 27GB and a wedged full run.
        # `bool` is excluded because it is an `int` subclass in Python, so a
        # stray Python True would otherwise type as the integer 1.
        return lisptype.lisp_bool(
            isinstance(object, int) and not isinstance(object, bool) and object >= 0)
    elif type_name == 'SIGNED-BYTE':
        return lisptype.lisp_bool(
            isinstance(object, int) and not isinstance(object, bool))
    elif type_name == 'FIXNUM':
        # Fixnum: integers within machine word range
        return lisptype.lisp_bool(isinstance(object, int) and FIXNUM_MIN <= object <= FIXNUM_MAX)
    elif type_name == 'BIGNUM':
        # Bignum: integers outside fixnum range
        return lisptype.lisp_bool(isinstance(object, int) and (object < FIXNUM_MIN or object > FIXNUM_MAX))
    elif type_name in ('FLOAT', 'SHORT-FLOAT', 'SINGLE-FLOAT', 'DOUBLE-FLOAT', 'LONG-FLOAT'):
        # Every CL float subtype is the same Python `float` here (no distinct
        # short/single/double/long representations), same as the compound
        # `(single-float ...)`/`(short-float ...)` branch above -- this is
        # that branch's atomic-specifier twin and must agree with it, or
        # `(typep x 'short-float)` and `(typep x '(short-float * *))` answer
        # differently for the same `x`.
        return lisptype.lisp_bool(isinstance(object, float))
    elif type_name == 'COMPLEX':
        return lisptype.lisp_bool(isinstance(object, complex))
    elif type_name == 'REAL':
        return lisptype.lisp_bool(isinstance(object, (int, float, Fraction)))
    elif type_name == 'RATIONAL':
        return lisptype.lisp_bool(isinstance(object, (int, Fraction)))
    elif type_name == 'RATIO':
        return lisptype.lisp_bool(isinstance(object, Fraction))
    elif type_name == 'CHARACTER':
        return lisptype.lisp_bool(isinstance(object, lisptype.Character) or (isinstance(object, str) and len(object) == 1))
    elif type_name == 'SYMBOL':
        # One predicate, shared with SYMBOLP (CLHS 4.2): a LispSymbol, a
        # lispKeyword, or NIL in any of its three Python spellings.
        return lisptype.lisp_bool(lisptype.is_symbol(object))
    elif type_name == 'KEYWORD':
        return lisptype.lisp_bool(lisptype.is_keyword(object))
    elif type_name == 'FUNCTION':
        return lisptype.lisp_bool(callable(object))
    elif type_name in ('GENERIC-FUNCTION', 'STANDARD-GENERIC-FUNCTION'):
        # A DEFINE-CONDITION :READER is marked as a generic function (CLHS
        # 9.4) at the point it's built -- see evaluation_conditions.py's
        # `_make_condition_reader` -- because this codebase's CLOS
        # `GenericFunction` (fclpy/classes.py) is not wired into FUNCALL/APPLY
        # at all (plan.md Finding L) and so cannot serve as a real accessor.
        # There is exactly one generic-function metaclass here, so every
        # `GenericFunction` answers both the generic name and its standard
        # subtype -- a reader-generic, which is never a real GenericFunction
        # object, only ever answers the generic name.
        return lisptype.lisp_bool(
            isinstance(object, classes.GenericFunction)
            or (type_name == 'GENERIC-FUNCTION' and callable(object)
                and getattr(object, '_condition_reader_generic', False)))
    elif type_name in ('METHOD', 'STANDARD-METHOD'):
        # Same one-metaclass reasoning as GENERIC-FUNCTION/STANDARD-
        # GENERIC-FUNCTION above: a `classes.Method` is always a standard
        # method here, so both names answer T for it. Before this, DEFMETHOD
        # tests like `(typep (eval '(defmethod ...)) 'standard-method)` were
        # NIL no matter what DEFMETHOD returned, because TYPEP had no branch
        # for a Method object at all and fell through to the final NIL.
        return lisptype.lisp_bool(isinstance(object, classes.Method))
    elif type_name == 'STANDARD-OBJECT' or type_name == 'INSTANCE':
        # CLHS 4.3.7/7.1.2: the standardized STANDARD-OBJECT subclasses
        # include condition, class, method, generic-function, structure-object
        # (CLHS figure 4.3) -- so a condition instance IS a standard-object,
        # which the narrow `LispInstance` check silently answered NIL for,
        # breaking ansi-test's TYPES.9A for every supertype query naming
        # STANDARD-OBJECT, INSTANCE, METHOD, STANDARD-METHOD, ...
        # Built-in classes like NUMBER and CHARACTER are NOT standard-object
        # subclasses (CLHS 4.2), and this check correctly still answers NIL
        # for them. The typespec lattice already classifies conditions as
        # standard-objects; this brings the TYPEP ladder in line with it.
        return lisptype.lisp_bool(
            isinstance(object, (classes.LispInstance, lisptype.Condition)))
    elif _arrays.is_array_type_name(type_name):
        # CLHS 15.1: a string *is* a vector and every vector is an array, so
        # these cannot be separate `isinstance` tests; they are one question
        # about the object's rank, element type and simplicity, which the
        # array model answers. (Excluding strings from VECTOR is what once
        # stopped the ANSI harness's own `equalp-with-case` from comparing
        # two strings element-wise, so every string-valued test failed no
        # matter what the code under test returned.)
        return lisptype.lisp_bool(_arrays.array_type_matches(object, type_name))
    elif type_name == 'SEQUENCE':
        # CLHS 4.2: SEQUENCE is the union of LIST and VECTOR. TYPEP had no
        # branch for it at all, so `(typep '(1 2) 'sequence)` was NIL -- and
        # since ansi-test guards every `check-type-error` with `sequencep`,
        # that made those tests demand a TYPE-ERROR from *every* argument,
        # lists and vectors included. Answered by the sequence protocol's own
        # predicate so that TYPEP and `seq_elements` cannot disagree about
        # what a sequence is.
        from .sequence_protocol import is_sequence
        return lisptype.lisp_bool(is_sequence(object))
    elif type_name == 'RESTART':
        # TYPEP had no branch for RESTART, so it fell through to the CLOS
        # `find_class` branch below (which requires a `classes.LispInstance`)
        # and answered NIL for every real `lisptype.Restart` -- failing
        # compute-restarts.1/.2's `(typep r 'restart)`/`(typep r (find-class
        # 'restart))` regardless of what COMPUTE-RESTARTS actually returned.
        return lisptype.lisp_bool(isinstance(object, lisptype.Restart))
    elif type_name == 'HASH-TABLE':
        # Asked of the same object model HASH-TABLE-P answers for, so the
        # predicate and the type specifier cannot disagree. They did: this
        # branch tested `isinstance(object, dict)` while the live
        # `HASH-TABLE-P` tested a dead class, so `(typep ht 'hash-table)` was
        # T and `(hash-table-p ht)` was NIL for the same object.
        from .misc_hashtables import is_hash_table
        return lisptype.lisp_bool(is_hash_table(object))
    elif type_name == 'RANDOM-STATE':
        from .utilities_system import RandomState
        return lisptype.lisp_bool(isinstance(object, RandomState))
    elif type_name == 'READTABLE':
        # Asked of the same object model READTABLEP answers for, so the
        # predicate and the type specifier cannot disagree.
        from fclpy.readtable import Readtable
        return lisptype.lisp_bool(isinstance(object, Readtable))
    elif type_name == 'PATHNAME':
        # TYPEP had no branch for PATHNAME at all, so it fell through to the
        # CLOS `find_class` branch below (which requires a
        # `classes.LispInstance`), and `(typep p 'pathname)` was NIL for
        # every real pathname -- failing every `make-pathname-test` in
        # pathnames/make-pathname.lsp regardless of the pathname's actual
        # components.
        from .pathnames import Pathname
        return lisptype.lisp_bool(isinstance(object, Pathname))
    elif type_name == 'LOGICAL-PATHNAME':
        from .pathnames import Pathname
        return lisptype.lisp_bool(isinstance(object, Pathname) and object.logical)
    elif type_name == 'PACKAGE':
        # Asked of the same object model PACKAGEP answers for, so the
        # predicate and the type specifier cannot disagree. Without this
        # branch TYPEP fell through to the CLOS `find_class` lookup
        # below, which requires a `classes.LispInstance` -- a `Package`
        # is never one, so `(typep p 'package)` was NIL even though
        # `(packagep p)` was T, and `check-type-predicate` for PACKAGEP
        # collected that as a mismatch on every Package object in the
        # universe.
        return lisptype.lisp_bool(isinstance(object, lisptype.Package))
    elif type_name in ('STREAM', 'TWO-WAY-STREAM', 'ECHO-STREAM',
                       'CONCATENATED-STREAM', 'BROADCAST-STREAM',
                       'SYNONYM-STREAM', 'STRING-STREAM', 'FILE-STREAM'):
        # TYPEP had no branch for STREAM or any of its subtypes at all, so it
        # fell through to the CLOS `find_class` branch below, which requires
        # a `classes.LispInstance` -- a `streams.Stream` is never one, so
        # `(typep s 'stream)` was NIL for every real stream. Asked of the same
        # object model STREAMP answers for (`streams.stream_type_matches`), so
        # the predicate and every one of these type specifiers agree.
        from .streams import stream_type_matches
        return lisptype.lisp_bool(stream_type_matches(object, type_name))
    elif type_name == 'BOOLEAN':
        # In Common Lisp, BOOLEAN is equivalent to (OR NULL (EQL T)) -- only
        # NIL and T are booleans, by identity (CLHS 4.2). fclpy's NIL has
        # three Python spellings (None, the lisptype.NIL singleton, and the
        # interned LispSymbol whose value is the empty list), and the
        # symbol-NIL is the same Lisp object as the value-NIL via EQ. The
        # earlier `object is lisptype.NIL` test (Python identity) was too
        # narrow: a quoted `'NIL` from the suite's `(loop for x in
        # *universe*)` is the symbol itself, not the singleton, and silently
        # failed BOOLEAN-TYPE.3. Match `null`'s own notion of "null"
        # (`object == lisptype.NIL`) so the predicate and the type specifier
        # agree about what NIL is -- otherwise `(is-t-or-nil 'nil)` and
        # `(typep 'nil 'boolean)` would answer differently for the same
        # value, which is exactly the test's invariant.
        return lisptype.lisp_bool(
            null(object) == lisptype.T or object is lisptype.T)
    else:
        # Try to find a user-defined class with this name
        try:
            cls = classes.find_class(type_name)
            if cls and isinstance(object, classes.LispInstance):
                # Check if object is instance of this class
                for c in object.lisp_class.get_linearized_superclasses():
                    if c is cls:
                        return lisptype.T
        except Exception:
            pass
        
        return lisptype.NIL


@_registry.cl_function('TYPE-OF')
def type_of(object):
    """Return type of object."""
    from fclpy import classes

    # Check for user-defined instances first
    if isinstance(object, classes.LispInstance):
        return object.lisp_class.name

    # An array that records an element type, a rank or a fill pointer has a
    # *compound* type (CLHS 4.2.3): `(simple-array bit (5))`, not the bare
    # symbol SIMPLE-VECTOR that every array shape used to answer.
    if isinstance(object, _arrays.LispArray):
        from .sequence_protocol import make_lisp_list
        simple = _arrays.is_simple_array(object)
        dimensions = _arrays.array_dimensions_of(object)
        # The type name must be the *interned* CL symbol: a fresh LispSymbol
        # of the same name is a different object, prints as `#:SIMPLE-ARRAY`
        # and is not EQ to the symbol a caller compares against.
        intern = lisptype.COMMON_LISP_PACKAGE.intern
        if object.element_type is _arrays.BIT_TYPE and object.rank == 1:
            name = 'SIMPLE-BIT-VECTOR' if simple else 'BIT-VECTOR'
            return make_lisp_list([intern(name), dimensions[0]])
        name = 'SIMPLE-ARRAY' if simple else 'ARRAY'
        return make_lisp_list([intern(name), object.element_type,
                               make_lisp_list(dimensions)])
    
    # null() and consp() return Lisp T/NIL objects, compare against lisptype.T
    if null(object) == lisptype.T:
        return lisptype.LispSymbol('NULL')
    elif consp(object) == lisptype.T:
        return lisptype.LispSymbol('CONS')
    elif isinstance(object, lisptype.lispKeyword):
        return lisptype.LispSymbol('KEYWORD')
    elif isinstance(object, lisptype.LispSymbol):
        return lisptype.LispSymbol('SYMBOL')
    elif isinstance(object, lisptype.Character):
        return lisptype.LispSymbol('CHARACTER')
    elif isinstance(object, int):
        # Common Lisp often returns very specific integer types for small integers
        # e.g. 0 or 1 may be represented as BIT in some implementations. Return
        # a more specific type when possible.
        try:
            val = int(object)
        except Exception:
            return lisptype.LispSymbol('INTEGER')
        if val in (0, 1):
            return lisptype.LispSymbol('BIT')
        return lisptype.LispSymbol('INTEGER')
    elif isinstance(object, float):
        return lisptype.LispSymbol('SINGLE-FLOAT')
    elif isinstance(object, complex):
        return lisptype.LispSymbol('COMPLEX')
    elif isinstance(object, str):
        if len(object) == 1:
            return lisptype.LispSymbol('CHARACTER')
        else:
            return lisptype.LispSymbol('STRING')
    elif isinstance(object, (list, tuple)):
        return lisptype.LispSymbol('VECTOR')
    elif isinstance(object, classes.Method):
        # Checked ahead of the general `callable(object)` branch below: a
        # Method is not itself callable (only CALL-METHOD/standard
        # combination invoke `.function`), but even if it were, "FUNCTION"
        # would be the wrong answer -- CLHS 7.6.6.2's method object is a
        # STANDARD-METHOD, disjoint from FUNCTION.
        return lisptype.LispSymbol('STANDARD-METHOD')
    elif isinstance(object, classes.GenericFunction):
        # Checked ahead of `callable(object)` for the same reason: a
        # GenericFunction *is* callable (CLHS 7.6.6 dispatch), so the
        # generic branch would otherwise answer the wrong, less-specific
        # "FUNCTION" for it.
        return lisptype.LispSymbol('STANDARD-GENERIC-FUNCTION')
    elif isinstance(object, lisptype.Package):
        # `Package` is its own CLHS 4.2.3 type: `(type-of p)` must be
        # `PACKAGE` (then `CLASS-OF` looks it up in the registry, finding
        # the standard-class installed by `_init_builtin_classes`) rather
        # than falling through to `T`. Without this branch every Package
        # object answered `(class-of p) => T, t` -- so `(typep p 'package)`
        # was NIL even though `(packagep p)` was T, and the harness's
        # `check-type-predicate` collected that contradiction as a
        # mismatch.
        return lisptype.LispSymbol('PACKAGE')
    elif callable(object):
        return lisptype.LispSymbol('FUNCTION')
    else:
        return lisptype.T


@_registry.cl_function('SUBTYPEP')
def subtypep(*args):
    """Test if type1 is a subtype of type2 (CLHS 4.3.4): two values, whether
    type1 is known to be a subtype and whether that was determined for certain.

    Decided by `fclpy.typespec`, the one type-specifier model, as emptiness of
    `type1 \\ type2`. What this replaced was a table of hardcoded *string
    pairs* -- `if t1 == 'INTEGER' and t2 in ['RATIONAL','REAL','NUMBER']` -- with
    no entry for any compound specifier at all, so `(subtypep '(integer 0 10)
    'integer)` and `(subtypep 'fixnum 'integer)` both answered NIL, and it
    answered `NIL, T` ("certainly not a subtype") for every relationship it had
    no row for. That last part is why the table could not simply be grown: a
    lookup miss is indistinguishable from a real negative, so the wrong answers
    were reported as certain ones.

    The `MultipleValues` wrapper stays at this one boundary: `type_subtypep`
    answers a plain Python pair, and a bare Python tuple is a *single* value to
    MULTIPLE-VALUE-LIST, which is what once made
    `(multiple-value-list (subtypep 'x 'y))` read back `#(T T)` instead of the
    two-element list ANSI requires.
    """
    # SUBTYPEP takes two required arguments and an optional environment
    # (CLHS 4.3.4); anything else is a PROGRAM-ERROR, which ansi-test's
    # subtypep.error.1/.2/.3 check for 0, 1 and 4 arguments. Registered as
    # `*args` because a `cl_function` with a fixed signature raises a Python
    # TypeError instead -- standing rule 2, a Python exception as a Lisp value.
    if not 2 <= len(args) <= 3:
        raise lisptype.LispProgramError(
            'SUBTYPEP requires two type specifiers and an optional environment, '
            'got %d arguments' % (len(args),))
    type1, type2 = args[0], args[1]
    environment = args[2] if len(args) > 2 else None
    if _is_nil_designator(environment):
        environment = None
    from fclpy import typespec
    sub, certain = typespec.type_subtypep(type1, type2, environment)
    return lisptype.MultipleValues(lisptype.lisp_bool(sub),
                                   lisptype.lisp_bool(certain))


def _is_nil_designator(obj):
    """NIL, in any of the three shapes it takes here (see CLAUDE.md)."""
    return (obj is None or obj is lisptype.NIL
            or isinstance(obj, lisptype.lispNull)
            or (isinstance(obj, lisptype.LispSymbol) and obj.name.upper() == 'NIL'))


@_registry.cl_function('IDENTITY')
def identity(object):
    """Return the object unchanged."""
    return object


@_registry.cl_function('CONSTANTP')
def constantp(form, environment=None):
    """CONSTANTP (CLHS 3.1.2.1) -- is `form` a constant form?

    CLHS gives exactly three kinds of constant form, and this is written as
    those three rather than as a list of types:

    - a **self-evaluating object** -- which is *anything that is not a symbol
      and not a cons*, so there is nothing to enumerate. The previous version
      enumerated (`int`, `float`, `str`, `bool`, keyword, NIL) and therefore
      answered NIL for a character, a ratio, a complex, a `LispString`, an
      array, a hash table, a pathname and a structure instance. `constantp.1`
      asks the question the other way round -- every object in `*universe*`
      that is neither a symbol nor a cons must be CONSTANTP -- which is why an
      enumeration cannot pass it however long the list gets;
    - a **constant variable**: T, NIL, any keyword, and any name DEFCONSTANT
      has proclaimed. That last group is what `binding.is_constant_variable`
      answers, and until it existed `(constantp 'pi)` was NIL;
    - a **QUOTE form**.

    `environment` is accepted (a `&optional` lexical environment, so a
    supplied NIL means the null environment) but a lexical environment cannot
    make a name constant, so it does not enter the decision.
    """
    from .binding import is_constant_variable

    if lisptype.is_symbol(form):
        if null(form) or form is lisptype.T or form is True:
            return lisptype.T
        if lisptype.is_keyword(form):
            return lisptype.T
        # A symbol whose name happens to be T in some other package is not the
        # constant T; identity is what matters, and `is_symbol` already
        # accepted NIL in all three of its Python spellings above.
        return lisptype.lisp_bool(is_constant_variable(form))
    if consp(form):
        head = car(form)
        return lisptype.lisp_bool(
            isinstance(head, lisptype.LispSymbol) and head.name == 'QUOTE')
    # Self-evaluating.
    return lisptype.T


@_registry.cl_function('COMPLEMENT')
def complement(function):
    """Return complement of function."""
    def complemented_function(*args, **kwargs):
        # `function` is called directly here, not through `eval`'s CALL
        # path, so its raw Python return value never passes through that
        # path's `bool` -> T/NIL normalization -- and a predicate like
        # STRING= returns a plain Python `bool`. `not_fn` only recognizes
        # NIL/None as false, so an un-normalized `False` reads as *true* to
        # it (CLAUDE.md's `is_truthy(False)` landmine, from the opposite
        # direction): `(complement #'string=)` complemented nothing.
        # `lisp_bool` is the existing normalizer for exactly this Python
        # bool -> Lisp boolean gap.
        return not_fn(lisptype.lisp_bool(function(*args, **kwargs)))
    return complemented_function


@_registry.cl_function('CONSTANTLY')
def constantly(value):
    """Return function that always returns value."""
    def constant_function(*args, **kwargs):
        return value
    return constant_function


# =================================================================
# Additional symbols from the ANSI target list
# =================================================================

@_registry.cl_function('EQU')
def equ(obj1, obj2):
    """Test for object identity (alias for EQ).
    
    Note: EQU is not standard ANSI Common Lisp, but some implementations
    provide it as an alias for EQ.
    """
    return eq(obj1, obj2)


@_registry.cl_function('SIGN')
def sign(number):
    """Return the sign of a number: -1, 0, or 1.
    
    Note: The ANSI standard name is SIGNUM, but SIGN is provided as an alias.
    """
    if number == 0:
        return 0
    elif number > 0:
        return 1
    else:
        return -1


@_registry.cl_function('GETPROP')
def getprop(symbol, indicator, default=None):
    """Get property from symbol's property list (alias for GET).
    
    Note: GETPROP is not standard ANSI but provided for compatibility.
    """
    # Import get from symbols module
    from .symbols import get
    return get(symbol, indicator, default)


@_registry.cl_function('PROPERTY-LIST')
def property_list(symbol):
    """Return the property list of a symbol (alias for SYMBOL-PLIST).
    
    Note: PROPERTY-LIST is not standard ANSI but provided for compatibility.
    """
    if hasattr(symbol, 'plist'):
        return symbol.plist
    return lisptype.NIL


@_registry.cl_function('STRING-LENGTH')
def string_length(string):
    """Return the length of a string (alias for LENGTH).
    
    Note: STRING-LENGTH is not standard ANSI (use LENGTH), but provided
    for compatibility with target list.
    """
    if isinstance(string, str):
        return len(string)
    return 0


@_registry.cl_function('STRING<>')
def string_not_equal_alt(str1, str2):
    """Test if strings are not equal (alternate name for STRING/=).
    
    Note: STRING<> is not standard ANSI (use STRING/=), but provided
    for compatibility with target list.
    """
    if isinstance(str1, lisptype.LispSymbol):
        str1 = str1.name
    if isinstance(str2, lisptype.LispSymbol):
        str2 = str2.name
    return lisptype.lisp_bool(str(str1) != str(str2))
