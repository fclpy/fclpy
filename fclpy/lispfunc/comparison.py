"""Comparison and equality functions."""

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
        if element_type is _arrays.CHARACTER_TYPE or element_type is _arrays.NIL_TYPE:
            chars = []
            for e in _arrays.array_elements(obj):
                if isinstance(e, lisptype.Character):
                    chars.append(e.char)
                elif isinstance(e, str) and len(e) == 1:
                    chars.append(e)
                else:
                    return None
            return ''.join(chars)
    return None


@_registry.cl_function('EQ')
def eq(obj1, obj2):
    """Test for object identity."""
    return lisptype.lisp_bool(obj1 is obj2)


@_registry.cl_function('EQL')
def eql(obj1, obj2):
    """Test for object equality (numbers and characters)."""
    if obj1 is obj2:
        return lisptype.T
    
    # Numbers are eql if they are the same type and value
    if isinstance(obj1, (int, float, complex)) and isinstance(obj2, (int, float, complex)):
        return lisptype.lisp_bool(type(obj1) == type(obj2) and obj1 == obj2)
    
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
    """Test for structural equality."""
    if eql(obj1, obj2) == lisptype.T:
        return lisptype.T
    
    # Cons cells
    if consp(obj1) and consp(obj2):
        car_equal = equal(car(obj1), car(obj2))
        cdr_equal = equal(cdr(obj1), cdr(obj2))
        return lisptype.lisp_bool(car_equal == lisptype.T and cdr_equal == lisptype.T)
    
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

    # Lists and tuples
    if isinstance(obj1, (list, tuple)) and isinstance(obj2, (list, tuple)):
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

    s1 = _string_characters(obj1)
    s2 = _string_characters(obj2)
    if s1 is not None and s2 is not None:
        return lisptype.lisp_bool(s1.upper() == s2.upper())
    if s1 is not None or s2 is not None:
        return lisptype.NIL


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
    
    # Constants for fixnum boundary (2^29 is common choice for 32-bit-like semantics)
    FIXNUM_MAX = 2**29 - 1
    FIXNUM_MIN = -2**29
    
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
        return lisptype.NIL

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
        return lisptype.lisp_bool(isinstance(object, classes.LispInstance))
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
    elif type_name == 'HASH-TABLE':
        return lisptype.lisp_bool(isinstance(object, dict))
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
        # In Common Lisp, BOOLEAN is equivalent to (OR NULL (EQL T))
        # i.e., only NIL and T are booleans
        return lisptype.lisp_bool(object is lisptype.NIL or isinstance(object, lisptype.lispNull) or object is lisptype.T)
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
    """Test if form is a constant."""
    if isinstance(form, (int, float, str, bool)):
        return lisptype.T
    elif isinstance(form, lisptype.lispKeyword):
        return lisptype.T
    elif null(form):
        return lisptype.T
    elif consp(form) and car(form) == lisptype.LispSymbol('QUOTE'):
        return lisptype.T
    else:
        return lisptype.NIL


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
