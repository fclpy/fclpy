"""Comparison and equality functions."""

import fclpy.lisptype as lisptype
from .core import atom, car, cdr, consp
from fclpy.lispfunc import registry as _registry


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
    
    # Strings
    if isinstance(obj1, str) and isinstance(obj2, str):
        return lisptype.lisp_bool(obj1 == obj2)
    
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
    
    # Characters - case insensitive
    if isinstance(obj1, str) and isinstance(obj2, str) and len(obj1) == 1 and len(obj2) == 1:
        return lisptype.lisp_bool(obj1.upper() == obj2.upper())
    
    # Strings - case insensitive
    if isinstance(obj1, str) and isinstance(obj2, str):
        return lisptype.lisp_bool(obj1.upper() == obj2.upper())
    
    # Arrays/vectors
    if isinstance(obj1, (list, tuple)) and isinstance(obj2, (list, tuple)):
        if len(obj1) != len(obj2):
            return lisptype.NIL
        for x, y in zip(obj1, obj2):
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
            if compound_type == 'MOD':
                n = rest[0] if len(rest) > 0 else 1
                return lisptype.lisp_bool(0 <= object < n)
            elif compound_type == 'UNSIGNED-BYTE':
                n = rest[0] if len(rest) > 0 else 8
                return lisptype.lisp_bool(0 <= object < (2 ** n))
            elif compound_type == 'SIGNED-BYTE':
                n = rest[0] if len(rest) > 0 else 8
                limit = 2 ** (n - 1)
                return lisptype.lisp_bool(-limit <= object < limit)
        
        elif compound_type == 'SIMPLE-BIT-VECTOR':
            # (SIMPLE-BIT-VECTOR [size]) - for now, treat as vector check
            from fclpy.lispfunc.vectors import AdjustableVector
            return lisptype.lisp_bool(isinstance(object, (list, tuple, AdjustableVector)))
        
        elif compound_type in ('VECTOR', 'SIMPLE-VECTOR', 'ARRAY', 'SIMPLE-ARRAY'):
            # (VECTOR element-type [size]) etc.
            from fclpy.lispfunc.vectors import AdjustableVector
            if not isinstance(object, (list, tuple, AdjustableVector)):
                return lisptype.NIL
            # Check size if specified (second element in rest, first is element-type)
            if len(rest) >= 2:
                size = rest[1]
                if isinstance(size, int):
                    if len(object) != size:
                        return lisptype.NIL
            return lisptype.T
        
        elif compound_type == 'STRING' or compound_type == 'SIMPLE-STRING' or compound_type == 'BASE-STRING' or compound_type == 'SIMPLE-BASE-STRING':
            # (STRING [size]) - string with optional size
            if not isinstance(object, str):
                return lisptype.NIL
            if len(rest) > 0:
                size = rest[0]
                if isinstance(size, int) and len(object) != size:
                    return lisptype.NIL
            return lisptype.T
        
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
    
    # Handle string or symbol type specifiers
    if isinstance(type_specifier, str):
        type_name = type_specifier.upper()
    elif hasattr(type_specifier, 'name'):
        type_name = type_specifier.name.upper()
    else:
        type_name = str(type_specifier).upper()
    
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
    elif type_name == 'FIXNUM':
        # Fixnum: integers within machine word range
        return lisptype.lisp_bool(isinstance(object, int) and FIXNUM_MIN <= object <= FIXNUM_MAX)
    elif type_name == 'BIGNUM':
        # Bignum: integers outside fixnum range
        return lisptype.lisp_bool(isinstance(object, int) and (object < FIXNUM_MIN or object > FIXNUM_MAX))
    elif type_name == 'FLOAT' or type_name == 'SINGLE-FLOAT' or type_name == 'DOUBLE-FLOAT':
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
    elif type_name == 'STRING':
        return lisptype.lisp_bool(isinstance(object, str))
    elif type_name == 'SYMBOL':
        # In Common Lisp, NIL is both the empty list AND the symbol NIL
        # So we need to accept both LispSymbol instances and NIL
        return lisptype.lisp_bool(isinstance(object, lisptype.LispSymbol) or object is lisptype.NIL or isinstance(object, lisptype.lispNull))
    elif type_name == 'KEYWORD':
        return lisptype.lisp_bool(isinstance(object, lisptype.lispKeyword))
    elif type_name == 'FUNCTION':
        return lisptype.lisp_bool(callable(object))
    elif type_name == 'STANDARD-OBJECT' or type_name == 'INSTANCE':
        return lisptype.lisp_bool(isinstance(object, classes.LispInstance))
    elif type_name == 'VECTOR' or type_name == 'SIMPLE-VECTOR':
        from fclpy.lispfunc.vectors import AdjustableVector
        return lisptype.lisp_bool(isinstance(object, (list, tuple, AdjustableVector)))
    elif type_name == 'ARRAY':
        from fclpy.lispfunc.vectors import AdjustableVector
        return lisptype.lisp_bool(isinstance(object, (list, tuple, AdjustableVector)))
    elif type_name == 'HASH-TABLE':
        return lisptype.lisp_bool(isinstance(object, dict))
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
    from fclpy.lispfunc.vectors import AdjustableVector
    
    # Check for user-defined instances first
    if isinstance(object, classes.LispInstance):
        return object.lisp_class.name
    
    # Check for vectors (AdjustableVector must come before list/tuple check)
    if isinstance(object, AdjustableVector):
        return lisptype.LispSymbol('SIMPLE-VECTOR')
    
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
    elif callable(object):
        return lisptype.LispSymbol('FUNCTION')
    else:
        return lisptype.T


@_registry.cl_function('SUBTYPEP')
def subtypep(type1, type2):
    """Test if type1 is a subtype of type2."""
    # Convert to uppercase string names for comparison
    if isinstance(type1, lisptype.LispSymbol):
        t1 = type1.name.upper()
    elif isinstance(type1, str):
        t1 = type1.upper()
    else:
        t1 = str(type1).upper()

    if isinstance(type2, lisptype.LispSymbol):
        t2 = type2.name.upper()
    elif isinstance(type2, str):
        t2 = type2.upper()
    else:
        t2 = str(type2).upper()

    # T is supertype of everything
    if t2 == 'T':
        return lisptype.T, lisptype.T

    # Everything is a subtype of itself
    if t1 == t2:
        return lisptype.T, lisptype.T

    # Numeric type hierarchy
    if t1 == 'INTEGER' and t2 in ['RATIONAL', 'REAL', 'NUMBER']:
        return lisptype.T, lisptype.T
    if t1 == 'RATIONAL' and t2 in ['REAL', 'NUMBER']:
        return lisptype.T, lisptype.T
    if t1 in ['SINGLE-FLOAT', 'DOUBLE-FLOAT', 'FLOAT'] and t2 in ['REAL', 'NUMBER']:
        return lisptype.T, lisptype.T
    if t1 == 'REAL' and t2 == 'NUMBER':
        return lisptype.T, lisptype.T
    if t1 == 'COMPLEX' and t2 == 'NUMBER':
        return lisptype.T, lisptype.T

    # List type hierarchy
    if t1 == 'NULL' and t2 in ['LIST', 'ATOM']:
        return lisptype.T, lisptype.T
    if t1 == 'CONS' and t2 == 'LIST':
        return lisptype.T, lisptype.T

    # Character and string hierarchy
    if t1 == 'CHARACTER' and t2 == 'BASE-CHAR':
        return lisptype.T, lisptype.T
    if t1 == 'BASE-CHAR' and t2 == 'CHARACTER':
        return lisptype.T, lisptype.T

    # Symbol hierarchy
    if t1 == 'KEYWORD' and t2 == 'SYMBOL':
        return lisptype.T, lisptype.T

    # Atom hierarchy
    if t2 == 'ATOM' and t1 not in ['CONS', 'LIST']:
        return lisptype.T, lisptype.T
    if t2 == 'ATOM' and t1 == 'NULL':
        return lisptype.T, lisptype.T

    # Function types
    if t1 in ['COMPILED-FUNCTION', 'INTERPRETED-FUNCTION'] and t2 == 'FUNCTION':
        return lisptype.T, lisptype.T

    # Array and vector types
    if t1 == 'SIMPLE-VECTOR' and t2 in ['VECTOR', 'SIMPLE-ARRAY', 'ARRAY']:
        return lisptype.T, lisptype.T
    if t1 == 'VECTOR' and t2 == 'ARRAY':
        return lisptype.T, lisptype.T
    if t1 == 'SIMPLE-ARRAY' and t2 == 'ARRAY':
        return lisptype.T, lisptype.T

    # Stream types
    if t1 in ['INPUT-STREAM', 'OUTPUT-STREAM'] and t2 == 'STREAM':
        return lisptype.T, lisptype.T
    if t1 in ['FILE-STREAM', 'STRING-STREAM'] and t2 in ['STREAM', 'INPUT-STREAM', 'OUTPUT-STREAM']:
        return lisptype.T, lisptype.T

    # Hash table types
    if t1 == 'HASH-TABLE' and t2 == 'T':
        return lisptype.T, lisptype.T

    # Pathname types
    if t1 == 'LOGICAL-PATHNAME' and t2 == 'PATHNAME':
        return lisptype.T, lisptype.T

    # Package type
    if t1 == 'PACKAGE' and t2 == 'T':
        return lisptype.T, lisptype.T

    # Condition types (simplified hierarchy)
    if t1 in ['SIMPLE-ERROR', 'TYPE-ERROR', 'ARITHMETIC-ERROR'] and t2 in ['ERROR', 'SERIOUS-CONDITION', 'CONDITION']:
        return lisptype.T, lisptype.T
    if t1 == 'ERROR' and t2 in ['SERIOUS-CONDITION', 'CONDITION']:
        return lisptype.T, lisptype.T
    if t1 in ['WARNING', 'STYLE-WARNING'] and t2 == 'CONDITION':
        return lisptype.T, lisptype.T

    # No subtype relationship found
    return lisptype.NIL, lisptype.T


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
        return not_fn(function(*args, **kwargs))
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
