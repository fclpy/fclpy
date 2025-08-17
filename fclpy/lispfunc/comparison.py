"""Comparison and equality functions."""

import fclpy.lisptype as lisptype
from .core import atom, car, cdr, consp


def eq(obj1, obj2):
    """Test for object identity."""
    return obj1 is obj2


def eql(obj1, obj2):
    """Test for object equality (numbers and characters)."""
    if obj1 is obj2:
        return True
    
    # Numbers are eql if they are the same type and value
    if isinstance(obj1, (int, float, complex)) and isinstance(obj2, (int, float, complex)):
        return type(obj1) == type(obj2) and obj1 == obj2
    
    # Characters are eql if they are the same character
    if isinstance(obj1, str) and isinstance(obj2, str) and len(obj1) == 1 and len(obj2) == 1:
        return obj1 == obj2
    
    return False


def equal(obj1, obj2):
    """Test for structural equality."""
    if eql(obj1, obj2):
        return True
    
    # Cons cells
    if consp(obj1) and consp(obj2):
        return equal(car(obj1), car(obj2)) and equal(cdr(obj1), cdr(obj2))
    
    # Strings
    if isinstance(obj1, str) and isinstance(obj2, str):
        return obj1 == obj2
    
    # Lists and tuples
    if isinstance(obj1, (list, tuple)) and isinstance(obj2, (list, tuple)):
        if len(obj1) != len(obj2):
            return False
        return all(equal(x, y) for x, y in zip(obj1, obj2))
    
    return False


def equalp(obj1, obj2):
    """Test for liberal equality."""
    if equal(obj1, obj2):
        return True
    
    # Numbers - allow type coercion
    if isinstance(obj1, (int, float, complex)) and isinstance(obj2, (int, float, complex)):
        return obj1 == obj2
    
    # Characters - case insensitive
    if isinstance(obj1, str) and isinstance(obj2, str) and len(obj1) == 1 and len(obj2) == 1:
        return obj1.upper() == obj2.upper()
    
    # Strings - case insensitive
    if isinstance(obj1, str) and isinstance(obj2, str):
        return obj1.upper() == obj2.upper()
    
    # Arrays/vectors
    if isinstance(obj1, (list, tuple)) and isinstance(obj2, (list, tuple)):
        if len(obj1) != len(obj2):
            return False
        return all(equalp(x, y) for x, y in zip(obj1, obj2))
    
    return False


def not_fn(obj):
    """Logical NOT."""
    return obj is None or obj == lisptype.NIL


def null(obj):
    """Test for null/nil."""
    return obj is None or obj == lisptype.NIL


def typep(object, type_specifier):
    """Test if object is of given type."""
    if isinstance(type_specifier, str):
        type_name = type_specifier.upper()
    elif hasattr(type_specifier, 'name'):
        type_name = type_specifier.name.upper()
    else:
        type_name = str(type_specifier).upper()
    
    if type_name == 'T':
        return True
    elif type_name == 'NULL':
        return null(object)
    elif type_name == 'ATOM':
        return atom(object)
    elif type_name == 'CONS':
        return consp(object)
    elif type_name == 'LIST':
        return null(object) or consp(object)
    elif type_name == 'NUMBER':
        return isinstance(object, (int, float, complex))
    elif type_name == 'INTEGER':
        return isinstance(object, int)
    elif type_name == 'FLOAT' or type_name == 'SINGLE-FLOAT' or type_name == 'DOUBLE-FLOAT':
        return isinstance(object, float)
    elif type_name == 'COMPLEX':
        return isinstance(object, complex)
    elif type_name == 'REAL':
        return isinstance(object, (int, float))
    elif type_name == 'RATIONAL':
        return isinstance(object, (int, float))  # Python doesn't have rationals
    elif type_name == 'CHARACTER':
        return isinstance(object, str) and len(object) == 1
    elif type_name == 'STRING':
        return isinstance(object, str)
    elif type_name == 'SYMBOL':
        return isinstance(object, lisptype.LispSymbol)
    elif type_name == 'KEYWORD':
        return isinstance(object, lisptype.lispKeyword)
    elif type_name == 'FUNCTION':
        return callable(object)
    else:
        return False


def type_of(object):
    """Return type of object."""
    if null(object):
        return lisptype.LispSymbol('NULL')
    elif consp(object):
        return lisptype.LispSymbol('CONS')
    elif isinstance(object, lisptype.lispKeyword):
        return lisptype.LispSymbol('KEYWORD')
    elif isinstance(object, lisptype.LispSymbol):
        return lisptype.LispSymbol('SYMBOL')
    elif isinstance(object, int):
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
        return lisptype.LispSymbol('T')


def subtypep(type1, type2):
    """Test if type1 is a subtype of type2."""
    # Convert to string representation for comparison
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
    
    # T is the supertype of everything
    if t2 == 'T':
        return True, True
    
    # Everything is a subtype of itself
    if t1 == t2:
        return True, True
    
    # Numeric type hierarchy
    if t1 == 'INTEGER':
        if t2 in ['RATIONAL', 'REAL', 'NUMBER']:
            return True, True
    elif t1 == 'RATIONAL':
        if t2 in ['REAL', 'NUMBER']:
            return True, True
    elif t1 in ['SINGLE-FLOAT', 'DOUBLE-FLOAT', 'FLOAT']:
        if t2 in ['REAL', 'NUMBER']:
            return True, True
    elif t1 == 'REAL':
        if t2 == 'NUMBER':
            return True, True
    elif t1 == 'COMPLEX':
        if t2 == 'NUMBER':
            return True, True
    
    # List type hierarchy
    if t1 == 'NULL':
        if t2 in ['LIST', 'ATOM']:
            return True, True
    elif t1 == 'CONS':
        if t2 == 'LIST':
            return True, True
    
    # Character and string hierarchy
    if t1 == 'CHARACTER':
        if t2 == 'BASE-CHAR':
            return True, True
    elif t1 == 'BASE-CHAR':
        if t2 == 'CHARACTER':
            return True, True
    
    # Symbol hierarchy
    if t1 == 'KEYWORD':
        if t2 == 'SYMBOL':
            return True, True
    
    # Everything is an atom except cons cells
    if t2 == 'ATOM':
        if t1 not in ['CONS', 'LIST']:
            return True, True
        elif t1 == 'NULL':  # NULL is both a list and an atom
            return True, True
    
    # Function types
    if t1 in ['COMPILED-FUNCTION', 'INTERPRETED-FUNCTION']:
        if t2 == 'FUNCTION':
            return True, True
    
    # Array and vector types
    if t1 == 'SIMPLE-VECTOR':
        if t2 in ['VECTOR', 'SIMPLE-ARRAY', 'ARRAY']:
            return True, True
    elif t1 == 'VECTOR':
        if t2 in ['ARRAY']:
            return True, True
    elif t1 == 'SIMPLE-ARRAY':
        if t2 == 'ARRAY':
            return True, True
    
    # Stream types
    if t1 in ['INPUT-STREAM', 'OUTPUT-STREAM']:
        if t2 == 'STREAM':
            return True, True
    elif t1 in ['FILE-STREAM', 'STRING-STREAM']:
        if t2 in ['STREAM', 'INPUT-STREAM', 'OUTPUT-STREAM']:
            return True, True
    
    # Hash table types
    if t1 == 'HASH-TABLE':
        if t2 == 'T':
            return True, True
    
    # Pathname types
    if t1 == 'LOGICAL-PATHNAME':
        if t2 == 'PATHNAME':
            return True, True
    
    # Package type
    if t1 == 'PACKAGE':
        if t2 == 'T':
            return True, True
    
    # Condition types (simplified hierarchy)
    if t1 in ['SIMPLE-ERROR', 'TYPE-ERROR', 'ARITHMETIC-ERROR']:
        if t2 in ['ERROR', 'SERIOUS-CONDITION', 'CONDITION']:
            return True, True
    elif t1 == 'ERROR':
        if t2 in ['SERIOUS-CONDITION', 'CONDITION']:
            return True, True
    elif t1 in ['WARNING', 'STYLE-WARNING']:
        if t2 == 'CONDITION':
            return True, True
    
    # No subtype relationship found
    return False, True


def identity(object):
    """Return the object unchanged."""
    return object


def constantp(form, environment=None):
    """Test if form is a constant."""
    if isinstance(form, (int, float, str, bool)):
        return True
    elif isinstance(form, lisptype.lispKeyword):
        return True
    elif null(form):
        return True
    elif consp(form) and car(form) == lisptype.LispSymbol('QUOTE'):
        return True
    else:
        return False


def complement(function):
    """Return complement of function."""
    def complemented_function(*args, **kwargs):
        return not_fn(function(*args, **kwargs))
    return complemented_function


def constantly(value):
    """Return function that always returns value."""
    def constant_function(*args, **kwargs):
        return value
    return constant_function
