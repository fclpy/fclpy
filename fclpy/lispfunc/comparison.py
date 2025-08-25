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
    if isinstance(type_specifier, str):
        type_name = type_specifier.upper()
    elif hasattr(type_specifier, 'name'):
        type_name = type_specifier.name.upper()
    else:
        type_name = str(type_specifier).upper()
    
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
        return lisptype.lisp_bool(isinstance(object, (int, float, complex)))
    elif type_name == 'INTEGER':
        return lisptype.lisp_bool(isinstance(object, int))
    elif type_name == 'FLOAT' or type_name == 'SINGLE-FLOAT' or type_name == 'DOUBLE-FLOAT':
        return lisptype.lisp_bool(isinstance(object, float))
    elif type_name == 'COMPLEX':
        return lisptype.lisp_bool(isinstance(object, complex))
    elif type_name == 'REAL':
        return lisptype.lisp_bool(isinstance(object, (int, float)))
    elif type_name == 'RATIONAL':
        return lisptype.lisp_bool(isinstance(object, (int, float)))  # Python doesn't have rationals
    elif type_name == 'CHARACTER':
        return lisptype.lisp_bool(isinstance(object, str) and len(object) == 1)
    elif type_name == 'STRING':
        return lisptype.lisp_bool(isinstance(object, str))
    elif type_name == 'SYMBOL':
        return lisptype.lisp_bool(isinstance(object, lisptype.LispSymbol))
    elif type_name == 'KEYWORD':
        return lisptype.lisp_bool(isinstance(object, lisptype.lispKeyword))
    elif type_name == 'FUNCTION':
        return lisptype.lisp_bool(callable(object))
    else:
        return lisptype.NIL


@_registry.cl_function('TYPE-OF')
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
