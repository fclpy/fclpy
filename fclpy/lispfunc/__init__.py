"""
Lisp function library - modularized into functional groups.

This module provides all Common Lisp functions organized into logical groups:
- core: Basic data structures (cons, car, cdr, etc.)
- math: Mathematical operations (arithmetic, trigonometry, etc.)  
- sequences: List and sequence operations
- evaluation: Eval and special forms
- comparison: Equality and comparison functions
- characters: Character and string operations
- io: Input/output and stream operations
- utilities: System functions and utilities

All functions are re-exported from this module for compatibility.
"""

# Import all functions from submodules
from .core import *
from .math import *
from .sequences import *
from .evaluation import *
from .comparison import *
from .characters import *
from .io import *
from .utilities import *

# Import special operator functions explicitly (underscores prevent * import)
from .math import (_s_plus_, _s_minus_, _s_star_, _s_slash_, _s_eq_, _s_lt_, _s_gt_, 
                  _s_lt__s_eq_, _s_gt__s_eq_, _s_slash__s_eq_, _s_one_s_plus_, _s_one_s_minus_)
from .io import _s_print_
from .sequences import list_s_star_

# Create aliases for functions that conflict with Python builtins
list = lisp_list
round = round_fn
string = string_fn

# Additional aliases for lispenv compatibility
char_fn = char
apply_fn = apply
conjugate_fn = conjugate
byte = byte_fn
# vector_fn = vector  # Commented out - vector not defined yet

# Import centralized readtable functions
from ..readtable import get_macro_character, set_macro_character, set_dispatch_macro_character

# Register functions into the builtin registry so lispenv can populate from it.
from . import registry as _registry
from . import core as _core_mod, math as _math_mod, sequences as _sequences_mod, evaluation as _evaluation_mod, comparison as _comparison_mod, characters as _characters_mod, io as _io_mod, utilities as _utilities_mod

# Register modules (this will not overwrite explicit decorator registrations)
_registry.register_module(_core_mod)
_registry.register_module(_math_mod)
_registry.register_module(_sequences_mod)
_registry.register_module(_evaluation_mod)
_registry.register_module(_comparison_mod)
_registry.register_module(_characters_mod)
_registry.register_module(_io_mod)
_registry.register_module(_utilities_mod)

# Additional functions that need to be implemented
def adjust_array(*args):
    """Adjust array dimensions."""
    raise NotImplementedError("ADJUST-ARRAY")

def adjustable_array_p(array):
    """Test if array is adjustable."""
    raise NotImplementedError("ADJUSTABLE-ARRAY-P")

def allocate_instance(class_obj, **kwargs):
    """Allocate instance of class.""" 
    raise NotImplementedError("ALLOCATE-INSTANCE")

def add_method(generic_function, method):
    """Add method to generic function."""
    raise NotImplementedError("ADD-METHOD")

def aref(array, *subscripts):
    """Access array element."""
    raise NotImplementedError("AREF")

def svref(simple_vector, index):
    """Access simple vector element."""
    try:
        return simple_vector[index]
    except (IndexError, TypeError):
        return None

def vector(*objects):
    """Create vector from objects."""
    return list(objects)

def vectorp(obj):
    """Test if object is a vector."""
    return hasattr(obj, '__getitem__') and hasattr(obj, '__len__')

def arithmetic_error_operands(condition):
    """Get operands from arithmetic error."""
    raise NotImplementedError("ARITHMETIC-ERROR-OPERANDS")

def arithmetic_error_operation(condition):
    """Get operation from arithmetic error."""
    raise NotImplementedError("ARITHMETIC-ERROR-OPERATION")

def array_dimension(array, axis_number):
    """Get array dimension."""
    raise NotImplementedError("ARRAY-DIMENSION")

def arrayp(object):
    """Test if object is array."""
    raise NotImplementedError("ARRAYP")

# Export commonly used functions for easier access
__all__ = [
    # Core functions
    'car', 'cdr', 'cons', 'atom', 'consp', 'first', 'second', 'third', 
    'fourth', 'fifth', 'caar', 'cadr', 'cdar', 'cddr', 'caddr', 'butlast',
    
    # Math functions  
    'acos', 'asin', 'atan', 'cos', 'sin', 'tan', 'exp', 'expt', 'ceiling', 
    'floor', 'round_fn', 'truncate', 'abs_fn', 'max_fn', 'min_fn', 'plus', 
    'minus', 'times', 'divide', 'evenp', 'oddp', 'zerop', 'plusp', 'minusp',
    'numberp', 'integerp', 'floatp', 'gcd', 'lcm', 'ash',
    
    # Sequence functions
    'append', 'length', 'reverse', 'nreverse', 'subseq', 'copy_seq', 
    'find', 'find_if', 'find_if_not', 'member', 'assoc', 'nth', 'elt',
    'list_fn', 'make_list', 'concatenate',
    
    # Evaluation
    'eval', 'apply', 'funcall',
    
    # Comparison
    'eq', 'eql', 'equal', 'equalp', 'not_fn', 'null', 'typep', 'type_of',
    'identity',
    
    # Character functions
    'char', 'char_code', 'code_char', 'char_upcase', 'char_downcase',
    'char_equal', 'alpha_char_p', 'alphanumericp', 'digit_char_p',
    'characterp', 'string_fn', 'stringp', 'string_equal', 'string_upcase',
    'string_downcase',
    
    # I/O functions
    'format_fn', 'read', 'write', 'print_fn', 'prin1', 'princ', 'terpri',
    'read_line', 'write_line', 
    
    # Utilities
    'error', 'warn', 'gensym', 'gentemp', 'sleep', 'random', 'get_decoded_time',
    'get_universal_time', 'functionp', 'compiled_function_p', 'fboundp',
    
    # Reader macros
    'get_macro_character', 'set_dispatch_macro_character', 'set_macro_character'
]
