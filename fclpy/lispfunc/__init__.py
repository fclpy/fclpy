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
from .arrays import *
from .streams import *
from .pathnames import *
from .evaluation import *
from .comparison import *
from .characters import *
from .io import *
from .utilities import *
from .classes import *
from .misc_macros import *

# Import special operator functions explicitly (underscores prevent * import)
from .math import (_s_plus_, _s_minus_, _s_star_, _s_slash_, _s_eq_, _s_lt_, _s_gt_, 
                  _s_lt__s_eq_, _s_gt__s_eq_, _s_slash__s_eq_, _s_one_s_plus_, _s_one_s_minus_)
from .io import _s_print_
from .sequences import list_s_star_

# Explicitly import critical I/O functions to ensure they're always available
# (avoids timing issues with circular imports during evaluation)
from .io_write import print_fn, prin1, princ, terpri, fresh_line, write, write_char, write_string, write_line
from .io_read import read, read_line, read_char, peek_char

# Create aliases for functions that conflict with Python builtins
list = list_fn
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
# arrays.py is deliberately *not* auto-registered: every operator in it is
# registered by an explicit `@cl_function`, and `register_module` would also
# bind its model helpers (`row_major_get`, `is_array`, ...) as Lisp functions.
_registry.register_module(_evaluation_mod)
_registry.register_module(_comparison_mod)
_registry.register_module(_characters_mod)
_registry.register_module(_io_mod)
_registry.register_module(_utilities_mod)

# Additional functions that need to be implemented
def allocate_instance(class_obj, **kwargs):
    """Allocate instance of class."""
    raise NotImplementedError("ALLOCATE-INSTANCE")

def arithmetic_error_operands(condition):
    """Get operands from arithmetic error."""
    raise NotImplementedError("ARITHMETIC-ERROR-OPERANDS")

def arithmetic_error_operation(condition):
    """Get operation from arithmetic error."""
    raise NotImplementedError("ARITHMETIC-ERROR-OPERATION")

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
    'get_macro_character', 'set_dispatch_macro_character', 'set_macro_character',
    
    # Convenience API
    'eval_string', 'get_environment', 'setup_environment',
]


# ============================================================================
# Convenience API - what users expect to be able to import from lispfunc
# ============================================================================

def setup_environment():
    """Initialize and return the standard Lisp environment.
    
    Returns:
        Environment: The initialized standard environment
    """
    import fclpy.lispenv as lispenv
    lispenv.setup_standard_environment()
    return lispenv.current_environment


def get_environment():
    """Get the current Lisp environment, initializing if needed.
    
    Returns:
        Environment: The current environment
    """
    import fclpy.lispenv as lispenv
    # Always call setup_standard_environment to ensure the environment
    # is properly populated. It's idempotent and returns quickly if 
    # already set up (checks functions_loaded flag).
    lispenv.setup_standard_environment()
    return lispenv.current_environment


def eval_string(code, env=None):
    """Parse and evaluate a string of Lisp code.
    
    Args:
        code: A string containing Lisp code
        env: Optional environment (uses current if None)
        
    Returns:
        The result of evaluating the last expression
    
    Raises:
        ConditionException: For unhandled THROW with no matching CATCH
    """
    import io
    import fclpy.lispreader as lispreader
    from fclpy.readtable import get_current_readtable
    from fclpy.lispfunc.evaluation import eval as lisp_eval
    from fclpy.lispfunc.evaluation_core import ThrowException, ConditionException
    import fclpy.lisptype as lisptype
    
    if env is None:
        env = get_environment()
    
    string_io = io.StringIO(code)
    stream = lispreader.LispStream(string_io)
    readtable = get_current_readtable()
    reader = lispreader.LispReader(readtable, stream)
    
    result = None
    while True:
        try:
            expr = reader.read_1()
            if expr is None:  # EOF
                break
            try:
                result = lisp_eval(expr, env)
            except ThrowException as e:
                # Uncaught THROW - signal a CONTROL-ERROR condition
                # This allows Lisp handler-case to catch the error
                control_error = lisptype.ControlError(message=f"Uncaught THROW {e.tag}")
                raise ConditionException(control_error, recoverable=False)
        except EOFError:
            break
    
    return result
