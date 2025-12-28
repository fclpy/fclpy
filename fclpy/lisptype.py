"""
lisptype - Common Lisp type system (re-exporter).

This module provides backward compatibility by re-exporting all symbols from:
- lisptype_basic: Core types, symbols, characters, utilities
- lisptype_extended: Environment, package system, condition system, restarts

All existing code importing from lisptype continues to work unchanged.
"""

# Re-export all symbols from basic and extended modules
from .lisptype_basic import *  # noqa: F401, F403
from .lisptype_extended import *  # noqa: F401, F403

# Explicit exports for clarity - all public symbols
__all__ = [
    # From lisptype_basic
    'LispNotImplementedError', 'LispTypeError', 'LispError',
    'LispEndOfFileError', 'LispEnvironmentError',
    'lispT', 'lispSequence', 'lispList', 'lispNull', 'LispSymbol',
    'lispKeyword', 'Character', 'lispCons', 'lispConsIterator',
    'NIL', 'T',
    'symbol_value', 'set_symbol_value', 'symbol_function',
    'set_symbol_function', 'symbol_plist', 'set_symbol_plist',
    'lisp_bool', 'is_truthy', 'lisp_str', 'lisp_repr',
    'MultipleValues', 'py_str_map',
    'Binding', 'FunctionBinding', 'SpecialForm',
    # From lisptype_extended
    'Environment',
    'Package', 'KEYWORD_PACKAGE', 'COMMON_LISP_PACKAGE', 'COMMON_LISP_USER_PACKAGE',
    'make_package', 'find_package', 'intern_symbol', 'intern_keyword',
    'Condition', 'SimpleCondition', 'Warning', 'Error',
    'TypeError', 'ProgramError', 'ControlError', 'FileError', 'StreamError',
    'EndOfFile', 'ArithmeticError', 'DivisionByZero',
    'FloatingPointInvalidOperation', 'FloatingPointOverflow', 'FloatingPointUnderflow',
    'Restart', 'RestartException',
    'resolve_environment',
    'py_str_to_sym'
]
