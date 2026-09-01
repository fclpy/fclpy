
"""
FCLPY - A Common Lisp Interpreter in Python

FCLPY provides a complete Common Lisp environment implemented in Python,
including an interactive REPL, file evaluation, and a clean API for 
embedding Lisp functionality into Python applications.

Basic usage:
    >>> from fclpy import runtime, lispenv
    >>> lispenv.setup_standard_environment()
    >>> runtime.repl()
"""

__version__ = "1.0.0"
__author__ = "FCLPY Development Team"
__email__ = "fclpy@example.com"
__license__ = "MIT"

import importlib

# Core modules
from fclpy import lisptype
from fclpy import lispfunc  
from fclpy import lispenv
from fclpy import lispreader
from fclpy import runtime

# Main API exports
__all__ = [
    'runtime',
    'lispenv', 
    'lispfunc',
    'lisptype',
    'lispreader',
    '__version__'
]