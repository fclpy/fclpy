"""
Lisp functions module - backward compatibility wrapper.

This module maintains backward compatibility by importing all functions
from the modularized lispfunc package and re-exporting them as if they
were still in a single module.
"""

# Import everything from the modularized package
from .lispfunc import *

# This ensures that existing imports like:
#   from fclpy.lispfunc import car, cdr, cons
#   import fclpy.lispfunc as lispfunc
# continue to work exactly as before.
