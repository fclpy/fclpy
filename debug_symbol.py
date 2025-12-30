#!/usr/bin/env python3
"""Debug symbol str conversion"""

import sys
sys.path.insert(0, '.')
from fclpy import lisptype
from fclpy.reader import read

# Read an uninterned symbol
expr = read("#:DEFTEST")
print(f"Read result: {expr}")
print(f"Type: {type(expr)}")
print(f"Name: {expr.name if hasattr(expr, 'name') else 'N/A'}")
print(f"str(expr): {str(expr)}")
print(f"repr(expr): {repr(expr)}")
