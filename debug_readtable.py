#!/usr/bin/env python3
"""Test readtable uninterned symbol reading"""

import sys
sys.path.insert(0, '.')
import io
from fclpy.readtable import get_current_readtable, Readtable
from fclpy import lispreader, lisptype

# Create a stream with #:DEFTEST
content = "#:DEFTEST"
string_io = io.StringIO(content)
stream = lispreader.LispStream(string_io)

# Get readtable and _sharp_reader
readtable = get_current_readtable()

# Read '#' and then let _sharp_reader handle it
char = stream.read_char()
print(f"First char: {repr(char)}")

# Call _sharp_reader
result = readtable._sharp_reader(char, stream)
print(f"Result: {result}")
print(f"Type: {type(result)}")
if hasattr(result, 'name'):
    print(f"Name: {result.name}")
if hasattr(result, 'package'):
    print(f"Package: {result.package}")
