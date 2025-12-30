#!/usr/bin/env python3
"""Debug reader issues with rt-package.lsp"""

import sys
sys.path.insert(0, '.')
from fclpy.reader import read, read_all

# Just read rt-package.lsp directly
try:
    with open('../ansi-test/rt-package.lsp', 'r') as f:
        content = f.read()
    
    print("Content length:", len(content))
    print("First 500 chars:")
    print(content[:500])
    
    print("\n=== Attempting to read ===")
    exprs = read_all(content)
    print(f"Got {len(exprs)} expressions")
    for i, expr in enumerate(exprs):
        print(f"  {i}: {expr}")
except Exception as e:
    import traceback
    traceback.print_exc()
