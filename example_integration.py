#!/usr/bin/env python3
"""
Example: Using FCLPY as a library in your own Python project.

This demonstrates how to integrate FCLPY into other projects without
needing to go through the command-line interface.
"""

import sys
import os

# Add fclpy to path (adjust this path for your project)
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'fclpy'))

from fclpy import runtime
from fclpy import lispenv
from fclpy import lispfunc

def main():
    print("FCLPY Library Integration Example")
    print("=" * 40)
    
    # Initialize the Lisp environment
    lispenv.setup_standard_environment()
    environment = lispenv.current_environment
    
    print("\n1. Direct function evaluation:")
    # You can evaluate Lisp expressions directly
    # For now, we'll use the file loading method as an example
    print("   Using runtime.load_and_evaluate_file for expressions")
    
    print("\n2. Loading Lisp files programmatically:")
    # Load and evaluate Lisp files
    success = runtime.load_and_evaluate_file("test_script.lisp", environment, verbose=True)
    print(f"   File loaded successfully: {success}")
    
    print("\n3. Starting a programmatic REPL:")
    print("   (This would start an interactive session)")
    # runtime.repl(quiet=False, verbose=False)  # Uncomment to test interactively
    
    print("\n4. Defining and calling custom functions:")
    # You can define functions programmatically
    # This would need proper S-expression construction, but shows the concept
    print("   Custom function integration would go here")
    
    print("\nIntegration complete! FCLPY is ready to use in your project.")

if __name__ == "__main__":
    main()
