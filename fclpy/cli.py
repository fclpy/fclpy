#!/usr/bin/env python3
"""
FCLPY CLI Module
Entry point for the fclpy command-line interface.
"""

import sys
import os
from pathlib import Path

# Ensure the package can be imported
package_dir = Path(__file__).parent
if str(package_dir) not in sys.path:
    sys.path.insert(0, str(package_dir))

def main():
    """Main entry point for the CLI."""
    # Import the run module and execute main
    try:
        # Import from the parent directory where run.py is located
        parent_dir = Path(__file__).parent.parent
        sys.path.insert(0, str(parent_dir))
        
        import run
        run.main()
    except ImportError as e:
        print(f"Error importing run module: {e}")
        sys.exit(1)
    except Exception as e:
        print(f"Error running FCLPY: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()
