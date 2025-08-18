"""Central runtime state for fclpy.

This module holds mutable runtime state in one place to avoid scattering
globals across modules. Importing this module is safe because it should
not import other fclpy modules that depend on it.
"""

# Package registry: mapping name -> Package (populated by lisptype.make_package)
packages = {}

# Current package (set by in-package or initialization code)
current_package = None

# Environment object (set by lispenv during setup)
current_environment = None

# Flag used by lispenv.setup_standard_environment
functions_loaded = False
