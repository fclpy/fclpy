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
# Restart stack: list of dictionaries mapping restart names to functions
# Each entry is a dict of {name: callable, name: callable, ...}
restart_stack = []

# Handler stack: the active condition handlers, outermost first (CLHS 9.1.4).
# Each entry is one *handler cluster* -- the handlers established by a single
# HANDLER-BIND / HANDLER-CASE / IGNORE-ERRORS form -- represented as a list of
# (type-specifier, function-designator) pairs in the order they were written,
# because CLHS specifies that handlers within one cluster are tried in order.
#
# This exists so signaling can walk the handlers *at the signal point, before
# unwinding*, which is what ANSI requires and what running handlers from a
# Python `except` clause cannot do: by the time an `except` runs, every CATCH /
# RESTART-CASE / UNWIND-PROTECT frame inside the protected form has already
# been torn down, so a handler could never throw to a tag or invoke a restart
# established there (plan.md Finding E). Pushed/popped by the establishing
# forms in lispfunc/evaluation_conditions.py; walked by signal_condition().
handler_stack = []

# PPRINT-LOGICAL-BLOCK frame stack (CLHS 22.2.2), innermost last. Each entry
# is an io_write.PPrintFrame; PPRINT-POP and PPRINT-EXIT-IF-LIST-EXHAUSTED
# consult the top one, and *PRINT-LEVEL* nesting depth is this stack's length
# at entry, so both live here rather than as a scalar counter one call could
# forget to restore on a non-local exit.
pprint_stack = []