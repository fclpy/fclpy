"""Central runtime state for fclpy.

This module holds mutable runtime state in one place to avoid scattering
globals across modules. Importing this module is safe because it should
not import other fclpy modules that depend on it.
"""

# Package registry: mapping name -> Package (populated by lisptype.make_package)
packages = {}

# Current package -- a *mirror* of `*PACKAGE*`, not a second home for it.
# `binding.BindingFrame._mirror_package` keeps it in step when a binding form
# binds `*PACKAGE*`; read it through `current_package_value()` below, never
# directly, because a plain SETQ of `*PACKAGE*` writes the variable's value
# cell and does not touch this.
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

# The tags of the CATCH forms currently outstanding, innermost last (CLHS 5.2).
#
# THROW needs this to answer the one question the standard asks of it before it
# transfers control: "if there is no outstanding catcher whose tag is EQ to the
# tag argument, an error of type CONTROL-ERROR is signaled". Without it THROW
# raised its `ThrowException` unconditionally and an uncaught throw propagated
# all the way out of the evaluator as a **Python** exception -- which no
# handler can match, so it escaped `do-tests` and aborted the whole ANSI run
# rather than failing one test. `#.(throw 'foo 1)` inside a READ is exactly
# that shape.
#
# Pushed and popped by eval_catch for the dynamic extent of its body, so the
# stack is a record of what is *outstanding*, not of what is lexically
# enclosing -- a THROW from inside a function called by the CATCH body counts,
# and one from a closure invoked after the CATCH returned does not.
catch_tags = []

# PPRINT-LOGICAL-BLOCK frame stack (CLHS 22.2.2), innermost last. Each entry
# is an io_write.PPrintFrame; PPRINT-POP and PPRINT-EXIT-IF-LIST-EXHAUSTED
# consult the top one, and *PRINT-LEVEL* nesting depth is this stack's length
# at entry, so both live here rather than as a scalar counter one call could
# forget to restore on a non-local exit.
pprint_stack = []

# DEFSTRUCT `:TYPE` layouts (CLHS 19.4.7), keyed by upper-cased structure
# name. A `(:type list)`/`(:type vector)` structure has no class or instance
# -- it *is* a plain list/vector -- so `:INCLUDE` on one has nothing to walk
# but this flat record: {'representation': 'list'|'vector',
# 'element_type_form': <raw type form or None>, 'layout': [entries...]},
# where each layout entry is {'kind': 'pad'}, {'kind': 'name', 'value': sym}
# or {'kind': 'slot', 'name': str, 'slot_def': classes.SlotDefinition}, in
# flat storage order. Populated and read only by
# evaluation_special_forms.eval_defstruct.
typed_struct_layouts = {}

def current_package_value():
    """The current package: the value of `*PACKAGE*` (CLHS 11.1.2.1).

    The one resolver. `*PACKAGE*` is the authority and `current_package` above
    only mirrors it, so the variable is read first -- through the environment
    chain, then the symbol's value cell, which is where a proclaimed special's
    binding and any SETQ of it both land.

    This existed four times over, each copy reading `state.current_package`
    *only*: in `readtable._read_symbol`, in `lispreader._read_symbol`, in
    `reader.LispReader.__init__` and in `utilities_symbols.get_current_package`.
    Because the mirror is only written when a binding form binds `*PACKAGE*`,
    a plain ``(setq *package* (find-package "FOO"))`` -- which is exactly what
    a loaded file does, and what `load.15a` tests -- changed the variable and
    left every one of those readers interning into the old package.
    """
    import fclpy.lisptype as lisptype

    symbol = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*PACKAGE*')
    env = current_environment
    if env is not None and env.has_variable(symbol):
        package = env.find_variable(symbol)
        if isinstance(package, lisptype.Package):
            return package
    package = getattr(symbol, 'value', None)
    if isinstance(package, lisptype.Package):
        return package
    if isinstance(current_package, lisptype.Package):
        return current_package
    return lisptype.COMMON_LISP_USER_PACKAGE
