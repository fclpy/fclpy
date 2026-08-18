"""CLOS class, instance, slot, and method operations.

This module maps the Lisp-visible CLOS API into the more complete
implementation found in `fclpy.classes`. The goal is to provide
minimal, well-behaved bindings so the ANSI test-suite can define
classes and methods at load time without triggering assertions.
"""

import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry

import fclpy.classes as classes


def _protocol_gf(name):
    """Get (or create) the one GenericFunction backing a CLOS metaobject
    protocol operation (CLHS 7.1), so a user DEFMETHOD on e.g.
    SHARED-INITIALIZE extends the exact object this module installs its
    default method on below -- both resolve through
    `classes.ensure_generic_function`, which is keyed by name string
    (plan.md Finding L: this used to be two unconnected mechanisms, one a
    plain Python no-op function no DEFMETHOD could ever reach).

    A GF found with no methods at all self-heals inside
    `classes.call_generic_function` via the installers registered below,
    so this need only resolve the object, not repair it.
    """
    return classes.ensure_generic_function(lisptype.py_str_to_sym(name))


def _resolve_class(class_spec):
    if isinstance(class_spec, classes.LispClass):
        return class_spec
    cls = classes.find_class(class_spec)
    if cls is None:
        raise lisptype.LispError(f"Class not found: {class_spec}")
    return cls


def _initarg_key(key):
    name = key.name if hasattr(key, 'name') else str(key)
    return name.upper().lstrip(':')


def _initargs_to_map(initargs):
    """CLHS 7.1.2: when an initarg is supplied more than once, the
    leftmost occurrence takes precedence."""
    m = {}
    flat = list(initargs)
    i = 0
    while i + 1 < len(flat):
        key = _initarg_key(flat[i])
        if key not in m:
            m[key] = flat[i + 1]
        i += 2
    return m


def _initargs_to_positions(initargs):
    """First-occurrence position (by pair index) of each initarg keyword in
    the call's argument list. CLHS 7.1.2: when a slot has *more than one*
    declared initarg and the call supplies values under more than one of
    those names, "the leftmost in the initialization argument list" wins --
    leftmost in the *call*, not in the slot's own `:initarg` declaration
    order (class-07.9 supplies `:s1b` before `:s1a`, both naming slot S1,
    and requires :s1b's value, even though :s1a is declared first)."""
    positions = {}
    flat = list(initargs)
    i = 0
    while i + 1 < len(flat):
        key = _initarg_key(flat[i])
        if key not in positions:
            positions[key] = i // 2
        i += 2
    return positions


def _slot_names_selects(slot_names, name):
    """Does a SHARED-INITIALIZE `slot-names` argument (CLHS 7.1.2) select
    slot `name`: T selects every slot, NIL selects none, a list selects
    those named."""
    if slot_names is lisptype.T:
        return True
    if slot_names is lisptype.NIL or slot_names is None:
        return False
    from fclpy.lispfunc.core import _consp_internal, car, cdr
    current = slot_names
    while _consp_internal(current):
        item = car(current)
        item_name = item.name if hasattr(item, 'name') else str(item)
        if item_name.upper() == name.upper():
            return True
        current = cdr(current)
    return False


def _eval_initform(slot_def):
    from fclpy.lispfunc.evaluation_core import eval as _eval
    import fclpy.state as state
    env = slot_def.definition_env or state.current_environment
    if env is None:
        # Only reachable when MAKE-INSTANCE runs before any environment has
        # ever been set up (the unit-test suite calls it directly, without
        # the bootstrap `run_all_tests.py`/`run_ansi.py` always do first) --
        # setup_standard_environment() is idempotent, so this is free once
        # a real environment already exists.
        import fclpy.lispenv as lispenv
        lispenv.setup_standard_environment()
        env = state.current_environment
    return _eval(slot_def.initform, env)


# --- The instance-creation/reinitialization protocol (CLHS 7.1) ---
#
# MAKE-INSTANCE, ALLOCATE-INSTANCE, INITIALIZE-INSTANCE, SHARED-INITIALIZE,
# REINITIALIZE-INSTANCE, UPDATE-INSTANCE-FOR-DIFFERENT-CLASS and
# UPDATE-INSTANCE-FOR-REDEFINED-CLASS are themselves standard generic
# functions with one default primary method apiece (CLHS 7.1.1-7.1.7), not
# opaque Python no-ops: user code routinely adds methods to them --
# ansi-test's make-instance.lsp defines DEFMETHODs on MAKE-INSTANCE
# directly, and SHARED-INITIALIZE's default method below is what every
# MAKE-INSTANCE/REINITIALIZE-INSTANCE call actually passes through -- and a
# plain Python function bound under one of these names can never be
# extended by a DEFMETHOD; only a real GenericFunction object can.

def _default_make_instance(class_obj, *initargs):
    """MAKE-INSTANCE's default method (CLHS 7.1): resolve a symbol/string
    designator to its class, then allocate and initialize.

    Resolving here, not only in `lispfunc.classes.make_instance`, matters:
    evaluating even one DEFMETHOD on MAKE-INSTANCE (ansi-test's
    make-instance.lsp does) replaces MAKE-INSTANCE's *entire* environment
    binding with the bare GenericFunction object, so every later ordinary
    Lisp-level `(make-instance 'some-class)` call reaches this default
    method directly with the unresolved symbol -- the Python wrapper that
    used to resolve it is no longer in the call path at all.
    """
    class_obj = classes.resolve_class_designator(class_obj)
    if not isinstance(class_obj, classes.LispClass):
        raise lisptype.LispTypeError(f"MAKE-INSTANCE: expected a class or class name, got {class_obj}")
    instance = classes.call_generic_function(_protocol_gf('ALLOCATE-INSTANCE'), [class_obj] + list(initargs))
    classes.call_generic_function(_protocol_gf('INITIALIZE-INSTANCE'), [instance] + list(initargs))
    return instance


def _default_allocate_instance(cls, *initargs):
    return classes.LispInstance(lisp_class=cls)


def _default_shared_initialize(instance, slot_names, *initargs):
    initarg_map = _initargs_to_map(initargs)
    initarg_positions = _initargs_to_positions(initargs)
    for name, slot_def in instance.lisp_class.get_all_slots().items():
        supplied = False
        # CLHS 7.1.2: a slot may declare more than one :initarg. Of the
        # ones actually supplied to this call, the one occurring leftmost
        # in the *call's* argument list wins -- not the one declared first
        # on the slot, which a single `initarg` field could only ever
        # remember one of anyway.
        supplied_keys = [_initarg_key(sym) for sym in slot_def.initargs]
        supplied_keys = [k for k in supplied_keys if k in initarg_map]
        if supplied_keys:
            winner = min(supplied_keys, key=lambda k: initarg_positions[k])
            instance.slot_values[name] = initarg_map[winner]
            supplied = True
        # A slot with no declared :initarg still accepts a same-named
        # keyword as a convenience (predates this rewrite; no ANSI test
        # can rely on it, since real CLHS-conforming code always declares
        # the initarg it uses, but existing direct-Python callers of
        # MAKE-INSTANCE do).
        if not supplied and name in initarg_map:
            instance.slot_values[name] = initarg_map[name]
            supplied = True
        if not supplied and name not in instance.slot_values \
                and slot_def.initform is not None \
                and _slot_names_selects(slot_names, name):
            instance.slot_values[name] = _eval_initform(slot_def)
    return instance


def _default_initialize_instance(instance, *initargs):
    classes.call_generic_function(_protocol_gf('SHARED-INITIALIZE'), [instance, lisptype.T] + list(initargs))
    return instance


def _default_reinitialize_instance(instance, *initargs):
    classes.call_generic_function(_protocol_gf('SHARED-INITIALIZE'), [instance, lisptype.NIL] + list(initargs))
    return instance


def _default_update_instance_for_different_class(previous, current, *initargs):
    added = [name for name in current.lisp_class.get_all_slots() if name not in current.slot_values]
    if added:
        from fclpy.lispfunc.sequence_protocol import make_lisp_list
        added_list = make_lisp_list([lisptype.py_str_to_sym(n) for n in added])
        classes.call_generic_function(_protocol_gf('SHARED-INITIALIZE'), [current, added_list] + list(initargs))
    return current


def _default_update_instance_for_redefined_class(instance, added_slots, discarded_slots, property_list, *initargs):
    classes.call_generic_function(_protocol_gf('SHARED-INITIALIZE'), [instance, added_slots] + list(initargs))
    return instance


def _default_change_class(instance, new_class, *initargs):
    """CHANGE-CLASS's default method (CLHS 7.2): change instance's class
    in place, keeping the values of slots the old and new class share by
    name, discarding the rest, and running
    UPDATE-INSTANCE-FOR-DIFFERENT-CLASS -- whose default method fills in
    any slot the new class adds via SHARED-INITIALIZE. This replaces just
    swapping `.lisp_class` and leaving every newly-added slot unbound and
    never initialized.
    """
    if not isinstance(instance, classes.LispInstance):
        raise lisptype.LispTypeError(f"CHANGE-CLASS: not an instance: {instance}")
    new_cls = _resolve_class(new_class)

    old_class = instance.lisp_class
    old_slot_values = dict(instance.slot_values)
    new_all_slots = new_cls.get_all_slots()

    # CLHS 7.2.2: `previous` is a snapshot of the instance as it was in its
    # old class, passed to UPDATE-INSTANCE-FOR-DIFFERENT-CLASS for
    # introspection (e.g. reading a slot the new class discarded).
    previous = classes.LispInstance(lisp_class=old_class, slot_values=old_slot_values)

    instance.lisp_class = new_cls
    instance.slot_values = {name: val for name, val in old_slot_values.items() if name in new_all_slots}

    classes.call_generic_function(_protocol_gf('UPDATE-INSTANCE-FOR-DIFFERENT-CLASS'),
                                   [previous, instance] + list(initargs))
    return instance


def _default_slot_unbound(class_obj, instance, slot_name):
    raise lisptype.LispError(f"Slot unbound: {slot_name}")


# Specialized on T (None), not STANDARD-OBJECT: any user method
# specializing on the instance's own class is still more specific and
# still wins ordinary dispatch (T scores lowest in
# `classes._specificity_key`), and T does not depend on the
# STANDARD-OBJECT class object surviving -- `classes.find_class` is a bare
# module-level dict nothing stops Python code from clearing (the unit-test
# suite does, between tests, to isolate class definitions), which would
# otherwise silently strand a STANDARD-OBJECT-specialized default on a
# specializer no live instance's ancestry can ever match again.
#
# Registered with `classes.register_default_method_installer` rather than
# installed once at import time: `classes.call_generic_function` reinstalls
# a GF's default the moment it is found with *no* methods at all (whether
# because this module's own install never ran yet, or because something
# cleared the shared generic-function registry out from under it), so
# every call site self-heals uniformly instead of each caller needing its
# own recovery check.
#
# Also run once immediately, at import time: self-healing alone would
# leave a GF with no default until the *next* time it is found completely
# empty, but that "empty" state passes exactly once, at its very first
# creation -- and ansi-test's make-instance.lsp adds its own DEFMETHODs to
# MAKE-INSTANCE before this module could otherwise ever see it empty, so
# lazily-only installation would leave those user methods with no fallback
# to combine with.
_PROTOCOL_DEFAULTS = [
    ('MAKE-INSTANCE', [None], _default_make_instance),
    ('ALLOCATE-INSTANCE', [None], _default_allocate_instance),
    ('INITIALIZE-INSTANCE', [None], _default_initialize_instance),
    ('REINITIALIZE-INSTANCE', [None], _default_reinitialize_instance),
    ('SHARED-INITIALIZE', [None, None], _default_shared_initialize),
    ('UPDATE-INSTANCE-FOR-DIFFERENT-CLASS', [None, None], _default_update_instance_for_different_class),
    ('UPDATE-INSTANCE-FOR-REDEFINED-CLASS', [None], _default_update_instance_for_redefined_class),
    ('CHANGE-CLASS', [None, None], _default_change_class),
    ('SLOT-UNBOUND', [None, None, None], _default_slot_unbound),
]
def _make_installer(specializers, fn):
    return lambda gf: classes.add_method(gf, specializers, fn)


for _name, _specializers, _fn in _PROTOCOL_DEFAULTS:
    _installer = _make_installer(_specializers, _fn)
    classes.register_default_method_installer(_name, _installer)
    _installer(_protocol_gf(_name))
del _name, _specializers, _fn, _installer


# --- CLOS class and instance operations ---
@_registry.cl_function('FIND-CLASS')
def find_class(name, errorp=True, environment=None):
    """Find class by name; return NIL or raise LispError per `errorp`."""
    try:
        cls = classes.find_class(name)
        if cls is None:
            if errorp is True or errorp is lisptype.T:
                raise lisptype.LispError(f"Class not found: {name}")
            return lisptype.NIL
        return cls
    except Exception as e:
        if errorp is True or errorp is lisptype.T:
            raise lisptype.LispError(str(e))
        return lisptype.NIL


@_registry.cl_function('MAKE-INSTANCE')
def make_instance(class_designator, *initargs):
    """Create an instance using classes.make_instance. Initargs currently ignored."""
    try:
        # classes.make_instance accepts a LispSymbol or string
        return classes.make_instance(class_designator)
    except Exception as e:
        raise lisptype.LispError(str(e))


@_registry.cl_function('ALLOCATE-INSTANCE')
def allocate_instance(class_obj, *initargs):
    """Allocate a raw instance: dispatches through the ALLOCATE-INSTANCE
    generic function so a user DEFMETHOD on it (rare, but legal CLHS 7.1.1)
    actually runs."""
    return classes.call_generic_function(_protocol_gf('ALLOCATE-INSTANCE'),
                                          [_resolve_class(class_obj)] + list(initargs))


@_registry.cl_function('INITIALIZE-INSTANCE')
def initialize_instance(instance, *initargs):
    return classes.call_generic_function(_protocol_gf('INITIALIZE-INSTANCE'), [instance] + list(initargs))


@_registry.cl_function('REINITIALIZE-INSTANCE')
def reinitialize_instance(instance, *initargs):
    return classes.call_generic_function(_protocol_gf('REINITIALIZE-INSTANCE'), [instance] + list(initargs))


@_registry.cl_function('SHARED-INITIALIZE')
def shared_initialize(instance, slot_names, *initargs):
    return classes.call_generic_function(_protocol_gf('SHARED-INITIALIZE'), [instance, slot_names] + list(initargs))


@_registry.cl_function('UPDATE-INSTANCE-FOR-DIFFERENT-CLASS')
def update_instance_for_different_class(previous, current, *initargs):
    return classes.call_generic_function(_protocol_gf('UPDATE-INSTANCE-FOR-DIFFERENT-CLASS'),
                                          [previous, current] + list(initargs))


@_registry.cl_function('UPDATE-INSTANCE-FOR-REDEFINED-CLASS')
def update_instance_for_redefined_class(instance, added_slots=None, discarded_slots=None, property_list=None, *initargs):
    args = [instance,
            added_slots if added_slots is not None else lisptype.NIL,
            discarded_slots if discarded_slots is not None else lisptype.NIL,
            property_list if property_list is not None else lisptype.NIL] + list(initargs)
    return classes.call_generic_function(_protocol_gf('UPDATE-INSTANCE-FOR-REDEFINED-CLASS'), args)


@_registry.cl_function('CLASS-OF')
def class_of(object):
    """Return the LispClass of an instance, or Python type otherwise."""
    if isinstance(object, classes.LispInstance):
        return object.lisp_class
    return type(object)


@_registry.cl_function('CLASS-NAME')
def class_name(class_obj):
    """Return the class name symbol where possible."""
    if isinstance(class_obj, classes.LispClass):
        return class_obj.name
    return getattr(class_obj, '__name__', str(class_obj))


@_registry.cl_function('CHANGE-CLASS')
def change_class(instance, new_class, *initargs):
    """CHANGE-CLASS is itself a standard generic function (CLHS 7.2), the
    same shape as MAKE-INSTANCE above and for the same reason:
    ansi-test's change-class.lsp defines DEFMETHODs directly on it
    (including :before/:after methods), which replaces its *entire*
    environment binding with the bare GenericFunction object the moment
    the first one is evaluated -- so this thin wrapper only matters for a
    direct Python call; every Lisp-level call after that point reaches
    `_default_change_class` (or a more specific user method) directly.
    """
    return classes.call_generic_function(_protocol_gf('CHANGE-CLASS'), [instance, new_class] + list(initargs))


@_registry.cl_function('BUILT-IN-CLASS')
def built_in_class():
    return lisptype.T


@_registry.cl_function('STANDARD-CLASS')
def standard_class():
    return lisptype.T


@_registry.cl_function('STANDARD-OBJECT')
def standard_object():
    return lisptype.T


@_registry.cl_function('STRUCTURE-CLASS')
def structure_class():
    return lisptype.NIL


@_registry.cl_function('STRUCTURE-OBJECT')
def structure_object():
    return lisptype.NIL


# --- Slot operations ---
@_registry.cl_function('SLOT-BOUNDP')
def slot_boundp(instance, slot_name):
    try:
        if isinstance(instance, classes.LispInstance):
            name = slot_name.name if hasattr(slot_name, 'name') else slot_name
            return lisptype.T if name in instance.slot_values else lisptype.NIL
    except Exception:
        pass
    return lisptype.NIL


@_registry.cl_function('SLOT-EXISTS-P')
def slot_exists_p(instance, slot_name):
    """SLOT-EXISTS-P (CLHS 7.5.4): does `instance`'s *class* define this
    slot -- not whether it currently holds a value. Those are different
    questions (SLOT-BOUNDP asks the second one): a freshly
    ALLOCATE-INSTANCE'd instance has every slot the class defines but no
    value for any of them yet, and this used to conflate the two by
    asking `instance.slot_values` (the *value* dict) instead of the
    class's slot definitions, which happened to work only because the old
    MAKE-INSTANCE always pre-populated every slot's dict entry (even to
    None) at creation time -- a real ALLOCATE-INSTANCE leaves it empty.
    """
    try:
        if isinstance(instance, classes.LispInstance):
            name = slot_name.name if hasattr(slot_name, 'name') else slot_name
            return lisptype.T if name in instance.lisp_class.get_all_slots() else lisptype.NIL
    except Exception:
        pass
    return lisptype.NIL


@_registry.cl_function('SLOT-MAKUNBOUND')
def slot_makunbound(instance, slot_name):
    try:
        if isinstance(instance, classes.LispInstance):
            name = slot_name.name if hasattr(slot_name, 'name') else slot_name
            if name in instance.slot_values:
                instance.slot_values.pop(name, None)
    except Exception:
        pass
    return instance


@_registry.cl_function('SLOT-UNBOUND')
def slot_unbound(class_obj, instance, slot_name):
    """SLOT-UNBOUND is itself a standard generic function (CLHS 7.5.5,
    called by SLOT-VALUE et al. when a slot has no value) -- the same
    shape as MAKE-INSTANCE/CHANGE-CLASS above: ansi-test's
    slot-unbound.lsp defines a DEFMETHOD on it directly."""
    return classes.call_generic_function(_protocol_gf('SLOT-UNBOUND'), [class_obj, instance, slot_name])


@_registry.cl_function('SLOT-VALUE')
def slot_value(instance, slot_name):
    try:
        name = slot_name.name if hasattr(slot_name, 'name') else slot_name
        return classes.slot_value(instance, name)
    except Exception as e:
        raise lisptype.LispError(str(e))


@_registry.cl_function('SLOT-MISSING')
def slot_missing(class_obj, instance, slot_name, operation, *args):
    raise lisptype.LispError(f"Missing slot {slot_name} on {instance}")


# --- Method operations ---
def _specifier_name(spec):
    """Best-effort name for one specializer or qualifier, for FIND-METHOD's
    loose by-name comparison (a full designator-for-designator congruence
    check per CLHS 7.6.3 is `classes._specializer_eq`, used by ADD-METHOD;
    FIND-METHOD's callers pass class objects or bare symbols far more often
    than EQL-specializer lists, so a name comparison covers the common
    case without duplicating that check)."""
    if spec is None:
        return 'T'
    if isinstance(spec, classes.LispClass):
        return spec.name.name.upper() if isinstance(spec.name, lisptype.LispSymbol) else str(spec.name).upper()
    if hasattr(spec, 'name'):
        return spec.name.upper().lstrip(':')
    return str(spec).upper()


@_registry.cl_function('FIND-METHOD')
def find_method(generic_function, qualifiers, specializers, errorp=True):
    """FIND-METHOD (CLHS 7.6.6): find a method by qualifiers and
    specializers. errorp defaults to T; NIL makes a miss return NIL
    instead of signaling."""
    from fclpy.lispfunc.core import _consp_internal, car, cdr

    if errorp is None:
        errorp = True

    def _to_list(lisp_list):
        out = []
        current = lisp_list
        while _consp_internal(current):
            out.append(car(current))
            current = cdr(current)
        return out

    q_list = _to_list(qualifiers)
    spec_list = _to_list(specializers)

    for method in getattr(generic_function, 'methods', []):
        m_quals = [_specifier_name(q) for q in method.qualifiers]
        if m_quals != [_specifier_name(q) for q in q_list]:
            continue
        m_specs = [_specifier_name(s) for s in method.specializers]
        if m_specs == [_specifier_name(s) for s in spec_list]:
            return method

    if errorp is True or errorp is lisptype.T:
        raise lisptype.LispError(f"No method found for specializers: {specializers}")
    return lisptype.NIL


@_registry.cl_function('DEFMETHOD')
def defmethod(name, *args):
    """Never actually runs DEFMETHOD: the real implementation is
    `evaluation_special_forms.eval_defmethod`, reached directly by
    `evaluation_core.eval`'s special-form dispatch before any function
    lookup happens, because DEFMETHOD's specialized-lambda-list and body
    must not be evaluated as ordinary call arguments. This registration
    exists only so `(fboundp 'defmethod)`/`SYMBOL-FUNCTION` see a binding,
    matching DEFGENERIC/DEFCONSTANT/DEFPACKAGE/DEFSTRUCT's identical
    placeholders in misc_macros.py."""
    return name


@_registry.cl_function('REMOVE-METHOD')
def remove_method(generic_function, method):
    return classes.remove_method(generic_function, method)


@_registry.cl_special('MAKE-METHOD')
def make_method(*args):
    """MAKE-METHOD (CLHS 7.6.6.2) is only meaningful inside an effective
    method, where its argument is a *form* to be run as a method body --
    evaluating that form as an ordinary argument (which is what the
    `cl_function` registration this replaced did, before returning None
    regardless) runs the method body at the wrong time and discards it.
    The evaluator implements it; this registration exists so the symbol is
    bound as a special operator."""
    raise lisptype.LispNotImplementedError(
        "MAKE-METHOD is a special operator handled by the evaluator")


@_registry.cl_function('METHOD-FUNCTION')
def method_function(method):
    return getattr(method, 'function', None)


@_registry.cl_function('METHOD-GENERIC-FUNCTION')
def method_generic_function(method):
    return getattr(method, 'generic_function', None)


@_registry.cl_function('METHOD-SPECIALIZERS')
def method_specializers(method):
    from fclpy.lispfunc.sequence_protocol import make_lisp_list
    specs = getattr(method, 'specializers', [])
    return make_lisp_list([lisptype.T if s is None else s for s in specs])


@_registry.cl_function('METHOD-LAMBDA-LIST')
def method_lambda_list(method):
    return getattr(method, 'lambda_list', None) or lisptype.NIL


@_registry.cl_function('METHOD-QUALIFIERS')
def method_qualifiers(method):
    from fclpy.lispfunc.sequence_protocol import make_lisp_list
    return make_lisp_list(getattr(method, 'qualifiers', []))


@_registry.cl_function('NEXT-METHOD-P')
def next_method_p():
    return lisptype.lisp_bool(classes.next_method_p())


@_registry.cl_function('NO-APPLICABLE-METHOD')
def no_applicable_method(generic_function, *arguments):
    raise lisptype.LispError("No applicable method")


@_registry.cl_function('NO-NEXT-METHOD')
def no_next_method(generic_function, method, *arguments):
    raise lisptype.LispError("No next method")


@_registry.cl_special('CALL-METHOD')
def call_method(*args):
    """CALL-METHOD (CLHS 7.6.6.2): `(call-method method [next-method-list])`
    inside an effective method. Neither operand is evaluated -- the method
    argument is a method *object* spliced in by the method combination, or
    a `(make-method form)` form -- and the arguments the method receives
    are the original generic-function call's, which the operands do not
    name. Registering it as a `cl_function` (what this replaced) evaluated
    both operands and then ignored `next-method-list` entirely, so
    CALL-NEXT-METHOD had no chain to walk. The evaluator implements it."""
    raise lisptype.LispNotImplementedError(
        "CALL-METHOD is a special operator handled by the evaluator")


@_registry.cl_function('CALL-NEXT-METHOD')
def call_next_method(*args):
    return classes.call_next_method(*args)


@_registry.cl_function('COMPUTE-APPLICABLE-METHODS')
def compute_applicable_methods(generic_function, arguments):
    """COMPUTE-APPLICABLE-METHODS (CLHS 7.6.6.1): every method applicable
    to `arguments`, most-specific-first -- the same selection
    `call_generic_function` uses, so this cannot disagree with what a real
    call would actually invoke."""
    from fclpy.lispfunc.core import _consp_internal, car, cdr
    from fclpy.lispfunc.sequence_protocol import make_lisp_list
    args_list = []
    current = arguments
    while _consp_internal(current):
        args_list.append(car(current))
        current = cdr(current)
    return make_lisp_list(classes.compute_applicable_methods(generic_function, args_list))


@_registry.cl_function('ENSURE-GENERIC-FUNCTION')
def ensure_generic_function(function_name, *options):
    try:
        return classes.ensure_generic_function(function_name)
    except Exception as e:
        raise lisptype.LispError(str(e))


@_registry.cl_function('GENERIC-FUNCTION-LAMBDA-LIST')
def generic_function_lambda_list(generic_function):
    return getattr(generic_function, 'lambda_list', [])


@_registry.cl_function('GENERIC-FUNCTION-METHODS')
def generic_function_methods(generic_function):
    from fclpy.lispfunc.sequence_protocol import make_lisp_list
    return make_lisp_list(getattr(generic_function, 'methods', []))


@_registry.cl_function('GENERIC-FUNCTION-NAME')
def generic_function_name(generic_function):
    return getattr(generic_function, 'name', str(generic_function))


__all__ = [
    # Class operations
    'find_class',
    'make_instance',
    'allocate_instance',
    'initialize_instance',
    'reinitialize_instance',
    'shared_initialize',
    'update_instance_for_different_class',
    'update_instance_for_redefined_class',
    'class_of',
    'class_name',
    'change_class',
    'built_in_class',
    'standard_class',
    'standard_object',
    'structure_class',
    'structure_object',
    # Slot operations
    'slot_boundp',
    'slot_exists_p',
    'slot_makunbound',
    'slot_unbound',
    'slot_value',
    'slot_missing',
    # Method operations
    'find_method',
    'remove_method',
    'defmethod',
    'make_method',
    'method_function',
    'method_generic_function',
    'method_specializers',
    'method_lambda_list',
    'method_qualifiers',
    'next_method_p',
    'no_applicable_method',
    'no_next_method',
    'call_method',
    'call_next_method',
    'compute_applicable_methods',
    'ensure_generic_function',
    'generic_function_lambda_list',
    'generic_function_methods',
    'generic_function_name',
]
