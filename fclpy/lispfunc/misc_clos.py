"""CLOS class, instance, slot, and method operations.

This module maps the Lisp-visible CLOS API into the more complete
implementation found in `fclpy.classes`. The goal is to provide
minimal, well-behaved bindings so the ANSI test-suite can define
classes and methods at load time without triggering assertions.
"""

import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry
from .core import _consp_internal
from .evaluation_core import ConditionException

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

    The name lookup uses COMMON-LISP as the home package, not the
    defaulting one in `lisptype.py_str_to_sym` (COMMON-LISP-USER) -- the
    protocol GFs are CLOS standard-generic-function names, and
    `(symbol-package 'make-instance)` in any conforming program reads
    CL, never CL-USER. Interning into CL-USER was a pre-existing
    side-effect of `py_str_to_sym` that polluted CL-USER's `symbols`
    dict and broke WITH-PACKAGE-ITERATOR's INTERNAL/EXTERNAL/INHERITED
    classification: every CL symbol showed up as an INTERNAL symbol of
    CL-USER, so the test harness's `(find-symbol name pkg) ==
    (car (multiple-value-list (x)))` was always false.
    """
    sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol(name)
    return classes.ensure_generic_function(sym)


def _resolve_class(class_spec):
    if isinstance(class_spec, classes.LispClass):
        return class_spec
    cls = classes.find_class(class_spec)
    if cls is None:
        raise lisptype.LispError(f"Class not found: {class_spec}")
    return cls


def _initarg_key(key):
    name = _initarg_name(key)
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


def _eval_in_definition_env(form, definition_env):
    """Evaluate a stored-unevaluated DEFCLASS form (a slot's :initform, or a
    :default-initargs default-value-form) in the environment DEFCLASS
    lexically saw, falling back to the global environment for the two cases
    with no such environment recorded: the bootstrap's own built-in classes,
    and MAKE-INSTANCE running before any environment has been set up at all
    (the unit-test suite calls it directly, without the bootstrap
    `run_all_tests.py`/`run_ansi.py` always do first --
    `setup_standard_environment()` is idempotent, so this is free once a
    real environment already exists).
    """
    from fclpy.lispfunc.evaluation_core import eval as _eval
    import fclpy.state as state
    env = definition_env or state.current_environment
    if env is None:
        import fclpy.lispenv as lispenv
        lispenv.setup_standard_environment()
        env = state.current_environment
    return _eval(form, env)


def _eval_initform(slot_def):
    return _eval_in_definition_env(slot_def.initform, slot_def.definition_env)


def _merge_default_initargs(cls, initargs):
    """CLHS 7.1.8: default initargs supply additional initialization
    arguments as if the caller had passed them, but only for an initarg
    name the call did not itself supply -- and each default-value-form is
    evaluated fresh here, at most once per MAKE-INSTANCE call, never
    memoized (ansi-test's class-28 default form is `(incf y)`, so
    evaluating it when the caller *did* supply `:s2` explicitly, or more
    than once, is directly observable).
    """
    initargs = list(initargs)
    supplied = {_initarg_key(k) for k in initargs[0::2]}
    for key, (initarg_sym, form, def_env) in cls.get_all_default_initargs().items():
        if key in supplied:
            continue
        initargs.append(initarg_sym)
        initargs.append(_eval_in_definition_env(form, def_env))
    return initargs


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

def _method_declared_initargs(instance, cls):
    """The initialization arguments declared valid by the lambda lists of
    applicable methods (CLHS 7.1.2, second means): the keyword parameter
    names of every method on ALLOCATE-INSTANCE, INITIALIZE-INSTANCE,
    SHARED-INITIALIZE, REINITIALIZE-INSTANCE,
    UPDATE-INSTANCE-FOR-DIFFERENT-CLASS and
    UPDATE-INSTANCE-FOR-REDEFINED-CLASS applicable to this instance, plus
    whether any of them carries &allow-other-keys -- whose presence, per
    that section, disables validity checking."""
    valid = set()
    allow_all = False
    for gf_name, first_arg in (
            ('ALLOCATE-INSTANCE', cls),
            ('INITIALIZE-INSTANCE', instance),
            ('SHARED-INITIALIZE', instance),
            ('REINITIALIZE-INSTANCE', instance),
            ('UPDATE-INSTANCE-FOR-DIFFERENT-CLASS', instance),
            ('UPDATE-INSTANCE-FOR-REDEFINED-CLASS', instance)):
        try:
            gf = classes._generic_registry.find_generic(
                classes.generic_function_key(
                    lisptype.COMMON_LISP_PACKAGE.intern_symbol(gf_name)))
            if gf is None:
                continue
            for method in classes.compute_applicable_methods(gf, [first_arg]):
                ll = getattr(method, 'lambda_list', None)
                if ll is None:
                    continue
                from .evaluation_core import parse_lambda_list
                tail = parse_lambda_list(ll)
                if tail.get('allow_other_keys'):
                    allow_all = True
                from .evaluation_special_forms import (_keyword_param_parts,
                                                       keyword_argument_key)
                for spec in tail.get('keyword', []):
                    keyword = _keyword_param_parts(spec)[0]
                    valid.add(keyword_argument_key(keyword)[1].upper())
        except Exception:
            continue
    return valid, allow_all


def _initarg_name(key):
    """The upper-case name of one initialization-argument name symbol. NIL
    itself is a legal initarg name (class-14's `:initarg nil`), so the
    lispNull spelling must normalize to 'NIL' like any other symbol."""
    if key is lisptype.NIL or isinstance(key, lisptype.lispNull):
        return 'NIL'
    name = getattr(key, 'name', None)
    if isinstance(name, str):
        return name.upper().lstrip(':')
    return str(key).upper()


def _check_initargs_valid(instance, cls, initargs):
    """CLHS 7.1.2: an initialization argument supplied to any of the four
    initialization situations (making, reinitializing, redefined-class
    updating, different-class updating) must be valid -- declared by a
    slot's :initarg (inherited over the whole CPL), by :default-initargs,
    or by the lambda list of an applicable method on one of the six
    protocol generic functions -- or an error is signaled. A true
    :allow-other-keys in the call, or &allow-other-keys in an applicable
    method's lambda list, disables the check. `initargs` is the flat
    keyword/value plist the situation received."""
    flat = list(initargs)
    if len(flat) % 2:
        raise lisptype.LispProgramError(
            "odd number of initialization arguments: "
            f"{flat[-1]!r} has no value")

    supplied_keys = []
    aok_value = False
    aok_seen = False
    i = 0
    while i < len(flat):
        key = flat[i]
        # A plain Python string is the Python-direct-call spelling of an
        # initarg name (the unit-test suite calls MAKE-INSTANCE with
        # **kwargs); only *non-string, non-symbol* names are malformed.
        if not (lisptype.is_symbol(key) or isinstance(key, str)):
            raise lisptype.LispProgramError(
                f"{key!r} is not a valid initialization argument name")
        name = _initarg_name(key)
        supplied_keys.append(name)
        if name == 'ALLOW-OTHER-KEYS' and not aok_seen:
            # CLHS 3.4.1.4.1: the leftmost :allow-other-keys pair governs.
            aok_seen = True
            aok_value = lisptype.is_truthy(flat[i + 1])
        i += 2

    if aok_value:
        return

    valid = set()
    valid.add('ALLOW-OTHER-KEYS')
    for slot_def in cls.get_all_slots().values():
        for initarg in slot_def.initargs:
            valid.add(_initarg_key(initarg))
    for initarg_sym, _form, _env in cls.get_all_default_initargs().values():
        valid.add(_initarg_key(initarg_sym))
    declared, allow_all = _method_declared_initargs(instance, cls)
    valid.update(declared)

    if allow_all:
        return
    unknown = [name for name in supplied_keys if name not in valid]
    if unknown:
        raise lisptype.LispError(
            "not a valid initialization argument for "
            f"{cls.name.name}: :{unknown[0]}")


def _check_no_builtin_instance(obj, what):
    """CLHS 4.3.7 (BUILT-IN-CLASS): calling MAKE-INSTANCE to create a
    generalized instance of a built-in class, or CHANGE-CLASS to or from
    one, signals an error. The metaclass decides: an instance of
    built-in-class is what that page means by a built-in class."""
    if (isinstance(obj, classes.LispClass)
            and getattr(obj, 'metaclass_name', 'STANDARD-CLASS') == 'BUILT-IN-CLASS'):
        raise lisptype.LispError(
            f"{what}: {obj.name.name} is a built-in class; the operation "
            f"is not permitted on it")


def _default_make_instance(class_obj, *initargs):
    """MAKE-INSTANCE's default method (CLHS 7.1): resolve a symbol/string
    designator to its class, then allocate, check the validity of the
    initialization arguments (CLHS 7.1.2's "making an instance" situation),
    and initialize.

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
    _check_no_builtin_instance(class_obj, 'MAKE-INSTANCE')
    # CLHS 7.1.8: default-initargs are merged in here, once, by MAKE-INSTANCE
    # itself -- before ALLOCATE-INSTANCE/INITIALIZE-INSTANCE run -- so every
    # later step in the protocol (including a user :around method) sees them
    # exactly as if the caller had supplied them.
    initargs = _merge_default_initargs(class_obj, initargs)
    instance = classes.call_generic_function(_protocol_gf('ALLOCATE-INSTANCE'), [class_obj] + list(initargs))
    if isinstance(instance, classes.LispInstance):
        _check_initargs_valid(instance, instance.lisp_class, initargs)
    classes.call_generic_function(_protocol_gf('INITIALIZE-INSTANCE'), [instance] + list(initargs))
    return instance


def _default_allocate_instance(cls, *initargs):
    # A class cannot be instantiated until it is finalized, and it cannot be
    # finalized while any superclass at or above it is still only a
    # forward-referenced name (CLHS 4.3.6/4.3.7). Allocating anyway would hand
    # back an instance whose slot set and class precedence list are both wrong
    # -- and silently, since the missing superclass contributes nothing.
    pending = cls.unfinalized_superclasses() if isinstance(cls, classes.LispClass) else []
    if pending:
        names = ", ".join(sorted(c.name.name for c in pending))
        raise lisptype.LispError(
            f"ALLOCATE-INSTANCE: class {cls.name.name} is not finalized -- "
            f"undefined superclass(es): {names}")
    return classes.LispInstance(lisp_class=cls)


def _default_shared_initialize(instance, slot_names, *initargs):
    """SHARED-INITIALIZE's default method (CLHS 7.1.2): fill the slots
    slot-names selects, and every slot an initarg of this call names.

    The plist shape is checked here -- an odd number of keyword arguments
    is a PROGRAM-ERROR (CLHS 3.4.1.4; shared-initialize.error.3/.4 call
    shared-initialize directly with a broken plist) -- but the *validity*
    of the initialization arguments is not: CLHS 7.1.2 puts that check in
    the four *situations* (make-instance, reinitialize-instance,
    update-instance-for-redefined-class, update-instance-for-different-
    class), and shared-initialize.6.8 passes an undeclared `:foo` that the
    situation's own check accepts by the &allow-other-keys rule.
    """
    flat = list(initargs)
    if len(flat) % 2:
        raise lisptype.LispProgramError(
            "odd number of initialization arguments: "
            f"{flat[-1]!r} has no value")
    for i in range(0, len(flat), 2):
        if not (lisptype.is_symbol(flat[i]) or isinstance(flat[i], str)):
            # CLHS 3.4.1.4: a keyword argument name that is not a symbol is
            # a PROGRAM-ERROR in the callee's own argument processing
            # (shared-initialize.error.4). A plain Python string is the
            # Python-direct-call spelling of a name, not a malformed one.
            raise lisptype.LispProgramError(
                f"{flat[i]!r} is not a valid initialization argument name")
    initarg_map = _initargs_to_map(initargs)
    initarg_positions = _initargs_to_positions(initargs)
    cls = instance.lisp_class
    for name, slot_def in cls.get_all_slots().items():
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
            value = initarg_map[winner]
            supplied = True
        else:
            # A slot with no declared :initarg still accepts a same-named
            # keyword as a convenience (predates this rewrite; no ANSI test
            # can rely on it, since real CLHS-conforming code always declares
            # the initarg it uses, but existing direct-Python callers of
            # MAKE-INSTANCE do).
            if name in initarg_map:
                value = initarg_map[name]
                supplied = True

        if slot_def.allocation == "class":
            defining_class = cls.find_slot_definition_class(name) or cls
            already_bound = name in defining_class.class_slots
        else:
            defining_class = None
            already_bound = name in instance.slot_values

        # CLHS 7.1.2 / 7.5.3: an initarg always (re)sets the slot. Absent
        # one, the slot's initform applies only when slot-names selects it
        # AND the slot is not already bound -- shared-initialize.1.10 sets
        # slot A to 1000 then calls (shared-initialize obj '(a)) and requires
        # A to still be 1000 afterward, not reset to its initform.
        if supplied or (not already_bound and slot_def.initform is not None
                        and _slot_names_selects(slot_names, name)):
            if not supplied:
                value = _eval_initform(slot_def)

            if slot_def.allocation == "class":
                # Store in the class that defined this slot
                defining_class.class_slots[name] = value
            else:
                instance.slot_values[name] = value
    return instance


def _default_initialize_instance(instance, *initargs):
    classes.call_generic_function(_protocol_gf('SHARED-INITIALIZE'), [instance, lisptype.T] + list(initargs))
    return instance


def _default_reinitialize_instance(instance, *initargs):
    """REINITIALIZE-INSTANCE's default method (CLHS 7.1.5): check the
    validity of the initialization arguments (the "re-initializing an
    instance" situation of CLHS 7.1.2 -- reinitialize-instance.error.1's
    `:garbage` must signal, while the `:x` of reinitialize-instance.9 is
    declared valid by the :after method's `&key (x nil x-p)`) and then
    shared-initialize only what the initargs name."""
    _check_initargs_valid(instance, instance.lisp_class, initargs)
    classes.call_generic_function(_protocol_gf('SHARED-INITIALIZE'), [instance, lisptype.NIL] + list(initargs))
    return instance


def _default_update_instance_for_different_class(previous, current, *initargs):
    """UPDATE-INSTANCE-FOR-DIFFERENT-CLASS's default method (CLHS 7.2.2):
    shared-initialize the new instance with the *added* slots -- those of
    the new class that name no slot of the old class -- plus the initargs
    change-class was given. (The old computation here, "every unbound slot
    of the new class", re-initialized slots the two classes share and
    wrongly initialized slots an allocate-instance'd same-class change had
    no business touching -- change-class.4.5.)"""
    old_names = set(previous.lisp_class.get_all_slots())
    added = [name for name in current.lisp_class.get_all_slots()
             if name not in old_names]
    # The call happens whether or not any slot was added (CLHS 7.2.2): the
    # initargs can still initialize *shared* slots of the new class --
    # change-class.7.3 passes `:b 10` across a same-class change and
    # requires B to receive it.
    from fclpy.lispfunc.sequence_protocol import make_lisp_list
    added_list = make_lisp_list([lisptype.COMMON_LISP_PACKAGE.intern_symbol(n) for n in added])
    classes.call_generic_function(_protocol_gf('SHARED-INITIALIZE'), [current, added_list] + list(initargs))
    return current


def _default_update_instance_for_redefined_class(instance, added_slots, discarded_slots, property_list, *initargs):
    _check_initargs_valid(instance, instance.lisp_class, initargs)
    classes.call_generic_function(_protocol_gf('SHARED-INITIALIZE'), [instance, added_slots] + list(initargs))
    return instance


def _default_change_class(instance, new_class, *initargs):
    """CHANGE-CLASS's default method (CLHS 7.2): change instance's class
    in place, keeping the values of slots the old and new class share by
    name (a class-allocated old slot's value is one of those: it lives on
    the class that allocated it, not in instance.slot_values), discarding
    the rest, and running UPDATE-INSTANCE-FOR-DIFFERENT-CLASS -- whose
    default method fills in the *added* slots (the new class's slots that
    name no slot of the old class, CLHS 7.2.2) via SHARED-INITIALIZE.
    This replaces just swapping `.lisp_class` and leaving every
    newly-added slot unbound and never initialized.
    """
    if not isinstance(instance, classes.LispInstance):
        raise lisptype.LispTypeError(f"CHANGE-CLASS: not an instance: {instance}")
    new_cls = _resolve_class(new_class)
    # CLHS 4.3.7 (BUILT-IN-CLASS): changing the class of an object to or
    # from a built-in class signals an error (change-class.error.5).
    _check_no_builtin_instance(new_cls, 'CHANGE-CLASS')
    _check_no_builtin_instance(instance.lisp_class, 'CHANGE-CLASS')

    old_class = instance.lisp_class
    old_all_slots = old_class.get_all_slots()
    old_slot_values = {}
    for name, slot_def in old_all_slots.items():
        if slot_def.allocation == "class":
            defining_class = old_class.find_slot_definition_class(name) or old_class
            if name in defining_class.class_slots:
                old_slot_values[name] = defining_class.class_slots[name]
        else:
            if name in instance.slot_values:
                old_slot_values[name] = instance.slot_values[name]

    new_all_slots = new_cls.get_all_slots()
    # CLHS 7.2.2: `previous` is a snapshot of the instance as it was in its
    # old class, passed to UPDATE-INSTANCE-FOR-DIFFERENT-CLASS for
    # introspection (e.g. reading a slot the new class discarded).
    previous = classes.LispInstance(lisp_class=old_class,
                                    slot_values=dict(old_slot_values))

    instance.lisp_class = new_cls
    instance.slot_values = {name: val for name, val in old_slot_values.items()
                            if name in new_all_slots}

    _check_initargs_valid(instance, new_cls, initargs)
    classes.call_generic_function(_protocol_gf('UPDATE-INSTANCE-FOR-DIFFERENT-CLASS'),
                                   [previous, instance] + list(initargs))
    return instance


def _default_slot_unbound(class_obj, instance, slot_name):
    # Pass the slot_name symbol (not converted to string) to the condition
    cond = lisptype.UnboundSlot(name=slot_name, instance=instance)
    raise ConditionException(cond, recoverable=False)


def _default_slot_missing(class_obj, instance, slot_name, operation, *new_value):
    """SLOT-MISSING's default method (CLHS 7.5.3): called by SLOT-VALUE,
    (SETF SLOT-VALUE), SLOT-BOUNDP and SLOT-MAKUNBOUND when slot-name does
    not name any slot the instance's class defines at all -- distinct from
    SLOT-UNBOUND, where the slot is defined but simply has no value yet.
    The standard requires the default method to signal an error."""
    raise lisptype.LispError(f"The slot {slot_name} is missing from {instance}.")


def _default_make_load_form(object_arg, environment=None):
    """MAKE-LOAD-FORM's default method (CLHS 3.2.4): signals an error.

    MAKE-LOAD-FORM is a generic function that objects may specialize to
    provide custom serialization forms. The default method signals an error
    because most objects have no portable load form. User code can define
    methods specializing on their own classes to provide custom behavior.
    """
    raise lisptype.LispError(f"No MAKE-LOAD-FORM method for {object_arg}")


def _default_class_name(class_obj):
    """CLASS-NAME's default method (CLHS 7.6.15): return the class name.

    CLASS-NAME is a generic function that returns the name of a class.
    User code can specialize it to customize behavior for their classes.
    """
    if not isinstance(class_obj, classes.LispClass):
        raise lisptype.LispTypeError(f"CLASS-NAME: expected a class, got {class_obj}",
                                    expected_type="CLASS", actual_value=class_obj)
    return class_obj.name


def _default_set_class_name(new_name, class_obj):
    """(SETF CLASS-NAME)'s default method (CLHS 7.6.15): set the class name.

    The writer for CLASS-NAME allows renaming a class at runtime.
    """
    if not isinstance(class_obj, classes.LispClass):
        raise lisptype.LispTypeError(f"(SETF CLASS-NAME): expected a class, got {class_obj}",
                                    expected_type="CLASS", actual_value=class_obj)
    class_obj.name = new_name
    return new_name


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
def _describe_object_default():
    """DESCRIBE-OBJECT's default method, fetched lazily.

    Imported at call time rather than at module scope because `misc_macros`
    imports the printer and the stream machinery, and this module is imported
    from the CLOS bootstrap -- a module-level import would close the cycle.
    """
    def _describe(obj, stream):
        from .misc_macros import default_describe_object
        return default_describe_object(obj, stream)
    return _describe


# --- DOCUMENTATION / (SETF DOCUMENTATION) (CLHS 25.1.3) ---
#
# Both are standard *generic functions*, so a user DEFMETHOD on either name
# overrides these defaults by ordinary dispatch (ansi-test's
# environment/documentation.lsp defines exactly such methods). The plain
# `cl_function` this replaced read only a symbol's plist and answered NIL
# for every function object, class, package and method -- 57 of that file's
# 58 tests failed on it.
#
# The doc-type designator (CLHS 25.1.3's `doc-type`) is normalized through
# one helper: T (the default), or a symbol naming FUNCTION,
# COMPILER-MACRO, SETF, VARIABLE, TYPE, STRUCTURE or METHOD-COMBINATION.

_DOC_TYPE_DEFAULT = 'FUNCTION'


def _doc_type_name(doc_type):
    """Normalize a DOCUMENTATION doc-type designator to its upper-case name,
    or None when no doc-type was supplied (CLHS: `(documentation x)` is not a
    legal call -- doc-type is required -- but the old stub tolerated omission,
    defaulting to FUNCTION; keep that tolerance)."""
    if doc_type is None or doc_type is lisptype.NIL:
        return _DOC_TYPE_DEFAULT
    if doc_type is lisptype.T:
        return 'T'
    if isinstance(doc_type, lisptype.LispSymbol):
        return doc_type.name.upper()
    return str(doc_type).upper()


def _symbol_doc(symbol, kind):
    """Read one documentation string off a symbol's property list.

    DEFUN/DEFMACRO store under 'DOCUMENTATION'; DEFVAR/DEFPARAMETER/
    DEFCONSTANT store both 'DOCUMENTATION' and 'VARIABLE-DOCUMENTATION'.
    Reading by *kind* key keeps a function docstring from answering for
    `(documentation sym 'variable)` once both kinds exist on one symbol.
    """
    plist = getattr(symbol, 'plist', None)
    if isinstance(plist, dict):
        doc = plist.get(kind)
        if doc:
            return doc
    return lisptype.NIL


def _set_symbol_doc(symbol, kind, doc):
    """Write one documentation string onto a symbol's property list."""
    if not hasattr(symbol, 'plist') or not isinstance(symbol.plist, dict):
        symbol.plist = {}
    symbol.plist[kind] = doc
    return doc


def _default_documentation(x, doc_type=None):
    """DOCUMENTATION's default method (CLHS 25.1.3).

    Dispatches on what `x` actually is -- a function object, class, package,
    method, generic function, method combination, or a symbol naming one of
    those -- rather than only ever consulting a symbol plist.
    """
    import fclpy.classes as classes

    kind = _doc_type_name(doc_type)

    # A symbol names the thing whose documentation is wanted (CLHS: "x -- an
    # object ... or a symbol denoting one"). The plist keys follow the
    # defining forms' existing convention.
    if isinstance(x, lisptype.LispSymbol):
        if kind == 'VARIABLE':
            return _symbol_doc(x, 'VARIABLE-DOCUMENTATION')
        if kind == 'TYPE':
            # A type symbol may name a class (DEFCLASS/DEFSTRUCT), a DEFTYPE,
            # or neither; the class's own doc answers first.
            cls = classes.find_class(x.name)
            if cls is not None and getattr(cls, 'documentation', None):
                return cls.documentation
            # Then a DEFTYPE's stored docstring.
            import fclpy.state as _state
            _genv = _state.current_environment
            if _genv is not None:
                while _genv.parent is not None:
                    _genv = _genv.parent
                entry = getattr(_genv, 'user_types', {}).get(x.name)
                if entry and entry.get('documentation'):
                    return entry['documentation']
            return _symbol_doc(x, 'TYPE-DOCUMENTATION')
        if kind == 'STRUCTURE':
            cls = classes.find_class(x.name)
            if cls is not None and getattr(cls, 'documentation', None):
                return cls.documentation
            return _symbol_doc(x, 'STRUCTURE-DOCUMENTATION')
        if kind == 'SETF':
            return _symbol_doc(x, 'SETF-DOCUMENTATION')
        if kind == 'METHOD-COMBINATION':
            comb = classes.find_method_combination_type(x)
            if comb is not None and getattr(comb, 'documentation', None):
                return comb.documentation
            return _symbol_doc(x, 'METHOD-COMBINATION-DOCUMENTATION')
        if kind == 'COMPILER-MACRO':
            # CLHS 25.1.3: a compiler macro's documentation is its own --
            # separate from the function documentation a DEFUN on the same
            # name stores (define-compiler-macro.5/.6 define both and
            # require the answers not to interfere).
            return _symbol_doc(x, 'COMPILER-MACRO-DOCUMENTATION')
        # FUNCTION (and T): the symbol's function documentation. A generic
        # function named by the symbol carries its own doc too.
        gf = classes._generic_registry.find_generic(
            classes.generic_function_key(x))
        if gf is not None and getattr(gf, 'documentation', None):
            return gf.documentation
        return _symbol_doc(x, 'DOCUMENTATION')

    # Function objects: ordinary functions carry their docstring as __doc__
    # via make_ordinary_function; generic functions have a .documentation.
    if isinstance(x, classes.GenericFunction):
        return x.documentation if x.documentation else lisptype.NIL
    # CLHS 25.1.3: `x` may be a *function name* -- a symbol or a `(SETF
    # symbol)` list -- as well as a function object.
    if _consp_internal(x):
        from .utilities_functions import _function_spec_to_key
        key = _function_spec_to_key(x)
        if key is not None:
            return _symbol_doc(key, 'DOCUMENTATION')
        return lisptype.NIL
    if callable(x):
        doc = getattr(x, '__doc__', None)
        if doc:
            return doc
        return lisptype.NIL

    # Classes (standard-class / structure-class): doc-type T or TYPE.
    if isinstance(x, classes.LispClass):
        return x.documentation if x.documentation else lisptype.NIL

    # Methods.
    if isinstance(x, classes.Method):
        return x.documentation if x.documentation else lisptype.NIL

    # Method combinations.
    if isinstance(x, classes.MethodCombinationType):
        return x.documentation if x.documentation else lisptype.NIL

    # Packages.
    if isinstance(x, lisptype.Package):
        return x.documentation if x.documentation else lisptype.NIL

    return lisptype.NIL


def _default_set_documentation(doc, x, doc_type=None):
    """(SETF DOCUMENTATION)'s default method (CLHS 25.1.3): store `doc` where
    `_default_documentation` will find it again, and return `doc` (the SETF
    form's value is the new documentation string)."""
    import fclpy.classes as classes

    kind = _doc_type_name(doc_type)

    if isinstance(x, lisptype.LispSymbol):
        if kind == 'VARIABLE':
            return _set_symbol_doc(x, 'VARIABLE-DOCUMENTATION', doc)
        if kind == 'TYPE':
            cls = classes.find_class(x.name)
            if cls is not None:
                cls.documentation = doc
                return doc
            return _set_symbol_doc(x, 'TYPE-DOCUMENTATION', doc)
        if kind == 'STRUCTURE':
            cls = classes.find_class(x.name)
            if cls is not None:
                cls.documentation = doc
                return doc
            return _set_symbol_doc(x, 'STRUCTURE-DOCUMENTATION', doc)
        if kind == 'SETF':
            return _set_symbol_doc(x, 'SETF-DOCUMENTATION', doc)
        if kind == 'METHOD-COMBINATION':
            comb = classes.find_method_combination_type(x)
            if comb is not None:
                comb.documentation = doc
                return doc
            return _set_symbol_doc(x, 'METHOD-COMBINATION-DOCUMENTATION', doc)
        # FUNCTION / T
        gf = classes._generic_registry.find_generic(
            classes.generic_function_key(x))
        if gf is not None:
            gf.documentation = doc
            return doc
        return _set_symbol_doc(x, 'DOCUMENTATION', doc)

    if isinstance(x, classes.GenericFunction):
        x.documentation = doc
        return doc
    # CLHS 25.1.3: `x` may be a *function name* -- a symbol or a `(SETF
    # symbol)` list -- as well as a function object. documentation.list.*
    # passes the list form; store under the same synthetic "(SETF F)"
    # key DEFUN uses for such names.
    if _consp_internal(x):
        from .utilities_functions import _function_spec_to_key
        key = _function_spec_to_key(x)
        if key is not None:
            return _set_symbol_doc(key, 'DOCUMENTATION', doc)
        raise lisptype.LispError(
            f"(SETF DOCUMENTATION): {x!r} does not name a function")
    if callable(x):
        x.__doc__ = doc
        return doc
    if isinstance(x, classes.LispClass):
        x.documentation = doc
        return doc
    if isinstance(x, classes.Method):
        x.documentation = doc
        return doc
    if isinstance(x, classes.MethodCombinationType):
        x.documentation = doc
        return doc
    if isinstance(x, lisptype.Package):
        x.documentation = doc
        return doc

    raise lisptype.LispError(
        f"(SETF DOCUMENTATION): cannot set documentation on {x!r}")


# Default methods for the remaining CLHS 7 standard generic functions. Each
# delegates, at call time, to the same Python implementation the plain
# `cl_function` registration carries -- which is what makes these real
# generic functions (CLHS 7.7: every one of these names is a standard
# generic function) without duplicating the mechanism: the symbol's function
# cell holds the GenericFunction object, and the implementation runs as its
# default (unspecialized) method. ansi-test's
# all-standard-generic-functions-are-instances-of-that-class checks exactly
# that: `(symbol-function 'add-method)` must be TYPEP of both
# GENERIC-FUNCTION and STANDARD-GENERIC-FUNCTION.
#
# The delegations resolve the implementations through module globals (and
# lazy imports for the two that live in other modules) because the wrappers
# are defined before the implementations are.

def _default_add_method(gf, method):
    return add_method(gf, method)


def _default_remove_method(gf, method):
    return remove_method(gf, method)


def _default_find_method(gf, qualifiers, specializers, errorp=True):
    return find_method(gf, qualifiers, specializers, errorp)


def _default_method_qualifiers(method):
    return method_qualifiers(method)


def _default_no_applicable_method(gf, *arguments):
    return no_applicable_method(gf, *arguments)


def _default_no_next_method(gf, method, *arguments):
    return no_next_method(gf, method, *arguments)


def _default_compute_applicable_methods(gf, arguments):
    return compute_applicable_methods(gf, arguments)


def _default_make_instances_obsolete(cls):
    return make_instances_obsolete(cls)


def _default_print_object(obj, stream):
    from .misc_macros import print_object as _print_object
    return _print_object(obj, stream)


def _default_function_keywords(method):
    from .utilities_functions import function_keywords as _function_keywords
    return _function_keywords(method)


# The CLHS lambda list of each protocol generic function (CLHS 7.1/7.2/7.5's
# own lambda lists). Installed on the generic function when the default
# method is, so that (a) a user DEFMETHOD on these names is congruence-
# checked against the lambda list the standard gives it, and (b) the CLHS
# 7.6.5 keyword-argument rule -- "the set of keyword arguments accepted ...
# is the union of the keyword arguments accepted by all applicable methods
# and the keyword arguments mentioned after &key in the generic function
# definition" -- can see the &key/&allow-other-keys the standard puts in
# these lambda lists (reinitialize-instance.8's :after method binds &key
# while the call passes other keywords, which only the generic function's
# own &allow-other-keys makes legal).
_PROTOCOL_DEFAULTS = [
    ('MAKE-INSTANCE', [None], _default_make_instance,
     '(class &rest initargs &key &allow-other-keys)'),
    ('ALLOCATE-INSTANCE', [None], _default_allocate_instance,
     '(class &rest initargs &key &allow-other-keys)'),
    ('INITIALIZE-INSTANCE', [None], _default_initialize_instance,
     '(instance &rest initargs &key &allow-other-keys)'),
    ('REINITIALIZE-INSTANCE', [None], _default_reinitialize_instance,
     '(instance &rest initargs &key &allow-other-keys)'),
    ('SHARED-INITIALIZE', [None, None], _default_shared_initialize,
     '(instance slot-names &rest initargs &key &allow-other-keys)'),
    ('UPDATE-INSTANCE-FOR-DIFFERENT-CLASS', [None, None],
     _default_update_instance_for_different_class,
     '(previous current &rest initargs &key &allow-other-keys)'),
    ('UPDATE-INSTANCE-FOR-REDEFINED-CLASS', [None],
     _default_update_instance_for_redefined_class,
     '(instance added-slots discarded-slots property-list &rest initargs &key &allow-other-keys)'),
    ('CHANGE-CLASS', [None, None], _default_change_class,
     '(instance new-class &rest initargs &key &allow-other-keys)'),
    ('SLOT-UNBOUND', [None, None, None], _default_slot_unbound,
     '(class instance slot-name)'),
    ('SLOT-MISSING', [None, None, None, None], _default_slot_missing,
     '(class instance slot-name operation &optional new-value)'),
    # MAKE-LOAD-FORM (CLHS 3.2.4) is a generic function so users can define
    # methods that provide custom serialization forms for their objects.
    # The environment parameter is optional, so only one specializer.
    ('MAKE-LOAD-FORM', [None], _default_make_load_form,
     '(object &optional environment)'),
    # CLASS-NAME (CLHS 7.6.15) is a generic function that returns/sets class names
    ('CLASS-NAME', [None], _default_class_name, None),
    ('(SETF CLASS-NAME)', [None, None], _default_set_class_name, None),
    # DESCRIBE-OBJECT (CLHS 25.1.2) is a generic function for the same reason
    # the metaobject protocol operations above are: `(defmethod
    # describe-object ((x my-class) stream) ...)` is the specified way to
    # describe your own objects, and a plain `cl_function` registration is
    # something no DEFMETHOD can reach.
    ('DESCRIBE-OBJECT', [None, None], _describe_object_default(),
     '(object stream)'),
    # DOCUMENTATION and its SETF writer (CLHS 25.1.3) -- see the block comment
    # above `_default_documentation`.
    ('DOCUMENTATION', [None, None], _default_documentation, None),
    ('(SETF DOCUMENTATION)', [None, None, None], _default_set_documentation, None),
    # The CLHS 7.6 method-protocol generic functions -- see the block comment
    # above `_default_add_method`.
    ('ADD-METHOD', [None, None], _default_add_method,
     '(generic-function method)'),
    ('REMOVE-METHOD', [None, None], _default_remove_method,
     '(generic-function method)'),
    ('FIND-METHOD', [None, None, None], _default_find_method,
     '(generic-function qualifiers specializers &optional errorp)'),
    ('METHOD-QUALIFIERS', [None], _default_method_qualifiers,
     '(method)'),
    ('NO-APPLICABLE-METHOD', [None], _default_no_applicable_method,
     '(generic-function &rest args)'),
    ('NO-NEXT-METHOD', [None, None], _default_no_next_method,
     '(generic-function method &rest args)'),
    ('COMPUTE-APPLICABLE-METHODS', [None, None], _default_compute_applicable_methods,
     '(generic-function arguments)'),
    ('MAKE-INSTANCES-OBSOLETE', [None], _default_make_instances_obsolete,
     '(class)'),
    # CLHS 22.1.3 / 25.1.2: PRINT-OBJECT is the standard generic function the
    # printer's default representation dispatches through, and
    # FUNCTION-KEYWORDS (CLHS 7.6.5) is the standard generic function a
    # method-combination author calls on a method.
    ('PRINT-OBJECT', [None, None], _default_print_object,
     '(object stream)'),
    ('FUNCTION-KEYWORDS', [None], _default_function_keywords,
     '(method)'),
]


@_registry.cl_function('DOCUMENTATION')
def documentation(x, doc_type=None):
    """DOCUMENTATION (CLHS 25.1.3) is itself a standard generic function --
    the same shape as SLOT-UNBOUND/CHANGE-CLASS above: ansi-test's
    environment/documentation.lsp defines DEFMETHODs directly on it, which
    replaces its *entire* environment binding with the bare GenericFunction
    object the moment the first one is evaluated. This thin wrapper only
    matters for a direct call before any such method exists; every Lisp-level
    call after that point reaches `_default_documentation` (or a more
    specific user method) through ordinary dispatch.
    """
    return classes.call_generic_function(_protocol_gf('DOCUMENTATION'),
                                          [x, doc_type])


@_registry.cl_function('(SETF DOCUMENTATION)')
def set_documentation(doc, x, doc_type=None):
    """(SETF DOCUMENTATION) (CLHS 25.1.3).

    The *reader* is a generic function with a default method above; the
    writer is reached through CLHS 5.1.2.9's generic SETF fallback --
    `(setf (documentation x y) doc)` expands to
    `(funcall #'(setf documentation) doc x y)` -- so this registration is
    what makes the place writable at all. It dispatches through the same
    generic function the reader's counterpart uses (its name is the synthetic
    `(SETF DOCUMENTATION)` symbol), so a user DEFMETHOD on
    `(setf documentation)` overrides by ordinary dispatch rather than by
    environment-binding replacement.
    """
    return classes.call_generic_function(_protocol_gf('(SETF DOCUMENTATION)'),
                                          [doc, x, doc_type])
def _ll_from_string(text):
    """A lambda list written as a Python string (the CLHS lambda lists in
    `_PROTOCOL_DEFAULTS`) as a Lisp list of symbols."""
    result = lisptype.NIL
    for token in reversed(text.replace('(', ' ').replace(')', ' ').split()):
        result = lisptype.lispCons(lisptype.intern_symbol(token), result)
    return result


def _make_installer(specializers, fn, lambda_list=None):
    def installer(gf):
        # Install the standard's own lambda list for a protocol generic
        # function, unless something more specific already set one (a
        # DEFGENERIC naming one of these wins).
        if (lambda_list is not None
                and getattr(gf, 'lambda_list', None) is None):
            gf.lambda_list = (lambda_list if not isinstance(lambda_list, str)
                              else _ll_from_string(lambda_list))
        classes.add_method(gf, specializers, fn)
    return installer


for _name, _specializers, _fn, _ll in _PROTOCOL_DEFAULTS:
    _installer = _make_installer(_specializers, _fn, _ll)
    classes.register_default_method_installer(_name, _installer)
    _installer(_protocol_gf(_name))
del _name, _specializers, _fn, _ll, _installer

# For generic functions that should be directly accessible as GenericFunction objects
# (not as plain function wrappers), bind them in the current environment now that
# they've been created. This makes (TYPEP #'(SETF CLASS-NAME) 'STANDARD-GENERIC-FUNCTION)
# return T even before any DEFMETHOD is evaluated on them.
import fclpy.state as _state
_current_env = _state.current_environment
if _current_env is not None:
    _setf_class_name_gf = _protocol_gf('(SETF CLASS-NAME)')
    _current_env.bind_function(lisptype.COMMON_LISP_PACKAGE.intern_symbol('(SETF CLASS-NAME)'), _setf_class_name_gf)

# Every name CLHS 7 (and 22.1.3/25.1.2) defines as a *standard generic
# function* is bound to its GenericFunction object, not to the plain
# `cl_function` wrapper -- `(symbol-function 'make-instance)` must be
# TYPEP of STANDARD-GENERIC-FUNCTION (ansi-test
# all-standard-generic-functions-are-instances-of-that-class), and a user
# DEFMETHOD on any of them extends the very object that is bound. The
# default method each one carries (the `_PROTOCOL_DEFAULTS` entries above)
# delegates to the same implementation the wrapper held, so calls behave
# identically while going through ordinary dispatch.
_STANDARD_GENERIC_FUNCTION_NAMES = (
    'ADD-METHOD',
    'ALLOCATE-INSTANCE',
    'CHANGE-CLASS',
    'CLASS-NAME',
    'COMPUTE-APPLICABLE-METHODS',
    'DESCRIBE-OBJECT',
    'DOCUMENTATION',
    'FIND-METHOD',
    'FUNCTION-KEYWORDS',
    'INITIALIZE-INSTANCE',
    'MAKE-INSTANCE',
    'MAKE-INSTANCES-OBSOLETE',
    'MAKE-LOAD-FORM',
    'METHOD-QUALIFIERS',
    'NO-APPLICABLE-METHOD',
    'NO-NEXT-METHOD',
    'PRINT-OBJECT',
    'REINITIALIZE-INSTANCE',
    'REMOVE-METHOD',
    'SHARED-INITIALIZE',
    'SLOT-MISSING',
    'SLOT-UNBOUND',
    'UPDATE-INSTANCE-FOR-DIFFERENT-CLASS',
    'UPDATE-INSTANCE-FOR-REDEFINED-CLASS',
)
if _current_env is not None:
    for _gf_name in _STANDARD_GENERIC_FUNCTION_NAMES:
        _current_env.bind_function(
            lisptype.COMMON_LISP_PACKAGE.intern_symbol(_gf_name),
            _protocol_gf(_gf_name))
del _current_env, _setf_class_name_gf


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


# CLASS-NAME lives in `classes.py` (lispfunc) -- the one place, since it
# raises a proper TYPE-ERROR for a non-class argument rather than silently
# falling back to `str()` (standing rule 2/3: two registrations of the same
# Lisp name, import order silently picking the winner).


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


@_registry.cl_function('MAKE-LOAD-FORM')
def make_load_form(object_arg, environment=None):
    """MAKE-LOAD-FORM is itself a standard generic function (CLHS 3.2.4), the
    same shape as CHANGE-CLASS above: user code can define DEFMETHODs directly
    on it, which replaces its *entire* environment binding with the bare
    GenericFunction object the moment the first one is evaluated -- so this
    thin wrapper only matters for a direct Python call; every Lisp-level call
    after that point reaches `_default_make_load_form` (or a more specific
    user method) directly.
    """
    return classes.call_generic_function(_protocol_gf('MAKE-LOAD-FORM'), [object_arg, environment])


@_registry.cl_function('CLASS-NAME')
def class_name(class_obj):
    """CLASS-NAME is a standard generic function (CLHS 7.6.15) that returns
    a class's name. User code can define DEFMETHODs to customize behavior.
    This wrapper is replaced by the GenericFunction when user code evaluates
    DEFMETHOD on CLASS-NAME.
    """
    return classes.call_generic_function(_protocol_gf('CLASS-NAME'), [class_obj])


# (SETF CLASS-NAME) is handled as a pure generic function in _PROTOCOL_DEFAULTS.
# It is not registered as a plain wrapper function, so when accessed via
# #'(setf class-name), it should return the GenericFunction object directly.
# The generic function dispatch mechanism makes it callable for SETF forms.


def built_in_class():
    return lisptype.T


def standard_class():
    return lisptype.T


def standard_object():
    return lisptype.T


def structure_class():
    return lisptype.NIL


def structure_object():
    return lisptype.NIL


# --- Slot operations ---
#
# SLOT-VALUE, (SETF SLOT-VALUE), SLOT-BOUNDP and SLOT-MAKUNBOUND (CLHS 7.5.3)
# all resolve `slot-name` against the instance's *class* first: a name that
# names no slot the class defines invokes SLOT-MISSING, itself a standard
# generic function a DEFMETHOD can extend (ansi-test's slot-missing.lsp
# does). SLOT-VALUE additionally invokes SLOT-UNBOUND (CLHS 7.5.5) when the
# name is a real slot with no value yet. Both invocations return exactly
# what the generic function call returns -- not a fixed condition -- because
# a user SLOT-MISSING/SLOT-UNBOUND method may return normally instead of
# signaling (slot-unbound.lsp's `(values)` and `(values 1 2 3)` methods).
#
# (SETF SLOT-VALUE) is the one exception: CLHS 7.5.3 says
# `(setf (slot-value o s) v)` always yields `v`, so it calls SLOT-MISSING
# only for effect and still returns `value`; SLOT-MAKUNBOUND likewise always
# returns `instance` regardless of what SLOT-MISSING returns.
def _slot_name_str(slot_name):
    return slot_name.name if hasattr(slot_name, 'name') else slot_name


def _op_sym(name):
    """The symbol CLHS 7.5.3 passes as SLOT-MISSING's `operation` argument
    -- one of SLOT-VALUE, SETF, SLOT-BOUNDP or SLOT-MAKUNBOUND -- fetched
    from the COMMON-LISP package so it is EQL to the same symbol ansi-test's
    `(operation (eql 'slot-boundp))`-style specializers read (a
    freshly-built symbol of the same name would not be)."""
    return lisptype.intern_symbol(name, 'COMMON-LISP')


def _update_if_obsolete(instance):
    """Bring `instance` up to date if MAKE-INSTANCES-OBSOLETE has been called
    on its class since it was last updated (CLHS 7.3).

    The one place that check happens, called from every slot accessor below.
    CLHS specifies the update as *lazy* -- "the generic function
    UPDATE-INSTANCE-FOR-REDEFINED-CLASS is invoked ... the next time a slot
    of that instance is read or written" -- so this is where it belongs rather
    than in MAKE-INSTANCES-OBSOLETE, which would otherwise have to hold a
    registry of every live instance.

    The added/discarded/property arguments are all empty because the class
    definition itself has not changed; the standard still requires the generic
    function to be *called*, so a user method on it runs.
    """
    if not isinstance(instance, classes.LispInstance):
        return
    cls = instance.lisp_class
    if getattr(instance, 'instance_generation', 0) == getattr(cls, 'instance_generation', 0):
        return
    # Mark it current *before* dispatching, so a method that itself reads a
    # slot of the instance does not recurse forever.
    instance.instance_generation = cls.instance_generation
    classes.call_generic_function(
        _protocol_gf('UPDATE-INSTANCE-FOR-REDEFINED-CLASS'),
        [instance, lisptype.NIL, lisptype.NIL, lisptype.NIL])


@_registry.cl_function('MAKE-INSTANCES-OBSOLETE')
def make_instances_obsolete(class_):
    """MAKE-INSTANCES-OBSOLETE (CLHS 7.3): mark every existing instance of
    `class_` obsolete, and return the argument as given.

    Was entirely absent. The obsolescence itself is one integer bump: each
    instance carries the generation of its class it was last updated for, and
    `_update_if_obsolete` above does the actual
    UPDATE-INSTANCE-FOR-REDEFINED-CLASS call lazily at the next slot access,
    which is what CLHS specifies.

    The argument is a **class designator** -- a class object or a symbol
    naming one -- and the *argument* is returned, not the resolved class
    (ansi-test accepts either, but returning what it was handed is what CLHS
    says). Exactly one argument: `(make-instances-obsolete)` and
    `(make-instances-obsolete c nil)` are both PROGRAM-ERRORs, which the
    single required parameter here gives for free.
    """
    target = class_
    if not isinstance(target, classes.LispClass):
        name = target.name if isinstance(target, lisptype.LispSymbol) else str(target)
        target = classes.find_class(name)
        if target is None:
            raise lisptype.LispTypeError(
                f"MAKE-INSTANCES-OBSOLETE: {class_!r} does not name a class",
                expected_type="CLASS", actual_value=class_)
    target.instance_generation = getattr(target, 'instance_generation', 0) + 1
    return class_


def _signal_builtin_slot_access(instance, op):
    """CLHS 4.3.7 (BUILT-IN-CLASS): calling SLOT-VALUE, (SETF SLOT-VALUE),
    SLOT-BOUNDP or SLOT-MAKUNBOUND on a generalized instance of a built-in
    class signals an error (slot-value.error.6, slot-boundp.error.5 and
    slot-makunbound.error.4 walk the mini-universe's built-in instances
    asserting this). Only *built-in-class* instances -- a condition or a
    STANDARD-CLASS instance has ordinary, accessible slots."""
    if isinstance(instance, classes.LispInstance):
        return
    if isinstance(instance, lisptype.Condition):
        return
    cls = classes.class_of(instance)
    if (isinstance(cls, classes.LispClass)
            and getattr(cls, 'metaclass_name', 'STANDARD-CLASS') == 'BUILT-IN-CLASS'):
        raise lisptype.LispTypeError(
            f"{op}: {instance!r} is a generalized instance of the built-in "
            f"class {cls.name.name}; its slots are not accessible",
            expected_type='STANDARD-OBJECT', actual_value=instance)


@_registry.cl_function('SLOT-VALUE')
def slot_value(instance, slot_name):
    """SLOT-VALUE (CLHS 7.5.3, 7.5.5): read a slot's value directly.
    SLOT-VALUE itself has exactly one return value, so a SLOT-MISSING or
    SLOT-UNBOUND method returning several (slot-unbound.lsp has one
    returning `(values 1 2 3)`) is reduced to its primary value here, the
    same rule `primary_value` applies at every other single-value context.
    """
    _signal_builtin_slot_access(instance, 'SLOT-VALUE')
    if isinstance(instance, classes.LispInstance):
        pass  # ordinary CLOS path below
    elif isinstance(instance, lisptype.Condition) and _condition_slot_table(type(instance)):
        # A DEFINE-CONDITION-created instance: CLHS 9.1/9.4 make condition
        # classes standard-objects with slot-value-readable slots, and
        # restart-case.37 exercises exactly that (a HANDLER-BIND handler
        # reading a condition's slot). The condition object model stores
        # slot values in `Condition._slots` with the merged slot table on
        # the class, so `slot-value` reads through the same table the
        # :READER accessors use. Built-in conditions carry no slot table
        # and keep the type-error below -- they have no user slots.
        name = _slot_name_str(slot_name)
        if name in _condition_slot_table(type(instance)):
            value = instance.get_slot(name)
            return value if value is not None else lisptype.NIL
        raise lisptype.LispTypeError(
            f"SLOT-VALUE: the slot {name} is not defined for {type(instance).__name__}",
            expected_type='slot-name', actual_value=slot_name)
    else:
        raise lisptype.LispTypeError(f"SLOT-VALUE: not an instance: {instance}")
    _update_if_obsolete(instance)
    name = _slot_name_str(slot_name)
    cls = instance.lisp_class
    slot_def = cls.get_all_slots().get(name)
    if slot_def is None:
        return lisptype.primary_value(classes.call_generic_function(
            _protocol_gf('SLOT-MISSING'),
            [cls, instance, slot_name, _op_sym('SLOT-VALUE')]))

    # For class-allocated slots, read from the class that defined them
    # (all subclasses share the same class slot on the defining class)
    if slot_def.allocation == "class":
        defining_class = cls.find_slot_definition_class(name)
        if defining_class is None:
            defining_class = cls
        if name not in defining_class.class_slots:
            return lisptype.primary_value(classes.call_generic_function(
                _protocol_gf('SLOT-UNBOUND'), [cls, instance, slot_name]))
        return defining_class.class_slots[name]

    # For instance-allocated slots, read from instance.slot_values
    if name not in instance.slot_values:
        return lisptype.primary_value(classes.call_generic_function(
            _protocol_gf('SLOT-UNBOUND'), [cls, instance, slot_name]))
    return instance.slot_values[name]


def _condition_slot_table(condition_class):
    """The merged DEFINE-CONDITION slot table for a condition class, or None.

    Lazy bridge to `evaluation_conditions._condition_all_slots` -- the slot
    model's one home -- kept a function so the import stays inside the call
    (evaluation_conditions and misc_clos reference each other's registries).
    """
    from .evaluation_conditions import _condition_all_slots
    try:
        return _condition_all_slots(condition_class)
    except AttributeError:
        return None


@_registry.cl_function('(SETF SLOT-VALUE)')
def set_slot_value(value, instance, slot_name):
    """(SETF SLOT-VALUE) (CLHS 7.5.3). Binds the slot whether or not it
    already held a value -- SETF of an unbound slot is exactly how a slot
    *becomes* bound. A slot-name naming no slot at all invokes SLOT-MISSING
    for effect only; the setf form's value is always `value` regardless.
    """
    _signal_builtin_slot_access(instance, 'SLOT-VALUE')
    if not isinstance(instance, classes.LispInstance):
        raise lisptype.LispTypeError(f"SLOT-VALUE: not an instance: {instance}")
    name = _slot_name_str(slot_name)
    cls = instance.lisp_class
    slot_def = cls.get_all_slots().get(name)
    if slot_def is None:
        classes.call_generic_function(
            _protocol_gf('SLOT-MISSING'),
            [cls, instance, slot_name, _op_sym('SETF'), value])
    else:
        # For class-allocated slots, write to the class that defined them
        if slot_def.allocation == "class":
            defining_class = cls.find_slot_definition_class(name)
            if defining_class is None:
                defining_class = cls
            defining_class.class_slots[name] = value
        # For instance-allocated slots, write to instance.slot_values
        else:
            instance.slot_values[name] = value
    return value


@_registry.cl_function('SLOT-BOUNDP')
def slot_boundp(instance, slot_name):
    _signal_builtin_slot_access(instance, 'SLOT-BOUNDP')
    if not isinstance(instance, classes.LispInstance):
        return lisptype.NIL
    name = _slot_name_str(slot_name)
    cls = instance.lisp_class
    slot_def = cls.get_all_slots().get(name)
    if slot_def is None:
        return lisptype.primary_value(classes.call_generic_function(
            _protocol_gf('SLOT-MISSING'),
            [cls, instance, slot_name, _op_sym('SLOT-BOUNDP')]))

    # For class-allocated slots, check the class that defined them
    if slot_def.allocation == "class":
        defining_class = cls.find_slot_definition_class(name)
        if defining_class is None:
            defining_class = cls
        return lisptype.T if name in defining_class.class_slots else lisptype.NIL
    # For instance-allocated slots, check instance.slot_values
    return lisptype.T if name in instance.slot_values else lisptype.NIL


@_registry.cl_function('SLOT-MAKUNBOUND')
def slot_makunbound(instance, slot_name):
    _signal_builtin_slot_access(instance, 'SLOT-MAKUNBOUND')
    if isinstance(instance, classes.LispInstance):
        name = _slot_name_str(slot_name)
        cls = instance.lisp_class
        slot_def = cls.get_all_slots().get(name)
        if slot_def is None:
            classes.call_generic_function(
                _protocol_gf('SLOT-MISSING'),
                [cls, instance, slot_name, _op_sym('SLOT-MAKUNBOUND')])
        else:
            # For class-allocated slots, remove from the class that defined them
            if slot_def.allocation == "class":
                defining_class = cls.find_slot_definition_class(name)
                if defining_class is None:
                    defining_class = cls
                defining_class.class_slots.pop(name, None)
            # For instance-allocated slots, remove from instance.slot_values
            else:
                instance.slot_values.pop(name, None)
    return instance


@_registry.cl_function('SLOT-UNBOUND')
def slot_unbound(class_obj, instance, slot_name):
    """SLOT-UNBOUND is itself a standard generic function (CLHS 7.5.5,
    called by SLOT-VALUE when a defined slot has no value) -- the same
    shape as MAKE-INSTANCE/CHANGE-CLASS above: ansi-test's
    slot-unbound.lsp defines a DEFMETHOD on it directly."""
    return classes.call_generic_function(_protocol_gf('SLOT-UNBOUND'), [class_obj, instance, slot_name])


@_registry.cl_function('SLOT-MISSING')
def slot_missing(class_obj, instance, slot_name, operation, *new_value):
    """SLOT-MISSING is itself a standard generic function (CLHS 7.5.3,
    called by SLOT-VALUE/(SETF SLOT-VALUE)/SLOT-BOUNDP/SLOT-MAKUNBOUND when
    slot-name names no slot the class defines) -- the same shape as
    SLOT-UNBOUND above: ansi-test's slot-missing.lsp defines a DEFMETHOD on
    it directly."""
    return classes.call_generic_function(
        _protocol_gf('SLOT-MISSING'), [class_obj, instance, slot_name, operation] + list(new_value))


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

    Conditions are objects with slots too (CLHS 9.1: condition classes
    are standard-objects): a DEFINE-CONDITION slot answers T here just
    like a DEFCLASS slot (slot-exists-p.15/.16)."""
    try:
        if isinstance(instance, classes.LispInstance):
            name = slot_name.name if hasattr(slot_name, 'name') else slot_name
            return lisptype.T if name in instance.lisp_class.get_all_slots() else lisptype.NIL
        if isinstance(instance, lisptype.Condition):
            table = _condition_slot_table(type(instance))
            if table is not None:
                name = _slot_name_str(slot_name)
                return lisptype.T if name in table else lisptype.NIL
    except Exception:
        pass
    return lisptype.NIL


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
    specializers.

    - `specializers` holds *parameter specializer designators*: a class,
      a symbol naming one, or an `(eql form)` list whose value part is
      compared EQL against the method's specializer value.
    - The number of specializers must match the number of required
      parameters of the generic function's lambda list (CLHS: "an error
      is signaled if ... the number of specializers does not correspond
      to the number of required parameters"), regardless of `errorp`.
    - `errorp` decides only a *miss with matching arity*: any non-NIL
      value signals; NIL returns NIL.
    """
    from fclpy.lispfunc.core import _consp_internal, car, cdr

    def _to_list(lisp_list):
        out = []
        current = lisp_list
        while _consp_internal(current):
            out.append(car(current))
            current = cdr(current)
        return out

    q_list = _to_list(qualifiers)
    spec_list = _to_list(specializers)

    def _resolve_spec(spec):
        """One specializer designator -> a comparable specializer: an
        EqlSpecializer for `(eql form)`, the class object for a resolvable
        name, the object itself otherwise."""
        if _consp_internal(spec):
            head = car(spec)
            if (isinstance(head, lisptype.LispSymbol)
                    and head.name.upper() == 'EQL'
                    and _consp_internal(cdr(spec))):
                return classes.EqlSpecializer(car(cdr(spec)))
            return spec
        if isinstance(spec, lisptype.LispSymbol):
            cls = classes.find_class(spec.name)
            if cls is not None:
                return cls
        return spec

    def _spec_matches(method_spec, given_spec):
        if isinstance(method_spec, classes.EqlSpecializer):
            if isinstance(given_spec, classes.EqlSpecializer):
                from fclpy.lispfunc.comparison import eql as _eql
                return _eql(method_spec.value, given_spec.value) is lisptype.T
            return False
        if isinstance(method_spec, classes.LispClass):
            return method_spec is given_spec
        # A raw type-name symbol the registry had no class for: compare
        # by name.
        return _specifier_name(method_spec) == _specifier_name(given_spec)

    # Arity mismatch is an error regardless of errorp (CLHS find-method).
    required_count = None
    if (isinstance(generic_function, classes.GenericFunction)
            and getattr(generic_function, 'lambda_list', None) is not None):
        from .evaluation_core import parse_lambda_list
        required_count = len(parse_lambda_list(generic_function.lambda_list)
                              .get('required', []))
    if required_count is None:
        counts = {len(m.specializers)
                  for m in getattr(generic_function, 'methods', [])}
        required_count = counts.pop() if len(counts) == 1 else None
    if required_count is not None and len(spec_list) != required_count:
        raise lisptype.LispError(
            f"FIND-METHOD: {len(spec_list)} specializer(s) given, but the "
            f"generic function takes {required_count} required parameter(s)")

    resolved = [_resolve_spec(s) for s in spec_list]
    for method in getattr(generic_function, 'methods', []):
        m_quals = [_specifier_name(q) for q in method.qualifiers]
        if m_quals != [_specifier_name(q) for q in q_list]:
            continue
        if len(method.specializers) != len(resolved):
            continue
        if all(_spec_matches(ms, gs)
               for ms, gs in zip(method.specializers, resolved)):
            return method

    if errorp is None or lisptype.is_truthy(errorp):
        raise lisptype.LispError(f"No method found for specializers: {specializers}")
    return lisptype.NIL


@_registry.cl_function('CLASS-PRECEDENCE-LIST')
def class_precedence_list(class_obj):
    """CLASS-PRECEDENCE-LIST (CLHS 4.3.5 / 7.6.15): the class precedence
    list of `class_obj`, most specific first -- the total order method
    dispatch ranks specializers by, computed by CLHS 4.3.5.1's
    deterministic topological sort (`classes._topological_cpl`)."""
    if not isinstance(class_obj, classes.LispClass):
        raise lisptype.LispTypeError(
            f"CLASS-PRECEDENCE-LIST: expected a class, got {class_obj!r}",
            expected_type='CLASS', actual_value=class_obj)
    from fclpy.lispfunc.sequence_protocol import make_lisp_list
    return make_lisp_list(class_obj.get_linearized_superclasses())


@_registry.cl_function('COMPUTE-CLASS-PRECEDENCE-LIST')
def compute_class_precedence_list(class_obj):
    """COMPUTE-CLASS-PRECEDENCE-LIST (CLHS 4.3.5): compute the class
    precedence list of `class_obj` -- the same computation CLASS-PRECEDENCE-LIST
    answers with, named as the MOP-level hook."""
    if not isinstance(class_obj, classes.LispClass):
        raise lisptype.LispTypeError(
            f"COMPUTE-CLASS-PRECEDENCE-LIST: expected a class, got {class_obj!r}",
            expected_type='CLASS', actual_value=class_obj)
    from fclpy.lispfunc.sequence_protocol import make_lisp_list
    return make_lisp_list(class_obj.get_linearized_superclasses())


@_registry.cl_function('ADD-METHOD')
def add_method(generic_function, method):
    """ADD-METHOD (CLHS 7.6.6.2): add one *method object* to a generic
    function. The method becomes an element of *this* generic function: a
    method congruent in qualifiers and specializers to an existing one
    replaces it (CLHS 7.6.3, add-method.2). Exceptional situations (CLHS
    add-method): a method whose lambda list is not congruent with the
    generic function's signals an error, and so does adding a method that
    is still a method object of *another* generic function (add-method.1
    removes it from its owner first)."""
    if not isinstance(generic_function, classes.GenericFunction):
        raise lisptype.LispTypeError(
            f"ADD-METHOD: {generic_function!r} is not a generic function",
            expected_type='GENERIC-FUNCTION', actual_value=generic_function)
    if not isinstance(method, classes.Method):
        raise lisptype.LispTypeError(
            f"ADD-METHOD: {method!r} is not a method",
            expected_type='METHOD', actual_value=method)
    owner = method.generic_function
    if owner is not None and owner is not generic_function:
        raise lisptype.LispError(
            "ADD-METHOD: the method is still a method of another generic "
            "function -- remove it from that one first")
    # CLHS 7.6.4 congruence, against a lambda list the generic function
    # actually declared (a generic function created implicitly by DEFMETHOD
    # has none until the first method establishes it).
    if getattr(generic_function, 'lambda_list', None) is not None:
        method_ll = getattr(method, 'lambda_list', None)
        if method_ll is not None:
            from .evaluation_core import parse_lambda_list
            from .evaluation_special_forms import _check_method_congruent
            tail = parse_lambda_list(method_ll)
            _check_method_congruent(generic_function.name,
                                     generic_function.lambda_list,
                                     tail.get('required', []), tail)
    for i, existing in enumerate(generic_function.methods):
        if existing is method:
            return generic_function
        if (existing.qualifiers == method.qualifiers
                and classes._specializers_congruent(existing.specializers,
                                                     method.specializers)):
            method.generic_function = generic_function
            generic_function.methods[i] = method
            return generic_function
    method.generic_function = generic_function
    generic_function.methods.append(method)
    return generic_function


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
    """ENSURE-GENERIC-FUNCTION (CLHS 7.7.1): find the generic function
    named `function-name`, or create one if none exists, applying
    `options` to it.

    This is the *operator*; `classes.ensure_generic_function` is the one
    object-level mechanism every caller (DEFGENERIC, DEFMETHOD, DEFCLASS's
    accessors) shares. The operator's job on top of it is CLHS 7.7.1's own:

    - **function-name** may be a symbol or a `(SETF symbol)` list -- the
      same function-name designator DEFUN/DEFGENERIC accept.
    - Every option keyword is applied to the generic function: an option
      this implementation cannot act on must still be *accepted* (CLHS
      names :ENVIRONMENT as a no-op argument-passing convention), and an
      unrecognized one is a PROGRAM-ERROR, not a silent drop.
    - A name already fbound to an ordinary function, macro or special
      operator is an error (CLHS 7.7.1: "the functional value of
      function-name must be a generic function, or be undefined").
    - Re-running with a lambda list whose required-parameter count differs
      while methods exist makes those methods incongruent (CLHS 7.6.4) --
      an error, not a silent method discard.
    """
    from .evaluation_core import _consp_internal, car, cdr

    # CLHS 7.7.1: function-name is a symbol or (SETF symbol).
    def _is_setf_spec(x):
        return (_consp_internal(x)
                and isinstance(car(x), lisptype.LispSymbol)
                and car(x).name.upper() == 'SETF'
                and _consp_internal(cdr(x))
                and isinstance(car(cdr(x)), lisptype.LispSymbol))

    if not (isinstance(function_name, lisptype.LispSymbol) or _is_setf_spec(function_name)):
        raise lisptype.LispProgramError(
            f"ENSURE-GENERIC-FUNCTION: {function_name} does not name a function")

    # Parse the option plist before touching anything, so a malformed call
    # signals without having created a half-configured generic function.
    kwargs = {}
    i = 0
    while i < len(options):
        key = options[i]
        opt = lisptype._keyword_name(key) if hasattr(lisptype, '_keyword_name') else None
        if opt is None:
            if isinstance(key, lisptype.lispKeyword):
                opt = key.name.upper().lstrip(':')
            elif isinstance(key, lisptype.LispSymbol):
                opt = key.name.upper().lstrip(':')
            else:
                raise lisptype.LispProgramError(
                    f"ENSURE-GENERIC-FUNCTION: {key} is not a valid option name")
        if i + 1 >= len(options):
            raise lisptype.LispProgramError(
                f"ENSURE-GENERIC-FUNCTION: option {opt} has no value")
        kwargs[opt] = options[i + 1]
        i += 2

    # CLHS 7.7.1: error if the name is fbound to something that is not a
    # generic function. ENSURE-GENERIC-FUNCTION.1/.2/.3 pin this for CAR,
    # DEFCLASS and TAGBODY -- the last of which is a *special operator*, so
    # the special-form registry counts as "fbound to a non-generic" too.
    # With no environment established (bare library use), FBOUNDP's registry
    # check alone answers, and the registry entry is consulted directly
    # rather than through FDEFINITION, which needs an environment.
    from .utilities_functions import fboundp, _function_spec_to_key
    from .registry import function_registry, special_registry
    if lisptype.is_truthy(fboundp(function_name)):
        key = _function_spec_to_key(function_name)
        entry = (function_registry.get(key.name) if key is not None else None) \
            or (special_registry.get(key.name) if key is not None else None)
        existing = entry.func if entry is not None else None
        if key is None or entry is not None and not isinstance(existing, classes.GenericFunction):
            raise lisptype.LispProgramError(
                f"ENSURE-GENERIC-FUNCTION: {function_name} already names "
                f"a non-generic function")

    lambda_list = kwargs.get('LAMBDA-LIST')
    documentation = kwargs.get('DOCUMENTATION')

    # CLHS 7.6.4 congruence: re-declaring a lambda list whose required-
    # parameter count differs while methods exist would strand them. This
    # must be checked *before* `classes.ensure_generic_function` runs --
    # that call updates the stored lambda list (and discards now-incongruent
    # methods as its own recovery), so comparing afterwards always agrees.
    if lambda_list is not None:
        existing = classes._generic_registry.find_generic(
            classes.generic_function_key(function_name))
        if (existing is not None and existing.methods
                and existing.lambda_list is not None
                and classes._required_param_count(lambda_list)
                != classes._required_param_count(existing.lambda_list)):
            raise lisptype.LispProgramError(
                f"ENSURE-GENERIC-FUNCTION: new lambda list for {function_name} "
                f"is incongruent with its existing methods")

    gf = classes.ensure_generic_function(
        function_name,
        documentation=documentation,
        lambda_list=lambda_list,
    )

    # Options this single-class/single-combination implementation has
    # nothing to select between are accepted and ignored; anything else
    # unrecognized is a PROGRAM-ERROR rather than a silent drop.
    _ACCEPTED_NOOP = {'ENVIRONMENT', 'METHOD-CLASS',
                      'GENERIC-FUNCTION-CLASS', 'DECLARE'}
    for opt in kwargs:
        if opt not in ('LAMBDA-LIST', 'DOCUMENTATION',
                       'ARGUMENT-PRECEDENCE-ORDER') and opt not in _ACCEPTED_NOOP:
            raise lisptype.LispProgramError(
                f"ENSURE-GENERIC-FUNCTION: unrecognized option :{opt}")

    # CLHS 7.6.6.1: :argument-precedence-order is a permutation of the
    # lambda list's required parameters naming the order they are compared
    # in when ordering applicable methods. Validation and installation are
    # shared with DEFGENERIC via `classes.set_argument_precedence_order`.
    apo = kwargs.get('ARGUMENT-PRECEDENCE-ORDER')
    if apo is not None:
        from .evaluation_core import _consp_internal as _ci, car as _car
        order_list = []
        cur = apo
        while _ci(cur):
            order_list.append(_car(cur))
            cur = cdr(cur)
        classes.set_argument_precedence_order(
            gf, lambda_list if lambda_list is not None else gf.lambda_list,
            order_list)

    # CLHS 7.7.1: "The generic function is added to the environment" --
    # ensuring a generic function makes the name fbound, exactly as
    # DEFGENERIC/DEFMETHOD bind it (through the same function-name key
    # resolver), so SYMBOL-FUNCTION/FBOUNDP see it afterwards. With no
    # environment established at all (bare library use, unit tests), the
    # registry itself is the binding -- the GF is already findable by name.
    from .utilities_functions import _function_spec_to_key
    import fclpy.state as _state
    global_env = _state.current_environment
    if global_env is not None:
        from .binding import root_environment
        from .misc_macros import install_function_binding
        install_function_binding(_function_spec_to_key(function_name), gf,
                                 root_environment(global_env))

    return gf


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
    # Documentation (CLHS 25.1.3)
    'documentation',
]
