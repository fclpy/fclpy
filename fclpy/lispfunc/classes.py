"""Class system functions - DEFCLASS, MAKE-INSTANCE, type checking."""

import fclpy.lisptype as lisptype
from fclpy import classes
from fclpy.lisptype import LispProgramError
from . import registry as _registry


@_registry.cl_function('DEFCLASS')
def defclass(name, direct_superclasses=None, slots=None, **options):
    """DEFCLASS: Define a new class.
    
    Syntax: (DEFCLASS name (superclass*) (slot-spec*) option*)
    
    Simplified version supporting:
    - Basic slot definitions with :initarg and :initform
    - Simple inheritance (single parent)
    - Documentation
    """
    # Coerce Lisp-style list arguments (NIL or cons lists) into Python lists
    def _to_py_list(x):
        # NIL -> []
        if x is None or x == lisptype.NIL:
            return []
        # Handle lispCons
        if hasattr(x, 'car') and hasattr(x, 'cdr'):
            out = []
            cur = x
            while cur is not None and cur != lisptype.NIL:
                out.append(cur.car)
                cur = cur.cdr
            return out
        # Already a Python sequence
        if isinstance(x, (list, tuple)):
            return list(x)
        # Single element
        return [x]

    if direct_superclasses is None:
        direct_superclasses = []
    else:
        direct_superclasses = _to_py_list(direct_superclasses)

    if slots is None:
        slots = []
    else:
        slots = _to_py_list(slots)
    
    # Parse direct_superclasses into class objects
    parsed_superclasses = []
    for sc in direct_superclasses:
        # If given as a symbol, look up the class by name
        if isinstance(sc, lisptype.LispSymbol):
            found = classes.find_class(sc.name)
            if found is None:
                raise NameError(f"Superclass not found: {sc.name}")
            parsed_superclasses.append(found)
        elif isinstance(sc, classes.LispClass):
            parsed_superclasses.append(sc)
        elif isinstance(sc, str):
            found = classes.find_class(sc)
            if found is None:
                raise NameError(f"Superclass not found: {sc}")
            parsed_superclasses.append(found)
        else:
            raise TypeError(f"Invalid superclass spec: {sc}")

    # Parse slot specifications into SlotDefinition objects
    slot_defs = []
    seen_slot_names = set()
    for slot_spec in slots:
        if isinstance(slot_spec, lisptype.LispSymbol):
            # Simple slot: just a name
            if slot_spec in seen_slot_names:
                raise LispProgramError(
                    f"DEFCLASS: duplicate slot named {slot_spec}")
            seen_slot_names.add(slot_spec)
            slot_defs.append(
                classes.SlotDefinition(name=slot_spec)
            )
        elif isinstance(slot_spec, (list, tuple)):
            # Slot with options: (name :initarg SLOT-NAME :initform default-value ...)
            if not slot_spec:
                raise LispProgramError("DEFCLASS: empty slot specification")
            
            slot_name = slot_spec[0]
            if not isinstance(slot_name, lisptype.LispSymbol):
                raise LispProgramError(f"Slot name must be symbol, got {slot_name}")
            if slot_name in seen_slot_names:
                raise LispProgramError(
                    f"DEFCLASS: duplicate slot named {slot_name}")
            seen_slot_names.add(slot_name)
            
            # Parse options
            initform = None
            allocation = "instance"
            documentation = None
            # CLHS 7.5.3: :initarg/:reader/:writer/:accessor may each
            # appear more than once on the same slot, so these accumulate.
            initargs = []
            readers = []
            writers = []
            accessors = []
            # CLHS 7.5.3: every *other* slot option may appear at most once
            # per slot; a repeat is a program-error (defclass-errors.lsp).
            _SINGLE_SLOT_OPTIONS = {'INITFORM', 'ALLOCATION', 'DOCUMENTATION', 'TYPE'}
            _KNOWN_SLOT_OPTIONS = _SINGLE_SLOT_OPTIONS | {
                'INITARG', 'READER', 'WRITER', 'ACCESSOR'}
            seen_single_options = set()
            slot_type = None

            i = 1
            while i < len(slot_spec):
                key = slot_spec[i]
                if isinstance(key, lisptype.lispKeyword):
                    key_name = key.name.upper()
                    if i + 1 >= len(slot_spec):
                        raise LispProgramError(f"Missing value for {key}")
                    value = slot_spec[i + 1]

                    if key_name not in _KNOWN_SLOT_OPTIONS:
                        raise LispProgramError(
                            f"DEFCLASS: unrecognized slot option :{key_name} "
                            f"on slot {slot_name}")
                    if key_name in _SINGLE_SLOT_OPTIONS:
                        if key_name in seen_single_options:
                            raise LispProgramError(
                                f"DEFCLASS: duplicate slot option :{key_name} "
                                f"on slot {slot_name}")
                        seen_single_options.add(key_name)

                    if key_name == 'INITARG':
                        initargs.append(value)
                    elif key_name == 'INITFORM':
                        initform = value
                    elif key_name == 'TYPE':
                        # CLHS 7.5.3: stored unevaluated; consulted by
                        # SLOT-VALUE type checking and the reader's
                        # CHECK-TYPE expansion, not enforced at DEFCLASS
                        # time.
                        slot_type = value
                    elif key_name == 'ALLOCATION':
                        if isinstance(value, lisptype.LispSymbol):
                            allocation = value.name.lower()
                    elif key_name == 'DOCUMENTATION':
                        if isinstance(value, str):
                            documentation = value
                    elif key_name == 'READER':
                        readers.append(value)
                    elif key_name == 'WRITER':
                        writers.append(value)
                    elif key_name == 'ACCESSOR':
                        accessors.append(value)

                    i += 2
                else:
                    raise LispProgramError(
                        f"DEFCLASS: invalid slot option {key} on slot {slot_name}")

            slot_defs.append(
                classes.SlotDefinition(
                    name=slot_name,
                    initargs=initargs,
                    initform=initform,
                    allocation=allocation,
                    documentation=documentation,
                    readers=readers,
                    writers=writers,
                    accessors=accessors,
                    type_spec=slot_type,
                )
            )
        else:
            raise TypeError(f"Invalid slot specification: {slot_spec}")
    
    # Handle documentation option
    documentation = options.get('documentation', None)

    # CLHS 7.1: standard-object is a superclass of every class defined via
    # DEFCLASS that does not otherwise specify one. Without this, a class
    # with no explicit superclass had *no* ancestors at all -- its own
    # linearization was just itself -- so a DEFMETHOD specializing on
    # STANDARD-OBJECT could never match its instances, even though
    # `(typep instance 'standard-object)` already special-cases this true
    # (see comparison.typep's STANDARD-OBJECT branch).
    if not parsed_superclasses:
        std_object = classes.find_class('STANDARD-OBJECT')
        if std_object is not None:
            parsed_superclasses = [std_object]

    # Thread the defining environment through so a slot's :initform (stored
    # unevaluated) can later be evaluated where DEFCLASS lexically saw it
    # (CLHS 7.1.2), not wherever MAKE-INSTANCE happens to run.
    definition_env = options.get('definition_env', None)
    for slot_def in slot_defs:
        slot_def.definition_env = definition_env

    # CLHS 7.1.8: :default-initargs, threaded from eval_defclass as a list of
    # (initarg-keyword, unevaluated-form) pairs -- the environment attaches
    # here for the same reason a slot's :initform does, above.
    default_initargs_raw = options.get('default_initargs', None) or []
    default_initargs = [(key, form, definition_env) for (key, form) in default_initargs_raw]

    # If this name is currently a *forward-referenced* class -- some earlier
    # DEFCLASS named it as a superclass before it existed -- fill that same
    # object in rather than building a new one. Its identity is load-bearing:
    # the subclass already holds a reference to it, and DEFCLASS must return
    # the object `(find-class name)` answers (CLHS 4.3.7; see
    # `classes.define_forward_referenced_class`).
    existing = classes.find_class(
        name.name if isinstance(name, lisptype.LispSymbol) else str(name))
    if existing is not None and existing.forward_referenced:
        lisp_class = classes.define_forward_referenced_class(
            existing,
            direct_superclasses=parsed_superclasses,
            direct_slots=slot_defs,
            documentation=documentation,
            direct_default_initargs=default_initargs,
        )
    else:
        # Create the class directly (don't use classes.defclass since we've already parsed)
        lisp_class = classes.make_class(
            name=name,
            direct_superclasses=parsed_superclasses,
            direct_slots=slot_defs,
            documentation=documentation,
            direct_default_initargs=default_initargs
        )

        # Register it and return the created class object
        lisp_class = classes.register_class(lisp_class)

    _define_slot_accessors(lisp_class, slot_defs, definition_env)

    # CLHS 7.7 defclass, Values: "new-class -- the new class object", not
    # its name (unlike DEFUN/DEFVAR, which return the name being defined).
    # `find-class.lsp`'s FIND-CLASS.15/.16/.17/.18/.19/.20/.21 all pin this:
    # each does `(eqt (eval '(defclass ...)) (find-class 'name))`, which a
    # returned symbol can never satisfy since FIND-CLASS itself always
    # answers the class object.
    return lisp_class


def _define_slot_accessors(lisp_class, slot_defs, definition_env):
    """CLHS 7.5.3: a slot's :reader/:writer/:accessor options each define a
    generic function (creating it if this is its first mention anywhere)
    and add a method on it specialized to the class being defined -- not a
    plain Python function, or a later `(defmethod some-reader ...)` on the
    same name could never join it, and `(typep #'reader 'generic-function)`
    (CLASS-04.2/.3) would be false. A :writer's method has an *unspecialized*
    new-value parameter (CLHS: "the second... is unspecialized"); :accessor
    is exactly a :reader under its own name plus a :writer named
    `(setf accessor)`, per CLHS's own wording, not a separate mechanism.
    """
    import fclpy.state as state

    env = definition_env or state.current_environment
    if env is None:
        import fclpy.lispenv as lispenv
        lispenv.setup_standard_environment()
        env = state.current_environment
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent

    def _binding_symbol(gf_name):
        """The symbol a *function name* is bound under in the environment.

        A slot option's function name may be a `(SETF reader)` **list** --
        `:writer (setf s1)` in a DEFCLASS is written exactly that way -- and
        `Environment.bind_function` takes a symbol. Both this and
        `classes.generic_function_key` go through
        `_function_spec_to_key`, the one function-name resolver, so the
        environment binding and the generic-function registry agree on the
        name; a `:writer (setf foo)` and an `:accessor foo` therefore land on
        the *same* function, which is what CLHS means by "an accessor is a
        reader plus a writer named (setf reader)".
        """
        from fclpy.lispfunc.utilities_functions import _function_spec_to_key
        key = _function_spec_to_key(gf_name)
        if key is None:
            raise lisptype.LispTypeError(
                f"DEFCLASS: {gf_name} is not a function name",
                expected_type='(OR SYMBOL (CONS (EQL SETF) (CONS SYMBOL NULL)))',
                actual_value=gf_name)
        return key

    def _bind_reader(gf_name, slot_name):
        # Deferred import: `misc_clos` is the one home of SLOT-VALUE's real
        # CLHS 7.5.3/7.5.5 protocol (SLOT-MISSING/SLOT-UNBOUND dispatch), and
        # a reader-generated method must go through exactly that, not a
        # second copy -- ansi-test's slot-unbound.lsp calls readers directly
        # (sunb-a, sunb-b) and requires SLOT-UNBOUND to fire through them.
        from fclpy.lispfunc.misc_clos import slot_value
        gf = classes.ensure_generic_function(gf_name)
        classes.add_method(gf, [lisp_class], lambda instance: slot_value(instance, slot_name))
        global_env.add_function(_binding_symbol(gf_name), gf)

    def _bind_writer(gf_name, slot_name):
        from fclpy.lispfunc.misc_clos import set_slot_value
        gf = classes.ensure_generic_function(gf_name)
        classes.add_method(
            gf, [None, lisp_class],
            lambda new_value, instance: set_slot_value(new_value, instance, slot_name))
        global_env.add_function(_binding_symbol(gf_name), gf)

    for slot_def in slot_defs:
        slot_name = slot_def.name
        for reader_name in slot_def.readers:
            _bind_reader(reader_name, slot_name)
        for writer_name in slot_def.writers:
            _bind_writer(writer_name, slot_name)
        for accessor_name in slot_def.accessors:
            _bind_reader(accessor_name, slot_name)
            _bind_writer(lisptype.LispSymbol(f"(SETF {accessor_name.name})"), slot_name)


@_registry.cl_function('MAKE-INSTANCE')
def make_instance(class_spec, *args, **kwargs):
    """MAKE-INSTANCE: Create an instance of a class.

    Syntax: (MAKE-INSTANCE class-spec &key initarg*)

    class-spec can be a class object or a symbol naming a class.

    MAKE-INSTANCE is itself a standard generic function (CLHS 7.1) that
    ansi-test's make-instance.lsp defines methods on directly -- including
    one specialized on an *instance* passed as `class-spec`, to prove
    dispatch on this argument works at all, not only the normal case -- so
    a symbol/string designator is resolved to its class object before
    dispatch (the ordinary case), but anything else is passed through
    unresolved rather than rejected, so a method specializing on some other
    type can still match it. The default method (installed in
    misc_clos.py, alongside its ALLOCATE-INSTANCE/INITIALIZE-INSTANCE
    siblings) is what actually rejects a non-class argument.
    """
    class_designator = classes.resolve_class_designator(class_spec)

    # Build a flat initarg plist from both call conventions this function
    # supports: ordinary Lisp calls pass alternating keyword/value *args;
    # the Python-level test suite calls this directly with **kwargs (plain
    # string keys, no leading colon).
    initargs = []
    for key, value in kwargs.items():
        initargs.append(key)
        initargs.append(value)
    i = 0
    while i < len(args):
        if isinstance(args[i], lisptype.lispKeyword) and i + 1 < len(args):
            initargs.append(args[i])
            initargs.append(args[i + 1])
            i += 2
        else:
            i += 1

    gf = classes.ensure_generic_function(lisptype.py_str_to_sym('MAKE-INSTANCE'))
    return classes.call_generic_function(gf, [class_designator] + initargs)


# SLOT-VALUE, (SETF SLOT-VALUE), SLOT-BOUNDP and SLOT-MAKUNBOUND live in
# `misc_clos.py` -- the one place that implements their full CLHS 7.5.3
# protocol (SLOT-MISSING for a slot the class doesn't define, SLOT-UNBOUND
# for one that's defined but has no value). This module used to carry a
# second copy of SLOT-VALUE/(SETF SLOT-VALUE) that skipped both and raised
# a bare Python AttributeError on a missing slot -- and, because both
# modules registered the same Lisp name, import order silently decided
# which one every caller got.


@_registry.cl_function('CLASS-NAME')
def class_name(lisp_class):
    """CLASS-NAME: Get the name of a class."""
    # Handle T specially - it represents the universal type
    if isinstance(lisp_class, lisptype.LispSymbol) and lisp_class.name.upper() == 'T':
        return lisptype.T
    if not isinstance(lisp_class, classes.LispClass):
        raise lisptype.LispTypeError(
            f"Expected a class, got {lisp_class!r}",
            expected_type='CLASS', actual_value=lisp_class)
    return lisp_class.name


@_registry.cl_function('CLASS-DIRECT-SLOTS')
def class_direct_slots(lisp_class):
    """CLASS-DIRECT-SLOTS: Get direct slots of a class (not inherited)."""
    # Handle T specially - it has no slots
    if isinstance(lisp_class, lisptype.LispSymbol) and lisp_class.name.upper() == 'T':
        return []
    if not isinstance(lisp_class, classes.LispClass):
        raise lisptype.LispTypeError(
            f"Expected a class, got {lisp_class!r}",
            expected_type='CLASS', actual_value=lisp_class)
    
    # Return as a list of slot names
    return [slot.name for slot in lisp_class.direct_slots]


@_registry.cl_function('CLASS-SLOTS')
def class_slots(lisp_class):
    """CLASS-SLOTS: Get all slots of a class (including inherited)."""
    # Handle T specially - it has no slots
    if isinstance(lisp_class, lisptype.LispSymbol) and lisp_class.name.upper() == 'T':
        return []
    if not isinstance(lisp_class, classes.LispClass):
        raise lisptype.LispTypeError(
            f"Expected a class, got {lisp_class!r}",
            expected_type='CLASS', actual_value=lisp_class)
    
    # Return as a list of slot names
    all_slots = lisp_class.get_all_slots()
    return list(all_slots.keys())


@_registry.cl_function('CLASS-SUPERCLASSES')
def class_superclasses(lisp_class):
    """CLASS-SUPERCLASSES: Get direct superclasses of a class."""
    # Handle T specially - it has no superclasses (it's the root)
    if isinstance(lisp_class, lisptype.LispSymbol) and lisp_class.name.upper() == 'T':
        return lisptype.NIL
    if not isinstance(lisp_class, classes.LispClass):
        raise lisptype.LispTypeError(
            f"Expected a class, got {lisp_class!r}",
            expected_type='CLASS', actual_value=lisp_class)
    
    # Build a Lisp list from the superclasses
    result = lisptype.NIL
    for cls in reversed(lisp_class.direct_superclasses):
        result = lisptype.lispCons(cls, result)
    return result


@_registry.cl_function('FIND-CLASS')
def find_class_fn(name, errorp=True, environment=None):
    """FIND-CLASS: Find a class by name.
    
    (FIND-CLASS symbol &optional errorp environment)
    Returns the class named by symbol. If errorp is true (default) and no
    class is found, signals an error. Otherwise returns NIL.
    """
    if isinstance(name, lisptype.LispSymbol):
        name = name.name
    elif not isinstance(name, str):
        raise TypeError(f"Class name must be symbol, got {name}")

    cls = classes.find_class(name)
    if cls is not None:
        return cls

    # Conditions (built-in or DEFINE-CONDITION-created) are plain Python
    # classes, not CLOS `LispClass` objects -- see `_condition_class_for_name`
    # -- so they live outside the `classes.find_class` registry above. Without
    # this, `(find-class 'my-condition-type)` on any DEFINE-CONDITION type
    # raised "Class not found" (plan.md X1: a Python exception as a Lisp
    # value), which is exactly what every DEFINE-CONDITION-generated
    # IS-SUBCLASS-OF/IS-NOT-SUPERCLASS-OF test calls to get a SUBTYPEP
    # argument.
    from fclpy.lispfunc.evaluation_conditions import _condition_class_for_name
    condition_cls = _condition_class_for_name(name)
    if condition_cls is not None:
        return condition_cls

    if lisptype.is_truthy(errorp):
        raise NameError(f"Class not found: {name}")
    return lisptype.NIL


@_registry.cl_function('INSTANCEP')
def instancep(obj):
    """INSTANCEP: Test if object is an instance of a class."""
    return lisptype.lisp_bool(isinstance(obj, classes.LispInstance))


@_registry.cl_function('CLASS-OF')
def class_of(obj):
    """CLASS-OF (CLHS 7.1.1): the class object naming obj's class -- every
    object has one, not only CLOS instances. Delegates to
    `classes.class_of`, which resolves any value's class via TYPE-OF rather
    than answering the bare symbol T for anything that isn't a
    LispInstance (a type error waiting to happen: callers of CLASS-OF want
    a class object, and generic-function dispatch on a built-in type
    specializer needs the real answer to match against)."""
    return classes.class_of(obj)


# Generic function support

# ENSURE-GENERIC-FUNCTION is registered once, in `misc_clos.py`, whose
# implementation applies CLHS 7.7.1's function-name designator and option
# handling on top of `classes.ensure_generic_function`. A second
# registration here used to win on *import order* (`__init__` imports
# `.utilities` -> `utilities_misc` -> `misc_clos` before it imports
# `.classes`) and silently replaced the real operator with a symbol-only,
# no-options stub -- the duplicate-register defect class.

@_registry.cl_function('ADD-METHOD')
def add_method(gf, specializers, method_func):
    """ADD-METHOD: Add a method to a generic function."""
    if not isinstance(gf, classes.GenericFunction):
        raise TypeError(f"Not a generic function: {gf}")
    
    # Parse specializers (list of class objects or NIL for T)
    spec_list = []
    if isinstance(specializers, (list, tuple)):
        spec_list = list(specializers)
    else:
        spec_list = [specializers]
    
    # Convert NIL, T, or T-like symbols to None (no specializer)
    parsed_specs = []
    for spec in spec_list:
        if spec is None or spec is lisptype.NIL:
            parsed_specs.append(None)
        elif spec is lisptype.T:
            parsed_specs.append(None)
        elif isinstance(spec, lisptype.LispSymbol) and spec.name.upper() == 'T':
            # Handle T symbols from parsed forms (compare by name, not identity)
            parsed_specs.append(None)
        elif isinstance(spec, classes.LispClass):
            parsed_specs.append(spec)
        else:
            raise TypeError(f"Specializer must be a class, got {spec}")
    
    return classes.add_method(gf, parsed_specs, method_func)


@_registry.cl_function('CALL-GENERIC-FUNCTION')
def call_generic_function(gf, *args):
    """CALL-GENERIC-FUNCTION: Call a generic function with dispatch."""
    if not isinstance(gf, classes.GenericFunction):
        raise TypeError(f"Not a generic function: {gf}")
    
    # Handle both cases:
    # 1. When called with multiple args: (CALL-GENERIC-FUNCTION gf arg1 arg2 ...)
    # 2. When called with a list: (CALL-GENERIC-FUNCTION gf [arg1 arg2 ...])
    if len(args) == 1 and isinstance(args[0], list):
        # Already a list - pass as-is
        return classes.call_generic_function(gf, args[0])
    else:
        # Multiple args - pass as list
        return classes.call_generic_function(gf, list(args))


@_registry.cl_function('CALL-NEXT-METHOD')
def call_next_method(*args):
    """CALL-NEXT-METHOD: Call the next method in the dispatch chain."""
    return classes.call_next_method(*args)
