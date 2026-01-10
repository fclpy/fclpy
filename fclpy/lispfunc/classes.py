"""Class system functions - DEFCLASS, MAKE-INSTANCE, type checking."""

import fclpy.lisptype as lisptype
from fclpy import classes
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
    for slot_spec in slots:
        if isinstance(slot_spec, lisptype.LispSymbol):
            # Simple slot: just a name
            slot_defs.append(
                classes.SlotDefinition(name=slot_spec)
            )
        elif isinstance(slot_spec, (list, tuple)):
            # Slot with options: (name :initarg SLOT-NAME :initform default-value ...)
            if not slot_spec:
                continue
            
            slot_name = slot_spec[0]
            if not isinstance(slot_name, lisptype.LispSymbol):
                raise TypeError(f"Slot name must be symbol, got {slot_name}")
            
            # Parse options
            initarg = None
            initform = None
            allocation = "instance"
            documentation = None
            
            i = 1
            while i < len(slot_spec):
                key = slot_spec[i]
                if isinstance(key, lisptype.lispKeyword):
                    key_name = key.name.upper()
                    if i + 1 >= len(slot_spec):
                        raise ValueError(f"Missing value for {key}")
                    value = slot_spec[i + 1]
                    
                    if key_name == 'INITARG':
                        initarg = value
                    elif key_name == 'INITFORM':
                        initform = value
                    elif key_name == 'ALLOCATION':
                        if isinstance(value, lisptype.LispSymbol):
                            allocation = value.name.lower()
                    elif key_name == 'DOCUMENTATION':
                        if isinstance(value, str):
                            documentation = value
                    
                    i += 2
                else:
                    i += 1
            
            slot_defs.append(
                classes.SlotDefinition(
                    name=slot_name,
                    initarg=initarg,
                    initform=initform,
                    allocation=allocation,
                    documentation=documentation
                )
            )
        else:
            raise TypeError(f"Invalid slot specification: {slot_spec}")
    
    # Handle documentation option
    documentation = options.get('documentation', None)
    
    # Create the class directly (don't use classes.defclass since we've already parsed)
    lisp_class = classes.make_class(
        name=name,
        direct_superclasses=direct_superclasses,
        direct_slots=slot_defs,
        documentation=documentation
    )
    
    # Register it and return the created class object
    lisp_class = classes.register_class(lisp_class)
    # Per expected runtime behavior, DEFCLASS returns the class name symbol
    return name


@_registry.cl_function('MAKE-INSTANCE')
def make_instance(class_spec, *args, **kwargs):
    """MAKE-INSTANCE: Create an instance of a class.
    
    Syntax: (MAKE-INSTANCE class-spec &key initarg*)
    
    class-spec can be a class object or a symbol naming a class.
    """
    # Get the class
    if isinstance(class_spec, lisptype.LispSymbol):
        class_name = class_spec.name
        lisp_class = classes.find_class(class_name)
        if lisp_class is None:
            raise NameError(f"Class not found: {class_name}")
    elif isinstance(class_spec, str):
        # Handle string class names
        lisp_class = classes.find_class(class_spec)
        if lisp_class is None:
            raise NameError(f"Class not found: {class_spec}")
    elif isinstance(class_spec, classes.LispClass):
        lisp_class = class_spec
    else:
        raise TypeError(f"MAKE-INSTANCE: Expected class or class name, got {class_spec}")
    
    # Parse initargs (keyword arguments)
    initargs = {}
    for key, value in kwargs.items():
        # Convert Python kwargs to Lisp keyword format
        if not key.startswith(':'):
            key = ':' + key
        initargs[key] = value
    
    # Also handle positional args if they're in keyword form
    # This handles both (MAKE-INSTANCE 'MyClass :slot1 value1) style calls
    i = 0
    while i < len(args):
        if isinstance(args[i], lisptype.lispKeyword):
            key = args[i].name
            if i + 1 < len(args):
                value = args[i + 1]
                initargs[key] = value
                i += 2
            else:
                raise ValueError(f"Missing value for keyword {args[i]}")
        else:
            i += 1
    
    # Create the instance
    instance = classes.LispInstance(lisp_class=lisp_class)
    
    # Get all slots
    all_slots = lisp_class.get_all_slots()
    
    # Initialize slots
    for slot_name, slot_def in all_slots.items():
        value = None
        
        # Check if initarg was provided
        if slot_def.initarg:
            arg_key = slot_def.initarg.name if isinstance(slot_def.initarg, lisptype.LispSymbol) else slot_def.initarg
            # Try with colon prefix
            if ':' + arg_key in initargs:
                value = initargs[':' + arg_key]
            elif arg_key in initargs:
                value = initargs[arg_key]
        
        # Also try matching the slot name directly (Common Lisp behavior)
        if value is None:
            if ':' + slot_name in initargs:
                value = initargs[':' + slot_name]
            elif slot_name in initargs:
                value = initargs[slot_name]
        
        # Use initform if no value provided
        if value is None and slot_def.initform is not None:
            value = slot_def.initform
        
        # Store the value
        instance.slot_values[slot_name] = value
    
    return instance


@_registry.cl_function('SLOT-VALUE')
def slot_value(instance, slot_name):
    """SLOT-VALUE: Get the value of a slot in an instance."""
    if not isinstance(instance, classes.LispInstance):
        raise TypeError(f"Not an instance: {instance}")
    
    if isinstance(slot_name, lisptype.LispSymbol):
        slot_name = slot_name.name
    elif not isinstance(slot_name, str):
        raise TypeError(f"Slot name must be symbol, got {slot_name}")
    
    if slot_name not in instance.slot_values:
        raise AttributeError(f"Slot {slot_name} not found")
    
    return instance.slot_values[slot_name]


@_registry.cl_function('(SETF SLOT-VALUE)')
def set_slot_value(value, instance, slot_name):
    """(SETF SLOT-VALUE): Set the value of a slot in an instance."""
    if not isinstance(instance, classes.LispInstance):
        raise TypeError(f"Not an instance: {instance}")
    
    if isinstance(slot_name, lisptype.LispSymbol):
        slot_name = slot_name.name
    elif not isinstance(slot_name, str):
        raise TypeError(f"Slot name must be symbol, got {slot_name}")
    
    if slot_name not in instance.slot_values:
        raise AttributeError(f"Slot {slot_name} not found")
    
    instance.slot_values[slot_name] = value
    return value


@_registry.cl_function('CLASS-NAME')
def class_name(lisp_class):
    """CLASS-NAME: Get the name of a class."""
    if not isinstance(lisp_class, classes.LispClass):
        raise TypeError(f"Expected a class, got {lisp_class}")
    return lisp_class.name


@_registry.cl_function('CLASS-DIRECT-SLOTS')
def class_direct_slots(lisp_class):
    """CLASS-DIRECT-SLOTS: Get direct slots of a class (not inherited)."""
    if not isinstance(lisp_class, classes.LispClass):
        raise TypeError(f"Expected a class, got {lisp_class}")
    
    # Return as a list of slot names
    return [slot.name for slot in lisp_class.direct_slots]


@_registry.cl_function('CLASS-SLOTS')
def class_slots(lisp_class):
    """CLASS-SLOTS: Get all slots of a class (including inherited)."""
    if not isinstance(lisp_class, classes.LispClass):
        raise TypeError(f"Expected a class, got {lisp_class}")
    
    # Return as a list of slot names
    all_slots = lisp_class.get_all_slots()
    return list(all_slots.keys())


@_registry.cl_function('CLASS-SUPERCLASSES')
def class_superclasses(lisp_class):
    """CLASS-SUPERCLASSES: Get direct superclasses of a class."""
    if not isinstance(lisp_class, classes.LispClass):
        raise TypeError(f"Expected a class, got {lisp_class}")
    
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
    if cls is None:
        if lisptype.is_truthy(errorp):
            raise NameError(f"Class not found: {name}")
        return lisptype.NIL
    return cls


@_registry.cl_function('INSTANCEP')
def instancep(obj):
    """INSTANCEP: Test if object is an instance of a class."""
    return lisptype.lisp_bool(isinstance(obj, classes.LispInstance))


@_registry.cl_function('CLASS-OF')
def class_of(obj):
    """CLASS-OF: Get the class of an object."""
    if isinstance(obj, classes.LispInstance):
        return obj.lisp_class
    # For built-in types, return type-based classes
    # (simplified - just return T)
    # In full CLOS, every object would have a class
    return lisptype.T


# Generic function support

@_registry.cl_function('ENSURE-GENERIC-FUNCTION')
def ensure_generic_function(name, **options):
    """ENSURE-GENERIC-FUNCTION: Get or create a generic function."""
    if not isinstance(name, lisptype.LispSymbol):
        raise TypeError(f"Generic function name must be symbol, got {name}")
    
    return classes.ensure_generic_function(name, **options)


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
