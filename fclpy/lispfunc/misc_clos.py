"""CLOS class, instance, slot, and method operations.

This module maps the Lisp-visible CLOS API into the more complete
implementation found in `fclpy.classes`. The goal is to provide
minimal, well-behaved bindings so the ANSI test-suite can define
classes and methods at load time without triggering assertions.
"""

import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry

import fclpy.classes as classes


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
def allocate_instance(class_obj, **kwargs):
    """Allocate a raw instance object for `class_obj`.

    Accepts either a class object or class name/symbol.
    """
    try:
        # Ensure we have a LispClass
        if isinstance(class_obj, classes.LispClass):
            cls = class_obj
        else:
            cls = classes.find_class(class_obj)
            if cls is None:
                raise lisptype.LispError(f"Class not found: {class_obj}")
        return classes.LispInstance(lisp_class=cls)
    except Exception as e:
        raise lisptype.LispError(str(e))


@_registry.cl_function('INITIALIZE-INSTANCE')
def initialize_instance(instance, *initargs):
    """No-op initializer; instances are initialized by `make_instance`."""
    return instance


@_registry.cl_function('REINITIALIZE-INSTANCE')
def reinitialize_instance(instance, *initargs):
    """Reinitialize instance (best-effort no-op)."""
    return instance


@_registry.cl_function('SHARED-INITIALIZE')
def shared_initialize(instance, slot_names, *initargs):
    """No-op shared initialization."""
    return instance


@_registry.cl_function('UPDATE-INSTANCE-FOR-DIFFERENT-CLASS')
def update_instance_for_different_class(previous, current, *initargs):
    """Best-effort update; return `current` instance."""
    return current


@_registry.cl_function('UPDATE-INSTANCE-FOR-REDEFINED-CLASS')
def update_instance_for_redefined_class(instance, added_slots=None, discarded_slots=None, property_list=None, *initargs):
    """Best-effort update for redefined class."""
    return instance


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
    """Best-effort change-class: update the instance's class reference."""
    if isinstance(instance, classes.LispInstance):
        if isinstance(new_class, classes.LispClass):
            instance.lisp_class = new_class
        else:
            cls = classes.find_class(new_class)
            if cls is None:
                raise lisptype.LispError(f"Class not found: {new_class}")
            instance.lisp_class = cls
    return instance


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
    try:
        if isinstance(instance, classes.LispInstance):
            name = slot_name.name if hasattr(slot_name, 'name') else slot_name
            return lisptype.T if name in instance.slot_values else lisptype.NIL
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
    raise lisptype.LispError(f"Slot unbound: {slot_name}")


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
@_registry.cl_function('FIND-METHOD')
def find_method(generic_function, qualifiers, specializers, errorp=True):
    """Find a method in a generic function by specializers.
    
    ANSI CL: find-method generic-function qualifiers specializers &optional (errorp t)
    
    Per ANSI spec:
    - errorp defaults to T (true)
    - If method found, return the method object
    - If not found and errorp=T, signal an error
    - If not found and errorp=NIL, return NIL
    """
    from fclpy.lispfunc.core import _consp_internal, car, cdr
    
    # Normalize errorp: treat None as True (ANSI default)
    if errorp is None:
        errorp = True
    
    # Convert specializers list to a Python list
    spec_list = []
    current = specializers
    while _consp_internal(current):
        spec = car(current)
        spec_list.append(spec)
        current = cdr(current)
    
    # Try to find the method in the generic function
    try:
        # Case 1: generic_function is our eval_defgeneric GenericFunction (stores methods as tuples)
        if hasattr(generic_function, 'methods'):
            for method_item in generic_function.methods:
                # Method item might be:
                # - A tuple: (specializers_list, function)
                # - An object with .specializers and .function attributes
                if isinstance(method_item, tuple) and len(method_item) == 2:
                    m_specs, m_fn = method_item
                    # m_specs is a list of specializer symbols or None
                    if isinstance(m_specs, list):
                        if len(m_specs) == len(spec_list):
                            # Check if specializers match
                            match = _specializers_match(m_specs, spec_list)
                            if match:
                                # Create a method wrapper for the caller
                                class MethodWrapper:
                                    def __init__(self, fn, specs):
                                        self.function = fn
                                        self.specializers = specs
                                    def __repr__(self):
                                        gf_name = getattr(generic_function, 'name', '?')
                                        return f"#<METHOD {gf_name}>"
                                return MethodWrapper(m_fn, m_specs)
                else:
                    # Object with attributes
                    m_specs = getattr(method_item, 'specializers', None)
                    m_fn = getattr(method_item, 'function', None)
                    if m_specs is not None and len(m_specs) == len(spec_list):
                        match = _specializers_match(m_specs, spec_list)
                        if match and m_fn is not None:
                            class MethodWrapper:
                                def __init__(self, fn, specs):
                                    self.function = fn
                                    self.specializers = specs
                                def __repr__(self):
                                    gf_name = getattr(generic_function, 'name', '?')
                                    return f"#<METHOD {gf_name}>"
                            return MethodWrapper(m_fn, m_specs)
    except Exception:
        pass
    
    # Method not found
    if errorp is True or errorp is lisptype.T:
        raise lisptype.LispError(f"No method found for specializers: {specializers}")
    return lisptype.NIL


def _specializers_match(method_specs, query_specs):
    """Check if method specializers match the query specializers.
    
    Both are lists of specifier symbols/objects or None (meaning T/any type).
    """
    if len(method_specs) != len(query_specs):
        return False
    
    for m_spec, q_spec in zip(method_specs, query_specs):
        if m_spec is None:
            # Method has no specialization for this arg (matches T)
            continue
        
        # Both should be LispClass or LispSymbol
        # Compare by name if they're symbols/classes
        m_name = None
        q_name = None
        
        if isinstance(m_spec, lisptype.LispSymbol):
            m_name = m_spec.name.upper()
        elif isinstance(m_spec, classes.LispClass):
            m_name = m_spec.name.name.upper() if isinstance(m_spec.name, lisptype.LispSymbol) else str(m_spec.name).upper()
        
        if isinstance(q_spec, lisptype.LispSymbol):
            q_name = q_spec.name.upper()
        elif isinstance(q_spec, classes.LispClass):
            q_name = q_spec.name.name.upper() if isinstance(q_spec.name, lisptype.LispSymbol) else str(q_spec.name).upper()
        
        # If either is None, we couldn't determine the name, so fail gracefully
        if m_name is None or q_name is None:
            # Try object identity as fallback
            if m_spec is not q_spec:
                return False
        elif m_name != q_name:
            return False
    
    return True


@_registry.cl_function('ADD-METHOD')
def add_method(generic_function, method):
    try:
        # Expect generic_function to be classes.GenericFunction
        if hasattr(generic_function, 'methods'):
            # method may be a callable with attributes; try to append
            generic_function.methods.append(method)
            return generic_function
    except Exception:
        pass
    raise lisptype.LispNotImplementedError("ADD-METHOD")


@_registry.cl_function('REMOVE-METHOD')
def remove_method(generic_function, method):
    try:
        if hasattr(generic_function, 'methods'):
            generic_function.methods = [m for m in generic_function.methods if m is not method]
            return generic_function
    except Exception:
        pass
    return generic_function


@_registry.cl_function('DEFMETHOD')
def defmethod(name, *args):
    return name


@_registry.cl_function('MAKE-METHOD')
def make_method(*args):
    return None


@_registry.cl_function('METHOD-FUNCTION')
def method_function(method):
    return getattr(method, 'function', None)


@_registry.cl_function('METHOD-GENERIC-FUNCTION')
def method_generic_function(method):
    return getattr(method, 'generic_function', None)


@_registry.cl_function('METHOD-SPECIALIZERS')
def method_specializers(method):
    return getattr(method, 'specializers', [])


@_registry.cl_function('METHOD-LAMBDA-LIST')
def method_lambda_list(method):
    return getattr(method, 'lambda_list', [])


@_registry.cl_function('METHOD-QUALIFIERS')
def method_qualifiers(method):
    return getattr(method, 'qualifiers', [])


@_registry.cl_function('NEXT-METHOD-P')
def next_method_p():
    return lisptype.NIL


@_registry.cl_function('NO-APPLICABLE-METHOD')
def no_applicable_method(generic_function, *arguments):
    raise lisptype.LispError("No applicable method")


@_registry.cl_function('NO-NEXT-METHOD')
def no_next_method(generic_function, method, *arguments):
    raise lisptype.LispError("No next method")


@_registry.cl_function('CALL-METHOD')
def call_method(method, next_methods, *args):
    fn = getattr(method, 'function', None)
    if callable(fn):
        return fn(*args)
    return None


@_registry.cl_function('CALL-NEXT-METHOD')
def call_next_method(*args):
    return classes.call_next_method(*args)


@_registry.cl_function('COMPUTE-APPLICABLE-METHODS')
def compute_applicable_methods(generic_function, arguments):
    return getattr(generic_function, 'methods', [])


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
    return getattr(generic_function, 'methods', [])


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
    'add_method',
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
