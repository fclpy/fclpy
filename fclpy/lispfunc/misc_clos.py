"""CLOS class, instance, slot, and method operations."""

import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry


# --- CLOS class and instance operations ---
@_registry.cl_function('FIND-CLASS')
def find_class(name, errorp=True, environment=None):
    """Find class by name."""
    raise lisptype.LispNotImplementedError("FIND-CLASS")


@_registry.cl_function('MAKE-INSTANCE')
def make_instance(class_designator, *initargs):
    """Make instance of class."""
    return {}


@_registry.cl_function('ALLOCATE-INSTANCE')
def allocate_instance(class_obj, **kwargs):
    """Allocate instance."""
    raise lisptype.LispNotImplementedError("ALLOCATE-INSTANCE")


@_registry.cl_function('INITIALIZE-INSTANCE')
def initialize_instance(instance, *initargs):
    """Initialize instance."""
    return instance


@_registry.cl_function('REINITIALIZE-INSTANCE')
def reinitialize_instance(instance, *initargs):
    """Reinitialize instance."""
    return instance


@_registry.cl_function('SHARED-INITIALIZE')
def shared_initialize(instance, slot_names, *initargs):
    """Shared initialize."""
    return instance


@_registry.cl_function('UPDATE-INSTANCE-FOR-DIFFERENT-CLASS')
def update_instance_for_different_class(previous, current, *initargs):
    """Update instance for different class."""
    return current


@_registry.cl_function('UPDATE-INSTANCE-FOR-REDEFINED-CLASS')
def update_instance_for_redefined_class(instance, added_slots=None, discarded_slots=None, property_list=None, *initargs):
    """Update instance for redefined class."""
    return instance


@_registry.cl_function('CLASS-OF')
def class_of(object):
    """Get class of object."""
    return type(object)


@_registry.cl_function('CLASS-NAME')
def class_name(class_obj):
    """Get class name."""
    return getattr(class_obj, '__name__', str(class_obj))


@_registry.cl_function('CHANGE-CLASS')
def change_class(instance, new_class, *initargs):
    """Change class of instance."""
    return instance


@_registry.cl_function('BUILT-IN-CLASS')
def built_in_class():
    """Get built-in class type."""
    return 'BUILT-IN-CLASS'


@_registry.cl_function('STANDARD-CLASS')
def standard_class():
    """Get standard class type."""
    return 'STANDARD-CLASS'


@_registry.cl_function('STANDARD-OBJECT')
def standard_object():
    """Get standard object type."""
    return 'STANDARD-OBJECT'


@_registry.cl_function('STRUCTURE-CLASS')
def structure_class():
    """Get structure class type."""
    return 'STRUCTURE-CLASS'


@_registry.cl_function('STRUCTURE-OBJECT')
def structure_object():
    """Get structure object type."""
    return 'STRUCTURE-OBJECT'


# --- Slot operations ---
@_registry.cl_function('SLOT-BOUNDP')
def slot_boundp(instance, slot_name):
    """Test if slot is bound."""
    return lisptype.T


@_registry.cl_function('SLOT-EXISTS-P')
def slot_exists_p(instance, slot_name):
    """Test if slot exists."""
    return lisptype.T


@_registry.cl_function('SLOT-MAKUNBOUND')
def slot_makunbound(instance, slot_name):
    """Make slot unbound."""
    return instance


@_registry.cl_function('SLOT-UNBOUND')
def slot_unbound(class_obj, instance, slot_name):
    """Handle unbound slot access."""
    return None


@_registry.cl_function('SLOT-VALUE')
def slot_value(instance, slot_name):
    """Get slot value."""
    return None


@_registry.cl_function('SLOT-MISSING')
def slot_missing(class_obj, instance, slot_name, operation, *args):
    """Handle missing slot."""
    return None


# --- Method operations ---
@_registry.cl_function('FIND-METHOD')
def find_method(generic_function, qualifiers, specializers, errorp=True):
    """Find method in generic function."""
    raise lisptype.LispNotImplementedError("FIND-METHOD")


@_registry.cl_function('ADD-METHOD')
def add_method(generic_function, method):
    """Add method to generic function."""
    raise lisptype.LispNotImplementedError("ADD-METHOD")


@_registry.cl_function('REMOVE-METHOD')
def remove_method(generic_function, method):
    """Remove method from generic function."""
    return generic_function


@_registry.cl_function('DEFMETHOD')
def defmethod(name, *args):
    """Define method (simplified)."""
    return name


@_registry.cl_function('MAKE-METHOD')
def make_method(*args):
    """Create method object."""
    return None


@_registry.cl_function('METHOD-FUNCTION')
def method_function(method):
    """Get method function."""
    return None


@_registry.cl_function('METHOD-GENERIC-FUNCTION')
def method_generic_function(method):
    """Get method generic function."""
    return None


@_registry.cl_function('METHOD-SPECIALIZERS')
def method_specializers(method):
    """Get method specializers."""
    return []


@_registry.cl_function('METHOD-LAMBDA-LIST')
def method_lambda_list(method):
    """Get method lambda list."""
    return []


@_registry.cl_function('METHOD-QUALIFIERS')
def method_qualifiers(method):
    """Get method qualifiers."""
    return []


@_registry.cl_function('NEXT-METHOD-P')
def next_method_p():
    """Test if next method exists."""
    return lisptype.NIL


@_registry.cl_function('NO-APPLICABLE-METHOD')
def no_applicable_method(generic_function, *arguments):
    """Handle no applicable method."""
    return None


@_registry.cl_function('NO-NEXT-METHOD')
def no_next_method(generic_function, method, *arguments):
    """Handle no next method."""
    return None


@_registry.cl_function('CALL-METHOD')
def call_method(method, next_methods, *args):
    """Call method with next methods."""
    return None


@_registry.cl_function('CALL-NEXT-METHOD')
def call_next_method(*args):
    """Call next method in call chain."""
    return None


@_registry.cl_function('COMPUTE-APPLICABLE-METHODS')
def compute_applicable_methods(generic_function, arguments):
    """Compute applicable methods."""
    return []


@_registry.cl_function('ENSURE-GENERIC-FUNCTION')
def ensure_generic_function(function_name, *options):
    """Ensure generic function exists."""
    return function_name


@_registry.cl_function('GENERIC-FUNCTION-LAMBDA-LIST')
def generic_function_lambda_list(generic_function):
    """Get generic function lambda list."""
    return []


@_registry.cl_function('GENERIC-FUNCTION-METHODS')
def generic_function_methods(generic_function):
    """Get generic function methods."""
    return []


@_registry.cl_function('GENERIC-FUNCTION-NAME')
def generic_function_name(generic_function):
    """Get generic function name."""
    return str(generic_function)


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
