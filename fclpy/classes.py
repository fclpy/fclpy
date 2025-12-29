"""Common Lisp class system (simplified CLOS).

This module implements a basic class system supporting:
- DEFCLASS: Define classes with slots
- MAKE-INSTANCE: Create instances
- Slot readers/writers
- Simplified method dispatch (single dispatch only)
- Basic inheritance (linear order)
"""

from dataclasses import dataclass, field
from typing import Dict, List, Optional, Any, Callable
from fclpy.lisptype import LispSymbol, T, NIL, is_truthy


@dataclass
class SlotDefinition:
    """Definition of a single slot in a class."""
    name: LispSymbol
    type_spec: Optional[Any] = None
    initform: Optional[Any] = None
    initarg: Optional[LispSymbol] = None
    allocation: str = "instance"  # "instance" or "class"
    documentation: Optional[str] = None
    
    def __repr__(self):
        return f"SlotDefinition({self.name.name})"


@dataclass
class LispClass:
    """Represents a Common Lisp class object.
    
    Stores slot definitions, parent classes, and class-level metadata.
    Uses simplified linear inheritance order: parent -> grandparent -> ... -> T
    """
    name: LispSymbol
    direct_superclasses: List['LispClass'] = field(default_factory=list)
    direct_slots: List[SlotDefinition] = field(default_factory=list)
    class_slots: Dict[str, Any] = field(default_factory=dict)  # For class-allocated slots
    documentation: Optional[str] = None
    
    def __post_init__(self):
        """Initialize class metadata."""
        if not self.direct_superclasses:
            # If no parent specified, use (T) as the ultimate parent
            # We'll add implicit T parent later
            pass
    
    def get_linearized_superclasses(self) -> List['LispClass']:
        """Get list of all superclasses in linear order.
        
        Returns classes in order: self -> parents -> grandparents -> ... -> T
        """
        result = [self]
        for parent in self.direct_superclasses:
            # Get parent's linearized list (excluding self)
            parent_list = parent.get_linearized_superclasses()
            # Add any new classes from parent's list
            for cls in parent_list:
                if cls not in result:
                    result.append(cls)
        return result
    
    def get_all_slots(self) -> Dict[str, SlotDefinition]:
        """Get all slots (direct and inherited) as a dict by slot name.
        
        Returns:
            Dictionary mapping slot name -> SlotDefinition
            Later slot definitions (from subclasses) override earlier ones.
        """
        slots = {}
        
        # Get slots from superclasses first (reverse order for override)
        for cls in reversed(self.get_linearized_superclasses()):
            if cls is self:
                continue
            for slot in cls.direct_slots:
                slots[slot.name.name] = slot
        
        # Add direct slots (these override parent slots)
        for slot in self.direct_slots:
            slots[slot.name.name] = slot
        
        return slots
    
    def __repr__(self):
        return f"#<STANDARD-CLASS {self.name.name}>"


@dataclass
class LispInstance:
    """Represents an instance of a Common Lisp class.
    
    Stores the class reference and slot values.
    """
    lisp_class: LispClass
    slot_values: Dict[str, Any] = field(default_factory=dict)
    
    def __repr__(self):
        return f"#<{self.lisp_class.name.name} {id(self)}>"
    
    def __str__(self):
        return repr(self)


class ClassRegistry:
    """Global registry of defined classes."""
    
    def __init__(self):
        self._classes: Dict[str, LispClass] = {}
    
    def register_class(self, cls: LispClass) -> LispClass:
        """Register a class in the registry."""
        self._classes[cls.name.name] = cls
        return cls
    
    def find_class(self, name: str) -> Optional[LispClass]:
        """Find a class by name."""
        return self._classes.get(name)
    
    def get_class_or_error(self, name: str) -> LispClass:
        """Find a class by name or raise error."""
        cls = self.find_class(name)
        if cls is None:
            raise NameError(f"Class not found: {name}")
        return cls
    
    def list_classes(self) -> List[LispClass]:
        """List all registered classes."""
        return list(self._classes.values())


# Global class registry
_class_registry = ClassRegistry()


def register_class(cls: LispClass) -> LispClass:
    """Register a class in the global registry."""
    return _class_registry.register_class(cls)


def find_class(name: str) -> Optional[LispClass]:
    """Find a class by name."""
    if isinstance(name, LispSymbol):
        name = name.name
    return _class_registry.find_class(name)


def make_class(
    name: LispSymbol,
    direct_superclasses: Optional[List[LispClass]] = None,
    direct_slots: Optional[List[SlotDefinition]] = None,
    documentation: Optional[str] = None
) -> LispClass:
    """Create a new class.
    
    Args:
        name: Symbol naming the class
        direct_superclasses: List of parent classes
        direct_slots: List of SlotDefinition objects
        documentation: Documentation string
    
    Returns:
        The created LispClass object
    """
    if direct_superclasses is None:
        direct_superclasses = []
    if direct_slots is None:
        direct_slots = []
    
    cls = LispClass(
        name=name,
        direct_superclasses=direct_superclasses,
        direct_slots=direct_slots,
        documentation=documentation
    )
    
    return cls


def defclass(
    name: LispSymbol,
    direct_superclasses: Optional[List[LispClass]] = None,
    slot_specs: Optional[List[Any]] = None,
    documentation: Optional[str] = None
) -> LispClass:
    """DEFCLASS function: define and register a new class.
    
    This is a simplified version that handles basic slot definitions.
    
    Args:
        name: Symbol naming the class
        direct_superclasses: List of parent classes
        slot_specs: List of slot specifications (each can be a symbol or dict)
        documentation: Documentation string
    
    Returns:
        The created and registered LispClass object
    """
    if direct_superclasses is None:
        direct_superclasses = []
    if slot_specs is None:
        slot_specs = []
    
    # Parse slot specifications
    slots = []
    for spec in slot_specs:
        if isinstance(spec, LispSymbol):
            # Simple slot name
            slot = SlotDefinition(name=spec)
        elif isinstance(spec, dict):
            # Slot with options
            slot_name = spec.get('name')
            if not slot_name:
                raise ValueError("Slot spec must have a 'name'")
            
            slot = SlotDefinition(
                name=slot_name,
                type_spec=spec.get('type'),
                initform=spec.get('initform'),
                initarg=spec.get('initarg'),
                allocation=spec.get('allocation', 'instance'),
                documentation=spec.get('documentation')
            )
        else:
            raise TypeError(f"Invalid slot spec: {spec}")
        
        slots.append(slot)
    
    # Create and register the class
    cls = make_class(
        name=name,
        direct_superclasses=direct_superclasses,
        direct_slots=slots,
        documentation=documentation
    )
    
    return register_class(cls)


def make_instance(
    class_name: str,
    **initargs
) -> LispInstance:
    """MAKE-INSTANCE function: create an instance of a class.
    
    Args:
        class_name: Name of the class (as string or symbol)
        **initargs: Initialization arguments (keyword -> value)
    
    Returns:
        A new LispInstance object
    """
    if isinstance(class_name, LispSymbol):
        class_name = class_name.name
    
    cls = _class_registry.get_class_or_error(class_name)
    
    # Create instance
    instance = LispInstance(lisp_class=cls)
    
    # Get all slots (inherited and direct)
    all_slots = cls.get_all_slots()
    
    # Initialize slots
    for slot_name, slot_def in all_slots.items():
        value = None
        
        # Check if initarg was provided
        if slot_def.initarg:
            arg_name = slot_def.initarg.name if isinstance(slot_def.initarg, LispSymbol) else slot_def.initarg
            if arg_name in initargs:
                value = initargs[arg_name]
        
        # Use initform if no value provided
        if value is None and slot_def.initform is not None:
            value = slot_def.initform
        
        # Store the value
        instance.slot_values[slot_name] = value
    
    return instance


def slot_value(instance: LispInstance, slot_name: str) -> Any:
    """Get the value of a slot in an instance.
    
    Args:
        instance: A LispInstance object
        slot_name: Name of the slot (as string or symbol)
    
    Returns:
        The slot value
    """
    if isinstance(slot_name, LispSymbol):
        slot_name = slot_name.name
    
    if not isinstance(instance, LispInstance):
        raise TypeError(f"Not an instance: {instance}")
    
    if slot_name not in instance.slot_values:
        raise AttributeError(f"Slot {slot_name} not found in {instance}")
    
    return instance.slot_values[slot_name]


def set_slot_value(instance: LispInstance, slot_name: str, value: Any) -> Any:
    """Set the value of a slot in an instance.
    
    Args:
        instance: A LispInstance object
        slot_name: Name of the slot (as string or symbol)
        value: The new value
    
    Returns:
        The value that was set
    """
    if isinstance(slot_name, LispSymbol):
        slot_name = slot_name.name
    
    if not isinstance(instance, LispInstance):
        raise TypeError(f"Not an instance: {instance}")
    
    if slot_name not in instance.slot_values:
        raise AttributeError(f"Slot {slot_name} not found in {instance}")
    
    instance.slot_values[slot_name] = value
    return value


# Generic function support

@dataclass
class Method:
    """A method in a generic function.
    
    Stores the specializers (type restrictions) and the actual function.
    """
    specializers: List[Optional[LispClass]]  # None means T (any type)
    function: Callable


@dataclass
class GenericFunction:
    """A generic function that dispatches based on argument types.
    
    Supports single dispatch on the first argument (simplified CLOS).
    """
    name: LispSymbol
    methods: List[Method] = field(default_factory=list)
    documentation: Optional[str] = None
    
    def __repr__(self):
        return f"#<STANDARD-GENERIC-FUNCTION {self.name.name}>"


class GenericFunctionRegistry:
    """Global registry of generic functions."""
    
    def __init__(self):
        self._generics: Dict[str, GenericFunction] = {}
    
    def register_generic(self, gf: GenericFunction) -> GenericFunction:
        """Register a generic function."""
        self._generics[gf.name.name] = gf
        return gf
    
    def find_generic(self, name: str) -> Optional[GenericFunction]:
        """Find a generic function by name."""
        return self._generics.get(name)
    
    def get_generic_or_error(self, name: str) -> GenericFunction:
        """Find a generic function or raise error."""
        gf = self.find_generic(name)
        if gf is None:
            raise NameError(f"Generic function not found: {name}")
        return gf
    
    def list_generics(self) -> List[GenericFunction]:
        """List all registered generic functions."""
        return list(self._generics.values())


# Global generic function registry
_generic_registry = GenericFunctionRegistry()


def ensure_generic_function(
    name: LispSymbol,
    documentation: Optional[str] = None
) -> GenericFunction:
    """ENSURE-GENERIC-FUNCTION: get or create a generic function.
    
    Args:
        name: Symbol naming the generic function
        documentation: Documentation string
    
    Returns:
        The generic function (newly created or existing)
    """
    name_str = name.name if isinstance(name, LispSymbol) else str(name)
    
    gf = _generic_registry.find_generic(name_str)
    if gf:
        return gf
    
    gf = GenericFunction(name=name, documentation=documentation)
    return _generic_registry.register_generic(gf)


def add_method(
    generic_function: GenericFunction,
    specializers: List[Optional[LispClass]],
    method_function: Callable
) -> GenericFunction:
    """ADD-METHOD: add a method to a generic function.
    
    Methods are kept sorted by specificity (more specific first).
    
    Args:
        generic_function: The generic function to add to
        specializers: List of class specializers for each argument
                      (None means no restriction, i.e., all types)
        method_function: The actual method function
    
    Returns:
        The generic function
    """
    method = Method(specializers=specializers, function=method_function)
    generic_function.methods.append(method)
    
    # Sort methods by specificity (more specific first)
    # A method is more specific if its specializers are more specific
    generic_function.methods.sort(
        key=lambda m: _method_specificity(m.specializers),
        reverse=True
    )
    
    return generic_function


def _method_specificity(specializers: List[Optional[LispClass]]) -> tuple:
    """Calculate specificity score for a method.
    
    More specific methods have higher scores.
    None (T) specializers are least specific.
    """
    score = []
    for spec in specializers:
        # Count how deep in the inheritance hierarchy the class is
        # Deeper classes are more specific
        if spec is None:
            score.append(0)  # T is least specific
        else:
            # Deeper classes get higher scores
            depth = len(spec.get_linearized_superclasses())
            score.append(depth)
    return tuple(score)


def call_generic_function(
    gf: GenericFunction,
    args: List[Any],
    next_methods: Optional[List[Method]] = None
) -> Any:
    """Call a generic function with argument-based method dispatch.
    
    Selects the most specific matching method based on argument types.
    
    Args:
        gf: The generic function to call
        args: Arguments to pass to the method
        next_methods: Internal: remaining methods for CALL-NEXT-METHOD
    
    Returns:
        The return value from the selected method
    """
    if not args:
        raise ValueError("Generic function requires at least one argument")
    
    # Find the first matching method
    first_arg = args[0]
    
    for method in gf.methods:
        if _matches_specializers(first_arg, method.specializers):
            # Found a matching method
            # Pass remaining methods for CALL-NEXT-METHOD support
            remaining = [m for m in gf.methods if m is not method]
            
            # Call the method function
            # Store next methods in a context for CALL-NEXT-METHOD
            old_next_methods = getattr(call_generic_function, '_next_methods', None)
            call_generic_function._next_methods = remaining
            call_generic_function._current_gf = gf
            
            try:
                return method.function(*args)
            finally:
                call_generic_function._next_methods = old_next_methods
                call_generic_function._current_gf = None
    
    # No matching method found
    raise TypeError(
        f"No matching method for {gf.name.name} with arguments: {args}"
    )


def _matches_specializers(obj: Any, specializers: List[Optional[LispClass]]) -> bool:
    """Check if an object matches a list of specializers.
    
    Returns True if the object is an instance of all specializers.
    None specializers (T) always match.
    """
    # For now, only check the first specializer (single dispatch)
    if not specializers:
        return True
    
    first_spec = specializers[0]
    if first_spec is None:
        # No specializer means any type matches
        return True
    
    # Check if obj is an instance of first_spec
    if isinstance(obj, LispInstance):
        return _is_instance_of(obj, first_spec)
    
    return False


def _is_instance_of(instance: LispInstance, lisp_class: LispClass) -> bool:
    """Check if an instance is of a given class (including superclasses)."""
    for cls in instance.lisp_class.get_linearized_superclasses():
        if cls is lisp_class:
            return True
    return False


def call_next_method(*args) -> Any:
    """CALL-NEXT-METHOD: call the next method in the dispatch chain.
    
    Args:
        *args: Arguments to pass to the next method (if empty, uses original args)
    
    Returns:
        The return value from the next method
    """
    remaining_methods = getattr(call_generic_function, '_next_methods', None)
    gf = getattr(call_generic_function, '_current_gf', None)
    
    if not remaining_methods or not gf:
        raise RuntimeError("CALL-NEXT-METHOD: No next method available")
    
    # Get the first remaining method
    if not remaining_methods:
        raise RuntimeError("CALL-NEXT-METHOD: No next method available")
    
    method = remaining_methods[0]
    remaining = remaining_methods[1:]
    
    # Store updated next methods
    old_next_methods = getattr(call_generic_function, '_next_methods', None)
    call_generic_function._next_methods = remaining
    
    try:
        return method.function(*args) if args else method.function()
    finally:
        call_generic_function._next_methods = old_next_methods
