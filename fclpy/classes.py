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


# ==============================================================================
# Built-in Type Classes
# ==============================================================================
# Register the standard Common Lisp built-in type classes.
# These are used for CLOS dispatch and FIND-CLASS.

def _make_builtin_class(name: str) -> LispClass:
    """Create and register a built-in type class."""
    sym = LispSymbol(name)
    cls = LispClass(name=sym)
    return register_class(cls)


def _init_builtin_classes():
    """Initialize all built-in type classes.
    
    This is called lazily on first use to avoid circular import issues.
    """
    global _builtin_classes_initialized
    if _builtin_classes_initialized:
        return
    
    # Root class
    _make_builtin_class('T')
    
    # Numeric types
    _make_builtin_class('NUMBER')
    _make_builtin_class('REAL')
    _make_builtin_class('RATIONAL')
    _make_builtin_class('INTEGER')
    _make_builtin_class('FIXNUM')
    _make_builtin_class('BIGNUM')
    _make_builtin_class('RATIO')
    _make_builtin_class('FLOAT')
    _make_builtin_class('SHORT-FLOAT')
    _make_builtin_class('SINGLE-FLOAT')
    _make_builtin_class('DOUBLE-FLOAT')
    _make_builtin_class('LONG-FLOAT')
    _make_builtin_class('COMPLEX')
    
    # Sequence types
    _make_builtin_class('SEQUENCE')
    _make_builtin_class('LIST')
    _make_builtin_class('CONS')
    _make_builtin_class('NULL')
    _make_builtin_class('VECTOR')
    _make_builtin_class('STRING')
    _make_builtin_class('SIMPLE-STRING')
    _make_builtin_class('BASE-STRING')
    _make_builtin_class('SIMPLE-BASE-STRING')
    _make_builtin_class('BIT-VECTOR')
    _make_builtin_class('SIMPLE-BIT-VECTOR')
    _make_builtin_class('SIMPLE-VECTOR')
    _make_builtin_class('ARRAY')
    _make_builtin_class('SIMPLE-ARRAY')
    
    # Character type
    _make_builtin_class('CHARACTER')
    _make_builtin_class('BASE-CHAR')
    _make_builtin_class('STANDARD-CHAR')
    _make_builtin_class('EXTENDED-CHAR')
    
    # Symbol types
    _make_builtin_class('SYMBOL')
    _make_builtin_class('KEYWORD')
    
    # Function types
    _make_builtin_class('FUNCTION')
    _make_builtin_class('COMPILED-FUNCTION')
    _make_builtin_class('GENERIC-FUNCTION')
    _make_builtin_class('STANDARD-GENERIC-FUNCTION')
    _make_builtin_class('METHOD')
    _make_builtin_class('STANDARD-METHOD')
    
    # Class types
    _make_builtin_class('CLASS')
    _make_builtin_class('STANDARD-CLASS')
    _make_builtin_class('BUILT-IN-CLASS')
    _make_builtin_class('STRUCTURE-CLASS')
    _make_builtin_class('STANDARD-OBJECT')
    _make_builtin_class('STRUCTURE-OBJECT')
    
    # Stream types
    _make_builtin_class('STREAM')
    _make_builtin_class('BROADCAST-STREAM')
    _make_builtin_class('CONCATENATED-STREAM')
    _make_builtin_class('ECHO-STREAM')
    _make_builtin_class('FILE-STREAM')
    _make_builtin_class('STRING-STREAM')
    _make_builtin_class('SYNONYM-STREAM')
    _make_builtin_class('TWO-WAY-STREAM')
    
    # Hash table
    _make_builtin_class('HASH-TABLE')
    
    # Pathname types
    _make_builtin_class('PATHNAME')
    _make_builtin_class('LOGICAL-PATHNAME')
    
    # Package
    _make_builtin_class('PACKAGE')
    
    # Readtable
    _make_builtin_class('READTABLE')
    
    # Random state
    _make_builtin_class('RANDOM-STATE')
    
    # Condition types
    _make_builtin_class('CONDITION')
    _make_builtin_class('SERIOUS-CONDITION')
    _make_builtin_class('ERROR')
    _make_builtin_class('SIMPLE-ERROR')
    _make_builtin_class('SIMPLE-CONDITION')
    _make_builtin_class('WARNING')
    _make_builtin_class('STYLE-WARNING')
    _make_builtin_class('SIMPLE-WARNING')
    _make_builtin_class('TYPE-ERROR')
    _make_builtin_class('SIMPLE-TYPE-ERROR')
    _make_builtin_class('CELL-ERROR')
    _make_builtin_class('UNBOUND-VARIABLE')
    _make_builtin_class('UNDEFINED-FUNCTION')
    _make_builtin_class('UNBOUND-SLOT')
    _make_builtin_class('CONTROL-ERROR')
    _make_builtin_class('PROGRAM-ERROR')
    _make_builtin_class('PACKAGE-ERROR')
    _make_builtin_class('STREAM-ERROR')
    _make_builtin_class('READER-ERROR')
    _make_builtin_class('END-OF-FILE')
    _make_builtin_class('FILE-ERROR')
    _make_builtin_class('PARSE-ERROR')
    _make_builtin_class('PRINT-NOT-READABLE')
    _make_builtin_class('STORAGE-CONDITION')
    _make_builtin_class('ARITHMETIC-ERROR')
    _make_builtin_class('DIVISION-BY-ZERO')
    _make_builtin_class('FLOATING-POINT-OVERFLOW')
    _make_builtin_class('FLOATING-POINT-UNDERFLOW')
    _make_builtin_class('FLOATING-POINT-INEXACT')
    _make_builtin_class('FLOATING-POINT-INVALID-OPERATION')
    
    # Restart
    _make_builtin_class('RESTART')
    
    # Other
    _make_builtin_class('ATOM')
    _make_builtin_class('NIL')
    
    _builtin_classes_initialized = True


_builtin_classes_initialized = False

# Wrap find_class to ensure built-in classes are initialized
_original_find_class = find_class

def find_class(name: str) -> Optional[LispClass]:
    """Find a class by name, initializing built-in classes if needed."""
    global _builtin_classes_initialized
    if not _builtin_classes_initialized:
        _init_builtin_classes()
    return _original_find_class(name)


# =============================================================================
# Built-in type classes
# =============================================================================
# Register built-in Common Lisp type classes
# These are needed for CLOS method dispatch and FIND-CLASS

def _init_builtin_classes():
    """Initialize built-in type classes."""
    global _builtin_classes_initialized
    if _builtin_classes_initialized:
        return
    _builtin_classes_initialized = True
    
    from fclpy.lisptype import COMMON_LISP_PACKAGE
    
    # Create a list of built-in type names
    # These correspond to CL type specifiers that can be used as specializers
    builtin_types = [
        'T',  # The supertype of all types
        'NIL',
        'NULL',
        'ATOM',
        'SYMBOL',
        'KEYWORD',
        'CONS',
        'LIST',
        'SEQUENCE',
        'ARRAY',
        'VECTOR',
        'STRING',
        'BIT-VECTOR',
        'SIMPLE-ARRAY',
        'SIMPLE-VECTOR',
        'SIMPLE-STRING',
        'SIMPLE-BIT-VECTOR',
        'NUMBER',
        'REAL',
        'RATIONAL',
        'INTEGER',
        'RATIO',
        'FLOAT',
        'SHORT-FLOAT',
        'SINGLE-FLOAT',
        'DOUBLE-FLOAT',
        'LONG-FLOAT',
        'COMPLEX',
        'CHARACTER',
        'BASE-CHAR',
        'STANDARD-CHAR',
        'EXTENDED-CHAR',
        'FUNCTION',
        'COMPILED-FUNCTION',
        'GENERIC-FUNCTION',
        'STANDARD-GENERIC-FUNCTION',
        'METHOD',
        'STANDARD-METHOD',
        'CLASS',
        'STANDARD-CLASS',
        'BUILT-IN-CLASS',
        'STRUCTURE-CLASS',
        'STRUCTURE-OBJECT',
        'STANDARD-OBJECT',
        'HASH-TABLE',
        'PACKAGE',
        'PATHNAME',
        'LOGICAL-PATHNAME',
        'STREAM',
        'BROADCAST-STREAM',
        'CONCATENATED-STREAM',
        'ECHO-STREAM',
        'FILE-STREAM',
        'STRING-STREAM',
        'SYNONYM-STREAM',
        'TWO-WAY-STREAM',
        'READTABLE',
        'RANDOM-STATE',
        'CONDITION',
        'WARNING',
        'STYLE-WARNING',
        'SIMPLE-CONDITION',
        'SIMPLE-WARNING',
        'SIMPLE-ERROR',
        'SIMPLE-TYPE-ERROR',
        'ERROR',
        'TYPE-ERROR',
        'PARSE-ERROR',
        'PROGRAM-ERROR',
        'CONTROL-ERROR',
        'READER-ERROR',
        'UNDEFINED-FUNCTION',
        'UNDEFINED-VARIABLE',
        'DIVISION-BY-ZERO',
        'FLOATING-POINT-INVALID-OPERATION',
        'FLOATING-POINT-OVERFLOW',
        'FLOATING-POINT-UNDERFLOW',
        'CELL-ERROR',
        'UNBOUND-VARIABLE',
        'UNBOUND-SLOT',
        'RESTART',
        'METHOD-COMBINATION',
    ]
    
    # First create T as the root class
    t_sym = COMMON_LISP_PACKAGE.intern_symbol('T')
    t_class = LispClass(name=t_sym, direct_superclasses=[], direct_slots=[])
    register_class(t_class)
    
    # Create all other built-in type classes with T as superclass
    # except for condition classes which have a proper hierarchy
    condition_classes = {
        'CONDITION', 'WARNING', 'STYLE-WARNING', 'SIMPLE-CONDITION',
        'SIMPLE-WARNING', 'SIMPLE-ERROR', 'SIMPLE-TYPE-ERROR',
        'ERROR', 'TYPE-ERROR', 'PARSE-ERROR', 'PROGRAM-ERROR', 'CONTROL-ERROR',
        'READER-ERROR', 'UNDEFINED-FUNCTION', 'UNDEFINED-VARIABLE',
        'DIVISION-BY-ZERO', 'FLOATING-POINT-INVALID-OPERATION',
        'FLOATING-POINT-OVERFLOW', 'FLOATING-POINT-UNDERFLOW',
        'CELL-ERROR', 'UNBOUND-VARIABLE', 'UNBOUND-SLOT'
    }
    
    for type_name in builtin_types:
        if type_name == 'T':
            continue  # Already created
        
        sym = COMMON_LISP_PACKAGE.intern_symbol(type_name)
        
        # Build proper condition hierarchy
        if type_name in condition_classes:
            if type_name == 'CONDITION':
                # CONDITION is a direct subclass of T
                cls = LispClass(name=sym, direct_superclasses=[t_class], direct_slots=[])
            elif type_name in ('WARNING', 'ERROR'):
                # WARNING and ERROR are direct subclasses of CONDITION
                condition_cls = _original_find_class('CONDITION')
                if condition_cls is None:
                    # Fallback to T if CONDITION not yet created
                    condition_cls = t_class
                cls = LispClass(name=sym, direct_superclasses=[condition_cls], direct_slots=[])
            elif type_name == 'SIMPLE-CONDITION':
                # SIMPLE-CONDITION is a direct subclass of CONDITION
                condition_cls = _original_find_class('CONDITION')
                if condition_cls is None:
                    condition_cls = t_class
                cls = LispClass(name=sym, direct_superclasses=[condition_cls], direct_slots=[])
            elif type_name == 'SIMPLE-WARNING':
                # SIMPLE-WARNING inherits from both SIMPLE-CONDITION and WARNING
                warning_cls = _original_find_class('WARNING')
                simple_condition_cls = _original_find_class('SIMPLE-CONDITION')
                if warning_cls is None:
                    warning_cls = t_class
                if simple_condition_cls is None:
                    simple_condition_cls = t_class
                cls = LispClass(name=sym, direct_superclasses=[simple_condition_cls, warning_cls], direct_slots=[])
            elif type_name == 'SIMPLE-ERROR':
                # SIMPLE-ERROR inherits from both SIMPLE-CONDITION and ERROR
                error_cls = _original_find_class('ERROR')
                simple_condition_cls = _original_find_class('SIMPLE-CONDITION')
                if error_cls is None:
                    error_cls = t_class
                if simple_condition_cls is None:
                    simple_condition_cls = t_class
                cls = LispClass(name=sym, direct_superclasses=[simple_condition_cls, error_cls], direct_slots=[])
            elif type_name == 'TYPE-ERROR':
                # TYPE-ERROR is a subclass of ERROR
                error_cls = _original_find_class('ERROR')
                if error_cls is None:
                    error_cls = t_class
                cls = LispClass(name=sym, direct_superclasses=[error_cls], direct_slots=[])
            elif type_name == 'SIMPLE-TYPE-ERROR':
                # SIMPLE-TYPE-ERROR is both SIMPLE-ERROR and TYPE-ERROR
                simple_error_cls = _original_find_class('SIMPLE-ERROR')
                type_error_cls = _original_find_class('TYPE-ERROR')
                if simple_error_cls is None:
                    simple_error_cls = t_class
                if type_error_cls is None:
                    type_error_cls = t_class
                cls = LispClass(name=sym, direct_superclasses=[simple_error_cls, type_error_cls], direct_slots=[])
            elif type_name in ('PARSE-ERROR', 'PROGRAM-ERROR', 'CONTROL-ERROR', 'READER-ERROR'):
                # These are direct subclasses of ERROR
                error_cls = _original_find_class('ERROR')
                if error_cls is None:
                    error_cls = t_class
                cls = LispClass(name=sym, direct_superclasses=[error_cls], direct_slots=[])
            elif type_name in ('UNDEFINED-FUNCTION', 'UNDEFINED-VARIABLE'):
                # These are subclasses of CELL-ERROR
                cell_error_cls = _original_find_class('CELL-ERROR')
                if cell_error_cls is None:
                    # Fallback to ERROR
                    cell_error_cls = _original_find_class('ERROR')
                    if cell_error_cls is None:
                        cell_error_cls = t_class
                cls = LispClass(name=sym, direct_superclasses=[cell_error_cls], direct_slots=[])
            elif type_name == 'CELL-ERROR':
                # CELL-ERROR is a subclass of ERROR
                error_cls = _original_find_class('ERROR')
                if error_cls is None:
                    error_cls = t_class
                cls = LispClass(name=sym, direct_superclasses=[error_cls], direct_slots=[])
            elif type_name in ('DIVISION-BY-ZERO', 'FLOATING-POINT-INVALID-OPERATION',
                               'FLOATING-POINT-OVERFLOW', 'FLOATING-POINT-UNDERFLOW'):
                # These are subclasses of ARITHMETIC-ERROR (which is an ERROR)
                error_cls = _original_find_class('ERROR')
                if error_cls is None:
                    error_cls = t_class
                cls = LispClass(name=sym, direct_superclasses=[error_cls], direct_slots=[])
            elif type_name == 'UNBOUND-VARIABLE':
                # UNBOUND-VARIABLE is a subclass of UNDEFINED-VARIABLE
                undefined_var_cls = _original_find_class('UNDEFINED-VARIABLE')
                if undefined_var_cls is None:
                    undefined_var_cls = _original_find_class('CELL-ERROR')
                    if undefined_var_cls is None:
                        undefined_var_cls = t_class
                cls = LispClass(name=sym, direct_superclasses=[undefined_var_cls], direct_slots=[])
            elif type_name == 'UNBOUND-SLOT':
                # UNBOUND-SLOT is a subclass of CELL-ERROR
                cell_error_cls = _original_find_class('CELL-ERROR')
                if cell_error_cls is None:
                    cell_error_cls = t_class
                cls = LispClass(name=sym, direct_superclasses=[cell_error_cls], direct_slots=[])
            else:
                # Default condition class as ERROR subclass
                error_cls = _original_find_class('ERROR')
                if error_cls is None:
                    error_cls = t_class
                cls = LispClass(name=sym, direct_superclasses=[error_cls], direct_slots=[])
        else:
            # All other classes are direct subclasses of T
            cls = LispClass(name=sym, direct_superclasses=[t_class], direct_slots=[])
        
        register_class(cls)


# Initialize built-in classes when module is loaded
_init_builtin_classes()
