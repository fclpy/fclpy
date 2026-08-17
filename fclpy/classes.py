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
from fclpy.lisptype import LispSymbol, T, NIL, is_truthy, LispProgramError


@dataclass
class SlotDefinition:
    """Definition of a single slot in a class."""
    name: LispSymbol
    type_spec: Optional[Any] = None
    initform: Optional[Any] = None
    initarg: Optional[LispSymbol] = None
    allocation: str = "instance"  # "instance" or "class"
    documentation: Optional[str] = None
    # The environment DEFCLASS was evaluated in, captured so a slot's
    # initform (an unevaluated form) can later be evaluated the way CLHS
    # 7.1.2 wants -- lexically where the class was defined, not wherever
    # MAKE-INSTANCE/SHARED-INITIALIZE happen to run. None (the bootstrap
    # case, and any caller that builds a SlotDefinition directly) falls
    # back to the global environment when the initform is actually needed.
    definition_env: Optional[Any] = None

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


def resolve_class_designator(spec: Any) -> Any:
    """Resolve a MAKE-INSTANCE-style class designator (CLHS 7.1): a symbol
    or string names a class and must resolve to one. Anything else --
    already a class object, or any other value a DEFMETHOD chooses to
    specialize MAKE-INSTANCE's first parameter on -- passes through
    unchanged, so a method dispatching on some other type still matches
    it (ansi-test's make-instance.lsp defines one specialized on an
    *instance*, purely to exercise that dispatch on this argument works at
    all).

    Shared by `lispfunc.classes.make_instance` (the entry point for a
    direct Python call, which the unit-test suite makes) and
    `lispfunc.misc_clos._default_make_instance` (the entry point for every
    ordinary Lisp-level `(make-instance ...)` call, since evaluating even
    one DEFMETHOD on MAKE-INSTANCE replaces its *entire* environment
    binding with the bare GenericFunction object -- see plan.md's CLOS
    consolidation notes -- bypassing the Python wrapper from then on).
    """
    if isinstance(spec, LispSymbol):
        cls = find_class(spec.name)
        if cls is None:
            raise NameError(f"Class not found: {spec.name}")
        return cls
    if isinstance(spec, str):
        cls = find_class(spec)
        if cls is None:
            raise NameError(f"Class not found: {spec}")
        return cls
    return spec


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
#
# This is the *one* generic-function/method mechanism in fclpy. DEFGENERIC
# and DEFMETHOD (fclpy/lispfunc/evaluation_special_forms.py) build directly
# on ensure_generic_function/add_method/call_generic_function below, rather
# than each rolling its own dispatcher -- they used to (plan.md Finding L:
# "two CLOS implementations coexist"), which is also why CALL-NEXT-METHOD
# used to raise "No next method available" unconditionally: the dispatcher
# that actually ran a DEFMETHOD-defined method never populated the next-
# method context this module's CALL-NEXT-METHOD reads.


class EqlSpecializer:
    """An `(eql form)` specializer (CLHS 7.6.2): matches an argument by EQL
    to one specific object, evaluated once when the method is defined."""
    __slots__ = ('value',)

    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f"#<EQL-SPECIALIZER {self.value!r}>"


@dataclass(eq=False)
class Method:
    """A method in a generic function.

    `specializers` holds one entry per specializable (required) parameter:
    `None` for T/unspecialized, a `LispClass`, an `EqlSpecializer`, or a raw
    type-name symbol that named no modeled class (matched via TYPEP).
    `qualifiers` holds the method's raw qualifier objects (e.g. one
    `:before` keyword) -- CLHS 7.6.6.2's standard method combination reads
    their names to decide primary/before/after/around role.

    `eq=False` keeps the dataclass's default (identity-based, hashable)
    `__eq__`/`__hash__` instead of the field-wise ones `@dataclass` would
    otherwise generate. A field-wise `__eq__` also makes a class unhashable
    unless told not to (Python sets `__hash__ = None` alongside a generated
    `__eq__`), and a GENERIC-FUNCTION's methods are ordinary Lisp function
    values that the evaluator's `get_func_signature_info` caches keyed
    partly on the callable object itself -- `TypeError: unhashable type` as
    the value of *any* call to a method-carrying generic function is what
    that produces, not a niche corner case.
    """
    specializers: List[Any]
    function: Callable
    qualifiers: List[Any] = field(default_factory=list)
    generic_function: Optional['GenericFunction'] = None
    lambda_list: Optional[Any] = None


@dataclass(eq=False)
class GenericFunction:
    """A generic function: a name, a lambda-list, a set of methods, and the
    method combination that decides how those methods are assembled into an
    effective method (CLHS 7.6.6).

    `method_combination` is None until DEFGENERIC's `:method-combination`
    option supplies one, and None means *standard* combination -- not "no
    combination". Resolving it lazily rather than storing STANDARD here
    keeps `ensure_generic_function` free of an import cycle back into the
    combination registry, which is populated after this class is defined.
    """
    name: LispSymbol
    methods: List[Method] = field(default_factory=list)
    documentation: Optional[str] = None
    lambda_list: Optional[Any] = None
    method_combination: Optional['MethodCombination'] = None

    def __repr__(self):
        return f"#<STANDARD-GENERIC-FUNCTION {self.name.name}>"

    def __call__(self, *args):
        # Makes a GenericFunction usable anywhere an ordinary callable is
        # (FUNCALL/APPLY, mapped over by MAPCAR, ...) instead of needing a
        # separate "is this a generic function?" branch at every call site.
        return call_generic_function(self, list(args))


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
    documentation: Optional[str] = None,
    lambda_list: Optional[Any] = None,
) -> GenericFunction:
    """ENSURE-GENERIC-FUNCTION: get or create a generic function.

    Keyed by name string rather than symbol identity, so a DEFMETHOD in one
    package and a DEFGENERIC in another that both name the (inherited,
    non-shadowed) standard symbol SHARED-INITIALIZE resolve to the same
    generic function -- the way CALL-GENERIC-FUNCTION/ADD-METHOD already
    expected before this function existed.

    Args:
        name: Symbol naming the generic function
        documentation: Documentation string, set only if provided
        lambda_list: Lambda list, set only if provided (DEFGENERIC re-sets
            it every time it runs; DEFMETHOD alone never supplies one)

    Returns:
        The generic function (newly created or existing)
    """
    name_str = name.name if isinstance(name, LispSymbol) else str(name)

    gf = _generic_registry.find_generic(name_str)
    if gf is None:
        gf = GenericFunction(name=name)
        _generic_registry.register_generic(gf)

    if documentation is not None:
        gf.documentation = documentation
    if lambda_list is not None:
        gf.lambda_list = lambda_list

    return gf


def _specializer_eq(a: Any, b: Any) -> bool:
    """Congruence test for two specializers (CLHS 7.6.3): adding a method
    whose qualifiers and specializers are congruent to an existing method
    replaces it rather than adding a second, shadowed copy -- without this,
    re-evaluating a DEFMETHOD (routine while iterating on one) piles up
    duplicates that all still fire."""
    if a is None or b is None:
        return a is b
    if isinstance(a, EqlSpecializer) and isinstance(b, EqlSpecializer):
        from fclpy.lispfunc.comparison import eql as _eql
        return _eql(a.value, b.value) is T
    if isinstance(a, LispClass) and isinstance(b, LispClass):
        return a is b
    if isinstance(a, LispSymbol) and isinstance(b, LispSymbol):
        return a.name.upper() == b.name.upper()
    return a == b


def _specializers_congruent(a_list: List[Any], b_list: List[Any]) -> bool:
    if len(a_list) != len(b_list):
        return False
    return all(_specializer_eq(x, y) for x, y in zip(a_list, b_list))


def _qualifier_names(method: 'Method') -> set:
    """Upcased, colon-stripped qualifier names, used only to decide the
    method's standard-method-combination role -- introspection
    (METHOD-QUALIFIERS) reads `method.qualifiers` itself, unnormalized."""
    names = set()
    for q in method.qualifiers:
        n = q.name if hasattr(q, 'name') else str(q)
        names.add(n.upper().lstrip(':'))
    return names


def add_method(
    generic_function: GenericFunction,
    specializers: List[Any],
    method_function: Callable,
    qualifiers: Optional[List[Any]] = None,
) -> GenericFunction:
    """ADD-METHOD: add a method to a generic function.

    Replaces any existing method with congruent qualifiers and specializers
    (CLHS 7.6.3) instead of appending a duplicate. Methods are stored
    unsorted; `call_generic_function` sorts applicable methods by
    specificity at call time, since a class's specificity can change after
    the method was added (a later DEFCLASS widening the hierarchy).

    Args:
        generic_function: The generic function to add to
        specializers: One specializer per specializable parameter (None,
            a LispClass, an EqlSpecializer, or a raw type-name symbol)
        method_function: The actual method function
        qualifiers: Method qualifiers, e.g. [:before] -- () for a primary
            method

    Returns:
        The generic function
    """
    qualifiers = list(qualifiers or [])
    new_method = Method(
        specializers=specializers,
        function=method_function,
        qualifiers=qualifiers,
        generic_function=generic_function,
    )
    for i, existing in enumerate(generic_function.methods):
        if existing.qualifiers == qualifiers and _specializers_congruent(existing.specializers, specializers):
            generic_function.methods[i] = new_method
            return generic_function
    generic_function.methods.append(new_method)
    return generic_function


def remove_method(generic_function: GenericFunction, method: Method) -> GenericFunction:
    """REMOVE-METHOD: drop one method object from a generic function."""
    generic_function.methods = [m for m in generic_function.methods if m is not method]
    return generic_function


def class_of(obj: Any) -> LispClass:
    """CLASS-OF (CLHS 7.1.1): the class object naming obj's class.

    Every object has a class, not only CLOS instances -- this used to
    answer the bare symbol T for anything that was not a LispInstance
    ("In full CLOS, every object would have a class" read the comment this
    replaced), which is a type error waiting to be TYPEP'd and, for
    specializer matching below, made it impossible to dispatch a method on
    any built-in type. TYPE-OF already classifies any value correctly
    (including compound array specifiers); this asks it for the type name
    and resolves that to the matching built-in LispClass, falling back to T
    only when the type has no modeled class at all.
    """
    if isinstance(obj, LispInstance):
        return obj.lisp_class
    from fclpy.lispfunc.comparison import type_of as _type_of
    from fclpy.lispfunc.core import _consp_internal, car as _car
    result = _type_of(obj)
    if _consp_internal(result):
        result = _car(result)
    name = result.name if isinstance(result, LispSymbol) else str(result)
    return find_class(name) or find_class('T')


def _arg_matches_specializer(arg: Any, spec: Any) -> bool:
    """Does one argument satisfy one specializer (CLHS 7.6.2)?"""
    if spec is None:
        return True
    if isinstance(spec, EqlSpecializer):
        from fclpy.lispfunc.comparison import eql as _eql
        return _eql(arg, spec.value) is T
    if isinstance(spec, LispClass):
        if isinstance(arg, LispInstance):
            return _is_instance_of(arg, spec)
        # A built-in-type specializer (INTEGER, STRING, ...) applied to a
        # non-instance: TYPEP already knows every one of these, and asking
        # it -- rather than walking this class's (approximate, non-CLOS)
        # get_linearized_superclasses -- is one type-predicate mechanism
        # instead of a second, competing one (plan.md Finding M).
        from fclpy.lispfunc.comparison import typep as _typep
        return _typep(arg, spec.name) is T
    # A specializer symbol that named no modeled LispClass at all (a CLHS
    # type this codebase has no class object for): still ask TYPEP, so any
    # type name is usable as a specializer, not only the ones in
    # _init_builtin_classes's list.
    from fclpy.lispfunc.comparison import typep as _typep
    return _typep(arg, spec) is T


def _matches_specializers(args: List[Any], specializers: List[Any]) -> bool:
    """Is a method with these specializers applicable to these call args?"""
    if len(specializers) > len(args):
        return False
    return all(_arg_matches_specializer(arg, spec) for arg, spec in zip(args, specializers))


def _specificity_key(specializers: List[Any]) -> tuple:
    """Approximate specificity ordering (CLHS 7.6.6.1's true rule is the
    argument's class precedence list position, which needs a real C3
    linearization this codebase does not have -- get_linearized_superclasses
    gives ancestor *count*, which agrees with CPL order for the single-
    inheritance chains DEFCLASS mostly produces here). An EQL specializer is
    always more specific than any class specializer, per CLHS."""
    key = []
    for spec in specializers:
        if spec is None:
            key.append(-1)
        elif isinstance(spec, EqlSpecializer):
            key.append(10_000)
        elif isinstance(spec, LispClass):
            key.append(len(spec.get_linearized_superclasses()))
        else:
            key.append(0)
    return tuple(key)


def _is_instance_of(instance: LispInstance, lisp_class: LispClass) -> bool:
    """Check if an instance is of a given class (including superclasses)."""
    for cls in instance.lisp_class.get_linearized_superclasses():
        if cls is lisp_class:
            return True
    return False


def _no_applicable_method(gf: GenericFunction, args: List[Any]):
    gf_name = gf.name.name if isinstance(gf.name, LispSymbol) else str(gf.name)
    raise LispProgramError(
        f"No applicable method for {gf_name} with arguments: {args}"
    )


class MakeMethod:
    """The method `(MAKE-METHOD form)` denotes (CLHS 7.6.6.2): a method with
    no qualifiers and no specializers whose body is one form.

    Method combination uses it to splice a computed sub-combination into a
    position that expects a method -- standard combination's before/primary/
    after core is passed to the innermost :around method this way -- so
    CALL-METHOD and the next-method chain have exactly *one* kind of thing
    to call rather than "a method, or else the special core closure".
    """
    __slots__ = ('function', 'qualifiers', 'specializers', 'generic_function', 'lambda_list')

    def __init__(self, function: Callable, generic_function: Optional[GenericFunction] = None):
        self.function = function
        self.qualifiers = []
        self.specializers = []
        self.generic_function = generic_function
        self.lambda_list = None

    def __repr__(self):
        return "#<METHOD (MAKE-METHOD)>"


# The dynamic (per-call, reentrant) CALL-NEXT-METHOD/NEXT-METHOD-P context:
# a stack of frames, one per method currently executing, so a method that
# itself triggers a nested generic-function call (directly, or by calling a
# different generic function that recurses back into this one) sees its own
# frame on top rather than a sibling call's -- a single flat "last call's
# next methods" slot (what this replaced) is exactly the kind of state a
# single-threaded interpreter with recursive calls cannot share safely.
#
# A frame is `{'args', 'next', 'gf'}` and there is only one *kind* of it:
# `next` is the ordered list of methods CALL-NEXT-METHOD may reach from
# here, empty when there is none. It used to carry a `kind` discriminator
# ('around'/'primary'/'none') with a separate `core` closure for the around
# chain, which is why NEXT-METHOD-P answered T inside every :around method
# whether or not anything remained, and why nothing but standard
# combination could ever build a chain at all.
_call_stack: List[Dict[str, Any]] = []


def call_method(method: Any, next_methods: List[Any], args: List[Any]) -> Any:
    """CALL-METHOD's operator (CLHS 7.6.6.2): invoke one method with
    `next_methods` as the chain CALL-NEXT-METHOD may walk from inside it.

    **This is the one place a method is ever invoked.** Standard
    combination, the short and long forms of DEFINE-METHOD-COMBINATION and
    the CALL-METHOD operator itself all bottom out here, so the
    next-method context cannot disagree between them.
    """
    _call_stack.append({
        'args': args,
        'next': list(next_methods),
        'gf': getattr(method, 'generic_function', None),
    })
    try:
        return method.function(*args)
    finally:
        _call_stack.pop()


# Some generic functions (the CLOS metaobject protocol's ALLOCATE-INSTANCE/
# INITIALIZE-INSTANCE/SHARED-INITIALIZE/... -- see
# fclpy/lispfunc/misc_clos.py) are supposed to always have a default
# primary method installed by the module that owns them, not only a set of
# user-added ones. `_generic_registry` is a bare module-level dict nothing
# stops arbitrary code (deliberately, the unit-test suite, to isolate
# user-defined generics between tests) from clearing or replacing wholesale,
# which used to strand these "system" generic functions with no default
# and no way to recover one. A generic function with no methods at all
# self-heals here rather than at every call site that might reach it.
_default_method_installers: Dict[str, Callable[[GenericFunction], None]] = {}


def register_default_method_installer(name, installer: Callable[[GenericFunction], None]):
    """Register `installer(gf)` to (re-)populate a generic function's
    default method whenever it is found with none at all."""
    name_str = name.name if isinstance(name, LispSymbol) else str(name)
    _default_method_installers[name_str] = installer


def compute_applicable_methods(gf: GenericFunction, args: List[Any]) -> List[Method]:
    """Every method of `gf` applicable to `args`, most-specific-first (CLHS
    7.6.6.1) -- the one selection every caller uses, so COMPUTE-APPLICABLE-
    METHODS cannot disagree with what a real call would invoke."""
    if not isinstance(gf, GenericFunction):
        # Reached from the COMPUTE-APPLICABLE-METHODS operator, which any
        # value can be handed. Signalling beats letting Python's
        # AttributeError surface as the form's value (standing rule 2).
        raise LispProgramError(
            f"COMPUTE-APPLICABLE-METHODS: {gf!r} is not a generic function")
    applicable = [m for m in gf.methods if _matches_specializers(args, m.specializers)]
    # Stable, so methods whose specificity this codebase cannot yet tell
    # apart (see _specificity_key: there is no real class precedence list
    # for the built-in classes) keep definition order rather than an
    # arbitrary one.
    applicable.sort(key=lambda m: _specificity_key(m.specializers), reverse=True)
    return applicable


class MethodCombinationError(LispProgramError):
    """The condition METHOD-COMBINATION-ERROR names (CLHS 7.6.6.4): the
    applicable methods cannot be assembled into an effective method -- an
    unrecognized qualifier, or a required method group left empty."""


# ==============================================================================
# Method combination (CLHS 7.6.6)
# ==============================================================================
# A method combination *type* knows how to turn the applicable methods into
# an effective method. A method combination *object* is that type plus the
# options the generic function supplied in `(:method-combination name
# . options)`. The generic function holds the object; the registry below
# holds the types.
#
# Every type produces its effective method out of `call_method` above, so
# CALL-NEXT-METHOD, NEXT-METHOD-P and CALL-METHOD behave identically no
# matter which combination is in force. Standard combination assembles the
# chain in Python because its shape is fixed; the DEFINE-METHOD-COMBINATION
# forms assemble a Lisp *form* instead, because the operator they combine
# with may be a macro whose evaluation order is part of the semantics --
# `(and (call-method m1) (call-method m2))` must stop at the first NIL, and
# ansi-test observes exactly that.


class MethodCombinationType:
    """Base class: a named way of combining methods."""

    def __init__(self, name: LispSymbol, documentation: Optional[str] = None):
        self.name = name
        self.documentation = documentation

    @property
    def name_string(self) -> str:
        return self.name.name if isinstance(self.name, LispSymbol) else str(self.name)

    def invoke(self, gf: GenericFunction, applicable: List[Method],
               args: List[Any], options: List[Any]) -> Any:
        raise NotImplementedError

    def __repr__(self):
        return f"#<METHOD-COMBINATION {self.name_string}>"


class MethodCombination:
    """A method combination object: a type plus the (unevaluated) options
    one generic function gave it."""
    __slots__ = ('type', 'options')

    def __init__(self, type_: MethodCombinationType, options: Optional[List[Any]] = None):
        self.type = type_
        self.options = list(options or [])

    @property
    def name(self):
        return self.type.name

    @property
    def documentation(self):
        return self.type.documentation

    def invoke(self, gf: GenericFunction, applicable: List[Method], args: List[Any]) -> Any:
        return self.type.invoke(gf, applicable, args, self.options)

    def __repr__(self):
        return f"#<METHOD-COMBINATION {self.type.name_string}>"


class StandardMethodCombination(MethodCombinationType):
    """CLHS 7.6.6.2: :around methods outermost, then :before methods
    most-specific-first, the most specific applicable primary method (which
    may CALL-NEXT-METHOD into the rest), then :after methods
    least-specific-first."""

    def invoke(self, gf, applicable, args, options):
        primaries = [m for m in applicable if not m.qualifiers]
        befores = [m for m in applicable if _qualifier_names(m) == {'BEFORE'}]
        afters = [m for m in applicable if _qualifier_names(m) == {'AFTER'}]
        arounds = [m for m in applicable if _qualifier_names(m) == {'AROUND'}]

        recognized = ({'BEFORE'}, {'AFTER'}, {'AROUND'})
        for m in applicable:
            if m.qualifiers and _qualifier_names(m) not in recognized:
                raise MethodCombinationError(
                    f"{_gf_name(gf)}: standard method combination accepts only "
                    f":BEFORE, :AFTER and :AROUND qualifiers, not "
                    f"{[str(q) for q in m.qualifiers]}")

        def run_core(*_args):
            if not primaries:
                _no_applicable_method(gf, args)
            for m in befores:
                call_method(m, [], args)
            result = call_method(primaries[0], primaries[1:], args)
            for m in reversed(afters):
                call_method(m, [], args)
            return result

        if not arounds:
            return run_core()
        return call_method(arounds[0], arounds[1:] + [MakeMethod(run_core, gf)], args)


def _gf_name(gf) -> str:
    name = getattr(gf, 'name', gf)
    return name.name if isinstance(name, LispSymbol) else str(name)


def _order_option(options: List[Any], default: str = 'MOST-SPECIFIC-FIRST') -> str:
    """The ordering a short-form combination was given in
    `(:method-combination name [order])` -- :MOST-SPECIFIC-FIRST (the
    default) or :MOST-SPECIFIC-LAST."""
    for opt in options:
        text = (opt.name if isinstance(opt, LispSymbol) else str(opt)).upper().lstrip(':')
        if text in ('MOST-SPECIFIC-FIRST', 'MOST-SPECIFIC-LAST'):
            return text
    return default


def _apply_order(methods: List[Any], order: str) -> List[Any]:
    return list(reversed(methods)) if order == 'MOST-SPECIFIC-LAST' else list(methods)


class ShortFormMethodCombination(MethodCombinationType):
    """The short form of DEFINE-METHOD-COMBINATION (CLHS), and with it the
    nine built-in combination types of CLHS 7.6.6.4.

    Every applicable method must be qualified either with the combination's
    own name (a *primary* method) or with :AROUND; the effective method is
    `(operator (call-method p1) (call-method p2) ...)` wrapped in the
    :AROUND chain. Building that as a real form rather than folding the
    results in Python is what makes the AND and OR combinations short-
    circuit, which is directly observable: ansi-test's
    DEFGENERIC-METHOD-COMBINATION.AND.1 asserts that the methods after the
    first NIL never run.
    """

    def __init__(self, name, operator=None, identity_with_one_argument=False,
                 documentation=None):
        super().__init__(name, documentation)
        self.operator = operator if operator is not None else name
        self.identity_with_one_argument = identity_with_one_argument

    def invoke(self, gf, applicable, args, options):
        from fclpy.lispfunc.sequence_protocol import make_lisp_list

        own = self.name_string.upper()
        primaries, arounds = [], []
        for m in applicable:
            names = _qualifier_names(m)
            if names == {own}:
                primaries.append(m)
            elif names == {'AROUND'}:
                arounds.append(m)
            else:
                raise MethodCombinationError(
                    f"{_gf_name(gf)}: the {own} method combination accepts only "
                    f"{own} and :AROUND qualifiers, not {[str(q) for q in m.qualifiers]}")

        if not primaries:
            raise MethodCombinationError(
                f"{_gf_name(gf)}: no applicable primary ({own}) method")

        primaries = _apply_order(primaries, _order_option(options))
        calls = [make_lisp_list([_CALL_METHOD_SYM, m]) for m in primaries]
        if self.identity_with_one_argument and len(calls) == 1:
            core = calls[0]
        else:
            core = make_lisp_list([self.operator] + calls)

        return _run_effective_method(gf, core, arounds, args)


class LongFormMethodCombination(MethodCombinationType):
    """The long form of DEFINE-METHOD-COMBINATION: `builder(gf, applicable,
    options)` answers `(form, env)`, the effective method the user's body
    computed. Parsing the method-group specifiers and the combination's own
    lambda list is the evaluator's job (see
    `evaluation_special_forms.eval_define_method_combination`), so this
    class holds no second copy of either."""

    def __init__(self, name, builder: Callable, documentation=None):
        super().__init__(name, documentation)
        self.builder = builder

    def invoke(self, gf, applicable, args, options):
        from fclpy.lispfunc.evaluation_core import eval as _eval
        form, env = self.builder(gf, applicable, options, args)
        _effective_context.append({'gf': gf, 'args': args, 'env': env})
        try:
            return _eval(form, env)
        finally:
            _effective_context.pop()


# The arguments of the generic-function call whose effective method *form*
# is currently being evaluated. CALL-METHOD reads its arguments from here
# rather than taking them as operands, because CLHS gives it none: an
# effective method is a function of the original call's arguments, and the
# combination that built the form does not name them.
_effective_context: List[Dict[str, Any]] = []

def _cl_symbol(name: str) -> LispSymbol:
    """The interned COMMON-LISP symbol of this name. A form built out of
    bare `LispSymbol(...)` objects would name *different* symbols from the
    ones the environment binds -- function and variable lookup is by symbol
    identity, not by name (see CLAUDE.md) -- so the `(PROGN ...)` an
    effective method is made of has to be the real PROGN."""
    from fclpy.lisptype import COMMON_LISP_PACKAGE
    return COMMON_LISP_PACKAGE.intern_symbol(name)


_CALL_METHOD_SYM = _cl_symbol('CALL-METHOD')


def _run_effective_method(gf, core_form, arounds, args):
    """Evaluate a combination-built effective method: `core_form` wrapped in
    `arounds` (most-specific-first), which is the same :AROUND chain
    standard combination builds -- one shape, not one per combination."""
    from fclpy.lispfunc.evaluation_core import eval as _eval
    import fclpy.state as _state

    # The form names only the combination's operator and CALL-METHOD, so the
    # global environment is the right (and only correct) place to resolve it
    # -- a short-form combination's operator is looked up where the
    # combination was defined, not wherever the call happens to originate.
    env = _state.current_environment

    def run_core(*call_args):
        _effective_context.append({'gf': gf, 'args': list(call_args) or args, 'env': env})
        try:
            return _eval(core_form, env)
        finally:
            _effective_context.pop()

    if not arounds:
        return run_core(*args)
    return call_method(arounds[0], arounds[1:] + [MakeMethod(run_core, gf)], args)


# ------------------------------------------------------------------ registry

_method_combination_types: Dict[str, MethodCombinationType] = {}


def register_method_combination_type(type_: MethodCombinationType) -> MethodCombinationType:
    """Install a method combination type under its name. DEFINE-METHOD-
    COMBINATION redefining an existing name replaces it here, which is what
    makes every generic function already using it pick the new definition
    up (they hold the *object*, which holds the type by reference)."""
    _method_combination_types[type_.name_string.upper()] = type_
    return type_


def find_method_combination_type(name) -> Optional[MethodCombinationType]:
    key = (name.name if isinstance(name, LispSymbol) else str(name)).upper()
    return _method_combination_types.get(key)


STANDARD_METHOD_COMBINATION = StandardMethodCombination(_cl_symbol('STANDARD'))
register_method_combination_type(STANDARD_METHOD_COMBINATION)

# CLHS 7.6.6.4's built-in combination types, each defined as if by the short
# form of DEFINE-METHOD-COMBINATION with itself as the operator. APPEND and
# LIST are the two that are *not* :identity-with-one-argument, which is
# observable: with a single applicable method, the LIST combination answers
# `(x)` and not `x`.
for _op, _identity in (('PROGN', True), ('AND', True), ('OR', True), ('+', True),
                       ('MAX', True), ('MIN', True), ('NCONC', True),
                       ('APPEND', False), ('LIST', False)):
    register_method_combination_type(
        ShortFormMethodCombination(_cl_symbol(_op), identity_with_one_argument=_identity))
del _op, _identity


def method_combination_of(gf: GenericFunction) -> MethodCombination:
    """The method combination in force for `gf` -- standard unless
    DEFGENERIC said otherwise."""
    comb = getattr(gf, 'method_combination', None)
    if comb is None:
        return MethodCombination(STANDARD_METHOD_COMBINATION)
    return comb


def call_generic_function(gf: GenericFunction, args: List[Any]) -> Any:
    """Call a generic function: compute the applicable methods, then let the
    generic function's method combination assemble and run them (CLHS
    7.6.6)."""
    if not gf.methods:
        installer = _default_method_installers.get(_gf_name(gf))
        if installer is not None:
            installer(gf)

    applicable = compute_applicable_methods(gf, args)
    if not applicable:
        # CLHS 7.6.6: no applicable methods is decided *before* the method
        # combination is consulted, and is an error for every combination --
        # not something a combination can answer by combining nothing. A
        # long-form combination whose body maps over an empty method group
        # otherwise happily produces `(vector)` and returns #().
        _no_applicable_method(gf, args)
    return method_combination_of(gf).invoke(gf, applicable, args)


def call_next_method(*args) -> Any:
    """CALL-NEXT-METHOD: call the next method in the chain the currently
    executing method was given by CALL-METHOD."""
    if not _call_stack:
        raise LispProgramError(
            "CALL-NEXT-METHOD: no method is currently executing")
    frame = _call_stack[-1]
    call_args = list(args) if args else frame['args']
    remaining = frame['next']
    if not remaining:
        raise LispProgramError(
            f"CALL-NEXT-METHOD: no next method for {_gf_name(frame['gf'])}")
    return call_method(remaining[0], remaining[1:], call_args)


def next_method_p() -> bool:
    """NEXT-METHOD-P: would CALL-NEXT-METHOD succeed right now?"""
    if not _call_stack:
        return False
    return bool(_call_stack[-1]['next'])


def effective_method_arguments() -> List[Any]:
    """The arguments of the generic-function call whose effective method
    form is being evaluated -- CALL-METHOD's implicit operands."""
    if not _effective_context:
        raise LispProgramError(
            "CALL-METHOD is only valid inside an effective method form")
    return _effective_context[-1]['args']


def make_method_from_thunk(run: Callable, generic_function=None) -> MakeMethod:
    """Wrap `run(*args)` as the method `(MAKE-METHOD form)` denotes, with the
    effective-method context established around it so a CALL-METHOD nested
    inside that form still knows the original call's arguments."""
    def invoke(*call_args):
        _effective_context.append(
            {'gf': generic_function, 'args': list(call_args), 'env': None})
        try:
            return run(*call_args)
        finally:
            _effective_context.pop()

    return MakeMethod(invoke, generic_function)


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
