"""Registry for builtin Common Lisp functions and special operators.

Provides decorators @cl_function and @cl_special to register Python callables
with metadata and utilities to register entire modules.
"""
from typing import Callable, Dict, Optional
from dataclasses import dataclass, field
import inspect
import fclpy.lisptype as lisptype


@dataclass
class RegistryEntry:
    """Metadata for a registered Lisp function or special operator."""
    name: str                              # LISP_NAME (uppercase)
    py_name: str                           # Python function name
    kind: str = 'function'                 # 'function', 'special', or 'macro'
    arg_spec: Optional[str] = None         # Lambda-list specification, e.g. '(x y &optional z)'
    documentation: Optional[str] = None    # Docstring or help text
    side_effects: bool = False             # Whether the function has side effects
    extra: Dict = field(default_factory=dict)  # Additional metadata fields
    func: Optional[Callable] = None        # The actual Python callable
    
    def get(self, key: str, default=None):
        """Dict-compatible get() method for backward compatibility."""
        if key == 'py_name':
            return self.py_name
        elif key == 'name':
            return self.name
        elif key == 'kind':
            return self.kind
        elif key == 'arg_spec':
            return self.arg_spec
        elif key == 'documentation':
            return self.documentation
        elif key == 'side_effects':
            return self.side_effects
        else:
            return self.extra.get(key, default)
    
    def __getitem__(self, key: str):
        """Dict-compatible [] access for backward compatibility."""
        result = self.get(key)
        if result is None:
            raise KeyError(key)
        return result
    
    def items(self):
        """Dict-compatible items() for backward compatibility."""
        yield 'name', self.name
        yield 'py_name', self.py_name
        yield 'kind', self.kind
        yield 'arg_spec', self.arg_spec
        yield 'documentation', self.documentation
        yield 'side_effects', self.side_effects
        for k, v in self.extra.items():
            yield k, v


# Mapping: LISP_NAME (str) -> RegistryEntry
function_registry: Dict[str, RegistryEntry] = {}
special_registry: Dict[str, RegistryEntry] = {}


def _to_lisp_name(py_name: str) -> str:
    """Convert a Python identifier used in lispfunc modules to a Lisp name.

    Uses the same py_str_to_sym helper in lisptype to ensure consistency.
    Returns the upper-case string name, e.g. 'car' -> 'CAR', 'list_s_star_' -> 'LIST*'.
    """
    try:
        sym = lisptype.py_str_to_sym(py_name)
        return sym.name
    except Exception:
        return py_name.upper()


def cl_function(lisp_name: str, **meta):
    """Decorator to register a Python function as a Common Lisp function.

    Example:
      @cl_function('CAR', arg_spec='(x)', side_effects=False)
      def car(x): ...
    """
    def decorator(func: Callable):
        # Extract standard fields from meta (don't mutate original)
        arg_spec = meta.get('arg_spec', None)
        documentation = meta.get('documentation', None)
        side_effects = meta.get('side_effects', False)
        
        # Build extra dict with remaining fields (exclude standard ones)
        standard_keys = {'arg_spec', 'documentation', 'side_effects'}
        extra = {k: v for k, v in meta.items() if k not in standard_keys}
        
        # Create registry entry with backward compatibility
        entry = RegistryEntry(
            name=lisp_name,
            py_name=func.__name__,
            kind='function',
            arg_spec=arg_spec,
            documentation=documentation,
            side_effects=side_effects,
            extra=extra,
            func=func  # Store actual callable for direct lookup
        )
        function_registry[lisp_name] = entry
        return func
    return decorator


def cl_special(lisp_name: str, **meta):
    """Decorator to register a special operator (handled by evaluator).

    Example:
      @cl_special('IF')
      def special_if(...): ...
    """
    def decorator(func: Callable):
        # Extract standard fields from meta (don't mutate original)
        arg_spec = meta.get('arg_spec', None)
        documentation = meta.get('documentation', None)
        side_effects = meta.get('side_effects', False)
        
        # Build extra dict with remaining fields (exclude standard ones)
        standard_keys = {'arg_spec', 'documentation', 'side_effects'}
        extra = {k: v for k, v in meta.items() if k not in standard_keys}
        
        # Create registry entry with backward compatibility
        entry = RegistryEntry(
            name=lisp_name,
            py_name=func.__name__,
            kind='special',
            arg_spec=arg_spec,
            documentation=documentation,
            side_effects=side_effects,
            extra=extra,
            func=func  # Store actual callable for direct lookup
        )
        special_registry[lisp_name] = entry
        return func
    return decorator


def register_module(module):
    """Inspect a module and register its public callables into the function registry.

    This registers only plain functions (not classes), using the Python name -> Lisp
    name conversion. Existing explicit decorator registrations are not overwritten.
    """
    # Build set of already-registered Python names to avoid duplicate auto-registrations
    registered_py = {m.get("py_name") for m in function_registry.values()} | {m.get("py_name") for m in special_registry.values()}

    for name, obj in vars(module).items():
        if name.startswith("_"):
            continue
        if not callable(obj):
            continue
        # Skip modules/classes
        if inspect.isclass(obj) or inspect.ismodule(obj):
            continue

        # If this Python callable has already been registered (via decorator), skip auto-registration
        if name in registered_py:
            continue

        # Compute the candidate Lisp name for the python function name.
        lisp_name = _to_lisp_name(name)
        # Prefer canonical hyphenated Lisp names (e.g. HASH-TABLE-P) over underscore variants
        hyphenated = lisp_name.replace("_", "-")

        

        # Derive a shorter canonical form by stripping common implementation suffixes
        # e.g. -FN and -TYPE are often used in Python names; prefer the base Lisp name when safe.
        canonical = hyphenated
        if hyphenated.endswith("-FN"):
            canonical = hyphenated[:-3]
        elif hyphenated.endswith("-TYPE"):
            canonical = hyphenated[:-5]

        # If the canonical (stripped) form already exists, prefer non-stub implementations.
        if canonical in function_registry or canonical in special_registry:
            # If an existing entry points to evaluation_stubs, allow overwrite by clearing it.
            existing = function_registry.get(canonical) or special_registry.get(canonical)
            try:
                existing_py = existing.py_name if hasattr(existing, 'py_name') else (existing.get('py_name') if isinstance(existing, dict) else None)
                if existing_py:
                    import fclpy.lispfunc as _lispfunc_mod
                    existing_fn = getattr(_lispfunc_mod, existing_py, None)
                    if existing_fn is not None and getattr(existing_fn, '__module__', '').endswith('evaluation_stubs'):
                        # Remove the existing stub entry so this module can register a real impl
                        if canonical in function_registry:
                            del function_registry[canonical]
                        if canonical in special_registry:
                            del special_registry[canonical]
                    else:
                        # Existing implementation is not a stub; skip creating a duplicate.
                        continue
            except Exception:
                # On any error inspecting existing entry, conservatively skip.
                continue

        # If an explicit registration already exists for the hyphenated or underscored form, prefer it.
        if hyphenated in function_registry or hyphenated in special_registry:
            continue
        if lisp_name in function_registry or lisp_name in special_registry:
            # migrate underscored entry to hyphenated canonical key if necessary
            if '_' in lisp_name and hyphenated not in function_registry:
                function_registry[hyphenated] = function_registry.pop(lisp_name)
            continue

        # Register using the derived canonical name (prefer stripped base if different)
        entry = RegistryEntry(
            name=canonical,
            py_name=name,
            kind='function'
        )
        function_registry[canonical] = entry


def get_function_py_name(lisp_name: str):
    entry = function_registry.get(lisp_name)
    return entry.get("py_name") if entry else None


def get_special_py_name(lisp_name: str):
    entry = special_registry.get(lisp_name)
    return entry.get("py_name") if entry else None


def collect_function_symbols():
    """Collect mapping of Python function names to the Lisp names they implement.

    Returns a dict: {py_name: [lisp_name1, lisp_name2, ...]}
    This can be used by tests to detect if a single Python callable has been
    registered under multiple Lisp names (possible duplicate registrations).
    """
    mapping = {}
    for lisp_name, meta in function_registry.items():
        py = meta.get("py_name")
        if py is None:
            continue
        mapping.setdefault(py, []).append(lisp_name)

    for lisp_name, meta in special_registry.items():
        py = meta.get("py_name")
        if py is None:
            continue
        mapping.setdefault(py, []).append(lisp_name)

    return mapping
