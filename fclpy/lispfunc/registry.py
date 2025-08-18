"""Registry for builtin Common Lisp functions and special operators.

Provides decorators @cl_function and @cl_special to register Python callables
with metadata and utilities to register entire modules.
"""
from typing import Callable, Dict
import inspect
import fclpy.lisptype as lisptype

# Mapping: LISP_NAME (str) -> metadata dict (contains 'py_name' and any extras)
function_registry: Dict[str, Dict] = {}
special_registry: Dict[str, Dict] = {}


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
        function_registry[lisp_name] = {"py_name": func.__name__, **meta}
        return func
    return decorator


def cl_special(lisp_name: str, **meta):
    """Decorator to register a special operator (handled by evaluator).

    Example:
      @cl_special('IF')
      def special_if(...): ...
    """
    def decorator(func: Callable):
        special_registry[lisp_name] = {"py_name": func.__name__, **meta}
        return func
    return decorator


def register_module(module):
    """Inspect a module and register its public callables into the function registry.

    This registers only plain functions (not classes), using the Python name -> Lisp
    name conversion. Existing explicit decorator registrations are not overwritten.
    """
    for name, obj in vars(module).items():
        if name.startswith("_"):
            continue
        if not callable(obj):
            continue
        # Don't overwrite explicit decorator-based registrations
        lisp_name = _to_lisp_name(name)
        if lisp_name in function_registry or lisp_name in special_registry:
            continue
        # Skip modules/classes
        if inspect.isclass(obj) or inspect.ismodule(obj):
            continue
        function_registry[lisp_name] = {"py_name": name}


def get_function_py_name(lisp_name: str):
    entry = function_registry.get(lisp_name)
    return entry.get("py_name") if entry else None


def get_special_py_name(lisp_name: str):
    entry = special_registry.get(lisp_name)
    return entry.get("py_name") if entry else None
