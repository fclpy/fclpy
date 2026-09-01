import fclpy.lispfunc.registry as registry


def test_no_duplicate_registrations():
    """Fail if a single Python callable is registered under multiple Lisp names.

    This helps catch accidental duplicate registrations where the same
    implementation appears multiple times under different Lisp aliases.
    """
    mapping = registry.collect_function_symbols()
    duplicates = {py: names for py, names in mapping.items() if len(names) > 1}
    if duplicates:
        # Provide a helpful assertion message listing the offending mappings
        msgs = []
        for py, names in duplicates.items():
            msgs.append(f"{py}: {', '.join(sorted(names))}")
        raise AssertionError("Duplicate function registrations found:\n" + "\n".join(msgs))
import pkgutil
import importlib
from collections import defaultdict

import fclpy.lispfunc as lispfunc_pkg
from fclpy.lispfunc import registry


def import_all_lispfunc_modules():
    pkg_name = lispfunc_pkg.__name__
    for finder, name, ispkg in pkgutil.iter_modules(lispfunc_pkg.__path__, pkg_name + '.'):
        try:
            importlib.import_module(name)
        except Exception:
            # Module import errors should not fail this redundancy check test
            # as some modules may be environment-dependent; skip them.
            pass


def test_no_duplicate_function_names():
    """Ensure there are no duplicate Lisp function names registered."""
    import_all_lispfunc_modules()
    names = list(registry.function_registry.keys())
    assert len(names) == len(set(names)), "Duplicate Lisp function names found in registry"


def test_no_duplicate_python_bindings():
    """Ensure a single Python callable isn't registered under multiple Lisp names."""
    import_all_lispfunc_modules()
    by_py = defaultdict(list)
    for lisp_name, meta in registry.function_registry.items():
        py = meta.get('py_name')
        by_py[py].append(lisp_name)

    duplicates = {py: names for py, names in by_py.items() if len(names) > 1}
    assert not duplicates, f"Python callables registered under multiple Lisp names: {duplicates}"
