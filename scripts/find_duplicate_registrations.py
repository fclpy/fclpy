#!/usr/bin/env python3
"""Find duplicate Lisp function registrations in fclpy.lispfunc modules.

This script imports each module in `fclpy.lispfunc`, asks the registry to
collect registrations, and then reports any Python callables that are
registered under more than one Lisp name (possible duplicates).
"""
import pkgutil
import importlib
import sys
from pathlib import Path
from collections import defaultdict

# Ensure package root is on sys.path so imports work when run as a script
ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))

import fclpy.lispfunc as lispfunc_pkg
from fclpy.lispfunc import registry


def import_all_lispfunc_modules():
    pkg_name = lispfunc_pkg.__name__
    for finder, name, ispkg in pkgutil.iter_modules(lispfunc_pkg.__path__, pkg_name + '.'):
        try:
            mod = importlib.import_module(name)
            # Let registry.register_module pick up any callables not decorated
            try:
                registry.register_module(mod)
            except Exception:
                pass
        except Exception as e:
            print(f"Failed to import {name}: {e}")


def find_duplicates():
    by_py = defaultdict(list)  # py_name -> [lisp_names]
    for lisp_name, meta in registry.function_registry.items():
        py = meta.get('py_name')
        by_py[py].append(lisp_name)

    duplicates = {py: names for py, names in by_py.items() if len(names) > 1}
    return duplicates


def main():
    import_all_lispfunc_modules()
    duplicates = find_duplicates()
    if not duplicates:
        print('No duplicate function registrations found')
        return 0

    print('Duplicate registrations found:')
    for py, lisp_names in duplicates.items():
        print(f'  {py}: {", ".join(lisp_names)}')
    return 1


if __name__ == '__main__':
    raise SystemExit(main())
