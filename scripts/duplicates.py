#!/usr/bin/env python3
"""Report every operator implemented twice, and every Python name defined twice.

Why this exists
---------------
Standing rule 3 -- "when two implementations of one operator exist, delete
one" -- has produced the largest single wins in this project, repeatedly:

  * nine near-duplicate LOOP iteration engines (the last one parsed won)
  * three unrelated Python shapes for an array, plus five copies of the array
    operators, with import order deciding which ran
  * five copies of the relative-pathname search, two of which looked
    `*DEFAULT-PATHNAME-DEFAULTS*` up in *different packages*
  * three no-op logical-pathname stubs shadowing working implementations
  * three ordinary-lambda-list binders, only one of them correct
  * a second, dead hash-table implementation that still registers the
    operators of the live one

Every one of those was found by accident, weeks after it was introduced,
usually while chasing an unrelated failure. They are all mechanically
detectable, because `registry.cl_function` and friends do

    function_registry[lisp_name] = entry

-- last writer wins, silently. This script makes that visible.

It reports, and does not fix. A duplicate is not automatically a defect: a
module may deliberately re-register an operator to override a bootstrap
version. But every one should be *known*, and a new one appearing between two
runs of this script is the thing worth looking at.

Usage
-----
    pipenv run python scripts/duplicates.py             # report
    pipenv run python scripts/duplicates.py --baseline  # compare to the
                                                        # committed snapshot
    pipenv run python scripts/duplicates.py --save-baseline

Exit status is 1 when `--baseline` finds a duplicate that is not in the
snapshot, so it can be used as a gate.
"""

from __future__ import annotations

import argparse
import ast
import collections
import json
import os
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BASELINE = os.path.join(ROOT, 'docs', 'duplicates_baseline.json')
sys.path.insert(0, ROOT)


DECORATORS = {'cl_function': 'function', 'cl_special': 'special',
              'cl_macro': 'macro'}


def _decorator_name(node):
    """The bare name of a decorator call, `_registry.cl_function` included."""
    func = node.func if isinstance(node, ast.Call) else node
    if isinstance(func, ast.Attribute):
        return func.attr
    if isinstance(func, ast.Name):
        return func.id
    return None


def registry_duplicates():
    """Lisp operator names registered from more than one place.

    Read statically rather than by importing and watching the registry dicts,
    for two reasons. Registration is a decorator side effect that happens at
    *import* time, so by the time anything can wrap the dicts every write has
    already landed. And a module that is never imported still counts: the
    dead second hash-table implementation is exactly that shape -- it names
    the same operators, and whether it wins depends on import order, which is
    the property that makes these bugs so hard to see.
    """
    registrations = collections.defaultdict(list)
    package = os.path.join(ROOT, 'fclpy')
    for dirpath, dirnames, filenames in os.walk(package):
        dirnames[:] = [d for d in dirnames if d != '__pycache__']
        for filename in sorted(filenames):
            if not filename.endswith('.py'):
                continue
            path = os.path.join(dirpath, filename)
            tree = _parse(path)
            if tree is None:
                continue
            relative = os.path.relpath(path, ROOT)
            for node in ast.walk(tree):
                if not isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
                    continue
                for decorator in node.decorator_list:
                    kind = DECORATORS.get(_decorator_name(decorator))
                    if kind is None or not isinstance(decorator, ast.Call):
                        continue
                    if not decorator.args:
                        continue
                    first = decorator.args[0]
                    if not isinstance(first, ast.Constant) or not isinstance(first.value, str):
                        continue
                    registrations[(kind, first.value)].append(
                        (f"{relative}:{node.lineno}", node.name))

    return {key: sites for key, sites in registrations.items() if len(sites) > 1}


def _parse(path):
    """Parse a source file, or report and skip it.

    `utf-8-sig` because at least one module in this package carries a BOM,
    which `utf-8` reads as a leading U+FEFF and the parser rejects.
    """
    try:
        with open(path, encoding='utf-8-sig') as handle:
            return ast.parse(handle.read(), filename=path)
    except (SyntaxError, UnicodeDecodeError) as exc:
        print(f"  ! could not parse {path}: {exc}", file=sys.stderr)
        return None


def redefinition_duplicates():
    """Module-level names `def`ed or `class`ed twice in the same file.

    The second definition wins and the first is dead code that still reads as
    live -- `classes.py` has carried two `_init_builtin_classes` this way, and
    they disagree about the class hierarchy.
    """
    found = []
    package = os.path.join(ROOT, 'fclpy')
    for dirpath, dirnames, filenames in os.walk(package):
        dirnames[:] = [d for d in dirnames if d != '__pycache__']
        for filename in sorted(filenames):
            if not filename.endswith('.py'):
                continue
            path = os.path.join(dirpath, filename)
            tree = _parse(path)
            if tree is None:
                continue
            seen = {}
            for node in tree.body:
                if not isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef,
                                         ast.ClassDef)):
                    continue
                # A decorated definition may legitimately repeat a name only
                # via @overload/@property-setter, neither of which is used at
                # module level here.
                if node.name in seen:
                    found.append((os.path.relpath(path, ROOT), node.name,
                                  seen[node.name], node.lineno))
                seen[node.name] = node.lineno
    return found


def collect():
    registry_dups = registry_duplicates()
    redefinitions = redefinition_duplicates()
    return registry_dups, redefinitions


def key_set(registry_dups, redefinitions):
    keys = {f"{kind}:{name}" for kind, name in registry_dups}
    keys |= {f"redef:{path}:{name}" for path, name, _first, _second in redefinitions}
    return keys


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--baseline', action='store_true',
                        help='compare against docs/duplicates_baseline.json and '
                             'exit 1 if anything new appears')
    parser.add_argument('--save-baseline', action='store_true',
                        help='write the current set as the accepted snapshot')
    args = parser.parse_args()

    registry_dups, redefinitions = collect()

    print(f"Operators registered more than once: {len(registry_dups)}")
    for (kind, name), sites in sorted(registry_dups.items()):
        print(f"  {kind:8s} {name}")
        for location, py_name in sites:
            # Which one wins is decided by import order, not by anything
            # visible here -- that is the defect, so this deliberately does
            # not guess a winner.
            print(f"      {location}  {py_name}")

    print(f"\nModule-level names defined twice in one file: {len(redefinitions)}")
    for path, name, first, second in sorted(redefinitions):
        print(f"  {path}:{second}  {name}  (first definition at line {first}, dead)")

    current = key_set(registry_dups, redefinitions)

    if args.save_baseline:
        os.makedirs(os.path.dirname(BASELINE), exist_ok=True)
        with open(BASELINE, 'w', encoding='utf-8') as handle:
            json.dump(sorted(current), handle, indent=1)
        print(f"\nWrote {len(current)} accepted duplicates to {BASELINE}")
        return 0

    if args.baseline:
        if not os.path.exists(BASELINE):
            print(f"\nNo baseline at {BASELINE}; run --save-baseline first.")
            return 1
        with open(BASELINE, encoding='utf-8') as handle:
            accepted = set(json.load(handle))
        new = sorted(current - accepted)
        gone = sorted(accepted - current)
        for key in gone:
            print(f"\nRESOLVED  {key}")
        for key in new:
            print(f"\nNEW DUPLICATE  {key}")
        if new:
            print(f"\n{len(new)} new duplicate(s). Delete one implementation, or "
                  f"accept it with --save-baseline and say why in plan.md.")
            return 1
        print("\nNo new duplicates.")
    return 0


if __name__ == '__main__':
    sys.exit(main())
