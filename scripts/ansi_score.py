#!/usr/bin/env python3
"""Per-subsystem ANSI conformance scoreboard (plan.md M0 step 6).

Reads the raw test-name dumps `run_all_tests.py` writes to `ansi_results/`
(`all.txt`, `passed.txt`, `failed.txt` — one RT test name per line, produced
directly from RT's own `*entries*`/`*passed-tests*`/`*failed-tests*` lists, not
parsed from FORMAT-rendered log text) and cross-references each name against
the `.lsp` file that defines it via `(deftest <name> ...)`, so failures can be
attributed to a subsystem (`ansi-test/<directory>/`) instead of read one at a
time out of a 22000-line log.

Also flags two integrity problems that a plain pass/fail count would hide:
  - registered names with no discoverable `deftest` source (renamed/generated
    tests, or the name map's regex missing an unusual form)
  - `deftest` names found in source but never registered (the file failed to
    load, or a later duplicate `deftest` with the same name overwrote it —
    see plan.md Finding on the ~2 duplicate registrations)
"""
import json
import os
import re
import sys
from collections import defaultdict
from pathlib import Path

REPO_ROOT = Path(__file__).parent.parent
ANSI_TEST_ROOT = REPO_ROOT.parent / 'ansi-test'
RESULTS_DIR = REPO_ROOT / 'ansi_results'

DEFTEST_RE = re.compile(r'^\s*\(deftest\s+([^\s()]+)', re.IGNORECASE | re.MULTILINE)


def read_names(filename):
    path = RESULTS_DIR / filename
    if not path.exists():
        print('error: %s not found — run run_all_tests.py first' % path, file=sys.stderr)
        sys.exit(1)
    with open(path) as f:
        return [line.strip() for line in f if line.strip()]


def build_name_to_file_map():
    """Scan every .lsp file under ansi-test/ for `(deftest NAME ...)` forms.

    Returns (name_to_relpath, file_to_source_names) where name is upper-cased
    (RT symbol names are read upper-case by the standard readtable).
    """
    name_to_file = {}
    file_to_names = defaultdict(list)
    for path in ANSI_TEST_ROOT.rglob('*.lsp'):
        if '.git' in path.parts:
            continue
        try:
            text = path.read_text(encoding='utf-8', errors='replace')
        except OSError:
            continue
        relpath = str(path.relative_to(ANSI_TEST_ROOT))
        for m in DEFTEST_RE.finditer(text):
            name = m.group(1).upper()
            name_to_file[name] = relpath
            file_to_names[relpath].append(name)
    return name_to_file, file_to_names


def top_dir(relpath):
    parts = Path(relpath).parts
    return parts[0] if len(parts) > 1 else '(root)'


def main():
    all_names = read_names('all.txt')
    passed_names = set(read_names('passed.txt'))
    failed_names = set(read_names('failed.txt'))

    name_to_file, file_to_source_names = build_name_to_file_map()

    per_dir = defaultdict(lambda: {'total': 0, 'passed': 0, 'failed': 0})
    per_file = defaultdict(lambda: {'total': 0, 'passed': 0, 'failed': 0})
    unmapped = []

    for name in all_names:
        relpath = name_to_file.get(name)
        is_pass = name in passed_names
        is_fail = name in failed_names
        if relpath is None:
            unmapped.append(name)
            bucket = per_dir['(unmapped: programmatically generated)']
            bucket['total'] += 1
            bucket['passed'] += int(is_pass)
            bucket['failed'] += int(is_fail)
            continue
        d = top_dir(relpath)
        for bucket in (per_dir[d], per_file[relpath]):
            bucket['total'] += 1
            bucket['passed'] += int(is_pass)
            bucket['failed'] += int(is_fail)

    all_names_set = set(all_names)
    unregistered_source_names = sorted(
        n for n in name_to_file if n not in all_names_set)

    print('%-30s %8s %8s %8s %8s' % ('DIRECTORY', 'TOTAL', 'PASSED', 'FAILED', 'FAIL%'))
    print('-' * 66)
    for d in sorted(per_dir, key=lambda k: -per_dir[k]['failed']):
        row = per_dir[d]
        pct = 100.0 * row['failed'] / row['total'] if row['total'] else 0.0
        print('%-30s %8d %8d %8d %7.1f%%' % (d, row['total'], row['passed'], row['failed'], pct))
    print('-' * 66)
    print('%-30s %8d %8d %8d' % ('TOTAL', len(all_names), len(passed_names), len(failed_names)))

    if unmapped:
        print('\n%d registered test name(s) with no discoverable deftest source:' % len(unmapped))
        for n in unmapped[:50]:
            print('  UNMAPPED: %s' % n)
        if len(unmapped) > 50:
            print('  ... and %d more' % (len(unmapped) - 50))

    if unregistered_source_names:
        print('\n%d deftest name(s) found in source but never registered (load failure or shadowed duplicate):'
              % len(unregistered_source_names))
        for n in unregistered_source_names[:50]:
            print('  UNREGISTERED: %s (%s)' % (n, name_to_file[n]))
        if len(unregistered_source_names) > 50:
            print('  ... and %d more' % (len(unregistered_source_names) - 50))

    baseline = {
        'total': len(all_names),
        'passed': len(passed_names),
        'failed': len(failed_names),
        'per_directory': {d: per_dir[d] for d in per_dir},
        'unmapped_count': len(unmapped),
        'unregistered_source_count': len(unregistered_source_names),
    }
    docs_dir = REPO_ROOT / 'docs'
    docs_dir.mkdir(exist_ok=True)
    with open(docs_dir / 'ansi_baseline.json', 'w') as f:
        json.dump(baseline, f, indent=2, sort_keys=True)
    print('\nBaseline snapshot written to docs/ansi_baseline.json')


if __name__ == '__main__':
    main()
