#!/usr/bin/env python3
"""Generate the ANSI failure checklist -- the artifact that replaces re-running
the whole suite.

Why this exists
---------------
`ansi_results/failed.txt` is 13076 bare test *names*. It is not actionable: it
does not say which `.lsp` file a test lives in, so it cannot tell you what to
run to re-check one. A full suite run costs ~7.5 hours, so "just run it again"
is not an answer either.

This turns that flat list into a checklist grouped by **directory -> file**, with
the exact `run_ansi.py` command for each entry, so a fix can be verified against
the affected file in seconds instead of hours.

Reuse, not duplication
----------------------
The name -> source-file mapping already exists in `ansi_score.py`, which scans
every `.lsp` for `(deftest NAME ...)`. That module already computes per-file
counts too -- it simply never reports them. This script imports that mapping
rather than re-implementing the scan, because two copies of the mapping is
exactly the duplication failure mode plan.md's standing rule 3 exists to prevent:
they would drift, and a checklist that disagrees with the scoreboard is worse
than no checklist.

Keeping it current without a full run
-------------------------------------
`ansi_results/*.txt` is written by the full runner, so without help the
checklist could only be refreshed by a 4+ hour run -- which in practice means it
goes stale the moment anyone fixes anything, and a stale checklist is worse than
none (it is supposed to be *the authority for what is failing*).

`--merge` closes that gap. A targeted `run_ansi.py` run reports RT's own
`*passed-tests*`/`*failed-tests*` for exactly the files it loaded; merging that
back into `ansi_results/*.txt` updates the status of those tests and leaves
every other test untouched. The result is the last full run, amended with every
targeted run since -- which is precisely what a checklist needs to be.

What merging is *not*: it is not a new scoreboard. A targeted run can register a
slightly different test set than the full run does (files that generate tests at
load time, aux files loaded in a different order), so the merged totals are an
index, not an official measurement. Refresh `--save-baseline` only from a full
run.

Usage
-----
    python scripts/ansi_checklist.py                  # write docs/ansi_checklist.md
    python scripts/ansi_checklist.py --detail         # also list each failing test name
    python scripts/ansi_checklist.py --dir sequences  # restrict to one directory
    python scripts/ansi_checklist.py --baseline docs/ansi_checklist_baseline.json
                                                      # mark progress vs. a saved run
    python scripts/ansi_checklist.py --merge ansi_results/targeted-last.json
                                                      # fold in a targeted run, then regenerate
"""

import argparse
import json
import os
import sys
from collections import defaultdict
from datetime import datetime
from pathlib import Path

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# The single source of truth for name -> file. See module docstring.
from ansi_score import (  # noqa: E402
    ANSI_TEST_ROOT,
    REPO_ROOT,
    RESULTS_DIR,
    build_name_to_file_map,
    read_names,
    top_dir,
)

UNMAPPED = '(unmapped: programmatically generated)'
MERGE_LOG = 'merges.log'


def merge_targeted(results_path):
    """Fold one targeted run's outcomes into `ansi_results/*.txt`.

    Only names the targeted run actually accounted for are touched; a test the
    run did not load keeps whatever status the last full run gave it. Names the
    run reports that the full run never registered are appended to `all.txt`,
    since they demonstrably exist.

    Returns (moved_to_passed, moved_to_failed, newly_seen) counts.
    """
    with open(results_path, encoding='utf-8') as handle:
        results = json.load(handle)

    run_passed = [n.upper() for n in results.get('passed', [])]
    run_failed = [n.upper() for n in results.get('failed', [])]

    all_names = read_names('all.txt')
    passed = set(read_names('passed.txt'))
    failed = set(read_names('failed.txt'))

    moved_to_passed = sum(1 for n in run_passed if n in failed)
    moved_to_failed = sum(1 for n in run_failed if n in passed)

    for name in run_passed:
        failed.discard(name)
        passed.add(name)
    for name in run_failed:
        passed.discard(name)
        failed.add(name)

    known = set(all_names)
    newly_seen = [n for n in run_passed + run_failed if n not in known]
    all_names.extend(newly_seen)

    _write_names('all.txt', all_names)
    # Keep the ordering of all.txt so diffs between runs stay readable.
    order = {name: i for i, name in enumerate(all_names)}
    _write_names('passed.txt', sorted(passed, key=lambda n: order.get(n, len(order))))
    _write_names('failed.txt', sorted(failed, key=lambda n: order.get(n, len(order))))

    with open(RESULTS_DIR / MERGE_LOG, 'a', encoding='utf-8') as handle:
        handle.write('%s  targets=%s  passed=%d failed=%d  '
                     '(fixed %d, regressed %d, new %d)\n'
                     % (datetime.now().isoformat(timespec='seconds'),
                        ','.join(results.get('targets', [])) or '?',
                        len(run_passed), len(run_failed),
                        moved_to_passed, moved_to_failed, len(newly_seen)))

    return moved_to_passed, moved_to_failed, len(newly_seen)


def _write_names(filename, names):
    (RESULTS_DIR / filename).write_text('\n'.join(names) + '\n', encoding='utf-8')


def _recent_merges():
    """Targeted runs folded in since the last full run, most recent first."""
    path = RESULTS_DIR / MERGE_LOG
    if not path.exists():
        return []
    lines = [l.strip() for l in path.read_text(encoding='utf-8').splitlines() if l.strip()]
    return list(reversed(lines))


def collect(restrict_dir=None):
    """Join RT's result lists against the deftest source map.

    Returns (per_file, per_dir, unmapped_failures) where per_file maps a
    repo-relative .lsp path to its counts and the names of its failing tests.
    """
    all_names = read_names('all.txt')
    passed = set(read_names('passed.txt'))
    failed = set(read_names('failed.txt'))

    name_to_file, _ = build_name_to_file_map()

    per_file = defaultdict(lambda: {'total': 0, 'passed': 0, 'failed': 0, 'names': []})
    per_dir = defaultdict(lambda: {'total': 0, 'passed': 0, 'failed': 0, 'files': set()})
    unmapped_failures = []

    for name in all_names:
        relpath = name_to_file.get(name)
        is_fail = name in failed
        is_pass = name in passed

        if relpath is None:
            if is_fail:
                unmapped_failures.append(name)
            continue

        directory = top_dir(relpath)
        if restrict_dir and directory != restrict_dir:
            continue

        fbucket = per_file[relpath]
        fbucket['total'] += 1
        fbucket['passed'] += int(is_pass)
        fbucket['failed'] += int(is_fail)
        if is_fail:
            fbucket['names'].append(name)

        dbucket = per_dir[directory]
        dbucket['total'] += 1
        dbucket['passed'] += int(is_pass)
        dbucket['failed'] += int(is_fail)
        dbucket['files'].add(relpath)

    return per_file, per_dir, unmapped_failures


def render(per_file, per_dir, unmapped_failures, detail, baseline):
    """Render the checklist as markdown."""
    out = []
    w = out.append

    total_failed = sum(d['failed'] for d in per_dir.values()) + len(unmapped_failures)
    total_tests = sum(d['total'] for d in per_dir.values()) + len(unmapped_failures)
    files_with_failures = sum(1 for f in per_file.values() if f['failed'])

    w('# ANSI failure checklist')
    w('')
    w('Generated by `scripts/ansi_checklist.py` from `ansi_results/*.txt`.')
    w('**Regenerate after any run** (full or targeted) -- do not hand-edit counts.')
    w('')
    merges = _recent_merges()
    if merges:
        w('> These counts are the last **full** run amended by %d targeted run(s)'
          % len(merges))
        w('> (`run_ansi.py <target> --update-checklist`). Most recent first:')
        w('>')
        for line in merges[:5]:
            w('> - `%s`' % line)
        w('>')
        w('> Amended counts are an *index*, not an official scoreboard: a targeted run')
        w('> can register a slightly different test set than the full run does. Move')
        w('> the official number, and `--save-baseline`, only from a full run.')
        w('')
    w('| | |')
    w('|---|---|')
    w('| Failing tests | **%d** |' % total_failed)
    w('| Files containing failures | **%d** |' % files_with_failures)
    w('| Failures not attributable to a file | %d |' % len(unmapped_failures))
    w('')
    w('## How to use this')
    w('')
    w('**Do not work this list top-to-bottom.** It is ordered by failure count so')
    w('that *clusters* are visible; a file with 90 failures is nearly always one')
    w('missing mechanism, not 90 bugs. Pick a cluster, find the mechanism, fix it,')
    w('then re-run just that file or directory:')
    w('')
    w('```powershell')
    w('pipenv run python scripts/run_ansi.py <group>            # whole directory')
    w('pipenv run python scripts/run_ansi.py <group>/<file>.lsp # single file')
    w('pipenv run python scripts/ansi_checklist.py              # regenerate this')
    w('```')
    w('')
    w('A full-suite run takes ~7.5 hours and is **not** required to verify a fix.')
    w('Run it to move the official scoreboard or close a milestone.')
    w('')

    w('## Directories')
    w('')
    w('| dir | failed | total | pass rate | command |')
    w('|---|---|---|---|---|')
    for d in sorted(per_dir, key=lambda k: -per_dir[k]['failed']):
        row = per_dir[d]
        rate = 100.0 * row['passed'] / row['total'] if row['total'] else 0.0
        w('| **%s** | %d | %d | %.1f%% | `run_ansi.py %s` |'
          % (d, row['failed'], row['total'], rate, d))
    w('')

    for d in sorted(per_dir, key=lambda k: -per_dir[k]['failed']):
        row = per_dir[d]
        if not row['failed']:
            continue
        w('## %s — %d failing of %d' % (d, row['failed'], row['total']))
        w('')
        files = sorted(
            (f for f in row['files'] if per_file[f]['failed']),
            key=lambda f: -per_file[f]['failed'])
        for relpath in files:
            fb = per_file[relpath]
            posix = relpath.replace('\\', '/')
            marker = ''
            if baseline is not None:
                was = baseline.get(posix)
                if was is not None:
                    delta = fb['failed'] - was
                    if delta < 0:
                        marker = '  **(-%d since baseline)**' % -delta
                    elif delta > 0:
                        marker = '  **(+%d REGRESSION)**' % delta
                elif fb['failed']:
                    marker = '  *(new)*'
            w('- [ ] `%s` — **%d** failing of %d%s' % (posix, fb['failed'], fb['total'], marker))
            w('      `pipenv run python scripts/run_ansi.py %s`' % posix)
            if detail:
                for name in sorted(fb['names']):
                    w('      - %s' % name)
        w('')

    if unmapped_failures:
        w('## Not statically attributable to a source file — %d' % len(unmapped_failures))
        w('')
        w('These names have no literal `(deftest ...)` form: they are generated at')
        w('load time by macros, e.g. `cons/cxr.lsp` builds 40 of its tests with')
        w('``(eval `(deftest ,(intern ...) ...))``.')
        w('')
        w('**They are still reachable by targeted runs.** Loading the file whose')
        w('macros generate them registers and runs them exactly as the full suite')
        w('does -- only this checklist\'s *static attribution* misses them, not the')
        w('runner. A consequence worth knowing: `run_ansi.py <file>` normally')
        w('reports **more** registered tests than this checklist attributes to that')
        w('file (cxr.lsp: 176 registered vs 136 attributed here). **The targeted')
        w('run is the authority for a file; this checklist is an index, not a')
        w('count.**')
        w('')
        if detail:
            for name in sorted(unmapped_failures):
                w('- %s' % name)
        else:
            w('Run with `--detail` to list them.')
        w('')

    return '\n'.join(out) + '\n'


def main():
    parser = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    parser.add_argument('--detail', action='store_true',
                        help='list every failing test name under its file')
    parser.add_argument('--dir', dest='restrict_dir',
                        help='restrict the checklist to one directory')
    parser.add_argument('--out', default=None,
                        help='output path (default docs/ansi_checklist.md)')
    parser.add_argument('--baseline',
                        help='JSON of {file: failed_count} to mark progress against')
    parser.add_argument('--save-baseline',
                        help='write the current {file: failed_count} map to this path')
    parser.add_argument('--merge', action='append', default=[], metavar='RESULTS.JSON',
                        help='fold a targeted run\'s results (run_ansi.py --results-out) '
                             'into ansi_results/*.txt before regenerating; repeatable')
    args = parser.parse_args()

    for results_path in args.merge:
        fixed, regressed, new = merge_targeted(results_path)
        print('Merged %s: %d newly passing, %d newly failing, %d not previously registered'
              % (results_path, fixed, regressed, new))
        if regressed:
            print('  WARNING: %d test(s) moved from passed to failed -- investigate before '
                  'treating this as progress' % regressed)

    main_render(restrict_dir=args.restrict_dir, detail=args.detail,
                baseline_path=args.baseline, out=args.out,
                save_baseline=args.save_baseline)


def main_render(restrict_dir=None, detail=False, baseline_path=None,
                out=None, save_baseline=None):
    """Regenerate docs/ansi_checklist.md from the current ansi_results/*.txt.

    Separated from main() so run_ansi.py --update-checklist can regenerate the
    checklist directly instead of shelling out to a second process (or, worse,
    growing its own copy of the renderer).
    """
    per_file, per_dir, unmapped_failures = collect(restrict_dir)

    baseline = None
    if baseline_path:
        with open(baseline_path) as handle:
            baseline = json.load(handle)

    text = render(per_file, per_dir, unmapped_failures, detail, baseline)

    out_path = Path(out) if out else REPO_ROOT / 'docs' / 'ansi_checklist.md'
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(text, encoding='utf-8')

    if save_baseline:
        snapshot = {f.replace('\\', '/'): v['failed']
                    for f, v in per_file.items() if v['failed']}
        with open(save_baseline, 'w') as handle:
            json.dump(snapshot, handle, indent=2, sort_keys=True)
        print('Baseline written to %s' % save_baseline)

    total_failed = sum(d['failed'] for d in per_dir.values()) + len(unmapped_failures)
    files_with = sum(1 for f in per_file.values() if f['failed'])
    print('Checklist written to %s' % out_path)
    print('  %d failing tests across %d files (+%d unattributable)'
          % (total_failed, files_with, len(unmapped_failures)))

    print('\nWorst files:')
    worst = sorted(per_file.items(), key=lambda kv: -kv[1]['failed'])[:15]
    for relpath, row in worst:
        print('  %-52s %5d failing of %5d'
              % (relpath.replace('\\', '/'), row['failed'], row['total']))


if __name__ == '__main__':
    main()
