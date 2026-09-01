#!/usr/bin/env python3
"""Run the cheap checks together and exit non-zero if any of them fails.

    pipenv run python scripts/gate.py

Three checks already existed and nothing composed them, so passing all three
depended on remembering all three:

  * `pytest -q`                            -- unit regressions (~50s)
  * `scripts/duplicates.py --baseline`     -- standing rule 3 (<1s)
  * `scripts/ansi_checklist.py --baseline` -- per-file ANSI regressions (<1s)

This is deliberately **not** a full ANSI run. A full run is ~86 minutes and is
the scoreboard, not the loop (CLAUDE.md, "The development loop"). This gate is
what you run after every repair; the full run is what you run to move the
official number, and mandatorily after touching a bootstrap operator.

What this gate cannot tell you
------------------------------
It compares against *baselines*, and a baseline can be refreshed. Refreshing
one to make a failure go away is the evasion this project's rules exist to
prevent -- see plan.md, "Ways to fake compliance". In particular:

  * `duplicates.py --save-baseline` after adding a second implementation of an
    operator silences a genuine standing-rule-3 violation.
  * `ansi_checklist.py --save-baseline` after a regression records the
    regression as the new normal, and is full-run-only for that reason.

Neither is detectable from inside this script. Both are visible in `git diff`
of `docs/duplicates_baseline.json` / `docs/ansi_checklist_baseline.json`, which
is why both files are committed: **a baseline change is a reviewable event, not
a fix.**
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
import time

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

CHECKLIST = os.path.join('docs', 'ansi_checklist.md')

CHECKS = [
    ('pytest', [sys.executable, '-m', 'pytest', '-q'],
     'unit regressions'),
    ('duplicates', [sys.executable, os.path.join('scripts', 'duplicates.py'), '--baseline'],
     'standing rule 3: no new duplicate implementation'),
    ('ansi-checklist', [sys.executable, os.path.join('scripts', 'ansi_checklist.py'),
                        '--baseline', os.path.join('docs', 'ansi_checklist_baseline.json')],
     'no per-file ANSI regression'),
]


def checklist_regressions():
    """The `(+N REGRESSION)` lines `ansi_checklist.py --baseline` just wrote.

    It marks them **in the generated checklist**, not on stdout, and exits 0
    either way -- so a gate that only inspected the command's output and exit
    status passed unconditionally. That is precisely the false-green this
    project's rules exist to prevent, so the check reads the artifact.
    """
    path = os.path.join(ROOT, CHECKLIST)
    if not os.path.exists(path):
        return ['(no %s was generated -- the check did not run)' % CHECKLIST]
    with open(path, encoding='utf-8') as handle:
        return [line.strip() for line in handle if 'REGRESSION' in line]


def run(name, command, description, verbose):
    print(f"=== {name}: {description}")
    started = time.time()
    result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True)
    elapsed = time.time() - started
    output = (result.stdout or '') + (result.stderr or '')

    failed = result.returncode != 0
    if failed or verbose:
        print(output.rstrip())

    if name == 'ansi-checklist':
        # plan.md is explicit that a regression in a file you did not touch is
        # a failure *even when the total improved*, because a total can hide a
        # mechanism trade. So this is checked per file, from the artifact.
        regressions = checklist_regressions()
        if regressions:
            failed = True
            print(f"  ! {len(regressions)} file(s) worse than the baseline:")
            for line in regressions[:20]:
                print(f"      {line}")
            if len(regressions) > 20:
                print(f"      ... and {len(regressions) - 20} more; see {CHECKLIST}")

    print(f"--- {name}: {'FAIL' if failed else 'ok'} ({elapsed:.1f}s)\n")
    return not failed


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('-v', '--verbose', action='store_true',
                        help='print each check\'s output even when it passes')
    parser.add_argument('--skip-pytest', action='store_true',
                        help='run only the two sub-second checks')
    args = parser.parse_args()

    checks = [c for c in CHECKS if not (args.skip_pytest and c[0] == 'pytest')]
    results = [(name, run(name, command, description, args.verbose))
               for name, command, description in checks]

    failed = [name for name, ok in results if not ok]
    if failed:
        print(f"GATE: FAIL ({', '.join(failed)})")
        print("Do not refresh a baseline to clear this. See plan.md, "
              "\"Ways to fake compliance\".")
        return 1
    print("GATE: ok")
    print("This is the cheap gate, not the scoreboard. A full "
          "`run_all_tests.py` is still required to move the official number, "
          "and is mandatory after touching an ansi-test bootstrap operator "
          "(CLAUDE.md, \"The one thing a targeted run cannot verify\").")
    return 0


if __name__ == '__main__':
    sys.exit(main())
