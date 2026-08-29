#!/usr/bin/env python3
"""Serialize access to the shared checklist artifacts across concurrent agents.

Why this exists
----------------
`docs/ansi_checklist.md` and the files it is generated from
(`ansi_results/all.txt`, `passed.txt`, `failed.txt`, `merges.log`) are a
read-modify-write shared state with no locking of their own. plan.md already
warns about this for a single extra process ("never merge from two processes
at once") -- a multi-agent session with several teammates each running
`run_ansi.py --update-checklist` (or `gate.py`, which regenerates the
checklist as a side effect of its `ansi_checklist.py --baseline` check)
concurrently is exactly the failure mode that warning describes, except with
more than two writers instead of two.

This wraps one command in an exclusive lock so only one agent's
checklist-mutating command runs at a time. The others wait their turn instead
of interleaving reads and writes of the same files.

Usage
-----
    pipenv run python scripts/with_checklist_lock.py -- python scripts/run_ansi.py <target> --update-checklist
    pipenv run python scripts/with_checklist_lock.py -- python scripts/ansi_checklist.py --baseline docs/ansi_checklist_baseline.json
    pipenv run python scripts/with_checklist_lock.py -- python scripts/gate.py

Anything that only *reads* ansi_results/*.txt or docs/ansi_checklist.md and
does not call `ansi_checklist.py`'s render/merge path (plain `pytest -q`,
`duplicates.py --baseline`) does not need the lock.

Mechanism
---------
`os.makedirs(LOCK_DIR)` is the mutex: directory creation is atomic on both
POSIX and Windows, so exactly one caller ever succeeds when several race to
create the same path. Losers poll until it is removed or they time out. A
stale lock (the process that held it crashed / was killed) is broken after
`--stale-after` seconds so one dead agent cannot wedge the other four
forever -- print a warning when that happens rather than doing it silently,
since it means the interrupted command's writes may be incomplete.
"""

import argparse
import os
import subprocess
import sys
import time

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LOCK_DIR = os.path.join(REPO_ROOT, 'ansi_results', '.checklist.lock')


def acquire(timeout, stale_after, poll=1.0):
    start = time.time()
    warned_stale = False
    while True:
        try:
            os.makedirs(LOCK_DIR)
            return
        except FileExistsError:
            try:
                held_for = time.time() - os.path.getmtime(LOCK_DIR)
            except OSError:
                held_for = 0  # lock vanished between the makedirs failure and the stat
            if held_for > stale_after:
                if not warned_stale:
                    print("with_checklist_lock: breaking a stale lock held for "
                          "%.0fs (> --stale-after %.0fs) -- the process that held it "
                          "may have been interrupted; its checklist writes could be "
                          "incomplete" % (held_for, stale_after), file=sys.stderr)
                    warned_stale = True
                try:
                    os.rmdir(LOCK_DIR)
                except OSError:
                    pass
                continue
            if time.time() - start > timeout:
                raise SystemExit(
                    "error: could not acquire checklist lock (%s) within %ss -- "
                    "another agent is still updating the checklist" % (LOCK_DIR, timeout))
            time.sleep(poll)


def release():
    try:
        os.rmdir(LOCK_DIR)
    except OSError:
        pass


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                      formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--timeout', type=float, default=600,
                         help="give up waiting for the lock after this many seconds "
                              "(default 600 -- a single run_ansi.py target is usually "
                              "2-30s, so 600s means something is actually stuck)")
    parser.add_argument('--stale-after', type=float, default=1200,
                         help="treat the lock as abandoned and break it after this many "
                              "seconds held (default 1200, above --timeout so a live "
                              "holder is never preempted by an impatient waiter)")
    parser.add_argument('command', nargs=argparse.REMAINDER,
                         help="the command to run while holding the lock, after --")
    args = parser.parse_args()

    cmd = args.command
    if cmd and cmd[0] == '--':
        cmd = cmd[1:]
    if not cmd:
        parser.error("give a command to run, e.g. -- python scripts/run_ansi.py "
                      "iteration --update-checklist")

    os.makedirs(os.path.dirname(LOCK_DIR), exist_ok=True)
    acquire(args.timeout, args.stale_after)
    try:
        result = subprocess.run(cmd, cwd=REPO_ROOT)
        return result.returncode
    finally:
        release()


if __name__ == '__main__':
    sys.exit(main())
