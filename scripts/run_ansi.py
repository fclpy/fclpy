#!/usr/bin/env python3
"""Run a *subset* of the ANSI test suite -- one file, one directory, or a few of each.

Why this exists
---------------
`run_all_tests.py` loads `doit.lsp`, which loads every test file in the suite and
runs all 22036 tests. That run now takes **over four hours**, which makes it
useless as a development loop: you cannot iterate on a fix if verifying it costs
half a day. The full suite is still the authority for the scoreboard, but it is
the wrong tool for "did my change fix DEPOSIT-FIELD.1 without breaking SORT?".

The ansi-test harness splits cleanly at exactly the seam this needs:

    gclload1.lsp  -- the harness only: RT, the CL-TEST package, ansi-aux
                     macros/functions, universe.lsp, random-aux, cl-symbol-names
    gclload2.lsp  -- (load "<dir>/load.lsp") for each of the ~25 test directories

So loading `gclload1.lsp` plus *only the files you care about* registers only
those tests with RT, and `(do-tests)` then runs only those. That is the whole
mechanism -- no test selection logic, no filtering, no second harness. RT's own
bookkeeping (`*passed-tests*` / `*failed-tests*`) stays the source of truth,
exactly as in `run_all_tests.py`, so results are directly comparable.

Usage
-----
    # one directory (a "group")
    python scripts/run_ansi.py packages
    # one file
    python scripts/run_ansi.py numbers/deposit-field.lsp
    # several targets at once
    python scripts/run_ansi.py cons/nconc.lsp cons/append.lsp iteration
    # list what directories are available
    python scripts/run_ansi.py --list

A directory argument is resolved to its own `load.lsp`, which is what
`gclload2.lsp` itself loads, so intra-directory file ordering and any
directory-local auxiliary file are handled by the suite's own manifest rather
than guessed at here.

Timeouts
--------
The implementation still has LOOP forms that never terminate (a `for var = expr`
driver whose only bound is `repeat n`), and LOOP's hard cap only guards the
simple-loop path, so a driver-path runaway hangs forever. `--timeout` installs a
process-level watchdog thread that reports what was running and hard-exits, so
an unattended targeted run cannot wedge. `--loop-cap` additionally lowers LOOP's
own in-evaluator cap so a runaway is charged to the individual test as a failure
instead of costing the default 600 seconds.
"""

import argparse
import os
import sys
import threading
import time

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ANSI_ROOT = os.path.abspath(os.path.join(REPO_ROOT, '..', 'ansi-test'))

sys.path.insert(0, REPO_ROOT)


def _lisp_list_to_str_list(value):
    """Convert a Lisp list returned by eval_string into a Python list of str.

    Mirrors run_all_tests.py: NIL/empty comes back as something that is not a
    lispCons and is therefore not iterable; every non-empty proper list is a
    lispCons chain that yields each car.
    """
    try:
        return [str(item) for item in value]
    except TypeError:
        return []


def resolve_target(target):
    """Map a user-supplied target to the .lsp file that registers its tests.

    A directory becomes its own load.lsp -- the same manifest gclload2.lsp uses
    -- so we inherit the suite's declared file ordering instead of inventing one.
    """
    candidate = os.path.join(ANSI_ROOT, target)

    if os.path.isdir(candidate):
        load_lsp = os.path.join(candidate, 'load.lsp')
        if not os.path.exists(load_lsp):
            raise SystemExit(
                "error: directory %r has no load.lsp; name the .lsp files directly"
                % target)
        return load_lsp

    if not candidate.endswith('.lsp'):
        candidate += '.lsp'
    if not os.path.exists(candidate):
        raise SystemExit("error: no such test file or directory: %s" % target)
    return candidate


def aux_preamble_forms(target):
    """Return the auxiliary-loading forms a single test file needs.

    Each test directory's load.lsp opens with a preamble of
    `(compile-and-load* "<something>-aux.lsp")` forms that define the helpers
    its test files call -- `packages/do-symbols.lsp`, for instance, calls
    `set-up-packages`, which lives in `auxiliary/packages00-aux.lsp` and is
    pulled in by `packages/load.lsp`. Loading a lone test file without that
    preamble produces failures that are artifacts of the harness rather than
    real defects, which is worse than useless for a checklist.

    Only the preamble is taken -- the `(load "...")` lines that follow are the
    directory's other test files, and pulling those in would defeat the point of
    targeting one file.
    """
    directory = os.path.dirname(target)
    load_lsp = os.path.join(directory, 'load.lsp')
    if not os.path.exists(load_lsp) or os.path.basename(target) == 'load.lsp':
        return []

    forms = []
    with open(load_lsp, 'r', errors='replace') as handle:
        for line in handle:
            stripped = line.strip()
            if stripped.startswith('(compile-and-load'):
                forms.append(stripped)
            elif stripped.startswith('(load '):
                # Reached the test-file manifest; the preamble is over.
                break
    return forms


def list_directories():
    """Print the test directories that can be used as a group argument."""
    names = []
    for entry in sorted(os.listdir(ANSI_ROOT)):
        full = os.path.join(ANSI_ROOT, entry)
        if os.path.isdir(full) and os.path.exists(os.path.join(full, 'load.lsp')):
            count = len([f for f in os.listdir(full) if f.endswith('.lsp')])
            names.append((entry, count))
    print("test groups (directory -> .lsp file count):")
    for name, count in names:
        print("  %-28s %4d files" % (name, count))
    print("\nrun one with:  python scripts/run_ansi.py <group>")


def start_watchdog(seconds, state):
    """Hard-exit the process if a targeted run overruns.

    A daemon thread rather than signal.alarm: SIGALRM does not exist on Windows.
    os._exit is deliberate -- a runaway is stuck inside the evaluator, so a
    normal exception would just be caught by RT and the process would keep
    spinning.
    """
    if seconds <= 0:
        return

    def _watch():
        time.sleep(seconds)
        print("\n*** TIMEOUT after %ds -- last phase: %s ***"
              % (seconds, state.get('phase', 'unknown')), file=sys.stderr)
        print("*** the run did not finish; a LOOP driver runaway is the usual cause ***",
              file=sys.stderr)
        sys.stderr.flush()
        sys.stdout.flush()
        os._exit(2)

    threading.Thread(target=_watch, daemon=True).start()


def main():
    parser = argparse.ArgumentParser(
        description="Run a subset of the ANSI test suite (one file or directory).")
    parser.add_argument('targets', nargs='*',
                        help="test files (numbers/sqrt.lsp) or directories (packages)")
    parser.add_argument('--list', action='store_true',
                        help="list the available test group directories and exit")
    parser.add_argument('--timeout', type=int, default=900,
                        help="process-level watchdog in seconds (0 disables; default 900)")
    parser.add_argument('--loop-cap', type=int, default=0,
                        help="override LOOP's in-evaluator hard cap in seconds "
                             "(0 keeps the default 600)")
    parser.add_argument('--quiet', action='store_true',
                        help="suppress the suite's own per-test chatter")
    args = parser.parse_args()

    if args.list:
        list_directories()
        return 0
    if not args.targets:
        parser.error("give at least one test file or directory, or --list")

    if not os.path.isdir(ANSI_ROOT):
        raise SystemExit("error: ansi-test checkout not found at %s" % ANSI_ROOT)

    targets = [resolve_target(t) for t in args.targets]

    state = {'phase': 'startup'}
    start_watchdog(args.timeout, state)

    # Imported after the watchdog is armed so that a hang during environment
    # setup is also caught.
    from fclpy import runtime
    from fclpy.lispfunc import setup_environment, eval_string

    if args.loop_cap > 0:
        import fclpy.lispfunc.evaluation_loops_conditionals as elc
        elc.LOOP_TIMEOUT_ERROR = args.loop_cap

    env = setup_environment()

    # The suite loads its own files with relative pathnames, resolved against
    # *default-pathname-defaults*; running from the ansi-test root is what the
    # full-suite path effectively does via doit.lsp's *load-truename* binding.
    original_cwd = os.getcwd()
    os.chdir(ANSI_ROOT)
    try:
        state['phase'] = 'loading harness (gclload1.lsp)'
        print("Loading harness: gclload1.lsp")
        runtime.load_and_evaluate_file(
            os.path.join(ANSI_ROOT, 'gclload1.lsp'), env, verbose=False)

        for target in targets:
            rel = os.path.relpath(target, ANSI_ROOT).replace('\\', '/')
            for form in aux_preamble_forms(target):
                print("Loading aux:    %s" % form)
                eval_string(form, env)
            state['phase'] = 'loading %s' % rel
            print("Loading tests:  %s" % rel)
            runtime.load_and_evaluate_file(target, env, verbose=False)

        registered = _lisp_list_to_str_list(eval_string(
            "(mapcar (lambda (e) (string (regression-test::name e))) "
            "(cdr regression-test::*entries*))", env))
        print("Registered %d tests" % len(registered))

        state['phase'] = 'running tests'
        started = time.perf_counter()
        eval_string("(regression-test:do-tests)", env)
        elapsed = time.perf_counter() - started
    finally:
        os.chdir(original_cwd)

    state['phase'] = 'reporting'
    passed = _lisp_list_to_str_list(eval_string(
        "(mapcar #'string regression-test:*passed-tests*)", env))
    failed = _lisp_list_to_str_list(eval_string(
        "(mapcar #'string regression-test:*failed-tests*)", env))

    accounted = set(passed) | set(failed)
    missing = [n for n in registered if n not in accounted]

    print("\n" + "=" * 62)
    print("TARGETS : %s" % ", ".join(args.targets))
    print("RESULT  : passed=%d failed=%d registered=%d unaccounted=%d  (%.1fs)"
          % (len(passed), len(failed), len(registered), len(missing), elapsed))
    if registered:
        print("RATE    : %.1f%% passing" % (100.0 * len(passed) / len(registered)))

    if missing:
        # Same completeness check the full runner makes: a test that is
        # registered but neither passed nor failed means the run aborted
        # partway, which would otherwise read as a clean result.
        print("\nUNACCOUNTED (run aborted before these ran): %d" % len(missing))
        for name in missing[:20]:
            print("  %s" % name)
        if len(missing) > 20:
            print("  ... and %d more" % (len(missing) - 20))

    if failed:
        print("\nFAILED (%d):" % len(failed))
        for name in sorted(failed):
            print("  %s" % name)

    return 1 if (failed or missing) else 0


if __name__ == '__main__':
    sys.exit(main())
