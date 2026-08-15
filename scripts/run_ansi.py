#!/usr/bin/env python3
"""Run a *subset* of the ANSI test suite -- one file, one directory, or a few of each.

Why this exists
---------------
`run_all_tests.py` loads `doit.lsp`, which loads every test file in the suite and
runs all 22113 tests. That run takes **~113 minutes** (measured 2026-08-16),
which makes it a poor development loop: you cannot iterate on a fix if verifying
it costs two hours. The full suite is still the authority for the scoreboard, but it is
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
    # run a target AND amend docs/ansi_checklist.md with the outcome
    python scripts/run_ansi.py iteration --update-checklist

A directory argument is resolved to its own `load.lsp`, which is what
`gclload2.lsp` itself loads, so intra-directory file ordering and any
directory-local auxiliary file are handled by the suite's own manifest rather
than guessed at here.

Keeping the checklist current
----------------------------
`docs/ansi_checklist.md` is generated from `ansi_results/*.txt`, which only the
full runner writes -- so without help it could only ever be refreshed by a
~2 hour run. Every run here writes its outcomes to `ansi_results/targeted-last.json`,
and `--update-checklist` folds them straight back in, updating the status of
exactly the tests that ran. See plan.md, "Keeping the checklist current without a
full run".

Timeouts
--------
LOOP's in-evaluator hard cap only catches a loop that is *going around* too many
times: it is evaluated in `LoopWatchdog.tick()`, once per iteration. A loop
wedged *inside* one iteration never reaches it, which is how MAKE-LIST.ERROR.1
held a run at 27GB for half an hour without producing a single diagnostic.

`--timeout` therefore installs `fclpy.watchdog`, a process-level detector of
*time without progress* (default 900s here, 600s for the full runner), which
warns at 120s and hard-exits at the timeout, dumping every thread's traceback
both times so a wedged run says where it is stuck. `--loop-cap` still lowers
LOOP's own in-evaluator cap, which remains the cheaper way to charge a runaway
*iteration count* to the individual test as a failure.
"""

import argparse
import json
import os
import sys
import threading
import time

REPO_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ANSI_ROOT = os.path.abspath(os.path.join(REPO_ROOT, '..', 'ansi-test'))

sys.path.insert(0, REPO_ROOT)

from fclpy import watchdog


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

    Ancestor directories are searched too, outermost first. Some test
    directories nest: `printer/format/` has its own load.lsp listing the
    `format-*.lsp` files, but the `compile-and-load*` of `printer-aux.lsp`
    that defines the `def-format-test` macro they all use lives one level up
    in `printer/load.lsp`. Looking only at the immediate directory meant no
    file under `printer/format/` could be targeted at all -- every one of
    them failed to load with `Undefined function DEF-FORMAT-TEST`, i.e. the
    exact harness artifact this function exists to prevent.
    """
    if os.path.basename(target) == 'load.lsp':
        return []

    # Walk from the target's directory up to the suite root, then apply the
    # preambles outermost first so a helper defined in a parent directory is
    # available to the aux files of its children.
    directories = []
    directory = os.path.dirname(os.path.abspath(target))
    root = os.path.abspath(ANSI_ROOT)
    while directory.startswith(root) and len(directory) >= len(root):
        directories.append(directory)
        if directory == root:
            break
        directory = os.path.dirname(directory)

    forms = []
    for directory in reversed(directories):
        load_lsp = os.path.join(directory, 'load.lsp')
        if not os.path.exists(load_lsp):
            continue
        with open(load_lsp, 'r', errors='replace') as handle:
            for line in handle:
                stripped = line.strip()
                if stripped.startswith('(compile-and-load'):
                    if stripped not in forms:
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
    """Start hang detection for a targeted run.

    This used to be a private daemon thread here that slept for the whole
    timeout and then `os._exit`ed. Two things were wrong with it, and both
    are why the 2026-08-15 `run_ansi.py cons` run sat wedged at 35GB for 35
    minutes against a 900s timeout without ever exiting:

      * it measured *total runtime*, not time without progress, so the
        timeout had to be set high enough for the slowest legitimate run --
        which makes it useless as a hang detector; and
      * it depended on an ordinary Python thread being scheduled and
        re-acquiring the GIL. On a process that has ballooned into swap that
        is not something to rely on for the *last-resort* escape.

    `fclpy.watchdog` replaces it with one shared, progress-based mechanism
    (the same one `run_all_tests.py` now uses -- it previously had none at
    all), whose hard stop is `faulthandler`'s C-level timer rather than a
    Python thread, and which dumps every thread's traceback so a killed run
    says *where* it died.
    """
    if seconds <= 0:
        return
    watchdog.watch_output()
    watchdog.arm(warn_after=min(watchdog.WARN_AFTER, seconds), kill_after=seconds)


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
    parser.add_argument('--results-out', default=None, metavar='PATH',
                        help="write this run's passed/failed names as JSON "
                             "(default ansi_results/targeted-last.json)")
    parser.add_argument('--update-checklist', action='store_true',
                        help="merge this run's results into ansi_results/*.txt and "
                             "regenerate docs/ansi_checklist.md -- the way to keep the "
                             "checklist current without a ~2 hour full run")
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
        watchdog.set_label(state['phase'])
        print("Loading harness: gclload1.lsp")
        runtime.load_and_evaluate_file(
            os.path.join(ANSI_ROOT, 'gclload1.lsp'), env, verbose=False)

        # gclload2.lsp -- the file this runner stands in for -- opens with
        # (in-package :cl-test) and only then loads each directory's load.lsp,
        # so every target is read with CL-TEST current. gclload1.lsp's own
        # in-package does not carry over: LOAD binds *PACKAGE* for the extent
        # of the file (CLHS 24.1), exactly as it does in a conforming Lisp.
        #
        # Without this the aux files are read in CL-USER, and a preamble file
        # is *not* interchangeable with one read in CL-TEST: several of them
        # reference variables that ansi-aux.lsp defines in CL-TEST, so the two
        # end up as different symbols with the same name. That made
        # types-and-classes' TYPES.9/9A read a CL-USER::*SUBTYPE-TABLE* that
        # nothing ever bound, and it would silently make any targeted run
        # disagree with the full-suite path it is supposed to reproduce.
        eval_string("(in-package :cl-test)", env)

        for target in targets:
            rel = os.path.relpath(target, ANSI_ROOT).replace('\\', '/')
            for form in aux_preamble_forms(target):
                print("Loading aux:    %s" % form)
                eval_string(form, env)
            state['phase'] = 'loading %s' % rel
            watchdog.set_label(state['phase'])
            print("Loading tests:  %s" % rel)
            runtime.load_and_evaluate_file(target, env, verbose=False)

        registered = _lisp_list_to_str_list(eval_string(
            "(mapcar (lambda (e) (string (regression-test::name e))) "
            "(cdr regression-test::*entries*))", env))
        print("Registered %d tests" % len(registered))

        state['phase'] = 'running tests'

        watchdog.set_label(state['phase'])
        started = time.perf_counter()
        eval_string("(regression-test:do-tests)", env)
        elapsed = time.perf_counter() - started
    finally:
        os.chdir(original_cwd)

    state['phase'] = 'reporting'

    watchdog.set_label(state['phase'])
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

    # Always record the run so the checklist can be brought up to date from it
    # later, whether or not --update-checklist was asked for on this invocation.
    results_path = args.results_out or os.path.join(
        REPO_ROOT, 'ansi_results', 'targeted-last.json')
    os.makedirs(os.path.dirname(results_path), exist_ok=True)
    with open(results_path, 'w', encoding='utf-8') as handle:
        json.dump({'targets': args.targets,
                   'passed': sorted(passed),
                   'failed': sorted(failed),
                   'unaccounted': sorted(missing)},
                  handle, indent=1)
    print("\nResults written to %s" % os.path.relpath(results_path, REPO_ROOT))

    if args.update_checklist:
        if missing:
            print("NOT updating the checklist: %d registered tests never ran, so this "
                  "run's view of those files is incomplete." % len(missing))
        else:
            sys.path.insert(0, os.path.join(REPO_ROOT, 'scripts'))
            import ansi_checklist
            fixed, regressed, new = ansi_checklist.merge_targeted(results_path)
            print("Checklist merge: %d newly passing, %d newly failing, %d new names"
                  % (fixed, regressed, new))
            ansi_checklist.main_render()

    return 1 if (failed or missing) else 0


def run_with_deep_stack(func):
    """Run `func` on a thread with a large stack and a raised recursion limit.

    fclpy evaluates by recursive descent, so one level of Lisp recursion
    costs roughly a dozen Python frames. CPython's default 1000-frame limit
    therefore caps Lisp recursion at under a hundred levels -- and the ANSI
    harness itself exceeds that routinely: `equalp-with-case` in rt.lsp
    compares lists by recursing on the cdr, one level per element, so
    checking a test whose value is a ~60-element list overflows before the
    code under test is ever at fault.

    That is a limit of the host, not a defect being hidden: the recursion is
    bounded by the data, and a `RecursionError` here aborts the whole run
    rather than failing one test, which destroys the measurement. The larger
    thread stack is what makes the raised limit safe -- lifting
    `setrecursionlimit` alone just trades a Python exception for a hard
    interpreter crash once the real C stack runs out.
    """
    sys.setrecursionlimit(60000)
    # Platform limits on thread stack size vary (Windows rejects sizes it
    # considers unreasonable outright), so take the largest that is accepted
    # rather than assuming one works.
    for megabytes in (512, 256, 128, 64, 32, 16):
        try:
            threading.stack_size(megabytes * 1024 * 1024)
            break
        except (ValueError, RuntimeError):
            continue

    result = {}

    def target():
        try:
            result['code'] = func()
        except BaseException as exc:  # re-raised on the main thread below
            result['error'] = exc

    thread = threading.Thread(target=target)
    thread.start()
    thread.join()

    if 'error' in result:
        raise result['error']
    return result.get('code', 0)


if __name__ == '__main__':
    sys.exit(run_with_deep_stack(main))
