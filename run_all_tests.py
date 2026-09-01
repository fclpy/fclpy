#!/usr/bin/env python3
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from fclpy import runtime, watchdog
from fclpy.lispfunc import setup_environment, eval_string

# This runner had no hang detection of any kind, so a form that never
# returned wedged the whole ~67-minute run silently and forever -- no
# warning, no traceback, no exit. scripts/run_ansi.py grew a watchdog and
# this file never did. See fclpy/watchdog.py for why LoopWatchdog cannot
# cover this case.
watchdog.watch_output()
# The hard stop must sit *above* LOOP_TIMEOUT_ERROR (600s), not at it. A loop
# that hits its own in-evaluator cap runs ~600s without printing anything,
# then aborts that one test and the run continues -- `characters` does exactly
# this. Killing the process at 600s would race that and turn a recoverable
# test failure into a dead run. The 120s warning still fires meanwhile, so a
# slow patch is visible in the log either way.
watchdog.arm(warn_after=120, kill_after=900)

REPO_ROOT = os.path.dirname(os.path.abspath(__file__))


def _lisp_list_to_str_list(value):
    """Convert a Lisp list returned by eval_string into a Python list of str.

    NIL/empty comes back as something that is not a lispCons; every non-empty
    proper list is a lispCons chain and is iterable (yields each car).
    """
    try:
        return [str(item) for item in value]
    except TypeError:
        return []


def check_completeness(env):
    """M0 step 2: assert RT actually ran every registered test.

    A prior silent-abort bug (LOOP with no implicit NIL block) made a truncated
    run look like a clean finish because the log tail coincided with the last
    test in a file. The only trustworthy check is RT's own bookkeeping:
    every entry in *entries* must end up in exactly one of *passed-tests* /
    *failed-tests*. Pulled from the live environment (not parsed from log text)
    so this cannot be fooled by a FORMAT bug in the printed summary.
    """
    entry_names = _lisp_list_to_str_list(eval_string(
        "(mapcar (lambda (e) (string (regression-test::name e))) "
        "(cdr regression-test::*entries*))", env))
    passed_names = _lisp_list_to_str_list(eval_string(
        "(mapcar #'string regression-test:*passed-tests*)", env))
    failed_names = _lisp_list_to_str_list(eval_string(
        "(mapcar #'string regression-test:*failed-tests*)", env))

    total = len(entry_names)
    accounted_set = set(passed_names) | set(failed_names)
    missing = [n for n in entry_names if n not in accounted_set]
    extra = [n for n in accounted_set if n not in set(entry_names)]

    print('COMPLETENESS: total=%d passed=%d failed=%d accounted=%d missing=%d extra=%d'
          % (total, len(passed_names), len(failed_names), len(accounted_set), len(missing), len(extra)))

    # The check above is purely *internal*: it proves every registered test was
    # accounted for, not that everything got registered. Two ways a whole
    # directory can go missing without it noticing, both closed here.
    #
    # (a) A top-level form that failed to evaluate. `runtime.load_and_evaluate_file`
    #     absorbs those and continues, so the `deftest` forms after it in that
    #     file never register. See runtime.LOAD_ERRORS.
    load_errors = list(getattr(runtime, 'LOAD_ERRORS', ()))

    # (b) The registered total silently shrinking for any other reason. The
    #     previous full run's entry list is right here on disk, so compare
    #     against it -- *before* it gets overwritten below. Growth is fine (new
    #     tests); a drop means tests that used to register no longer do.
    results_dir = os.path.join(REPO_ROOT, 'ansi_results')
    previous_total = None
    previous_path = os.path.join(results_dir, 'all.txt')
    try:
        with open(previous_path) as handle:
            previous_total = len([line for line in handle.read().split('\n') if line.strip()])
    except (OSError, ValueError):
        previous_total = None
    shrank = (previous_total is not None and total < previous_total)

    ok = not missing and not extra and not load_errors and not shrank
    if ok:
        print('COMPLETENESS: OK')
    else:
        print('COMPLETENESS: MISMATCH')
        for name in missing:
            print('MISSING-ENTRY: %s' % name)
        for name in extra:
            print('EXTRA-ACCOUNTED-NAME: %s' % name)
        for filename, index, description in load_errors:
            print('DROPPED-TOP-LEVEL-FORM: %s (expression %s): %s'
                  % (filename, index, description))
        if shrank:
            print('REGISTERED-TEST-COUNT-SHRANK: %d now, %d in the previous run '
                  '(ansi_results/all.txt) -- %d test(s) stopped registering'
                  % (total, previous_total, previous_total - total))

    os.makedirs(results_dir, exist_ok=True)
    with open(os.path.join(results_dir, 'all.txt'), 'w') as f:
        f.write('\n'.join(entry_names) + '\n')
    with open(os.path.join(results_dir, 'passed.txt'), 'w') as f:
        f.write('\n'.join(passed_names) + '\n')
    with open(os.path.join(results_dir, 'failed.txt'), 'w') as f:
        f.write('\n'.join(failed_names) + '\n')

    # A full run supersedes every targeted run merged in since the last one
    # (scripts/ansi_checklist.py --merge), so the amendment log starts over --
    # otherwise the checklist would keep claiming to be "amended by" runs whose
    # results this file has just overwritten.
    merge_log = os.path.join(results_dir, 'merges.log')
    if os.path.exists(merge_log):
        os.remove(merge_log)

    return ok


env = setup_environment()
base = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
doit_lsp = os.path.join(base, 'ansi-test', 'doit.lsp')


if os.path.exists(doit_lsp):
    print('Loading doit.lsp...')
    watchdog.set_label('running doit.lsp')
    res = runtime.load_and_evaluate_file(doit_lsp, env, verbose=False)
    print('Result: %s' % str(res))
    watchdog.set_label('checking completeness')
    complete = check_completeness(env)
    if not complete:
        sys.exit(1)
else:
    print('doit.lsp not found; continuing')
