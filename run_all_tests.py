#!/usr/bin/env python3
import os, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from fclpy import runtime
from fclpy.lispfunc import setup_environment, eval_string

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

    ok = not missing and not extra
    if ok:
        print('COMPLETENESS: OK')
    else:
        print('COMPLETENESS: MISMATCH')
        for name in missing:
            print('MISSING-ENTRY: %s' % name)
        for name in extra:
            print('EXTRA-ACCOUNTED-NAME: %s' % name)

    results_dir = os.path.join(REPO_ROOT, 'ansi_results')
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
    res = runtime.load_and_evaluate_file(doit_lsp, env, verbose=False)
    print('Result: %s' % str(res))
    complete = check_completeness(env)
    if not complete:
        sys.exit(1)
else:
    print('doit.lsp not found; continuing')
