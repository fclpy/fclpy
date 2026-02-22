# Test Suite Crash Repair Guide

## Objective
Eliminate crashes so **all ANSI tests complete execution** (pass or fail, but don't crash).

---

## Workflow

### 1. Run Full Test Suite

```bash
pipenv run python run_all_tests.py > run_all_tests.log
```

NOTE: Python must be run with pipenv

- Check stderr for exception traceback
- Open `run_all_tests.log` and find the last test name

### 2. Identify Crashing Test

The crashing test is **the next test after the last one in the log**.

Example: If log ends with `DEFMACRO.10 failed`, the crashing test is `DEFMACRO.11`.

**If unsure:** Read the stderr traceback to see which test raised the exception.

If the crashed test cannot be identified from the because the last completed test is also the final test listed in the .lsp file, then determine the test order using doit.log.

The file doit.log contains the execution output from a Lisp process that ran all tests sequentially and can be used to identify the test that was running when the crash occurred.


### 3. Isolate & Reproduce

Modify `run_do_test.py` to run only the suspected test:

```lisp
test_lisp = "(in-package :cl-test) (do-test 'TESTNAME.N)"
```

Run:
```bash
pipenv run python run_do_test.py
```

Verify the exception reproduces. If not, the wrong test was identified—return to Step 2.

### 4. Diagnose & Fix

**Before making changes:**
```bash
git diff
```

This shows you all pending modifications. Clean up any leftover debug code from previous repairs.

**Repair strategy:**

1. **Check the reader/parser first** — Sometimes crashes are not in evaluation logic but in how code is read (tokenizer, readtable, lispreader). Check if the input syntax is being parsed correctly.

2. **Use targeted diagnostics only when needed** — Add `print()` statements to understand data shapes, then remove them immediately after the fix.

3. **Test one component at a time** — If multiple systems are involved (reader, parser, evaluator), isolate the problematic one.

4. **Fix ANSI compliance** — Prioritize fixes that make the code behave like standard Common Lisp.

5. **Remove all debug output** — Before verifying the fix, delete any temporary diagnostic prints.

### 5. Verify & Cleanup

**Run just the isolated test again:**
```bash
pipenv run python run_do_test.py
```

Confirm it completes without exception (output: `TESTNAME` or `TESTNAME failed`).

**Clean up debug code:**
```bash
git diff
```

Review the diff. Remove:
- All temporary `print()` statements
- Debug variable assignments
- Test files created for this repair


### 6. Re-run Full Suite

```bash
pipenv run python run_all_tests.py > run_all_tests.log
```

Verify the previously crashing test now appears in the log.

If a **regression** occurred (a different test now crashes), use `git diff` to review your changes and address the side effect.

### 7. Repeat

Loop back to Step 1 until the entire test suite completes without crashes.

---

## Rules for Efficient Repair

1. **One test at a time** — Fix crashes sequentially.
2. **No refactoring** — Only modify code to fix the crash; do not optimize unrelated code.
3. **Prioritize ANSI compliance** — Fixes should align with Common Lisp standards.
4. **Clean working tree** — Remove debug code immediately; do not commit changes automatically. Leave commits to the repository maintainer or perform them only after a careful manual review.
5. **Trust the traceback** — If unsure which test crashed, the stderr exception message shows exactly which test failed.

---

## Common Patterns

### Issue: Diagnostic prints polluting output

**Solution:** Remove ALL `print()` statements before verifying the fix. They interfere with test output parsing.

```bash
# Before cleanup (BAD)
[macro] rest_param repr: (BAR . BAZ)
[macro] tail_candidate type: ...
DEFMACRO.15

# After cleanup (GOOD)
DEFMACRO.15
```

### Issue: Root cause in parser, not evaluator

**Solution:** If crashes occur during macro expansion or special form evaluation, check the reader/parser first:
- `readtable.py` — list/token reading
- `lispreader.py` — token parsing
- `tokenizer.py` — character-level tokenization

### Issue: Dotted pairs or special syntax not parsed correctly

**Solution:** Verify the readtable handles Common Lisp syntax correctly (e.g., `(a . b)` should create a cons cell, not a list with three elements).

---

## Success Criteria

- All tests in `run_all_tests.log` show either `TESTNAME` or `TESTNAME failed`
- No exceptions interrupt execution
- No diagnostic prints clutter the output
