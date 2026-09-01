# Compliance Audit — Are There Cheats?

**Date:** 2026-09-01
**Scope:** Read-only source review of `fclpy/`, `scripts/`, `run_all_tests.py`, `run_do_test.py`,
`tests/`, and the git history of the baseline/checklist files, checked against this project's
own anti-cheat rubric in `plan.md` ("The final compliance gate" and "Ways to fake compliance").
No test-affecting commands were run; this was performed in parallel with a real full
`run_all_tests.py` run and avoided touching `ansi_results/*` or `docs/ansi_checklist.md`.

**Bottom line: no confirmed cheat was found.** The mechanisms this project explicitly bans for
itself — `rt:load-expected-failures`, `*FEATURES*` shrinking, exception-swallowing in the runner,
baseline-refresh-without-a-fix, silently deleting `§5` deviations, test-detection special-casing —
were each checked and each came back clean. Four real, lower-severity issues surfaced and are
recorded below; none of them make a test pass that should fail, but two are worth fixing and two
are just documentation lag.

## Findings

### 1. `runtime.py:load_and_evaluate_file()` can silently drop a whole file's tests — real gap, not exploited today

`fclpy/runtime.py` (used by both `run_all_tests.py` loading `doit.lsp`, and `run_ansi.py` loading
`gclload1.lsp`) reads and evaluates one top-level form at a time. Its handler:

```python
except Exception as e:
    if "reader-error" in str(e) or not content.strip():
        break
    print(f"  Error evaluating expression {expr_count} in {filename}: {e}...")
    if verbose or os.environ.get('FCLPY_LOAD_TRACEBACK') == '1':
        traceback.print_exc()
```

re-raises a `ConditionException` one clause above (correct — that's a real Lisp condition), but a
**raw, unconverted Python exception** during a top-level form is printed and swallowed, and the
loop moves on to the next top-level form. Since `doit.lsp` is a sequence of `(load "<dir>/load.lsp")`
calls, a raw Python exception escaping mid-directory would abandon the rest of that one directory
(its tests never register in RT's `*entries*`) while every other directory loads normally and the
run reaches `(do-tests)` and finishes looking clean.

`check_completeness()` in `run_all_tests.py` only checks *internal* consistency — that every entry
in `*entries*` ends up in `*passed-tests*` or `*failed-tests*` — never that the total matches an
expected count. So `COMPLETENESS: OK` would still print, just with a smaller `total`, and nothing
downstream would notice a directory quietly went missing.

This is **not itself a cheat** (nobody engineered it to hide failures, and it requires a *second*,
independent bug — a raw exception escaping the otherwise-thorough Lisp-condition conversion in
`evaluation_core.py` — to manifest at all). But it is exactly the kind of silent-absorption path
standing rule 4 ("no silent-acceptance path") is meant to close, and it undermines the specific
guarantee CLAUDE.md leans on ("`COMPLETENESS: OK` ⇒ nothing was skipped").

**Recommendation:** re-raise (or at minimum count and hard-fail on) an unconverted exception in
`load_and_evaluate_file`, or have `check_completeness()` cross-check the registered-entry total
against the prior full run's count so a shrinkage is visible instead of silent.

### 2. `docs/duplicates_baseline.json` is stale-wide (safe direction, but sloppy)

Live `pipenv run python scripts/duplicates.py` finds 3 duplicate-registered functions
(`CALL-NEXT-METHOD`, `CLASS-OF`, `FIND-CLASS`) plus one dead module-level redefinition. The
committed baseline still lists 22 function entries, last refreshed 2026-08-22. `plan.md` itself
confirms most of those 22 were fixed since (`MAKE-INSTANCE`, `ENSURE-GENERIC-FUNCTION`, etc.) and
names the same three CLOS duplicates as the remaining debt.

This is the *safe* direction — a baseline wider than reality can't hide a genuinely new
duplicate, since anything new would still trip the gate — so it is **not** the banned pattern
("raise the ceiling to silence a new duplicate"). But it does violate the project's own stated
intent that the baseline is "shrinking debt, not a permanent allowlist": nobody has run
`--save-baseline` to tighten it as fixes landed, so it currently overstates debt by ~19 entries.

**Recommendation:** refresh `docs/duplicates_baseline.json` (a pure hygiene commit, safe by the
project's own rules since it only shrinks, never grows, unresolved debt) to keep the gate
meaningful as a debt tracker rather than dead weight.

### 3. `plan.md` §3 ("Known non-ANSI assertions in the unit suite") is stale in the *honest* direction

All three unit tests plan.md names as asserting known-wrong (non-ANSI) behavior appear to have
already been fixed:

- The old hash-table EQL-confusion test file no longer exists; its replacement
  (`tests/test_hash_tables.py`, from the 2026-08-24 "Repaired hash tables" commit) now asserts the
  correct ANSI answer.
- Same commit: the `hash-table-test` test now asserts the correct symbol (`EQL`) rather than a
  Python repr string.
- `tests/test_phase5_task2_sequence_functions.py`'s `FIND` test now asserts the correct
  `(funcall test item element)` argument order per CLHS 17.2.1, with a docstring citing it.

plan.md's line numbers for these have also drifted (files grew/were renamed). This is a
documentation-freshness bug, not evasion — if anything it makes the project look like it has *more*
open non-ANSI unit-test debt than it actually does, which is the opposite of the failure mode this
gate exists to prevent.

**Recommendation:** update or remove §3's three rows; each item is resolved per the project's own
criteria (a: fixed) and should say so or be dropped, consistent with `plan.md`'s rule that a
resolved row should leave the table rather than linger as a stale accusation.

### 4. Two "cannot fail" unit tests plan.md already flags — confirmed still present, one worse than described

- `tests/test_phase3_unwind_protect.py:131`, `test_unwind_protect_exception_preserves_cleanup` — body
  is a bare `pass` with a comment saying "for now, just document this behavior." Genuinely
  tautological (cannot fail). **Confirmed, unchanged.**
- `tests/test_phase4_multiple_values.py` (line drifted from `:330` to `:339-362`,
  `test_let_with_multiple_values`) — not a literal tautology (it can fail on a wrong value), but it
  accepts *either* of two mutually exclusive outcomes (a collapsed single value, or an
  un-collapsed `MultipleValues` object) as passing, so it doesn't pin the one ANSI-correct answer
  and wouldn't catch a regression between the two branches. Weaker than a real test, matches the
  spirit of plan.md's complaint even if "cannot fail" slightly overstates this one.

**Recommendation:** these are exactly the two plan.md's own gate (§7 item 7) already requires be
fixed or removed before the compliance gate can close; nothing new to add beyond confirming they're
still open.

### Checked and found clean

- **`expected-failures` mechanism:** not wired anywhere in `fclpy/`/`scripts/`; `rt:load-expected-failures`
  is never called; `docs/expected-failures.sexp` does not exist.
- **`*FEATURES*`:** exactly `(:FCLPY :COMMON-LISP :ANSI-CL)` in `fclpy/lispenv.py`, matching the
  documented allowlist — nothing has been added to shrink the test set.
- **Runner exception handling** (`run_all_tests.py`, `run_ansi.py`, `evaluation_core.py`'s
  apply/funcall ladders): Python exceptions are converted to real `ConditionException`s and
  re-raised, not swallowed into a false "pass"; the one real gap is finding #1 above, which is
  Lisp-condition-conversion-adjacent, not runner-level result-faking.
- **Test-mode / test-detection branches:** no `TESTING`/`is_test`/env-var-gated behavior switches
  anywhere in `fclpy/`; the only env var read that affects test loading (`FCLPY_LOAD_TRACEBACK`)
  only toggles Python traceback verbosity, never a Lisp-level answer.
- **Hardcoded/test-specific special-casing:** every `CL-TEST`/`ansi-test`-referencing comment in
  `fclpy/` cites the suite as the *motivating evidence* for a genuinely general fix (e.g. a builtin's
  `&key`-vs-`&optional` distinction, `LOAD`'s `*PACKAGE*` rebinding), never as a runtime detection
  target. No magic-value ladders keyed to specific test inputs were found; `TYPEP`'s known-duplicate
  ladder dispatches on type-specifier names (a bounded, legitimate vocabulary), not argument values.
- **`docs/ansi_checklist_baseline.json` refresh history:** every refresh in recent history rides
  along with a real, substantial source diff and a full-run citation in `plan.md`; per-file counts
  move down as fixes land, not sideways to hide a regression.
- **`plan.md` §5 ("Known temporary deviations") row history:** currently 37 rows, none silently
  deleted without a paired source fix; spot-checked rows that claim "implementation-defined" cite
  an actual CLHS section rather than asserting latitude that doesn't exist.
- **`xfail`/`skip` usage in `tests/`:** only three hits total, all `strict=True` and each explicitly
  cross-referenced to a live, disclosed `plan.md` §5 gap (so an accidental fix would break the build
  loudly, not slip through) — the model-correct way to track a known gap, not a way to hide one.

## Method

Four parallel read-only audits (general-purpose agents) covered, respectively: (1) runner/harness
exception handling and `expected-failures`/`*FEATURES*` wiring, (2) baseline/gate git history, (3)
test-detection/hardcoded-value special-casing in interpreter source, (4) weakened unit tests and
`plan.md` §3/§5 disclosure integrity. Each was graded against this project's own explicit rubric
(`plan.md`, "The final compliance gate" and "Ways to fake compliance") rather than a generic
checklist, since that rubric is unusually specific about what this project itself considers
cheating. Findings above are the union after removing duplicate/overlapping observations; nothing
here is a self-report from the code's own authors — it's independent grep/read verification against
the stated policy, including git-blame on the two baseline files to confirm refreshes were paired
with real fixes rather than used to launder regressions.
