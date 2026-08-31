# recursion-plan.md — RecursionError as an architecture defect

**Status:** planning (2026-08-31). No code changed under this plan yet.
**Source:** 2026-08-31 full run (`run_all_tests.log`), cross-referenced with
plan.md §1 item 3 and §"Discovered issues" (the 08-31 wedge note).

## The constraint this plan works under

**Do not fix by raising the recursion limit.** `sys.setrecursionlimit` /
`threading.stack_size` are out of scope: `scripts/run_ansi.py` already runs
that way (`run_with_deep_stack`, 60000 frames + a large thread stack), which
is precisely why targeted runs never reproduce these failures, and a
deep-stack wrapper for `run_all_tests.py` was written and **reverted at the
user's direction**. `run_all_tests.py` (107 lines) runs at CPython's default
**1000-frame** limit. The fixes below reduce *work per frame*, not the
number of frames allowed.

The two legitimate tools, per this plan:

1. **Refactor recursive walks into loops** — anywhere fclpy's own code
   recurses with depth proportional to a list's *length* (cdr-chain
   recursion), replace with iteration. A list's length is unbounded; only
   genuine tree *depth* may recurse, and even that can use an explicit stack.
2. **Reduce Python frames consumed per Lisp call level** — the evaluator's
   eval→call cycle currently burns `~a dozen Python frames` per level of Lisp
   recursion (measured, recorded in `scripts/run_ansi.py:377-383`), so
   CPython's 1000-frame limit caps Lisp recursion at **under 100 levels**.
   That is an architecture defect of the evaluator, independent of any
   one operator.

## 1. The failures

`run_all_tests.log` contains **22 matches for "RecursionError"**, which are
**11 distinct test failures** — each failure block prints the error string
twice (`#<ERROR Python error in function call: RecursionError: …> [Python
error in function call: RecursionError: …]`, same line). plan.md's item 3
listed ten of them as "RecursionError ×11"; `PRINT.BACKQUOTE.RANDOM.14` is
the eleventh and was missing from that list.

| # | Test | ansi-test file | log line | Failing form (abridged) |
|---|------|----------------|----------|--------------------------|
| 1 | `COPY-TREE.2` | cons.lsp | 158 | `(LET* ((X (COPY-LIST *UNIVERSE*)) (Y (COPY-TREE X))) (CHECK-CONS-COPY X Y))` |
| 2 | `INTERSECTION.12` | setf.lsp (cons) | 164 | `(INTERSECTION-12-BODY 100 100)` |
| 3 | `NINTERSECTION.3` | setf.lsp (cons) | 170 | `(NINTERSECTION-WITH-CHECK NIL (LOOP FOR I FROM 1 TO 100 COLLECT I))` |
| 4 | `NINTERSECTION.10` | setf.lsp (cons) | 176 | `(EQUALT (SORT (LET ((RESULT (NINTERSECTION-WITH-CHECK (LOOP … 0 TO 1000 BY 3 …) (LOOP … 0 TO 1000 BY 7 …)))) …)))` |
| 5 | `NINTERSECTION.11` | setf.lsp (cons) | 182 | same shape as .10 with `:TEST` on 0 TO 999 BY 5 / BY 7 |
| 6 | `NINTERSECTION.12` | setf.lsp (cons) | 188 | `(NINTERSECTION-12-BODY 100 100)` |
| 7 | `UNION.24` | setf.lsp (cons) | 194 | `(DO-RANDOM-UNIONS 100 100 200)` |
| 8 | `NUNION.24` | setf.lsp (cons) | 200 | `(DO-RANDOM-NUNIONS 100 100 200)` |
| 9 | `SET-DIFFERENCE.13` | setf.lsp (cons) | 206 | `(DO-RANDOM-SET-DIFFERENCES 100 100)` |
| 10 | `NSET-DIFFERENCE.13` | setf.lsp (cons) | 212 | `(DO-RANDOM-NSET-DIFFERENCES 100 100)` |
| 11 | `PRINT.BACKQUOTE.RANDOM.14` | printer.lsp | 508 | `(LOOP FOR X = (MAKE-RANDOM-BACKQUOTED-FORM 100) REPEAT 500 NCONC (RANDOMLY-CHECK-READABILITY X :TEST (FUNCTION IS-SIMILAR)))` |

The error message has two variants, and the split is diagnostic:

- 10 × `maximum recursion depth exceeded` — the recursion is on the Lisp
  call path (the evaluator's own frames), tests 1–10.
- 1 × `… while calling a Python object` (`PRINT.BACKQUOTE.RANDOM.14`) — the
  recursion went through Python-level call machinery (nested READ of a
  100-deep backquote form, and/or `is-similar*`'s CLOS recursion, generic
  dispatch on top of the eval cycle).

## 2. Where the frames go (root causes)

### A. The evaluator's call cycle is recursive descent with a fat frame cost

`scripts/run_ansi.py`'s docstring records the measurement: *one level of Lisp
recursion costs roughly a dozen Python frames; CPython's default 1000-frame
limit therefore caps Lisp recursion at under a hundred levels*. The chain for
a user-defined function call (`make_ordinary_function`,
`evaluation_special_forms.py:1684`):

```
eval(form)                     # dispatcher                      — 1 frame
  eval(arg_i) per argument     # transient but ON the stack when
                               #   the recursive call happens    — +1 (nested)
  func(*args) → call()         # the closure                     — +1
    run_body()                 # nested def inside call          — +1
      eval(body-form)          # → eval_if/eval_and/… handlers   — +1 each
        eval(call-form)        # next recursion level…
```

Plus per-call transients (`split_keyword_args`, `_check_ordinary_arity`,
`BindingFrame.bind`, `_bind_ordinary_lambda_list_tail`,
`_run_with_nil_block`, `_canonicalize_nil_symbol`). For a CLOS generic
(`is-similar*`) add `call_generic_function` → method selection →
`classes.call_method` → binder → `eval`.

The ansi-test helpers behind tests 2–10 are **loop-based themselves**
(`do-random-unions` etc. are `(loop for i from 1 to niters …)`); the deep
recursion is in their *callees*, which are also test code and cannot be
edited:

- `make-scaffold-copy` / `check-scaffold-copy` (`auxiliary/cons-aux.lsp:20/31`)
  recurse on **car and cdr** of a copy of the list — depth = list length
  (100 for tests 2, 3, 6–10; **334/143 for tests 4–5**).
- `split-list` (inside `shuffle`, `cons-aux.lsp:233`) recurses on `(cddr x)`
  — depth ≈ size/2.
- `is-similar*` (`auxiliary/ansi-aux.lsp:908`) recurses over conses through
  generic-function dispatch — the most frames per level of all.

At ~12 Python frames per Lisp level, `make-scaffold-copy` on a 334-element
list needs ~4000 frames — no argument tweaking makes that fit under 1000.
**Only reducing the per-level cost does.**

### B. fclpy's own tree walkers recurse on the cdr spine

Depth = list length, so *any* k fails once the list is long enough. Directly
implicated:

- **`COPY-TREE`** — `fclpy/lispfunc/misc_macros.py:1069-1086`
  (`copy_tree(obj.car)`, `copy_tree(obj.cdr)`). Test 1 copies `*UNIVERSE*`
  (hundreds of conses) → cdr-chain depth = list length → overflows all by
  itself. Also present as a **dead duplicate** at `fclpy/lispfunc/core.py:241-245`
  (unregistered, no `@cl_function`) — standing rule 3, delete the dead copy.
- **`TREE-EQUAL`** — `fclpy/lispfunc/sequences_compose.py:545-564`;
  `compare(car(a), car(b)) and compare(cdr(a), cdr(b))` — same shape. Not
  behind any of the 11 (test 11 goes through `is-similar*`), but it is the
  identical defect one spine away.

**The fix pattern already exists in-repo.** `EQUAL` (`comparison.py:153-176`)
was converted for exactly this defect and documents the rule:

> *only the `car` may recurse* … A list's length is unbounded in a way its
> nesting depth is not.

`EQUALP` (`comparison.py:231`) mirrors it. COPY-TREE and TREE-EQUAL get the
same shape: walk the cdr spine in a `while` loop in the current frame;
recurse (or push onto an explicit stack) only into cars.

### C. RecursionError becomes a Lisp *value* (and then a wedge)

`evaluation_core.py:1599-1602` — the call-site catch-all
`except Exception as e` wraps `RecursionError` into
`Error("Python error in function call: RecursionError: …")`, which RT then
records as the test's **actual value** (that is what the 11 blocks above are).
Two consequences: the failure is silent (nothing in `run_all_tests.err`
names the recursion — 0 RecursionError matches there), and once the stack is
exhausted near RT's failure printer, the run stalls (plan.md's 08-31 wedge:
100% CPU, no output, after NSET-DIFFERENCE.13).

CLHS puts stack/storage exhaustion under **`STORAGE-CONDITION`**
(see `universe.lsp:33`, `cl-symbol-names.lsp:1874`). A RecursionError must
never be wrapped into a plain `Error` value.

## 3. Work plan — one mechanism at a time

Each step: smallest reproduction via `eval_string` → fix →
`scripts/run_ansi.py <file> --update-checklist` → `scripts/gate.py` →
re-run the directories the change plausibly reaches.

### Step 1 — COPY-TREE: spine-iterative (fixes test 1)

Rewrite `misc_macros.py:copy_tree` following `EQUAL`'s pattern: loop the cdr
spine in the current frame, recurse into cars only. Worst-case Python depth
becomes tree *depth* (small for real data), not list length. Delete the dead
`core.py:241` copy (duplicate register; run `scripts/duplicates.py --baseline`
afterwards).

Verification: `cons` (COPY-TREE.1/.2, CHECK-CONS-COPY paths), then
`scripts/run_ansi.py cons/cons.lsp --update-checklist`.

### Step 2 — TREE-EQUAL: spine-iterative (same defect class)

Same conversion in `sequences_compose.py:tree_equal` (its inner `compare`
walks the spine iteratively; recursion only into cars). Verify
`tree-equal.1-16` + `sequences_search`-adjacent files. This is the same
mechanism as Step 1 applied to the second instance of the pattern — batch
them as *one* reviewable change per the repo's rule 2, or split if the diff
obscures attribution.

### Step 3 — Flatten the user-function call path (reduces k, helps tests 2–10)

Targets in `evaluation_special_forms.make_ordinary_function`:

- Inline `run_body` (the nested def at :1758) into `call` — one frame per
  call level saved for every user function, including every recursion.
- Audit the per-call transients on the hot path
  (`_canonicalize_nil_symbol` per arg, `_check_ordinary_arity`,
  `split_keyword_args` in the caller, `_run_with_nil_block`) — anything
  active *while* the next eval descends is a frame cost; move work off the
  descent path where it can be precomputed at definition time
  (arity shape, parameter list).

**Measure before/after** with a depth probe under the *default* limit
(no deep stack):

```powershell
pipenv run python -c "import sys; sys.path.insert(0,'.'); from fclpy import lispenv; from fclpy.lispfunc import eval_string; lispenv.setup_standard_environment(); print(eval_string('(progn (defun %probe (n) (if (zerop n) 0 (1+ (%probe (1- n))))) (%probe 500))'))"
```

Record the largest `n` that completes before/after. Baseline expectation
from the docstring measurement: <100 today; target ≥300 after flattening
(needed for the 334-deep scaffold copy in tests 4–5).

### Step 4 — Tail-call trampoline in the eval cycle (the architectural core)

Make the eval↔call cycle iterative for calls in tail position: `eval`'s
compound-form branch returns a lightweight `TailCall(func, args)` marker and
loops instead of recursing; the drivers that consume it (the compound-form
loop in `eval`, `run_body`, APPLY/FUNCALL) unwind it in a `while`.

Scope discipline:

- First shape to trampoline: a call in tail position of a function body
  (covers `check-scaffold-copy`'s cdr leg through `AND`'s last form,
  `split-list`, `is-similar*` tail legs).
- Second: tail position of `PROGN`/`LET`/`IF`-else bodies.
- Do **not** trampoline argument-position calls (they must grow the stack —
  their results are still needed); `make-scaffold-copy`'s car/cdr args stay
  recursive, which is why Step 3's flattening is what carries tests 4–5.

This is a binder-adjacent, wide-blast-radius change: per the dev loop, re-run
several directories (cons, control-flow, evaluation, plus whatever the
checklist shows moved), and a full run is worth it even if targeted runs look
clean.

### Step 5 — Depth budget → STORAGE-CONDITION (safety net, not a bypass)

The evaluator counts Lisp call depth (a counter threaded through
`make_ordinary_function`'s `call` / the eval cycle). When it exceeds a budget
sized as `(1000 - measured base) / k_current`, signal a real
`STORAGE-CONDITION` *while Python stack remains to construct and signal the
condition* — instead of letting CPython's `RecursionError` surface through
the catch-all at `evaluation_core.py:1599` as a Lisp value.

- The budget must be well under the real limit so the condition can be
  built; this is the software analogue of a stack check, and it is what
  removes the 08-31 wedge class (RT failure printing stalling at 100% CPU).
- `except RecursionError` is removed from the call-site catch-all: it is
  either prevented by the budget or it is a bug.
- This is **not** an expected-failure mechanism: a genuinely deep user
  program still fails — but as the ANSI-specified condition, diagnosably,
  and without wedging the harness.

## 4. Verification ladder

1. Per-step targeted runs (`run_ansi.py <group> --update-checklist`).
2. A **default-limit reproduction harness** for the 11 tests (runs the
   failing forms under `eval_string` with no deep-stack wrapper) — this is
   the acceptance test each step must move; keep it as a throwaway script in
   `C:\Users\Windows\AppData\Local\Temp\opencode`, not in the repo.
3. `pipenv run python scripts/gate.py` after every step (pytest +
   duplicates baseline + checklist baseline; never clear a failure with
   `--save-baseline`).
4. Re-run: `cons`, the set-operation files, `printer`, plus
   `control-structures`/`evaluation` after Step 4.
5. **Full `run_all_tests.py` run is mandatory** before moving the official
   scoreboard (plan.md §1) — these are harness-path-adjacent failures that
   targeted runs cannot fully see, and the run itself is the only place the
   default-limit behaviour is measured end to end.

## 5. Explicit non-goals

- No `sys.setrecursionlimit`, no `threading.stack_size`, no launcher wrapper
  for `run_all_tests.py` (reverted at user direction; do not re-propose).
- No edits to `../ansi-test/` (its recursive helpers are the workload the
  interpreter must support).
- No `expected-failures` wiring, no `*FEATURES*` evasions (plan.md,
  "Ways to fake compliance").

## 6. Pointers

- plan.md §1 item 3 (the ×11 list — this file supersedes it with the
  corrected 11-test list) and the 08-31 wedge note in "Discovered issues".
- `scripts/run_ansi.py:374-421` — the deep-stack wrapper and its frame-cost
  measurement (the numbers Step 3 must move).
- `comparison.py:153-176` — the in-repo pattern for spine-iterative rewrites.
- `misc_macros.py:626` — prior art for the same idea ("the spine (`cdr`) is
  walked iteratively, not recursively").
