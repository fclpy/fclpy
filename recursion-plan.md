# recursion-plan.md — RecursionError as an architecture defect

**Status:** **All steps implemented** (2026-08-31). Steps 1-3 and 5 first (see
"Outcome" mid-document), then **Step 4** (self tail-call elimination) and
**Step 6** (IF chains resolved in-frame + explicit continuation stack for
argument evaluation) — both at the end of this file, which is where the
current numbers live.

Net effect at the **default** 1000-frame limit, which is what
`run_all_tests.py` runs at:

| | before | after |
|---|---|---|
| Python frames per Lisp level | 5 | **2** |
| max non-tail recursion depth | 149 | **372** |
| max tail recursion depth | 186 | **unbounded** |

That clears the deepest thing the suite demands (334 levels, `make-scaffold-copy`
in NINTERSECTION.10/.11) without raising the recursion limit, which this plan
forbids. `STORAGE-CONDITION` remains the condition for genuine
implementation-resource exhaustion.

A full `run_all_tests.py` run is still mandatory before moving the official
scoreboard — Step 6 changes the argument-evaluation path of every user function
call.
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

## Outcome (2026-08-31)

### What was implemented

- **Step 1 — COPY-TREE** (`misc_macros.copy_tree`): cdr spine walked
  iteratively (build from the tail backwards), car recursion only; a circular
  spine signals instead of hanging (the old code died with RecursionError, so
  raising preserves termination semantics). The dead unregistered `copy_tree`
  in `core.py` is deleted. `(copy-tree (make-list 2000))` works at the default
  limit (baseline: failed at 2000, passed at 700).
- **Step 2 — TREE-EQUAL** (`sequences_compose.tree_equal`): same shape, same
  circularity guard. Verified on 2000-element spines and all 29
  tree-equal ansi tests.
- **Step 3 — the call path, flattened.** The measured repeating units were:
  plain defun ~5 frames/level, COND-based user code **19** (a macro frame +
  `eval`+`eval_if` pair per clause), alternating and/or **8**, CLOS method
  **19**. The cuts, each one mechanism:
  - `make_ordinary_function` and `_make_method_function`: body loop inline in
    the closure, implicit block via the new `_implicit_block_frame` context
    manager (`evaluation_loops_conditionals`), whose `with` holds no frame;
    `_run_with_nil_block` remains as the thunk face for the iteration forms
    (now implemented on the same class).
  - `eval_if`: a chain of IFs through the else branch (COND's expansion shape)
    is stepped through in one frame.
  - `eval`'s ladder dispatches COND/AND/OR/WHEN/UNLESS to their existing
    in-frame evaluators (`eval_cond`/`_eval_logic`) instead of through the
    macro pipeline; the macro definitions stay for MACROEXPAND/MACRO-FUNCTION.
    `_eval_logic` steps a nested logic form in last position in-frame.
  - CLOS: `call_generic_function` calls the combination type directly (past
    `MethodCombination.invoke`'s delegation frame).
- **Step 5 — depth budget → STORAGE-CONDITION.** `_enter_lisp_call`/
  `_leave_lisp_call` in `evaluation_core`: user-function depth is counted; past
  a floor the *actual* Python stack is measured against
  `sys.getrecursionlimit() - 250` (adapts to the deep-stack wrapper), and over
  budget a real `STORAGE-CONDITION` is signalled while stack remains. The
  call-site catch-all converts a residual `RecursionError` into the same
  condition instead of `Error("Python error in function call: ...")`.
  Verified: an unbounded loop signals `StorageCondition`,
  `handler-case` matches it via a `storage-condition` clause and NOT via
  `error` (CLHS 9.1).
- **Step 4's full trampoline was not needed**: the narrower mechanisms above
  carried every failing test, and argument-position recursion (which a
  trampoline cannot help) was the binding constraint only until the per-level
  cost dropped.

### Measured outcome

- Max Lisp recursion depth (plain defun, default limit): **140 → 197**.
- The 10 set-operation/copy-tree acceptance forms: **10/10 pass** at the
  default limit (baseline: 0/10).
- The PRINT.BACKQUOTE.RANDOM.14 workload (500 random backquoted forms,
  print/read/is-similar round trips): **500/500 pass** at the default limit
  (baseline: RecursionError at iteration ~176).
- `cons` directory: **1882/1882 (100%)**. `objects` 861/862 (the documented
  pre-existing DEFGENERIC.30), `data-and-control-flow` and `iteration` fully
  green after the fixes below.
- PRINT.BACKQUOTE.RANDOM.14 itself still fails 1/14 — but now on a
  *pre-existing printer-similarity mismatch* (printed vs reread forms differ
  under random `*print-*` settings), a different defect class from recursion,
  visible only under the deep-stack runner and present at HEAD.

### Fixed in passing (standing-rule discoveries)

- **pytest regression** (3 `test_loop_clauses` failures, introduced by an
  earlier commit today converting LOOP's duplicate-binding signal to a
  condition without updating the tests): the engine's own claim check now
  signals the same PROGRAM-ERROR condition as the expansion-time pre-check,
  and the tests pin the condition. The fix also required
  `_direct_macroexpand_1` to (a) pass `ConditionException` through its blanket
  catch (the sibling defect `eval_macroexpand_1`'s catch removal fixed) and
  (b) treat a NIL environment as the *global* environment (CLHS 3.8's "null
  lexical environment"), not "cannot look up macros".
- **Four pre-existing ansi failures** fell out of the same fix:
  LOOP.4.7/.4.8 and LOOP.5.ERROR.3/.4 (macroexpand-time PROGRAM-ERROR) now
  pass.
- **VALUES place distribution** (`values.20`/`.21`, the checklist's last
  flagged regression): the store clause of a VALUES place has one store
  variable per *direct* sub-place, each receiving one value of the value form;
  a nested VALUES place is handed that single value (its remaining sub-places
  get NIL, CLHS 5.1.3). The old expansion flattened every nested store var
  against the value form's values.

### Remaining

- The full `run_all_tests.py` run (mandatory for the scoreboard; also the only
  measurement of these fixes end to end).
- The printer-similarity defect behind PRINT.BACKQUOTE.RANDOM.14's remaining
  failure — a FORMAT/printer matter, not recursion.
- `eval_and`/`eval_or` are now thin delegating wrappers over `_eval_logic`
  (the ladder calls `_eval_logic` directly); they exist only as the
  `evaluation.py` export surface. The second MACROEXPAND-1 implementation
  (`misc_packages`, recorded in its docstring as standing-rule debt) remains.

## Step 4 IS required after all — measured, then IMPLEMENTED 2026-08-31 (second session)

**Status: Step 4 landed for self tail calls. A residual remains and is now
Step 6 (below).** Read this section for the diagnosis; the outcome is under
"Step 4 implemented" and the open work under "Step 6".

Step 4 (the tail-call trampoline) was closed above as "not needed: the
narrower mechanisms carried every failing test." That conclusion was drawn
from runs under `scripts/run_ansi.py`, which raises the recursion limit to
60000. **Under the default limit — which is what `run_all_tests.py` uses, and
`run_all_tests.py` is the scoreboard — it is still required.** The measurements
below are all at the default limit.

### What actually happens now

A bare `run_all_tests.py` **aborts the whole run** after 131 seconds, at test
5484 of 21908:

```
COMPLETENESS: total=21908 passed=5470 failed=14 accounted=5484 missing=16424
COMPLETENESS: MISMATCH
Error loading file '...ansi-test\doit.lsp': Stack overflow calling NOTNOT:
Lisp recursion exceeded the available stack
```

**This is the regression, and it is not "a test started failing."** The
failing test, `COPY-TREE.2`, was *already failing* before this work — it is in
the 2026-08-31 07:32 full run's failure list (verified by extracting that
list from the archived log, `ansi_results/snapshots/2026-08-31-run1-amended/`;
note that snapshot's `failed.txt` is the *amended* 62-entry file and cannot be
used for this — the 13:50 targeted merges ran under the deep stack and mark
COPY-TREE.2 passing). What changed is the **blast radius**:

| | before Step 5 | after Step 5 |
|---|---|---|
| stack exhausted | `RecursionError` | `STORAGE-CONDITION` signalled |
| how it surfaces | wrapped into an `Error` *value* | a real condition |
| RT's reaction | records the test as failed | **declines it** — RT guards on `error`, and `STORAGE-CONDITION` is a `SERIOUS-CONDITION`, not an `ERROR` |
| run | continues | **aborts out of `(load "doit.lsp")`** |

Step 5 is *correct* (CLHS puts stack exhaustion under `STORAGE-CONDITION`);
it simply made a pre-existing overflow fatal instead of local.

### The mechanism, measured

`check-cons-copy` (`auxiliary/cons-aux.lsp:56`) — **test-suite code, not
editable** — recurses on **car and cdr**:

```lisp
(defun check-cons-copy (x y)
  (cond
   ((consp x)
    (and (consp y)
         (not (eqt x y))
         (check-cons-copy (car x) (car y))
         (check-cons-copy (cdr x) (cdr y))))   ; <-- TAIL POSITION
   ((eqt x y) t)
   (t nil)))
```

So its depth is the **list length**, not the tree depth. `COPY-TREE.2` walks
`*universe*`, whose length is **700**.

Measured at the default limit (probe: monkeypatch `_enter_lisp_call`, record
the Lisp-name chain at peak Python depth):

- peak Python depth **747** against a budget of `getrecursionlimit() - 250` = **750**
- peak chain length **119**, composition: `DO-TESTS, DO-ENTRIES, DO-ENTRY, %DO,`
  then **113 × `CHECK-CONS-COPY`**, then `EQT`, `NOTNOT`
- **~6 Python frames per Lisp level** (steady, measured level by level)
- baseline before any user call: **6 frames** — harness overhead is *not* a factor
- `check-cons-copy` on a flat list: **120 elements OK, 200 overflows**
- `COPY-TREE.1` (the name the log prints last) is **fine** — its tree is ~8
  conses; verified passing in isolation, tree component by tree component. The
  log's last-printed name is not the failing test.

700 levels × 6 frames ≈ 4200 frames. **No frame-shaving fits that under 1000**
— at 1 frame per level it would still be 700 plus baseline. Step 3's approach
is exhausted here.

### Why Step 4 fixes it

The `(check-cons-copy (cdr x) (cdr y))` call is the **last form of the `AND`**,
which is the last form of the `COND` clause, which is the function body's
tail — a genuine tail call. Eliminating it turns the 700-deep cdr recursion
into a **loop**, leaving only the *car* recursion, which is bounded by tree
depth (tiny for real data). `COPY-TREE.2` then **passes** rather than failing
more gracefully — which is the right target: fclpy signalling
`STORAGE-CONDITION` on a 700-deep tail recursion is a conformance failure
either way, since a conforming implementation does not run out of stack there.

**It covers `check-scaffold-copy` but NOT `make-scaffold-copy`** — an earlier
draft of this section claimed both, and that was wrong. Check the shapes:

```lisp
(defun check-scaffold-copy (x xcopy)          ; cons-aux.lsp:31
  (and (eq x (scaffold-node xcopy))
       (or (not (consp x))
           (and (check-scaffold-copy (car x) (scaffold-car xcopy))
                (check-scaffold-copy (cdr x) (scaffold-cdr xcopy))))))
                                              ; ^ last operand of AND, of OR,
                                              ;   of AND -> TAIL. Step 4 helps.

(defun make-scaffold-copy (x)                 ; cons-aux.lsp:20
  (if (consp x)
      (make-scaffold :node x
                     :car (make-scaffold-copy (car x))
                     :cdr (make-scaffold-copy (cdr x)))
      ...))                                   ; ^ ARGUMENT position. No
                                              ;   tail-call transform applies.
```

`make-scaffold-copy` is therefore the residual, and it is what Step 6 exists
for.

### Step 4 implemented — measured outcome

`TailCall` (`evaluation_core.py`) + `tail_target` threaded into the tail
subforms of `IF`, `COND`, `AND`/`OR` and `PROGN`, unwound by a `while` loop in
`make_ordinary_function.call`. Measured at the default limit, against HEAD with
an *identical* probe (the "197" recorded in the earlier Outcome section is not
reproducible and was measured some other way — HEAD measures 149):

| | HEAD | after Step 4 |
|---|---|---|
| max **non-tail** depth | 149 | **149** (unchanged) |
| max **tail** depth | 186 | **unbounded** (200 000 verified) |

- `cons/copy-tree.lsp` **8/8 at the default limit**, COPY-TREE.2 included.
- `cons` + `data-and-control-flow` + `eval-and-compile`: **3628/3628**, 0 unaccounted.
- pytest **2097 passed, 3 xfailed**; `duplicates.py --baseline` clean.
- `tests/test_recursion_depth.py` added, pinning all of the above at the
  default limit.

Two design decisions worth keeping:

- **Self tail calls only, not a general trampoline.** A marker is produced only
  when the callee *is* the closure already on the stack waiting to unwind it
  (`func is tail_target`), so it can never escape. A general marker would have
  to travel out through FUNCALL, APPLY and every builtin taking a function
  designator (SORT's predicate, REMOVE-IF's test, the MAP\* family); one that
  forgot to unwind it would return the marker as a wrong *value* rather than
  crashing. Mutual tail recursion (f→g→f) stays an ordinary call: it costs
  frames but cannot produce a wrong answer.
- **Not threaded into LET/LET\*/BLOCK/CATCH/TAGBODY/UNWIND-PROTECT/WHEN/UNLESS.**
  For the binding forms because a dynamic binding must still be live while the
  callee runs — `(let ((*x* 1)) (f))` is tail position for the *value* but not
  for the dynamic environment. For the rest simply to limit surface: an
  unthreaded form produces no marker, so omitting one is safe, never wrong.
  The implicit block *is* threaded (every DEFUN has one, so excluding it would
  mean Step 4 never fires for a named function); sound because the block's
  extent genuinely ends when the tail call is taken.

### Step 6 — IMPLEMENTED 2026-08-31. Frames per level 5 → 2, depth 149 → 372

Both targets landed in `evaluation_core.eval`. Measured at the **default**
limit throughout:

| | HEAD | + target 1 | + target 2 |
|---|---|---|---|
| Python frames per Lisp level | 5 | 3 | **2** |
| max **non-tail** recursion depth | 149 | 248 | **372** |
| `make-scaffold-copy` shape at 334 | fail | fail | **OK** |
| max **tail** depth (Step 4) | 186 | unbounded | unbounded |

The repeating unit is now just `eval` + the closure's `call`.

**Target 1 — IF chains resolved in `eval`'s own frame.** A pre-dispatch `while`
loop that only rewrites `form`, so the 740-line ladder needed no
re-indentation. It sits *above* the self-evaluating checks deliberately: a
branch may be absent (`(if nil 1)`, whose value is Python `None` — preserved
exactly, not "improved" to NIL) or a literal, and letting it fall into the
existing normalization is what keeps the semantics identical to `eval_if`'s
instead of duplicating them. `eval_if` remains: the ladder still reaches it for
a malformed IF, and it is still the entry point for callers outside `eval`.
`tail_target` passes through untouched, so a self call in a branch is still a
Step 4 tail call.

**Target 2 — explicit continuation stack for argument evaluation.** A `pending`
list of `(func, vals, remaining_forms, env, tail_target)` records, looping in
one `eval` frame. Eligibility is decided by `_inline_user_callee`, which is
deliberately narrow: a plain call to a **user-defined** closure (identified by
the `__lisp_lambda_list__` marker only `make_ordinary_function` sets), not a
macro, not a special form, operator a symbol with a function binding. Anything
else falls back to the recursive path, so the change alters *how many host
frames* a form costs, never *which semantics* apply to it — which is what keeps
it from becoming a second evaluator.

Note `_registry.special_registry` does **not** contain every ladder branch:
`THE`, `LOCALLY` and `LOAD-TIME-VALUE` are absent (verified by enumeration), so
`_NOT_INLINABLE_OPERATORS` names them. The user-closure requirement already
excludes them, but the guard should not depend on that coincidence.

**Invariants, each verified by direct evaluation rather than assumed:**

| invariant | probe | result |
|---|---|---|
| left-to-right order | `(list (p 1) (p 2) (p 3))` with a side-effecting `p` | `(1 2 3)` |
| argument is single-value | `(list (values 1 2 3))` | `(1)` |
| top level keeps all values | `(multiple-value-list (values 1 2 3))` | `(1 2 3)` |
| dynamic binding live in callee | `(let ((*dv* 42)) (rd))` | `42` |
| handlers see the signal | `(handler-case (list 1 (error "boom") 3) (error () :caught))` | `:CAUGHT` |
| RETURN-FROM through an argument | `(block b (list 1 (return-from b :escaped) 3))` | `:ESCAPED` |
| THROW through an argument | `(catch 'tg (list 1 (throw 'tg :thrown) 3))` | `:THROWN` |

The non-local-exit case is *structurally* safer than the recursive version: the
whole pending stack lives in one Python frame and dies with it, so unwinding
past suspended arguments abandons them by construction rather than relying on a
`finally` to discard them.

`STORAGE-CONDITION` is untouched and still raised by the depth budget for
genuine exhaustion — the shape at 400 levels signals it correctly.

**Verified:** pytest **2102 passed / 3 xfailed** (the three pre-existing,
unrelated); `tests/test_recursion_depth.py`'s 334-level test is now a real
passing test with its strict xfail removed. Targeted sweep of cons +
data-and-control-flow + eval-and-compile + iteration + conditions + objects
after target 1: **6002 passed, 4 failed, 0 unaccounted**, and all four
(`CONDITION-16/17/18-REPORT.1`, `DEFGENERIC.30`) are pre-existing entries in
`ansi_results/failed.txt`.

**A full run is mandatory before this moves any scoreboard** — it changes the
argument-evaluation path of every user function call.

<details>
<summary>Original Step 6 plan (kept for the frame census and invariant list)</summary>

**The complete solution, per the maintainer's direction: audit the evaluator's
recursive descent and convert every depth-proportional host-stack path into an
explicit continuation stack or trampoline.** `STORAGE-CONDITION` stays as the
condition for genuine implementation-resource exhaustion — it is *not* to be
downgraded to an `ERROR` to make RT swallow it. (RT guards each test with
`handler-bind ((error ...))` only — rt.lsp:291 — and `storage-condition` is a
`SERIOUS-CONDITION`, not an `ERROR`; ansi-test additionally **pins** its class
precedence list in three places, `conditions/condition.lsp:36`,
`types-and-classes/class-precedence-lists.lsp:76` and
`auxiliary/ansi-aux.lsp:606`, so making it an `ERROR` subtype is not available
even as a shortcut.)

**Frame census of one non-tail level** (measured; probe recorded in the session
log). Baseline below the outermost activation is only 3 frames, so depth is
`~750 / frames-per-level`:

| # | frame | role |
|---|---|---|
| 1 | `eval` :1160 | dispatches IF → calls `eval_if` |
| 2 | `eval_if` :144 | evaluates chosen branch → calls `eval` |
| 3 | `eval` :1648 | evaluating an **argument** → calls `eval` |
| 4 | `eval` :1676 | `func(*eval_args)` → calls `call` |
| 5 | `call` :1860 | evaluates body form → calls `eval` |

**5 frames/level → 149 levels.** Targets, in order:

1. **Resolve IF chains in `eval`'s own frame** (collapses 1+2+3 into one
   frame): a small pre-dispatch loop that only rewrites `form`, so the 740-line
   ladder needs no re-indentation. Expected 5 → 3, depth ≈ 250.
2. **Explicit continuation stack for argument evaluation** (removes frame 3
   for nested calls): a work stack of pending
   `(func, args_so_far, remaining_arg_forms, env)` records, looping in one
   `eval` frame. Expected 3 → 2, depth ≈ 375 — which clears the 334 the suite
   demands.

**Invariants the conversion must preserve** (each is observable, and each has
ansi-test coverage):

- **left-to-right argument evaluation** — `SETF.ORDER.*`, `*.ORDER.1` tests
  throughout;
- **multiple values** — an argument is a single-value context and reduces to
  its primary (`lisptype.primary_value`), while a tail position passes all
  values through;
- **dynamic bindings** — a pending continuation must not outlive the
  `BindingFrame` whose bindings its remaining subforms will read;
- **handlers and restarts** — `signal_condition` walks `state.handler_stack`
  *at the signal point*, before unwinding, so a continuation record must not
  move the signal point relative to the establishing forms;
- **non-local exits** — `ReturnFromException` / `ThrowException` /
  `GoException` must still traverse the work stack and unwind its pending
  records; a `finally` that pops the stack is not enough if it does not also
  discard records belonging to the abandoned branch.

**Regression test already in place:**
`tests/test_recursion_depth.py::TestNonTailRecursionDepth::test_non_tail_recursion_at_334_levels`
is `xfail(strict=True)` with this step named as the reason — it will fail
loudly the moment Step 6 makes it pass, which is the signal to delete the
marker.

</details>

### Two process defects found alongside it (neither fixed)

1. **The watchdog resets its own timer, so its hard stop can never fire.**
   `watchdog.arm(warn_after=120, kill_after=900)` measures time without
   *output*; its own 120 s warning prints an all-threads traceback dump to
   stderr, which counts as output. Every warning is therefore followed
   immediately by `RESOLVED: progress resumed`, observing its own dump. A
   genuine wedge would hang forever while printing reassuring lines every two
   minutes. Fix: exclude the watchdog's own writes from the progress signal.
2. **A slow region that reads as a wedge.** Around the `TYPEP.n` tests the log
   can sit unchanged ~10 minutes inside
   `typespec` `intersect`/`_parse_compound`/`type_contains` under the random
   type tests (219 iterations in 120 s). It resolves on its own. Not a hang;
   don't kill a run for it.

### Rejected approach (do not re-propose)

Running the full suite through a wrapper that raises the recursion limit (a
`scripts/run_full_suite.py` calling `run_with_deep_stack`) was implemented and
**rejected by the maintainer**: the suite is to be driven by raw Lisp through
`run_all_tests.py`, unmodified, which is *why* that file is not to be changed.
A bigger stack hides the defect rather than fixing it; the interpreter must
complete `doit.lsp` at the default limit. The wrapper has been deleted.
