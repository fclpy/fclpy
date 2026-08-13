# fclpy — ANSI Compliance Plan

**Goal:** take existing, unmodified ANSI Common Lisp source and run it correctly.
A passing scoreboard is the instrument, not the objective.

**This document is organized around what is still broken**, ranked by the
*mechanism* at fault rather than by test count. It replaced a chronological plan
whose eight stacked "Update" sections had become longer than its content; that
history is preserved in condensed form in [Changelog](#changelog).

> ### 📋 This plan observes `docs/ansi_checklist.md`
>
> That generated file is **the authority for what is failing and where** — all
> 13076 failures grouped directory → file, each with the command to re-verify it.
> This plan supplies the *why* and the *order*; the checklist supplies the *what*
> and the *where*.
>
> **When they disagree, the checklist is right.** It is regenerated from RT's own
> output; prose here ages. Regenerate it after every run
> (`scripts/ansi_checklist.py`), never hand-edit it, and diff it against
> `docs/ansi_checklist_baseline.json` after every change — see
> [the development loop](#the-development-loop) step 6 and
> [preventing regression](#preventing-regression).

---

## 1. Status

**First complete run in the project's history: 2026-08-12.**

```
COMPLETENESS: total=22036 passed=8960 failed=13076 accounted=22036 missing=0 extra=0
COMPLETENESS: OK
```

| | value |
|---|---|
| Registered tests | 22036 |
| Executed (`accounted`) | **22036 (100%)** |
| Passed | **8960 (40.7%)** |
| Failed | 13076 |
| Never executed | **0** |
| Wall time | ~7.5 hours |

**These numbers are the last full run and move only on a full run.** The
*checklist* is kept current between full runs by merging targeted runs into it
(see [below](#keeping-the-checklist-current-without-a-full-run)); its header
lists which runs it has been amended with. Do not copy an amended count here.

**This is the first trustworthy scoreboard.** Every previous version of this
document ranked work using a sample of roughly a third of the suite, and said so.
That constraint is now gone — and the complete data **reordered the priorities
substantially**, exactly as the sampling-artifact warning in [§6](#6-the-two-dimensions)
predicted it would.

**What the completion changed.** Comparing against the last truncated run
(`accounted=8971 passed=4514`): passed nearly doubled to 8960, and the failure
count rose to 13076 because **13065 tests that had never executed now run**. That
rise is not a regression — those tests had no prior status to regress from. It is
previously-invisible failure becoming visible, which is what M0 existed to
achieve.

**The single biggest surprise:** `FORMAT`/`FORMATTER` is now the **largest
failing cluster in the suite at 1623 failures** — 3.6× LOOP, which the truncated
data had ranked first. It was invisible in every prior run because `printer/`
never executed.

### Per-directory scoreboard (complete)

| directory | passed | failed | total | pass rate |
|---|---|---|---|---|
| (programmatically generated) | 1075 | **3908** | 4983 | 21.6% |
| sequences | 990 | **2168** | 3158 | 31.3% |
| cons | 580 | **1058** | 1638 | 35.4% |
| arrays | 520 | 725 | 1245 | 41.8% |
| printer | 137 | 651 | 788 | **17.4%** |
| objects | 215 | 610 | 825 | 26.1% |
| numbers | 872 | 566 | 1438 | 60.6% |
| iteration | 366 | 472 | 838 | 43.7% |
| data-and-control-flow | 1007 | 413 | 1420 | 70.9% |
| strings | 113 | 388 | 501 | 22.6% |
| streams | 161 | 382 | 543 | 29.7% |
| types-and-classes | 283 | 262 | 545 | 51.9% |
| packages | 108 | 232 | 340 | 31.8% |
| conditions | 116 | 187 | 303 | 38.3% |
| pathnames | 79 | 136 | 215 | 36.7% |
| reader | 29 | 136 | 165 | **17.6%** |
| environment | 67 | 125 | 192 | 34.9% |
| misc | 618 | 122 | 740 | 83.5% |
| characters | 156 | 103 | 259 | 60.2% |
| structures | 14 | 101 | 115 | **12.2%** |
| eval-and-compile | 224 | 94 | 318 | 70.4% |
| hash-tables | 89 | 69 | 158 | 56.3% |
| files | 23 | 64 | 87 | 26.4% |
| system-construction | 11 | 64 | 75 | **14.7%** |
| symbols | 1105 | 40 | 1145 | **96.5%** |

The spread is the useful signal: `symbols` at 96.5% and `misc` at 83.5% against
`structures` at 12.2%, `system-construction` at 14.7%, `printer` at 17.4%, and
`reader` at 17.6%. **The four worst are all subsystems where one absent mechanism
fails everything downstream of it** — which is what makes them the cheapest wins,
not the hardest problems.

---

## 2. How to work

### The development loop

**`docs/ansi_checklist.md` is the authority for what is broken.** This plan
explains *why* and *in what order*; the checklist says *what and where*. When the
two disagree, the checklist is right — it is regenerated from RT's own output,
whereas prose in this document ages.

1. **Open `docs/ansi_checklist.md`** and pick a **cluster** — a file or group of
   files sharing a mechanism. Never pick a test.
2. Reproduce it in the smallest expression that shows the defect.
3. Fix the **mechanism**. Consolidate onto an existing helper if one exists.
4. **Verify with the targeted command printed next to that checklist entry.**
5. Run `pytest` for regressions.
6. **Fold that targeted run into the checklist — every time progress is made.**
   ```powershell
   pipenv run python scripts/run_ansi.py iteration --update-checklist
   ```
   This is not optional bookkeeping. `docs/ansi_checklist.md` is declared *the
   authority for what is failing*, and an authority that is only refreshed by a
   4+ hour run is stale the moment anyone fixes anything — at which point every
   later decision is made against numbers that are quietly wrong.
7. **Diff against the baseline** to classify the fix:
   ```powershell
   pipenv run python scripts/ansi_checklist.py --baseline docs/ansi_checklist_baseline.json
   ```
   Every file you did not touch must show no `+N REGRESSION`.
8. Run the full suite only to move the official scoreboard or close a milestone —
   then refresh the baseline with `--save-baseline`.

**The step that matters most is 7.** A fix that moves only the files you targeted
is a symptom fix; a fix that moves files you did not target is a mechanism fix.
The checklist diff is the instrument that tells you which one you just did.

### Keeping the checklist current without a full run

`ansi_results/*.txt` is written by the full runner, so on its own the checklist
could only ever be regenerated from a 4+ hour run. `run_ansi.py` closes that gap:

```powershell
# run a target and amend the checklist with its outcome in one step
pipenv run python scripts/run_ansi.py iteration --update-checklist

# every run writes its results anyway, so it can also be merged after the fact
pipenv run python scripts/run_ansi.py numbers/sqrt.lsp
pipenv run python scripts/ansi_checklist.py --merge ansi_results/targeted-last.json
```

Merging updates the status of **exactly the tests that ran** and leaves every
other test at whatever the last full run said. The checklist therefore reads as
"the last full run, amended with every targeted run since", which is what it
needs to be to stay usable between full runs.

Three rules that keep this honest:

- **A merged total is an index, not a scoreboard.** A targeted run can register
  a slightly different test set than the full run does (load-time-generated
  tests, aux files loaded in a different order). Move the official number in
  [§1](#1-status) only from a full run.
- **`--save-baseline` is full-run-only.** The baseline is the regression gate;
  refreshing it from a partial run erases the data for every file that run did
  not load.
- **The amendment log is provenance, not decoration.** Each merge appends to
  `ansi_results/merges.log`, the checklist header lists the merges it contains,
  and `run_all_tests.py` deletes the log because a full run supersedes them all.
  If the header says "amended by 6 targeted runs", the numbers below it have not
  been independently confirmed together.

`--update-checklist` refuses to merge a run that reported `unaccounted` tests: a
run that aborted partway has no opinion about the tests it never reached, and
recording one would mark them as still-failing on no evidence.

```powershell
pipenv install --dev                                              # one-time
pipenv run pytest -q                                              # ~15s
pipenv run python scripts/run_ansi.py --list                      # available groups
pipenv run python scripts/run_ansi.py iteration                   # one group
pipenv run python scripts/run_ansi.py numbers/sqrt.lsp            # one file
pipenv run python run_all_tests.py > run_all_tests.log 2> run_all_tests.err   # 4+ HOURS
```

**Do not use the full suite to check a fix.** `scripts/run_ansi.py` loads
`gclload1.lsp` (the harness alone: RT, `CL-TEST`, `ansi-aux`, `universe.lsp`)
plus only the files you name, so `(do-tests)` runs exactly what is registered —
no selection logic and no second harness to drift. It reports from RT's own
`*passed-tests*`/`*failed-tests*`, identically to `run_all_tests.py`, so numbers
are directly comparable, and it repeats that runner's completeness check so a
partial run cannot read as a clean one. A directory argument resolves to that
directory's own `load.lsp`; a single-file target first evaluates the
`(compile-and-load* "...-aux.lsp")` preamble from its directory's `load.lsp`,
without which the failures are harness artifacts rather than defects.

### Checklist discipline — the failure mode this section exists to prevent

A checklist invites working failures top-to-bottom. **That is the wrong mode and
it will waste the project's remaining budget.** With roughly half the suite
failing, the binding constraint is a small number of core mechanisms.

- **A fix that moves one checklist line is almost certainly the wrong fix.**
- After each fix, ask *which failures disappeared that you did not target*. That
  number — not the number you aimed at — measures whether you fixed a mechanism
  or a symptom.
- A test that passes for the wrong reason is not progress.
- Per-test work becomes correct only in [Tier 3](#tier-3--the-genuine-tail).

### The checklist artifact

**`docs/ansi_checklist.md`** is the working checklist: all 13076 failures grouped
**directory → file**, ordered by failure count, each with a checkbox and the
exact command to re-verify it. Generated, never hand-edited.

```powershell
pipenv run python scripts/ansi_checklist.py                # regenerate
pipenv run python scripts/ansi_checklist.py --detail       # + every failing test name
pipenv run python scripts/ansi_checklist.py --dir sequences
# mark progress against a saved snapshot (shows -N fixed / +N REGRESSION per file)
pipenv run python scripts/ansi_checklist.py --baseline docs/ansi_checklist_baseline.json
```

It reuses `ansi_score.py`'s `(deftest ...)` scan rather than re-implementing it —
two copies of that mapping would drift, and a checklist that disagrees with the
scoreboard is worse than none (standing rule 3).

**Two properties to understand before trusting a number in it:**

1. **It is an index, not a count.** 3908 failures have no literal `(deftest ...)`
   form because they are generated at load time — `cons/cxr.lsp` builds 40 of its
   own tests with ``(eval `(deftest ,(intern ...)))``. Those are **still reached
   by targeted runs**; only the static attribution misses them. Consequence:
   `run_ansi.py <file>` normally reports *more* registered tests than the
   checklist attributes to that file (cxr.lsp: 176 vs 136). **The targeted run is
   the authority for a file.**
2. **Order is by cluster size, not by priority.** A file with 90 failures is
   nearly always one missing mechanism, not 90 bugs.

### Other analyses

```powershell
sed 's/\.[0-9].*$//' ansi_results/failed.txt | sort | uniq -c | sort -rn | head -40   # by operator
pipenv run python scripts/ansi_score.py                                              # per-subsystem
grep -a -o "Python error in [^\"]\{0,70\}" run_all_tests.log | sort | uniq -c | sort -rn | head
grep -a -o "Undefined function[: ]*[A-Z0-9-]*" run_all_tests.log | sed 's/.*[: ]//' | sort | uniq -c | sort -rn | head
```

### Standing rules

1. **Never implement a test. Implement the mechanism the test checks.**
2. **Any Python object appearing as a Lisp value is a bug** — including
   exceptions (`TypeError`, `FileNotFoundError`, `RestartException`) surfacing as
   test results. There are currently **~1600 such leaks** ([X1](#x1-python-exceptions-leaking-as-lisp-values)).
3. **When two implementations of one operator exist, delete one.** Duplication is
   how fixes silently fail to apply.
4. **Never `pass`, never `return form` in a bare `except`, never "silently
   accept."** Raise `LispNotImplementedError`. A loud gap is measurable; a silent
   wrong answer is not, and it makes the log *undercount* what is missing.
5. **Temporary shortcuts go in [§5](#5-known-temporary-deviations)** with the
   milestone that removes them. Untracked shortcuts are how "mostly compatible"
   happens.
6. **Check which dimension you are starving** ([§6](#6-the-two-dimensions)).
   Failure counts keep pointing at semantics long after semantics stops being the
   constraint.
7. No debug `print()` in committed code. Commits are the maintainer's call.

---

## 3. The checklist

Ranked by evidence from the **complete** run. **Tier 1 items are core
mechanisms**: each is one defect behind many failures. The *command that produced
each number* is given so it can be re-derived rather than trusted.

### Cluster sizes (complete data, 13076 failures)

```powershell
sed 's/\.[0-9].*$//' ansi_results/failed.txt | sed 's/\.ERROR.*$//' | sort | uniq -c | sort -rn | head -40
grep -cE "^FORMAT" ansi_results/failed.txt        # aggregate a family
```

| cluster | failures | % of all failures | one mechanism? |
|---|---|---|---|
| **`FORMAT` + `FORMATTER`** | **1623** | 12.4% | yes — one directive engine |
| **Sequence functions** (`SORT`/`MERGE`/`CONCATENATE`/`FIND`/`POSITION`/`COUNT`/`REMOVE`/`SUBSTITUTE`) | **1266** | 9.7% | largely — `:test`/`:key` + designators |
| **`DEFSTRUCT` family** | **944** | 7.2% | yes — one macro generates nothing |
| **Set/list operations** | **598** | 4.6% | largely — shared `:test`/`:key` |
| **Arrays** (`MAKE-ARRAY`/`ADJUST-ARRAY`/`VECTOR-PUSH`/`SIMPLE-ARRAY`) | **574** | 4.4% | yes — array object model |
| **`LOOP`** | **450** | 3.4% | yes — `for var =` driver |
| **`PRINT*`** | **442** | 3.4% | yes — printer control variables |
| **CLOS** (`DEFGENERIC`/`DEFMETHOD`/`DEFCLASS`/`SHARED-INITIALIZE`/`CHANGE-CLASS`) | **291** | 2.2% | no — two implementations |
| **`OPEN`** | **193** | 1.5% | yes — stream/file model |
| **`SUBTYPEP`** | **156** | 1.2% | yes — no type lattice |

Those ten account for **~6537 failures, half the total**, across **nine
mechanisms**.

### Files failing 100% — the strongest mechanism-absent signal

**23 files fail every single test they contain (592 tests).** A file at 100% is
qualitatively different from a file at 60%: it means the operator is not merely
buggy, it is **absent or fundamentally broken**, so nothing downstream of it can
pass. These are the cheapest wins in the suite and the clearest evidence that
counting individual tests is the wrong lens.

```powershell
# regenerate this list
python -c "import re;[print(m.group(1),m.group(2)) for l in open('docs/ansi_checklist.md',encoding='utf-8') for m in [re.match(r'^- \[ \] .(\S+). — \*\*(\d+)\*\* failing of \2$',l)] if m]"
```

| tests | file | mechanism absent | cluster |
|---|---|---|---|
| 56 | `conditions/define-condition.lsp` | `DEFINE-CONDITION` creates no class | C9 |
| 47 | `iteration/loop6.lsp` | LOOP driver | C1 |
| 35 | `iteration/loop7.lsp` | LOOP driver | C1 |
| 30 | `packages/make-package.lsp` | package model | C10 |
| 30 | `cons/pushnew.lsp` | place protocol | C16 |
| 29 | `hash-tables/make-hash-table.lsp` | `:test` as a designator | X2 |
| 27 | `reader/read.lsp` | reader | C12 |
| 27 | `reader/read-preserving-whitespace.lsp` | reader | C12 |
| 27 | `packages/defpackage.lsp` | package model | C10 |
| 26 | `objects/defmethod.lsp` | CLOS | C8 |
| 25 | `streams/with-input-from-string.lsp` | string streams | C11 |
| 25 | `printer/format/format-logical-block.lsp` | pretty printer | C2 |
| 25 | `printer/format/format-e.lsp` | `~E` directive | C2 |
| 24 | `streams/peek-char.lsp` | stream protocol | C11 |
| 22 | `pathnames/make-pathname.lsp` | pathname model | C11 |
| 20 | `printer/print-cons.lsp` | printer | C7 |
| 19 | `printer/format/format-s.lsp` | `~S` directive | C2 |
| 18 | `sequences/search-vector.lsp` | sequence + designators | C3 |
| 17 | `printer/pprint-exit-if-list-exhausted.lsp` | pretty printer | C2 |
| 17 | `cons/ldiff.lsp` | `LDIFF` absent | C19 |
| 16 | `streams/with-output-to-string.lsp` | string streams | C11 |
| 15 | `printer/format/format-a.lsp` | `~A` directive | C2 |
| 15 | `environment/trace.lsp` | `TRACE` absent | C18 |

**`conditions/define-condition.lsp` at 56/56 is the single best cost/benefit
entry in the suite** — one macro, already diagnosed in [C9](#c9-conditions-restarts-and-define-condition),
no architectural prerequisite, and it also unblocks every user-defined condition
type in the `conditions/` directory's other files.

### Cross-cutting root causes

These are not clusters — they *inflate* the clusters above, so fixing them moves
many rows at once. Fix these first within any cluster you pick up.

#### X1. Python exceptions leaking as Lisp values

**Evidence** (`grep -a -c` on the run log): **889** `Undefined function`, **531**
`Unbound variable`, **530** `Python error in`, 18 `AttributeError`.

Raw Python exception text is returned *as the value of a Lisp form*, violating
standing rule 2. Worse, it makes the suite report a **wrong value** where it
should report an **unimplemented feature**, so the log systematically
*undercounts* what is missing. Sampled shapes:

| occurrences | leak |
|---|---|
| 82 | `FileNotFoundError: File not found: ...` |
| 76 | `FileExistsError: File exists: ...` |
| 18 | `RuntimeError: CALL-NEXT-METHOD: No next method available` |
| 10 each | `ValueError: math domain error`; `RestartException: Restart: FOO`; `NameError: Class not found: ARITHMETIC-ERROR`; `AttributeError: Slot A not found`; `OSError: Cannot open ...` |
| 6 | `RecursionError: maximum recursion depth exceeded` |
| 5 | `AttributeError: 'lispCons' object has no attribute 'remove'` |
| many | `NameError: Class not found: STRUCT-TEST-nn` |

Each row is a different underlying gap, but the *leak* is one mechanism: the
boundary that should convert a Python exception into a signaled Lisp condition.
**Owner:** M8's raise-site migration ([C8](#c8-conditions-restarts-and-define-condition)).

#### X2. Function designators are not resolved

**Evidence.** The most common "undefined functions" are single letters — `X`
(336), `S` (296), `OS` (192), `A` (166), `IS` (118), `S1` (94). These are
*variables holding functions*, not missing standard functions: designator
coercion is absent. **Finding J with over 1200 occurrences attached.**

Every site accepting a function designator (`:test`, `:key`, `FUNCALL`, `APPLY`,
`MAP*`, `SORT`, `REDUCE`) needs **one shared coercion**, not a local `callable()`
check. This is a large fraction of the 1266-failure sequence cluster and the
598-failure set-operation cluster. **Owner:** M3/M6 boundary.

#### X3. Reversed `:test` argument order

`SequenceIterator.matches` (`sequences_search.py`) calls the test with its
arguments reversed; CLHS 17.2.1 requires `(funcall test item element)`. One fix
affecting `FIND`/`POSITION`/`COUNT`/`REMOVE` and plausibly the whole set-operation
family. Pinned by a unit test asserting the wrong answer — see
[§3 non-ANSI assertions](#known-non-ansi-assertions-in-the-unit-suite).

### Tier 1 — core mechanisms (do these first)

#### C1. LOOP clause composition — **DONE (2026-08-12 f)**

`iteration/` **371 → 409 passing of 843** on `run_ansi.py iteration`: 38 tests
fixed, 0 regressions. Only 12 of the 38 were targeted; the other 26 moved
because the mechanism moved. Details in the [Changelog](#changelog).

**The diagnosis this section carried was wrong, and the way it was wrong is
worth keeping.** It read the six-expression table below as *two* defects — a
binding-environment defect ("the variable is bound where the driver sees it but
not where the body does") plus a separate `repeat`-folding defect. Both rows are
the same defect, and it is neither of those: `eval_loop` held a single scalar
`iteration_type` and had **nine near-duplicate iteration engines** selected by
it, so the *last* iteration-control clause parsed decided which engine ran and
silently discarded every other clause.

| expression | expected | before | why |
|---|---|---|---|
| `(loop for x = 7 repeat 5 collect x)` | `(7 7 7 7 7)` | `Unbound variable: X` | REPEAT parsed last → REPEAT's engine ran → no driver ever bound X |
| `(loop repeat 3 for x = 9 collect x)` | `(9 9 9)` | hangs | FOR parsed last → for-equals' engine ran → nothing bounded it |
| `(loop for x = 1 repeat 3 count t)` | `3` | correct | the control: REPEAT's engine is correct as long as nothing *references* the discarded driver |

Reading the control row as "the binding environment is wrong" was the error.
The variable was not mis-bound; **the clause that binds it was not executed at
all.** The fix is one engine over a list of composing drivers (CLHS 6.1.2), not
a repair to any binding form — which matters, because "fix the binding
environment" would have been a local patch to the environment model, exactly
what [§4's note on M2](#recommended-order) warns produces a seventh incompatible
mechanism.

**Evidence at the time.** 450 failures (`grep -cE "^LOOP" ansi_results/failed.txt`);
`for var =` appears 2784 times across 260 test files, so the blast radius extends
well beyond `iteration/` into every directory that uses LOOP to drive its own
assertions.

**This was never a performance problem.** Measured: `(loop for i below 400)` runs
at **2.5 µs/iteration**, and every shape tested scales **linearly**, ×1.9–2.1 per
doubling. The ~3h18m of a 4h20m run charged to ~31 LOOP forms (10 aborted at the
600s cap, 21 warning past 120s) was loops that *never terminate* —
`SQRT.12`–`.17`, `DEPOSIT-FIELD.1`–`.5`, `DPB.2`. `run_ansi.py iteration` now
completes in **6s**.

**Still open in LOOP** — see [Discovered issues](#c1-follow-ups-still-open):
`INTO` for multiple destinations of mixed type, `WITH`, `NEVER`, `MAXIMIZE`/
`MINIMIZE`, `OF-TYPE`, and full clause-order execution for the body itself.

##### C1 follow-ups, still open

Now that LOOP has one engine, these are additions to it rather than repairs of
it — but each is a *clause*, so none of them justifies a second engine.

- **Clause-order execution for the body.** WHILE/UNTIL now record their position
  and run before or after the body accordingly, which is what `collect x until x`
  needs. Body forms, WHEN/UNLESS conditionals and accumulations are still
  executed in bucket order, so a loop that interleaves them (`when a collect b
  when c collect d`) still applies the wrong conditionals. Making the whole main
  clause list ordered subsumes the pre/post flag.
- **`INTO` of mixed types into one destination.** Each destination's accumulator
  is typed on first use, so `collect a into x sum b into x` is wrong. ANSI
  forbids incompatible types here, so the fix is to *detect and signal* it.
- **`WITH`, `NEVER`, `MAXIMIZE`/`MINIMIZE`, `OF-TYPE`, `AND`-joined parallel
  clauses.** Unimplemented; their tokens are dropped.
- **The last silent path.** An unrecognized loop keyword is still discarded once
  a driver has been parsed (standing rule 4). It was left alone deliberately —
  turning it loud converts a large number of currently-wrong answers into
  errors at once, which should be its own measured change, not a rider on this
  one.

#### C2. `FORMAT` / `FORMATTER` — **largest cluster in the suite**

**Evidence.** **1623 failures** (`grep -cE "^FORMAT" ansi_results/failed.txt`),
of which 638 are `FORMATTER`. `printer/` overall is **137 passing of 788 —
17.4%**. Largest sub-clusters: `FORMAT.A` 59, `FORMAT.JUSTIFY` 58,
`FORMAT.LOGICAL-BLOCK` 54, `FORMAT.S` 49, `FORMATTER.A` 48, `FORMAT.R` 38,
`FORMAT.{` 35, plus eight `FORMAT`/`FORMATTER` `^`-directive variants at 39 each.

**Why this was never visible:** `printer/` had never executed in any prior run.
This is the sampling artifact in [§6](#6-the-two-dimensions) made concrete — the
plan warned that ranking by observed failures ranks language-semantics over
ecosystem *by construction*, and the moment the ecosystem directory ran, it took
the top spot.

One mechanism: a real directive engine covering `~A ~S ~R ~D ~B ~O ~X`,
iteration (`~{~}`), escape (`~^`), justification (`~<~>`), conditionals
(`~[~]`), case conversion, and the pretty-printer's logical blocks. `FORMATTER`
is the same engine reached through a macro, so it should not be a second
implementation. **The current implementation is known to contain
`if 'NIL' in part: break`** — a string hack in the directive loop (standing rule
4). **Checklist entries:** `printer/print-array.lsp` 66/67,
`printer/print-vector.lsp` 38/39, **`format/format-logical-block.lsp` 25/25**,
**`format/format-e.lsp` 25/25**, **`format/format-s.lsp` 19/19**,
**`format/format-a.lsp` 15/15**, **`pprint-exit-if-list-exhausted.lsp` 17/17** —
five total failures, i.e. those directives do not work *at all*.
**Owner:** M10. **Verify:** `run_ansi.py printer`.

#### C3. Sequence functions — `:test`/`:key` and designators

**Evidence.** **1266 failures** across
`SORT`/`MERGE`/`CONCATENATE`/`FIND`/`POSITION`/`COUNT`/`REMOVE`/`SUBSTITUTE`;
`sequences/` is 990 passing of 3158 (**31.3%**). Notable: `CONCATENATE` 39,
`MERGE-STRING` 38, `FIND-VECTOR` 36.

This cluster is dominated by the two cross-cutting causes above — **X2**
(designator coercion) and **X3** (reversed `:test` argument order) — rather than
by 40 individually wrong functions. Fix X2 and X3 first, re-run, and re-measure
before touching any individual sequence function. **Owner:** M6.
**Checklist entries:** `sequences/nsubstitute.lsp` 119/145,
`sequences/substitute.lsp` 118/147, `sequences/find.lsp` 115/174,
`sequences/remove.lsp` 102/127, `sequences/position.lsp` 98/154,
`sequences/mismatch.lsp` 95/149. **Six files, one shape** — that is the
signature of a shared defect, and it is why X2/X3 must be measured before any of
these files is opened individually.
**Verify:** `run_ansi.py sequences`.

#### C4. `DEFSTRUCT` generates no accessors and no class

**Evidence.** **944 failures** (`grep -cE "^STRUCT|^DEFSTRUCT|^COPY-STRUCTURE"`).
`structures/` is **14 passing of 115 — 12.2%, the worst rate in the suite**.
`COPY-STRUCTURE` 132, `MAKE-STRUCT-TEST-06` 22, `MAKE-SBT-16` 18, a long tail of
`STRUCT-TEST-nn-ann` accessor names, plus `NameError: Class not found:
STRUCT-TEST-nn` in the leak table.

One mechanism: `DEFSTRUCT` must define the constructor, copier, predicate,
accessors, and a real type/class. Nothing downstream can pass until it does —
which is exactly why the pass rate is 12%. **Owner:** M9.
**Checklist entries:** `structures/structures-03.lsp` 63/64,
`structures/structures-02.lsp` 26/28, `structures/structures-04.lsp` 8/8.
**Verify:** `run_ansi.py structures`.

#### C5. Set and list operations

**Evidence.** **598 failures** — `UNION` 37, `NUNION` 34, `SET-EXCLUSIVE-OR` 30,
`RASSOC` 30, `SET-DIFFERENCE` 26, `MEMBER` 26, `NSET-DIFFERENCE` 25,
`INTERSECTION` 24, `ADJOIN` 24, `NINTERSECTION` 23, `SUBSETP` 21.

Eleven operators failing in near-identical proportion is the signature of **one
shared `:test`/`:key` defect**, not eleven bugs — again X2 and X3. **Owner:** M6.

#### C6. Arrays — fill pointers, adjustability, displacement

**Evidence.** **574 failures**; `arrays/` is 520 passing of 1245 (41.8%).
`MAKE-ARRAY` 47, `SIMPLE-ARRAY` 44, `ARRAY` 41, `VECTOR-PUSH-EXTEND` 39,
`ADJUST-ARRAY` 39, `SIMPLE-ARRAY-T` 34, `ARRAY-T` 34, `MAKE-ARRAY.DISPLACED` 31,
`VECTOR-PUSH` 29, `ADJUST-ARRAY.STRING` 22, `ADJUST-ARRAY.BIT-VECTOR` 22. Also
`IndexError: Expected 2 indices, got 1` in the leak table.

The cluster shape says the array *object model* lacks these properties, rather
than that many functions are individually wrong. **Owner:** M9.

#### C7. The printer — **LARGELY DONE (2026-08-14)**

**`run_ansi.py` over the 25 `printer/` object-printing files: 36 → 128 passing
of 306. +92, zero regressions.** Details in the [Changelog](#changelog).

**The diagnosis this section carried named symptoms, not the mechanism.** It
listed "`PRINC` keeps the `:` on keywords", "`PRIN1` emits C-style escapes",
"`*PRINT-CASE*` returns a Python string" as separate specifics. They are one
defect: **there was no printer.** `lisptype.lisp_str`/`lisp_repr` are `str()` and
`repr()`, so the printed representation of every type lived in that type's
`__str__`/`__repr__` — and a dunder method takes no arguments, so it
*structurally cannot* consult `*PRINT-BASE*`, `*PRINT-ESCAPE*`, `*PRINT-CASE*`,
`*PRINT-LEVEL*` or `*PRINT-LENGTH*`. `PRINC` vs `PRIN1` was `__str__` vs
`__repr__`: two unrelated representations rather than one printer called with
`*PRINT-ESCAPE*` bound differently (CLHS 22.1.3.2).

**And a measurement gate sat in front of all of it,** the same shape as the
string/vector gate in [§4](#recommended-order) item 4: every output function
with no stream argument wrote to Python's `print()` instead of the value of
`*STANDARD-OUTPUT*`. Every `def-print-test` in `printer/` captures output as
`(with-output-to-string (*standard-output*) (prin1 form))`, so **all ~440 of them
saw the empty string no matter what the printer did.** No printer behaviour was
observable until that was fixed.

The control variables were not variables, either: they were Python globals on an
`io_write.PrinterSettings` object reached through `@cl_function('*PRINT-BASE*')`
accessors, which no binding form can assign — and registering a *function* under
a variable's name is what made `*print-base*` evaluate to a **Python function
object** (standing rule 2).

**Still open** — the pretty printer (`*PRINT-PRETTY*`, `PPRINT-*`,
`~<~:>`), `*PRINT-CIRCLE*`, and the array-model items in
[§5](#5-known-temporary-deviations): a bit vector cannot be told from a general
vector, and `MAKE-ARRAY` discards `:element-type`, so `#*1011` and
`(make-array n :element-type 'character)` printing as a string are both blocked
on M9 — that last one is most of what `print-strings.lsp` and `print-array.lsp`
still fail. **Owner:** M10. **Verify:** `run_ansi.py printer/print-cons.lsp` etc.
— note a *whole-directory* printer run cannot complete until the iteration-form
binding leak in [§5](#5-known-temporary-deviations) is fixed.

#### C8. CLOS — `DEFGENERIC` / `DEFMETHOD` / `DEFCLASS` / `CHANGE-CLASS`

**Evidence.** **291 failures** — `DEFGENERIC` 52, `SHARED-INITIALIZE` 41,
`CHANGE-CLASS` 34, `DEFMETHOD` 26, `DEFCLASS` 22, `MAKE-INSTANCES-OBSOLETE` 8 —
plus `RuntimeError: CALL-NEXT-METHOD: No next method available` (18) and
`AttributeError: Slot A not found` (10). `objects/` is 610 failing of 825;
`types-and-classes/` 262 of 545.

**Two CLOS implementations still coexist** (Finding L). Consolidate before
fixing, or fixes will silently fail to apply. **Owner:** M9.

#### C9. Conditions, restarts, and `DEFINE-CONDITION`

**Evidence.** `RESTART-CASE` 27, `RestartException` leaking (10),
`NameError: Class not found: ARITHMETIC-ERROR` (10); `conditions/` is 187 failing
of 303.

The signaling core landed (handlers run at the signal point, before unwinding).
**What remains is one coherent piece:**
- `RestartException` is in none of the evaluator's control-transfer pass-through
  tuples, so `funcall` catches it under `except Exception` and mangles it —
  Finding K's defect class in a fourth place. It then gets handled twice, and the
  restart is gone by the second time.
- `INVOKE-RESTART` calls the restart function and *then* raises; `RESTART-CASE`
  calls it **again** on catching.
- `COMPUTE-RESTARTS` / `FIND-RESTART` / `RESTART-NAME` / `MUFFLE-WARNING` /
  `USE-VALUE` / `STORE-VALUE` / `ABORT` / `CONTINUE` are stubs. RT binds a
  `STYLE-WARNING` muffler around **every test it runs**.
- **`DEFINE-CONDITION` creates no class**, so a user-defined condition degrades to
  the operator's default simple type. The dispatch mechanism is right; user types
  are not in the lattice it dispatches over.
- `_run_handlers_on_unwind` and `_condition_matches`' legacy branch are
  transitional compatibility paths that disappear once raise sites migrate onto
  `SIGNAL` — the same migration as [X1](#x1-python-exceptions-leaking-as-lisp-values).

**Checklist entries:** **`conditions/define-condition.lsp` 56/56** — a total
failure and the best cost/benefit entry in the suite — `restart-case.lsp` 27/37,
`restart-bind.lsp` 17/26, **`check-type.lsp` 9/9**.
**Owner:** M8. **Verify:** `run_ansi.py conditions`.

#### C10. Package model

**Evidence.** `MAKE-PACKAGE` 51, `DEFPACKAGE` 27, `UNUSE-PACKAGE` 23,
`PACKAGE-NAME` 21, `USE-PACKAGE` 20; `packages/` is 198 failing of 340 with 70
still unrun.

Known specifics: `shadowing_symbols`; CL/CL-USER/KEYWORD missing from
`state.packages`; `IMPORT`/`EXPORT`/`RENAME-PACKAGE`; and **`INTERN` case-folds
its string argument** — case conversion is the *reader's* job via
`readtable-case` (CLHS 23.1.2), so `(eq (intern "myvar") (intern "MYVAR"))` must
be NIL. **Owner:** M1. **Verify:** `run_ansi.py packages`.

### Tier 2 — subsystem gaps

These are large but conventional: the mechanism is absent rather than wrong.

| # | cluster | evidence (complete run) | owner |
|---|---|---|---|
| C11 | **Streams, files, pathnames** — `OPEN` 193 (`OPEN` 83, `OPEN.PROBE` 36, `OPEN.OUTPUT` 35, `OPEN.IO` 35); `streams/` 382 failing of 543, `pathnames/` 136 of 215, `files/` 64 of 87, `system-construction/` 64 of 75 (**14.7%**). ~170 `FileNotFoundError`/`FileExistsError`/`OSError` leaks. Gates ASDF and all library loading. | M10 |
| C12 | **Reader** — `reader/` 136 failing of 165 (**17.6%**). `#(1 2 3)` reads as the cons `(VECTOR 1 2 3)` (CLHS 2.4.8.3); the tokenizer interprets `\n` inside strings, where CLHS 2.4.5 requires backslash to be a single-escape included *without interpretation*. **Also: `fclpy/reader.py` is a dead ~480-line second reader** that nothing under `fclpy/` imports, yet **177 unit tests (14% of that suite)** certify it — while the live reader (`tokenizer.py` → `lispreader.py` → `readtable.py`) has essentially no unit coverage, and the two disagree on conformance. Retire it or repoint those tests. | M10 |
| C13 | **Strings** — `strings/` 388 failing of 501 (**22.6%**); `MERGE-STRING` 38. Rooted in the `LispString`/Python-`str` split (Finding I), which also blocks `EQUAL`/`EQUALP`. A length-1 `str` currently satisfies both `CHARACTER` and `STRING`, which are disjoint types (CLHS 4.2.2). | M9 |
| C14 | **Types / `SUBTYPEP`** — `SUBTYPEP` 156 (`SUBTYPEP.INTEGER` 46); `types-and-classes/` 262 failing of 545. `SUBTYPEP` is a string-pair lookup table with no type lattice (Finding F). | M9 |
| C15 | **Numeric tower** — `numbers/` 566 failing of 1438 (60.6% passing — better than most); `PARSE-INTEGER` 49, `ValueError: math domain error` leaks. Bignums, ratios, complex, float contagion. | Phase 4 |
| C16 | **Places / `SETF`** — `PSETF` 31, `PUSHNEW` 27, `ROTATEF` 23. Five parallel place protocols; `GET-SETF-EXPANSION` is a stub returning a Python 5-element list instead of five values; `PUSH`/`POP`/`PUSHNEW` are registered as *functions* over Python lists. No test pins either, so M5 is free to fix them. | M5 |
| C17 | **Lambda lists** — `FLET` 35, `LAMBDA` 22, `DESTRUCTURING-BIND` 22. Six copy-pasted binders (Finding C). | M3 |
| C18 | **Environment / misc** — `environment/` 125 failing of 192; `hash-tables/` 69 of 158; `characters/` 103 of 259. `misc/` is 83.5% passing — leave it alone. | M1 / Phase 4 |
| C19 | **Missing standard functions** — `LDIFF` 38, `TAILP` 20, `CHECK-TYPE` 18, `STREAM-ELEMENT-TYPE` 10, `MAKE-INSTANCES-OBSOLETE` 8. Genuinely absent; cheap; `STREAM-ELEMENT-TYPE` is also the one failing unit test. | M1 |

### Tier 3 — the genuine tail

Correct to work per-test, **but only after Tiers 1–2**:

- **Required error signaling** — the many `.ERROR.n` tests asserting a specific
  condition type for malformed input. Large and mechanical, but if three of them
  want a `PROGRAM-ERROR`, the fix is argument validation in the lambda-list
  engine, not three `raise` statements.
- **Implementation variables** given real values — `*FEATURES*`, `*READ-EVAL*`,
  `*MACROEXPAND-HOOK*`, `*RANDOM-STATE*`, `MOST-POSITIVE-FIXNUM` and friends.
- **Implementation-defined choices** ANSI permits — document each explicitly
  rather than leaving it accidental.
- Remaining sequence, array, string, character, and hash-table edge cases.

### Known non-ANSI assertions in the *unit* suite

A unit test that asserts a bug makes fixing the bug look like a regression.
Verified by execution:

| expression | fclpy | ANSI | pinned by |
|---|---|---|---|
| `(gethash 1.0 h)`, key `1` | `ONE` | `NIL` — `(eql 1 1.0)` is false | `test_phase5_task7_hashtables.py:136` |
| `(hash-table-test h)` | `"<FUNCTION EQUAL AT 0x…>"` | the **symbol** `EQUAL` | `:249` |
| `(find 3 '(1 2 3 4 5) :test #'>)` | `4` | `1` — called as `(funcall test item element)` | `test_phase5_task2_sequence_functions.py:50` |
| `(array-dimension <fill-pointer 5, size 10> 0)` | `1` | `10` | `test_phase5_task3_vectors.py:249` |

Also: **`lisptype.is_truthy(False)` is `True`** — any Python `False` reaching a
Lisp conditional is silently *true*. A live landmine. And tests that cannot fail
(`test_phase3_unwind_protect.py:131`, `test_phase4_multiple_values.py:330`) occupy
the place real coverage should be.

---

## 4. Milestones — re-scoped

Milestones now describe *mechanisms*, and map onto the clusters above.

| | milestone | state | clusters |
|---|---|---|---|
| **M0** | Trustworthy measurement | **DONE** — `COMPLETENESS: OK`, 22036/22036 accounted. Remaining: `expected-failures/` wiring | — |
| **M1** | Symbol, NIL, package identity | canonical CL symbol table **done**; package model outstanding | C10, C18, C19 |
| **M2** | Environment model | **not started** — the spine. Do not fix specials one binding form at a time; that produces a seventh mechanism | C1, X2 |
| **M3** | One lambda-list engine | not started — six copy-pasted binders | C17, X2 |
| **M4** | A real macro system | not started — ~90 standard macros are special forms. **Most ecosystem-critical** | — |
| **M5** | `GET-SETF-EXPANSION` / places | not started — deletes ~600 lines of ladder code | C16 |
| **M6** | Multiple values, sequences | partial | C3, C5, X2, X3 |
| **M7** | Non-local control flow | partial — name-based block/tag matching, no identity objects | — |
| **M8** | Conditions and restarts | **signaling core done**; restart half + `DEFINE-CONDITION` + raise-site migration remain | C9, X1 |
| **M9** | Types, `SUBTYPEP`, CLOS, structures | not started — two CLOS implementations; `SUBTYPEP` is a string-pair table | C4, C6, C8, C13, C14 |
| **M10** | Reader, printer, `FORMAT`, streams, pathnames, loader | not started — **now the largest single body of failures, and gates ASDF** | C2, C7, C11, C12 |

### Recommended order

1. ~~**C1 — LOOP `for var =`.**~~ **Done 2026-08-12 (f).** It did what it was
   ranked first to do: `run_ansi.py iteration` fell from a run dominated by
   never-terminating loops to **6 seconds**, so every later measurement is now
   affordable.
2. ~~**X2 + X3 — designator coercion and `:test` argument order.**~~ **Done
   2026-08-12/13.** `coerce_to_function` exists and the sequence/set operations
   share `_coerce_function_designator` and `_make_matcher`; `sequences` 1087 →
   1296 and `cons` 703 → 838. **Finding J's "there is no `coerce_to_function`"
   is obsolete** — the table in §8 was not updated when the fix landed.
3. ~~**C4 — `DEFSTRUCT`.**~~ **Largely done.** `structures` measures **680
   passing of 1645** on `run_ansi.py structures` — 41%, not the 12.2% in §1.
   The 12.2% was the *static checklist attribution*, which cannot see the
   `STRUCT-TEST-nn` tests generated at load time; the targeted run is the
   authority for a file (§3's property 1).
4. **C13 first, not C2 — the string/character representation.** *(New, and it
   reorders everything below it.)* `EQUAL`, `EQUALP`, `TYPEP` and `CHARACTERP`
   all type-tested `isinstance(x, str)`, which is false for the `LispString`
   the reader produces; `TYPEP` also denied that a string is a `VECTOR`. The
   ANSI harness compares every result with `equalp-with-case`, which walks
   vectors element-wise, so **no test with a string-valued expectation could
   pass regardless of the code under test.** This is the single highest-leverage
   thing found so far: it is not a cluster of its own, it is a *measurement
   gate* in front of `printer`, `strings`, `sequences` and every `.ERROR` test
   that names a message. Partly fixed (see the Changelog); the representation
   split itself remains M9's.
5. **M10's `FORMAT` engine (C2).** The largest cluster at 1623. The iteration,
   escape, justification and padding half is done (Changelog 2026-08-13); `~E`,
   `~F`, `~R`, `~T` and the pretty-printer's logical blocks are not.
6. ~~**C7 — the printer.**~~ **Largely done 2026-08-14.** One printer, control
   variables that are actually variables, and output that goes to
   `*STANDARD-OUTPUT*`. The last of those was a measurement gate in front of
   ~440 `def-print-test`s, so this was item 4's shape a second time.
7. **M2's binding model — now the top item, and it is blocking measurement.**
   All eight iteration forms assign to an enclosing variable rather than binding
   their own ([§5](#5-known-temporary-deviations)). It is a wrong answer in its
   own right, and because `DO-ALL-SYMBOLS`/`LOOP` clobber rt.lsp's report stream
   parameter `s`, **`run_ansi.py printer` cannot run to completion at all.** The
   fix is to extract the special-vs-lexical binding decision out of `eval_let`
   and `eval_letstar` — where it already exists twice — into one binder shared by
   LET, LET*, and the eight iteration forms. That is exactly the M2 slice this
   document has said to do properly rather than one binding form at a time, and
   there is now a concrete, measured reason to do it: the one-word local fix
   moves `iteration` 410 → 408 by breaking the four tests that bind the variable
   specially.
8. **Re-measure, then re-derive this list.** The residual distribution has
   already shifted enough that ranking further ahead is guesswork.

**A note on M2.** It remains the architectural spine, and C1/X2 both bottom out
in it. If fixing C1 and X2 turns into repeated local patches to the environment,
stop and do M2 properly instead — that is exactly the "seventh incompatible
mechanism" this plan has warned about since the beginning.

---

## 5. Known temporary deviations

Anything knowingly non-ANSI, with the milestone that removes it. Empty means
"nothing is knowingly wrong" — keep it honest.

| deviation | why tolerated | removed by |
|---|---|---|
| LOOP: one accumulation destination per *type*; `INTO` of mixed types into one var unsupported | accumulator state is typed on first use | C1 follow-up |
| LOOP `WITH`, `NEVER`, `MAXIMIZE`/`MINIMIZE`, `OF-TYPE` unimplemented; tokens dropped | never written | C1 follow-up |
| LOOP body/accumulation clauses execute in bucket order, not clause order | only WHILE/UNTIL are position-aware so far | C1 follow-up |
| LOOP silently drops an unrecognized keyword once a driver exists | violates standing rule 4 | C1 follow-up |
| `_run_handlers_on_unwind` + `_condition_matches` legacy branch | most raise sites bypass `SIGNAL` | M8 |
| `DEFINE-CONDITION` creates no class | predates the class lattice | M8 |
| `HANDLER-CASE` converts an uncaught `THROW` into `CONTROL-ERROR` | needs a catch-tag stack to decide at THROW time | M7 |
| 114 non-ANSI symbols exported from `CL` | registry auto-export | M1 |
| ~90 standard macros implemented as special forms | predates the macro system | M4 |
| Five parallel place protocols; `GET-SETF-EXPANSION` a stub | predates the setf protocol | M5 |
| Six copy-pasted lambda-list binders | never factored | M3 |
| Two CLOS implementations, two readers, two readtables, dead `reader.py`/`tokenizer.py` fork | historical forks | M9 / M10 |
| Pretty printer absent: `*PRINT-PRETTY*`, `PPRINT-*`, `~<~:>` logical blocks | the printer prints only the non-pretty style | C2 / M10 |
| `*PRINT-CIRCLE*` unimplemented; the printer instead cuts off at depth 256 | needs a labelling pass over the object graph | M10 |
| A bit vector prints as `#(1 0 1 1)`, not `#*1011` | a bit vector and a general vector are both a Python `list` with no recorded element type, so the distinction cannot be recovered at print time | C6 / M9 |
| `(format <string-with-fill-pointer> ...)` works for a `LispString` but signals for the `AdjustableVector` that `(make-array n :element-type 'character :fill-pointer 0)` returns | `MAKE-ARRAY` discards `:element-type`, so a character array is indistinguishable from a general vector; appending characters to one anyway would be a guess | C6 / M9 |
| `~&` sees only the column within its own control string, so a `~&` opening a control string cannot tell the stream is mid-line; `FRESH-LINE` is correct | FORMAT builds its whole output as a string before writing, and the column is not threaded through the eleven nested `_format_process_cursor` call sites | C2 |
| `SUBTYPEP` string-pair table | no type lattice | M9 |
| `LispString` vs. Python `str` split | two string representations | M9 (blocks EQUAL/EQUALP) |
| Name-based block/tag/catch matching | no block identity objects | M7 |
| `is_truthy(False)` is `True` | unaudited boundary | M2 |
| **All eight iteration forms (DO, DO*, DOLIST, DOTIMES, LOOP, DO-SYMBOLS, DO-EXTERNAL-SYMBOLS, DO-ALL-SYMBOLS) assign to an enclosing variable of the same name instead of binding their own** — `(let ((x 99)) (dolist (x '(1 2 3))) x)` is NIL. `(do-all-symbols (s) ...)` therefore clobbers rt.lsp's report stream parameter `s`, and LOOP's `for s = ...` at `print-strings.lsp:149` does the same, which **makes a whole-directory `run_ansi.py printer` run unable to complete** | the one-word fix (`add_variable` for the establishing call) regresses DO.14/DO*.14/DOTIMES.18/.18A, which bind the variable *specially*; needs the special-vs-lexical decision extracted from `eval_let`/`eval_letstar` and shared. Diagnosed, measured (`iteration` 410→408), reverted, and pinned by 13 `xfail`s in `tests/test_iteration_variable_binding.py` | **M2 — next task** |

---

## 6. The two dimensions

"100% ANSI" is **not** primarily an evaluator problem.

| | **A — language semantics** | **B — environment / ecosystem** |
|---|---|---|
| Covers | evaluation, environments, lambda lists, macros, places, values, control flow, conditions, CLOS, types | packages, reader, printer, `FORMAT`, streams, files, pathnames, `LOAD`, `COMPILE`, implementation variables |
| Failure if wrong | code computes the **wrong answer** | code **cannot be loaded at all** |
| Milestones | M2–M9 | M0, M1, M9 (types-as-interface), M10, Phase 4 |

**This warning was proven correct, and the proof is worth keeping.** Earlier
versions of this document said that ranking work by observed failure count ranks
A over B *by construction*, because B directories had never executed. When the
first complete run landed, **`FORMAT`/`FORMATTER` went straight to #1 at 1623
failures** — ahead of every A-dimension cluster — and four of the five worst pass
rates in the suite (`structures` 12.2%, `system-construction` 14.7%, `printer`
17.4%, `reader` 17.6%) are B-dimension. The sampling artifact was real and it had
been distorting this plan's priorities for its entire history.

**The standing implication:** B-work is not "later." M10 is now the largest single
body of failures in the suite *and* the milestone that gates ASDF, i.e. the
ability to load any real library at all.

**Rule of thumb:** when A-work and B-work are both unblocked, prefer whichever
**unblocks measurement or unblocks loading**. Correct semantics for code you
cannot load, and correct loading of code that then misbehaves, are equally
useless.

---

## 7. Acceptance — the ecosystem ladder

The end goal is running unmodified ANSI source, so the real acceptance test is
not the scoreboard. Each rung exercises a different subsystem, so a failure
localizes — **a rung that fails tells you which milestone lied about being
complete.**

| rung | dimension | what passing it proves |
|---|---|---|
| **Alexandria** | A: macros, lambda lists, places, types | the macro system and lambda-list engine are real (M3, M4, M5) |
| **CL-PPCRE** | A: computation, recursion, string/char semantics | evaluator, sequences, string representation are sound (M6, M9) |
| **FiveAM** | A+B: conditions, restarts, CLOS, printer | signaling before unwinding (M8) and CLOS dispatch (M9) work |
| **ASDF** | B: pathnames, streams, `LOAD`, packages | the implementation can *acquire code at all* (M1, M10) |

ASDF is the rung that converts this from "a conforming Lisp" into "a Lisp with an
ecosystem," and **nothing in the ANSI suite tests it.**

### Demonstrating completion

1. `ansi-test` reports **0 unexpected failures**, verified independently by
   `scripts/ansi_score.py` parsing raw output — not by a `FORMAT`-rendered
   summary produced by the implementation under test.
2. Every entry in `expected-failures/` is justified in writing against a CLHS
   citation, or fixed. It must never become a dumping ground.
3. All four ecosystem rungs load and run.
4. A conformance statement documenting every implementation-defined choice.

### Preventing regression

- **The checklist baseline is the regression gate.**
  `docs/ansi_checklist_baseline.json` is a committed `{file: failed_count}`
  snapshot; regenerating with `--baseline` marks any file that got worse as
  `+N REGRESSION`. **A per-file regression is a build failure even if the total
  improved** — a total can hide a mechanism trade where one fix breaks another
  subsystem, which is precisely the failure mode a single scoreboard number
  cannot see. Refresh the baseline only from a full run, never from a targeted
  one (a targeted run has no data for the files it did not load).
- CI runs the full suite; **any increase in failures is a build failure.** Commit
  the scoreboard so deltas are reviewable.
- `pytest` is the fast inner loop; **`ansi-test` is the authority.** When they
  disagree, the unit test is wrong — see the non-ANSI assertions in §3.
- Add a targeted regression test only if `ansi-test` does not already cover it.
- **Guard the architecture, not just behavior:** assert one lambda-list parser,
  one CLOS implementation, one reader, one macro path per operator, and no
  non-ANSI symbols exported from `CL`.

---

## 8. Architectural findings

The structural causes behind the clusters. Retained because each explains *why* a
cluster exists.

| | finding | cluster |
|---|---|---|
| **A** | The `CL` package has no canonical membership | C8 |
| **B** | The standard macros are not macros (~90 are special forms) | M4 |
| **C** | Lambda lists are parsed ad-hoc and incompletely, in six places | C11 |
| **D** | `(declare (special ...))` is not honored | M2 |
| **E** | Conditions are Python exceptions in a trenchcoat | C2, C7 |
| **F** | `SUBTYPEP` has no type lattice | C4, C6 |
| **G** | NIL has three representations | M1 |
| **H** | `GET-SETF-EXPANSION` is decoration; there are **five** place protocols | C10 |
| **I** | One `LispString`/`str` split explains the EQUAL/EQUALP cluster | M9 |
| **J** | ~~There is no `coerce_to_function`~~ — **obsolete, fixed 2026-08-12**; it exists in `evaluation_core.py` and the sequence/set operations share it | C3 |
| **M** | **Python type tests stand in for Lisp type tests.** `isinstance(x, str)` for "is a string", `isinstance(x, (list, tuple))` for "is a list", `callable(x)` for "is a function". Each is false for exactly the Lisp object it is meant to match, so the branch is dead and the code silently takes the wrong path. This is the *same* defect as X2 (designators), X3, `~{`'s cons blindness, and `EQUAL`/`TYPEP`/`CHARACTERP` on strings — **found five times in different subsystems, and it is what a "shared mechanism" audit should grep for first** | X2, C2, C13, C14 |
| **K** | Non-local exits swallowed by bare `except` — **recurs in a new operator each time it is found** (`funcall`, `IGNORE-ERRORS`, `RestartException`) | C2, C7 |
| **L** | Duplicate and dead implementations | C6, C15 |

**A recurring defect class worth naming:** *"the form does not establish the
block/tagbody/condition CLHS says it establishes."* Found seven times so far —
`LOOP`'s implicit NIL block, CLOS method bodies, `LOOP`'s `NAMED` clause,
`ERROR`/`CERROR`'s condition dispatch, `HANDLER-BIND`'s handler environment, and
`DO-SYMBOLS`/`DO-EXTERNAL-SYMBOLS`/`DO-ALL-SYMBOLS`' implicit tagbody. Shared
helpers now exist for both halves (`_run_with_nil_block`, `_exec_iteration_body`)
— **audit every remaining iteration/mapping form against them in one pass**
rather than discovering these one crash at a time.

---

## 9. Key files

| file | purpose |
|---|---|
| `CLAUDE.md` | architecture map — read first |
| `plan.md` | this document |
| `scripts/run_ansi.py` | **targeted runner — the development inner loop**; `--update-checklist` amends the checklist with the run |
| `scripts/ansi_score.py` | per-subsystem scoreboard → `docs/ansi_baseline.json` |
| `docs/ansi_checklist.md` | **the working checklist** — failures by directory → file, with per-entry verify commands |
| `scripts/ansi_checklist.py` | regenerates the checklist; `--merge` folds in a targeted run, `--baseline` marks fixed/regressed per file |
| `ansi_results/failed.txt` | raw RT output — the checklist's input, not a work list |
| `ansi_results/targeted-last.json` | the last targeted run's outcomes, written by every `run_ansi.py` run so it can be merged later |
| `ansi_results/merges.log` | which targeted runs the current checklist has been amended with; cleared by a full run |
| `run_all_tests.py` | full suite (4+ hours) — authority, not inner loop |
| `REPAIR.md` | crash-repair SOP — historical; crashes are no longer the constraint |

---

## Changelog

Condensed from the previous chronological plan. Each entry is a *mechanism*
landed, not a test count.

- **2026-08-14** — **C7: there is one printer, and output goes where the
  language says.** `fclpy/printer.py` — previously a complete printer that
  *nothing under `fclpy/` imported* — is now the only one, and every Lisp-visible
  printed representation comes from it: `PRIN1`, `PRINC`, `PRINT`, `WRITE`, the
  three `*-TO-STRING`s, and FORMAT's `~A`/`~S`. Deleted: `PrinterSettings` and
  its `@cl_function('*PRINT-...*')` accessors, the unreachable
  `_print_with_limits`, the `@cl_function('*STANDARD-OUTPUT*')`-style accessors
  returning raw `sys.stdout`, and `_write_stream_output` (a strictly worse
  duplicate of the new single `write_text` funnel). Three printers → one
  (standing rule 3).
  **The gate mattered more than the printer.** Every output function with no
  stream argument wrote to Python's `print()` rather than to the value of
  `*STANDARD-OUTPUT*`, and every `def-print-test` in `printer/` captures via
  `(with-output-to-string (*standard-output*) (prin1 form))` — so ~440 tests read
  the empty string regardless of the code under test, exactly as the
  string-is-a-vector gate did in the 2026-08-13 entry. `(format t ...)` was the
  same bug with a twist: FORMAT's `t` means `*STANDARD-OUTPUT*` (CLHS 22.3.1),
  not `*TERMINAL-IO*` as it would for a stream designator.
  Landed with it: the control variables are real variables with the ANSI initial
  values from one table (`printer.PRINTER_VARIABLES`, so bootstrap and printer
  cannot disagree — `*PRINT-RIGHT-MARGIN*` and `*PRINT-MISER-WIDTH*` are NIL, not
  80 and 40); `*PRINT-BASE*` 2–36 with upper-case digits and the radix prefix
  before the sign (`#b-1`, `#3r-11`, `10.`); `*PRINT-LEVEL*` applied to
  aggregates only, at `>=`, so an atom is never `#`; ratios as `n/d` and
  complexes as `#C(r i)` instead of Python's `Fraction(1, 2)` and `(1+2j)`;
  vectors as `#(...)` (a Python `list` is a *vector* here, and `str()` printed it
  as a list, so every vector read back as a cons) and arrays as `#2A((0 0) (0 0))`
  instead of `#(ARRAY (2, 2))` — a Python tuple's repr inside claimed Lisp
  syntax; the full CLHS 22.1.3.3.2 `READTABLE-CASE` × `*PRINT-CASE*` matrix;
  WRITE's keyword arguments, which were collected into `**kwargs` and dropped,
  plus `:allow-other-keys`; `PRINT` as newline-object-space rather than the
  reverse; and `FRESH-LINE`/`~&` as actual fresh lines — both had emitted
  unconditionally, `~&` with the comment "we don't track column".
  **Measured: the 25 `printer/` object-printing files 36 → 128 passing of 306,
  +92, 0 newly failing.** `iteration` 409 → 410, no regressions. `pytest` 1457
  passed, 1 pre-existing unrelated failure (`STREAM-ELEMENT-TYPE`), 16 xfailed;
  115 new tests in `tests/test_printer_ansi.py`, and `tests/test_printer.py`'s 7
  non-ANSI assertions corrected with citations (it asserted `PRINC` = `PRIN1` for
  keywords and characters, which pinned the two-representations bug, and `\n`
  escaping inside strings, which CLHS 2.4.5 forbids). Deleted
  `tests/test_printer_control.py`, 273 lines certifying the dead
  `PrinterSettings`/`_print_with_limits` pair — the same pathology as the dead
  `reader.py`'s 177 tests.
  **Discovered, diagnosed, and deliberately not fixed here:** all eight iteration
  forms *assign to* an enclosing variable of the same name instead of binding
  their own, so `(let ((x 99)) (dolist (x '(1 2 3))) x)` is NIL and
  `(do-all-symbols (s) ...)`/`(loop for s = ...)` clobber rt.lsp's own report
  stream parameter — which is why a whole-directory printer run cannot complete.
  See [§5](#5-known-temporary-deviations) and the 13 `xfail`s in
  `tests/test_iteration_variable_binding.py`; it is M2's, and the one-word fix
  regresses the four tests that bind the variable *specially*.
- **2026-08-13** — **A string is a vector, and its elements are characters.**
  Four functions type-tested `isinstance(x, str)`, which is false for every
  `LispString` the reader makes: `EQUAL`/`EQUALP` (so `(equal "abc" "abc")` was
  **NIL**), `TYPEP`'s `STRING` branch, and `CHARACTERP` (which also missed the
  `Character` class *and* returned a raw Python bool, the dangerous direction
  given `is_truthy(False)` is true). `TYPEP` additionally excluded strings from
  `VECTOR`/`ARRAY`, contradicting CLHS 15.1.
  **Why this mattered more than any cluster:** rt.lsp's `equalp-with-case`
  compares via `(typep x 'vector)` and walks elements; with strings not vectors
  it fell through to `EQL`, so *every* string-valued test failed no matter what
  the code under test returned. Fixing `TYPEP` then exposed the second half —
  `AREF`/`LOOP across`/`MAKE-ARRAY` yielded bare length-1 Python strings, so a
  "character" was also a one-element string and therefore a one-element vector,
  and element-wise traversal recursed until the stack died, aborting whole runs.
  String element access now yields `Character` through one shared
  `string_element`. **8 `LOOP.5.*` tests that had been passing via the EQL
  conflation are the proof the old behaviour was wrong, not the fix** — they
  expect `(#\a ...)`; all are passing again for the right reason.
  Also landed: **`WITH-OUTPUT-TO-STRING`/`WITH-INPUT-FROM-STRING`/
  `WITH-OPEN-STREAM` are real macros.** They were `cl_function` stubs that
  returned their last body form unevaluated — and because `cl_function`
  evaluates arguments eagerly, the binding spec `(stream)` was evaluated as a
  call, failing with `Undefined function STREAM`. Every `FORMATTER.*` test in
  the suite is written in terms of `WITH-OUTPUT-TO-STRING`, so this alone gated
  the 638 `FORMATTER` failures. **Three registrations of each existed**
  (`misc_macros.py`, `io_read.py`, `io_write.py`) and the undecorated ones would
  still auto-register via `register_module` because its dedup is by *Python*
  name; two deleted (standing rule 3).
  And **C2's iteration half**: `~{...~}`/`~?` tested `isinstance(arg, (list,
  tuple))` — false for the cons list the directive exists to iterate — so
  `(format nil "~{~A ~}" '(1 2 3))` returned `"(1 2 3) "`. `~^` escaped
  unconditionally instead of testing its CLHS 22.3.9.2 condition, and signalled
  via an in-band `' '` marker callers had to `str.replace` out; it is now a
  control transfer carrying its partial output. `~<...~>` processed only its
  last segment (no justification ever happened); `~A`/`~S` honoured only
  `mincol` with a hardcoded space; `~n[`/`~#[` ignored the prefix parameter and
  stole an argument.
  **Measured, before → after on the same seven files:** `format-a` 0→42/107,
  `format-brace` 0→55/152, `format-circumflex` 0→198/470, `format-justify`
  1→22/59, `format-conditional` 3→28/58, `format-question` 0→10/20, `format-s`
  0→33/87 — **4 → 388 of 953 (0.4% → 40.7%), +384, 0 regressions.**
  `iteration` 409 → 409 and `cons` 817 → 838, no regressions. `pytest` 1347
  passed, 1 pre-existing unrelated failure (`STREAM-ELEMENT-TYPE`); 71 new tests
  across `test_format.py`, `test_equality_strings.py`, `test_string_elements.py`.
  Tooling: `run_ansi.py` now collects the `compile-and-load*` preamble from
  *ancestor* directories, without which **no file under `printer/format/` could
  be targeted at all** (they need `def-format-test` from `printer/load.lsp`, one
  level up) — 2 tests registered before, 953 after; and it runs on a big-stack
  thread with a raised recursion limit, since one level of Lisp recursion costs
  ~15 Python frames and rt.lsp's own list comparison cdr-recurses per element.
- **2026-08-12 (f)** — **C1: LOOP has one iteration engine.** `eval_loop` held a
  scalar `iteration_type` and nine near-duplicate loops selected by it, so the
  last iteration-control clause parsed decided which one ran and discarded the
  rest — the cause of both `Unbound variable: X` for `for x = 7 repeat 5` and
  the non-terminating `repeat 5 for x = 7`. All nine are gone; drivers (FOR/AS,
  and REPEAT, now modelled as an anonymous driver) compose in one loop over
  `all(_driver_has_value(...))`, per CLHS 6.1.2. `for var = form` evaluates at
  bind time so a later clause can depend on an earlier driver, WHILE/UNTIL
  compose and are position-aware, and the eight copy-pasted accumulation parse
  branches became one table — which is how `INTO` came to be implemented at all
  (it had been silently dropped in every one of them). Also landed as part of
  the same mechanism: `INITIALLY`, several accumulation clauses per loop, the
  CLHS 6.1.1.4 rule that a FINALLY value overrides an accumulation, and the
  6.1.2.2 rule that ALWAYS/THEREIS skip the epilogue. Fixed en route: a local
  `import fclpy.lisptype` inside `_init_driver` that turned every
  `LispNotImplementedError` in that function into `UnboundLocalError`, and
  `for x to 5`'s start defaulting to `None` instead of 0.
  **`run_ansi.py iteration` 371 → 409 passing of 843, 0 regressions**, and the
  run itself went from LOOP-dominated to **6 seconds**. 13 new tests in
  `tests/test_loop.py`; `pytest` 1259 passed, 1 pre-existing unrelated failure
  (`STREAM-ELEMENT-TYPE`). Tooling: `run_ansi.py --update-checklist` and
  `ansi_checklist.py --merge` now keep `docs/ansi_checklist.md` current from
  targeted runs, so the checklist no longer needs a 4+ hour run to stop being
  stale.
- **2026-08-12 (d)** — `DO-SYMBOLS`/`DO-EXTERNAL-SYMBOLS`/`DO-ALL-SYMBOLS`
  implicit tagbody + NIL block. All three ran their bodies as a flat `eval` over
  the form list, so `(go foo)` raised an uncaught `GoException` to top level,
  aborting the run at `DO-SYMBOLS.8`. **No new helper was written** — the prior
  plan's claim that "the tagbody half has no shared helper yet" was wrong;
  `_exec_iteration_body` already existed and was already used by
  `DO`/`DO*`/`DOLIST`/`DOTIMES`. The fix is those three functions using the
  mechanisms their siblings already use. 7 new tests in
  `tests/test_do_symbols_family.py`; `pytest` 1246 passed, 1 pre-existing
  unrelated failure (`STREAM-ELEMENT-TYPE`). **ANSI impact unmeasured** — the run
  that would measure it was still in flight.
- **2026-08-12 (e)** — **First complete run in the project's history:
  `COMPLETENESS: OK`, 22036/22036 accounted, 0 missing.** M0's central goal, and
  the first trustworthy scoreboard: 8960 passing (40.7%), ~7.5 hours. It
  reordered the checklist immediately — `FORMAT`/`FORMATTER` took first place at
  1623 failures, ahead of LOOP (450), which the truncated data had ranked #1.
- **2026-08-12 (c)** — LOOP `for var = expr` diagnosed as the cause of ~76% of
  full-run wall time; `scripts/run_ansi.py` built; this document restructured
  around the failure checklist.
- **2026-08-12 (b)** — Audit of the *unit* suite for tests asserting non-ANSI
  behavior. Fixed: `(PROGN)` returning Python `None`; `(VALUES-LIST NIL)`
  returning one value instead of zero; two reader tests. Remainder catalogued in
  §3. Discovered `fclpy/reader.py` is a dead second reader certified by 177 tests.
- **2026-08-12 (a)** — **M8's signaling core.** Handlers now run at the signal
  point *before unwinding* (`state.handler_stack`); `HANDLER-BIND` catches
  nothing; `HANDLER-CASE`/`IGNORE-ERRORS` share the stack; one `build_condition`
  replaced three drifted designator constructors; `SIGNAL` became SIGNAL; handler
  type matching delegates to `TYPEP`; condition lattice completed to CLHS Fig 9-1.
  `conditions/` 92 → 116 passing, zero regressions. Unblocked measurement of
  ~79% of the suite: `accounted` 4687 → 8971, `passed` 2920 → 4514.
  Also consolidated three drifted loop-watchdog copies into one `LoopWatchdog`.
- **2026-08-11** — M1 steps 1–2: canonical CL symbol table; deleted the blanket
  `except Exception: pass` at `lispenv.py:513`.
- **2026-08-09** — M0's measurement-corruption bugs: `funcall`'s missing
  non-local-exit re-raise (Finding K); `WARN` routing through `format_fn`;
  `LOOP`'s implicit NIL block (which this document had twice claimed was already
  done); FORMAT argument cursor; the `COMPLETENESS:` assertion, which is what
  made every later truncation visible instead of silent.
