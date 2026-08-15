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

**Latest full run: 2026-08-15. The suite is past half passing.**

```
COMPLETENESS: total=22113 passed=11548 failed=10565 accounted=22113 missing=0 extra=0
COMPLETENESS: OK
```

| | value | previous full run (2026-08-12) |
|---|---|---|
| Registered tests | 22113 | 22036 |
| Executed (`accounted`) | **22113 (100%)** | 22036 (100%) |
| Passed | **11548 (52.2%)** | 8960 (40.7%) |
| Failed | 10565 | 13076 |
| Never executed | **0** | 0 |
| Wall time | **~67 minutes** (3999s) | ~7.5 hours |

**+2588 passing across three landed mechanisms** — the printer (08-14), the
shared binder (08-14 b) and the global value cell (08-15). No single change owns
that number; each one's own measured contribution is in its
[Changelog](#changelog) entry.

**Wall time fell from ~7.5 hours to ~67 minutes**, which is C1's dividend
arriving in full: the 2026-08-12 run spent ~3h18m in loops that never terminate.
Exactly one such loop survived into this run — `integer-binary-search`
(`auxiliary/numbers-aux.lsp:46`), burning its full 600s cap after 1.3M
iterations, 15% of the run by itself — and **it was fixed the same day**
(see the [Changelog](#changelog)): it was not a loop defect at all but
`CEILING` losing precision above 2**53. **No loop in the suite now hits the
watchdog**, so the next full run should come in around 57 minutes.
CLAUDE.md's "about 20 minutes" and this document's former "4+ HOURS" were both
wrong; 67 minutes is measured.

**These numbers are the last full run and move only on a full run.** The
*checklist* is kept current between full runs by merging targeted runs into it
(see [below](#keeping-the-checklist-current-without-a-full-run)); its header
lists which runs it has been amended with. Do not copy an amended count here.

**Registered tests rose 22036 → 22113 (+77).** As in the 08-12 run, a rise here
is not new work appearing from nowhere: tests generated at load time only
register once the code that generates them runs, so fixing a load-time failure
*adds* tests. Treat `total` as an outcome, not a constant.

**`FORMAT`/`FORMATTER` remains the largest failing cluster**, and `printer/` has
moved 137 → 386 passing (17.4% → 49.0%) without its directive engine being
finished — [C2](#c2-format--formatter--largest-cluster-in-the-suite) is still
the largest single body of failures.

### Per-directory scoreboard (complete)

Ordered by failures. `Δ passed` is against the 2026-08-12 full run.

| directory | passed | failed | total | pass rate | Δ passed |
|---|---|---|---|---|---|
| (programmatically generated) | 2238 | **2822** | 5060 | 44.2% | +1163 |
| sequences | 1674 | **1484** | 3158 | 53.0% | +684 |
| cons | 692 | **946** | 1638 | 42.2% | +112 |
| arrays | 533 | 712 | 1245 | 42.8% | +13 |
| objects | 217 | 608 | 825 | 26.3% | +2 |
| numbers | 935 | 503 | 1438 | 65.0% | +63 |
| iteration | 420 | 418 | 838 | 50.1% | +54 |
| printer | 386 | 402 | 788 | 49.0% | **+249** |
| data-and-control-flow | 1027 | 393 | 1420 | 72.3% | +20 |
| streams | 223 | 320 | 543 | 41.1% | +62 |
| strings | 199 | 302 | 501 | 39.7% | +86 |
| types-and-classes | 285 | 260 | 545 | 52.3% | +2 |
| packages | 112 | 228 | 340 | 32.9% | +4 |
| conditions | 119 | 184 | 303 | 39.3% | +3 |
| pathnames | 79 | 136 | 215 | 36.7% | 0 |
| reader | 38 | 127 | 165 | **23.0%** | +9 |
| misc | 622 | 118 | 740 | 84.1% | +4 |
| characters | 149 | 110 | 259 | 57.5% | **−7** |
| structures | 14 | 101 | 115 | **12.2%** | 0 |
| eval-and-compile | 236 | 82 | 318 | 74.2% | +12 |
| environment | 112 | 80 | 192 | 58.3% | +45 |
| hash-tables | 89 | 69 | 158 | 56.3% | 0 |
| files | 23 | 64 | 87 | 26.4% | 0 |
| system-construction | 11 | 64 | 75 | **14.7%** | 0 |
| symbols | 1113 | 32 | 1145 | **97.2%** | +8 |

**`characters` is the one directory that went backwards**, 156 → 149, and it is
tracked in [preventing regression](#preventing-regression) rather than absorbed
into the total. Six directories did not move at all — `pathnames`, `structures`,
`hash-tables`, `files`, `system-construction` and (net) `objects` — and those are
the ones where the absent mechanism is still absent.

The spread is still the useful signal: `symbols` at 97.2% and `misc` at 84.1%
against `structures` at 12.2%, `system-construction` at 14.7%, `reader` at 23.0%
and `objects` at 26.3%. **The worst are all subsystems where one absent mechanism
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
   ~1 hour run is stale the moment anyone fixes anything — at which point every
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
could only ever be regenerated from a full ~1 hour run. `run_ansi.py` closes that gap:

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
pipenv run python run_all_tests.py > run_all_tests.log 2> run_all_tests.err   # ~67 MINUTES
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
- ~~**`WITH`, `NEVER`, `MAXIMIZE`/`MINIMIZE`, `OF-TYPE`.**~~ **Done 2026-08-15
  (c)**, together with the `BEING` drivers for hash tables and packages:
  `iteration` **424 → 636**, 0 regressions, and only ~140 of the 212 targeted.
  See the [Changelog](#changelog). **`AND`-joined parallel FOR clauses remain**
  — unlike the rest of that list they are not another clause but a change to
  the engine's *step* phase, since parallel drivers must all step from the
  previous iteration's values.
- **`IT` (CLHS 6.1.2.1.4).** Unimplemented — `(loop for x in l when x collect
  it)` signals `Unbound variable: IT`. It *appeared* to work in a whole-file
  run only because the iteration-variable leak left an earlier test's `it`
  bound at the root; LOOP.14.38/39 were passing off that constant. Doing it
  properly means the conditional clause supplying its test value to its own
  body, so it belongs with the clause-order item above rather than beside it —
  as does `ELSE`/`END`, for the same reason: `when p collect x else collect y`
  currently applies `p` to *both* accumulations.
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
| **M2** | Environment model | **binding forms done**, and **the global environment done (2026-08-15)** — one `BindingFrame` decides lexical vs. dynamic for LET, LET* and all eight iteration forms, and a global variable has one home, the symbol's value cell. Outstanding: `is_truthy(False)`, and the lambda-list binders, which are M3's | C1, X2 |
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
7. ~~**M2's binding model.**~~ **Done 2026-08-14 (b).** One `BindingFrame`
   decides lexical vs. dynamic for LET, LET* and all eight iteration forms, so
   an iteration form binds its own variable instead of assigning to an
   enclosing one. `iteration` **410 → 423**; the measurement gate it was
   blocking is gone. Details in the [Changelog](#changelog).
8. ~~**M2's remaining slice — the global value cell.**~~ **Done 2026-08-15.**
   A global variable has one home, the symbol's value cell, because the global
   environment no longer has the lexical bindings Common Lisp does not give it
   (CLHS 3.1.1.1). **The predicted fix was wrong in an instructive way:** this
   item said it "has to move `SETQ` and the lookup order with it", and it moved
   neither. Delete the home that should not exist and `SETQ` is already right
   (its walk ends at the value cell) and "lexical chain, then value cell" is
   already right (the value cell *is* the end of the chain). +23 with 0
   regressions, 20 of them untargeted. Details in the [Changelog](#changelog).
9. ~~**C1's follow-ups — LOOP's clause vocabulary.**~~ **Done 2026-08-15 (c).**
   `LOOP` had become the largest single operator cluster in the suite (410 of
   10157 failures) *after* C1 landed, because C1 gave it one engine but not the
   clauses. `iteration` **424 → 636**, 0 regressions. It also confirms the
   ranking heuristic §3 gives: `loop6.lsp` and `loop7.lsp` were two of the
   23 files failing 100%, and both were one absent driver each.
10. **`SORT` must return a sequence of the argument's type** (CLHS 17.1). New,
   and cheap: `(sort (list 3 1 2) #'<)` returns a *vector* and
   `(sort (copy-seq "cba") #'char<)` returns `#("a" "b" "c")`. It is the entire
   residual of `iteration/loop6.lsp` and `loop7.lsp` (41 tests that only wrap a
   correct LOOP result in SORT), plus `sequences/sort.lsp` 20/34 and
   `stable-sort.lsp` 20/34. Fix it as the shared *sequence result-type*
   discipline (C3/M6) rather than in SORT alone — MERGE, REMOVE, SUBSTITUTE and
   CONCATENATE owe the same guarantee, and `sequences/merge.lsp` at 81/124 is
   the next-largest file in the directory.
11. **Re-measure, then re-derive this list.** The residual distribution has
   already shifted enough that ranking further ahead is guesswork. On the
   current evidence the next-largest unblocked mechanism is
   [C2](#c2-format--formatter--largest-cluster-in-the-suite)'s remaining
   `FORMAT` directives (`~E`, `~F`, `~R`, `~T`), with M3's lambda-list engine
   close behind now that it owns the last binding form that does not go
   through `BindingFrame` ([§5](#5-known-temporary-deviations)).

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
| LOOP `AND`-joined *FOR* clauses (parallel stepping) unimplemented; the token is dropped. `AND` in a `WITH` clause **is** implemented | parallel drivers mean stepping every driver from the values of the previous iteration — a change to the engine's step phase, not another clause | C1 follow-up |
| LOOP `IT` (CLHS 6.1.2.1.4) and `ELSE`/`END` unimplemented | all three need the conditional clause to own its own body, which is the clause-order item below rather than a separate feature | C1 follow-up |
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
| A variable bound *dynamically* by a form is invisible to that form's body if an **enclosing lexical** binding of the same name exists — `eval` checks the lexical chain before the value cell | narrowed but not gone. It no longer applies to a *globally special* variable, which has no lexical binding anywhere to shadow it (2026-08-15), nor to a local `(declare (special x))`, which redirects through `%SPECIAL-REF`. What remains is a lexical binding shadowing a `PROGV` of the same undeclared name, which no ANSI test in the measured groups needs | M2 |
| A function's lambda list binds a *proclaimed special* parameter lexically | the six copy-pasted binders do not go through `BindingFrame`, so `(defvar *x* 1) (defun f (*x*) ...)` binds `*x*` lexically instead of dynamically. Consolidating them onto the shared frame is M3's whole point, and doing it per-binder now would be the seventh mechanism §4 warns about | M3 |

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

#### Open regressions carried by the 2026-08-15 full run

The 08-15 run is **+2588 overall but worse in 19 files** against the 08-12
baseline. They are listed here rather than absorbed into the total, because
refreshing the baseline is what makes a regression invisible — do not refresh
`docs/ansi_checklist_baseline.json` until these are attributed or accepted in
writing.

| files | Δ failures | note |
|---|---|---|
| `characters/char-compare.lsp` +4, `characters/character.lsp` +3 | +7 | the only **directory**-level regression, `characters` 156 → 149. **Not the value cell's**: measured 149 of 259 both at HEAD and at HEAD with only the value-cell change reverted, so it belongs to the 08-13 `Character` representation or the 08-14 printer. Still open |
| ~~`numbers/` rounding family — `round`, `truncate`, `floor`, `ceiling`, `fceiling`, `ffloor`, `ftruncate`, `fround` (+2 each)~~ | ~~+16~~ | **Resolved 2026-08-15 (b).** The uniform +2 across eight operators was one shared defect, as the shape suggested: those eight files are now **16 → 103 of 138**, well past the baseline |
| `numbers/log.lsp`, `numbers/lcm.lsp` (+2), `numbers/asin.lsp`, `numbers/acos.lsp`, `numbers/rationalize.lsp` (+1) | +8 | still open |
| `cons/sublis.lsp` +2, `data-and-control-flow/every.lsp` +1, `notevery.lsp` +1, `streams/write-line.lsp` +2 | +6 | isolated |

**Attribution is not yet possible for most of these**, and the reason is worth
recording as a process lesson: the baseline predates *three* landed changes
(printer 08-14, binder 08-14 b, value cell 08-15), and the tree between them was
never a self-consistent commit — `fclpy/lispfunc/binding.py` stayed untracked
across two commits, so `ea24491` cannot even be checked out and run. **A
mechanism change should be measured against the commit before it, which requires
that commit to be runnable.** The value-cell change was measured this way over
ten directories (0 regressions); `characters` was not one of them.
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
| `run_all_tests.py` | full suite (~67 min) — authority, not inner loop |
| `REPAIR.md` | crash-repair SOP — historical; crashes are no longer the constraint |

---

## Changelog

Condensed from the previous chronological plan. Each entry is a *mechanism*
landed, not a test count.

- **2026-08-15 (c)** — **LOOP's clause vocabulary, in the one engine.** C1 gave
  LOOP a single iteration engine over composing drivers; what it did not give
  it was the *clauses*. Nine keywords — `WITH`, `MAXIMIZE`/`MAXIMIZING`,
  `MINIMIZE`/`MINIMIZING`, `NEVER`, `OF-TYPE`, and the `BEING` families for hash
  tables and packages — were absent from the parser, and **an absent keyword was
  silently dropped**, so the loop ran and returned a plausible wrong answer:
  `(loop for x in '(1 5 3) maximize x)` was NIL, and
  `(loop for x in '(1 2 3) never (> x 5))` was NIL — which means its sibling
  `never (> x 2)` had been *passing for the wrong reason*. `WITH` was worse than
  dropped: its token fell into the loop body and evaluated as a free reference,
  so every WITH loop signalled `Unbound variable: WITH`.
  **`iteration` 424 → 636 of 843. +212, 0 regressions**, and the shape of the
  movement is the point — only ~140 of the 212 were in the files targeted:
  | file | before | after | | file | before | after |
  |---|---|---|---|---|---|---|
  | `loop8.lsp` (WITH) | 27 failing | **0** | | `loop1.lsp` | 24 | **15** |
  | `loop6.lsp` (hash) | 47 | **15** | | `loop2.lsp` | 14 | **5** |
  | `loop7.lsp` (package) | 35 | **26** | | `loop3.lsp` | 16 | **6** |
  | `loop10.lsp` (numeric) | 62 | **23** | | `loop5.lsp` | 15 | **7** |
  | `loop12.lsp` (bool) | 22 | **11** | | `loop15/16.lsp` | 19/19 | **3/3** |
  loop1/2/3/5/11/13/15/16/17 were not targeted at all; they move because
  type-specs and destructuring are used throughout them.
  **Three sub-mechanisms did that untargeted work, and each replaced a partial
  copy rather than adding a branch.** (1) **One type-spec parser**
  (`_loop_type_spec`) for all three positions a type-spec can occupy — after a
  FOR variable, after a WITH variable, after a numeric accumulation's form.
  Only the numeric accumulations may consume one, because `collect x` followed
  by `t` would otherwise lose the T. (2) **One destructurer**
  (`_loop_destructure`), a recursive walk replacing three enumerated shapes;
  the shapes the enumeration could not express are exactly what was failing —
  a dotted tail `(a b . rest)`, a NIL hole `(nil . v)`, and a pattern longer
  than its value. (3) **One early decision** for ALWAYS/NEVER/THEREIS instead of
  two flags with different "did it fire?" tests (`always_failed` versus
  `thereis_result is not None`), which is what made NEVER an addition rather
  than a third convention.
  **Landed with it, because the new hash driver could not be correct without
  it: a hash table no longer stores its own options as entries.**
  `MAKE-HASH-TABLE` returned a plain `dict` carrying its test and sizing in
  three `'__hashmeta__...'` **keys**, i.e. in the key space that holds user
  entries. Four places knew to filter them (MAPHASH, CLRHASH,
  HASH-TABLE-COUNT, the printer) and everything else did not, so
  `(loop for k being the hash-keys of h collect k)` collected the Python string
  `"__hashmeta__test"` as a Lisp value (standing rule 2) — and filtering it in
  the driver would have been a fifth copy. `HashTableDict` keeps them as
  attributes, so a traversal is correct by default and the four filters are
  **gone**, not five.
  **And one shared package enumerator.** `for x being the symbols of p` needed
  the accessible/present/external distinction that `DO-SYMBOLS` and
  `DO-EXTERNAL-SYMBOLS` already open-coded, so `coerce_to_package` and
  `package_symbols` now serve all three. The consolidation found a live bug in
  the copy it replaced: `use_packages` holds package *names* as well as
  `Package` objects (`Package.intern` handles both), and DO-SYMBOLS read
  `external_symbols` straight off each entry — a string entry yields the empty
  set, so every inherited symbol was silently skipped. LOOP's own copy was
  worse: it swallowed a failed package lookup with a bare `except Exception`
  and iterated an *empty* package, so a misspelled name returned 0 instead of
  signalling (standing rule 4).
  **Measured, before → after, each `before` in a stash of the same tree:**
  | target | before | after |
  |---|---|---|
  | `iteration` | 424 of 843 | **636** |
  | `sequences`, `misc`, `types-and-classes`, `structures` | 3505 of 6291 | **3513** |
  | `hash-tables`, `packages`, `symbols`, `data-and-control-flow`, `cons`, `conditions`, `eval-and-compile`, `environment` | 4131 of 6305 | 4131 — unchanged |
  **+220 over 13,439 tests with a per-test diff of 0 regressions in all three.**
  The +8 outside `iteration` is `SEARCH-BITVECTOR.1`, `SEARCH-LIST.1`,
  `SEARCH-STRING.2`, `SEARCH-VECTOR.3/.5/.7` and the two
  `ALL-*-CLASSES-ARE-SUBTYPES-OF-*` tests — all of them aux code that drives its
  own assertions with LOOP, which is the blast radius C1 predicted.
  `pytest` 1616 passed (from 1552), same 1 pre-existing unrelated failure
  (`test_all_expected_functions_are_registered`), plus 64 new tests in
  `tests/test_loop_clauses.py`. They are pinned together rather than beside each
  feature because they share a failure *mode*, not a feature: several assert a
  value for a loop that already "worked", since a dropped clause produced a
  wrong answer rather than an error.
  **Discovered, diagnosed, not fixed:** **`SORT` does not preserve the sequence
  type.** `(sort (list 3 1 2) #'<)` returns a *vector*, and
  `(sort (copy-seq "cba") #'char<)` returns `#("a" "b" "c")` rather than
  `"abc"` — CLHS 17.1 requires the result to be of the same type as the
  argument. This is **the whole of what is left in `loop6.lsp` and
  `loop7.lsp`**: LOOP.6.6–.18 and LOOP.7.1–.20 all wrap their result in
  `(sort ... #'symbol<)`, so 41 of the 41 residual failures in those two files
  are SORT's, not LOOP's. It is also `sequences/sort.lsp` 20/34 and
  `stable-sort.lsp` 20/34, and it belongs with the sequence result-type
  discipline (C3/M6), not here. Also found: **two hash-table implementations**
  coexist — `lispfunc/hashtables.py`'s `HashTable` class and
  `lispfunc/misc_hashtables.py`'s dict, both registering `MAKE-HASH-TABLE` and
  `GETHASH`; the dict wins and the class is dead (standing rule 3, Finding L).
- **2026-08-15 (b)** — **The divide-then-round family is exact, and returns two
  values.** All eight of FLOOR/CEILING/TRUNCATE/ROUND and their F- variants
  computed `x / divisor` — Python **float** division — before rounding, so every
  one of them silently lost precision above 2**53:
  `(ceiling (+ (expt 2 62) (1+ (expt 2 62))) 2)` was one *less* than the true
  midpoint. `_exact_quotient` routes rationals through `Fraction`, on which
  `math.floor`/`math.ceil`/`int`/`round` are all exact — including `round`'s
  half-to-even, which is the rule CLHS gives ROUND.
  **The bug presented as a hang, not a wrong answer, and that is why it had
  survived.** `integer-binary-search` (`auxiliary/numbers-aux.lsp:46`) steps
  with `(ceiling (+ lo hi) 2)`, so once `lo` passed 2**53 the midpoint rounded
  back to `lo` itself, `(setq lo mid)` became a no-op and the search ran until
  the 600s watchdog killed it — 1,335,702 iterations, **15% of the whole ANSI
  run's wall time in one form**, reached from `numbers/sqrt.lsp`'s
  `(find-largest-exactly-floatable-integer most-positive-fixnum)`. This is the
  last of the never-terminating loops [C1](#c1-loop-clause-composition--done-2026-08-12-f)
  catalogued on 08-12 (SQRT.12–.17, DEPOSIT-FIELD.1–.5, DPB.2), and **it was
  never a LOOP defect** — the 08-12 diagnosis attributed it to the wrong
  subsystem. `run_ansi.py numbers/sqrt.lsp` went from a 600s abort to **1.8s**.
  **Landed with it**, because the same eight functions were failing their whole
  files for a second, unrelated reason: they returned the quotient **alone**
  where CLHS 12.2 requires *quotient and remainder*, and every ansi-test helper
  for them opens with `(eql (length vals) 2)` — so nothing in those files could
  pass whatever the quotient was. And `REM` was Python's `%`, which is
  floor-based: right for MOD, wrong for REM whenever the operands differ in sign
  (`(rem -7 2)` gave 1, ANSI requires -1). REM and MOD are now the remainders of
  TRUNCATE and FLOOR rather than a third implementation of "remainder"
  (standing rule 3).
  **Measured, before → after on the same eight files:**
  | file | before | after | | file | before | after |
  |---|---|---|---|---|---|---|
  | `round.lsp` | 2 of 23 | **10** | | `fceiling.lsp` | 2 of 13 | **12** |
  | `truncate.lsp` | 2 of 21 | **15** | | `ffloor.lsp` | 2 of 13 | **12** |
  | `floor.lsp` | 2 of 21 | **15** | | `ftruncate.lsp` | 2 of 13 | **12** |
  | `ceiling.lsp` | 2 of 21 | **15** | | `fround.lsp` | 2 of 13 | **12** |

  **16 → 103 of 138, +87.** `pytest` 1518 passed, same 1 pre-existing unrelated
  failure, plus 34 new tests in `tests/test_math_rounding.py` — which exist
  *because* the precision defect manifested as a hang: ansi-test covers the
  two-values contract cleanly, but a watchdog kill is not a failure signal, so
  the exactness property and the loop-termination property are pinned where
  they fail fast and say why.
  **Discovered, not fixed:** a `MultipleValues` reaching the printer renders as
  `#<MULTIPLEVALUES 0x...>` (standing rule 2). It does not affect RT, which
  compares through `multiple-value-list`, but a top-level multiple-value return
  should print as its values.
- **2026-08-15** — **M2: a global variable has one home.** The global
  environment no longer has lexical variable bindings, because Common Lisp
  does not have them: CLHS 3.1.1.1 makes the global environment's variable
  bindings the *dynamic* ones. `Environment.is_global` is true for the
  parentless environment at the root of every chain, and its `add_variable`/
  `find_variable`/`has_variable`/`set_variable` read and write the symbol's
  value cell — the cell `SYMBOL-VALUE`/`BOUNDP`/`SET`/`MAKUNBOUND`/`PROGV` and
  every dynamic binding already used. So `(defvar *x* 1)` now leaves
  `(boundp '*x*)` T, `(let ((*x* 2)) *x*)` reads **2**, and `(set '*x* 4)` is
  visible to a plain reference.
  **The predicted fix was wrong, and how it was wrong is the point.** [§4](#recommended-order)
  item 8 said the fix "has to move `SETQ` and `eval`'s lookup order with it".
  It moved neither, and neither needed moving: once the global lexical binding
  is gone, `SETQ`'s chain walk already ends at the value cell, and "lexical
  chain, then value cell" already resolves to the innermost binding **because
  the value cell is the end of the chain**. The defect was never that the
  lookup order was wrong; it was that there was a home for it to find first.
  Two homes → one, by deleting the one that should not exist.
  Landed with it: `binding.proclaim_special` is the single writer of the
  proclamation table `is_proclaimed_special` reads, replacing three inline
  copies in `eval_defvar`, `eval_defparameter` and `_store_special_declaration`;
  the **standard variables are proclaimed special at bootstrap**
  (`lispenv.STANDARD_SPECIAL_VARIABLES`, CLHS Figure 25-1), without which
  `(let ((*print-base* 2)) ...)` binds lexically and the printer — which reads
  the variable from Python through the *global* environment — never sees it;
  `(defvar *x*)` with no initial-value form no longer binds the variable to NIL,
  per CLHS, and `DEFVAR`'s "already bound?" test asks the value cell rather than
  whatever lexical binding surrounds the form; and the standard stream
  variables are re-initialized on a full bootstrap rather than guarded by
  `if find_variable(...) is None`, which also removes a latent
  `UnboundLocalError` where four of them referenced a `stdout_stream` created
  inside another's guard.
  **Measured, before → after on the same targets, same runner both sides
  (each `before` run in a stash of the same working tree, so like-for-like):**
  | target | before | after |
  |---|---|---|
  | `data-and-control-flow` | 1023 of 1428 | **1037** |
  | `packages` | 140 of 500 | **147** |
  | `eval-and-compile` | 234 of 318 | **236** |
  | `iteration`, `symbols`, `conditions`, `cons`, `environment`, `hash-tables`, `types-and-classes` | — | unchanged, 0 regressions |
  **+23, 0 regressions, and only 3 of the 23 were targeted** — `DEFVAR.3`,
  `DEFPARAMETER.3`, `DEFCONSTANT.1`. The other 20 moved because the mechanism
  moved: `LET.3`, `LET*.3`, `PROGV.6A`, `SETQ.5`, `PSETQ.8`/`.9`, `SETF.5`,
  `MULTIPLE-VALUE-BIND.7`, `FLET.40`, `FLET.69`, `LAMBDA-LIST-KEYWORDS.1`,
  `DEFINE-COMPILER-MACRO.7`/`.8`, and **all seven `IN-PACKAGE.7`–`.13`**, which
  move because `*PACKAGE*` is now genuinely special rather than a global
  lexical binding with a Python-side mirror. `pytest` 1518 passed (from 1494),
  same 1 pre-existing unrelated failure (`test_all_expected_functions_are_registered`);
  the 4 `xfail`s in `TestTheGlobalValueCellDefect` are now 12 passing tests in
  `TestAGlobalVariableHasOneHome`, plus a new `TestTheStandardVariablesAreSpecial`.
  **Two unit tests asserted the defect and were corrected with citations**
  (§7: when `pytest` and `ansi-test` disagree, the unit test is wrong).
  `TestLetStar` probed "LET* left nothing behind" with `(boundp '*bv*)` **=> NIL**,
  which only read NIL *because* `DEFVAR` was broken; it now asserts the value is
  restored to the DEFVAR value. `test_condition_in_lisp_env` stored with one
  freshly built `LispSymbol('*ERROR*')` and read back with a *second* one,
  which worked only because global bindings were keyed by symbol **name** —
  they are the symbol's own value cell now, so two uninterned symbols sharing a
  name are two variables, as CLHS requires.
  **Fixed en route, in the measurement instrument itself:** `scripts/run_ansi.py`
  never established `(in-package :cl-test)`. `gclload2.lsp` — the file the
  targeted runner stands in for — opens with it, and `gclload1.lsp`'s own
  in-package does not carry over because `LOAD` binds `*PACKAGE*` for the extent
  of a file (CLHS 24.1), just as in a conforming Lisp. So every aux preamble was
  read in `CL-USER`, and `auxiliary/types-aux.lsp`'s `*subtype-table*` became a
  *different symbol* from the `CL-TEST::*SUBTYPE-TABLE*` that `ansi-aux.lsp`
  binds. The old name-keyed global environment had been silently conflating the
  two; identity-keyed value cells surfaced it as TYPES.9/TYPES.9A failing, which
  is how it was found. **A targeted run now reproduces the full-suite package
  context it is supposed to**, and TYPES.9/.9A pass for the right reason.
  **Discovered, diagnosed and deliberately not fixed here:** a lambda list binds
  a *proclaimed special* parameter lexically — `(defvar *sv* 1)` then
  `(defun f (*sv*) (g))` leaves `g` seeing 1, not the argument. Verified
  directly. The six copy-pasted binders are exactly what M3 exists to
  consolidate onto `BindingFrame`, and repairing them one at a time here is the
  "seventh incompatible mechanism" [§4](#recommended-order) warns about. See
  [§5](#5-known-temporary-deviations).
- **2026-08-14 (b)** — **M2: one binder decides lexical vs. dynamic.**
  `fclpy/lispfunc/binding.py`'s `BindingFrame` is now the only place that
  answers "is this variable special here", and LET, LET* and all eight
  iteration forms (DO, DO*, DOLIST, DOTIMES, LOOP, DO-SYMBOLS,
  DO-EXTERNAL-SYMBOLS, DO-ALL-SYMBOLS) go through it. Establishing a binding
  and stepping it are one operation, `frame.bind`: the first call decides where
  the binding lives, later calls assign to that same binding — which is also
  what makes successive iterations share one binding (DO.15).
  **The defect was not "the iteration variable is mis-bound", it was that the
  clause establishing it never bound anything.** All eight forms established
  their variable with `Environment.set_variable`, which *walks the environment
  chain and mutates the first binding of that name it finds*; since
  `Environment.__init__` hands a child its parent's `variable_bindings` list,
  that walk always reached an enclosing binding. So `(let ((x 99)) (dolist (x
  '(1 2 3))) x)` was NIL, and — because rt.lsp's failure reporter takes its
  output stream in a parameter named `s` — a `(do-all-symbols (s) ...)` or
  `(loop for s = ...)` in the suite overwrote RT's own stream with a symbol.
  **That was a measurement gate, the third of this shape after the
  string-is-a-vector gate (08-13) and the `*STANDARD-OUTPUT*` gate (08-14):
  `run_ansi.py printer` could not run to completion at all.**
  **Why the one-word fix was wrong.** `add_variable` for the establishing call
  fixes all eight leaks and measured `iteration` 410 → 408: it gains DOLIST.14
  and DOTIMES.16 and loses DO.14, DO*.14, DOTIMES.18 and .18A, which declare
  the iteration variable special in the body and so must be bound *dynamically*
  — the rule that lived in `eval_let` and, copy-pasted, in `eval_letstar`.
  **LET*'s copy was not merely duplicated, it was wrong**: for a special
  variable it called `global_env.add_variable`, putting a *lexical* binding in
  the global environment that outlived the LET* and was invisible to
  `SYMBOL-VALUE`. Two copies and eight absences → one.
  **The distinction the shared binder had to get right** is declaration vs.
  proclamation: a local `(declare (special x))` governs the form it heads and
  free references within it, but must *not* make a nested binding form bind
  dynamically, while a `DEFVAR` proclamation must. DOTIMES.17 and .18 differ
  only in whether the loop body declares the variable, and expect `(0 0 0 0)`
  and `(3 2 1 0)` respectively — so `is_proclaimed_special` consults the root
  environment only, and walking the chain (the obvious "more correct" reading)
  collapses the pair. Landed with it: a binding form's *free* special
  declarations now redirect through the same `%SPECIAL-REF` symbol macro
  LOCALLY already used (`special_reference`), which is what DOLIST.17 and DO.17
  need for a result form and a step form respectively; `eval_locally` reuses
  the shared declaration parser instead of its own inline copy; declarations
  are stripped from an iteration body before it runs as a TAGBODY, where a
  declaration is not a statement; and a bare symbol in LET*'s binding list
  binds to NIL (CLHS 3.1.2.1.1) instead of being skipped and left unbound.
  **Landed with it, and it turned out to matter more than the binder: the
  `*PACKAGE*` mirror was never restored.** The restore was guarded by
  `if old_package is not None`, which conflates "nothing was saved" with "None
  *is* the saved value" — and None is `state.current_package`'s normal state
  until something binds `*PACKAGE*`, because a plain reference falls back to a
  default. So the **first** `(let ((*package* p)) ...)` of a session never
  restored, and every symbol read afterwards interned into `p`. Found by a
  smoke test whose every later form came back with keywords where it had
  written symbols; it is why `packages` could not complete.
  **Measured, before → after on the same targets** (each run in a worktree at
  the previous commit, so these are like-for-like and not full-run numbers):
  | target | before | after |
  |---|---|---|
  | `iteration` | 410 of 843 | **424** of 843 |
  | `data-and-control-flow` | 1022 of 1428 | **1023** of 1428 |
  | `cons` | 868 of 1882 | 868 — unchanged |
  | `conditions` | 159 of 664 | 159 — unchanged |
  | `packages` | **crashes** — `ConditionException: Not an output stream: #\f` | **140 of 500, completes** |
  | `printer/print-strings.lsp` | registers 16, **no result** | **8 of 16, completes** |
  | `printer/print-symbols.lsp` | registers 31, **no result** | **7 of 31, completes** |
  The bottom three are RT's own report stream being overwritten by a loop
  variable and then printed to; **that they now complete is the untargeted
  movement that says this was a mechanism** and not a repair to `iteration`.
  A whole-directory `run_ansi.py printer` run still does **not** complete, but
  it now reaches `printer/format/` (FORMAT.S.7) instead of dying in
  `print-strings.lsp`, on an unrelated defect — `ValueError: I/O operation on
  closed file` leaking as a Lisp value ([X1](#x1-python-exceptions-leaking-as-lisp-values))
  and then an abrupt exit with no traceback. **That is the next thing in front
  of the printer directory, and it is not this one.**
  **Per-test diff on `iteration`: 16 fixed, 2 lost — and both losses are false
  passes the leak was manufacturing.** Fixed: DO.17/.18/.19, DO*.17/.18/.19,
  DOLIST.6/.14/.17, DOTIMES.16/.17/.17A/.23/.23A, LOOP.2.17, LOOP.3.17 — only
  five of which were targeted. Lost: LOOP.14.38 and LOOP.14.39, which are
  `(loop for x in '(1 2 nil 3 4 nil 5 nil) when x count it)`. **LOOP's `IT` is
  not implemented at all** — `(loop ... count it)` signals `Unbound variable:
  IT` in isolation both before and after this change. They passed only because
  `iteration/loop14.lsp:260` runs `(loop for it on '(a b c d) ...)` earlier in
  the same file and the old leak left `it` bound at the root to a truthy value,
  so `count it` counted a leaked constant once per iteration where `when x`
  held, arriving at 5 by coincidence. Standing rule: a test that passes for the
  wrong reason is not progress, so these were not preserved; `IT` is now a
  visible [C1 follow-up](#c1-follow-ups-still-open) instead of an invisible one.
  `pytest` 1494 passed (from 1457), same 1 pre-existing unrelated failure
  (`test_all_expected_functions_are_registered`); the 13 `xfail`s in
  `tests/test_iteration_variable_binding.py` are now passing tests, and the
  module grew coverage for the special-vs-lexical decision, unwinding on a
  non-local exit, the `*PACKAGE*` mirror, and LET*'s two repairs.
  **Discovered, diagnosed and deliberately not fixed here:** a special variable
  has **two homes that never reconcile** — `DEFVAR`/`SETQ` maintain a lexical
  binding in the global environment, `SYMBOL-VALUE`/`BOUNDP`/`PROGV` and every
  dynamic binding use the value cell, and `eval` checks the lexical chain first,
  so `(defvar *x* 1)` leaves `(boundp '*x*)` NIL and `(let ((*x* 2)) *x*)` reads
  1. Consolidating the binder is what isolated it: the frame's dynamic bindings
  are provably correct through `SYMBOL-VALUE`, so the residual wrong answer is
  entirely the global lexical binding's. See [§5](#5-known-temporary-deviations)
  and the 4 `xfail`s in `TestTheGlobalValueCellDefect`; it is M2's next slice,
  and the fix has to move `SETQ` and `eval`'s lookup order with it.
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
