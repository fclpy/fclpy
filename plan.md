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
> 7341 failures grouped directory → file, each with the command to re-verify it.
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

**Latest full run: 2026-08-16. Two thirds passing.**

```
COMPLETENESS: total=22113 passed=14772 failed=7341 accounted=22113 missing=0 extra=0
COMPLETENESS: OK
```

| | value | previous full run (2026-08-15) |
|---|---|---|
| Registered tests | 22113 | 22113 |
| Executed (`accounted`) | **22113 (100%)** | 22113 (100%) |
| Passed | **14772 (66.8%)** | 11548 (52.2%) |
| Failed | 7341 | 10565 |
| Never executed | **0** | 0 |
| Wall time | **~113 minutes** (6760s) | ~67 minutes |

**+3224 passing.** The dominant contribution is not a new feature but a
*measurement* repair, and it is the third time this plan has recorded that shape
([§4](#recommended-order) items 4, 6 and the COND fix): **LOOP's `unless`
clause never evaluated its test**, and ansi-test's `check-type-error*` is built
entirely on that clause, so for every `.ERROR` test that uses it the function
under test *was never called* and the test passed vacuously. Fixing the clause
converted a large block of false passes into real ones — and, on the way,
exposed the hang described below. See the [Changelog](#changelog).

**Wall time rose ~67 → ~113 minutes, and that is the expected direction.** The
old 67 minutes was partly cheap because work was being skipped: a
`check-type-error` that never calls its function returns fast. Four loops now
cross the 120s warning and all four resolve; none reaches the 600s cap.

> **The 08-15 run wedged and this one did not.** The 08-15 tree could not
> complete a full run at all: it sat at 27GB of allocated memory for over half
> an hour with no diagnostic. The cause was three defects stacked —
> `TYPEP` not knowing the atomic `UNSIGNED-BYTE`, `MAKE-LIST` building
> unboundedly, and **no runner having any hang detection that could see it**.
> All three are fixed; the mechanism is in the [Changelog](#changelog) and the
> new detector is [`fclpy/watchdog.py`](fclpy/watchdog.py).

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

Ordered by failures. `Δ rate` is against the 2026-08-15 full run.

| directory | failed | total | pass rate | Δ rate |
|---|---|---|---|---|
| cons | **647** | 1638 | 60.5% | +18.3 |
| objects | **594** | 825 | 28.0% | +1.7 |
| numbers | 400 | 1438 | 72.2% | +7.2 |
| printer | 377 | 788 | 52.2% | +3.2 |
| sequences | 352 | 3158 | **88.9%** | **+35.9** |
| types-and-classes | 337 | 545 | 38.2% | −14.1 |
| streams | 334 | 543 | 38.5% | −2.6 |
| data-and-control-flow | 305 | 1420 | 78.5% | +6.2 |
| strings | 203 | 501 | 59.5% | **+19.8** |
| packages | 202 | 340 | 40.6% | +7.7 |
| iteration | 160 | 838 | **80.9%** | **+30.8** |
| pathnames | 140 | 215 | 34.9% | −1.8 |
| conditions | 130 | 303 | 57.1% | **+17.8** |
| reader | 129 | 165 | **21.8%** | −1.2 |
| characters | 108 | 259 | 58.3% | +0.8 |
| arrays | 101 | 1245 | **91.9%** | +0.8 |
| structures | 101 | 115 | **12.2%** | 0 |
| environment | 83 | 192 | 56.8% | −1.5 |
| hash-tables | 73 | 158 | 53.8% | −2.5 |
| eval-and-compile | 70 | 318 | 78.0% | 0 |
| misc | 70 | 740 | **90.5%** | +6.4 |
| system-construction | 64 | 75 | **14.7%** | 0 |
| files | 62 | 87 | 28.7% | +2.3 |
| symbols | 40 | 1145 | **96.5%** | −0.7 |

**`sequences` and `iteration` are the run's story** — +35.9 and +30.8 points.
Neither had a sequence- or iteration-specific change land; both moved because
the `unless` clause and the type/number repairs are used *throughout* their
assertions. That is the mechanism-versus-symptom signal this plan keeps asking
for, and it is why the recommended order below leads with mechanisms rather than
with the largest file.

**`types-and-classes` fell 14.1 points and is the one directory that clearly went
backwards.** Do not absorb it into the total — it is tracked in
[preventing regression](#preventing-regression). The likely reason is the same
`unless` repair: tests that never ran their body now run it and fail honestly,
which is a *reporting* correction rather than a code defect, but that is a
hypothesis and it is **not yet verified**.

`structures` (12.2%), `system-construction` (14.7%), `reader` (21.8%) and
`objects` (28.0%) are unchanged or nearly so, and remain the subsystems where one
absent mechanism fails everything downstream of it.

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

### Policy change trigger: under 3,000 failures and under 100 files

**This mode has a planned expiration, and it is a measured one, not a vibe.**
The mechanism-first strategy above is justified by a specific premise: with
roughly half the suite failing, a small number of core mechanisms binds most
of the failures, so a fix that only moves the file you targeted is suspect.
That premise gets weaker as the suite shrinks — [§3](#3-the-checklist) already
recorded the distribution *flattening* well before this threshold ("no cluster
now exceeds 6% of the remainder... the argument that one mechanism unblocks
everything is weaker than at any previous point").

**Once a full run reports fewer than 3,000 failing tests *and* fewer than 100
files containing failures, switch modes:**

- Stop hunting for clusters as the default move. At this scale most remaining
  files fail for their *own* reason — a missing operator, an edge case, a
  format directive — not because one shared mechanism is still absent. Working
  the checklist file-by-file (still cheapest-first, still checking the
  disappeared-failures signal after each fix) is no longer the wrong mode; it
  is [Tier 3](#tier-3--the-genuine-tail) becoming the *main* mode rather than
  the tail.
- Still open every file with the same discipline as before: reproduce the
  smallest failing case, find the actual defect, and check whether the fix
  moves other files before assuming it doesn't. A shared mechanism can still be
  hiding under a small footprint — the point of this section is to stop
  *assuming* one is there, not to stop looking when one surfaces on its own.
- Still run the full per-file regression check ([§7](#preventing-regression))
  before and after every fix — a smaller remaining surface makes a regression
  *cheaper to catch* immediately, not less costly if missed.
- Re-derive the ranking from the live checklist rather than from this
  document's Tier 1/2 lists, which were written against a much larger, more
  clustered failure set and will be stale by the time this threshold is
  reached.
- If a full run afterward pushes failures back above 3,000 or files back above
  100 (a mechanism regressed, or a large new test population registered — see
  [§1](#1-status)'s note on `total` rising), fall back to mechanism-first until
  it drops again. This is a threshold on the live scoreboard, not a one-way
  door.

### The checklist artifact

**`docs/ansi_checklist.md`** is the working checklist: all 7341 failures grouped
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

### Cluster sizes (complete data, 7341 failures, 2026-08-16)

```powershell
sed 's/\.[0-9].*$//' ansi_results/failed.txt | sed 's/\.ERROR.*$//' | sort | uniq -c | sort -rn | head -40
grep -cE "^FORMAT" ansi_results/failed.txt        # aggregate a family
```

| cluster | failures | % of all failures | one mechanism? |
|---|---|---|---|
| **`FORMAT` + `FORMATTER`** | **417** | 5.7% | yes — one directive engine |
| **`PRINT.INTEGERS.*`** (`BASE` 84, `RADIX.BASE` 77) | **161** | 2.2% | yes — printer radix/base |
| **`LOOP`** | **154** | 2.1% | remaining clauses, not the engine |
| **CLOS** (`DEFGENERIC` 51, `SHARED-INITIALIZE` 41, `CHANGE-CLASS` 34) | **~150** | 2.0% | no — two implementations |
| ~~**Method combination** (`DEFGENERIC-METHOD-COMBINATION` 95, `DEFINE-METHOD-COMBINATION` 20)~~ | ~~**115**~~ | ~~1.6%~~ | **yes — done 2026-08-18**, and the answer was "the operator did not exist" |
| **`OPEN`** (`OPEN` 82, `PROBE` 36, `OUTPUT` 35, `IO` 35) | **188** | 2.6% | yes — stream/file model |
| **Arrays** (`MAKE-ARRAY` 43, `DISPLACED` 31) | **~74** | 1.0% | residual, model is done |
| **`SUBTYPEP`** (`MEMBER` 34, `INTEGER` 30) | **~64** | 0.9% | yes — no type lattice |
| **Packages** (`MAKE-PACKAGE` 51) | **~51** | 0.7% | yes — package model |
| **`PARSE-INTEGER`** | **49** | 0.7% | yes — one reader/number path |
| **`MAKE-HASH-TABLE`** | **29** | 0.4% | yes — `:test` as a designator |

**The distribution has flattened, and that is the headline.** At 08-15 the top
ten clusters were ~6537 failures, half the total, and `FORMAT` alone was 1623.
`FORMAT` is now 417 and no cluster exceeds 6% of the remainder. The three
former giants — sequences (1266), `DEFSTRUCT` (944), set/list operations (598) —
have dropped out of the table entirely.

**Consequence for how to work:** this plan's premise, that a small number of
core mechanisms bind everything, is now *less* true than it was. The next
ranking should be re-derived from the checklist rather than inherited from the
list below, and [Tier 3](#tier-3--the-genuine-tail)'s per-test work becomes
correct sooner than previously assumed.

### Files failing 100% — the strongest mechanism-absent signal

**49 files fail every single test they contain (493 tests, 2026-08-16).** More
files, fewer tests: the big totally-failing files have broken up, and what is
left is a longer tail of smaller ones. `conditions/define-condition.lsp` (56/56),
`iteration/loop6.lsp` (47) and `loop7.lsp` (35) — the top three at 08-15 — are
all gone from this list. A file at 100% is
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
| 30 | `packages/make-package.lsp` | package model | C10 |
| 30 | `cons/pushnew.lsp` | place protocol | C16 |
| 29 | `hash-tables/make-hash-table.lsp` | `:test` as a designator | X2 |
| 27 | `packages/defpackage.lsp` | package model | C10 |
| 26 | `objects/defmethod.lsp` | CLOS | C8 |
| 25 | `printer/format/format-logical-block.lsp` | pretty printer | C2 |
| 22 | `pathnames/make-pathname.lsp` | pathname model | C11 |
| 20 | `printer/print-cons.lsp` | printer | C7 |
| 17 | `printer/pprint-exit-if-list-exhausted.lsp` | pretty printer | C2 |
| 17 | `cons/ldiff.lsp` | `LDIFF` absent | C19 |
| 14 | `printer/print-backquote.lsp` | printer | C7 |
| 13 | `system-construction/modules.lsp` | `PROVIDE`/`REQUIRE` absent | C11 |
| 13 | `pathnames/pathname.lsp` | pathname model | C11 |
| 13 | `hash-tables/with-hash-table-iterator.lsp` | hash iterator absent | C18 |
| 12 | `reader/set-syntax-from-char.lsp` | readtable | C12 |
| 12 | `objects/defclass-03.lsp` | CLOS | C8 |
| 11 | `printer/format/format-justify.lsp` | `~<~>` | C2 |
| 10 | `cons/tailp.lsp` | `TAILP` absent | C19 |
| 9 | `streams/stream-element-type.lsp` | `STREAM-ELEMENT-TYPE` absent | C19 |
| 9 | `conditions/check-type.lsp` | `CHECK-TYPE` absent | C19 |

**`packages/` now owns the top two entries** (57 tests across `make-package.lsp`
and `defpackage.lsp`), which makes the package model [C10](#c10-package-model)
the best cost/benefit entry in the suite — the position
`conditions/define-condition.lsp` held at 08-15 and has now vacated entirely.
`cons/pushnew.lsp` at 30/30 is second and belongs to M5's place protocol.

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

#### C2. `FORMAT` / `FORMATTER` — **still the largest single family (417)**

**Evidence (2026-08-16): 417 failures**, down from 1623 at 08-15 — the engine's
iteration/escape/justification/padding half landed and `printer/` is now 52.2%.
It remains the biggest *family*, but it no longer dominates the suite, and the
adjacent `PRINT.INTEGERS.BASE`/`RADIX.BASE` pair (161) is printer radix
handling rather than FORMAT. **Historical evidence below (08-12): 1623 failures** (`grep -cE "^FORMAT" ansi_results/failed.txt`),
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

#### C6. Arrays — **LARGELY DONE (2026-08-15 d)**

**`run_ansi.py arrays`: 518 → 1233 passing of 1356. +715, and the failures fell
838 → 123.** Details in the [Changelog](#changelog).

**The cluster shape was right and the owner was wrong.** This section read the
574 failures as "the array *object model* lacks these properties" and assigned
them to M9 (types/CLOS). The model was indeed absent — but it was absent in a
specific way that made it its own milestone-sized item rather than a corollary
of the type system: there were **three unrelated Python shapes** for an array
(`AdjustableVector`, `Array`, a bare `list`), none of which recorded an element
type, *and* the operators were **duplicated across five modules**, with import
order deciding which copy ran. Building one object model with one home for the
operators moved the type-system work (`TYPEP` on `(array et dims)`) along with
it, because the type predicates could finally ask the object.

**Still open** (123 failures): `SUBTYPEP` has no lattice, so
`UPGRADED-ARRAY-ELEMENT-TYPE.8`'s consistency check cannot pass ([C14](#tier-2--subsystem-gaps));
`(upgraded-array-element-type nil)`; argument-evaluation order for `(setf
(svref ...))`; and the residual of `make-array.lsp`, which is now blocked on a
*macro-lambda-list* defect rather than on arrays — see
[Discovered issues](#discovered-2026-08-15-d).

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

**Method combination landed 2026-08-18** — see the [Changelog](#changelog).
`run_ansi.py objects`: **298 → 413 passing of 862** (34.6% → 47.9%), 0
regressions. The `DEFGENERIC-METHOD-COMBINATION.*` cluster went **95 → 4** and
`DEFINE-METHOD-COMBINATION*` **20 → 0**. **The rest of C8 is untouched.**

**Remaining evidence (2026-08-18, `run_ansi.py objects`).** 449 failures:
`DEFGENERIC` 49, `DEFCLASS` 26, `DEFMETHOD` 26, `MAKE-LOAD-FORM` 26,
`SHARED-INITIALIZE` 21, `CHANGE-CLASS` 20, `WITH-SLOTS` 16,
`ENSURE-GENERIC-FUNCTION` 14, `WITH-ACCESSORS` 12, `REMOVE-METHOD` 11 — plus
`RuntimeError: CALL-NEXT-METHOD: No next method available` (18) and
`AttributeError: Slot A not found` (10). `types-and-classes/` 337 of 545.

**Two things now block most of the remainder, and neither is "CLOS" as a
whole:**

- **There is no class precedence list.** `classes._specificity_key` orders
  applicable methods by *ancestor count*, and the live `_init_builtin_classes`
  makes every built-in class a direct subclass of `T` — so `INTEGER`,
  `RATIONAL` and `NUMBER` are all equally specific and only the (stable)
  definition order separates them. It is why the four residual
  `DEFGENERIC-METHOD-COMBINATION.*.7` tests fail: they are the ones that
  dispatch over the multiply-inheriting `dgmc-class-04 (dgmc-class-02
  dgmc-class-03)`. A real C3 linearization plus a real built-in class
  hierarchy is one mechanism serving [C14](#tier-2--subsystem-gaps)'s type
  lattice as well, and it is the next-largest CLOS item on the evidence.
- **`DEFGENERIC`'s lambda-list congruence is unchecked**, which is 18 of the
  22 `DEFGENERIC.ERROR.*` tests. That is M3's lambda-list engine, not CLOS.

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

#### C10. Package model — **LARGELY DONE (2026-08-16 targeted, not yet in a full run)**

`run_ansi.py packages`: **201 → 373 passing of 500** (40.2% → 74.6%), 0
per-file regressions against `docs/ansi_checklist_baseline.json`.
`make-package.lsp` and `defpackage.lsp` — the two largest 100%-failing files in
the whole suite — are now 37/51 (72.5%) and 25/28 (89.3%).

**The mechanism was "a Python object leaking as a Lisp value," found five
times over** (plan.md Finding M's pattern again): `PACKAGE-NICKNAMES`,
`PACKAGE-USE-LIST`, `PACKAGE-USED-BY-LIST`, `PACKAGE-SHADOWING-SYMBOLS`,
`PACKAGE-EXTERNAL-SYMBOLS`, `PACKAGE-INTERNAL-SYMBOLS`, `LIST-ALL-PACKAGES` and
`FIND-ALL-SYMBOLS` all returned a bare Python `list`. A Python `list` is a
*vector* here (plan.md Finding M / `sequence_protocol.make_lisp_list`'s own
docstring), so `(equal (package-nicknames p) nil)` compared an empty vector to
NIL and was false regardless of what the package under test actually
contained — the harness could not observe a correct answer from these
accessors even when the underlying package model was right. Fixed by routing
every one of them through a shared `_lisp_list` builder.

**A second, independent mechanism sat behind that one: at least six separate
copies of "resolve a package/string designator" existed** (`MAKE-PACKAGE`,
`IN-PACKAGE`, `INTERN`, `FIND-SYMBOL`, `FIND-PACKAGE`, `EXPORT`,
`DELETE-PACKAGE`, `coerce_to_package` itself), each hand-rolling its own
`isinstance(x, lispKeyword) ... elif LispSymbol ... else str(x)` chain. Every
copy agreed on strings/symbols/keywords and *disagreed* on everything else
(characters, and every specialized character-array shape ANSI's
`make-package.lsp` deliberately exercises as a name designator) — so
`(make-package name)` and `(in-package name)` for the identical `name` value
silently resolved to two *different*, wrongly-named packages. Consolidated
onto one `_designator_to_string` (misc_packages.py) and `coerce_to_package`
now calls it, so every caller of `coerce_to_package` was fixed at once instead
of six times. **`DEFPACKAGE` itself was a hardcoded, incomplete branch in
`evaluation_core.py`'s `eval()` dispatch** (correctly *not* a `cl_function`,
per the registry note in CLAUDE.md, since its option clauses must not be
evaluated) supporting only `:USE`/`:NICKNAMES`/`:INTERN`/`:EXPORT`; rewritten
to also handle `:SHADOW`, `:SHADOWING-IMPORT-FROM`, `:IMPORT-FROM`, `:SIZE`,
`:DOCUMENTATION`, nickname-clash and CLHS 7.2's disjointness checks, and to
merge (not overwrite) repeated `:NICKNAMES` clauses. `SHADOW` and
`SHADOWING-IMPORT` were both complete stubs (`return T`); `IMPORT` re-interned
a same-named symbol instead of preserving the given symbol's identity, so an
imported symbol was never `EQ` to the one it was imported from.

**One more defect surfaced only once `LIST-ALL-PACKAGES` returned every
package for real:** exporting a name a package only *inherited* (via `:USE`)
left `Package.intern`'s inheritance branch returning the inherited symbol
without ever adding it to the exporting package's own `symbols`, so
`FIND-SYMBOL` kept answering `:INHERITED` instead of `:EXTERNAL` — CLHS
11.1.2.1.2 requires the symbol become directly present. Fixed in
`lisptype_extended.Package.intern`.

**Cross-cutting bug found while fixing this, not specific to packages:**
`IGNORE-ERRORS`'s non-error path unconditionally returned `(values result
NIL)` instead of passing `result` through unmodified, so
`(multiple-value-list (ignore-errors (values 1 2 3)))` answered `(1 NIL)`
instead of `(1 2 3)` — CLHS: "if execution completes normally, ignore-errors
returns whatever values the forms return." This alone unblocked four
`defpackage.lsp` tests with no package-specific change at all, which is the
plan's own signal that a fix crossed from symptom to mechanism.

**Discovered, not fixed (separate mechanisms):** (1) two tests that regressed
from "passing" to failing *because* `LIST-ALL-PACKAGES` now enumerates every
package for real — `FIND-ALL-SYMBOLS.1` (a consistency check across every
`FCLPY-INTERNAL` implementation symbol) and `WITH-PACKAGE-ITERATOR.12/13/14`
(errors on unrelated leftover packages, e.g. an "Undefined function G22468"
gensym artifact) — both were vacuous passes before (LOOP over a bare Python
list iterated zero times), not real coverage, and now fail honestly; (2)
`intern_keyword` force-uppercases its argument, so a pipe-escaped lowercase
keyword designator (`:|f|`) loses its case before any designator-resolution
code ever sees it (`DEFPACKAGE.6`'s remaining failure) — a reader/interning
defect, not a package-model one; (3) `MAKE-PACKAGE.ERROR.1-4` and
`DEFPACKAGE.24/25` need a *continuable* `PACKAGE-ERROR` (a working
restart/`CERROR` chain) that M8 owns — `PackageError` is now signaled
correctly via `signal_error_object` rather than crashing the run (a bare
`raise` bypassed `signal_condition` entirely, since `PackageError` is in the
*real* condition hierarchy, not the "legacy" `lisptype.LispError` one
`HANDLER-CASE`/`IGNORE-ERRORS` also catch directly), but nothing in this
implementation can yet make the handler *continue*.

**Owner:** M1. **Verify:** `run_ansi.py packages`. Previous evidence, now
superseded: `MAKE-PACKAGE` 51, `DEFPACKAGE` 27, `UNUSE-PACKAGE` 23,
`PACKAGE-NAME` 21, `USE-PACKAGE` 20 failing; `packages/` 198 failing of 340
with 70 unrun. Known specifics not yet addressed above: CL/CL-USER/KEYWORD
missing from `state.packages`; `RENAME-PACKAGE`; and **`INTERN` case-folds its
string argument** — case conversion is the *reader's* job via
`readtable-case` (CLHS 23.1.2), so `(eq (intern "myvar") (intern "MYVAR"))`
must be NIL.

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
11. ~~**C6 — the array object model.**~~ **Largely done 2026-08-15 (d).**
   `arrays` **518 → 1233 of 1356**, and it was mis-owned: [C6](#c6-arrays--largely-done-2026-08-15-d)
   ranked it under M9 as a corollary of the type system, but the model was
   three unrelated Python shapes plus five competing copies of the operators,
   and building it *supplied* the type-system half rather than depending on it.
12. **The next task, on this run's evidence: a nested destructuring pattern in
   a `defmacro` lambda list (M3/M4).** `(defmacro m ((&rest vars) form &body
   body) ...)` does not bind `vars`, and ansi-aux defines
   `multiple-value-bind*` exactly that way — so every harness check that uses
   it fails inside the *harness*, not in the code under test. It is 90 of the
   118 tests still failing in `arrays/make-array.lsp` by itself, the pattern is
   idiomatic throughout `auxiliary/`, and it is the same class of
   measurement gate as items 4 and 6. Confirm the blast radius first with
   `grep -rn "multiple-value-bind\*" ../ansi-test | wc -l`.
13. **Re-measure, then re-derive this list.** The residual distribution has
   already shifted enough that ranking further ahead is guesswork. On the
   current evidence the next-largest unblocked mechanism is
   [C2](#c2-format--formatter--largest-cluster-in-the-suite)'s remaining
   `FORMAT` directives (`~E`, `~F`, `~R`, `~T`), with M3's lambda-list engine
   close behind now that it owns the last binding form that does not go
   through `BindingFrame` ([§5](#5-known-temporary-deviations)).

### Re-derived from the 2026-08-16 run

Items 1–13 above are history; this is the current order. **The distribution has
flattened** — no cluster now exceeds 6% of the remainder — so the argument for
"one mechanism unblocks everything" is weaker than at any previous point, and
these are ranked on measured evidence rather than on that premise.

14. ~~**Attribute `types-and-classes`' 14.1-point fall, and the ~11-file
    transcendental/float regression cluster.**~~ **Both attributed; neither
    is live breakage.** `types-and-classes` is the `unless` repair making
    previously-vacuous `SUBTYPEP` assertions run and fail honestly against
    C14's known-absent type lattice — diagnosed in
    [preventing regression](#preventing-regression) and accepted there in
    writing. **The transcendental/float cluster does not exist** (checked
    2026-08-16 c): all fourteen of its files run **250 passing of 254** on
    `run_ansi.py`, and the four failures are two unrelated defects —
    `MIN.27`/`.28` are LOOP's documented bucket-order execution (a C1
    follow-up), and `RATIONALIZE.1`/`.3` are one genuine `RATIONALIZE`
    defect that neither round-trips nor handles denormals (C15). The
    "uniform small delta across eleven numeric files" was an artefact of
    diffing against a **three-run-old** baseline, which is the cost
    [§7](#preventing-regression)'s process lesson already named. **The
    standing action from this is [§7](#preventing-regression)'s, not a code
    fix: save a dated snapshot of `ansi_results/` beside each full run** so
    the next mechanism change can be attributed against the run before it
    rather than against 08-12.
15. ~~**[C10](#c10-package-model), the package model.**~~ **Largely done
    (2026-08-16 targeted; not yet folded into a full run).** `packages/`
    **201 → 373 of 500 (74.6%)**, 0 regressions; `make-package.lsp` and
    `defpackage.lsp` — the two 100%-failing files this item was ranked for —
    are now 72.5% and 89.3%. Still open: `RENAME-PACKAGE` (a stub),
    `INTERN`'s case-folding, `DELETE-PACKAGE`/`DO-SYMBOLS`/`WITH-PACKAGE-ITERATOR`
    edge cases, and the two M8-owned continuable-`PACKAGE-ERROR` test pairs.
    It is also M1, i.e. a prerequisite for the ASDF rung in
    [§7](#7-acceptance--the-ecosystem-ladder).
16. ~~**[C2](#c2-format--formatter--largest-cluster-in-the-suite)'s remaining
    directives**, plus the adjacent `PRINT.INTEGERS.BASE`/`RADIX.BASE` pair at
    161 — the printer's radix/base handling.~~ **The second half was
    misdiagnosed and is now done (2026-08-16 b).** The 161 were not radix/base
    handling — that was already correct, measured before any change — they were
    `(copy-readtable nil)` raising underneath `my-with-standard-io-syntax`, so
    `printer/print-integers.lsp` went **0 → 189 of 194** by fixing the
    *readtable object model*. **`FORMAT`'s remaining directives (`~E`, `~F`,
    `~R`, `~T`, logical blocks) are still open and still the largest single
    family at 417.** The lesson is [§3](#the-checklist)'s own: a 100%-failing
    file names an absent mechanism, and the mechanism is not always in the
    subsystem the test names belong to.
16a. ~~**`WITH-STANDARD-IO-SYNTAX` (new, and the cheapest thing in the suite).**~~
    **Done 2026-08-16 (c).** It established *no* bindings, because it was a
    `cl_function`. It is now a `cl_macro` expanding to the `LET` of CLHS
    23.4's twenty-one bindings, so `BindingFrame` does the binding and there
    is no second mechanism. Measured, same runner both sides over eight files
    that use it: **122 → 142 passing of 182**, failures 60 → 40, **0 newly
    failing**; `reader/with-standard-io-syntax.lsp` **19 → 1 failing of 23**.
    Details in the [Changelog](#changelog). **The predicted blast radius did
    not materialise, and that is the finding:** it was ranked here as a
    *gate* in front of the pretty-printer files, on the strength of 455 uses
    across 58 files. Only 2 of the 20 recovered tests were outside the file
    that tests the operator itself — the pretty-printer files are blocked on
    the pretty printer *being absent*, not on the gate, and `def-pprint-test`
    binds `*print-pretty*` to T immediately after this macro sets it to NIL.
    Cheap and correct, but a symptom-sized fix by [§2](#the-development-loop)
    step 7's own test.
17. **[C8](#c8-clos--defgeneric--defmethod--defclass--change-class), CLOS.**
    `objects/` is 28.0% and barely moved (+1.7) across a run that moved
    everything else. Two implementations still coexist (Finding L), so
    consolidate before fixing.
    **First slice done 2026-08-18: method combination** (CLHS 7.6.6), which
    was absent — `objects` **298 → 413 of 862**, 0 regressions. Details in
    the [Changelog](#changelog).
    **The disappeared-failures signal came back negative, and that is
    honest rather than disappointing.** [§2](#the-development-loop) step 7
    asks which failures moved that were *not* targeted; here, none did — all
    115 are in the eleven files that test method combination. That is what
    an *absent operator* looks like as opposed to a *broken shared
    mechanism*: nothing else could have been depending on a combination
    type that did not exist. The eleven files were still the right unit of
    work — one mechanism, not 115 bugs — but this is [§3](#the-checklist)'s
    "the distribution has flattened" playing out, and it is evidence for
    the [policy-change trigger](#policy-change-trigger-under-3000-failures-and-under-100-files)
    rather than against it.
17a. **The class precedence list (new, and now the largest CLOS item).**
    Ranked here on the evidence method combination exposed: with the
    combination machinery correct, the four `DEFGENERIC-METHOD-COMBINATION.*.7`
    tests that still fail are exactly the ones dispatching over a
    multiply-inheriting class, and `classes._specificity_key` cannot order
    them because the live `_init_builtin_classes` gives every built-in class
    `T` as its only superclass. Two consequences worth ranking on: the
    ordering of applicable methods is currently decided by *definition
    order* wherever the ancestor counts tie (which is most of the time, and
    is why so much of `objects/` passes by luck), and `SUBTYPEP` has no
    lattice to consult either — so this is one mechanism serving
    [C8](#c8-clos--defgeneric--defmethod--defclass--change-class) and
    [C14](#tier-2--subsystem-gaps) at once. It also retires the duplicate
    `_init_builtin_classes` (standing rule 3).
18. **The reader ([C12](#tier-2--subsystem-gaps)), 21.8% and the worst rate in
    the suite.** Newly concrete: **ratios do not read** — `3/5` comes back as an
    unbound *variable*, not a number.

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
| `~&` sees only the column within its own control string, so a `~&` opening a control string cannot tell the stream is mid-line; `FRESH-LINE` is correct | FORMAT builds its whole output as a string before writing, and the column is not threaded through the eleven nested `_format_process_cursor` call sites | C2 |
| `SUBTYPEP` string-pair table | no type lattice | M9 |
| **No class precedence list.** `classes._specificity_key` orders applicable methods by *ancestor count*, and the live `_init_builtin_classes` gives every built-in class `T` as its only superclass — so `INTEGER`, `RATIONAL` and `NUMBER` are equally specific and ties are broken by *definition order* (a stable sort). Much of `objects/` therefore passes because ansi-test happens to define its methods most-specific-first | CLHS 7.6.6.1 wants the argument's class precedence list position, which needs a real C3 linearization *and* a real built-in class hierarchy; both are the same mechanism as C14's type lattice, so doing it here would be a second one | M9 / §4 item 17a |
| `classes.py` defines `_init_builtin_classes` twice; the second wins and the first is dead | standing rule 3, unresolved — the two disagree about the class hierarchy, which is exactly why it matters | M9 / §4 item 17a |
| The reader does not parse **ratios**: `3/5` reads as a symbol, so it evaluates as an unbound variable | found 2026-08-16 while probing `*mini-universe*`, whose ratio entry is therefore not a ratio | M10 / C12 |
| `WITH-STANDARD-IO-SYNTAX` binds `*PRINT-PPRINT-DISPATCH*` to a dispatch table that dispatches nothing | the *object* now has one home (`io_write.standard_pprint_dispatch`) and the binding is correct, but `SET-PPRINT-DISPATCH`/`PPRINT-DISPATCH` are stubs, so `WITH-STANDARD-IO-SYNTAX.23` cannot pass. It is the pretty printer's absence, not the macro's | C2 / M10 |
| `SET-SYNTAX-FROM-CHAR` returns T without doing anything, and there is no character *syntax type* model for it to act on | `Readtable` records macro characters and a case and nothing else; building syntax types is the bulk of what `reader/` still owes | M10 / C12 |
| The reader upcases every symbol token regardless of `readtable-case`, though the readtable records it and the printer honours it (CLHS 23.1.2) | `Readtable._read_symbol` predates the readtable being a real object | M10 / C12 |
| `MAKE-LIST`/`MAKE-SEQUENCE` refuse a size above `CONSTRUCTIBLE_LIMIT` (2**30) with a plain error rather than a `STORAGE-CONDITION` | CLHS 4.4 permits refusing, but the condition *type* should be `STORAGE-CONDITION` once the class lattice exists | M8 / M9 |
| `EQUAL` descends a *general* vector element-wise | CLHS 5.3 descends only conses, strings, bit vectors and pathnames, so `(equal #(1 2) #(1 2))` must be NIL. Conses, strings and bit vectors are now right; the general-vector branch predates them and turning it off changes the answer for a heavily-used predicate, which should be its own measured change | M6 |
| A *displaced* character vector is a `LispArray`, not a `LispString`, so the STRING-specific operators do not accept it | `LispString` stores its characters directly, and threading displacement through it means a second indirection in every string access; every other character array (fill-pointered and adjustable included) is a `LispString` | M9 |
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

#### Open regressions carried by the 2026-08-16 full run

The 08-16 run is **+3224 overall but worse in 75 files** against
`docs/ansi_checklist_baseline.json`. They are listed here rather than absorbed
into the total, because refreshing the baseline is what makes a regression
invisible — `--save-baseline` has **deliberately not been run**, so the gate
still points at 08-12 and the evidence survives.

**Read the 75 with the baseline's age in mind.** The baseline is the **08-12**
run — now *three* full runs old — so this count folds together the 19 files
already documented as regressed at 08-15 with anything new. The two cannot be
separated from the artifacts on disk, because `ansi_results/failed.txt` is
overwritten by each full run and no 08-15 snapshot was saved.

| files | Δ vs 08-12 baseline | note |
|---|---|---|
| `characters/char-compare.lsp` +2, `characters/character.lsp` +3 | +5 | carried from 08-15, still open. The *directory* has recovered (57.5% → 58.3%) but these two files have not |
| `numbers/log.lsp` +2, `lcm.lsp` +3, `asin.lsp` +2, `acos.lsp` +2, `rationalize.lsp` +2 | +11 | carried from 08-15, still open |
| `numbers/atan.lsp` +3, `sin/cos/tan.lsp` +1 each, `acosh.lsp` +1, `phase.lsp` +1, `logbitp.lsp` +2, `min.lsp` +1, `make-random-state.lsp` +1 | +11 | **new at 08-16.** A transcendental/float cluster next to the one above; the shape says one shared defect, not eleven |
| `cons/cxr.lsp` +2, `sublis.lsp` +1, `nthcdr.lsp` +1, `endp.lsp` +1, `rplaca/rplacd.lsp` +1 each | +7 | `sublis` carried from 08-15; the rest are new and small |
| `objects/defclass-01.lsp` +1, `streams/write-line.lsp` +1 | +2 | isolated |
| ~~`data-and-control-flow/every.lsp`, `notevery.lsp`~~ | ~~+2~~ | **Resolved:** 21→7 and 20→6, well past baseline |

**`types-and-classes` at −14.1 points is the largest single regression and is
not yet in this table**, because it is a *rate* change spread across files
rather than a per-file count. **Diagnosed 2026-08-16 (confirms the
hypothesis): this is the `unless` repair making previously-vacuous assertions
run and fail honestly, not new breakage.** Every one of the large SUBTYPEP
regressions (`subtypep-member.lsp` +32, `subtypep-real.lsp` +19,
`subtypep-cons.lsp` +15, `subtypep-rational.lsp` +12, `subtypep-eql.lsp` +5,
`subtypep.lsp` +1) is a `(loop ... unless (equal (subtypep* ...) '(t t))
collect ...)` or `check-equivalence` form: before the `unless` fix, the
`SUBTYPEP*` call inside the loop's test was never evaluated, so the loop
always collected `NIL` regardless of what `SUBTYPEP` actually returned —
`subtypep-real.lsp`'s `SUBTYPEP.REAL.1` alone drives 121 `SUBTYPEP*` pairs
that all silently passed. Once `unless` evaluates its test, the pairs are
genuinely checked against [C14](#tier-2--subsystem-gaps)'s known-absent type
lattice (`SUBTYPEP` is a string-pair table with no interval/lattice logic for
bounded `(real lo hi)`/`(integer lo hi)`/`(rational lo hi)` types), and fail
honestly. **Action per this section's own rule: accept this in writing rather
than "fixing" it here** — the correct fix is C14's type lattice (owned by M9),
not a patch local to this regression. `types-and-classes/subtypep-array.lsp`
(−8) and `coerce.lsp` (−7) improving in the same run corroborate the
diagnosis: `coerce.lsp` exercises the same numeric-tower functions as the
2026-08-16(b) fixes below, so the same `unless` mechanism cuts both ways —
honest failures where the lattice is missing, honest passes where the
underlying operator was already correct.

**Process lesson, unchanged from 08-15 and now more expensive:** a mechanism
change should be measured against the run before it, which requires saving that
run's per-file counts. `--save-baseline` is full-run-only *and* the gate for
attributing the next run — refusing to refresh it protects the old evidence but
also means every future diff is measured against an ever-staler point. **Save a
dated snapshot of `ansi_results/` alongside each full run** so the two concerns
stop competing.
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

- **2026-08-18** — **Method combination exists (CLHS 7.6.6).** A generic
  function had no method combination at all: `DEFGENERIC` parsed `:METHOD`
  and `:DOCUMENTATION` and dropped every other option on the floor, and
  `call_generic_function` hard-coded standard combination's four qualifier
  buckets. So `(:method-combination progn)` silently produced a *standard*
  generic function, and each of its `progn`-qualified methods matched none
  of `{}`/`{BEFORE}`/`{AFTER}`/`{AROUND}` and was **discarded without a
  diagnostic** — the call then failed with "no applicable method" naming
  nothing that had gone wrong (standing rule 4, twice over).
  `DEFINE-METHOD-COMBINATION` existed **twice** — a special form in
  `evaluation_special_forms.py` and a `cl_function` in `utilities_errors.py`
  — and neither copy defined anything: both built an anonymous Python object
  and bound it as a *variable* under the combination's name (standing rules
  2 and 3).
  **The mechanism is a method-combination object with one invocation
  primitive under it.** `classes.call_method(method, next_methods, args)` is
  now the only place a method is ever invoked, and the frame it pushes is
  the one `CALL-NEXT-METHOD`/`NEXT-METHOD-P` read. That replaced a frame
  carrying a `kind` discriminator plus, for `:around`, its own `core`
  closure — which is why `NEXT-METHOD-P` answered T inside *every* `:around`
  method whether or not anything remained, and why nothing except standard
  combination could build a chain at all. A generic function now holds a
  `MethodCombination`; `None` means standard, not "none".
  **The effective method is a *form*, and that is forced rather than
  stylistic.** Standard combination assembles its chain in Python because its
  shape is fixed, but the short form (CLHS 7.6.6.4's nine built-ins, plus
  `DEFINE-METHOD-COMBINATION`'s short form) and the long form build
  `(operator (call-method m1) (call-method m2) ...)` and evaluate it,
  because the operator may be a **macro whose evaluation order is the
  semantics**: `defgeneric-method-combination.and.1` asserts that the methods
  after the first NIL never run. Folding the method results in Python gives
  the right answer for `PROGN` and `LIST` and the wrong one for `AND`/`OR`.
  That in turn required `CALL-METHOD` and `MAKE-METHOD` to become real
  special operators — they were `cl_function`s that evaluated both operands
  (one of which is an unevaluated method body) and then ignored
  `next-method-list` entirely, i.e. the registry defect `CLAUDE.md` names, in
  the one place where it makes CALL-NEXT-METHOD structurally impossible.
  Building the form out of *interned* `COMMON-LISP` symbols matters for the
  same reason it did in 08-16 (c): lookup is by symbol identity.
  **Two defects surfaced only once the machinery worked**, both found by the
  regression diff rather than by aiming at them: (1) "no applicable methods"
  is decided **before** the combination is consulted (CLHS 7.6.6) — a
  long-form body mapping over an empty method group otherwise cheerfully
  returns `#()`; (2) CLHS's `:arguments` lambda list binds its variables to
  **forms**, not values, so a `&rest` list of `(:z1 4)` spliced into the
  effective method was evaluated as a call to the function `:Z1`. Each
  variable is now bound in the body's environment to its own symbol, and
  that symbol to the argument value in the environment the resulting form is
  evaluated in.
  **Measured, same runner both sides:** `run_ansi.py objects` **298 → 413
  passing of 862** (34.6% → 47.9%), **0 regressions** verified by diffing
  the passed sets across a `git stash` of the change.
  `DEFGENERIC-METHOD-COMBINATION.*` **95 → 4**, `DEFINE-METHOD-COMBINATION*`
  **20 → 0**, `DEFGENERIC.ERROR.*` 22 → 18 (the option-validation four;
  the rest want lambda-list congruence, which is M3's).
  **The disappeared-failures signal was negative** — every recovered test is
  in the eleven files that test method combination — and
  [§4 item 17](#re-derived-from-the-2026-08-16-run) records why that is the
  expected shape for an *absent operator* rather than a reason to distrust
  the fix.

- **2026-08-16 (c)** — **`WITH-STANDARD-IO-SYNTAX` establishes its bindings,
  and a predicted gate turns out not to be one.** It was a `cl_function` whose
  body was "evaluate every argument eagerly, return the last", so it
  established **none** of the twenty-one bindings CLHS 23.4 gives it —
  `(let ((*print-base* 2)) (with-standard-io-syntax (prin1-to-string 5)))`
  answered `"101"` where ANSI requires `"5"`. That is exactly the registry
  defect `CLAUDE.md` names: a form whose subforms must run in a *modified*
  dynamic environment cannot be a `cl_function`, because `cl_function`
  evaluates them before the form runs at all. It is now a `cl_macro` in
  `evaluation_special_forms.py`, beside the WITH-*-STRING expanders that were
  the same defect, expanding to the `LET` of CLHS 23.4's binding list. **This
  deliberately adds no binding mechanism:** every one of the twenty-one
  variables is proclaimed special by `lispenv.STANDARD_SPECIAL_VARIABLES`, so
  `BindingFrame` already binds them in their value cells, and the form's
  value, its multiple values and any non-local exit out of it are LET's —
  which is what `WITH-STANDARD-IO-SYNTAX.19/.20/.21/.22` check.
  **Two details the expansion cannot get wrong quietly.** The binding
  variables are the *interned* `COMMON-LISP` symbols, not bare
  `LispSymbol(...)`: a global variable's home is the symbol's own value cell
  and lookup is by symbol *identity*, so a freshly built `*PRINT-BASE*` would
  be bound and read as a different variable from the one the printer
  consults. And `*PACKAGE*` binds `(find-package "COMMON-LISP-USER")` by name
  rather than to whatever the caller had — `WITH-STANDARD-IO-SYNTAX.1` checks
  precisely that.
  **One object model came with it.** CLHS 23.4 binds
  `*PRINT-PPRINT-DISPATCH*` to "the standard pprint dispatch table", and
  there was no such object to name: `COPY-PPRINT-DISPATCH` answered a bare
  Python `dict` (standing rule 2) and `lispenv` built the initial table from a
  class declared *inline inside* `setup_standard_environment`, so nothing
  else could reach the object the macro has to rebind to.
  `io_write.standard_pprint_dispatch()` is now its one home, the same shape
  as `readtable.standard_readtable()`, and `COPY-PPRINT-DISPATCH` resolves
  NIL to it and raises on anything else rather than answering a dict.
  **Measured, same runner both sides**, over eight files that use the macro
  (`reader/with-standard-io-syntax.lsp`, `printer/print-level.lsp`,
  `pprint-indent.lsp`, `print-strings.lsp`, `write.lsp`, `print-characters.lsp`,
  `format/format-x.lsp`, `format/format-o.lsp`; 182 registered both sides):
  **122 → 142 passing, failures 60 → 40, and every failure that remains is a
  strict subset of the ones before — 0 newly failing.**
  `reader/with-standard-io-syntax.lsp` **19 → 1 failing of 23**. `pytest`
  **1669 → 1699 passed**, same single pre-existing `STREAM-ELEMENT-TYPE`
  failure; the 30 new tests are `tests/test_with_standard_io_syntax.py`, and
  they include the architectural guards [§7](#preventing-regression) asks
  for — the registration is a macro, and no competing special-form
  registration exists.
  **Checklist effect:** 6776 → **6756** failing (−20), files with failures
  544 → 543, merge recorded as `fixed 20, regressed 0, new 0`, and the
  `REGRESSION` marker count against the 08-12 baseline stays at **55** —
  none added. `printer/print-level.lsp` leaves the list entirely.
  **The prediction was wrong, and the way it was wrong is the point.**
  [§4](#recommended-order) item 16a ranked this as a *measurement gate* in
  front of the pretty-printer files, on 455 uses across 58 files — the shape
  items 4, 6, 16 and the COND fix all had. It is not one. Across the **8 of
  those 58 files** measured here — the heaviest users that finish in
  reasonable time — only **2** of the 20 recovered tests were outside the
  file that tests the operator itself
  (`printer/print-level.lsp` 2 → 0, untargeted). The pretty-printer files are
  blocked on the pretty printer *being absent*, not on the gate:
  `def-pprint-test` binds `*print-pretty*` to T in the very next form, so
  what this macro sets it to never mattered to them. By [§2](#the-development-loop)
  step 7's own test — how many failures disappeared that you did not target —
  this is a symptom-sized fix, and the four preceding gate-shaped wins made
  it look larger than it was. **`printer/`'s remaining failures belong to the
  pretty printer and to `FORMAT`'s missing directives, not to their
  preamble.**
  **Discovered, not fixed:** `WITH-STANDARD-IO-SYNTAX.23` needs
  `SET-PPRINT-DISPATCH`/`PPRINT-DISPATCH` to actually dispatch — both are
  stubs returning NIL, so the table's *contents* are still unobservable even
  though its identity now is.
  **And item 14's transcendental/float "regression cluster" does not exist.**
  All fourteen of its files run **250 passing of 254** on `run_ansi.py`
  (`atan`/`sin`/`log`/`acos`/`lcm`/`min` 123 of 125; `tan`/`cos`/`asin`/
  `acosh`/`phase`/`logbitp`/`rationalize`/`make-random-state` 127 of 129),
  and the four failures share no mechanism with each other, let alone one
  float defect:
  - `MIN.27`/`.28` are
    `(loop for i ... for x = (make-list i ...) do (setf (elt x (random i)) 0)
    unless (eql (apply #'min x) 0) collect x)`. `(apply #'min '(1 0 1))`
    answers `0` correctly in isolation; they fail on **LOOP's documented
    bucket-order execution** ([§5](#5-known-temporary-deviations)), which
    evaluates the `unless` test before the `do` that plants the zero, so it
    reads an all-ones list. A C1 follow-up.
  - `RATIONALIZE.1`/`.3` are a real **`RATIONALIZE`** defect: CLHS requires
    the simplest rational that reads back as the *same* float, and this one
    neither round-trips (`(float (rationalize x) x)` ≠ `x`) nor handles
    denormals — `(rationalize 1.4e-45)` is `0`. C15/Phase 4, one operator.

  So the +1/+2 per-file deltas item 14 reads as one shared defect are an
  artefact of measuring against a **three-run-old** baseline, not live
  breakage — which is the cost [§7](#preventing-regression)'s own "process
  lesson" predicted when it noted that refusing to refresh the baseline
  protects old evidence at the price of attributing every future diff
  against an ever-staler point.

- **2026-08-16 (b)** — **The readtable becomes an object with one home, and a
  fourth measurement gate falls.** `(copy-readtable nil)` raised
  `AttributeError: 'lispNull' object has no attribute 'copy'` *as the value of
  the form* (standing rule 2). ansi-test's `my-with-standard-io-syntax` binds
  `*readtable*` to exactly that, `def-print-test` is built on
  `my-with-standard-io-syntax`, and **189 of the 194 tests in
  `printer/print-integers.lsp` are `def-print-test`s** — so every one of them
  failed regardless of what the printer did.
  **The printer was not at fault, and [§4](#recommended-order) item 16 said it
  was.** That item attributes the 161 `PRINT.INTEGERS.BASE` /
  `PRINT.INTEGERS.RADIX.BASE` failures to "the printer's radix/base handling".
  Measured before changing anything: `(prin1-to-string 1)` is `"1"`,
  `(let ((*print-base* 2)) (prin1-to-string 5))` is `"101"`, and
  `(let ((*print-radix* t)) (prin1-to-string 5))` is `"5."` — the radix/base
  half was already correct. This is [§4](#recommended-order) items 4 and 6 and
  the COND fix a **fourth** time: rank a cluster by its failure count and you
  rank the gate in front of it, not the defect.
  **Six defects, one mechanism: there was no readtable *object model*.**
  (1) **No standard readtable existed.** CLHS 23.1.1 makes it a distinct object
  and the glossary makes NIL denote *it* — not the current readtable — wherever
  a readtable designator is accepted. `readtable.standard_readtable()` is now
  that object, and it is **immutable**: it is shared, so a form that mutated it
  would silently redefine what "standard syntax" means for every later
  `(copy-readtable nil)`. The current readtable starts as a *copy* of it.
  (2) **The designator rule was copy-pasted eight times.** Every operator in
  `io_read.py` carried its own `if readtable is None: readtable =
  get_current_readtable()`, which resolves an *omitted* argument and nothing
  else — so all eight broke on exactly the NIL the rule exists for. One
  `coerce_to_readtable` now serves them all. It needs an `_OMITTED` sentinel
  rather than the usual `=None` default, because NIL is a *meaningful* value
  here and `None` cannot tell "omitted" (current) from "given NIL" (standard).
  (3) **`*READTABLE*` was not connected to the reader** — plan.md
  [C7](#c7-the-printer--largely-done-2026-08-14)'s defect in a second
  subsystem. The reader read a module global `readtable._current_readtable`
  while `*READTABLE*` was a separate variable nothing consulted, so
  `(let ((*readtable* rt)) (read ...))` bound the variable and then read with
  the old table. `get_current_readtable()` now reads the symbol's value cell,
  which is its one home; **every reader entry point already funnels through
  that function**, so all of them were fixed at once rather than one at a time.
  (4) `READTABLEP` returned NIL unconditionally — "we don't have readtable
  objects yet" — long after `Readtable` existed, so `(readtablep *readtable*)`
  denied the object `*READTABLE*` was bound to; `TYPEP`'s `READTABLE` branch
  was absent. Both now ask the one object model, so they cannot disagree.
  (5) `READTABLE-CASE` answered the Python string `'UPCASE'` (standing rule 2),
  which is not `EQ` to the `:UPCASE` every caller compares it against, and had
  no writer at all. It answers a keyword, and `SET-READTABLE-CASE` is the
  `(setf (readtable-case rt) ...)` half — reached through SETF's existing
  `SET-<name>` fallback, so this is **not** a sixth entry in the place ladder
  (M5).
  (6) `COPY-READTABLE` ignored its `to-readtable` argument, so
  `copy-readtable.6`'s "modify and return *that* table" could not hold.
  **Measured, same runner both sides:** `printer/print-integers.lsp`
  **0 → 189 passing of 194**. `run_ansi.py reader` (a directory that was *not*
  targeted): **+20 newly passing, 0 newly failing, 0 regressions** merged into
  the checklist. `pytest` **1642 → 1664 passed**, with only the documented
  pre-existing `STREAM-ELEMENT-TYPE` failure; 20 new tests in
  `tests/test_readtable_designator.py` plus one in
  `tests/test_readtable_advanced.py`, and one non-ANSI assertion retired —
  `test_readtable_case_function` pinned `readtable_case() == 'DOWNCASE'`, i.e.
  it certified the Python-string leak.
  **A cost that is a measurement correction, not a regression.** `printer/` as
  a *whole directory* now runs far slower and at one point reached 16.9GB
  resident. The cause is the gate opening: `randomly-check-readability`
  (`printer-aux.lsp:77`) is *also* built on `my-with-standard-io-syntax`, so
  every `PRINT.*.RANDOM` test used to die at the gate immediately. They now
  really run — 1000 random iterations each, with `*print-base*` random in
  2..36 and `*print-level*`/`*print-length*` random in 0..50 — and printing the
  large random structures they build is expensive. This is the same shape as
  the 67 → 113 minute rise the `unless` repair caused: work that used to be
  skipped is really being done. **It is nevertheless unresolved as a practical
  matter, and it cost this change its directory-wide number:** `run_ansi.py
  printer` was attempted twice and abandoned both times (once at 16.9GB
  resident, once after 16 minutes on five files), so **`printer/` as a whole is
  unmeasured here** — only `print-integers.lsp` is. The watchdog printed
  `RESOLVED: progress resumed` at every warning, so this is slow, not wedged.
  See [Discovered issues](#discovered-2026-08-16-b).
  **Checklist effect:** 6985 → **6776** failing (−209), unattributable
  2202 → 2012, files with failures 546 → 544, and the `REGRESSION` marker count
  against the 08-12 baseline fell 56 → 55 — one cleared, **none added**. Note
  the `printer` directory *row* does not move despite the +189, because
  `def-print-test` expands to `deftest` at load time and the checklist's static
  scan cannot attribute those tests to a file ([§3](#the-checklist)'s property
  1); they come out of the *unattributable* bucket instead. `reader` does move,
  129 → 110.

  <a id="discovered-2026-08-16-b"></a>
  **Discovered, diagnosed, not fixed:**
  - **`WITH-STANDARD-IO-SYNTAX` establishes no bindings.** It is registered as
    a `cl_function` in `misc_macros.py` whose body is "evaluate every argument
    eagerly, return the last" — so it binds none of the fourteen variables
    CLHS requires. Reproduction:
    `(let ((*print-base* 2)) (with-standard-io-syntax (prin1-to-string 5)))`
    answers `"101"` where ANSI requires `"5"`. **58 ansi-test files use it**,
    and `def-pprint-test` — the whole pretty-printer test vocabulary — is built
    on it. It is a `cl_function` where the registry note in `CLAUDE.md` says it
    must be a `cl_special`/`cl_macro`. This is the obvious next task and it is
    cheap now that `(copy-readtable nil)` works, since the binding list needs
    it.
  - **`SET-SYNTAX-FROM-CHAR` is a stub that returns T** (standing rule 4) and
    `reader/set-syntax-from-char.lsp` is 12/12 failing. There is no
    *character syntax type* model at all — `Readtable` records macro characters
    and a case, and nothing else — so this is a genuine absent mechanism rather
    than a bug, and it is most of what `reader/` still owes.
  - **The reader ignores `readtable-case`.** `Readtable._read_symbol`
    upcases every token unconditionally, so `:preserve`/`:downcase`/`:invert`
    have no effect on reading even though the readtable now records them
    faithfully and the *printer* honours them. CLHS 23.1.2.
  - **`printer.py:integer_digits` is O(d²) in the digit count** — it `divmod`s
    a shrinking bignum one digit at a time. Fine for fixnums, and it is the
    frame the watchdog caught repeatedly during the slow `printer/` run.
  - `(copy-readtable *readtable* nil nil)` should signal a `PROGRAM-ERROR`
    (`copy-readtable.error.1`); it raises a Python `TypeError` for too many
    arguments instead, which is [X1](#x1-python-exceptions-leaking-as-lisp-values)'s
    boundary rather than anything readtable-specific.

- **2026-08-16** — **A test that never ran, a hang nothing could see, and the
  package leak under both.** The 08-15 tree **could not complete a full run**:
  it sat at 27GB of allocated memory for over half an hour and produced no
  diagnostic of any kind. Three independent defects stacked to make that
  possible, and the third is the one worth keeping.
  **(1) `check-type-error` never called the function under test.** `31c7c59`
  fixed LOOP's `unless` clause, which had never evaluated its test —
  `(loop for e in *mini-universe* unless (typep e 'unsigned-byte) collect e)`
  collected **0** before and **23** after. ansi-test's `check-type-error*` *is*
  that clause: evaluating its `unless` test is what calls the function under
  test. So every `.ERROR` test built on it returned NIL without running
  anything and **passed for the wrong reason** — MAKE-LIST.ERROR.1 among them.
  This is the largest single contributor to +3224, and it is a measurement
  repair, not a feature.
  **(2) What the repair then reached.** `TYPEP` had no branch for the *atomic*
  `UNSIGNED-BYTE`, only the compound `(unsigned-byte n)`, so
  `(typep 5 'unsigned-byte)` was NIL and the guard rejected *everything* —
  handing `(make-list 10000000000000000000000)`, a legitimate `unsigned-byte`
  from `*mini-universe*`, to a `MAKE-LIST` that coerced its size with `int()`
  and built the result one cons at a time. Both fixed: `UNSIGNED-BYTE`/
  `SIGNED-BYTE` atomic and `*`-sized forms in `comparison.py`, and
  `MAKE-LIST`/`MAKE-SEQUENCE` now validate the size (shared
  `arrays.nonnegative_integer`) *and* refuse a size they cannot build.
  **(3) No runner could see it, and that was the real gap.**
  `LoopWatchdog` evaluates its 120s warning and 600s cap inside `tick()`, once
  per iteration, so it is structurally blind to a loop wedged *inside* one
  iteration — which is exactly this. `run_all_tests.py` had **no**
  process-level watchdog at all, in any of its three commits; `run_ansi.py`
  had one, but it measured *total runtime*, so its timeout had to exceed the
  slowest legitimate run. New [`fclpy/watchdog.py`](fclpy/watchdog.py) is one
  shared detector for both runners that measures **time without progress**
  (progress = the harness writing output, so it needs no ansi-test change),
  warns at 120s and hard-stops at 900s, dumping every thread's traceback both
  times. Its last-resort escape is `faulthandler`'s C-level timer rather than a
  Python thread. Output is line-buffered now too: block buffering left
  `run_all_tests.log` ~30 minutes behind the true position, which sent the
  first investigation to the wrong test entirely.
  **Landed with it, because the run could not be trusted without them:** four
  load-time failures, each of which silently removed a whole *file* from the
  run rather than failing one test. `DECODE-FLOAT` returned a Python **tuple**
  instead of `MultipleValues` (unlike its sibling `INTEGER-DECODE-FLOAT`), so
  `(nth-value 1 (decode-float x))` was NIL and ansi-aux's `float-exponent` fed
  NIL to `ABS`; `ABS` called Python's `abs` directly instead of the
  `_ensure_number` the rest of its module uses; `_ensure_number`/`_ensure_real`
  tested `isinstance(x, lisptype.Symbol)` — **a class that has never existed**,
  so the branch raised `AttributeError` instead of the TYPE-ERROR it was
  written to signal — and rejected `Fraction`, though a RATIO is a REAL.
  **And `LOAD` did not bind `*PACKAGE*` (CLHS 24.1).** It restored the package
  only when it had been `None` on entry, and not in a `finally`, so a *nested*
  load's `IN-PACKAGE` leaked into the rest of the enclosing file: `init.lsp`'s
  second form loads `gclload1.lsp`, whose `(in-package :cl-test)` then stayed
  current, so `init.lsp`'s third form was read in `CL-TEST` and `*ROOT-PATH*`
  interned as a different symbol from the `CL-USER` one its own `DEFVAR` had
  bound. Global lookup is by symbol *identity*, not name, so the failure reads
  as "Unbound variable: `*ROOT-PATH*`" immediately after a successful `DEFVAR`
  of that name — and it aborted the rest of `init.lsp` every time.
  **Measured:** full run **11548 → 14772 of 22113**, `COMPLETENESS: OK`,
  0 missing. `pytest` 1642 passed, 1 pre-existing unrelated failure
  (`STREAM-ELEMENT-TYPE`). Four loops crossed the 120s warning and **all four
  resolved**; none reached the cap. Wall time rose ~67 → ~113 minutes, which is
  the expected direction — the old figure was partly cheap because a
  `check-type-error` that never calls its function returns fast.
  **Discovered, not fixed:** `types-and-classes` fell 14.1 points (see
  [preventing regression](#preventing-regression)); a new
  transcendental/float regression cluster of ~11 files; and the reader does not
  parse ratios — `3/5` reads as an unbound *variable*, so `*mini-universe*`'s
  ratio entry is not a ratio at all.

- **2026-08-15 (d)** — **One array object model.** CLHS 15.1 gives an array five
  properties — dimensions, element type, adjustability, fill pointer,
  displacement — and fclpy had a representation for none of them. There were
  *three* unrelated Python shapes: `vectors.AdjustableVector` (a 1-D vector with
  a fill pointer, which is also what the reader built for a `#(...)` literal, so
  every **simple** vector claimed to be adjustable), `vectors.Array` (a separate
  multi-dimensional class that was not even `ARRAYP`), and a bare Python `list`
  for everything else. None recorded an element type, so `MAKE-ARRAY` discarded
  `:element-type` outright — `(make-array 5 :element-type 'bit)` was a vector of
  NIL — `:displaced-to` was accepted and ignored, and `ARRAY-ELEMENT-TYPE`
  returned the Python string `'T'` (standing rule 2).
  **The operators were duplicated across five modules and import order picked
  the winner** (standing rule 3, Finding L): `vectors.py`'s fill-pointer-aware
  `AREF`, `VECTOR-PUSH`, `ARRAY-DIMENSION(S)` and `ADJUSTABLE-ARRAY-P` all
  *lost* to copies in `sequences_higher.py` / `misc_hashtables.py` /
  `math_arithmetic.py` / `core.py` that knew nothing about any of it — the live
  `VECTOR-PUSH` was `vector.append(...)`, which an array object does not have,
  so it leaked an `AttributeError` as the value of the form, and the live
  `AREF` indexed one subscript at a time, so a 2-D reference raised
  `IndexError: Expected 2 indices, got 1` (both are rows in [X1](#x1-python-exceptions-leaking-as-lisp-values)'s
  leak table). `ADJUSTABLE-ARRAY-P` was a stub returning NIL; `ARRAY-ROW-MAJOR-INDEX`
  returned 0; `ROW-MAJOR-AREF` returned None.
  **`lispfunc/arrays.py` is now the one model and the one home for every array
  operator**, with the same shape as `sequence_protocol.py`: **three
  representations, one protocol.** A Python `list` is a *simple general vector*,
  a `LispString` is a character vector, and `LispArray` is everything else — any
  other rank, any specialized element type, any fill pointer, adjustability or
  displacement. `_new_array` is the only place that decides which, and nothing
  asks `isinstance` (Finding M) — `array_rank_of` / `array_dimensions_of` /
  `element_type_of` / `fill_pointer_of` / `row_major_get` answer for all three.
  Displaced arrays forward every access to their target rather than copying, so
  writes are visible through both.
  **Measured, same runner both sides:** `run_ansi.py arrays` **518 → 1233 of
  1356** — **+715**, failures 838 → 123, `arrays/` 42.8% → **90.1%** in the
  checklist. `pytest` 1642 passed with the same 1 pre-existing unrelated
  failure (`STREAM-ELEMENT-TYPE`), plus 43 new tests in
  `tests/test_array_model.py`. Those replaced
  `test_phase5_task3_vectors.py`/`test_phase5_task4_arrays.py` (648 lines),
  which certified the `vectors.py` classes — i.e. the copies that had *lost* the
  registry, so no Lisp form could reach the code they tested. One of them
  asserted `(array-dimension <fill-pointer 5, size 10> 0)` = 1, a row in
  [§3's non-ANSI assertion table](#known-non-ansi-assertions-in-the-unit-suite)
  that is now gone.
  **Three mechanisms outside `arrays/` moved with it, and each was found by the
  array work rather than aimed at.** (1) **`COND` answered the unevaluated
  *form* of a body-less clause**: `(cond ((+ 1 2)))` was the list `(+ 1 2)`,
  not 3 (CLHS 5.3 says the value of the test). ansi-test's own
  `make-array-with-checks` — and every aux helper written as one long `cond` of
  test-only clauses — returns exactly that shape, so the harness compared the
  check's *source text* against the expected value and **no test using one
  could pass whatever the implementation did**. That is the measurement-gate
  shape of [§4](#recommended-order) items 4 and 6, a third time. (2) **A
  keyword argument repeated in a call took the *rightmost* pair**, where CLHS
  3.4.1.4.1 takes the leftmost — which ansi-test checks directly with
  `:allow-other-keys t :allow-other-keys nil` — and **an odd number of keyword
  arguments passed the dangling keyword on as a positional argument**, so the
  callee raised a Python `TypeError` where CLHS 3.5.1.6 requires a
  PROGRAM-ERROR. Both are in the one argument-passing site in
  `evaluation_core.py`. (3) **`EQUAL` now descends bit vectors** (CLHS 5.3), so
  `(equal #*101 #*101)` is T.
  **And one shared place accessor.** `SETF`, `PSETF`, `INCF`, `DECF` and
  `ROTATEF` each open-coded the `AREF` place, and **every copy read exactly one
  subscript** — `(setf (aref a i j) v)` silently wrote element `i` — and every
  copy "helpfully" extended a Python list when the index was out of range,
  turning an error into a longer vector (standing rule 4). One reader/writer
  pair in `arrays.py` now serves all five, and it covers `SVREF`, `BIT`,
  `SBIT`, `ROW-MAJOR-AREF` and `FILL-POINTER` as well. This is *not* M5: the
  place ladder is untouched, only its array rung.

  **Not yet measured — do this first on the next run.** Only `arrays` was run
  to completion after the last three changes, and the checklist was amended
  with it. The **cross-group regression sweep was started and stopped**, so the
  COND fix, the keyword-argument rules and `EQUAL` on bit vectors have *no*
  measured blast radius yet. All three are shared mechanisms with wide reach,
  and the COND one in particular changes what a great many ansi-aux helpers
  return, so it should move numbers well outside `arrays/` — in which
  direction is unverified. Run
  `run_ansi.py sequences printer strings types-and-classes cons misc iteration
  data-and-control-flow --update-checklist`, then diff against
  `docs/ansi_checklist_baseline.json` per [the development loop](#the-development-loop)
  step 7, before treating any of it as landed.

  <a id="discovered-2026-08-15-d"></a>
  **Discovered, diagnosed, not fixed:**
  - **A nested destructuring pattern in a `defmacro` lambda list does not
    bind.** `(defmacro multiple-value-bind* ((&rest vars) form &body body) ...)`
    — ansi-aux's own helper — signals `Unbound variable: VARS` when expanded, so
    every ansi-test check that goes through `multiple-value-bind*` (including
    `subtypep-or-unknown`, which `make-array-with-checks` calls for *every*
    array) fails there. **This is now the whole residual of
    `arrays/make-array.lsp`**, 90 of its 118 tests, and it is M3/M4's, not
    arrays'. It is a cheap, well-localized target with a large blast radius:
    the pattern is idiomatic in the harness.
  - **`TYPE-OF` returns uninterned symbols** on most of its branches
    (`lisptype.LispSymbol('VECTOR')` rather than the `CL` symbol), so its
    result prints as `#:VECTOR` and is not `EQ` to the symbol a caller wrote.
    The array branches were fixed here; the rest were left alone as their own
    change.
  - **`numbers/number-comparison.lsp` fails to load** with
    `bad operand type for abs(): 'lispNull'` — a load-time failure, so the 145
    tests in it never register. Not investigated; noticed while running a
    multi-group target.
  - `SUBTYPEP` still has no lattice ([C14](#tier-2--subsystem-gaps)), which is
    what `UPGRADED-ARRAY-ELEMENT-TYPE.8` measures, and it also makes
    `make-array-with-checks`' element-type checks vacuous rather than failing.
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
