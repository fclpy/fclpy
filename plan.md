# fclpy — ANSI Compliance Plan

**Goal:** take existing, unmodified ANSI Common Lisp source and run it correctly.
A passing scoreboard is the instrument, not the objective.

**This document is organized as a checklist of what is still broken**, ranked by
the *mechanism* at fault rather than by test count. It replaced a chronological
plan whose eight stacked "Update" sections had become longer than its content;
that history is preserved in condensed form in [Changelog](#changelog).

---

## 1. Status

**Last full run: 2026-08-12, in flight at time of writing.** The numbers below
are from the **previous** run (2026-08-12 01:03), which was still truncated —
treat every figure as provisional and regenerate after the current run lands.

| | value |
|---|---|
| Registered tests | 22036 |
| Executed (`accounted`) | 8971 |
| Passed | 4514 |
| Failed | 4457 |
| Never executed | 13065 |

**Three things changed recently, and they reset the plan's premises:**

1. **The suite now reaches every test.** Every prior version of this document was
   written under a truncated run — the majority of tests had never executed, so
   every priority call was made on a sample of roughly a third of the suite. The
   last blocker (`DO-SYMBOLS`' missing implicit tagbody) is fixed.
2. **A full run now costs 4+ hours**, so it can no longer be the development
   loop. See [§2](#2-how-to-work).
3. **~76% of that runtime is one defect** — LOOP's `for var = expr` driver. See
   [C1](#c1-loop-for-var--expr-driver--core).

**Read the per-directory table with care.** In the data above, a directory with
`failed=0` is almost always *unmeasured*, not passing:

| directory | passed | failed | never ran | total |
|---|---|---|---|---|
| sequences | 0 | 0 | **3158** | 3158 |
| numbers | 0 | 0 | **1438** | 1438 |
| printer | 0 | 0 | **788** | 788 |
| misc | 0 | 0 | **740** | 740 |
| types-and-classes | 0 | 0 | **545** | 545 |
| streams | 0 | 0 | **543** | 543 |
| strings | 0 | 0 | **501** | 501 |
| characters / pathnames / environment / reader / structures / files / system-construction | 0 | 0 | **1108** | 1108 |
| cons | 580 | 1058 | 0 | 1638 |
| arrays | 520 | 725 | 0 | 1245 |
| objects | 215 | 610 | 0 | 825 |
| iteration | 366 | 472 | 0 | 838 |
| data-and-control-flow | 1005 | 415 | 0 | 1420 |
| conditions | 116 | 187 | 0 | 303 |
| packages | 72 | 198 | 70 | 340 |
| eval-and-compile | 224 | 94 | 0 | 318 |
| hash-tables | 89 | 69 | 0 | 158 |
| symbols | 1105 | 40 | 0 | 1145 |
| (programmatically generated) | 220 | 589 | 4174 | 4983 |

---

## 2. How to work

### The development loop

1. Pick a **cluster** from [§3](#3-the-checklist), never a test.
2. Reproduce it in the smallest expression that shows the defect.
3. Fix the **mechanism**. Consolidate onto an existing helper if one exists.
4. Verify with a **targeted run** of the owning group.
5. Run `pytest` for regressions.
6. Run the full suite only to move the scoreboard or close a milestone.

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

### Regenerating the checklist

```powershell
# cluster failures by operator
sed 's/\.[0-9].*$//' ansi_results/failed.txt | sort | uniq -c | sort -rn | head -40
# per-subsystem scoreboard -> docs/ansi_baseline.json
pipenv run python scripts/ansi_score.py
# root-cause shapes leaking into results
grep -a -o "Python error in [^\"]\{0,70\}" run_all_tests.log | sort | uniq -c | sort -rn | head
grep -a -o "Undefined function[: ]*[A-Z0-9-]*" run_all_tests.log | sed 's/.*[: ]//' | sort | uniq -c | sort -rn | head
```

### Standing rules

1. **Never implement a test. Implement the mechanism the test checks.**
2. **Any Python object appearing as a Lisp value is a bug** — including
   exceptions (`TypeError`, `FileNotFoundError`, `RestartException`) surfacing as
   test results. There are currently **~1600 such leaks** ([C2](#c2-python-exceptions-leaking-as-lisp-values--core)).
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

Ranked by evidence. **Tier 1 items are core mechanisms**: each is one defect
behind many failures. Counts are from the provisional data in §1 and from the
in-flight run's log; the *command that produced each number* is given so it can
be re-derived rather than trusted.

### Tier 1 — core mechanisms (do these first)

#### C1. LOOP `for var = expr` driver — **CORE**

**Evidence.** `LOOP` is the single largest failing cluster at **449 failures**
(`grep "^LOOP" ansi_results/failed.txt | wc -l`). `for var =` appears **2784
times across 260 test files** (`grep -rn "for [a-z-]* = " --include=*.lsp | wc -l`).
It is also responsible for **~76% of full-run wall time** (below).

**Two distinct defects**, both verified by direct execution, each case in its own
process:

| expression | expected | fclpy |
|---|---|---|
| `(loop repeat 5 collect 1)` | `(1 1 1 1 1)` | correct |
| `(loop for x = 7 repeat 5 collect x)` | `(7 7 7 7 7)` | **`Unbound variable: X`** |
| `(loop for x = 0 then (1+ x) repeat 4 collect x)` | `(0 1 2 3)` | **`Unbound variable: X`** |
| `(loop for a = 1 for b = 2 repeat 4 collect (+ a b))` | `(3 3 3 3)` | **`Unbound variable: A`** |
| `(loop repeat 3 for x = 9 collect x)` | `(9 9 9)` | **infinite loop (hangs)** |
| `(loop for x = 1 repeat 3 count t)` | `3` | correct |

1. **The variable is bound where the driver sees it but not where the body and
   accumulation clauses do.** The last row is the control: the loop is correct
   precisely when it never *references* the variable. A binding-environment
   defect, not a parsing one.
2. **`repeat` before `for =` loses termination entirely.** Clause order decides
   whether the loop terminates, so `repeat` is being folded into the driver
   instead of composing with it (CLHS 6.1.2.1 — bounding clauses compose with
   other drivers, they do not replace them).

**This is not a performance problem.** Measured: `(loop for i below 400)` runs at
**2.5 µs/iteration**, and every shape tested (`collect`, `count`, `always`,
`unless collect`, `for =`) scales **linearly**, ×1.9–2.1 per doubling. A
65536-iteration char loop should cost ~6s; the observed 473s is ~80× that because
the loops burning the time *never terminate*.

**Runtime accounting** (run started 05:57, still going at 10:17):

| | count | wall time |
|---|---|---|
| LOOP forms aborted at the 600s cap | 10 | 1h40m |
| LOOP forms that warned (>120s) then finished | 21 | 1h38m |
| **~31 loop forms** | | **~3h18m of 4h20m** |

Tests confirmed aborting on this shape: `SQRT.12`–`.17`, `DEPOSIT-FIELD.1`–`.5`,
`DPB.2`.

**Also fix in the same change:** `LOOP_TIMEOUT_ERROR`'s hard cap guards only the
*simple-loop* path, on the stated premise that "every other iteration construct
is naturally bounded by its own driver." The hang above disproves that premise —
a driver-path runaway is bounded by nothing. Extend the cap to all paths or
delete the premise from the comment.

**Owner:** M3-adjacent (LOOP is its own clause parser today).
**Verify:** `run_ansi.py iteration`, then `numbers/deposit-field.lsp`, `numbers/sqrt.lsp`.

#### C2. Python exceptions leaking as Lisp values — **CORE**

**Evidence** (`grep -a -c` on the run log): **889** `Undefined function`, **531**
`Unbound variable`, **530** `Python error in`, 18 `AttributeError`.

Raw Python exception text is being returned *as the value of a Lisp form*, which
violates standing rule 2 and — worse — means the suite reports a **wrong value**
where it should report an **unimplemented feature**. Sampled shapes:

| occurrences | leak |
|---|---|
| 82 | `FileNotFoundError: File not found: ...` |
| 76 | `FileExistsError: File exists: ...` |
| 18 | `RuntimeError: CALL-NEXT-METHOD: No next method available` |
| 10 | `ValueError: math domain error` |
| 10 | `RestartException: Restart: FOO` |
| 10 | `NameError: Class not found: ARITHMETIC-ERROR` |
| 10 | `AttributeError: Slot A not found` |
| 10 | `OSError: Cannot open ...` |
| ~7 | `ValueError: I/O operation on closed file` |
| 6 | `RecursionError: maximum recursion depth exceeded` |
| 5 | `AttributeError: 'lispCons' object has no attribute 'remove'` |
| many | `NameError: Class not found: STRUCT-TEST-nn` |

Each row is a different underlying gap, but the *leak itself* is one mechanism:
the boundary that should convert a Python exception into a signaled Lisp
condition. Fixing that boundary converts ~1600 wrong answers into honest,
countable failures — which is a prerequisite for trusting every other number in
this document.

**Owner:** M8's raise-site migration (see [C7](#c7-conditions-restarts-and-define-condition)).

#### C3. Function designators are not resolved — **CORE**

**Evidence.** The most common "undefined functions" are single letters —
`X` (336), `S` (296), `OS` (192), `A` (166), `IS` (118), `S1` (94). These are
*variables holding functions*, not missing standard functions: the implementation
is failing to coerce a function designator to a function.

This is **Finding J** ("there is no `coerce_to_function`") with a number attached:
over **1200** occurrences. Every site that accepts a function designator
(`:test`, `:key`, `FUNCALL`, `APPLY`, `MAP*`, `SORT`, `REDUCE`) needs one shared
coercion, not a local `callable()` check.

**Owner:** M3/M6 boundary. **Verify:** `run_ansi.py sequences` once it runs.

#### C4. `DEFSTRUCT` generates no accessors and no class

**Evidence.** `COPY-STRUCTURE` (132), plus `MAKE-STRUCT-TEST-06` (22),
`MAKE-SBT-16` (18), and a long tail of `STRUCT-TEST-nn-ann` accessor names (8
each), plus `NameError: Class not found: STRUCT-TEST-nn` in the leak table.
`structures/` (115 tests) has never executed; the failures above are its spill
into `objects/` and the generated tests.

One mechanism: `DEFSTRUCT` must define the constructor, copier, predicate,
accessors, and a real type/class. **Owner:** M9.

#### C5. `MAKE-ARRAY` / adjustable / displaced arrays

**Evidence.** `MAKE-ARRAY` 47, `SIMPLE-ARRAY` 44, `ARRAY` 41,
`VECTOR-PUSH-EXTEND` 39, `ADJUST-ARRAY` 39, `SIMPLE-ARRAY-T` 34, `ARRAY-T` 34,
`MAKE-ARRAY.DISPLACED` 31, `VECTOR-PUSH` 29, `ADJUST-ARRAY.STRING` 22,
`ADJUST-ARRAY.BIT-VECTOR` 22 — **~380 failures**, and `arrays/` is 725 failing of
1245. Also `IndexError: Expected 2 indices, got 1` in the leak table.

The cluster shape (fill pointers, adjustability, displacement, element types)
says the array *object model* is missing those properties rather than that many
functions are individually wrong. **Owner:** M9.

#### C6. CLOS — `DEFGENERIC` / `DEFMETHOD` / `DEFCLASS` / `CHANGE-CLASS`

**Evidence.** `DEFGENERIC` 52, `SHARED-INITIALIZE` 41, `CHANGE-CLASS` 34,
`DEFMETHOD` 26, `DEFCLASS` 22, `MAKE-INSTANCES-OBSOLETE` 8, plus
`RuntimeError: CALL-NEXT-METHOD: No next method available` (18) and
`AttributeError: Slot A not found` (10). `objects/` is 610 failing of 825, and
`types-and-classes/` (545) has never run.

**Two CLOS implementations still coexist** (Finding L). Consolidate before
fixing. **Owner:** M9.

#### C7. Conditions, restarts, and `DEFINE-CONDITION`

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
  `SIGNAL` — the same migration as [C2](#c2-python-exceptions-leaking-as-lisp-values--core).

**Owner:** M8. **Verify:** `run_ansi.py conditions`.

#### C8. Package model

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

| # | cluster | evidence | owner |
|---|---|---|---|
| C9 | **Set/list operations** — `UNION` 37, `NUNION` 34, `SET-EXCLUSIVE-OR` 30, `RASSOC` 30, `SET-DIFFERENCE` 26, `MEMBER` 26, `NSET-DIFFERENCE` 25, `INTERSECTION` 24, `ADJOIN` 24, `SUBSETP` 21, `NINTERSECTION` 23 (**~300**) — almost certainly one shared `:test`/`:key` defect, not 11 bugs. The known reversed `:test` argument order in `SequenceIterator.matches` (`sequences_search.py`) is a single fix affecting `FIND`/`POSITION`/`COUNT`/`REMOVE` and likely all of these. | M6 |
| C10 | **Places / `SETF`** — `PSETF` 31, `PUSHNEW` 27, `ROTATEF` 23. Five parallel place protocols; `GET-SETF-EXPANSION` is a stub returning a Python 5-element list instead of five values; `PUSH`/`POP`/`PUSHNEW` are registered as *functions* over Python lists. | M5 |
| C11 | **Lambda lists** — `FLET` 35, `LAMBDA` 22, `DESTRUCTURING-BIND` 22. Six copy-pasted binders. | M3 |
| C12 | **Streams / files / pathnames** — `FileNotFoundError`/`FileExistsError`/`OSError` leaks (~170). `streams/` 543, `pathnames/` 215, `files/` 87 never ran. | M10 |
| C13 | **Missing standard functions** — `LDIFF` 38, `TAILP` 20, `CHECK-TYPE` 18, `STREAM-ELEMENT-TYPE` 10, `MAKE-INSTANCES-OBSOLETE` 8. Genuinely absent; cheap. | M1 |
| C14 | **Numeric tower** — `ValueError: math domain error` (10); `numbers/` 1438 never ran. Bignums, ratios, complex, float contagion. | Phase 4 |
| C15 | **Reader / printer** — `reader/` 165 and `printer/` 788 never ran. `#(1 2 3)` reads as the cons `(VECTOR 1 2 3)`; `PRIN1` emits C-style escapes; `PRINC` keeps `:` on keywords and `#\` on characters; `READTABLE-CASE` returns a Python string. **Also: `fclpy/reader.py` is a dead ~480-line second reader** that nothing imports but **177 tests (14% of the unit suite)** certify — while the live reader has essentially no unit coverage. Retire it or repoint those tests. | M10 |

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
| **M0** | Trustworthy measurement | **essentially done** — suite reaches every test; scoreboard + targeted runner exist. Remaining: `expected-failures/` wiring | — |
| **M1** | Symbol, NIL, package identity | canonical CL symbol table **done**; package model outstanding | C8, C13 |
| **M2** | Environment model | **not started** — the spine. Do not fix specials one binding form at a time; that produces a seventh mechanism | C1, C3 |
| **M3** | One lambda-list engine | not started — six copy-pasted binders | C11, C1 |
| **M4** | A real macro system | not started — ~90 standard macros are special forms. **Most ecosystem-critical** | — |
| **M5** | `GET-SETF-EXPANSION` / places | not started — deletes ~600 lines of ladder code | C10 |
| **M6** | Multiple values | partial | C9, C3 |
| **M7** | Non-local control flow | partial — name-based block/tag matching, no identity objects | — |
| **M8** | Conditions and restarts | **signaling core done**; restart half + `DEFINE-CONDITION` + raise-site migration remain | C2, C7 |
| **M9** | Types, `SUBTYPEP`, CLOS | not started — two CLOS implementations; `SUBTYPEP` is a string-pair table | C4, C5, C6 |
| **M10** | Reader, printer, streams, pathnames, loader | not started — **gates ASDF and all library loading** | C12, C15 |

**Ordering.** C1 first (cheapest large unlock, and it makes full runs affordable
again). Then M2, because C3 and much of C1 bottom out in the environment model,
and M3/M4 depend on it. Then re-derive priorities from the **first complete
scoreboard** — 13065 tests have never run, and they will reorder this table.

---

## 5. Known temporary deviations

Anything knowingly non-ANSI, with the milestone that removes it. Empty means
"nothing is knowingly wrong" — keep it honest.

| deviation | why tolerated | removed by |
|---|---|---|
| LOOP `for var = expr` invisible to body/accumulation clauses | driver-path binding defect | C1 |
| LOOP `repeat` before `for =` never terminates | `repeat` folded into the driver | C1 |
| LOOP hard cap guards only the simple-loop path | its premise is false | C1 |
| `_run_handlers_on_unwind` + `_condition_matches` legacy branch | most raise sites bypass `SIGNAL` | M8 |
| `DEFINE-CONDITION` creates no class | predates the class lattice | M8 |
| `HANDLER-CASE` converts an uncaught `THROW` into `CONTROL-ERROR` | needs a catch-tag stack to decide at THROW time | M7 |
| 114 non-ANSI symbols exported from `CL` | registry auto-export | M1 |
| ~90 standard macros implemented as special forms | predates the macro system | M4 |
| Five parallel place protocols; `GET-SETF-EXPANSION` a stub | predates the setf protocol | M5 |
| Six copy-pasted lambda-list binders | never factored | M3 |
| Two CLOS implementations, two readers, two readtables, dead `printer.py`, dead `reader.py`/`tokenizer.py` fork | historical forks | M9 / M10 |
| `SUBTYPEP` string-pair table | no type lattice | M9 |
| `LispString` vs. Python `str` split | two string representations | M9 (blocks EQUAL/EQUALP) |
| Name-based block/tag/catch matching | no block identity objects | M7 |
| `is_truthy(False)` is `True` | unaudited boundary | M2 |

---

## 6. The two dimensions

"100% ANSI" is **not** primarily an evaluator problem.

| | **A — language semantics** | **B — environment / ecosystem** |
|---|---|---|
| Covers | evaluation, environments, lambda lists, macros, places, values, control flow, conditions, CLOS, types | packages, reader, printer, `FORMAT`, streams, files, pathnames, `LOAD`, `COMPILE`, implementation variables |
| Failure if wrong | code computes the **wrong answer** | code **cannot be loaded at all** |
| Milestones | M2–M9 | M0, M1, M9 (types-as-interface), M10, Phase 4 |

**Why this must stay explicit.** Nearly every failure visible today is an
A-dimension failure *because A is most of what has executed*. Ranking work by
observed failure count ranks A over B by construction — a sampling artifact.
`sequences`, `printer`, `streams`, `strings`, `characters`, `pathnames`,
`reader`, `structures`, and `files` are **~8000 tests that have never run**.

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
| **J** | There is no `coerce_to_function` | **C3** |
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
| `scripts/run_ansi.py` | **targeted runner — the development inner loop** |
| `scripts/ansi_score.py` | per-subsystem scoreboard → `docs/ansi_baseline.json` |
| `ansi_results/failed.txt` | **the working checklist** — group by operator; never work top-to-bottom |
| `run_all_tests.py` | full suite (4+ hours) — authority, not inner loop |
| `REPAIR.md` | crash-repair SOP — historical; crashes are no longer the constraint |

---

## Changelog

Condensed from the previous chronological plan. Each entry is a *mechanism*
landed, not a test count.

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
