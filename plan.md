# fclpy — ANSI Compliance Plan

**Goal:** take existing, unmodified ANSI Common Lisp source and run it correctly.
A passing scoreboard is the instrument, not the objective.

**This document is organized around what is still broken**, ranked by the
*mechanism* at fault rather than by test count. It replaced a chronological plan
whose eight stacked "Update" sections had become longer than its content; that
history is preserved in condensed form in [Changelog](docs/changelog.md).

> ### 📋 This plan observes `docs/ansi_checklist.md`
>
> That generated file is **the authority for what is failing and where** — all
> 5037 failures grouped directory → file, each with the command to re-verify it.
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

> ### Multi-agent round (2026-08-30): checklist index 597 → ~333
>
> Amended-index movement from one orchestration session (three parallel
> worker worktrees + lead fixes; the official scoreboard still moves only
> on a full run). Clusters closed or advanced, by mechanism:
>
> - **FORMAT engine** (~88): `~/` function directive, `~_`/`~*`/`~^`
>   (incl. the `~:^` last-sublist rule and `~?`/`~(` scopes), `~[`,
>   `~{` error signaling, `~<newline>`, `~W` (new), `~:C`, `~B/D/X/O/R`
>   fallback binding of the print control vars.
> - **PPrint** (~55): PPRINT-FILL/LINEAR/TABULAR real CLHS drivers, a real
>   PPRINT-TAB, PPRINT-POP/PPRINT-NEWLINE.FILL/PPRINT-LOGICAL-BLOCK.17,
>   pprint-local `*print-circle*` labelling, plus io_write leftovers
>   (broadcast FILE-LENGTH/POSITION, FILE-AUTHOR, RENAME-FILE.5, WRITE's
>   `&key stream`, DELETE-FILE rmdir).
> - **CLOS** (~69 direct + unlocked): real class precedence lists (CLHS
>   4.3.5.1 topological sort, not C3) over a real built-in class
>   hierarchy + condition hierarchy + metaclasses; CLASS-PRECEDENCE-LIST;
>   class_of rewritten; method maintenance; CALL-NEXT-METHOD/NEXT-METHOD-P
>   as frame-capturing locals; CHANGE-CLASS protocol; DEFGENERIC options
>   (incl. `:generic-function-class` → recorded, DEFGENERIC.30); CLHS
>   7.1.2 initarg validity; ADD-METHOD's shadowing 3-arg duplicate deleted.
> - **Compile-file MAKE-LOAD-FORM protocol** (14): CLHS 3.2.4 creation/
>   initialization forms with data-flow ordering (creation deps recursive,
>   inits topological, bound-vs-emitted distinction), object/class
>   constants externalized via reference symbols / `(FIND-CLASS 'name)`.
> - **Macro system M4 phase 1** (28 of 66 converted): WHEN/UNLESS/PROG1/
>   PROG2/NTH-VALUE/MULTIPLE-VALUE-{LIST,BIND,SETQ}/PSETQ/AND/OR/CASE
>   family/INCF/DECF/PUSH/POP/PUSHNEW/REMF/ROTATEF/SHIFTF/RETURN/IGNORE-
>   ERRORS/CHECK-TYPE/STEP/DECLAIM/DESTRUCTURING-BIND etc. now REAL
>   macros in the new `lispfunc/standard_macros.py` (one implementation;
>   eval branches deleted). Compiler-macro facility built: registry,
>   `(SETF COMPILER-MACRO-FUNCTION)`, DEFINE-COMPILER-MACRO as a real
>   macro, COMPILE expanding compiler macros (notinline + decline).
> - **Evaluator/conditions**: LOOP main clauses run in source order
>   (CLHS 6.1.2.1); quasiquote unquote takes the primary value only
>   (DEFMACRO.17/.17A); HANDLER-CASE passes a THROW through to its
>   enclosing CATCH (the obsolete M7 approximation removed — the
>   uncaught-throw CONTROL-ERROR is signaled at the throw site since
>   `state.catch_tags` exists); SLOT-VALUE's SETF path honors the
>   built-in-class guard; DPB/DEPOSIT-FIELD semantics split; SETF of
>   LDB/MASK-FIELD evaluation order; RATIONALIZE's exact rounding-interval
>   algorithm; EXPT single-float overflow gated on integer powers;
>   ARITHMETIC-ERROR-OPERANDS/OPERATION read their slots; DEFUN returns
>   the written `(setf f)` name; PACKAGE-ERROR-PACKAGE's None-stub
>   duplicate deleted.
>
> **pytest 2097 green throughout; duplicates gate clean; every integrated
> cluster verified in-tree with targeted ANSI runs (0 unaccounted).**
>
> ### Remaining findings for the next run (recorded 2026-08-30)
>
> 1. **TYPE-OF.4 residual (+1 gate flag, `comparison.py`)**: `type_of`'s
>    fallback answers T for streams/hash-tables/pathnames/readtables/
>    conditions, contradicting the new `class_of`. A fix that consults
>    `classes.class_of` broke the ansi-aux bootstrap (NOTNOT-MV undefined
>    — cause undiagnosed; a type_of↔class_of recursion was ruled out
>    empirically). Root-cause the aux-load failure first, then re-apply.
> 2. **`printer/print-cons.lsp` (+1 gate flag)**: 5 failing vs the 08-28
>    baseline's 4 — drift that predates the 2026-08-30 session; diff the
>    five against the baseline before assuming the session caused it.
> 3. **PPRINT-DISPATCH.7**: `.0001` reads as the symbol `|.0001|` —
>    potential-number syntax (tokenizer.py/lispreader.py).
> 4. **EXPT.16**: the reader must keep exact rational parts in `#c(...)`
>    literals (`readtable.py::_read_complex_number` ~1199, mirroring
>    `misc_macros.complex_fn`).
> 5. **FORMAT.E.26 vs exp.error.4/.5/.8/.9**: jointly unsatisfiable
>    without float-subtype tracking (single vs double range); accepted
>    net-loss until then.
> 6. **FORMAT.C.4A/FORMATTER.C.4A + FORMAT.S.8**: need CHAR-NAME coverage
>    for every non-graphic character (characters.py); `~:C`'s delegation
>    is already in place.
> 7. **FORMAT.LOGICAL-BLOCK.CIRCLE.1-3**: `*print-circle*` across several
>    `~A` calls in one FORMAT needs shared circle bookkeeping in
>    printer.py (io_write's pprint-local labelling is the model).
> 8. **M4 phase 2**: 38 of the 90 `*cl-macro-symbols*` still lack a
>    macro function — the definer forms (defun/defmacro/defclass/...),
>    DO/DOLIST/DOTIMES, COND, SETF/PSETF, PROG/PROG*, LAMBDA, FORMATTER,
>    ASSERT, the WITH-* family's remaining names. MACRO-FUNCTION.1/.2/.3
>    pass only at full coverage. `standard_macros.py`'s
>    `_standard_macro` pattern is the established home.
> 9. **dcf/eval-and-compile tail**: EVERY/SOME/NOTANY/NOTEVERY `.ERROR.*`
>    and FUNCALL.ERROR.* (bad-designator signaling), LET*/LET, PSETQ/
>    ROTATEF/SETF.ORDER leftovers, CASE/CCASE/CTYPECASE residuals,
>    EQUAL/EQUALP, VALUES*, DCF-MACROS, DESTRUCTURING-BIND.ERROR.10,
>    COMPILED-FUNCTION-P.1, OR.6, NIL.7, PROG*.11, misc/misc.lsp (16).
> 10. **Process notes**: a Windows-console UnicodeEncodeError (cp1252) can
>    kill a run silently while printing diagnostics — set
>    PYTHONIOENCODING=utf-8 for runs that may print non-ASCII;
>    `git apply --3way` can roll back atomically while still printing
>    per-file success — verify with `git diff --stat` after every apply;
>    PowerShell `>` writes UTF-16 — use `git diff --output`; worker
>    sessions can end empty at ~50% — always verify the worktree diff on
>    disk rather than trusting the report.
> 11. A **full ~86-minute run is mandatory before moving the official
>    scoreboard**: DELETE-FILE/OPEN (bootstrap path) and the whole
>    evaluator/printer surface were touched this session.


**Latest full run: 2026-08-28. 95.4% passing, 1004 failing.**

```
COMPLETENESS: total=21881 passed=20877 failed=1004 accounted=21881 missing=0 extra=0
COMPLETENESS: OK
```

| | value | previous full run (2026-08-27, second) | previous full run (2026-08-27, first) |
|---|---|---|---|
| Registered tests | 21881 | 21881 | 21881 |
| Executed (`accounted`) | **21881 (100%)** | 21881 (100%) | 21881 (100%) |
| Passed | **20877 (95.4%)** | 20575 (94.05%) | 20553 (93.9%) |
| Failed | **1004** | 1306 | 1328 |
| Never executed | **0** | 0 | 0 |
| Wall time | ~127 minutes (7612s) | ~102 minutes | ~127 minutes |

`docs/ansi_checklist_baseline.json` was refreshed from this run
(`ansi_checklist.py --save-baseline`) — the only way that file is allowed to
move (§7, "Ways to fake compliance"). Refreshing it was **not** contingent on
a clean gate this time: the run surfaced four small file-level regressions
against the 2026-08-27 baseline —

| file | 08-27 baseline | 08-28 | delta |
|---|---|---|---|
| `numbers/number-comparison.lsp` | 8 | 14 | +6 |
| `numbers/divide.lsp` | 12 | 13 | +1 |
| `printer/format/format-e.lsp` | 1 | 3 | +2 |
| `types-and-classes/types-and-class.lsp` | 8 | 9 | +1 |

— and they were **accepted in writing rather than root-caused**: the user
explicitly asked for the baseline to move to this run's numbers without first
isolating the regressing commits. That is a deliberate exception to the
default rule in [Ways to fake compliance](#ways-to-fake-compliance) ("only
once the regression is understood and accepted in writing or fixed"); the
"accepted in writing" branch is what was invoked, not "understood." These
four files are real, un-investigated regressions carried forward as debt —
they are not visible as `+N REGRESSION` markers any more because the baseline
now equals the current run, but they represent a net decline from 08-27 in
those four files specifically, inside a run whose total moved forward
(1306 → 1004 failing) by a wide enough margin that the aggregate number does
not surface it. Whoever next touches `number-comparison.lsp`, `divide.lsp`,
`format-e.lsp` or `types-and-class.lsp` should diff their current failures
against what 08-27's checklist recorded for them before assuming a clean
slate.

> ### Same day, two full runs — why
>
> The first 2026-08-27 run measured the batch of missing-system work
> described in `docs/changelog.md`'s **2026-08-27 (a)** entry (numeric-token
> syntax/ratios, the reader control variables, `*PRINT-READABLY*`'s override,
> `~<...~>` justification rewritten to spec, forward-referenced classes,
> LOOP's selectable-clause grammar, DOCUMENTATION as a real generic
> function, and more). Regenerating the checklist from it immediately found
> **6 file-level regressions `gate.py`'s per-file baseline check had never
> seen** — no targeted run along the way happened to touch the affected
> files: `numbers/incf.lsp`, `numbers/decf.lsp`,
> `data-and-control-flow/macrolet.lsp`, `data-and-control-flow/places.lsp`,
> `data-and-control-flow/rotatef.lsp`, `conditions/restart-case.lsp`.
>
> Bisected with `git worktree` + selective file reverts, not guessed, to one
> line: `new is expanded`, comparing a place/expansion form against the
> `MultipleValues` wrapper CLHS 3.8's `MACROEXPAND` now correctly returns.
> Three separate call sites had exactly this comparison — `GET-SETF-EXPANSION`
> (so every `SETF` place, `INCF`/`DECF`/`ROTATEF`), `RESTART-CASE`'s
> CLHS 9.1 condition-association detector, and the `MACROEXPAND-1` special
> form — and fixing them surfaced a **fourth, genuine, previously-masked
> gap**: `MACROEXPAND`/`MACROEXPAND-1` never expanded a bare symbol naming a
> symbol-macro, only a cons-shaped macro call. `MACROLET.14` had been
> passing at the old baseline for the wrong reason — the exact case
> prompt.txt names: "if a test passes for the wrong reason, it is not
> progress" — so it was implemented rather than left alone. Full account in
> `docs/changelog.md`'s **2026-08-27 (b)** entry.
>
> The second full run (this one) is what turned "all six files are back at
> or below baseline, `gate.py` is clean, a directory sweep found nothing
> else" into a verified fact rather than an assumption: a SETF-place /
> macroexpansion / restart-association fix reaches too much of the suite for
> a targeted sweep to stand in for a full run (CLAUDE.md, dev-loop step 8's
> "wide blast radius" case). It confirms clean: +22 passing over the first
> 2026-08-27 run, 0 files worse than the newly-saved baseline.
>
> An earlier attempt at the *first* of these two runs (started 2026-08-26/27)
> was lost to a VSCode crash mid-run, not to a test failure — the working
> tree was already clean and every change already committed when that run
> started, so nothing was lost, only the run itself had to be relaunched.

> ### ⚠️ `ae7e4ca` and the two runs that reported nothing
>
> **The committed tree at `ae7e4ca` ("Repaired hash tables") ran _zero_ tests**,
> and said so only in the completeness line:
>
> ```
> COMPLETENESS: total=20612 passed=0 failed=0 accounted=0 missing=20612 extra=0
> Error loading file '...ansi-test\doit.lsp': CADR: invalid structure
> ```
>
> `ae7e4ca` did not introduce the defect — it made GETHASH return its
> specified *two* values (CLHS 18.2) and thereby exposed one that had been
> latent for as long as `BindingFrame` has existed: **a binding held the
> `MultipleValues` wrapper instead of the primary value**, so
> `(let ((x (floor 7 2))) x)` answered `#<MULTIPLEVALUES 3 1>`. RT's
> `add-entry` is `(let* ((pred (gethash ...))) ... (setf (cadr pred) entry))`,
> so `pred` was the wrapper and the SETF signalled. `init.lsp` loads the test
> files through the Lisp `LOAD`, which propagates, so the whole suite aborted
> at load. Fixed in `binding.BindingFrame.bind` — the one place every binding
> form goes through.
>
> **Three lessons, in descending order of how much they cost:**
>
> 1. **`ae7e4ca` was committed without a full run.** `scripts/run_ansi.py`
>    starts at `gclload1.lsp` and *stands in for* `gclload2.lsp` (its own
>    comment says so), so it never loads `init.lsp` and every targeted run
>    looked healthy. This is the asymmetry CLAUDE.md already names as the one
>    thing the targeted loop structurally cannot verify — and the operator
>    list it gives (APPEND/DIRECTORY/MAPC/…) is **not** the whole exposure:
>    the trigger here was GETHASH plus variable binding.
> 2. **A run can look finished and be a fraud.** Exit code 0, a normal
>    "Results written to…", and RT's own `N failures … out of 21881 tests`
>    line all print regardless. Only `COMPLETENESS:` and `accounted == total`
>    distinguish them, which is why merging a run into the checklist must be
>    gated on `unaccounted == 0`.
> 3. **A single test can end the run.** RT's `do-entries` iterates with
>    `DOLIST`, which establishes an implicit NIL block, so *any* stray
>    `(return ...)` escaping a test is caught there and silently truncates the
>    suite — no error, no diagnostic, just a short run. `loop.13.9` did
>    exactly this (see [§7](#preventing-regression)).
>
> **Registered tests fell 22132 → 21881 (−251) and that is not yet
> explained.** The per-file attribution in `docs/ansi_checklist.md` is
> unchanged directory for directory, so the difference is in the
> load-time-*generated* population, and `reader/set-syntax-from-char.lsp`
> alone now logs 27 `Redefining test` warnings (a redefinition replaces an
> entry rather than adding one). That accounts for a fraction, not all, of it.
> Treat `total` as an outcome — but a *fall* wants explaining, where a rise
> usually does not.

<details>
<summary>Previous full run (2026-08-22), kept for the analysis it carries — note its numbers were the last ones measured before <code>ae7e4ca</code> stopped the suite running at all</summary>

| | value | previous full run (2026-08-18) |
|---|---|---|
| Registered tests | 22132 | 22124 |
| Executed (`accounted`) | **22132 (100%)** | 22124 (100%) |
| Passed | **19703 (89.0%)** | 17087 (77.2%) |
| Failed | **2429** | 5037 |
| Never executed | **0** | 0 |
| Wall time | ~125 minutes (7445s) | ~86 minutes |

**+2616 passing, and this run spans nine commits, not one** — `743581f`
(files), `2567fb9` (FORMAT `~<~:>`), `9e27ab5` ((SETF FIND-CLASS)), `48ce713`
(DEFGENERIC/DEFMETHOD congruence), `8609009` (binary streams, macro `&key`
ordering), `a351743` (conditions), `a34ab2e` (numbers), `37ead34` (objects)
and `679452f` (the ordinary lambda list). The total is theirs jointly; per
mechanism deltas measured on both sides of a specific change are in
`docs/changelog.md`.

**The run is healthy, and that was checked rather than assumed.** Exit code 0,
`COMPLETENESS: OK`, `accounted == total`, 0 missing and 0 extra. The watchdog
warned three times that no progress had been made for ~120s, and each warning
was followed by `RESOLVED: progress resumed` — so there were three slow
stretches and **no hang**; the 900s hard stop never fired.

**Wall time rose 86 → 125 minutes (+45%), and that is expected rather than a
regression** — the same shape as 08-15 → 08-16. 2616 more tests pass, which
means 2616 more assertions actually execute instead of failing early, and the
new lambda-list arity checks make `check-type-error`-style helpers call the
function under test where they previously did not. A measured ~5% of it is the
`BindingFrame` construction every function call now does (see
[§7](#preventing-regression) on the absence of a speed gate — this is the
number that would have been caught automatically if one existed). Treat wall
time as a measurement, not a constant.

**Six files regressed against the 2026-08-18 baseline** — recorded in
[§7](#open-regressions-carried-by-the-2026-08-22-full-run) rather than cleared.
The baseline was **not** refreshed: under
[Ways to fake compliance](#ways-to-fake-compliance) it may be refreshed only
once a regression is understood and accepted in writing or fixed, and these
six are neither yet.

**`system-construction` (75) and `auxiliary` (2) are at 100%**, `pathnames` is
1 of 215 from it, and `arrays` and `cons` are both at 98.9%. The constraint is
now `printer` (203), `numbers` (157), `objects` (155) and `iteration` (140);
by *rate* it is `structures` (32.2%), `hash-tables` (55.7%) and `environment`
(57.8%), all three of which are dominated by an identified absent or duplicated
mechanism rather than by many separate bugs.

> **The working mode has changed** — see
> [§2's Working mode](#working-mode-tail-mode-2026-08-22). The failure
> distribution is a tail, not a set of clusters, and the Tier 1/2 rankings
> in [§3](#3-the-checklist) are kept as history rather than as a plan.
>
> **`docs/ansi_checklist.md` is regenerated from this run** and carries no
> merge amendments: a full run supersedes them and `run_all_tests.py` deletes
> `ansi_results/merges.log`. Every number in it is therefore from one
> self-consistent run — including the cross-session merge of 2026-08-22T13:23,
> which was made against a half-edited tree and is now gone.

</details>


<details>
<summary>Previous full run (2026-08-18), kept for the analysis it carries</summary>

**Latest full run: 2026-08-18. Over three quarters passing.**

**Latest full run: 2026-08-18. Over three quarters passing.**

```
COMPLETENESS: total=22124 passed=17087 failed=5037 accounted=22124 missing=0 extra=0
COMPLETENESS: OK
```

| | value | previous full run (2026-08-16) |
|---|---|---|
| Registered tests | 22124 | 22113 |
| Executed (`accounted`) | **22124 (100%)** | 22113 (100%) |
| Passed | **17087 (77.2%)** | 14772 (66.8%) |
| Failed | 5037 | 7341 |
| Never executed | **0** | 0 |
| Wall time | **~86 minutes** (5163s) | ~113 minutes |

**+2315 passing, and the wall time *fell* 113 → 86 minutes.** Both directions
are the same cause and it is worth reading carefully, because it is the
opposite of the 08-15 → 08-16 movement, where more real work made the run
slower. Here the time was being spent on work that was not merely wasted but
*destructive*: the printer had no bound on a circular structure, so a handful
of forms in `printer/` burned minutes and gigabytes each before failing. Two
runs before this one died outright — one at 10GB, one at 21GB — and neither was
a new defect. See the [Changelog](docs/changelog.md).

**This run spans four commits, not one.** `598af8d` (SUBTYPEP), `43fbffb`
(CLOS), `6de426c` and `896d935` (cons/sequences) landed between the 08-16 run
and this one, so the +2315 is their total and not attributable to any single
change. The per-mechanism deltas in the Changelog are the ones measured with
`run_ansi.py` on both sides of a specific change.

> **Three consecutive attempts at this run died before it completed, and none
> of the three causes was a new defect.** Recorded because the *shape* keeps
> recurring: an unbounded traversal in the printer is invisible to the
> development loop and fatal to the scoreboard.
>
> 1. `DIRECTORY` returned a Python `list` — a *vector* here — and ansi-test's
>    `init.lsp` opens with `(append (directory ...) ...)`. **No targeted run
>    can catch this**: `run_ansi.py` starts at `gclload1.lsp` and never loads
>    `init.lsp`. That asymmetry is now the one thing the targeted loop
>    structurally cannot verify.
> 2. The printer had no bound on a circular *cdr chain* (10GB).
> 3. `PPRINT-FILL` and five siblings were stubs calling Python's `print()`,
>    which rendered through `lispCons.__str__` — the pre-printer path, with no
>    guards at all (21GB, then 11GB).
>
> All three are fixed. (2) and (3) had been latent for runs: `print.cons.random.2`
> wires a **random** cons graph, so whether it cycles depends on the draw, which
> is why earlier full runs completed with the same code.

**These numbers are the last full run and move only on a full run.** The
*checklist* is kept current between full runs by merging targeted runs into it
(see [below](#keeping-the-checklist-current-without-a-full-run)); its header
lists which runs it has been amended with. Do not copy an amended count here.

**Registered tests rose 22113 → 22124 (+11).** As in the 08-12 and 08-16 runs, a
rise here is not new work appearing from nowhere: tests generated at load time
only register once the code that generates them runs, so fixing a load-time
failure *adds* tests. Treat `total` as an outcome, not a constant.

**`cons` is effectively finished (99.0%) and `sequences` is at 94.9%**, so the
two directories this plan led with for three revisions are no longer the
constraint. `objects` (422), `streams` (319) and `printer` (302) are, and
between them they hold 21% of the remaining failures.

### Per-directory scoreboard (complete)

Ordered by failures. `Δ rate` is against the 2026-08-16 full run.

| directory | failed | total | pass rate | Δ rate |
|---|---|---|---|---|
| objects | **422** | 825 | 48.8% | **+20.8** |
| streams | **319** | 547 | 41.7% | +3.2 |
| printer | **302** | 788 | 61.7% | +9.5 |
| data-and-control-flow | 277 | 1420 | 80.5% | +2.0 |
| numbers | 208 | 1438 | 85.5% | **+13.3** |
| strings | 175 | 501 | 65.1% | +5.6 |
| sequences | 161 | 3158 | **94.9%** | +6.0 |
| iteration | 140 | 838 | 83.3% | +2.4 |
| pathnames | 133 | 215 | 38.1% | +3.2 |
| conditions | 131 | 303 | 56.8% | −0.3 |
| structures | 102 | 115 | **11.3%** | −0.9 |
| arrays | 98 | 1245 | **92.1%** | +0.2 |
| reader | 95 | 165 | 42.4% | **+20.6** |
| packages | 87 | 340 | 74.4% | **+33.8** |
| environment | 83 | 192 | 56.8% | 0 |
| hash-tables | 72 | 158 | 54.4% | +0.6 |
| eval-and-compile | 70 | 318 | 78.0% | 0 |
| system-construction | 63 | 75 | **16.0%** | +1.3 |
| files | 58 | 87 | 33.3% | +4.6 |
| types-and-classes | 56 | 545 | **89.7%** | **+51.5** |
| misc | 39 | 740 | **94.7%** | +4.2 |
| symbols | 37 | 1145 | **96.8%** | +0.3 |
| cons | **16** | 1638 | **99.0%** | **+38.5** |
| characters | 10 | 259 | **96.1%** | **+37.8** |

**`types-and-classes` recovered +51.5 points, which settles a question this
section left open at 08-16.** That run recorded it as "the one directory that
clearly went backwards" (−14.1) and offered a hypothesis — that the LOOP
`unless` repair had converted vacuous passes into honest failures — explicitly
flagged as **not yet verified**. The recovery came with the SUBTYPEP lattice and
the CLOS work, so the −14.1 was a real gap being reported honestly rather than a
measurement artifact. The hypothesis was wrong in its mechanism and right in its
direction; either way it is now closed.

**`characters` +37.8 and `cons` +38.5 are the two largest single-directory
moves**, and `cons` at 16 failures of 1638 is the first directory to approach
done. What remains there is not cons: five of the sixteen are the SETF place
protocol ([M5](#4-milestones--re-scoped)), and the rest are a lambda-list arity
check and `MAKE-LIST.ERROR.1`.

`structures` (11.3%) and `system-construction` (16.0%) are unchanged and remain
the subsystems where one absent mechanism fails everything downstream of it.

> **`system-construction` is done as of 2026-08-20 (targeted): 77 of 77.**
> `files` 29 → 47 of 87 in the same work. Not yet reflected in the table above,
> which only moves on a full run. It took **eleven** mechanisms and only three
> of them were about building systems — see the
> [Changelog](docs/changelog.md) entry, which is worth reading before ranking any
> other low-percentage directory as a "subsystem gap".

> **`pathnames` is done as of 2026-08-21 (targeted): 214 of 215 (99.5%),
> up from 82 of 215 (38.1%).** This is Tier 2's C11 (the pathname half) closed:
> `Pathname` was a namestring wrapper — a parsed `pathlib.Path` plus the
> original string — with no representation for a wildcard, an
> `:absolute`/`:relative` marker, or a component that was simply never
> supplied, so `MAKE-PATHNAME`/`MERGE-PATHNAMES`/`DIRECTORY` had nothing to
> *compose*. It is now a component record (host/device/directory/name/type/
> version, CLHS 19.2) with real parsing and rendering in both directions,
> plus a working logical-pathname/translation mechanism `misc_macros.py` had
> been silently shadowing with three no-op stubs (standing rule 3 — import
> order, not a missing feature, was the defect). The one surviving failure,
> `PATHNAMES-PRINT-AND-READ-PROPERLY`, is a real representational gap left
> alone rather than hacked around: a physical pathname's `VERSION` has no
> namestring syntax to round-trip through when `NAME`/`TYPE` are both NIL, so
> `(make-pathname :version :newest)` and `(make-pathname :version :wild)`
> both print as `#P""` and read back with `VERSION` NIL. See the Changelog.

</details>

### Per-directory scoreboard (2026-08-27 full run, complete)

Ordered by failures. Regenerate from `docs/ansi_checklist.md`, which is
generated from this run's raw output.

| directory | failed | total | pass rate |
|---|---|---|---|
| objects | 96 | 824 | 88.3% |
| numbers | 95 | 1438 | 93.4% |
| printer | 89 | 788 | 88.7% |
| data-and-control-flow | 77 | 1420 | 94.6% |
| iteration | 76 | 838 | 90.9% |
| sequences | 72 | 3158 | 97.7% |
| packages | 66 | 340 | 80.6% |
| streams | 64 | 547 | 88.3% |
| types-and-classes | 40 | 545 | 92.7% |
| eval-and-compile | 30 | 318 | 90.6% |
| symbols | 28 | 1145 | 97.6% |
| misc | 22 | 740 | 97.0% |
| conditions | 21 | 303 | 93.1% |
| files | 18 | 87 | 79.3% |
| cons | 14 | 1638 | 99.1% |
| reader | 14 | 165 | 91.5% |
| arrays | 12 | 1245 | 99.0% |
| characters | 7 | 259 | 97.3% |
| strings | 6 | 501 | 98.8% |
| environment | 6 | 192 | 96.9% |
| pathnames | 1 | 215 | 99.5% |
| auxiliary | 0 | 2 | 100.0% |
| hash-tables | 0 | 158 | 100.0% |
| structures | 0 | 115 | 100.0% |
| system-construction | 0 | 75 | 100.0% |

**`structures` reached 100%, from the 33.0% the previous full run (2026-08-24)
recorded as "the one genuine subsystem gap".** Not this session's work — the
"Repaired structures" commits that fixed it predate the 2026-08-27 (a) batch
but postdate 2026-08-24, and this is the first full run since to measure it.
`system-construction`, `auxiliary`, `hash-tables` and now `structures` are
complete; `pathnames` is one test from it.

**`printer` dropped from 201 (2026-08-24) to 89** — the format engine and
reader work in `docs/changelog.md`'s 2026-08-27 (a) entry. `objects` (96) and
`numbers` (95) are now the largest two directories; neither was touched this
session, so they are the next place to look for a shared mechanism rather
than individually-diagnosed files.

### Diagnosed, not yet fixed: `environment/documentation.lsp` (57 of 58)

Investigated 2026-08-26 against the 2026-08-26 full run (1730 failing). The
whole file is one absent mechanism: **DOCUMENTATION is not a generic
function here, and `(SETF DOCUMENTATION)` does not exist.**

What the code actually has:

- `misc_macros.py` registers `DOCUMENTATION` as a plain `cl_function` that
  reads `symbol.plist['DOCUMENTATION']` and answers NIL for anything that is
  not a `LispSymbol`. So `(documentation fn t)` on a *function object*,
  `(documentation class 'type)`, `(documentation pkg t)` and
  `(documentation method t)` all answer NIL unconditionally.
- There is **no `(SETF DOCUMENTATION)` registration anywhere**, so every
  `(setf (documentation x y) doc)` in the file falls through to CLHS
  5.1.2.9's generic fallback and calls a nonexistent `#'(setf documentation)`
  — the SETF *form* appears to succeed only where nothing checks its result.
- CLHS 25.1.3 makes both DOCUMENTATION and (SETF DOCUMENTATION) **standard
  generic functions**; ansi-test's last section defines methods on them
  (`documentation.new-method.1`) and cannot pass without real dispatch.

Where the fix belongs (all verified present):

- The `_PROTOCOL_DEFAULTS` installer list in `lispfunc/misc_clos.py` — the
  same mechanism SLOT-UNBOUND/DESCRIBE-OBJECT use — is the right home for
  default methods on both names, so user DEFMETHODs override by ordinary
  dispatch.
- Storage already exists per doc-type and needs one reader/writer each:
  functions/macros → `symbol.plist['DOCUMENTATION']` (DEFUN/DEFMACRO already
  write it via `split_function_body`); variables → the same plist key
  (DEFVAR/DEFPARAMETER/DEFCONSTANT write `VARIABLE-DOCUMENTATION` too);
  classes/structures → `LispClass.documentation` (DEFCLASS/DEFSTRUCT already
  populate it); methods → `Method` has no documentation field yet (add one;
  `_make_method_function` already extracts the docstring via
  `split_function_body` but discards it); packages → `Package` has no
  documentation field yet; DEFTYPE stores no doc either.
- The old `cl_function` in `misc_macros.py` must be **deleted**, not kept
  beside the GF (standing rule 3; `utilities_misc.py` re-exports it).
- Doc-type normalization needed: `t`, `function`, `compiler-macro`, `setf`,
  `variable`, `type`, `structure`, `method-combination` — tests exercise
  each against symbols *and* function/class/package/method objects.

Expected reach: ~57 tests here, plus whatever `describe` gains once it can
read real documentation. Nothing else was probed beyond this file.

**Tail mode still holds, and has tightened**: 308 files contain failures, the
median failing file has **3**, the largest is **62 (4.6% of the remainder)**,
and only **17 files / 82 tests** fail 100% (was 20 files / 132 tests).


---

## 2. How to work

### The development loop

> **`CLAUDE.md`'s "The development loop" is the canonical one** — it is what
> gets read first. This section is the *checklist mechanics* behind it: what a
> merged count means, what the baseline gate does, and what a targeted run
> structurally cannot see. Keeping two step-by-step lists in step is standing
> rule 3 applied to documentation, so if the steps below and CLAUDE.md's ever
> disagree, CLAUDE.md is right.

**`docs/ansi_checklist.md` is the authority for what is broken.** This plan
explains *why*; the checklist says *what and where*. When the
two disagree, the checklist is right — it is regenerated from RT's own output,
whereas prose in this document ages.

1. **Open `docs/ansi_checklist.md`** and pick a **file** — cheapest first, and
   the 20 files still failing 100% first of all. (This step used to say "pick a
   *cluster*, never a file"; see
   [Working mode](#working-mode-tail-mode-2026-08-22) for why it no
   longer does. Never pick a *test* — that part has not changed.)
2. Reproduce it in the smallest expression that shows the defect.
3. Fix the **mechanism**. Consolidate onto an existing helper if one exists.
4. **Verify with the targeted command printed next to that checklist entry.**
5. Run `pytest` for regressions.
5a. `pipenv run python scripts/duplicates.py --baseline` — under a second, and
   it is the only automatic check for the defect class that has cost this
   project the most (see [the duplicate register](#the-duplicate-register--the-one-place-a-cluster-argument-still-holds)).
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

Four rules that keep this honest:

- **A merge records no provenance, and a bad merge is invisible.** On
  2026-08-22 a run launched from a *different session* merged while this tree
  was half-edited and logged `regressed 57` — 57 files marked worse on the
  evidence of a tree that never existed as a commit. Nothing in
  `merges.log`, the checklist header, or `--update-checklist`'s own
  `unaccounted` guard can tell that entry from a good one. **Until the merge
  records the git HEAD and whether the tree was dirty, treat a merge you did
  not personally launch as unverified**, and never merge from two processes at
  once. (Recording `git rev-parse HEAD` plus `git status --porcelain`'s
  emptiness in `merges.log` is the fix; it is not written yet.)

- **Cancelling a run does not stop it.** Same day, an hour later, and worse
  because it was self-inflicted: a sweep was cancelled through the agent
  harness and *relaunched* against the corrected tree, and both were still
  running 35 minutes later — the cancel had detached the task, not killed the
  `pipenv → python → python` tree. The two competed for CPU, both held the
  same `>` redirect open at independent offsets (so the log interleaved and
  its size froze at the other's high-water mark), and both were headed for
  `--update-checklist`, the second of them carrying pre-fix code loaded into
  memory at import time. **Verify with the process list, not the task list**,
  and kill the whole tree:
  ```powershell
  Get-CimInstance Win32_Process -Filter "Name='python.exe'" |
    Where-Object { $_.CommandLine -like '*run_ansi*' } |
    Select-Object ProcessId, CreationDate, CommandLine
  ```
  Two entries with different `CreationDate`s means two runs. A live run shows
  ~1s of CPU per second of wall clock and flat working set; that — not log
  growth — is how to tell running from wedged, because a shared log file stops
  growing while both processes are still writing to it. **Read the outcome
  from `ansi_results/targeted-last.json`, never from a redirected log**; the
  JSON is written once, at the end, by whichever process actually finishes.

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

### Checklist discipline

> **Amended 2026-08-22.** The first two bullets below were written when half
> the suite was failing and one mechanism really could unblock a directory.
> Working the checklist file-by-file is now the *right* mode
> ([Working mode](#working-mode-tail-mode-2026-08-22)) — but the
> instrument stays: after every fix, still ask which failures disappeared
> that you did not target. It no longer decides whether the fix was *wrong*;
> it decides whether you have just found one of the few shared mechanisms
> left, and should keep pulling.

- ~~A fix that moves one checklist line is almost certainly the wrong fix.~~
  A fix that moves one checklist line is now the normal case. A fix that moves
  *several files you did not target* is the signal to stop and look for what
  else that mechanism holds up.
- After each fix, ask *which failures disappeared that you did not target.*
- A test that passes for the wrong reason is not progress.
- ~~Per-test work becomes correct only in [Tier 3](#tier-3--the-genuine-tail).~~
  Per-*file* work is correct now. Per-*test* work still is not: three
  `.ERROR.n` tests wanting a PROGRAM-ERROR are one argument-validation fix,
  not three `raise` statements.

### The duplicate register — the one place a cluster argument still holds

> **Updated 2026-08-24: 22 → 11.** The nine hash-table operators are resolved
> — `lispfunc/hashtables.py` is deleted and `misc_hashtables.py` is the one
> model. `hash-tables/` went **55.7% → 100%** (70 failing → 0), which is the
> largest single-directory move this register has produced and the first
> confirmation of its central claim. Two things about *how* it went are worth
> more than the number:
>
> - **Deleting the dead copy was necessary and nowhere near sufficient.** The
>   register predicted ~29 tests from `HASH-TABLE-P` alone; the actual cause of
>   the other ~40 was that the *live* implementation had no key-equivalence
>   model at all — `HashTableDict` was a `dict` whose `test` attribute nothing
>   read, so the declared test was decoration and an EQUAL table could not find
>   a list key it had just stored. A duplicate-registration entry names *where*
>   two answers compete; it does not tell you the surviving answer is right.
> - **The fix reached outside the directory**, which is the tail-mode signal
>   §2 asks for. `(SETF GETHASH)` had **four** separate writers all doing
>   `table[key] = value` on the raw dict, EQUALP had no hash-table clause
>   (CLHS 5.3 — `data-and-control-flow`'s `equalp.21`–`.29`), LOOP's
>   `being the hash-keys` error path raised a Python `TypeError` from its own
>   `LispTypeError` call, and SXHASH's specified *return type* exposed three
>   surviving copies of the fixnum boundary (see §1's note). None of those is
>   in `hash-tables/`.

**22 operators are registered from two different modules, and nothing said so
until 2026-08-22.** `registry.cl_function` ends in
`function_registry[lisp_name] = entry` — last writer wins, silently — so which
implementation runs is decided by *import order*, and a module that is never
imported still competes. Standing rule 3 has produced this project's largest
single wins (nine LOOP engines, five copies of the pathname search, three
array shapes, three lambda-list binders), and every one of them was found by
accident, weeks later, while chasing something else.

```powershell
pipenv run python scripts/duplicates.py             # the register
pipenv run python scripts/duplicates.py --baseline  # exit 1 on a NEW one
pipenv run python scripts/duplicates.py --save-baseline
```

`docs/duplicates_baseline.json` is the **known debt**, not an approval list.
The gate is "no *new* duplicate"; the 23 in it are work.

| duplicated operators | modules | what it plausibly costs |
|---|---|---|
| ~~`MAKE-HASH-TABLE`, `GETHASH`, `REMHASH`, `CLRHASH`, `MAPHASH`, `HASH-TABLE-P/-COUNT/-SIZE/-TEST` (9)~~ | ~~`hashtables.py` (dead) vs `misc_hashtables.py` (live)~~ | **done 2026-08-24.** `hashtables.py` deleted; `misc_hashtables.py` is the one object model. The dead copy's `HASH-TABLE-P` was indeed answering NIL for every real table, but the *live* copy had no key-equivalence model either — see the note above. `hash-tables/` **55.7% → 100%** |
| `MAKE-INSTANCE`, `CLASS-OF`, `FIND-CLASS`, `CALL-NEXT-METHOD`, `ENSURE-GENERIC-FUNCTION` (5) | `lispfunc/classes.py` vs `misc_clos.py` | the "two CLOS implementations" of [§5](#5-known-temporary-deviations), now enumerated. `objects/` 80.9%, and `ensure-generic-function.lsp` is 13 failing of 16 |
| `MAKE-STRING-INPUT-STREAM`, `MAKE-STRING-OUTPUT-STREAM`, `GET-OUTPUT-STREAM-STRING` (3) | `io_read.py`/`io_write.py` vs `streams.py` | `streams/` 83.0% |
| ~~`GET-UNIVERSAL-TIME`, `DECODE-UNIVERSAL-TIME` (2)~~ | ~~`core.py` vs `utilities_system.py`~~ | **done 2026-08-24.** `utilities_system.py` is the one home of the universal-time model (CLHS 25.1.4) and `core.py`'s five time operators are deleted. The register understated it: the copy that *won* on import order took **no `time-zone` argument at all**, so every test passing one got a Python `TypeError`, and the loser's ENCODE went through `time.mktime` (local-zone, and out of range before 1970). `environment/` **57.8% -> 95.8%** |
| `ERROR` | `io_write.py`(!) vs `utilities_errors.py` | `ERROR` is the condition system's entry point; which one runs decides whether a raise site signals |
| `FILE-WRITE-DATE` | `io_write.py` vs `pathnames.py` | |
| `GRAPHIC-CHAR-P` | `characters.py` vs `core.py` | |
| `find_class` defined twice in `classes.py` | line 273 dead, line 1249 wins | not previously recorded; [§5](#5-known-temporary-deviations) named `_init_builtin_classes`, which has since been resolved |

**This is the highest-value work remaining and it is not in the tail.** It is
~40 tests of directly attributable failure in `hash-tables/` and
`environment/` alone, it is mechanically enumerated rather than guessed, and
deleting a duplicate cannot regress a file that was already running the other
copy — the two answers were never both reachable.

**Add `--baseline` to the development loop** (step 5a below). It costs under a
second and it is the only check in this project that catches the defect class
that has cost it the most.

### Working mode: tail mode (2026-08-22)

**Read this before picking anything up.** The mechanism-first mode the rest of
this document is written in has expired. Its premise was that with roughly half
the suite failing, a small number of core mechanisms binds most of the
failures, so a fix that only moves the file you targeted is suspect. Measured
against the live checklist, that premise no longer holds:

| | 2026-08-16 | 2026-08-22 | 2026-08-24 |
|---|---|---|---|
| failing tests | 7341 | 2522 | **2102** |
| files containing failures | — | 363 | 308 |
| median failures per failing file | — | 3 | **3** |
| largest single failing file | — | 62 (3.6% of the remainder) | **62 (4.6%)** |
| files failing **100%** | 49 files / 493 tests | 20 files / 132 tests | **17 files / 82 tests** |

**The largest file is a larger *share* of a smaller remainder**, which is the
shape tail mode predicts: nothing is left to lead with, and `printer` holds all
twelve of the biggest files.

Regenerate any of those:

```powershell
pipenv run python -c "import re;rows=[(int(m.group(2)),int(m.group(3)),m.group(1)) for l in open('docs/ansi_checklist.md',encoding='utf-8') for m in [re.match(r'^- \[ \] .(\S+). — \*\*(\d+)\*\* failing of (\d+)$',l)] if m];c=sorted((r[0] for r in rows),reverse=True);print('files',len(c),'median',c[len(c)//2],'max %%%.1f'%(100*c[0]/sum(c)));print('100%%:',sum(1 for f,t,_ in rows if f==t),'files',sum(f for f,t,_ in rows if f==t),'tests')"
```

**What tail mode means in practice**

- **Work the checklist file by file**, cheapest-first. The 20 files still
  failing 100% are the cheapest and the clearest: a file at 100% means the
  operator is absent or fundamentally broken, not merely buggy.
- **Keep every other part of the discipline.** Reproduce the smallest failing
  case; find the actual defect; fix the mechanism, not the test; check what
  moved that you did not target. Tail mode changes *how you choose the next
  file*, not how you work it.
- **Do not stop looking for shared mechanisms — stop *assuming* one is there.**
  They are now rare and they no longer announce themselves as a big directory;
  they show up as the same shape in unrelated places. The 2026-08-22
  ordinary-lambda-list fix is the model: 79 failures in `flet.lsp`,
  `labels.lsp`, `lambda.lsp` and `macrolet.lsp` — three different directories,
  none of them near the top of the list — because FLET/LABELS, LAMBDA and
  DEFUN each had a private binder. It was found by *opening a file and reading
  the failures*, not by ranking directories.
- **Re-derive the ranking from the live checklist.** [§3](#3-the-checklist)'s
  Tier 1/2 lists and cluster tables were written against a 7,000–12,000
  failure suite; they are kept below as history and their ordering is wrong now.
- **This is not a one-way door.** If a full run pushes failures back above
  3,000, or a new test population registers (see [§1](#1-status) on `total`
  rising), fall back to mechanism-first until it drops again.

<details>
<summary>The trigger as originally written (2026-08-18), and why half of it was
the wrong measurement</summary>

The original threshold was "fewer than 3,000 failing tests **and** fewer than
100 files containing failures". The first half fired; the second never will and
never should have been the test. A 22,000-test suite with 2,500 failures spread
across a long tail touches hundreds of files *by construction* — 107 of the 363
have exactly one failure. Counting files measures how thinly the remainder is
spread, which is the opposite of what the trigger wanted to know. The
replacement conditions are **largest single file below ~5% of the remainder**
and **median failing file ≤ 3**, both of which say directly that no cluster is
left to lead with.

The original text:

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

</details>

### The checklist artifact

**`docs/ansi_checklist.md`** is the working checklist: all 5037 failures grouped
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

> ### ⚠️ This whole section is history as of 2026-08-22.
>
> Its rankings were derived from runs with 7,341 and 12,000+ failures, when a
> handful of mechanisms really did bind most of the suite. They no longer
> describe what is left: most of Tier 1 is done, the cluster table below is six
> days and 4,800 failures stale, and the failure distribution has flattened
> into a tail ([§2](#working-mode-tail-mode-2026-08-22)).
>
> **Pick your next file from `docs/ansi_checklist.md`, not from here.** Keep
> reading this section for *why* a mechanism is the shape it is — the C*
> entries record diagnoses that were wrong and how, which is the part that
> stays useful — but do not take its ordering.
>
> The one table below that is regenerated and current is
> [Files failing 100%](#files-failing-100--the-strongest-mechanism-absent-signal).

### Cluster sizes (complete data, 7341 failures, 2026-08-16 — stale, see below)

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

**20 files fail every test they contain — 132 tests (2026-08-22).** A file at
100% is qualitatively different from a file at 60%: the operator is not merely
buggy, it is **absent or fundamentally broken**, so nothing downstream of it can
pass. In tail mode these are the cheapest and clearest work in the suite, and
this is the one ranking in §3 that is still live.

The signal is also nearly exhausted, which is itself the headline: 49 files /
493 tests at 08-16, 20 files / 132 tests now, and none of the survivors is
larger than 29. Every entry that used to lead this table — `make-package.lsp`,
`defpackage.lsp`, `defmethod.lsp`, `pushnew.lsp`, `ldiff.lsp`, `tailp.lsp`,
`check-type.lsp`, `modules.lsp`, `make-pathname.lsp` — is gone from it.

```powershell
# regenerate this list
pipenv run python -c "import re;[print(m.group(2).rjust(3),m.group(1)) for l in open('docs/ansi_checklist.md',encoding='utf-8') for m in [re.match(r'^- \[ \] .(\S+). — \*\*(\d+)\*\* failing of \2$',l)] if m]"
```

> **This table is dated 2026-08-22 and several rows below are stale — see the
> per-row notes.** It has not been regenerated wholesale since (that needs a
> full run's checklist, which is currently in flight — see §1); the notes
> record what individual targeted runs since then have confirmed.

| tests | file | mechanism absent |
|---|---|---|
| ~~29~~ | ~~`hash-tables/make-hash-table.lsp`~~ | **Fixed 2026-08-24 — `hash-tables/` is 100%.** Was **not** `:test` as a designator, which is what this table said for three revisions; the actual cause was the second, dead hash-table implementation winning import order (standing rule 3) with no key-equivalence model at all. See `docs/changelog.md`'s 2026-08-24 entry |
| 15 | `packages/with-package-iterator.lsp` | package iterator absent |
| 13 | `hash-tables/with-hash-table-iterator.lsp` | hash iterator absent |
| 5 (was 12) | `reader/set-syntax-from-char.lsp` | **Mostly fixed 2026-08-26/27** — a real character *syntax type* model now exists (C12) and `SET-SYNTAX-FROM-CHAR` acts on it; 0/75 → 67/75. No longer 100%-failing; the 5 remaining are unprobed |
| ~~11~~ | ~~`printer/format/format-justify.lsp`~~ | **Fixed 2026-08-27 — 59/59.** `~<...~>` justification (C2) rewritten to spec: `~^` segment-discard, `~T` real column tracking, `~n,m:;` line-overflow prefix, `colinc` rounding. See `docs/changelog.md` |
| 8 | `environment/time.lsp` | `TIME` absent |
| ~~6~~ | ~~`printer/format/format-tilde.lsp`~~ | **Fixed — 10/10** (earlier of the two 2026-08-27-logged sessions) |
| 5 | `printer/print-integers.lsp` | printer radix/base |
| ~~5~~ | ~~`printer/format/format-percent.lsp`~~ | **Fixed — 11/11** |
| 6 (was 4, file grew) | `printer/format/format-paren.lsp` | **Partially fixed** — 12/50 → 44/50; `~(~)` case conversion (C2) mostly works, 6 remain |
| 4 | `printer/format/format-p.lsp` | `~P` (C2) |
| ~~4~~ | ~~`objects/make-instances-obsolete.lsp`~~ | **Fixed — 4/4.** `MAKE-INSTANCES-OBSOLETE` plus the lazy `UPDATE-INSTANCE-FOR-REDEFINED-CLASS` hook |
| ~~4~~ | ~~`objects/defclass-forward-reference.lsp`~~ | **Fixed 2026-08-27 — 4/4.** CLHS 4.3.7 forward-referenced classes; see `docs/changelog.md` |
| 3 | `streams/stream-error-stream.lsp` | `STREAM-ERROR-STREAM` absent |
| ~~3~~ | ~~`printer/print-unreadable-object.lsp`~~ | **Fixed — 7/7.** `PRINT-UNREADABLE-OBJECT` as a real macro (CLHS 22.4.1) |
| 2 | `objects/unbound-slot.lsp` | `UNBOUND-SLOT` condition |
| 1 each | `printer/print-structure.lsp`, ~~`print-ratios.lsp`~~, `print-random-state.lsp`, `pathnames/pathnames.lsp` | `print-ratios.lsp` **fixed 2026-08-27 — 1/1** (CLHS 2.3.1 numeric-token syntax, `fclpy/numtoken.py`); the other three unprobed |

**Three of the top five were one absent iterator or model each**, which is what
"cheapest wins" means at this stage: `make-hash-table.lsp` (29, now fixed),
`with-package-iterator.lsp` (15) and `with-hash-table-iterator.lsp` (13) are
57 tests behind three named mechanisms, two still open. The `printer/format/*`
entries were C2's remaining directives and the one place a *cluster* argument
still held; most of that cluster is now closed (`format-justify`,
`format-tilde`, `format-percent` all fixed; `format-paren` mostly).

<details>
<summary>The same table at 2026-08-16, for comparison (49 files, 493 tests)</summary>

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

</details>

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
**Owner:** M8's raise-site migration ([C8](#c8-clos--defgeneric--defmethod--defclass--change-class)).

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
because the mechanism moved. Details in the [Changelog](docs/changelog.md).

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
  See the [Changelog](docs/changelog.md). **`AND`-joined parallel FOR clauses remain**
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

#### C3. Sequence functions — **LARGELY DONE (2026-08-18 b)**

**`sequences/` is 2997 passing of 3158 (94.9%)**, from 990 (31.3%) when this
section was written. X2/X3 were the bulk of it, and the list-traversal
mechanism ([Changelog](docs/changelog.md) 08-18 b) took most of the remainder.

**What is left is one specific, bounded thing: the `**kwargs` families have not
declared their `&key` sets.** `FIND`/`POSITION`/`COUNT` and their `-IF`
variants, `REMOVE`/`DELETE`/`SUBSTITUTE`/`NSUBSTITUTE` and theirs,
`MISMATCH`/`SEARCH`/`REPLACE`/`MAKE-SEQUENCE`/`MERGE`/`REDUCE` all take
`**kwargs`, so `split_keyword_args` cannot validate a keyword against them and
their `.ERROR.3`-shaped tests (unrecognized keyword, dangling keyword,
`:allow-other-keys` handling) cannot pass. Migrating them is mechanical —
spell the `&key` parameters keyword-only — and it is the same edit that closes
`strings/string-comparisons.lsp` (72 failing of 138, the second-largest failing
file in the suite), whose `start1`/`end1`/`start2`/`end2` are ANSI `&key`.
**This is the recommended next task.** **Owner:** M6.

**Historical evidence below (2026-08-16).** **1266 failures** across
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

#### C5. Set and list operations — **DONE (2026-08-18 b)**

**`cons` 217 → 16 failures of 1638 (99.0%)**, and `set-exclusive-or`,
`nset-exclusive-or`, `union`, `nunion`, `intersection`, `nintersection`,
`set-difference`, `nset-difference`, `subsetp`, `member`, `adjoin`, `assoc` and
`rassoc` are all clean. Details in the [Changelog](docs/changelog.md).

**The "one shared `:test`/`:key` defect" reading was right about there being one
mechanism and wrong about which.** X2/X3 had already landed by the time this was
measured, and eleven operators were *still* failing in near-identical
proportion — because the shared defect was not in the test protocol at all. It
was that **nothing walked the list**: `seq_elements` folded a dotted list's
terminator in as an element and no operator checked that its argument was a list
at all, so `(union '(a b c) '(d e f . g))` answered a value and
`(union '(a b c) 'x)` leaked a Python type name. The set operations did have one
genuine test-protocol bug left, and it was narrower than X3: UNION/NUNION called
the test with an element of `list-2` first, because they iterated `list2` asking
whether each element was already present — the right *algorithm* and the wrong
*call*, which `union.28`-`.31` detect by `RETURN-FROM`ing out of the whole form.

**What remains in `cons` is not cons.** Five of the sixteen are the SETF place
protocol (`PUSH.ERROR.1`, `POP.ERROR.1`, `REST-SET-1`, `SETF-GETF.ORDER.2`,
`NTH.ORDER.1` — M5); `TREE-EQUAL.ERROR.6` needs a lambda list to reject a
too-few-arguments call (M3); the rest are `MAKE-LIST.ERROR.1` and two randomized
set tests. **Owner:** none — closed.

#### C6. Arrays — **LARGELY DONE (2026-08-15 d)**

**`run_ansi.py arrays`: 518 → 1233 passing of 1356. +715, and the failures fell
838 → 123.** Details in the [Changelog](docs/changelog.md).

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
[the 2026-08-15 (d) Changelog entry](docs/changelog.md).

#### C7. The printer — **LARGELY DONE (2026-08-14)**

**`run_ansi.py` over the 25 `printer/` object-printing files: 36 → 128 passing
of 306. +92, zero regressions.** Details in the [Changelog](docs/changelog.md).

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

**Method combination landed 2026-08-18** — see the [Changelog](docs/changelog.md).
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
| C11 | **Streams, files, pathnames** — `system-construction/` is **done** (77 of 77, 2026-08-20) and **`pathnames/` is done** (214 of 215, 99.5%, 2026-08-21 targeted): LOAD/COMPILE-FILE are real, the five copies of the relative-pathname search are one (`pathnames.resolve_filespec`), a missing file is a FILE-ERROR carrying its pathname, and `Pathname` is now a component record (host/device/directory/name/type/version) rather than a namestring wrapper, so `MAKE-PATHNAME`/`MERGE-PATHNAMES`/`DIRECTORY`/`PATHNAME-MATCH-P`/`TRANSLATE-PATHNAME` genuinely compose components, and a basic logical-pathname/translation mechanism exists. `files/` is not yet re-measured with the component model in place — the `(directory (make-pathname :version :wild :defaults p))` gap this row used to name should be closed by it, but confirm before assuming `delete-all-versions`/`rename-file` are fixed rather than merely no-longer-blocked. `streams/` remains large and untouched. | M10 |
| C12 | **Reader** — `reader/` 136 failing of 165 (**17.6%**). `#(1 2 3)` reads as the cons `(VECTOR 1 2 3)` (CLHS 2.4.8.3); the tokenizer interprets `\n` inside strings, where CLHS 2.4.5 requires backslash to be a single-escape included *without interpretation*. **Also: `fclpy/reader.py` is a dead ~480-line second reader** that nothing under `fclpy/` imports, yet **177 unit tests (14% of that suite)** certify it — while the live reader (`tokenizer.py` → `lispreader.py` → `readtable.py`) has essentially no unit coverage, and the two disagree on conformance. Retire it or repoint those tests. | M10 |
| C13 | **Strings** — `strings/` 388 failing of 501 (**22.6%**); `MERGE-STRING` 38. Rooted in the `LispString`/Python-`str` split (Finding I), which also blocks `EQUAL`/`EQUALP`. A length-1 `str` currently satisfies both `CHARACTER` and `STRING`, which are disjoint types (CLHS 4.2.2). | M9 |
| C14 | **Types / `SUBTYPEP`** — `SUBTYPEP` 156 (`SUBTYPEP.INTEGER` 46); `types-and-classes/` 262 failing of 545. `SUBTYPEP` is a string-pair lookup table with no type lattice (Finding F). | M9 |
| C15 | **Numeric tower** — `numbers/` 566 failing of 1438 (60.6% passing — better than most); `PARSE-INTEGER` 49, `ValueError: math domain error` leaks. Bignums, ratios, complex, float contagion. | Phase 4 |
| C16 | **Places / `SETF`** — `PSETF` 31, `PUSHNEW` 27, `ROTATEF` 23. Five parallel place protocols; `GET-SETF-EXPANSION` is a stub returning a Python 5-element list instead of five values; `PUSH`/`POP`/`PUSHNEW` are registered as *functions* over Python lists. No test pins either, so M5 is free to fix them. | M5 |
| C17 | **Lambda lists** — **the ordinary lambda list is done (2026-08-22)**: `flet.lsp`+`labels.lsp`+`lambda.lsp`+`macrolet.lsp` went 170 → 232 passing of 249 (79 → 17 failures), FLET 34→1 and LABELS 19→0, by deleting two of the three binders rather than repairing them. The diagnosis this row carried — "six copy-pasted binders" — was right about the count and wrong about the remedy being an *engine*: `_bind_ordinary_lambda_list_tail` already existed and was already correct, and only DEFUN reached it. What remains under M3 is the **macro** lambda list and `DESTRUCTURING-BIND`, which are a different lambda list (CLHS 3.4.4, nested patterns) and share `bind_destructuring_pattern`; neither signals a PROGRAM-ERROR for anything. | M3 |
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

> **These are audit items, not trivia**, and
> [the final compliance gate](#half-two--the-known-non-compliance-audit)
> requires them closed. A unit test that asserts a bug is how a known defect
> survives to the end: fixing the bug shows up as a broken test, so it gets
> deferred. Each of the three is either corrected or explicitly renamed and
> documented as a test of *non-ANSI* behaviour. `is_truthy(False)` belongs in
> [§5](#5-known-temporary-deviations) until it is fixed — it already is.

---

## 4. Milestones — re-scoped

Milestones now describe *mechanisms*, and map onto the clusters above.

| | milestone | state | clusters |
|---|---|---|---|
| **M0** | Trustworthy measurement | **DONE** — `COMPLETENESS: OK`, 22036/22036 accounted. `expected-failures/` is not an open item: it stays unwired **by policy**, see [Why `expected-failures/` stays unwired](#why-expected-failures-stays-unwired--deliberately) | — |
| **M1** | Symbol, NIL, package identity | canonical CL symbol table **done**; package model outstanding | C10, C18, C19 |
| **M2** | Environment model | **binding forms done**, and **the global environment done (2026-08-15)** — one `BindingFrame` decides lexical vs. dynamic for LET, LET* and all eight iteration forms, and a global variable has one home, the symbol's value cell. Outstanding: `is_truthy(False)`, and the lambda-list binders, which are M3's | C1, X2 |
| **M3** | One lambda-list engine | **ordinary lambda list done (2026-08-22)** — LAMBDA/DEFUN/FLET/LABELS share `make_ordinary_function`, which binds through `BindingFrame` and signals the CLHS 3.5.1 arity errors; DEFMETHOD shares its tail binder. Outstanding: the **macro** lambda list (`_create_macro_function`) and `bind_destructuring_pattern` are still two more binders, neither signalling a PROGRAM-ERROR | C17, X2 |
| **M4** | A real macro system | not started — ~90 standard macros are special forms. **Most ecosystem-critical** | — |
| **M5** | `GET-SETF-EXPANSION` / places | not started — deletes ~600 lines of ladder code | C16 |
| **M6** | Multiple values, sequences | partial | C3, C5, X2, X3 |
| **M7** | Non-local control flow | partial — name-based block/tag matching, no identity objects | — |
| **M8** | Conditions and restarts | **signaling core done**; restart half + `DEFINE-CONDITION` + raise-site migration remain | C9, X1 |
| **M9** | Types, `SUBTYPEP`, CLOS, structures | not started overall — two CLOS implementations; `SUBTYPEP` is a string-pair table. **`DEFSTRUCT` done 2026-08-25**: BOA/keyword constructors (item 3, above) and `:TYPE list`/`:TYPE vector` (`structures`: 52 failing → 0, `state.typed_struct_layouts` is the flat-layout model a `:TYPE` DEFSTRUCT and its `:INCLUDE` children share, since there is no class/instance to hang a slot descriptor on) | C4, C6, C8, C13, C14 |
| **M10** | Reader, printer, `FORMAT`, streams, pathnames, loader | not started — **now the largest single body of failures, and gates ASDF** | C2, C7, C11, C12 |
| **M11** | Exact `COMPLEX` | not started — **fclpy's COMPLEX is Python's native `complex` (a float pair)**, so an integer/rational real or imaginary part loses exactness (`numbers/oneminus.lsp`'s `1-.9`: `(1- (complex most-positive-fixnum 3))` should stay exact and instead rounds through a double). RATIO closed the equivalent gap by using `Fraction` instead of float division; COMPLEX has had no equivalent pass. Touches every `isinstance(x, complex)` site in `math_arithmetic.py` (COMPLEXP, REALPART/IMAGPART, `/`, ...) plus the printer and reader, so it is its own milestone, not a one-file patch | — |

### Recommended order

> **Superseded 2026-08-22.** Eleven of the thirteen items below are done, and
> the two that are not (C2's remaining FORMAT directives; M5's place protocol)
> are now ordinary checklist entries rather than a plan. There is no
> "recommended order" any more — that was the point of the mode change in
> [§2](#working-mode-tail-mode-2026-08-22). What is left worth naming:
>
> | | what | why it is still worth naming |
> |---|---|---|
> | **C2** | `~<~>` justify, `~~`, `~%`, `~(~)`, `~P`, printer radix | the last group of files that genuinely share one engine — 30+ tests across six 100%-failing files |
> | **M5 / C16** | `GET-SETF-EXPANSION` and the place protocol | five parallel place protocols; still the answer to a scattered handful across `cons`, `data-and-control-flow` and `iteration` |
> | **M3 / C17** | the *remaining* lambda-list binders | the ordinary one is done (2026-08-22); the **macro** lambda list (`_create_macro_function`) and `bind_destructuring_pattern` are still separate and still signal no PROGRAM-ERROR — `macrolet.lsp` is 8 failures of 53 |
> | **X1** | Python exceptions leaking as Lisp values | not a cluster and never was — it is a *standing rule*, checked per fix |
>
> Everything else: open the checklist, take the cheapest file, read its
> failures. The list below is kept as the record of how the project got here.

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
   **BOA constructors done 2026-08-25.** The gap the docstring itself named —
   `:constructor name (boa-lambda-list)` recovered only the name, so a BOA
   constructor still behaved as a keyword constructor — was the whole
   reason `structures` was the worst *rate* in the suite at 32.2%
   ([§1](#1-status)). `make_boa_constructor` now binds the lambda list
   through the same `parse_lambda_list`/`_bind_ordinary_lambda_list_tail`/
   `BindingFrame` machinery LAMBDA/DEFUN/FLET/LABELS share, with one new hook
   (`default_fallback`) for CLHS 3.4.6's own rule: an `&optional`/`&key`
   parameter with no default-value form of its own defaults to the matching
   slot's initform, not NIL. The default keyword constructor is now the same
   mechanism called with a synthesized `(&key slot...)` lambda list, which
   deleted the second, hand-rolled keyword-matching loop that never checked
   arity, rejected an odd keyword count, or told a repeated keyword's
   leftmost occurrence from its rightmost.
   Four smaller defects surfaced by the same test files, each with its own
   fix rather than a structures-only patch: `STRING` on a keyword answered
   its *printed* form (`":A"`) instead of its name, via `str(x)`;
   `DEFSTRUCT`'s own `_sym_name` had the same bug for a `Character` option
   value (`(:conc-name #\X)`) and had no case at all for a bare `:conc-name`
   atom (CLHS: no value supplied suppresses the prefix, same as
   `(:conc-name nil)`, which the list form `(:conc-name)` already got right);
   `Package.intern` special-cased the string `"T"` to always return
   `COMMON-LISP:T` regardless of which package was being interned into, so
   `(intern "T" "KEYWORD")` answered plain `T` (`KEYWORDP` NIL) instead of
   `:T` — wrong for any package that does not `:use COMMON-LISP`, KEYWORD
   included; and the new constructor evaluated a slot's default-value form
   in an environment rooted at the struct's *global* environment rather than
   the lexical environment DEFSTRUCT was itself written in, so a slot
   default closing over an enclosing `LET`/`FLET` (structures-02's
   `STRUCT-TEST-62`, a slot defaulting to `#'%f` for a local `%f`) could not
   see it. `structures` **1312 → 1340 of 1394** on `run_ansi.py structures`
   (targeted; not yet in a full run), checklist-tracked rate 33.0% → 88.7%,
   0 regressions (`pytest`, `duplicates.py --baseline`,
   `ansi_checklist.py --baseline` all clean net of three pre-existing,
   unrelated flaky files — `numbers/boole.lsp`, `cons/nintersection.lsp`,
   `streams/write-sequence.lsp` — confirmed via `git stash` to already
   regress against baseline before this session's changes, driven by
   `(random ...)` in their own test generation).
   **What is left in `structures` is a single, separate feature, not more of
   this mechanism: CLHS 3.4.6's `:TYPE list`/`:TYPE vector`/`:NAMED` option**,
   which builds a structure as a plain list or vector instead of a
   `classes.LispInstance` — already named in `evaluation_special_forms.py`'s
   own comment ("not modeled — left unhandled, matching prior behavior") and
   now the entirety of `structures/structures-02.lsp`'s remaining 13
   failures (`STRUCT-TEST-37` through `STRUCT-TEST-70`, `DEFSTRUCT.ERROR.3/4`).
   It needs a second struct representation and constructors/accessors that
   branch on it, which `eval_defstruct` currently has no hook for at all —
   worth its own item rather than folding into a future BOA-shaped fix.
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
   blocking is gone. Details in the [Changelog](docs/changelog.md).
8. ~~**M2's remaining slice — the global value cell.**~~ **Done 2026-08-15.**
   A global variable has one home, the symbol's value cell, because the global
   environment no longer has the lexical bindings Common Lisp does not give it
   (CLHS 3.1.1.1). **The predicted fix was wrong in an instructive way:** this
   item said it "has to move `SETQ` and the lookup order with it", and it moved
   neither. Delete the home that should not exist and `SETQ` is already right
   (its walk ends at the value cell) and "lexical chain, then value cell" is
   already right (the value cell *is* the end of the chain). +23 with 0
   regressions, 20 of them untargeted. Details in the [Changelog](docs/changelog.md).
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
   [C2](#c2-format--formatter--still-the-largest-single-family-417)'s remaining
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
15. ~~**[C10](#c10-package-model--largely-done-2026-08-16-targeted-not-yet-in-a-full-run), the package model.**~~ **Largely done
    (2026-08-16 targeted; not yet folded into a full run).** `packages/`
    **201 → 373 of 500 (74.6%)**, 0 regressions; `make-package.lsp` and
    `defpackage.lsp` — the two 100%-failing files this item was ranked for —
    are now 72.5% and 89.3%. Still open: `RENAME-PACKAGE` (a stub),
    `INTERN`'s case-folding, `DELETE-PACKAGE`/`DO-SYMBOLS`/`WITH-PACKAGE-ITERATOR`
    edge cases, and the two M8-owned continuable-`PACKAGE-ERROR` test pairs.
    It is also M1, i.e. a prerequisite for the ASDF rung in
    [§7](#7-acceptance--the-ecosystem-ladder).
16. ~~**[C2](#c2-format--formatter--still-the-largest-single-family-417)'s remaining
    directives**, plus the adjacent `PRINT.INTEGERS.BASE`/`RADIX.BASE` pair at
    161 — the printer's radix/base handling.~~ **The second half was
    misdiagnosed and is now done (2026-08-16 b).** The 161 were not radix/base
    handling — that was already correct, measured before any change — they were
    `(copy-readtable nil)` raising underneath `my-with-standard-io-syntax`, so
    `printer/print-integers.lsp` went **0 → 189 of 194** by fixing the
    *readtable object model*. **`FORMAT`'s remaining directives (`~E`, `~F`,
    `~R`, `~T`, logical blocks) are still open and still the largest single
    family at 417.** The lesson is [§3](#3-the-checklist)'s own: a 100%-failing
    file names an absent mechanism, and the mechanism is not always in the
    subsystem the test names belong to.
16a. ~~**`WITH-STANDARD-IO-SYNTAX` (new, and the cheapest thing in the suite).**~~
    **Done 2026-08-16 (c).** It established *no* bindings, because it was a
    `cl_function`. It is now a `cl_macro` expanding to the `LET` of CLHS
    23.4's twenty-one bindings, so `BindingFrame` does the binding and there
    is no second mechanism. Measured, same runner both sides over eight files
    that use it: **122 → 142 passing of 182**, failures 60 → 40, **0 newly
    failing**; `reader/with-standard-io-syntax.lsp` **19 → 1 failing of 23**.
    Details in the [Changelog](docs/changelog.md). **The predicted blast radius did
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
    the [Changelog](docs/changelog.md).
    **The disappeared-failures signal came back negative, and that is
    honest rather than disappointing.** [§2](#the-development-loop) step 7
    asks which failures moved that were *not* targeted; here, none did — all
    115 are in the eleven files that test method combination. That is what
    an *absent operator* looks like as opposed to a *broken shared
    mechanism*: nothing else could have been depending on a combination
    type that did not exist. The eleven files were still the right unit of
    work — one mechanism, not 115 bugs — but this is [§3](#3-the-checklist)'s
    "the distribution has flattened" playing out, and it is evidence for
    the [policy-change trigger](#working-mode-tail-mode-2026-08-22)
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

> **This table is the known-non-compliance audit list, and
> [the final compliance gate](#half-two--the-known-non-compliance-audit)
> requires it to be empty or fully resolved.** That is what makes it binding
> rather than decorative: zero ansi-test failures does not clear a row here,
> because ansi-test does not exercise everything. Several rows are known
> *wrong answers* that no current test catches — `is_truthy(False)` is true,
> `EQUAL` descends a general vector where CLHS 5.3 says it must not, a
> length-1 `str` satisfies both `CHARACTER` and `STRING` (disjoint, CLHS
> 4.2.2), 114 non-ANSI symbols are exported from `CL`.
>
> A row leaves this table exactly four ways: **(a)** fixed, **(b)** shown to be
> conforming already with the CLHS section cited, **(c)** documented as an
> allowed implementation-defined choice with the CLHS section that grants the
> latitude, or **(d)** shown no longer to exist. Record which in
> `docs/changelog.md`. **Deleting a row is not a fifth way**, and "no test
> covers it" is not a resolution.
>
> Adding a row is cheap and expected — standing rule 5 exists so a shortcut is
> *tracked* rather than forgotten. The cost is only that it must be discharged
> before compliance is claimed.

| deviation | why tolerated | removed by |
|---|---|---|
| LOOP: one accumulation destination per *type*; `INTO` of mixed types into one var unsupported | accumulator state is typed on first use | C1 follow-up |
| LOOP `AND`-joined *FOR* clauses (parallel stepping) unimplemented; the token is dropped. `AND` in a `WITH` clause **is** implemented | parallel drivers mean stepping every driver from the values of the previous iteration — a change to the engine's step phase, not another clause | C1 follow-up |
| LOOP `IT` (CLHS 6.1.2.1.4) and `ELSE`/`END` unimplemented | all three need the conditional clause to own its own body, which is the clause-order item below rather than a separate feature | C1 follow-up |
| LOOP body/accumulation clauses execute in bucket order, not clause order | only WHILE/UNTIL are position-aware so far | C1 follow-up |
| LOOP silently drops an unrecognized keyword once a driver exists | violates standing rule 4 | C1 follow-up |
| **A builtin whose ANSI `&key` parameters are still spelled as defaulted *positional* parameters cannot be validated**, so `split_keyword_args` falls back to `_split_inferred_keywords` for it: an unrecognized keyword becomes a positional argument instead of a PROGRAM-ERROR | `inspect.signature` cannot tell `&key` from `&optional` unless `&key` is written keyword-only, and until a family is migrated the standard's checks are undecidable for it. The sequence/cons/set families are done; `FIND`/`POSITION`/`COUNT`/`REMOVE`/`SUBSTITUTE`/`MISMATCH`/`SEARCH`/`REPLACE`/`MAKE-SEQUENCE`/`MERGE`/`REDUCE` (all `**kwargs`) and `characters.py`'s string comparisons are not. **When you touch a builtin, spell its `&key` parameters keyword-only** | M3 / M6 |
| **The `PPRINT-*` operators ignore `prefix`/`suffix`/`per-line-prefix`/`colon-p`** and print their argument on one line through the ordinary printer | there is no pretty printer to put a logical block in; implementing the delimiters here would be a second printer. They were stubs calling Python's `print()`, i.e. the wrong stream *and* the unguarded `lispCons.__str__`, which is strictly worse than a documented deviation | C2 / M10 |
| **A circular structure prints as `...` rather than with `#1=`/`#1#` labels, and `PRINT_BUDGET` caps the aggregates one print may enter** | without `*PRINT-CIRCLE*` there is no label to emit, and cycle detection alone does not bound the work — a twenty-node cons graph has exponentially many simple paths. The budget is what guarantees the printer can never abort a run | C2 / M10 |
| `CLASS-SLOTS`, `CLASS-DIRECT-SLOTS` and `COMPUTE-RESTARTS` return a Python `list`, i.e. a **vector**, where CLHS says list | plan.md Finding M, found 2026-08-18 by auditing every list-returning operator after `DIRECTORY` had the same defect and broke the harness bootstrap. These three are not reached by it | M8 / M9 |
| `_run_handlers_on_unwind` + `_condition_matches` legacy branch | most raise sites bypass `SIGNAL` | M8 |
| `DEFINE-CONDITION` creates no class | predates the class lattice | M8 |
| `HANDLER-CASE` converts an uncaught `THROW` into `CONTROL-ERROR` | needs a catch-tag stack to decide at THROW time | M7 |
| 114 non-ANSI symbols exported from `CL` | registry auto-export | M1 |
| ~90 standard macros implemented as special forms | predates the macro system | M4 |
| Five parallel place protocols; `GET-SETF-EXPANSION` a stub | predates the setf protocol | M5 |
| **Two** lambda-list binders left, for the *macro* lambda list — `_create_macro_function` and `bind_destructuring_pattern`. Both ignore `&aux` and `&allow-other-keys` and signal no PROGRAM-ERROR for a malformed call | was six; the ordinary lambda list is one constructor as of 2026-08-22. These two implement a *different* lambda list (CLHS 3.4.4, nested destructuring patterns) and already share `bind_destructuring_pattern`, so folding them together is its own change | M3 |
| Two CLOS implementations, two readers, two readtables, dead `reader.py`/`tokenizer.py` fork | historical forks | M9 / M10 |
| Pretty printer absent: `*PRINT-PRETTY*`, `PPRINT-*`, `~<~:>` logical blocks | the printer prints only the non-pretty style | C2 / M10 |
| `*PRINT-CIRCLE*` unimplemented; the printer instead cuts off at depth 256 | needs a labelling pass over the object graph | M10 |
| `~&` sees only the column within its own control string, so a `~&` opening a control string cannot tell the stream is mid-line; `FRESH-LINE` is correct | FORMAT builds its whole output as a string before writing, and the column is not threaded through the eleven nested `_format_process_cursor` call sites | C2 |
| **`~T`/`~<...~:;...~>`'s "current column" is likewise control-string-local, not stream-wide** (2026-08-27, `_current_column`) — `~T` now tabs correctly *within* one FORMAT call (`format-justify.lsp` 0/59 → 59/59), but a preceding `WRITE-STRING` to the same stream is invisible to it, the same gap the row above already names for `~&` | same root cause as the row above; one fix (threading the stream's real column through FORMAT) closes both | C2 |
| `SUBTYPEP` string-pair table | no type lattice | M9 |
| **No class precedence list.** `classes._specificity_key` orders applicable methods by *ancestor count*, and the live `_init_builtin_classes` gives every built-in class `T` as its only superclass — so `INTEGER`, `RATIONAL` and `NUMBER` are equally specific and ties are broken by *definition order* (a stable sort). Much of `objects/` therefore passes because ansi-test happens to define its methods most-specific-first | CLHS 7.6.6.1 wants the argument's class precedence list position, which needs a real C3 linearization *and* a real built-in class hierarchy; both are the same mechanism as C14's type lattice, so doing it here would be a second one | M9 / §4 item 17a |
| `classes.py` defines `_init_builtin_classes` twice; the second wins and the first is dead | standing rule 3, unresolved — the two disagree about the class hierarchy, which is exactly why it matters | M9 / §4 item 17a |
| ~~The reader does not parse **ratios**: `3/5` reads as a symbol, so it evaluates as an unbound variable~~ | **Fixed 2026-08-27.** `fclpy/numtoken.py` is now the one place CLHS 2.3.1's numeric-token grammar is decided, shared by the reader's step 10 and the `#B`/`#O`/`#X`/`#nR` dispatch readers; `(read-from-string "3/5")` reads the ratio | M10 / C12 |
| `WITH-STANDARD-IO-SYNTAX` binds `*PRINT-PPRINT-DISPATCH*` to a dispatch table that dispatches nothing | the *object* now has one home (`io_write.standard_pprint_dispatch`) and the binding is correct, but `SET-PPRINT-DISPATCH`/`PPRINT-DISPATCH` are stubs, so `WITH-STANDARD-IO-SYNTAX.23` cannot pass. It is the pretty printer's absence, not the macro's | C2 / M10 |
| `SET-SYNTAX-FROM-CHAR` acts on a real character *syntax type* model as of 2026-08-26 (was a stub returning T); `reader/set-syntax-from-char.lsp` is 67/75, not 0/75 | 5 of the 75 remain unprobed | M10 / C12 |
| ~~The reader upcases every symbol token regardless of `readtable-case`, though the readtable records it and the printer honours it (CLHS 23.1.2)~~ | **Stale — already fixed before this row was last checked.** `lispreader._convert_case` applies `readtable-case` per character, escaped characters exempt; verified 2026-08-27: `:downcase`/`:preserve` both read correctly. Left the table without a dated fix entry, which is itself worth noting — a row here needs re-verifying against current behavior before being trusted, not just against the diagnosis that first added it | M10 / C12 |
| `MAKE-LIST`/`MAKE-SEQUENCE` refuse a size above `CONSTRUCTIBLE_LIMIT` (2**30) with a plain error rather than a `STORAGE-CONDITION` | CLHS 4.4 permits refusing, but the condition *type* should be `STORAGE-CONDITION` once the class lattice exists | M8 / M9 |
| `EQUAL` descends a *general* vector element-wise | CLHS 5.3 descends only conses, strings, bit vectors and pathnames, so `(equal #(1 2) #(1 2))` must be NIL. Conses, strings and bit vectors are now right; the general-vector branch predates them and turning it off changes the answer for a heavily-used predicate, which should be its own measured change | M6 |
| A *displaced* character vector is a `LispArray`, not a `LispString`, so the STRING-specific operators do not accept it | `LispString` stores its characters directly, and threading displacement through it means a second indirection in every string access; every other character array (fill-pointered and adjustable included) is a `LispString` | M9 |
| `LispString` vs. Python `str` split | two string representations | M9 (blocks EQUAL/EQUALP) |
| Name-based block/tag/catch matching | no block identity objects | M7 |
| `is_truthy(False)` is `True` | unaudited boundary | M2 |
| A variable bound *dynamically* by a form is invisible to that form's body if an **enclosing lexical** binding of the same name exists — `eval` checks the lexical chain before the value cell | **Was still live when this row claimed otherwise.** The row said a local `(declare (special x))` "redirects through `%SPECIAL-REF`", but only a *free* declaration did; a declaration on a variable the form itself binds installed no redirection, so `(let ((y :a)) (let ((y :b)) (declare (special y)) ...))` read `:a`. Fixed 2026-08-22 in `BindingFrame.bind`. What remains is a lexical binding shadowing a `PROGV` of the same undeclared name, which no ANSI test in the measured groups needs | M2 |
| ~~A function's lambda list binds a *proclaimed special* parameter lexically~~ | **Fixed 2026-08-22.** Parameters bind through `BindingFrame`, so `(defvar *x* 1) (defun f (*x*) ...)` binds `*x*` dynamically. The row's own prediction held: doing it per-binder would have been a seventh mechanism, and consolidating the binders supplied it for free | M3 |

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

### The final compliance gate

**Zero ansi-test failures is necessary and not sufficient.** ansi-test does not
exercise everything, so a defect it happens to miss is still a defect. Both
halves have to be closed before fclpy is called ANSI compliant.

#### Half one — the suite

1. **`COMPLETENESS: OK`.** Not the "N failures out of 22036" summary, which
   prints the initial pending count unconditionally and looks complete even
   when a run died partway.
2. **Zero failures**, from a **full** `run_all_tests.py`, read out of raw RT
   output by `scripts/ansi_score.py` — not from a `FORMAT`-rendered summary
   produced by the implementation under test, and not from an amended
   checklist count (see [the merge rules](#keeping-the-checklist-current-without-a-full-run):
   a merged total is an index, not a scoreboard).
3. **`expected-failures/` is unwired**, `rt:load-expected-failures` is called
   from nowhere, and `docs/expected-failures.sexp` does not exist.
4. **`*FEATURES*` is unchanged** from `(:FCLPY :COMMON-LISP :ANSI-CL)` unless a
   keyword was added because fclpy genuinely has that feature, argued in
   writing. ansi-test branches on `#+`/`#-`, so `*FEATURES*` sets the
   denominator; adding a keyword to shrink it is an expected-failure in a hat.
5. **No per-file regression** — `scripts/ansi_checklist.py --baseline` marks no
   file `(+N REGRESSION)`, and the baseline it compares against was saved from
   a full run, not refreshed to clear one.

#### Half two — the known non-compliance audit

**Every known deviation must be resolved, whether or not ansi-test catches
it.** This is the half that does not happen automatically, and the half a
green scoreboard will otherwise be mistaken for. Each item below is *resolved*
only when it is one of:

  **a.** fixed;
  **b.** demonstrated to be conforming already, with the CLHS section cited;
  **c.** documented as an allowed **implementation-defined** choice, with the
  CLHS section that grants the latitude and a statement of what fclpy chose;
  or **d.** shown to no longer exist.

"ansi-test does not test it" is **not** a resolution. Neither is deleting the
row.

6. **[§5, Known temporary deviations](#5-known-temporary-deviations), is empty
   or fully resolved.** Its own header already says empty means "nothing is
   knowingly wrong"; this is the gate that makes that binding. It currently
   holds ~34 rows, several of which are known *wrong answers* rather than
   latitude — `is_truthy(False)` being true, `EQUAL` descending a general
   vector (CLHS 5.3 says it must not), a length-1 `str` satisfying both
   `CHARACTER` and `STRING` (disjoint types, CLHS 4.2.2), 114 non-ANSI symbols
   exported from `CL`. None of those becomes acceptable by being untested.
7. **The known non-ANSI assertions in the *unit* suite are gone** — see
   [§3](#known-non-ansi-assertions-in-the-unit-suite). Each of the three is
   either corrected, or explicitly renamed and documented as a test of
   *non-ANSI* behaviour so it cannot be mistaken for a conformance assertion.
   A unit test that asserts a bug makes fixing the bug look like a regression,
   which is the mechanism by which a known defect survives to the end.
   The tests that *cannot fail* (`test_phase3_unwind_protect.py:131`,
   `test_phase4_multiple_values.py:330`) are fixed or removed: a test that
   cannot fail is worse than no test, because it occupies the slot.
8. **The duplicate register is empty.** `scripts/duplicates.py` reports no
   operator registered from two modules and no module-level name defined twice
   in one file. Standing rule 3 admits no exception, and
   `docs/duplicates_baseline.json` is debt to be worked down — **never an
   approval list**. Two implementations of one operator means the answer
   depends on import order, which is not a property a conforming
   implementation can have.
9. **No Python object appears as a Lisp value** anywhere reachable — standing
   rule 2. Grep the full-run log for the leak shapes in
   [X1](#x1-python-exceptions-leaking-as-lisp-values); zero occurrences.
10. **No silent-acceptance path remains** — standing rule 4. The known one is
    LOOP dropping an unrecognized clause keyword ([§5](#5-known-temporary-deviations)).

#### Half three — the record

11. **All four ecosystem rungs load and run**
    ([the ladder above](#7-acceptance--the-ecosystem-ladder)). Nothing in
    ansi-test tests ASDF, and "runs unmodified ANSI source" is the actual goal.
12. **A conformance statement** listing every implementation-defined choice
    with its CLHS citation. It is a *record of what was chosen*, never a list
    of what was skipped, and every item admitted under (c) above appears in it.

#### Why `expected-failures/` stays unwired — deliberately

RT supports it: `rt.lsp` defines `*expected-failures*`, and
`rt:load-expected-failures` reads a `.sexp` list of test names that then stop
counting as failures. Five implementations ship one in
`../ansi-test/expected-failures/`. **fclpy will not.**

That mechanism exists so an implementation can decline a feature whose cost it
does not want to pay — usually for speed — and still report a clean run. It is
the wrong trade for this project, whose entire point is a *reference*
implementation of the standard: correctness first, and no line in the tooling
that lets a real gap read as an accepted one. **A faster Lisp can be forked
from this one and make those trades explicitly.** It cannot be recovered the
other way round, because once a test is on the list nothing ever measures it
again.

This is not a new policy so much as the one already being followed. When
`SUBTYPEP` could not answer the twelve certainty questions `check-equivalence`
asks, the response was to build a real type lattice with a complement-closed
representation per sort — not to declare `subtypep.member.27` expected to
fail, which every clause of CLHS 4.4's "may return NIL NIL" would have
licensed. `MOST-POSITIVE-FIXNUM`, `(subtypep '(and A (not B)) nil)` and the
randomised `subtypep.cons.44` pairs are all cases where the standard permits
latitude and ansi-test asserts a specific answer, and each was met by
supplying that answer.

**The operative rule, then:** *where CLHS permits several conforming
behaviours and ansi-test asserts one of them, that one is fclpy's
implementation-defined choice.* Record it in the conformance statement
(criterion 12) and implement it. A test that looks unpassable is a claim that
needs a CLHS citation and a written argument in
[§5](#5-known-temporary-deviations) — not an entry on a skip list.

Note the direction of that rule. It resolves latitude *toward* the test; it
never resolves a failing test *into* latitude. Where ansi-test does not assert
a choice, the choice is still fclpy's to make — but **implementation-defined
is not implementation-arbitrary**: the standard has to actually grant the
latitude, and the grant gets cited.

### Ways to fake compliance

Each of these produces a green result without changing what fclpy does. They
are listed because several are one command away, and two of them are switches
this project added itself.

| evasion | why it is available | the rule |
|---|---|---|
| `rt:load-expected-failures` | RT supports it; five implementations ship a file | **Never.** See [Why `expected-failures/` stays unwired](#why-expected-failures-stays-unwired--deliberately) |
| push a keyword onto `*FEATURES*` | ansi-test branches on `#+`/`#-`, so this deletes tests | Only if fclpy genuinely has the feature, argued in writing |
| `scripts/duplicates.py --save-baseline` after adding a duplicate | the switch exists so real debt can be recorded | A baseline change is a **reviewable event, not a fix**. Both baselines are committed for this reason: it shows up in `git diff` |
| `scripts/ansi_checklist.py --save-baseline` after a regression | same | Full-run-only, and only once the regression is understood and *accepted in writing* or fixed |
| delete or soften a [§5](#5-known-temporary-deviations) row | it is a hand-maintained table | A row leaves only by (a)–(d) above, and the Changelog records which |
| call a defect "implementation-defined" | ANSI does grant latitude in many places | Requires the CLHS section that grants it. **Implementation-*defined* is not implementation-*arbitrary*** — the standard has to actually permit the choice |
| weaken a unit test to match fclpy | pytest is ours to edit | ansi-test is the authority; when they disagree the unit test is wrong ([§3](#known-non-ansi-assertions-in-the-unit-suite)) |
| catch an exception in the runner so a test "passes" | the runner is ours | Standing rules 2 and 4. A Python exception surfacing as a Lisp value is a bug, not an error to hide |
| quote an amended checklist count as the scoreboard | the checklist is the day-to-day authority | A merged total is an **index**; the official number moves only on a full run |

**The general form:** if a change makes the number better without making the
Lisp better, it is one of these. The test is whether the same change would
have been worth making if nobody were counting.


### Preventing regression

> **Policy, 2026-08-25: a discovered regression gets fixed, not filed.**
> Confirming a regression predates your own diff (`git stash` A/B, `git show
> HEAD:docs/ansi_checklist.md`) is a **diagnostic step**, not a conclusion —
> it tells you the regression isn't attributable to what you just did, not
> that fixing it is someone else's job. The 2026-08-24 entry below is the
> anti-pattern this replaces: three files were confirmed pre-existing,
> "recorded here rather than cleared," and then sat unfixed through at least
> two more sessions' worth of commits, because "confirmed not mine" was
> quietly read as "done." A regression is a real defect sitting in the tree
> either way, and it is usually cheap: `numbers/boole.lsp` and
> `streams/write-sequence.lsp` (both flagged 2026-08-24, both still broken
> 2026-08-25) turned out to be a one-line constant-vs-function registration
> bug and a missing binary-stream branch respectively — each a smaller fix
> than the DEFSTRUCT `:TYPE` work that was the actual assignment that day.
> **The default action on finding a regression is: reproduce the smallest
> failing case, find the mechanism, fix it, verify, fold into the same
> commit** — exactly the loop CLAUDE.md already describes for anything on
> the checklist, because a regression *is* a checklist entry, just one this
> tree used to pass. Only when the real fix is its own separate mechanism —
> not a quick patch, and touching a wide, unrelated blast radius — does it
> get deferred, and then it must be named as a **specific, scoped
> milestone** (what the defect is, why it's out of scope for the current
> diff, what the real fix requires) rather than left as a vague "pre-existing,
> not mine" note. `numbers/oneminus.lsp`'s `1-.9` (below) is the worked
> example of that second case: it is not a quick patch, and saying so
> precisely is the honest version of deferring it.
>
> **Why regressions are worth chasing rather than routing around: they are
> usually a real defect that a *different* piece of non-compliant code had
> been silently hiding.** `write-sequence.lsp` didn't newly break WRITE-BYTE
> or binary streams — WRITE-SEQUENCE simply never had a binary-stream branch,
> and nothing forced the question until something else (unrelated,
> same-day work) started exercising that path. The "regression" is the
> first honest signal that a real gap exists; attributing it away and
> moving on throws that signal out.

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

  > **There is no CI** — no `.github/workflows`, no config for any other
  > runner — so every gate in this section is something a person or an agent
  > has to remember, and the 2026-08-22 cross-session merge above shows what
  > that is worth. **`scripts/gate.py` is the cheap standing-in-for-CI
  > version** (added 2026-08-22): it runs `pytest -q`,
  > `scripts/duplicates.py --baseline` and
  > `scripts/ansi_checklist.py --baseline` and exits non-zero if any fails.
  > Run it after every repair; it is ~50s, almost all of it pytest
  > (`--skip-pytest` drops it to under a second).
  >
  > **Its third check reads the generated checklist, not the command's
  > output**, and that detail is the point. `ansi_checklist.py --baseline`
  > writes `(+N REGRESSION)` markers *into `docs/ansi_checklist.md`* and exits
  > 0 either way — so the obvious implementation of this gate passed
  > unconditionally, and did on its first run. It found six regressed files
  > the moment it was corrected. A gate that cannot fail is worse than no
  > gate, because it is mistaken for one.

- **Nothing measures speed, and speed is a first-order constraint here.** The
  entire working mode is shaped by run cost — 86 minutes for the authority,
  2–30s for the loop — and this plan records two occasions where a runtime
  blow-up, not a wrong answer, was what killed a run (a loop wedged inside one
  iteration at 27GB; the printer at 21GB on a circular structure). Yet wall
  time appears only as narrative in [§1](#1-status), per full run, where a 20%
  regression is indistinguishable from a change in how much work the suite
  actually does. The 2026-08-22 lambda-list work cost about **5%** on a pure
  function-call benchmark — measured only because it was deliberately checked
  against a `git stash`. **A `scripts/bench.py` with four fixed
  microbenchmarks (function call, LOOP iteration, printer, reader) and a
  committed baseline** would make that a number in the loop instead of an
  archaeology exercise, and it is the same shape as the duplicate register:
  cheap, mechanical, and currently done by luck.

- **Save a dated snapshot of `ansi_results/` with every full run.** §7's own
  process lesson says this and it is still not a convention — `ansi_results/`
  holds ad-hoc `base-*.json` / `tmp-*.json` / `probe1.json` files instead.
  Without it, attributing a regression to a commit costs a second full run
  (`git stash`, re-run, diff by test *name*), because the baseline stores
  counts and not names. With it, attribution is a diff.

#### Gate status at 2026-08-24 (hash-table work) — resolved 2026-08-25

> **This section is history, not a live status.** It sat for a day-plus as
> "recorded here rather than cleared" — the anti-pattern the new policy note
> above now forbids. Two of the four are fixed; the third turned out to be a
> real, separately-scoped architectural gap and is named as such rather than
> deferred vaguely.

`scripts/gate.py` failed on **4 files** as of 2026-08-24, and none of them was
attributable to the hash-table change. What each one actually was:

- **`numbers/boole.lsp` — fixed 2026-08-25.** BOOLE-1/-2/-AND/.../-ORC2 (the
  sixteen CLHS 12.1.4 op codes) are constant *variables*, but `core.py` had a
  same-named zero-argument *function* for each, auto-registered by
  `registry.register_module`. Since none of the sixteen was ever bound as a
  variable, referencing the bare symbol (`numbers/boole.lsp` builds
  `*boole-vals*` by evaluating each name, `(list boole-1 boole-2 boole-and
  ...)`) fell through `evaluation_core.eval`'s unbound-variable-but-fbound
  fallback and returned the raw Python function object as the value — and
  `BOOLE-1`/`BOOLE-AND` both happened to return the Python int `1`, so even a
  caller that funcalled them would have collapsed two distinct operations
  onto one code. Fixed by adding the sixteen to `lispenv.STANDARD_CONSTANTS`
  (the one real-constant table, alongside PI/MOST-POSITIVE-FIXNUM/...),
  deleting the sixteen wrong functions from `core.py`, and rewriting `BOOLE`
  itself (`math_arithmetic.py`) — it previously hardcoded three op values
  (1, 2, 6, none of which are the real codes) and returned 0 for the other
  thirteen. `numbers/boole.lsp`: 10 failing → 0.
- **`streams/write-sequence.lsp` — fixed 2026-08-25.** WRITE-SEQUENCE
  (`streams.py`) unconditionally rendered every element through
  `_char_text` and wrote it as text, so `(write-sequence #*00111010 os)` on a
  binary (`(unsigned-byte 8)`) output stream raised "cannot store 0 in a
  string" — a Python exception as a Lisp value, standing rule 2 — instead of
  writing a byte. Fixed by checking `stream.binary` (the same flag WRITE-BYTE
  already reads from OPEN's declared `:element-type`) and routing element-by-
  element through WRITE-BYTE's own encoding when true. Two more defects
  surfaced by the same file once the crash stopped hiding them: `:start`/
  `:end` were plain optional-positional Python parameters rather than
  keyword-only, so CLHS 3.4.1.4's unrecognized-keyword/`:allow-other-keys`
  checking never applied to them (`STRING.9`-`.11`, `ERROR.11`-`.12`); and
  the shared `sequence_protocol.bounding_indices` — every CLHS sequence
  function's one bounding-index accessor — coerced `:start`/`:end` with
  `int(x)`, which silently truncates a float instead of signalling TYPE-ERROR
  (`ERROR.7`, `.10`, `.15`, `.16`). Both are now real CLHS-shaped checks,
  which reaches every one of the ~20 other callers of `bounding_indices`, not
  just WRITE-SEQUENCE. `streams/write-sequence.lsp`: 16 failing → 0.
- **`numbers/oneminus.lsp` (`1-.9`) — named, not fixed: a real architectural
  gap, not a quick patch.** `(1- (complex most-positive-fixnum 3))` answers
  `9.223372036854776e+18+3j` instead of the exact
  `9223372036854774806+3j`, because **fclpy's COMPLEX is Python's native
  `complex`**, which is a pair of IEEE doubles — there is no way to hold an
  exact-integer real/imaginary pair in it at all, the same representational
  gap RATIO closed by using `Fraction` instead of float division. The test
  is `repeat 1000` over `random-fixnum`, so whether a given run's draw
  exceeds a double's 53-bit mantissa (and therefore whether the test passes
  or fails) is chance, not code — which is why this shows up as an
  intermittent "regression" rather than a clean break. **This is not a
  same-diff fix**: `complex` (Python's builtin) is tested with `isinstance`
  in at least 10 sites in `math_arithmetic.py` alone (COMPLEXP, REALPART/
  IMAGPART, the printer, `/`'s complex branch, ...), so an exact
  representation means a new Lisp-level complex type and updating every one
  of those sites together, in the same shape SUBTYPEP's type lattice or the
  pathname component-record rewrite were each their own milestone. Filed as
  a new milestone rather than attempted piecemeal here; see
  [§4](#4-milestones--re-scoped) for where a numeric-tower item belongs.
- **`types-and-classes/standard-generic-function.lsp` (baseline 1 → 2) is not
  new breakage** — see the direct-measurement analysis below, still current.

- **Three were already failing the gate at HEAD** — `numbers/boole.lsp` (+2),
  `numbers/oneminus.lsp` (+1), `streams/write-sequence.lsp` (+1). All three
  are in the 08-22 table below already, and HEAD's *committed* checklist
  carries the identical `(+N REGRESSION)` annotations, from the
  `2026-08-24T11:25:48` merge. `streams` was not even in the target list of
  the run that produced the current numbers.
- **`types-and-classes/standard-generic-function.lsp` (baseline 1 → 2) is new
  to the register and is not new breakage.** Attributed by direct measurement,
  which is worth recording because the *cheap* comparison said the opposite:
  - Run at HEAD in a sibling `git worktree` (so `../ansi-test` resolves), the
    file fails **0 of 2** — exactly as it does now.
  - Same runner, same directory: HEAD `passed=537 failed=82 registered=619`,
    current `passed=538 failed=81 registered=619`. One test *better*, with the
    registration count unchanged.
  - The tests want `(sgf-cpl-gf.1 #'make-instance)` to be 1, i.e. dispatch to
    the GENERIC-FUNCTION method. `#'make-instance` is a plain Python function
    in both trees and `(typep #'make-instance 'generic-function)` is NIL in
    both, so the answer is 2 in both. This is the absent class precedence list
    ([§5](#5-known-temporary-deviations)) plus `MAKE-INSTANCE` not being a
    generic function object — not the hash table.

**Two measurement traps this cost time to, both worth avoiding next time.**

1. **"Absent from the checklist" does not mean "zero failures".** It means no
   failures were *attributed*, which conflates *passed* with *never ran*. A
   per-file diff of two checklists that reads a missing key as 0 will
   manufacture regressions. The baseline JSON had this file at **1**; the
   merged checklist had it at 0; a targeted run says 2.
2. **The annotation for a worse file is `(+N REGRESSION)`, not
   `(+N since baseline)`** — that second wording is used for files that
   *improved*. Grepping the generated checklist for "since baseline" to look
   for regressions finds nothing and looks like a clean result. Use
   `scripts/gate.py`, which reads the markers properly, rather than grepping
   the artifact.

Also worth knowing: **a targeted run of `types-and-classes` registers 619
tests where the 08-22 full run recorded 545.** [§2](#keeping-the-checklist-current-without-a-full-run)
says a targeted run "can register a slightly different test set"; a 74-test
difference is not slight, and it means merged per-file counts for this
directory are not comparable with full-run ones.

#### Open regressions carried by the 2026-08-24 full run

**Three files, +5 tests, against the committed baseline — and none of them is
from this run's work.** Verified by running the three files on this tree and on
`ae7e4ca` in a `git worktree`: **99 passed / 26 failed on both**, identically.
They entered with `34fc95f`/`ae7e4ca`, and the checklist header's own merge log
shows a previous session touching exactly these three files.

| file | baseline | now | Δ |
|---|---|---|---|
| `numbers/boole.lsp` | 8 | 10 | +2 |
| `cons/nintersection.lsp` | 2 | 4 | +2 |
| `streams/write-sequence.lsp` | 6 | 7 | +1 |

**The baseline was not refreshed.** Under
[Ways to fake compliance](#ways-to-fake-compliance) it may be refreshed only
once a regression is understood *and* accepted in writing or fixed. These three
are now understood and attributed, but attributing a regression is not the same
as accepting it, and `--save-baseline` is the maintainer's call — it is
committed precisely so that moving it shows up in `git diff` as the reviewable
event it is. `scripts/gate.py` will keep reporting them until then. That is
intended.

**The six files carried by the 2026-08-22 run are no longer reported**, since
this run supersedes those counts; if any of them mattered, the diff to check is
against the same committed baseline, not against the 08-22 numbers.

<details>
<summary>Open regressions carried by the 2026-08-22 full run (superseded)</summary>

**Six files, +17 tests, against the 2026-08-18 baseline — which was
deliberately *not* refreshed.** Under
[Ways to fake compliance](#ways-to-fake-compliance) a baseline may be
refreshed only once a regression is understood and accepted in writing or
fixed, and none of these six is yet. Leaving it stale costs nothing but a
little noise; refreshing it would erase the only record that these files got
worse.

| file | Δ | now |
|---|---|---|
| `characters/character.lsp` | **+9** | 16 failing of 124 |
| `printer/pprint-dispatch.lsp` | +2 | 10 of 15 |
| `numbers/boole.lsp` | +2 | 10 of 15 |
| `cons/nintersection.lsp` | +2 | 4 of 48 |
| `numbers/oneminus.lsp` | +1 | 3 of 18 |
| `streams/write-sequence.lsp` | +1 | 7 of 16 |

**Not yet attributed, and attribution is harder than usual here.** Nine
commits sit between the two runs, so the `git stash` instrument the 08-18
entry used does not apply — that works for *uncommitted* work. This needs
either a bisect over the nine, or a targeted run of each file against each
commit. `characters/character.lsp` at +9 is the one worth doing first: it is
the largest single regression, it is a directory otherwise at 93.4%, and a
9-test jump in one file is far more likely to be one mechanism than nine
bugs.

**A note on the instrument itself.** These six were invisible until
`scripts/gate.py` was corrected on 2026-08-22: `ansi_checklist.py --baseline`
writes its `(+N REGRESSION)` markers *into the generated checklist* and exits
0 regardless, so the obvious gate — inspect the command's exit status — passed
unconditionally. Six regressed files had been sitting in a file nothing read.

</details>

#### Open regressions carried by the 2026-08-18 (b) full run

**Four files, +12 tests, and none of them from the change this run measured.**
`--save-baseline` *was* run this time, because the baseline had drifted three
full runs behind and a gate that reports 75 stale files is noise rather than a
gate. The four are recorded here so refreshing it did not erase them:

| file | Δ | note |
|---|---|---|
| `streams/peek-char.lsp` | +4 | |
| `numbers/parse-integer.lsp` | +4 | already the 5th-largest failing file at 54/56 |
| `reader/read-from-string.lsp` | +3 | |
| `streams/write-sequence.lsp` | +1 | |

**Attributed, not assumed.** All four were re-run with `run_ansi.py` on this
tree and on `HEAD` with the change stashed: **149 failures both sides, the same
test names.** So they belong to the three intervening commits — `598af8d` is
named "SUBTYPEP repairs with likely regressions" — and not to the list/keyword/
printer work. That `git stash` diff is the instrument to use here; the
per-file count in the baseline cannot attribute a regression to a commit,
because it stores counts and not names.

The three of them touching `PEEK-CHAR`, `READ-FROM-STRING` and `PARSE-INTEGER`
is the shape of one shared defect in the reader's stream/index handling rather
than three, and that is where to start.

#### Open regressions carried by the 2026-08-16 full run (superseded)

The 08-16 table below was written against the **08-12** baseline, which has now
been replaced. It is kept for the diagnoses it records, not as a live gate.

The 08-16 run was **+3224 overall but worse in 75 files** against the 08-12
baseline. Those 75 are no longer a live count -- the baseline was refreshed from
the 08-18 (b) run, which reports four -- but the diagnoses below are still the
record of what was investigated and why.

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
| **C** | Lambda lists are parsed ad-hoc and incompletely, in six places — **half-resolved 2026-08-22**: the *ordinary* lambda list has one constructor (`make_ordinary_function`) shared by LAMBDA/DEFUN/FLET/LABELS, with DEFMETHOD on the same tail binder. The **macro** lambda list (`_create_macro_function`) and `bind_destructuring_pattern` are still separate, still ignore `&aux`/`&allow-other-keys`, and still signal nothing | C17 |
| **D** | `(declare (special ...))` is not honored — **resolved for parameters 2026-08-22** (every binder went through `Environment.add_variable`, which cannot bind dynamically; they now go through `BindingFrame`). It also surfaced two general defects that were *not* about parameters: a dynamically bound variable was invisible under an enclosing lexical one of the same name, and `%SPECIAL-REF` had a reader and no writer | M2 |
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
| `CLAUDE.md` | architecture map **and the canonical development loop** — read first |
| `plan.md` | this document — status, working mode, live items, deviations, acceptance |
| `docs/changelog.md` | the mechanism-by-mechanism record, including the diagnoses that were wrong — archive, not required reading |
| `scripts/gate.py` | **the cheap gate** — `pytest -q` + `duplicates.py --baseline` + `ansi_checklist.py --baseline`, non-zero exit if any fails. Run after every repair; never a substitute for a full run |
| `scripts/duplicates.py` | **the duplicate register** — operators registered from two modules, and names defined twice in one file; `--baseline` is a one-second gate |
| `docs/duplicates_baseline.json` | the *known* duplicates. Debt, not an approval list; the gate is "no new ones" |
| `scripts/run_ansi.py` | **targeted runner — the development inner loop**; `--update-checklist` amends the checklist with the run |
| `scripts/ansi_score.py` | per-subsystem scoreboard → `docs/ansi_baseline.json` |
| `docs/ansi_checklist.md` | **the working checklist** — failures by directory → file, with per-entry verify commands |
| `scripts/ansi_checklist.py` | regenerates the checklist; `--merge` folds in a targeted run, `--baseline` marks fixed/regressed per file |
| `ansi_results/failed.txt` | raw RT output — the checklist's input, not a work list |
| `ansi_results/targeted-last.json` | the last targeted run's outcomes, written by every `run_ansi.py` run so it can be merged later |
| `ansi_results/merges.log` | which targeted runs the current checklist has been amended with; cleared by a full run |
| `run_all_tests.py` | full suite (~86 min) — moves the official scoreboard; **never the inner loop** |
| `REPAIR.md` | crash-repair SOP — historical; crashes are no longer the constraint |

---

## Changelog

**Moved to [docs/changelog.md](docs/changelog.md).** It had grown to 1385 lines,
45% of this document, and nothing in it is needed to decide what to do next.

Read it when you want the record of *why* a mechanism is the shape it is —
including the entries that record a diagnosis being wrong, which is the part
that stops the same wrong diagnosis being made twice.
