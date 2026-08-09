# fclpy Roadmap: Zero ANSI Failures → 100% ANSI Common Lisp Compliance

## The goal, and why it is the goal

fclpy is a Common Lisp implementation in Python. The objective is **100% ANSI Common
Lisp compliance** — not as a scoreboard, but because ANSI compliance is the entry
ticket to the **existing Common Lisp ecosystem**: Alexandria, Bordeaux-Threads,
CL-PPCRE, FiveAM, ASDF, and the several decades of ANSI source code that assumes a
conforming host. A Lisp that requires its own dialect of every library has no
ecosystem and must build everything from scratch.

**Therefore: implement the underlying ANSI mechanism, never the test.** A fix that
makes `PSETF.7` pass without implementing `GET-SETF-EXPANSION` is worse than no fix,
because it hides the architectural gap that will block real code later.

Specialized/AI-sandbox features come **after** ANSI compliance, not alongside it.

---

## Update (2026-08-09): M0's measurement-corruption bugs are fixed — the "Reality check" section below is now historical

The FORMAT argument-cursor rewrite, the WARN consolidation, and the Finding K
(`funcall`) fix described in the Reality Check and Finding H/J sections below have been
implemented. Verified with an isolated before/after (`git stash`) run of the identical
22036-test suite:

| | Before (this session's baseline) | After |
|---|---|---|
| `Doing N pending test(s) of N test(s) total` | `pending test of ... test total` (bad grammar — `~:P`/pluralization corrupted) | `pending tests of ... tests total` (correct) |
| Summary line | `629 failures with 629 unexpected failures and 629 unexpected successes out of 629 tests.` — the exact self-contradictory line this document warned about | `629 failures with 629 unexpected failures and 0 unexpected successes out of 22036 tests.` — self-consistent |
| `Failures:` / `Unexpected failures:` / `Unexpected successes:` labels | Misaligned exactly as documented: `Failures:` → `0`, `Unexpected failures:` → `22036`, `Unexpected successes:` → the actual failing-test names | Correctly labeled |
| Printed failure-name list | Truncated at 528 of 629 names (stops at `MULTIPLE-VALUE-LIST.7`, silently drops `NIL.1` onward) | All 629 (631 raw tokens, ~2 duplicate registrations) printed |
| `WARN` diagnostics | Literal unprocessed control string: `Warning: Redefining test ~:@(~S~)` | Correctly formatted: `Warning: Redefining test SET-SYNTAX-FROM-CHAR-TRAIT-X-A` |

**The failure count itself did not change (629 both times)** — confirmed by diffing the
two runs: the first 528 failure names are byte-identical and in the same order between
baseline and fixed; the 101 names newly visible after the fix are exactly the tail that
was being silently dropped by the `~{~}` `'NIL' in part` heuristic (includes `NIL.1`,
`NIL.2`, `NIL.7`, confirming the plan's own diagnosis). This was a pure measurement fix,
not a semantic one — 629/22036 (~2.9%) is the real current failure rate, not the "817
failures" or "21980/21980 processed" numbers this document previously flagged as
artifacts, and not the ~21%-of-13.6%-coverage figure below either.

**Still not done from M0's list**: the run-completeness assertion (step 2), deleting
REPAIR.md's stop-condition heuristic (step 3), the RT-list-based structured reporter
that bypasses FORMAT entirely (step 6), wiring `ansi-test/expected-failures/fclpy.sexp`
so "unexpected failure" is a real regression signal (step 7), Python-object-leakage
triage (step 8), and the 113-entry registered-vs-reference-suite surplus investigation
(step 9). `scripts/ansi_score.py` per-subsystem scoreboard still does not exist.

---

## Two dimensions of compliance — read this before the milestones

"100% ANSI" is **not** primarily an evaluator problem. It has two independent
dimensions, and a milestone number is not a statement about which matters more.

| | **A — Language semantics** | **B — Environment / ecosystem** |
|---|---|---|
| Covers | evaluation, environments, lambda lists, macros, `SETF`/places, multiple values, control flow, conditions, CLOS, types | packages, reader, printer, `FORMAT`, streams, files, pathnames, `LOAD`, `COMPILE`/`COMPILE-FILE`, implementation variables, external formats, `*FEATURES*` |
| Failure mode if wrong | existing code computes the **wrong answer** | existing code **cannot be loaded at all** |
| Milestones | M2, M3, M4, M5, M6, M7, M8, M9 | M0, M1, M9 (types-as-interface), M10, Phase 4 |
| Test evidence today | 13.6% measured, ~21% failing | **essentially unmeasured** |

**Why this distinction has to be explicit.** Every visible failure in the log is an
A-dimension failure, because A is the only dimension that has ever executed. `packages/`,
`reader/`, `printer/`, `streams/`, `files/`, `pathnames/`, and `system-construction/`
have **never run a single test**. Ranking work by observed failure count therefore
ranks A over B *by construction* — a pure sampling artifact. Do not let the failure
table drive the roadmap past M0.

**B is not "later."** Note how much B-work this plan already front-loads: M0 is
`FORMAT` and the reader-conditional/`*FEATURES*` gap; M1 is the package system. That is
not a detour before the "real" work — it is the ecosystem dimension asserting itself
first, because you cannot even *measure* A without B.

**The two dimensions gate different things:**

- **A alone** gets you a Lisp that runs correct code correctly. Nothing loads into it.
- **B alone** gets you a Lisp that loads real source and then computes wrong answers.
- **Both** get you the actual goal.

### The acceptance criterion, stated up front

The end goal is not a passing scoreboard — it is **taking existing ANSI source and
running it unmodified**. So the real acceptance test is the ecosystem ladder (detailed
in Phase 5), and it is worth knowing from day one what each rung proves:

| Rung | Dimension exercised | What passing it proves |
|---|---|---|
| **Alexandria** | A-heavy: macros, lambda lists, `SETF` places, types | The macro system and lambda-list engine are real (M3, M4, M5) |
| **CL-PPCRE** | A: pure computation, deep recursion, string/char semantics | Evaluator, sequences, and the string representation are sound (M6, M9) |
| **FiveAM** | A+B: conditions, restarts, CLOS, printer | The condition system signals before unwinding (M8) and CLOS dispatches (M9) |
| **ASDF** | B-heavy: pathnames, streams, `LOAD`, packages, `COMPILE-FILE` | The implementation can *acquire* code at all (M1, M10) |

Each rung exercises a substantially different part of the implementation, and the
ladder is deliberately ordered so a failure localizes. **A rung that fails tells you
which milestone lied about being complete.** Treat these as first-class milestones
alongside the ANSI suite, not as a victory lap — in particular, ASDF is the one that
converts this project from "a conforming Lisp" into "a Lisp with an ecosystem," and
nothing in the ANSI suite tests it.

**Rule of thumb for sequencing:** when A-work and B-work are both unblocked, prefer
whichever one **unblocks measurement or unblocks loading**. Correct semantics for code
you cannot load, and correct loading of code that then misbehaves, are equally useless.

---

## Reality check: the current status is not measured

**Before any planning, correct the record.** The previously reported status ("21980/21980
tests processed, 817 unexpected failures, zero crashes") is not supported by the
evidence in `run_all_tests.log`.

What the log actually shows:

| Claim | Evidence |
|---|---|
| `Doing 22036 pending test of 22036 test total.` (line 29) | The suite registered 22036 tests |
| ~622 `Test X failed` lines; run ends at `DOTIMES.ERROR.1` | Only a few hundred tests visibly executed |
| Whole run took 128s (docs claim ~20 min) | The run did not do 20 minutes of work |
| `629 failures with 629 unexpected failures and 629 unexpected successes out of 629 tests.` | Self-contradictory — one number in every slot |
| `Failures:` → `0`; `Unexpected failures:` → `22036`; `Unexpected successes:` → *a list of test names* | The report's arguments landed in the **wrong slots** |
| Early log lines print the literal string `Warning: Redefining test ~:@(~S~)` | `FORMAT` is emitting its **control string unprocessed** |

### The exact root cause of the truncation — a single missing implicit block

Traced precisely. RT's `do-entries` (`ansi-test/rt.lsp:489`) is one `(dolist (entry
(cdr *entries*)) ...)` with **no early-exit path**. The last executed test,
`DOTIMES.ERROR.1`, is the final form in `iteration/dotimes.lsp:223`. The very next
entry is `iteration/loop.lsp:9`:

```lisp
(deftest sloop.1 (loop (return 'a)) a)
```

**`eval_loop` establishes no implicit `NIL` block**
(`evaluation_loops_conditionals.py:2172-2179`; `execute_iteration_body` at `:1710` has
no `ReturnFromException` handler). So `(return 'a)` → `RETURN-FROM NIL` →
`ReturnFromException` propagates out of `eval_loop`, past `do-entry`'s `(catch
'*in-test* ...)` (which only intercepts `ThrowException`), and lands in
**`eval_dolist`'s `_run_with_nil_block`** (`:2227-2231`) — which catches *any*
`ReturnFromException` tagged `NIL` and returns normally.

RT's driver `dolist` therefore **returns cleanly** at entry ~2990 of 22036, falls
through to the summary, and `do-tests` returns `T`. No traceback, no crash, no missing
output. That is why the run took 128s instead of 20 minutes.

Arithmetic confirms it: `deftest` count in the four executed areas = symbols 1145 +
eval-and-compile 326 + data-and-control-flow 1436 + iteration{do,dostar,dolist,dotimes}
99 = 3006, minus 26 duplicate registrations ≈ **2980**. Observed: 2983–2990.

### The real denominator

| | |
|---|---|
| Registered | 22 036 |
| **Actually executed** | **~2 990** |
| **Measured fraction** | **13.6%** |
| Failure rate on what *was* measured | 629 / 2990 ≈ **21%** |
| Areas with **zero** tests ever executed | **20 of 24** |

Never executed: `objects/` (CLOS), `conditions/`, `cons/`, `arrays/`, `hash-tables/`,
`packages/`, `numbers/`, `sequences/`, `structures/`, `types-and-classes/`, `strings/`,
`characters/`, `pathnames/`, `files/`, `streams/`, `printer/`, `reader/`,
`system-construction/`, `environment/`, `misc/`, and 18 of 19 `iteration/` files.

### Three conclusions that reorder the roadmap

1. **"Zero crashes" is false in substance.** The suite does not crash because the
   crash-equivalent is *swallowed*. Worse, **REPAIR.md's stop condition is precisely
   the ambiguity this hides behind**: `REPAIR.md:26-33` says that if the log's last
   test is also the last test in its `.lsp` file, treat the run as complete.
   `DOTIMES.ERROR.1` *is* the last test in `dotimes.lsp`. The SOP read a silent abort
   as a clean finish. **That heuristic must be deleted.**
2. **Every compliance number this project has quoted is an artifact.** "817 unexpected
   failures" was read out of a mis-aligned `FORMAT` slot; "21980/21980 processed" is
   the doubled-`count` artifact of a `~:P` bug (below). Neither is
   `(length *unexpected-failures*)`.
3. **`FORMAT`'s argument cursor is structurally wrong**, and it corrupts the harness's
   own reporting. Decoded precisely:
   - **`~:P` permanently shifts the argument pointer −1** (`io_write.py:1319-1333`):
     it backs up, then reads `args[arg_idx - 1]` — two slots back, not one — and
     returns the decremented index without re-advancing. ANSI `~:P` is net-zero. This
     single bug makes every `~A` after the first re-print the *same* value, which is
     the entire "629 629 629" line.
   - **Nested directives don't propagate consumption.** `~[`, `~:[`, `~<`, `~(` each
     call `_format_process(clause, args[arg_idx:])`, which starts a *fresh* `arg_idx =
     0` and discards it (`io_write.py:1187-1213`, `:1041`, `:1090`). So `~:*` inside a
     clause clamps to `max(0, 0-1) = 0` and becomes a no-op.
   - **`~{~}` silently aborts on any item whose printed form contains the substring
     `"NIL"`** (`io_write.py:1288`: `if 'NIL' in part: break`). This is why the failure
     list stops at `MULTIPLE-VALUE-LIST.7` after 528 of 629 entries — the next failing
     test is `NIL.1`.
   - `~&` and `~T` are stubs with no column tracking; `~:(`/`~@(` semantics are swapped.

   **Correction to an earlier hypothesis:** the literal `Warning: Redefining test
   ~:@(~S~)` is **not** a FORMAT bug. `~:@(` works. It is `WARN`
   (`utilities_errors.py:46-50`), which does `print(f"Warning: {datum}")` — it never
   calls FORMAT and **discards its arguments entirely**. It also returns without
   signaling a `WARNING`, so `HANDLER-BIND`/`MUFFLE-WARNING` cannot intercept warnings.
   Practical cost: 26 duplicate test names silently overwrote earlier entries and
   nobody can tell which.

**Milestone 0 exists to fix this.** Nothing else can be prioritized honestly until the
denominator is real.

---

## Architectural findings driving this plan

These are verified against the source, not inferred from test names.

### Finding A — The CL package has no canonical membership

`lispenv.py:44-71` populates `COMMON-LISP` as a **side effect of the function registry**:
every registered Python callable gets interned and exported. Nothing declares what CL
*should* contain. Measured against `ansi-test/cl-symbol-names.lsp` (the authoritative 978):

- **936** present, **921** external
- **42 absent entirely** — and the pattern is diagnostic: almost all are symbols that
  are *not functions*. REPL history vars (`**`, `***`, `++`, `//`), declaration
  identifiers (`SPEED`, `SAFETY`, `DEBUG`, `OTHERWISE`, `COMPILATION-SPEED`,
  `DECLARATION`, `VARIABLE`, `STRUCTURE`), type-specifier heads (`SATISFIES`,
  `UNSIGNED-BYTE`, `SIGNED-BYTE`), and hooks (`*MACROEXPAND-HOOK*`, `*READ-EVAL*`,
  `*READ-SUPPRESS*`, `*DEBUGGER-HOOK*`, `*RANDOM-STATE*`). **Nothing registers a name
  unless something registers a callable for it.**
- **15 present but not external** (`*FEATURES*`, `*DEBUG-IO*`, `*COMPILE-VERBOSE*`,
  `*DEFAULT-PATHNAME-DEFAULTS*`, `SYMBOL`, `WARNING`, …) — pure copy-paste drift in
  `lispenv.py:87-515`, where some vars get a paired `export_symbol` call and some don't.
- **114 non-ANSI symbols wrongly exported from CL** — `EVAL-IF`, `EVAL-LET`,
  `EVAL-TAGBODY`, `PUTPROP`, `PUTHASH`, `LIST-STAR`, `QUIT`, `GET-ENV`, … leaked by
  `registry.register_module()` auto-registering every public callable in a module.
  This is its own ANSI violation (`cl-symbols.lsp` checks for extras) **and** a
  namespace hazard for real library code.

Aggravating factor: all ~430 lines of standard-variable setup in `lispenv.py:87-515`
sit inside **one `try:` with `except Exception: pass` (`:513-515`)**. A single failure
silently drops every remaining variable. This is why the class of bug was invisible.

### Finding B — The standard macros are not macros

```
Test MACRO-FUNCTION.1 failed
Form: (LOOP FOR N IN *CL-MACRO-SYMBOLS* UNLESS (MACRO-FUNCTION N) COLLECT N)
Expected: NIL
Actual:   (AND ASSERT CASE CCASE ECASE CHECK-TYPE COND DECLAIM DEFCLASS DEFCONSTANT
           DEFGENERIC ... WITH-SLOTS WITH-STANDARD-IO-SYNTAX)   ; ~90 symbols
```

**Essentially every standard macro is implemented as a hardcoded special form in the
evaluator, with no macro function behind it.** This is the single most
ecosystem-hostile fact in the codebase. Real CL code walks and expands macros
constantly — code walkers, `macroexpand`-based tooling, `DEFINE-COMPILER-MACRO`,
`SETF` of macro places, and anything built on `MACROLET` all depend on standard
macros being *actual macros*. It also means every one of those ~90 operators has
its semantics written twice or once-and-wrong, with no shared expansion path.

### Finding C — Lambda lists are parsed ad-hoc and incompletely

```
Test FLET.8 failed
Form: (FLET ((%F (&KEY A (B 0 B-P)) (VALUES A B (NOT (NOT B-P))))) (%F))
Actual: <UNBOUNDVARIABLE: Unbound variable: B-P>
```
```
Test FLET.6 failed
Form: (BLOCK %F (FLET ((%F (&AUX (X (RETURN-FROM %F 10))) 20)) (%F)))
Expected: 10   Actual: 20
```

`&key` supplied-p variables are not bound; `&aux` init forms are not evaluated in a
context that permits non-local exit. This is why FLET (35), LAMBDA (23),
DESTRUCTURING-BIND (22), and LABELS (18) cluster — **they are all the same lambda-list
parser, or worse, several copies of it.**

### Finding D — `(declare (special ...))` is not honored

```
Test DOTIMES.23A failed
Form: (LET ((X :GOOD) (BOUND 10)) (DECLARE (SPECIAL X))
        (LET ((X :BAD)) (DOTIMES (I BOUND X) (DECLARE (SPECIAL X)))))
Expected: :GOOD   Actual: :BAD
```

The inner reference resolved lexically to `:BAD` instead of dynamically to `:GOOD`.
Either there is no dynamic binding stack distinct from lexical frames, or `DECLARE
(SPECIAL …)` is not processed. This is a **binding-model** defect, not a `DOTIMES`
defect, and it will misbehave under every binding form.

### Finding E — Conditions are Python exceptions in a trenchcoat

- `_CONDITION_HIERARCHY` (`evaluation_conditions.py:16-23`) is a **hardcoded 6-entry
  dict**; type matching converts a Python class name to a hyphenated *string* and does
  membership tests. Python `isinstance` — which would give a real lattice for free —
  is never used.
- `DEFINE-CONDITION` (`evaluation_core.py:1359-1392`) stores a dict and **creates no
  class, no type, no constructor; nothing ever reads it back**. User-defined conditions
  can never be signaled or handled.
- **`HANDLER-BIND` runs handlers inside a Python `except` block — after the stack has
  already unwound** (`evaluation_conditions.py:664-679`). ANSI requires handlers to run
  in the dynamic environment of the `SIGNAL` call. Consequence: every `RESTART-CASE`
  inside the protected form has already had its `finally` pop run, so **a handler can
  never invoke a restart** — the entire point of `HANDLER-BIND`.
- `COMPUTE-RESTARTS` → `[]`, `FIND-RESTART` → `NIL`, `MUFFLE-WARNING`/`STORE-VALUE`/
  `USE-VALUE`/`ABORT` → no-ops. `INVOKE-RESTART` calls the handler *then* raises, and
  `RESTART-CASE` catches and calls it **a second time**.
- `IGNORE-ERRORS` returns `str(e)` — a Python string — as the condition object.

This cannot be retrofitted onto `try/except`. It needs a **handler stack walked at the
signal point, before unwinding**.

### Finding F — `SUBTYPEP` has no type lattice

`comparison.py:484-583` is ~25 hardcoded `if t1 == 'X' and t2 in [...]` string
comparisons on uppercased names. It **cannot handle compound specifiers at all**:
`(subtypep '(integer 0 10) 'integer)` stringifies the cons and returns `NIL, T` —
"definitely not a subtype", the *worst* legal answer, where `NIL, NIL` ("can't tell")
would at least be conforming. It asserts both `CHARACTER ⊆ BASE-CHAR` and
`BASE-CHAR ⊆ CHARACTER`. It never returns `NIL, NIL`, so it always claims certainty it
does not have. `TYPEP` by contrast is a genuine recursive walk and is
incomplete-but-sound (missing: `SATISFIES` → hardwired `NIL`, `DEFTYPE` user types
never consulted, array rank/dimensions ignored).

### Finding G — NIL has three representations

Python `None`, the `lisptype.NIL` singleton (a `lispNull(lispList)`, **not** a
`LispSymbol`), and a `LispSymbol` named `"NIL"`. So `(eq nil 'nil)` is false, and 61+
sites carry `x is None or x == lisptype.NIL` guards. `SYMBOL-VALUE` special-cases by
*name string*, so `FOO::NIL` self-evaluates. Note `T` *is* a real `LispSymbol` — NIL is
the odd one out only because `lispNull` subclasses `lispList` and not also `LispSymbol`.

### Finding H — `GET-SETF-EXPANSION` is decoration; there are **five** place protocols

`misc_macros.py:290-293` in full:

```python
@_registry.cl_function('GET-SETF-EXPANSION')
def get_setf_expansion(place, environment=None):
    return [], [], [], place, place
```

A fixed tuple, ignoring `place` except to echo it. **Nothing in the codebase calls it.**

`SETF` is instead a ~360-line `elif op_name == '...'` ladder inlined in the evaluator
(`evaluation_core.py:435-793`). And it is not the only one:

| Protocol | Location | Places | Used by |
|---|---|---|---|
| SETF's ladder | `evaluation_core.py:435-793` | ~30 | SETF only |
| A *copy* of the ladder | `evaluation_core.py:794-917` | 8 | PSETF only |
| `_place_accessor` | `evaluation_special_forms.py:2247-2289` | 6 | ROTATEF only |
| `_assign_variable_or_place` | `evaluation_conditions.py:387` | 2 | SETQ only |
| INCF/DECF mini-protocol | `evaluation_special_forms.py:172,246` | symbols + `AREF`/`SVREF` | INCF/DECF |

This *is* the failure distribution: PSETF 33 (8 of ~30 places), ROTATEF 23 (6 places),
SETF 5 (broadest ladder, fewest failures). `SHIFTF` **does not exist** — the only
occurrence of the string in the package is a comment. `DEFINE-MODIFY-MACRO` raises
`LispNotImplementedError`. `DEFINE-SETF-EXPANDER` parses and stores an expander that
**nothing ever executes** (`evaluation_core.py:1289-1347` stores `type: 'expander'`;
the sole consumer at `:728` tests for `'short'` and falls through to `pass`). `DEFSETF`
long form is `else: pass`.

Worse than missing: `SETF` of `SUBSEQ`, `GETF`, `LDB`, `MASK-FIELD` are `pass`
statements that **silently succeed while doing nothing**, and unknown places are
"silently accepted" (`:785`). Evaluation order is inverted — `:457` evaluates the new
value **before** the place's subforms. `(setf (caddr x) v)` navigates the composition
left-to-right (CL composes right-to-left) and one step too far.

### Finding I — One `LispString`/`str` split explains the EQUAL/EQUALP cluster

`lisptype_basic.py:338`: `class LispString(lispSequence)` — **no `str` base**. Yet every
string branch in `comparison.py` tests `isinstance(obj, str)`. So those branches never
fire for a `LispString` and `(equal (copy-seq "abc") (copy-seq "abc"))` falls through to
`return NIL`. Compounding it, **two string representations coexist**: the live reader
returns a Python `str` (`lispreader.py:129`) while `make-string`/`copy-seq` return
`LispString`. The highest-leverage fix for EQUAL/EQUALP is not in `comparison.py` — it
is unifying the string representation.

### Finding J — There is no `coerce_to_function`

`EVERY`/`SOME`/`NOTEVERY`/`NOTANY` (~80 failures) call `predicate(*args)` as a raw
Python callable (`sequences_higher.py:71,88`), so `(every 'oddp '(1 3))` raises
`TypeError: 'LispSymbol' object is not callable`. A correct coercion **does exist** — in
`funcall`/`apply` (`evaluation_core.py:1748-1843`) — but these functions don't use it.
`COMPLEMENT` has the identical defect (`comparison.py:610`), which then compounds into
EVERY/SOME whenever a COMPLEMENT result is passed. Two further independent bugs in the
same 45 lines: `_cons_to_list` falls through to `return [seq]` for anything that isn't a
list/tuple/cons, so a string or vector is treated as a **one-element sequence containing
itself**; and `SOME` returns `T` instead of the predicate's value.

### Finding K — One-line bug worth fixing immediately, out of sequence

`apply` re-raises `ReturnFromException`/`ThrowException`/`GoException`
(`evaluation_core.py:1785-1789`). **`funcall` does not** (`:1858-1889` ends in a bare
`except Exception`). Every `RETURN-FROM`, `GO`, or `THROW` crossing a `FUNCALL`
boundary is silently converted into an `ERROR` condition. Wide blast radius across
`UNWIND-PROTECT`, `BLOCK`, `CATCH`, and every higher-order function.

### Finding L — Duplicate and dead implementations

Two CLOS implementations (`classes.py`, plus a **second `GenericFunction` defined inline
inside `eval_defmethod`** at `evaluation_special_forms.py:2749-2790`); two `find_class`
and two `_init_builtin_classes` in `classes.py`; `readtable.py`/`readtable_simple.py`
and `reader.py`/`lispreader.py` pairs. Several operators (`RESTART-CASE`, `SIGNAL`,
`ERROR`, `DEFCLASS`, `DEFGENERIC`, `DEFMETHOD`) have **two contradictory
implementations** — a `cl_function` stub that gets exported into CL, and an evaluator
intercept that actually runs. Fixes applied to one path silently don't apply to the
other.

### The real dependency graph

The graph proposed in the brief is close but not right for this codebase. Validated:

```
  [M0] trustworthy measurement (FORMAT + harness + scoreboard)
        │  ← nothing below can be prioritized without this
        ▼
  [M1] symbol / NIL / package identity  ──┐
        ▼                                 │  (independent of each other;
  [M2] environment model:                 │   both feed M4)
       lexical frames × namespaces        │
       + dynamic binding stack            │
       + declaration processing           │
        ▼                                 │
  [M3] ONE lambda-list engine ────────────┘
        ▼
  [M4] macro system: real macro functions, macro envs, MACROLET/SYMBOL-MACROLET
        ▼                        ╲
  [M5] GET-SETF-EXPANSION         ╲──→ [M6] multiple values (mostly independent,
       (5-value protocol)          ╲         but SETF of VALUES needs both)
        ▼                           ╲
  [M7] conditions: signal-before-unwind + real class lattice
        ▼
  [M8] types: real lattice, SUBTYPEP, CLOS consolidation
        ▼
  [M9] reader / printer / FORMAT / streams / pathnames / LOAD / COMPILE
        ▼
  [M10] conformance completion
```

Two corrections to the brief's assumed graph worth stating explicitly:

- **Multiple values are *not* downstream of SETF.** They are a runtime-representation
  concern that must be settled early because `MACROEXPAND-1`, `FIND-SYMBOL`,
  `GET-SETF-EXPANSION`, `SUBTYPEP`, and `FLOOR` all return them. Note `SUBTYPEP`
  currently returns a bare Python tuple while `FIND-SYMBOL` next door returns a
  `MultipleValues` — the representation isn't even self-consistent yet.
- **Conditions sit *below* types, not above.** `HANDLER-CASE`/`HANDLER-BIND` dispatch on
  type specifiers, so a real condition system needs at least a working class lattice,
  which is the same machinery `SUBTYPEP` needs. Build the lattice once, use it twice.

---

## Phase 1 — Foundational language semantics

### M0. Trustworthy measurement *(blocks everything)*

**ANSI semantics.** `FORMAT` (CLHS 22.3) — directive interpretation, argument
consumption, `~:@(...)` case conversion, `~{...~}` iteration, `~<...~>` justification,
`~[...~]` conditionals, `~^`, `~*`, `~P`.

**Components.** The FORMAT implementation; `run_all_tests.py`; a new
`scripts/ansi_score.py`.

**Failures addressed.** Indirectly all of them — plus the RT report itself.

**Work, in strict dependency order.**
1. **Give `LOOP` its implicit `NIL` block** (`evaluation_loops_conditionals.py:2172-2179`
   and the driver paths). This one fix unblocks ~19 000 tests. Expect the run to jump
   from 128s toward the documented ~20 min — **and expect it to abort again at the next
   stray `RETURN`/`GO`/`THROW` leak.** Iterate until the run is complete.
2. **Add a run-completeness assertion**: compare `(length *passed-tests*) + (length
   *failed-tests*)` against `(length (cdr *entries*))` and fail loudly on mismatch.
   This check would have caught the truncation on day one. Add it before anything else,
   so step 1's iterations are self-verifying.
3. **Delete REPAIR.md's "last test in the file means it completed" heuristic** — it is
   what made a silent abort look like a clean finish.
4. **[DONE 2026-08-09]** ~~Fix `WARN`~~ (`utilities_errors.py:46-50`) to route through
   `format_fn` the way `ERROR` already does, and to actually signal a `WARNING`. Until
   this is done, every diagnostic the suite emits is unreadable. Resolved: both the
   special-form (`eval_warn`) and function-designator (`warn_fn`) entry points now
   delegate to one `signal_warning()` helper in `evaluation_conditions.py` that formats
   through `FORMAT` and builds a real condition object. Not done: real handler-stack
   dispatch (so `HANDLER-BIND`/`MUFFLE-WARNING` can intercept before printing) — that
   needs M8's signal-before-unwind rewrite; faking it here would be exactly the kind of
   operator-specific workaround this plan warns against.
5. **[DONE 2026-08-09]** ~~Fix FORMAT's argument-cursor model.~~ The structural fix is
   to make `_format_process` share **one mutable argument cursor** across nested
   directives instead of slicing `args[arg_idx:]` and restarting at 0. That single
   change fixes `~[`, `~:[`, `~{`, `~<`, `~(`, and `~:*` together. Then: make `~:P`
   net-zero, and **delete the `'NIL' in part` heuristic at `io_write.py:1288`**.
   Resolved via a `_FormatCursor` class shared by `~<...~>`, `~(...~)`, `~[...~]`, and
   `~@?`; `~{...~}` and plain `~?` correctly keep their own independent per-scope cursor
   instead (CLHS 22.3.7 gives them their own argument scope, that was never the bug).
   Also fixed while rewriting the same code path: `~:(`/`~@(` case-conversion semantics
   were swapped (`~:(` must capitalize every word, `~@(` only the first) — see the
   "Update" note near the top of this document for verified before/after numbers.
6. **Add a structured reporter — do not parse the log.** RT already maintains
   `*passed-tests*`/`*failed-tests*`/`*unexpected-failures*` (`rt.lsp:503-506`). A
   ~30-line Lisp epilogue after `do-tests` can dump those lists one name per line,
   **bypassing FORMAT entirely**. A Python post-pass maps each name to its defining
   `.lsp` file (grep `(deftest <name>` across `ansi-test/`) and emits per-file and
   per-directory pass/fail/total. This is the only scoreboard that is independent of
   the FORMAT bugs — build it this way even after FORMAT is fixed.
7. **Create `ansi-test/expected-failures/fclpy.sexp`** and call `(do-tests
   :expected-failures ...)`. Today `*expected-failures*` is permanently `NIL` — `init.lsp:44-70`
   loads it only under `#+allegro`/`#+clasp`/`#+lispworks`/`#+cmucl` — so "unexpected
   failure" is currently just a synonym for "failure" and `*unexpected-successes*` is
   structurally always empty. Wiring this makes "unexpected" a real regression signal.
8. **Triage by Python-object leakage.** The current log shows 47 `TypeError`, 42
   `UnboundVariable`, 37 `Error`, 4 `ProgramError`, and one raw `<function
   _pop_expander at 0x...>` appearing *as test result values*. **A Python object
   surfacing as a Lisp value is always a bug** — cheap, high-signal triage axis.
9. **Investigate the 113-entry surplus**: fclpy registers 22 036 entries vs. 21 923 in
   the reference real-Lisp run (`doit-utf8.log:7`). Likely `#+`/`#-` reader
   conditionals not being honored — fclpy pushes no implementation feature, so
   `#-sbcl`-guarded tests register that a real Lisp skips. Fixing `*FEATURES*` and
   reader conditionals is a prerequisite for comparing against any reference.

**Completion criteria.** All 22036 tests execute, asserted programmatically, not
eyeballed. `ansi_score.py` emits a per-subsystem table derived from RT's own lists.
A baseline snapshot is committed. RT's summary line is self-consistent.

**Edge cases.** RT reports through `FORMAT` — verify against RT's *actual* control
strings (`rt.lsp:507-518`, `rt.lsp:484`), not synthetic ones. Note 7 failed entries
produced no output at all in the last run — a loose thread worth pulling.

**A structural note for step 1.** `_run_with_nil_block` matching a **bare tag name**
`NIL` is what let the leak be swallowed. Name-based block matching
(`evaluation_control_flow.py:36-51`) is the underlying defect; M7 replaces it with
lexical block identity. Step 1 is the tourniquet, M7 is the cure.

**Foundational vs. polish.** Pure foundation. This is the instrument; everything else
is measured with it.

---

### M1. Symbol, NIL, and package identity

**ANSI semantics.** CLHS 10 (symbols) and 11 (packages). Symbols have name, home
package, value cell, function cell, plist — value and function cells independent.
`NIL` is simultaneously a symbol, a boolean, and the empty list, and is `EQ` to
itself in all three roles. Packages track present/external/inherited/shadowing
symbols; `FIND-SYMBOL` returns four states; name conflicts signal correctable errors.

**Components.** `lisptype_basic.py` (LispSymbol, lispNull), `lisptype_extended.py:322-496`
(Package), `lispenv.py:44-515`, `lispfunc/registry.py`, `utilities_symbols.py`,
`misc_packages.py`.

**Failures addressed.** All `SYMBOL-*` (22 visible, but the real count is the full
`cl-symbol-names.lsp` file), `MAKE-SYMBOL` (10), `COPY-SYMBOL` (4), `GENSYM` (4),
`SYMBOLP`/`SYMBOL-NAME` (6), `NIL` (3), and the entire `packages/` directory that has
never been measured.

**Dependencies.** None. Start here in parallel with M0.

**Work, in leverage order.**
1. **Canonical CL symbol table.** Extract the 978 names from
   `ansi-test/cl-symbol-names.lsp` into a data file. At bootstrap, intern + export all
   978 **unconditionally**, independent of whether a binding exists. The registry then
   supplies *bindings only* — it must never decide *membership*. This closes 57 gaps
   mechanically. **Also assert no extras**, which removes the 114 leaked internals;
   move them to an `FCLPY-INTERNAL` package and stop `register_module()` from
   auto-exporting into CL.
2. **Delete the blanket `except Exception: pass` at `lispenv.py:513-515`.** Bootstrap
   failures must be loud. This one line has been hiding an unknown number of defects.
3. **Unify NIL.** Make `lispNull` subclass both `LispSymbol` and `lispList`, and make
   the singleton *be* `COMMON-LISP::NIL`. Then delete the 61+ `x is None or x ==
   lisptype.NIL` guards and the name-string special cases in `SYMBOL-VALUE`. Invasive
   but bounded, and it removes a permanent tax on every predicate.
4. **Package model repairs.** Add the missing `shadowing_symbols` field (its absence
   makes **`UNINTERN` raise `AttributeError` on every call**). Make `external_symbols`
   hold symbol objects, not name strings. Put CL/CL-USER/KEYWORD into
   `state.packages` (today `LIST-ALL-PACKAGES` omits all three). Make `IMPORT` import
   the *original symbol* rather than fabricating a same-named copy. Make `EXPORT`
   check accessibility instead of inventing symbols. Implement `SHADOW`,
   `SHADOWING-IMPORT`, `WITH-PACKAGE-ITERATOR`, `PACKAGE-SHADOWING-SYMBOLS` (all
   currently `return T`/`return NIL` stubs). Fix `RENAME-PACKAGE` (sets `.name` but
   never updates the registry key, making the package unfindable). Extend `DEFPACKAGE`
   with `:SHADOW`, `:SHADOWING-IMPORT-FROM`, `:IMPORT-FROM`, `:SIZE`, `:DOCUMENTATION`.
5. `INTERN` must **not** upcase — the *reader* upcases; `INTERN` takes the string
   verbatim, or `|foo|` is unreachable. Remove the `if name == 'T'` special case that
   makes `MYPKG::T` return `COMMON-LISP:T`.
6. `COPY-SYMBOL` must honor `copy-props` (currently accepts and discards the argument).

**Completion criteria.** `cl-symbol-names.lsp` and `cl-symbols.lsp` pass at 100%
(both directions: nothing missing, nothing extra). The `packages/` test directory
executes fully with a recorded baseline. `(eq nil 'nil)` → `T`.

**Edge cases.** `NIL` and `T` as symbols vs. constants; keyword symbols self-evaluate
and are always external; uninterned symbol printing under `*print-escape*`;
`UNINTERN` of a shadowing symbol must signal if it uncovers a conflict; `DO-SYMBOLS`
may see a symbol more than once; symbols from deleted packages.

**Verification.** `ansi-test/packages/`, `ansi-test/symbols/`, `cl-symbol-names.lsp`.

**Foundational vs. polish.** Steps 1–4 are foundational. Step 6 is polish but free.

---

### M2. The environment model

**ANSI semantics.** CLHS 3.1. Lexical environments carry **separate namespaces** for
variables, functions, blocks, tags, macros, symbol-macros, and declarations. Dynamic
(special) bindings live on a distinct stack, are established by binding forms when the
variable is `SPECIAL`, and unwind on *every* exit including non-local. `(declare
(special x))` in a binding form makes *that binding* dynamic; in a body it makes
*references* dynamic. `PROCLAIM`/`DECLAIM (special x)` is global and retroactive for
subsequent bindings.

**Components.** `lisptype_extended.py` (Environment), `lispfunc/evaluation_core.py`,
`evaluation_special_forms.py`, `evaluation_loops_conditionals.py`, `state.py`.

**Failures addressed.** `LET` (3), `LET*` (5), `DOTIMES` (10), `DOLIST` (6),
`PROCLAIM` (10), `THE` (9), `DYNAMIC-EXTENT` (3), plus the 42 `UnboundVariable`
leaks — and it is a hard prerequisite for M3/M4.

**Dependencies.** M1 (symbols must have stable identity before you can key an
environment on them).

**Confirmed root causes.** Four, and they are the spine of the whole project:

- **RC-1 — `Environment` has no notion of "kind of binding."** It carries variables and
  functions-plus-macros, both **keyed by `symbol.name` (a string)**, so `FOO::X` and
  `BAR::X` are the same variable. There is **no block namespace, no tag namespace, no
  declaration set, and no dynamic frame**. `tag_bindings` has zero readers; `bindings`
  is dead code shadowed by `variable_bindings`. Child frames **snapshot the parent's
  list by value** (`lisptype_extended.py:43`) while `set_variable` mutates the inherited
  `Binding` node in place — so an inner assignment silently writes an outer frame.
- **RC-2 — Lambda-list handling was copy-pasted six times.** `parse_lambda_list`,
  `eval_lambda`'s inline binder, `eval_defun`'s near-verbatim copy, `make_lambda_closure`
  (FLET/LABELS), `_create_macro_function` (400 lines of per-shape special-casing), and
  `eval_destructuring_bind`'s `_bind` — which supports **no lambda-list keywords at all**,
  binding `&REST` as if it were an ordinary variable name. ≈113 failures.
- **RC-3 — The ~110-branch `elif operator.name == '...'` chain inside `eval`**
  (`evaluation_core.py:383-1512`) runs **before** any macro or function lookup (`:1515`).
  Operator precedence is inverted: nothing in that list can be shadowed by
  `FLET`/`MACROLET`, and `PROCLAIM`/`MACROEXPAND-1`/`MACRO-FUNCTION` can't be `#'`d or
  `funcall`ed. Dispatch is on `.name` only, so a symbol named `"IF"` in *any* package is
  the special operator.
- **RC-4 — `state.current_environment`**, a global mutable "current env." Maintained by
  only 2 of ~10 binding forms; `APPLY`/`FUNCALL` resolve function designators through it
  rather than lexically; a *second*, stale global `lispenv.current_environment` (an
  import-time snapshot) is read by `COMPILE` and `MACRO-FUNCTION`.

**Work.**
1. **Rebuild `Environment` first (RC-1).** Separate namespaces for variables, functions,
   macros, blocks, tags, symbol-macros, and declarations; **key on symbol identity, not
   name**; child frames must not alias the parent's binding list. Then delete
   `state.current_environment` and `lispenv.current_environment` and thread the
   environment (RC-4).
2. Introduce a **real dynamic binding stack**, replacing the current save/overwrite/restore
   of the symbol object's `.value` attribute. Note `LET` and `LET*` today use **completely
   different mechanisms**: `LET` saves/restores `var.value` in a `finally`; `eval_letstar`
   calls `global_env.add_variable(...)` and **never removes it**, so `LET*` of a special
   variable permanently corrupts the global environment.
3. **Build a declaration processing layer used by *every* binding form.** Today
   `DECLARE` is dispatched as a special operator and `eval_declare` appends to
   `env._declarations`, which **has zero readers** — `TYPE`, `FTYPE`, `IGNORE`,
   `IGNORABLE`, `DYNAMIC-EXTENT`, `INLINE`, `NOTINLINE` are total no-ops. `LET`/`LET*`
   each independently re-parse `SPECIAL` into a local Python set that is **thrown away**.
   Every other binding form (`DOTIMES`, `DO`, `DOLIST`, `MULTIPLE-VALUE-BIND`,
   `DESTRUCTURING-BIND`, `FLET`, `LABELS`, `SYMBOL-MACROLET`, lambda, `DEFUN`) hands
   `DECLARE` to `eval_tagbody` as an ordinary form and discards it.
4. Because `DECLARE` is dispatchable, **a `DECLARE` in an illegal position is never an
   error** — it silently returns NIL anywhere. Declaration *position* validation is
   required, and it is what the `...ERROR.n` tests in this family check.
5. Delete the `%SPECIAL-REF` symbol-macro trick that `LOCALLY` uses to fake dynamic
   reference — it is a heuristic standing in for the missing dynamic frame.
6. Make `DOTIMES.23A` pass **by fixing the binding model**, not by touching `DOTIMES`.
   Use it as the canary: **if the fix is in `DOTIMES`, it is the wrong fix.**
7. Fix the Finding K one-liner (`funcall` missing its non-local-exit re-raise) — out of
   sequence, immediately, because it corrupts diagnostics for every later milestone.

**Completion criteria.** `DOTIMES.23A` and its siblings pass with zero changes inside
the iteration macros. A globally-`DECLAIM`ed special binds dynamically in every
binding form. `UnboundVariable` leaks drop to zero in the score table.

**Edge cases.** A `LET` of a globally-special variable binds dynamically, not
lexically. Free declarations vs. bound declarations. `(declare (special x))` where `x`
is also a parameter. Dynamic bindings unwound by `THROW` through multiple frames.
Constant variables may not be bound. `PROGV` with runtime-computed symbol lists.

**Verification.** `ansi-test/data-and-control-flow/` (let, let*, progv, declare),
`ansi-test/environment/`, `ansi-test/eval-and-compile/`.

**Foundational vs. polish.** Entirely foundational — the highest-leverage milestone
in Phase 1 after M0.

---

### M3. One lambda-list engine

**ANSI semantics.** CLHS 3.4. Ordinary, macro, destructuring, boa, defsetf,
deftype, and method lambda lists. `&optional` init forms + supplied-p; `&rest`;
`&key` with `((:keyword var) init supplied-p)` syntax; `&allow-other-keys` and
runtime `:allow-other-keys t`; `&aux`; `&whole`; `&body`; `&environment`; nested
destructuring. **Init forms evaluate left-to-right in an environment containing the
preceding parameters.**

**Components.** Wherever lambda lists are currently parsed — the point of this
milestone is that the answer must become *one place*.

**Failures addressed.** FLET (35), LAMBDA (23), DESTRUCTURING-BIND (22), LABELS (18),
`LAMBDA-LIST-KEYWORDS` (3), DEFUN (5), DEFMACRO (4), and a large share of the 42
`UnboundVariable` leaks. **~105 visible failures from one component.**

**Dependencies.** M2 (init forms need sequential environments and declaration
processing; `&aux` with `RETURN-FROM` needs correct non-local exit).

**Work.**
1. Find every lambda-list parser in the codebase and **delete all but one.** Write a
   single parser producing a normalized parameter description, plus a single binder
   that consumes it.
2. Fix the two proven defects: `&key` supplied-p variables must be bound (`FLET.8`),
   and `&aux` init forms must be evaluated such that `RETURN-FROM` out of one works
   (`FLET.6`).
3. Implement full destructuring for macro lambda lists, including `&whole`,
   `&environment`, and dotted/nested patterns.
4. Signal `PROGRAM-ERROR` for: unknown keyword arguments, odd-length keyword lists,
   too few/too many arguments, and malformed lambda lists.

**Completion criteria.** FLET, LABELS, LAMBDA, and DESTRUCTURING-BIND test files pass
at 100%. `grep` finds exactly one lambda-list parser.

**Edge cases.** `:allow-other-keys t` supplied at *call* time overrides the absence of
`&allow-other-keys`. Duplicate keyword arguments — leftmost wins. `&rest` and `&key`
together share the same arguments. A `&key` whose keyword is not the symbol's name.
`&aux` variables are not parameters and take no supplied-p. `NIL` as a destructuring
pattern element.

**Verification.** `ansi-test/data-and-control-flow/{flet,labels,lambda,destructuring-bind}.lsp`.

**Foundational vs. polish.** Foundational, and the single best failures-per-unit-effort
item in the plan.

---

## Phase 2 — Core language facilities

### M4. A real macro system

**ANSI semantics.** CLHS 3.1.2.1.2, 5.3. Every standard macro operator has a **macro
function** of `(form, environment)` retrievable via `MACRO-FUNCTION` and settable via
`(setf macro-function)`. `MACROEXPAND-1` returns `(expansion, expanded-p)` and honors
`*MACROEXPAND-HOOK*`. `MACROLET`/`SYMBOL-MACROLET` create **lexical** macro bindings.
A `MACROLET` expander body runs in the **null lexical environment** — it must not see
surrounding variable bindings.

**Components.** `registry.py`, `evaluation_core.py`, `evaluation_special_forms.py`.

**Failures addressed.** `MACRO-FUNCTION` (8), `MACROLET` (15), `MACROEXPAND-1` (6),
`MACROEXPAND` (3), `SYMBOL-MACROLET` (5), `DEFINE-COMPILER-MACRO` (8), `DEFMACRO` (4),
`COND` (5), `EVAL-WHEN` (5), and — via Finding B — correctness for ~90 operators at once.

**Dependencies.** M2 (macro environments are environments), M3 (macro lambda lists).

**Work.**
1. **Reclassify.** ANSI defines exactly 25 special operators. Everything else in that
   ~90-symbol list from `MACRO-FUNCTION.1` **must become a real macro** with a macro
   function. Where the semantics are already correct in the evaluator, the fastest
   honest path is to define the macro in Lisp (or as a Python macro function returning
   an expansion) and let the existing special-form code be deleted, not kept in
   parallel.
2. Implement `MACRO-FUNCTION` as a real accessor honoring its environment argument,
   and make it `SETF`-able.
3. Make `MACROEXPAND-1`/`MACROEXPAND` return proper two values, expand symbol-macros,
   honor `*MACROEXPAND-HOOK*`, and respect the environment argument.
4. Implement `&environment` threading so a macro can pass its env to
   `MACROEXPAND`/`GET-SETF-EXPANSION`.
5. Implement compiler macros (`DEFINE-COMPILER-MACRO`, `COMPILER-MACRO-FUNCTION`).
6. Implement `EVAL-WHEN` with real `:compile-toplevel`/`:load-toplevel`/`:execute`
   situations and top-level-form processing rules.

**Completion criteria.** `MACRO-FUNCTION.1` returns `NIL` — i.e. every standard macro
has a macro function. `MACROLET`/`SYMBOL-MACROLET` files pass at 100%. No operator has
both a special-form implementation and a macro implementation.

**Edge cases.** `MACROLET` expander body sees only macro definitions, not lexical
variables. A lexical `FLET` shadows a global macro and vice versa. Symbol-macro
expansion in `SETF` places. `MACROEXPAND` loops until no longer a macro form.
Recursive macro definitions. Special operators must **not** have macro functions.

**Verification.** `ansi-test/eval-and-compile/`, `ansi-test/data-and-control-flow/macrolet.lsp`.

**Foundational vs. polish.** Foundational, and the **most ecosystem-critical milestone
in the entire plan** — code walkers and macro-heavy libraries are unusable without it.

---

### M5. `GET-SETF-EXPANSION` and generalized places

**ANSI semantics.** CLHS 5.1. `GET-SETF-EXPANSION` returns **five values**: temporary
variables, value forms, store variables, storing form, accessing form. **Every**
place-modifying operator is defined in terms of it, which is what guarantees subforms
are evaluated **exactly once, left-to-right**.

**Components.** wherever `SETF` currently dispatches; `lisptype_extended.py`
(setf-expanders).

**Failures addressed.** PSETF (33), ROTATEF (23), SHIFTF (6), SETF (5), PSETQ (5),
`DEFINE-SETF-EXPANDER` (6), `DEFSETF` (5), `DEFINE-MODIFY-MACRO` (6),
`GET-SETF-EXPANSION` (3), `SETF-VALUES` (4), `SETF-APPLY` (4), `SETF-SYMBOL-MACRO` (3),
`SETF-MACRO` (3). **~106 visible failures from one protocol.**

**Dependencies.** M4 (places may be macros or symbol-macros that must expand first),
M6 (`(setf (values …) …)`).

**Work.**
1. Implement `GET-SETF-EXPANSION` properly, taking an environment argument.
2. **Rewrite `SETF`, `PSETF`, `SHIFTF`, `ROTATEF`, `INCF`, `DECF`, `PUSH`, `POP`,
   `PUSHNEW`, `REMF`, and `DEFINE-MODIFY-MACRO` as thin layers over it.** The
   33-failure PSETF cluster and 23-failure ROTATEF cluster are the signature of each
   being hand-rolled: they are exactly the operators whose correctness *is*
   single-evaluation and ordering.
3. Implement `DEFSETF` (both short and long form) and `DEFINE-SETF-EXPANDER`.
4. Support `(setf (values a b) …)`, `(setf (apply #'f …) …)`, setf of a symbol-macro,
   setf of a macro that expands to a place, and `(setf (the type place) …)`.
5. Provide setf expanders for every standard accessor CLHS defines one for.

**Completion criteria.** Every place operator's implementation calls
`GET-SETF-EXPANSION`; none dispatches on the place's head symbol. All setf/place test
files pass at 100%.

**Edge cases.** Subforms evaluated **once**, left-to-right, even when the operator
reads and writes (`INCF`, `ROTATEF`). `PSETF` computes all values before any store.
`SETF` with multiple pairs is sequential; `PSETF` is parallel. `ROTATEF` returns `NIL`.
`SHIFTF` returns the *old* first value. Store variables may be more than one (for
`VALUES`). A place with side-effecting subforms is the test everything hinges on.

**Verification.** `ansi-test/data-and-control-flow/` setf/psetf/shiftf/rotatef files.

**Foundational vs. polish.** Foundational. Ecosystem code uses places constantly.

---

### M6. Multiple values

**ANSI semantics.** CLHS 3.1.7. A form returns zero or more values. In a
single-value context, extra values are discarded and zero values yield `NIL`. Values
are **not** a first-class object — they cannot be stored in a variable.

**Components.** the evaluator's return path; `MultipleValues`.

**Failures addressed.** `MULTIPLE-VALUE-SETQ` (3+), `VALUES` (3), `VALUES-LIST` (3),
`MULTIPLE-VALUE-BIND/CALL/PROG1/LIST`, and correctness of every multi-value-returning
standard function.

**Dependencies.** M2. Note this is *not* downstream of SETF.

**Work.**
1. Settle **one** representation and enforce it. Today `FIND-SYMBOL` returns
   `MultipleValues` while `SUBTYPEP` returns a bare Python tuple — that inconsistency
   is itself a bug class.
2. Guarantee truncation to the primary value at every single-value context, and `NIL`
   for zero values.
3. Build `VALUES`, `VALUES-LIST`, `MULTIPLE-VALUE-BIND/CALL/LIST/PROG1/SETQ`,
   `NTH-VALUE`, and `MULTIPLE-VALUE-CALL` on the one mechanism.
4. Audit every standard function that must return multiple values (`FLOOR`, `TRUNCATE`,
   `ROUND`, `CEILING`, `GETHASH`, `INTERN`, `FIND-SYMBOL`, `MACROEXPAND-1`,
   `GET-SETF-EXPANSION`, `SUBTYPEP`, `PARSE-INTEGER`, `DECODE-FLOAT`, …).

**Completion criteria.** `(values)` in a single-value context is `NIL` and
distinguishable from `(values nil)` where ANSI distinguishes them. No standard
multi-value function returns a bare tuple.

**Edge cases.** `MULTIPLE-VALUE-PROG1` preserves values across the body.
`MULTIPLE-VALUE-CALL` with several arg forms concatenates all their values. `THE` and
`SETF` pass values through. `VALUES` as a place. `MULTIPLE-VALUES-LIMIT`. Values
through `UNWIND-PROTECT` and `CATCH`/`THROW`.

**Foundational vs. polish.** Foundational; small in scope, wide in blast radius.

---

### M7. Non-local control flow

**ANSI semantics.** `BLOCK`/`RETURN-FROM` and `TAGBODY`/`GO` are **lexically** scoped
with **dynamic** extent — invoking an exit after its extent has ended signals a
control error. `CATCH`/`THROW` are dynamic. `UNWIND-PROTECT` cleanup runs on **every**
exit path.

**Failures addressed.** `UNWIND-PROTECT` (5), `BLOCK`/`RETURN`/`PROG`/`PROG*`/`PROGV`,
and the correctness of `FLET.6`-style `&aux` exits.

**Dependencies.** M2 (block/tag names are environment namespaces; dynamic bindings
must unwind).

**Work.** Make block and tag names lexically scoped environment entries with unique
identity (not name-string matching). Ensure a closure that returns from an exited
block signals `CONTROL-ERROR` rather than succeeding. Make `UNWIND-PROTECT` cleanup
run for Lisp non-local exits, Python exceptions, and dynamic-binding unwinding alike.

**Edge cases.** Nested identically-named blocks. `GO` out of `UNWIND-PROTECT` cleanup.
Cleanup forms that themselves exit non-locally. Values preserved through cleanup.

---

## Phase 3 — ANSI ecosystem infrastructure *(dimension B — co-equal, not subordinate)*

> **The phase number is a dependency ordering, not a priority ranking.** This phase is
> where "can existing Common Lisp software run here?" is actually decided. Phases 1–2
> make correct code compute correct answers; **this phase is what lets correct code
> arrive in the first place.** Three of the four ecosystem-ladder rungs (FiveAM's
> conditions and CLOS, ASDF's pathnames/streams/`LOAD`) are gated here, and none of it
> is tested by the failure log — because none of these areas has ever executed.
>
> Do not defer M10 on the grounds that Phase 1–2 milestones still have open failures.
> Once M2–M6 are structurally sound, M10 should run **in parallel**, not after.

### M8. Conditions and restarts *(rewrite, not repair)*

**ANSI semantics.** CLHS 9. Conditions are **objects of a real class hierarchy**.
`SIGNAL` walks the handler stack **without unwinding**; a handler runs in the dynamic
environment of the signaling form and declines by returning normally. Restarts
established between the handler and the signal point are therefore visible and
invocable. `HANDLER-CASE` unwinds *then* runs its clause; `HANDLER-BIND` does not.

**Failures addressed.** The entire `ansi-test/conditions/` directory — **never yet
measured**.

**Dependencies.** M2 (dynamic environment), M9's class lattice (type-based dispatch).

**Work.** Replace the string-matching `_CONDITION_HIERARCHY` with `isinstance` over a
real class tree covering the full ANSI condition graph (add the missing
`SERIOUS-CONDITION`, `STORAGE-CONDITION`, `CELL-ERROR`, `PACKAGE-ERROR`,
`PARSE-ERROR`, `READER-ERROR`, `PRINT-NOT-READABLE`, `STYLE-WARNING`,
`SIMPLE-WARNING`, `SIMPLE-TYPE-ERROR`, `UNBOUND-SLOT`). Make `DEFINE-CONDITION`
actually create a type with slots, initargs, readers, and a report function.
**Rewrite signaling as a handler stack walked at the signal point.** Build real
restart objects with `:test`/`:report`/`:interactive` and condition association;
implement `COMPUTE-RESTARTS`, `FIND-RESTART`, `INVOKE-RESTART`, `RESTART-NAME`,
`WITH-SIMPLE-RESTART`, `WITH-CONDITION-RESTARTS`, `MUFFLE-WARNING`, `STORE-VALUE`,
`USE-VALUE`, `ABORT`, `CONTINUE`. Delete the duplicate `cl_function` stubs for
`SIGNAL`/`ERROR`/`RESTART-CASE`. Make `IGNORE-ERRORS` return the condition object,
not `str(e)`.

**Completion criteria.** A handler can invoke a restart established inside the
protected form. User-defined conditions signal and are handled. `IGNORE-ERRORS`'
second value is a condition.

**Edge cases.** Handlers disestablished while running (no self-recursion).
`*BREAK-ON-SIGNALS*`. `SIGNAL` of a non-serious condition returns `NIL` if unhandled.
`ERROR` never returns. `WARN` and `MUFFLE-WARNING`. Correctable errors in the reader,
in `EXPORT`, in `USE-PACKAGE`.

**Foundational vs. polish.** Foundational for the ecosystem — condition handling is
how real CL libraries do control flow.

---

### M9. Types, `SUBTYPEP`, and CLOS

**Work.** Replace `SUBTYPEP`'s string-pair table with a real lattice over type
*objects*, supporting compound specifiers, and returning honest `NIL, NIL` when
undecidable. Make `TYPEP` consult `DEFTYPE` user types, implement `SATISFIES`
(currently hardwired to `NIL`), and honor array rank/dimensions. **Consolidate the two
CLOS implementations into one** — delete the inline `GenericFunction` in
`eval_defmethod` and the duplicate `find_class`/`_init_builtin_classes` in
`classes.py`. Add multiple dispatch, C3 linearization, EQL specializers, and
`:BEFORE`/`:AFTER`/`:AROUND` method combination (qualifiers are currently **parsed and
discarded**, silently turning every auxiliary method into a primary).

**Verification.** `ansi-test/types-and-classes/`, `ansi-test/objects/`.

### M10. Reader, printer, streams, pathnames, loader

**Work.** Resolve the `reader.py`/`lispreader.py` and `readtable.py`/`readtable_simple.py`
duplication — one live implementation, delete the dead one. Real readtables,
`SET-MACRO-CHARACTER`, dispatch macro characters, `*READ-EVAL*`/`#.`, `*READ-BASE*`,
`*READ-SUPPRESS*`, readtable-case, package-qualified symbols, correct (including
nested) backquote. Printer: `PRINT-OBJECT` dispatch, the full `*PRINT-*` variable set,
`*PRINT-CIRCLE*`, and print/read consistency under `*PRINT-READABLY*`. Complete
`FORMAT` beyond M0's harness-critical subset. Streams: the full stream class tree,
string streams, `WITH-OPEN-FILE`, element types, `READ-SEQUENCE`/`WRITE-SEQUENCE`.
Pathnames: components, wildcards, `MERGE-PATHNAMES`, `TRANSLATE-PATHNAME`, logical
pathnames. `LOAD`/`COMPILE`/`COMPILE-FILE` with correct `EVAL-WHEN` interaction and
the `*COMPILE-*`/`*LOAD-*` variables.

**Ecosystem note.** This milestone is what makes ASDF and third-party library loading
possible. Nothing from Quicklisp loads without pathnames, streams, and `LOAD`.

---

## Phase 4 — Conformance completion

Only after Phases 1–3 does per-test work become the right mode. Scope:

- Required **error signaling** — `ansi-test`'s many `.ERROR.n` tests assert that a
  specific condition type is signaled for malformed input. These are a large,
  mechanical, genuinely required tail.
- **Evaluation-order** requirements in the few places ANSI pins them down.
- **Implementation variables** — `*FEATURES*`, `*READ-EVAL*`, `*DEBUG-IO*`,
  `*LOAD-VERBOSE*`, `*COMPILE-VERBOSE*`, `*BREAK-ON-SIGNALS*`, `*DEBUGGER-HOOK*`,
  `*MACROEXPAND-HOOK*`, `*MODULES*`, `*RANDOM-STATE*`, `MOST-POSITIVE-FIXNUM` and
  friends — given real, correctly-typed values (M1 makes the *symbols* exist; this
  makes the *values* right).
- **Implementation-defined choices** ANSI permits: document each explicitly rather
  than leaving it accidental.
- **Numeric tower** completion: bignums, ratios, complex, float contagion,
  `least-positive-normalized-*`, and the numeric edge cases.
- Remaining sequence, array, string, character, and hash-table edge cases.

**Discipline for this phase.** Every fix still asks "what mechanism is missing?"
first. If three `.ERROR.n` tests want a `PROGRAM-ERROR` that isn't signaled, the fix is
argument validation in the lambda-list engine, not three `raise` statements.

---

## Phase 5 — Verification and regression prevention

**Demonstrating 100%.**
1. `ansi-test` reports **0 unexpected failures** across all 22036 tests, with a
   `FORMAT` implementation trustworthy enough for its own report — verified
   independently by `scripts/ansi_score.py` parsing raw output.
2. Every entry in `expected-failures/` is either justified in writing against a CLHS
   citation or fixed. "Expected failure" must never become a dumping ground.
3. **Ecosystem proof, which is the actual goal**: load and run real ANSI code
   unmodified. Suggested ladder — Alexandria (macro/lambda-list heavy) → CL-PPCRE
   (pure-ANSI, computational) → FiveAM or a test framework (conditions + CLOS) →
   ASDF (pathnames, streams, `LOAD`, packages). Each rung exercises a different
   subsystem; passing all four is stronger evidence than the test suite alone.
4. A conformance statement documenting every implementation-defined choice.

**Preventing regression.**
- CI runs the full ANSI suite on every commit; **any increase in failures is a build
  failure.** Store the scoreboard as a committed artifact so deltas are reviewable.
- Keep `pytest` (`tests/`) as the fast inner loop, but treat `ansi-test` as the
  authority. When they disagree, the unit test is wrong.
- When a bug is fixed, add a targeted regression test **only if** `ansi-test` does not
  already cover it — otherwise the suite is the regression test.
- Guard the architecture, not just behavior: assert one lambda-list parser, one CLOS
  implementation, one reader, one macro path per operator, and no non-ANSI symbols
  exported from `CL`.

---

## The development loop

Replaces the crash-repair loop (which achieved its goal — crashes are no longer the
binding constraint).

1. Pick the **next milestone**, not the next failing test.
2. Record a baseline scoreboard (`scripts/ansi_score.py`).
3. Implement the mechanism. Resist the local fix.
4. Re-run the suite; diff the scoreboard.
5. **Ask which failures disappeared that you did not target.** That number is the
   measure of whether you fixed a mechanism or a symptom. If a milestone fixes only
   the tests you aimed at, the mechanism is probably still missing.
6. Investigate what remains; attribute each residual failure to a milestone.
7. Repeat.

Track **both** total failures and failures grouped by subsystem — but treat
architectural correctness as the primary metric. A change that fixes 40 tests by
special-casing is a regression in disguise.

### Standing rules
1. Never implement a test. Implement the mechanism the test is checking.
2. Any Python object appearing as a Lisp value is a bug — including exceptions
   (`TypeError`, `UnboundVariable`) surfacing as test results.
3. When two implementations of one operator exist, delete one. Duplication is how
   fixes silently fail to apply.
4. **Never `pass`, never `return form` in a bare `except`, never "silently accept."**
   This is the single worst pattern in the codebase and it is systematic: `pass` for
   unimplemented SETF places (`evaluation_core.py:651,685,688,691`), `except Exception:
   return form` around macroexpansion (`evaluation_special_forms.py:1179`,
   `misc_packages.py:407,422,433`), `return None` for `REMF`, "silently accept" at
   `:785`, the blanket `except` at `lispenv.py:513`, and `if 'NIL' in part: break` in
   FORMAT. **For a compliance project this is the worst possible failure mode**: the
   suite reports a wrong *value* instead of an unimplemented *feature*, so the log
   systematically **undercounts** how much is missing. Raise `LispNotImplementedError`
   instead — a loud gap is measurable, a silent wrong answer is not.
5. Temporary shortcuts must be recorded in "Known temporary deviations" below, with
   the milestone that removes them. Untracked shortcuts are how "mostly compatible"
   happens.
6. **Check which dimension you are starving.** If the last three milestones were all
   dimension A, the next one should probably be B. Failure counts will keep pointing at
   A long after A stops being the constraint — see "Two dimensions of compliance."
7. No debug `print()` in committed code. Commits are the maintainer's call.

---

## Known temporary deviations

Anything knowingly non-ANSI goes here with an owning milestone. Empty means "nothing
is knowingly wrong" — keep it honest.

| Deviation | Why tolerated | Removed by |
|---|---|---|
| `lispenv.py:513-515` blanket `except Exception: pass` | Currently hides bootstrap failures | M1 (delete it) |
| 114 non-ANSI symbols exported from `CL` | Registry auto-export | M1 |
| ~90 standard macros implemented as special forms | Predates the macro system | M4 |
| Five parallel place protocols; `GET-SETF-EXPANSION` a stub | Predates the setf protocol | M5 |
| Six copy-pasted lambda-list binders | Copy-paste, never factored | M3 |
| Two CLOS implementations, two readers, two readtables, dead `printer.py`, dead `tokenizer.py`+`reader.py` fork | Historical forks | M9 / M10 |
| `SUBTYPEP` string-pair table | No type lattice yet | M9 |
| `LispString` vs. Python `str` split | Two string representations coexist | M9 (blocks EQUAL/EQUALP) |
| Name-based block/tag/catch matching | No block identity objects | M7 |

Note the dead `reader.py`/`tokenizer.py` fork (~41 KB) is **exercised by five test
files and used by nothing in production** — those tests pass against code the ANSI
suite never touches, which is actively misleading. Promote it or delete it; keeping
both is the worst option.

---

## Priority order (start here)

0. **[DONE 2026-08-09]** ~~Two fixes to make right now, out of sequence~~, because both
   corrupt the diagnostics every later milestone depends on:
   - `funcall`'s missing non-local-exit re-raise (Finding K) — one line. Fixed;
     regression-tested in `tests/test_phase3_nonlocal_exits.py::TestNonLocalExitThroughFuncall`.
   - `WARN` routing through `format_fn` and actually signaling — five lines. Fixed (see
     M0 item 4 above); ended up broader than five lines once the duplicate-implementation
     consolidation (Finding L) was accounted for.
1. **M0** — implicit `NIL` block for `LOOP` **[done, predates this update]**; run-completeness
   assertion **[not done]**; FORMAT argument cursor **[DONE 2026-08-09]**; RT-list-based
   scoreboard **[not done]**. The suite now runs to completion (22036/22036, verified) —
   remaining M0 work is the completeness *assertion*, the REPAIR.md heuristic deletion,
   the structured reporter, and `expected-failures` wiring (steps 2, 3, 6, 7 above).
2. **M1 step 1** — canonical CL symbol table. Highest yield per unit effort; also
   delete the blanket `except`.
3. **M2** — environment model (RC-1 through RC-4). The prerequisite for M3 and M4, and
   the spine of the project. Do not attempt to fix specials one binding form at a
   time — that just produces a seventh incompatible mechanism.
4. **M3** — one lambda-list engine. ~113 failures from one duplicated component.
5. **M4** — real macro system. The most ecosystem-critical milestone.
6. **M5** — `GET-SETF-EXPANSION`. ~106 failures, and deletes ~600 lines of duplicated
   ladder code.

Everything after M5 should be re-prioritized against the **first trustworthy
scoreboard**, not against this document — the 20 unmeasured areas will almost
certainly reorder the tail.

**Note the dimension mix in that list.** M0 and M1 are dimension **B** (measurement
infrastructure and packages); M2–M5 are dimension **A**. That is not an accident of
convenience — it is B work being genuinely prerequisite. Once M5 lands, **M10 should
start in parallel with M6–M9**, because from that point the two dimensions are
independent and the B side has 20 unmeasured areas to burn down while A finishes.

---

## Key files

| File | Purpose |
|------|---------|
| `CLAUDE.md` | Architecture map + development loop (read first) |
| `plan.md` | This roadmap |
| `REPAIR.md` | Crash-repair SOP — historical; crashes are no longer the constraint |
| `scripts/ansi_score.py` | *(to be built in M0)* per-subsystem scoreboard |
| `docs/ansi_targets.txt` | Prose checklist; superseded in M1 by the canonical 978-symbol table |

```powershell
pipenv install --dev                                              # one-time setup
pipenv run pytest -q                                              # fast inner loop
pipenv run python run_all_tests.py > run_all_tests.log 2> run_all_tests.err  # ANSI suite (~20 min when M0 is done)
```
