# fclpy — CLAUDE.md

fclpy is a Common Lisp interpreter written in Python. The goal is **full ANSI Common
Lisp compliance**: the real ANSI test suite (`ansi-test/`, a sibling directory one
level above this repo) runs to completion and reports **zero failures**, with no
test skipped, suppressed, or declared expected.

"As many tests as possible" was the objective while the suite still crashed. It
is not the objective now, and the difference matters: it is the wording under
which a hard test gets quietly reclassified. **Zero failures is necessary but
not sufficient** — ansi-test does not exercise everything, so a known defect it
happens to miss is still a defect (plan.md §5, and the final gate in §7).

> **Current status.** Last full run **2026-08-22**: `COMPLETENESS: OK`,
> 22132/22132 accounted, 0 missing, 0 extra, **19703 passing (89.0%)**,
> 2429 failing, ~125 minutes. `docs/ansi_checklist.md` is regenerated from it
> and carries no merge amendments. (History: 08-18 77.2%; 08-16 66.8%;
> 08-15 52.2%; first complete run 08-12 40.7% in ~7.5 hours.)
>
> **Six files regressed** against the 08-18 baseline and the baseline was
> deliberately not refreshed — `scripts/gate.py` will report them until they
> are fixed or accepted in writing. That is intended; see plan.md §7.
> `system-construction` and `auxiliary` are at 100%, `pathnames` 99.5%,
> `arrays` and `cons` 98.9%. The worst *rates* — `structures` 32.2%,
> `hash-tables` 55.7%, `environment` 57.8% — each have one named cause, and
> two of the three are duplicate registrations, not missing features.
>
> **The mode has changed, and this is the most important thing on this page.**
> Crashes stopped being the constraint on 08-12; *clusters* stopped being the
> constraint around 08-22. The evidence, from the live checklist: the median
> failing file has **3** failures, 217 of 363 failing files have ≤3, the single
> largest failing file is **3.6%** of the remainder, and only **20 files still
> fail 100%** (132 tests, down from 49 files / 493 tests on 08-16). Work the
> checklist **file by file** now — see plan.md §2's "Working mode". Keep the
> old discipline *inside* each file (smallest reproduction, fix the mechanism,
> check what else moved), and keep looking for a shared mechanism when one
> surfaces on its own; just stop *assuming* one is there before you look.
> The last big one found this way was the ordinary lambda list (2026-08-22):
> 79 failures across four files in three directories, because FLET/LABELS,
> LAMBDA and DEFUN each had their own binder.
>
> **Hang detection now lives in `fclpy/watchdog.py`, not in the loop forms.**
> `LoopWatchdog` evaluates its 120s warning and 600s cap inside `tick()`, once
> per *iteration*, so it cannot see a loop wedged **inside** one iteration —
> which is how the 08-15 tree sat at 27GB for half an hour with no diagnostic.
> `watchdog.arm()` measures *time without progress* instead, warns at 120s and
> hard-stops at 900s (above LOOP's own 600s cap, so a capped loop fails one
> test rather than killing the run), dumping every thread's traceback. Both
> runners arm it; `run_all_tests.py` previously had **no** hang detection at
> all. Runner output is line-buffered — block buffering once left the log ~30
> minutes behind the form actually executing.
>
> **`docs/ansi_checklist.md` is the authority for what is failing and where**,
> and at this stage it is also the *ranking* — read it first. **[plan.md](plan.md)**:
> read §1 (status), §2 (how to work — including **the duplicate register**,
> the one place a cluster argument still holds) and §5 (temporary deviations);
> treat its Tier 1/2 cluster lists as history — they were written against a
> 12,000-failure suite and their ordering no longer holds, and
> `docs/changelog.md` is the archive. The checklist is
> generated — never hand-edit it — and it is kept current *without* a full run
> by folding targeted runs into it:
>
> ```powershell
> pipenv run python scripts/run_ansi.py <group> --update-checklist
> ```
>
> Do that after every fix. See plan.md's "Keeping the checklist current without a
> full run" for what a merged count does and does not mean.
>
> Trust the `COMPLETENESS:` line, not the "N failures ... out of 22036 tests"
> summary, which prints the initial pending count unconditionally and looks
> complete even when a run died partway.
>
> **The target is zero failures, and there are no expected failures.** RT's
> `expected-failures/` mechanism — which five other implementations ship a
> file for — stays unwired **by policy, not as an open item**: it exists so an
> implementation can decline a feature it does not want to pay for and still
> report a clean run, and this one is a reference implementation. Correctness
> first; a faster Lisp can be forked from it and make those trades explicitly.
> **Where CLHS permits several conforming behaviours and ansi-test asserts
> one, that one is fclpy's implementation-defined choice** — that is how the
> SUBTYPEP lattice was decided rather than declaring `subtypep.member.27`
> unpassable. Adding a `*FEATURES*` keyword to make a test disappear is the
> same evasion in a different hat. See plan.md, "The final compliance gate" and "Ways to fake compliance".

## Environment

- Python 3.10, managed via **pipenv** — always `pipenv run <cmd>`, never bare
  `python`/`pip`. First-time setup: `pipenv install --dev`.
- Shell is PowerShell: chain commands with `;`, not `&&`.
- The real ANSI test suite lives at `../ansi-test/` (sibling of this repo, e.g.
  `C:\Users\Windows\git\fclpy\ansi-test`), not inside `fclpy/`. `run_all_tests.py`
  and `run_do_test.py` resolve it via `../ansi-test` relative to this file.
- **Timing**: loading `init.lsp`/`rt.lsp` (the test harness bootstrap) takes about
  90 seconds by itself, so even a single isolated test via `run_do_test.py` takes
  ~90s+ before it prints a result — don't assume a run has hung just because
  nothing has printed yet; give it at least 2 minutes. The full ANSI suite via
  `run_all_tests.py` takes **about 86 minutes** end to end (measured 2026-08-18;
  113 min at 08-16, ~67 min at 08-15, ~7.5 hours before LOOP got one iteration
  engine). That figure has moved in *both* directions for real reasons — up
  when a LOOP fix made `check-type-error` actually call the function under
  test, down when the printer stopped burning minutes and gigabytes on
  circular structure — so treat it as a measurement, not a constant.
  **`scripts/run_ansi.py` is the development loop, not this:** a single group
  is usually 2–30s, though a few are far
  slower because one form in them burns the 600s LOOP cap. **Do not conclude a
  run has hung from silence alone** — `fclpy/watchdog.py` warns to stderr after
  120s without progress and dumps every thread's traceback, so a genuine wedge
  says so and a slow-but-live run does not.

## Architecture map

- **Reader**: `tokenizer.py` (character-level) → `lispreader.py` (token → form) →
  `readtable.py` (macro characters, `#`-dispatch, case/readtable state). If a crash
  looks like a Python type error on oddly-shaped data, check here first — many
  "evaluator" bugs are actually mis-parsed syntax (dotted pairs, bit-vectors,
  exponent markers, etc).
- **Readtables**: `readtable.py` owns the object model, and there are three
  things in it that every caller must go through. **`standard_readtable()` is
  a distinct, immutable object** (CLHS 23.1.1) and **NIL denotes *it*** —
  not the current readtable — wherever a readtable designator is accepted;
  that is what `(copy-readtable nil)` asks for. **`coerce_to_readtable` is the
  one designator resolver**; the eight copies it replaced in `io_read.py` each
  read `if readtable is None: ... get_current_readtable()`, which resolves an
  *omitted* argument and nothing else, so every one of them raised on NIL. It
  takes an `_OMITTED` sentinel rather than defaulting to `None`, because NIL
  is a meaningful value here and `None` cannot tell "omitted" from "given
  NIL". **`get_current_readtable()` reads the `*READTABLE*` symbol's value
  cell**, which is the variable's one home — it used to read a module global
  while `*READTABLE*` was a separate variable nothing consulted, so
  `(let ((*readtable* rt)) (read ...))` read with the old table (the same
  defect the printer's control variables had). Every reader entry point funnels
  through that function, so change it there and not per-site. Note the internal
  `Readtable.readtable_case()` answers a Python string (`'UPCASE'`) for the
  reader and printer, while the Lisp `READTABLE-CASE` answers the *keyword* —
  `case_keyword`/`case_from_designator` are the only two places that convert.
  **Still absent:** there is no character *syntax type* model, so
  `SET-SYNTAX-FROM-CHAR` is a stub, and `_read_symbol` upcases unconditionally
  rather than consulting `readtable-case`.
- **`WITH-STANDARD-IO-SYNTAX`** is a `cl_macro` in
  `evaluation_special_forms.py` expanding to the `LET` of CLHS 23.4's
  twenty-one bindings — *not* a `cl_function`, which would evaluate its body
  before establishing anything. Two of the values are objects with one home
  each: `readtable.standard_readtable()` and
  `io_write.standard_pprint_dispatch()` (the latter is also what
  `*PRINT-PPRINT-DISPATCH*` starts out holding, so `lispenv` must not build
  its own). Its binding variables are the **interned** `COMMON-LISP` symbols;
  a bare `LispSymbol('*PRINT-BASE*')` in an expansion binds a *different*
  variable from the one the printer reads, because global lookup is by symbol
  identity.
- **Types**: `lisptype_basic.py` (symbols, cons cells, NIL/T, `MultipleValues`) and
  `lisptype_extended.py` (`Environment`, **`Package` — at `:322`, *not* in
  `lisptype_basic.py`**, symbol-macros, condition types). `lisptype.py` re-exports
  both. Note `setf-expanders` is **monkey-patched onto `Environment` at runtime**
  (`evaluation_core.py:1229-1230`) rather than declared in `__init__`.
- **Type specifiers**: `fclpy/typespec.py` — **the one place a type specifier is
  interpreted**, and the type lattice C14 asked for. `parse_type` turns a
  specifier into a `Ctype`; `type_subtypep` decides `SUBTYPEP` as emptiness of
  `type1 \ type2`, and `type_contains` is there for `TYPEP` to move onto (it has
  not yet — see below). It replaced three partial interpretations that could not
  see what the others knew: `TYPEP`'s `elif type_name == ...` ladder, `SUBTYPEP`'s
  table of hardcoded **string pairs**, and `DEFTYPE`'s expander store, which
  *nothing ever read*. They also disagreed on facts — `TYPEP` called an integer a
  FIXNUM below 2**29 while `MOST-POSITIVE-FIXNUM` answered 2**63-1, so
  `(typep most-positive-fixnum 'fixnum)` was false. `MOST_POSITIVE_FIXNUM` here is
  now that constant's one home — **and as of 2026-08-24 that is actually true**,
  which it was not when the sentence was first written. Three copies survived
  the consolidation and were found by an unrelated failure (SXHASH is specified
  to return a fixnum, so every `sxhash` test that checked its result's *type*
  failed on a value that was in range): a local `FIXNUM_MAX = 2**29 - 1` inside
  `comparison.typep`, a `2**63 - 1` literal in `math_arithmetic`'s
  `MOST-POSITIVE-FIXNUM`, and a third in `tests/test_big_integers.py` commented
  "matching the implementation". All three now read this constant. The lesson is
  the one standing rule 3 keeps teaching: *a note saying a constant has one home
  is not the same as it having one*, and the way this pair was caught is that
  TYPEP and SUBTYPEP contradicted each other about the same integer —
  `(typep 1000000000 'bignum)` was T while
  `(subtypep '(integer 0 1000000000) 'fixnum)` was T as well.
  The universe is partitioned into disjoint **sorts** (INTEGER, RATIO, FLOAT,
  COMPLEX, CHARACTER, SYMBOL, CONS, ARRAY, CLASS), each with a representation
  closed under union, intersection **and complement** — which is the requirement,
  because ansi-test's `check-equivalence` asks twelve questions per call and
  demands all twelve be *certain*, including `(subtypep '(and A (not B)) nil)` and
  `(subtypep t '(or A (not B)))`. Two invariants to keep:
  - **`top()`/`bottom()` are functions, not constants.** The CLASS sort's universe
    is the set of classes that *currently* exist, so a universe captured at import
    time would permanently omit every DEFCLASS/DEFINE-CONDITION type. Relatedly,
    the universal type contains the universal *cons* type, so `(cons * *)` holds
    the `ANY` placeholder rather than a materialised universe — resolving it
    eagerly does not terminate.
  - **An undecidable specifier becomes an `Opaque` literal, never a guess.**
    `(satisfies f)` is decided only when what remains of the conjunct is a finite
    set of concrete objects, in which case the predicate is called on each. That
    is not a shortcut: `subtypep.member.27` requires a *certain* T for
    `(member a b c d)` vs `(satisfies symbolp)`, while `subtypep.cons.44` builds a
    type from `(= 1 (random 2))` predicates and requires **NIL NIL**.
  Cell keys must stay **hashable** — a `classes.LispClass` defines `__eq__`
  without `__hash__`, so putting class objects into a cell `frozenset` directly
  made every `(subtypep (find-class 'x) ...)` answer
  `TypeError: unhashable type: 'LispClass'` *as the value of the Lisp form*; go
  through `_cell_key`.
  **`TYPEP` still has its own ladder** (`comparison.py`) and is therefore still a
  second interpretation of a specifier — standing rule 3, not yet resolved. Moving
  it onto `type_contains` needs the CLASS sort to answer "yes" for an object whose
  cell is not enumerated, or `(typep x t)` could go NIL; see plan.md.
- **Evaluator** (`lispfunc/`):
  - `evaluation_core.py` — the `eval`/`apply` dispatcher and the control-transfer
    exceptions (`ReturnFromException`, `ThrowException`, `GoException`,
    `ConditionException`). This is where dispatch order and argument-passing
    conventions live.
  - `evaluation_special_forms.py` — actual semantics for special forms (`LET`,
    `DEFSTRUCT`, `DEFSETF`, `MACROLET`, etc). **Edit here (and evaluation_core.py /
    evaluation_loops_conditionals.py) to change behavior.**
  - `evaluation_special_registrations.py` — just registers names as special forms
    so they're bound; every handler here raises `LispNotImplementedError` by
    design. It exists so the registry knows the symbol is a special operator, not
    a function. Adding a new special form to the language means registering it
    here *and* implementing it in `evaluation_core.py`/`evaluation_special_forms.py`.
  - `binding.py` — **the variable-binding model: the one place that decides
    lexical vs. dynamic.** `BindingFrame` is used by `LET`, `LET*` and all eight
    iteration forms (`DO`, `DO*`, `DOLIST`, `DOTIMES`, `LOOP`, `DO-SYMBOLS`,
    `DO-EXTERNAL-SYMBOLS`, `DO-ALL-SYMBOLS`). Establish a binding with
    `frame.bind(var, value)` and call it again to step it — the first call
    decides where the binding lives, later calls assign to that same binding.
    **Never use `Environment.set_variable` to establish a binding**: it walks
    the environment chain and mutates the first binding of that name it finds,
    which is how every iteration form used to assign to an enclosing variable
    instead of binding its own. A local `(declare (special x))` is *not* the
    same as a `DEFVAR` proclamation — only the latter makes a nested binding
    form bind dynamically, and `DOTIMES.17` vs `.18` is exactly that
    distinction, so `is_proclaimed_special` consults the root environment only.
  - `evaluation_loops_conditionals.py` — `LET`/`LET*`/`DO`/`DOLIST`/`DOTIMES`/`LOOP`/`COND`.
    **`LOOP` has exactly one iteration engine.** Every iteration-control clause
    (`FOR`/`AS`, and `REPEAT`) becomes a driver in `iteration_drivers`, and the
    loop runs while `all(_driver_has_value(...))` — CLHS 6.1.2's rule that these
    clauses *compose*. It previously had nine near-duplicate engines selected by
    a scalar `iteration_type`, so the last clause parsed silently discarded the
    rest. If you are about to add an `if kind == ...` branch to the four driver
    primitives, that is the right place; if you are about to add a second loop,
    it is not. **The clause-level helpers are module-level and shared on
    purpose** — `_loop_type_spec` (the optional type-spec, in all three
    positions it can occupy), `_loop_destructure` (every var-spec pattern, for
    WITH, every driver and USING) and `_loop_type_default`. A second copy of
    any of them is the same defect these replaced: the partial copy handles the
    shapes its author had in mind and silently mis-parses the rest.
    **An unrecognized clause keyword is still dropped silently** once a driver
    exists — the last such path, deliberately left loud-able as its own
    measured change (plan.md §5).
  - `evaluation_conditions.py` — `HANDLER-BIND`/`HANDLER-CASE`/`IGNORE-ERRORS`,
    `SIGNAL`/`ERROR`/`CERROR`/`WARN`, condition signaling. **Handlers are invoked
    in exactly one place: `signal_condition()`, which walks
    `state.handler_stack` at the signal point, before any unwinding.** The
    establishing forms catch nothing; they push a handler cluster for the extent
    of their body. If you are tempted to add a `try/except` that runs a handler,
    that is the bug this replaced — an `except` clause runs after the protected
    form's `CATCH`/`RESTART-CASE`/`UNWIND-PROTECT` frames are already gone.
    Condition *construction* is likewise one function, `build_condition`, whose
    only per-operator parameter is the default condition type; condition *type
    matching* delegates to `TYPEP`. `_run_handlers_on_unwind` is a transitional
    path for raise sites that bypass signaling — see plan.md's 2026-08-12 update.
  - `evaluation_control_flow.py` — `BLOCK`/`RETURN-FROM`/`CATCH`/`THROW`/`TAGBODY`/`GO`.
  - `registry.py` — `@cl_function`/`@cl_special`/`@cl_macro` decorators and
    `register_module()` auto-registration. A form registered as `cl_function`
    gets its arguments evaluated eagerly before the call; a form that needs
    unevaluated arguments (macros, `DEFSETF`, `DEFPACKAGE`, ...) **must** be
    `cl_special` or `cl_macro` instead, or its arguments will be evaluated too
    early and it will crash or silently misbehave. **`cl_special` means
    "unevaluated arguments", not "CLHS special operator"** — the two are
    independent, and `SPECIAL-OPERATOR-P` answers from CLHS 3.1.2.1.2.1's
    fixed twenty-five, not from `special_registry`.
  - **`standard_macros.py` — every CLHS macro is a real macro** (milestone M4,
    completed 2026-08-30: all **87** operators the CLHS dictionary types as
    *Macro*, plus 3 of the 5 *Local Macro*s). This is what makes
    `(macro-function 'x)` non-NIL and `MACROEXPAND`/`MACROEXPAND-1` work on
    standard operators, and it is not cosmetic — ansi-test reaches for the
    macro function directly (`loop-finish.error.1` FUNCALLs
    `(macro-function 'loop-finish env)` at three wrong arities and requires a
    PROGRAM-ERROR from each; the `destructuring-bind.error.*` family does the
    same). `_standard_macro(name)` is the one registration path: its wrapper
    enforces the two-argument `(whole-form, environment)` macro-function shape,
    which is where those PROGRAM-ERRORs come from, so a hand-rolled
    `cl_macro` registration that skips it will fail those tests.
    Two shapes live here, and the distinction matters:
    - **A real expansion** — `WHEN`, `COND`, `PUSH`, `ROTATEF`, `DO-SYMBOLS`,
      … build and return a form. Prefer this.
    - **`_reuse_definer(worker, module)`** — expands to a *pure* deferred
      form, `(%FCLPY-DEFERRED-EXPANSION "module" "worker" '<form>)`, whose
      runtime (the ladder branch of the same name in `evaluation_core.py`)
      invokes the existing `eval_xxx(form, env)` worker at **evaluation**
      time with the evaluation-time env. Used where rebuilding the expansion
      would duplicate a mechanism that already has exactly one home (LOOP's
      single iteration engine, the condition system's `:no-error`/
      in-transit-transfer handling, DEFSTRUCT's BOA lambda lists,
      DEFPACKAGE's literal option clauses). Earlier these expanded by
      *running* the worker and quoting its result, so a bare `MACROEXPAND-1`
      executed the program (plan.md finding 12: RESTART-CASE evaluated its
      protected form twice); the deferral removed that whole defect class at
      the factory — macroexpansion of the family is now side-effect-free and
      `macro_expansion_evaluates` answers False for all of it.
    **SETF and PSETF are real expansions (M5, 2026-08-30)**, and the place
    protocol is now one mechanism: `_setf_expander`/`_psetf_expander` in
    `standard_macros.py` resolve every target through `_place_full` —
    `get_setf_expansion` — and expand to
    `(let* ((temps vals...) (store value)) store-form)`. The old
    ~540-line per-operator ladder (`evaluation_core.eval_setf`) that
    bypassed the protocol is deleted, along with the dead
    `eval_incf/decf/push/pop/pushnew/remf/rotatef/shiftf/psetq` workers
    whose macro expanders had replaced them. INCF/DECF/PUSH/PUSHNEW/POP/
    REMF/ROTATEF/SHIFTF/PSETQ/MULTIPLE-VALUE-SETQ expand through the *same*
    `_place_full`, so a place kind is implemented once in
    `get_setf_expansion` (the form face, what GET-SETF-EXPANSION answers)
    and works for all ten operators. `_place_accessor` is the closure face
    of the same protocol — SETQ/PSETQ/MULTIPLE-VALUE-SETQ's symbol-macro
    path and the conditions system's STORE-VALUE restarts use it — and both
    faces share the `%FCLPY-SETF-*` runtimes and the same arithmetic
    (LDB/MASK-FIELD go through the real DPB/DEPOSIT-FIELD; SUBSEQ's write
    is one function used by both). **The one rule that keeps the faces from
    drifting apart: fix a place kind in `get_setf_expansion`, never by
    adding a branch to a caller.**
  - **A builtin's ANSI lambda list is its Python signature**, read by
    `evaluation_core.LambdaListShape` and enforced by `split_keyword_args` —
    the one place CLHS 3.4.1.4/3.5.1.5 is applied, for direct calls, FUNCALL
    and APPLY alike. The mapping is exact and **you must use the whole of
    Python's parameter model for it to be**: required = positional without a
    default, `&optional` = positional-*or-keyword* **with** a default, `&rest`
    = `*args`, **`&key` = keyword-only** (`def union(l1, l2, *, test=None...)`),
    `&allow-other-keys` = `**kwargs`. Writing an ANSI `&key` parameter as a
    plain defaulted positional makes it indistinguishable from `&optional`, and
    then the standard's checks are undecidable: that is why
    `(union nil nil :bad t)` returned an answer instead of a PROGRAM-ERROR,
    while `(intern "a" :cl-test)` must keep passing :CL-TEST as `package`'s
    *value*. Builtins whose `&key` set is still inferred fall back to
    `_split_inferred_keywords`; the families are being migrated cluster by
    cluster (plan.md §5), so **when you touch a builtin, spell its `&key`
    parameters keyword-only**.
  - **A *user* function's ordinary lambda list has one constructor**,
    `evaluation_special_forms.make_ordinary_function`, and LAMBDA, DEFUN, FLET
    and LABELS all go through it (DEFMETHOD shares its binder,
    `_bind_ordinary_lambda_list_tail`). There used to be three: LAMBDA located
    the keyword region by *scanning the arguments* for the first
    keyword-shaped value, so `&rest` never saw the keyword arguments and a
    repeated keyword took the rightmost value; FLET/LABELS had a hand-rolled
    parser that did not call `parse_lambda_list` at all and dropped every
    supplied-p variable, `&aux` and `&allow-other-keys` on the floor; and none
    of the three signalled a PROGRAM-ERROR for a wrong argument count, an odd
    keyword list or an unrecognized keyword. Three things it owns:
    **arity checking** (`_check_ordinary_arity` — a missing required argument
    used to be padded with NIL and a surplus one discarded, which are wrong
    *values*, not just missing errors); **`BindingFrame`**, so a parameter the
    body declares SPECIAL binds the value cell and is undone on exit, with
    free declarations installed only *after* the parameters are bound because
    CLHS 3.3.4 excludes init forms from their scope; and the fact that **the
    implicit block encloses the body only**, so a `RETURN-FROM` in an `&aux`
    init form leaves the function rather than returning from it.
    A `&key` parameter and an actual argument match on
    `keyword_argument_key` — *(package, name)*, not name — because `&key b`
    declares `:B` while `((b var) ...)` declares whatever symbol was written,
    and comparing names alone let `((lambda (&key b) b) 'b 100)` bind B.
- **Packages**: `lispfunc/misc_packages.py` — `coerce_to_package` (the package
  *designator* rule, CLHS 11.1.1.1) and `package_symbols(pkg, kind)` for the
  accessible / present / external symbol sets. `DO-SYMBOLS`,
  `DO-EXTERNAL-SYMBOLS` and LOOP's `for x being the symbols of p` all go through
  them; the copies they replaced disagreed, because `Package.use_packages` holds
  package **names** as well as `Package` objects and a copy that read
  `external_symbols` off a string silently dropped every inherited symbol.
- **Hash tables**: `lispfunc/misc_hashtables.py` — **the one hash-table object
  model**, and the one home of every CLHS 18.2 operator. A hash table *is* its
  test: two keys denote the same entry exactly when the table's test says they
  are equivalent, and that is the property the module exists to hold.
  `MAKE-HASH-TABLE` returns a `LispHashTable`, which is deliberately **not** a
  `dict` subclass — being one is what let the previous implementation compare
  keys with Python's `__eq__`/`__hash__` while its `test` attribute went
  unread, so `:test 'equal` could not find a list key it had just stored,
  `:test 'eql` matched two `equal` strings, and `(gethash 1 h)` found the value
  stored under `1.0`. Ask `is_hash_table` rather than testing `isinstance`
  (`HASH-TABLE-P`, `TYPEP`, `typespec`'s class cell and the printer all do, so
  they cannot disagree — they did: `HASH-TABLE-P` answered NIL for the very
  object `MAKE-HASH-TABLE` returns while `TYPEP` answered T). Three invariants:
  - **A key is bucketed by a coarse surrogate, then compared with the canonical
    predicate from `comparison.py`.** The equivalence relation a table
    implements is therefore the Lisp predicate *by construction* — there is no
    second copy of EQL to drift from it. `hashtables.py`'s `HashTable` was that
    second copy (its own `_compare_keys` ladder over Python `==`) and is gone.
  - **When in doubt, collide.** A surrogate must satisfy only "equivalent keys
    share a bucket", so making it coarser is always safe; a *missed* collision
    is a wrong answer. That is why a `Character` and a one-character Python
    `str` hash alike (`comparison.eql` crosses them) and why anything
    undecidable hashes by identity.
  - **`SXHASH` is the EQUAL surrogate**, not a function beside it — CLHS
    18.2.2's "(equal x y) implies (= (sxhash x) (sxhash y))" is what makes an
    EQUAL table work at all, so the two are the same function. It follows that
    a symbol hashes by *name* only, a general array by identity (EQUAL does not
    descend into one), and structural hashing is depth-bounded because a key
    may be circular.
  **`puthash` is the one place an entry is written.** There were four, each
  doing `table[key] = value` on the raw dict — `evaluation_core`'s SETF ladder,
  `_fclpy_setf_gethash`, `get_setf_expansion`'s GETHASH branch and
  `evaluation_special_forms`' getter/setter pair — so all four bypassed the
  table's test even once the test existed. `entries()` is likewise the one
  traversal, and it is a *snapshot* because CLHS 18.2 lets MAPHASH's and
  WITH-HASH-TABLE-ITERATOR's bodies remove entries while traversing.
  `WITH-HASH-TABLE-ITERATOR` is a `cl_macro` expanding to a `MACROLET` (it
  defines a local *macro*, not a function); `%MAKE-HASH-TABLE-ITERATOR` and
  `%HASH-TABLE-ITERATOR-NEXT` are its runtime. **`HASH-TABLE-SIZE` is a
  capacity, not the count** — it was an alias for `HASH-TABLE-COUNT` — and
  `_grow_if_needed` takes its growth *target* from the count rather than from
  the threshold, because `:rehash-threshold 0` and
  `:rehash-threshold least-positive-short-float` are both legal and neither
  terminates if the threshold sets the target.
- **Sequences**: `lispfunc/sequence_protocol.py` — **the one place that answers
  both halves of CLHS 17.1**: `seq_elements` (what are the elements of this Lisp
  sequence — `lispCons`, Python `list`/`tuple` vector, `LispString`, `str`,
  `LispArray`), and the constructors, `rebuild_like` (a result of the
  argument's own type, for REMOVE/SORT/REVERSE/SUBSEQ/…), `build_sequence`
  (a result of the type a `result-type` designator names, for
  MAP/CONCATENATE/MERGE/MAKE-SEQUENCE/COERCE), plus `bounding_indices`
  (`:start`/`:end`, NIL included) and `seq_set` (the destructive operators).
  `sequences_search.py` / `_modify.py` / `_compose.py` / `_higher.py` and
  `utilities_functions.COERCE` all go through it. **A Python `list` is a
  *vector* here, not a list** — a Lisp list is a `lispCons` chain, NIL when
  empty, and a string is a `LispString`. That confusion is what the protocol
  exists to prevent: every one of those modules used to build a Python list and
  return it, so `(union '(1 2) '(2 3))` and `(sort (list 3 1 2) #'<)` answered a
  vector that printed convincingly as a list. If you are about to write
  `isinstance(x, list)` to mean "is a Lisp list", or to build a result with
  `[...]`, that is the defect (plan.md Finding M).
- **List traversal**: the same module owns **`list_cells`, the one primitive
  that walks a Lisp list** (with `list_elements` and `list_tail` over it, and
  `core._check_list` — re-exported as `check_list` — as its entry check).
  Before it there was no such primitive, so ~30 CLHS 14.2 operators each walked
  a chain with their own `while isinstance(cur, lispCons)` and none of them
  checked what they were walking. Three properties to keep:
  - **A dotted list's terminator is never an element.** `seq_elements` used to
    append it as one "so callers can detect it", which no caller did — so
    `(append '(a . b) '(z))` answered `(A B Z)` and
    `(pairlis '(a . b) '(c . d))` paired B with D. Those are wrong *values*,
    not missing errors.
  - **`dotted='error'` vs `dotted='allow'` is the CLHS distinction, not a
    convenience.** A LIST argument (MEMBER, the set operations, the MAP*
    family, PAIRLIS, LIST-LENGTH, APPEND's non-final arguments) requires a
    *proper* list; LAST, BUTLAST/NBUTLAST, NTHCDR, LDIFF, TAILP and NCONC's
    non-final arguments are *defined* on a dotted one because they count
    conses.
  - **Traversal is lazy, and that is semantic.** `(nthcdr 1 (cons 'a 'b))` is
    `B` while `(nthcdr 3 (cons 'a 'b))` is a TYPE-ERROR, and
    `(member 'a '(a . b))` returns before the terminator is reached. The
    not-a-list check is raised eagerly, though, or an operator that abandons
    its walker early never reaches it.
  Note `seq_elements` accepts a *vector*, so it cannot express "this argument
  must be a list" — that is why the CLHS 14.2 operators call `list_elements`
  and the CLHS 17 ones call `seq_elements`. Choosing the wrong one makes
  `(mapcar #'identity "ab")` answer instead of signalling.
- **Arrays**: `lispfunc/arrays.py` — **the one array object model**, and the one
  home of every array operator. CLHS 15.1 gives an array five properties
  (dimensions, element type, adjustability, fill pointer, displacement) and
  this module owns all five. There are **three representations, one protocol**:
  a Python `list` is a *simple general vector*, a `LispString` is a character
  vector, and `LispArray` is everything else — any other rank, any specialized
  element type, any fill pointer, adjustability or displacement. `_new_array`
  is the only place that decides which. Ask `array_rank_of` /
  `array_dimensions_of` / `element_type_of` / `fill_pointer_of` /
  `row_major_get` rather than testing `isinstance`, or you will be right about
  one representation and wrong about the other two. `TYPEP`'s array
  specifiers, the printer's `#*`/`#2A`, the reader's `#*`/`#nA` and
  `sequence_protocol`'s bit-vector results all go through it. It replaced
  `vectors.py`'s `AdjustableVector`/`Array` **plus** competing copies of the
  same operators in `sequences_higher.py`, `misc_hashtables.py`,
  `math_arithmetic.py` and `core.py`, where import order decided which ran.
  **`ADJUSTABLE-ARRAY-P`, `FILL-POINTER` and friends signal for a non-array
  argument** — answering NIL conflated "no fill pointer" with "not an array".
- **CLOS**: `fclpy/classes.py` is the object model (`LispClass`, `LispInstance`,
  `Method`, `GenericFunction`) and the dispatcher; `lispfunc/misc_clos.py`
  registers the operators and the MOP generics' default methods;
  `evaluation_special_forms.py` owns `DEFCLASS`/`DEFGENERIC`/`DEFMETHOD`/
  `DEFINE-METHOD-COMBINATION`, which are special operators because none of
  them may have their subforms evaluated. Three things in here have exactly
  one home and must keep it:
  - **`classes.call_method` is the only place a method is ever invoked.** It
    pushes the frame `CALL-NEXT-METHOD`/`NEXT-METHOD-P` read — `{'args',
    'next', 'gf'}`, where `next` is the chain reachable from that method and
    empty when there is none. There is no second frame *kind*: the `'around'`
    variant that used to carry its own `core` closure is why `NEXT-METHOD-P`
    answered T inside every `:around` method whether or not anything remained.
  - **A generic function's `method_combination` decides how its applicable
    methods become an effective method (CLHS 7.6.6)**, and `None` means
    *standard*, not "none". `StandardMethodCombination` assembles the chain in
    Python because its shape is fixed; `ShortFormMethodCombination` (the nine
    built-ins of CLHS 7.6.6.4 and `DEFINE-METHOD-COMBINATION`'s short form) and
    `LongFormMethodCombination` build a Lisp **form** out of `CALL-METHOD`
    instead, because the operator being combined with may be a macro whose
    evaluation order is the semantics — `(and (call-method m1) (call-method
    m2))` must stop at the first NIL, and ansi-test observes exactly that.
    Folding the method results in Python gives the right answer for `PROGN`
    and the wrong one for `AND`/`OR`. New combination types go in the registry
    (`register_method_combination_type`); they do not go in
    `call_generic_function`.
  - **`CALL-METHOD` and `MAKE-METHOD` take their arguments unevaluated**, and
    are registered `cl_special` to get that: their operands are read
    structurally — a method object the combination spliced in, and an
    unevaluated body form. They were `cl_function`s that evaluated both
    operands and discarded the next-method list — the registry defect
    described above, in the one place where it makes CALL-NEXT-METHOD
    structurally impossible. **`cl_special` is the registry's
    unevaluated-arguments mechanism, not a claim that the operator is one
    of CLHS 3.1.2.1.2.1's twenty-five special operators** — these two are
    *local macros* (CLHS 7.6.6.2), valid only inside a
    `DEFINE-METHOD-COMBINATION` effective-method form, and
    `SPECIAL-OPERATOR-P` correctly answers NIL for both. They are the
    only two CLHS macros of any kind that are not registered as real
    macros here (M4), and deliberately so: a *global* macro definition
    would make them expandable outside the one context CLHS defines them
    in. Do not "fix" this by converting them without first giving
    `DEFINE-METHOD-COMBINATION` a real local-macro environment.
  Still absent: a real class precedence list. `_specificity_key` orders by
  *ancestor count*, and `_init_builtin_classes` makes every built-in class a
  direct subclass of `T`, so `INTEGER`, `RATIONAL` and `NUMBER` are all
  equally specific and only the (stable) definition order separates them.
  Note also that `classes.py` still defines `_init_builtin_classes` **twice**;
  the second definition wins and the first is dead (standing rule 3).
- **Pathnames and file operations**: `lispfunc/pathnames.py` owns
  **`resolve_filespec`, the one place a pathname designator becomes an OS
  path** — including CLHS's third designator case, "a stream associated with a
  file", so `(compile-file s)` and `:output-file s` work. It replaced five
  copies of the same relative-name search (LOAD, COMPILE-FILE,
  COMPILE-FILE-PATHNAME, DELETE-FILE, OPEN), which had already drifted: two of
  them looked `*DEFAULT-PATHNAME-DEFAULTS*` up in *different packages* (and
  lookup is by symbol identity, so those are two variables), and OPEN's took
  the `LISP_CWD` candidate unconditionally where the others took it only if it
  existed — so OPEN and PROBE-FILE could resolve one relative name to two
  different files. It always returns a path, existing or not, so a caller can
  *name* a missing file. **`Pathname` is still a namestring wrapper, not a
  component record**, so `MAKE-PATHNAME`/`MERGE-PATHNAMES`/`DIRECTORY` cannot
  compose components: `(directory (make-pathname :version :wild :defaults p))`
  answers no files, which is what gates most of `files/` and `pathnames/`
  (plan.md C11).
- **`LOAD` and `COMPILE-FILE`** live together in `lispfunc/misc_macros.py`
  because they are the same operation read from two ends — both read a file
  form by form with `*PACKAGE*` and `*READTABLE*` bound through
  `BindingFrame`, and COMPILE-FILE's output is what LOAD then reads. Three
  things here are load-bearing. **A form is read one at a time through READ**,
  not through one reader built at the top, because READ consults `*READTABLE*`
  and `*PACKAGE*` per call and a form in the file that assigns either governs
  how the *rest of the file is read*. **COMPILE-FILE must not run the
  program**: it evaluates only what CLHS 3.2.3.1 requires
  (`COMPILE_TIME_OPERATORS`, and `EVAL-WHEN`'s `:compile-toplevel`, recursing
  through PROGN/LOCALLY), which is what `(not (fboundp funname))` after a
  compile checks. And it **prints** the forms it read rather than copying
  bytes, because `#.` is *read*-time evaluation and must be resolved while
  `*COMPILE-FILE-TRUENAME*` is bound; the printer controls are pinned
  (`OUTPUT_PRINTER_CONTROLS`) while writing, since a caller's `*print-length*`
  would otherwise truncate the output to `...` — a corrupt file reported as a
  successful compilation.
- **A file operation reports failure through
  `evaluation_conditions.signal_file_error`**, which is the one place a
  FILE-ERROR is built and signalled. `lisptype.FileError` carries the PATHNAME
  slot CLHS gives it, and `FILE-ERROR-PATHNAME` returns that slot
  **unchanged** — not coerced to a pathname, because the suite passes
  namestrings, pathnames and streams as `:pathname` and requires each back
  out. Before this, LOAD/OPEN/DELETE-FILE/TRUENAME let Python's
  `FileNotFoundError`/`FileExistsError` escape, and a Python exception is not
  a condition: it matches no handler clause, so it surfaced as the *value* of
  the form.
- **Output to a fill-pointered string** is `streams.FillPointerOutputStream`,
  which `(WITH-OUTPUT-TO-STRING (var string) ...)` expands to via
  `%MAKE-FILL-POINTER-OUTPUT-STREAM`. The macro used to bind `var` to a fresh
  `MAKE-STRING-OUTPUT-STREAM` and transfer its contents nowhere, which is a
  **measurement gate**: the ANSI suite captures an operator's output with
  exactly this form and then asserts about it, so every test of what something
  *prints* compared against the empty string.
- **`*MODULES*` / PROVIDE / REQUIRE** (CLHS 24.1.5) are in
  `lispfunc/misc_macros.py`. A module name is a *string designator*, resolved
  through `misc_packages._designator_to_string` — the existing single resolver,
  not a fourth copy.
- **State**: `state.py` holds the few intentional cross-module globals
  (`packages`, `current_package`, `current_environment`, `restart_stack`,
  `handler_stack`). Don't add new ad-hoc globals elsewhere — put them here.
- **Environment bootstrap**: `lispenv.py` — `setup_standard_environment()` builds
  the initial global environment from the registries above.

## The development loop

**Never start with `run_all_tests.py`.** It is ~86 minutes, it moves the
official scoreboard and nothing else, and it is not how a fix is verified. The
loop is `scripts/run_ansi.py`, which loads only the harness plus the files you
name and is usually 2–30 seconds:

1. **Pick a file from `docs/ansi_checklist.md`.** At the current failure
   distribution that is the right unit — see the status note above. Cheapest
   first is fine; the 20 files still failing 100% are the cheapest of all.
2. **Reproduce it in the smallest expression that shows the defect**, not by
   running the test. A one-liner through `eval_string` is seconds:
   ```powershell
   pipenv run python -c "import sys; sys.path.insert(0,'.'); from fclpy import lispenv; from fclpy.lispfunc import eval_string; lispenv.setup_standard_environment(); print(eval_string('(flet ((f (&key (a 1 p)) (list a p))) (f))'))"
   ```
3. **Fix the mechanism, not the test.** Consolidate onto an existing shared
   helper if one exists — the architecture map above lists which module owns
   what, and "there are two of these" is the single most common root cause left.
4. **Verify with the targeted command the checklist prints next to that file**,
   and fold the result back in the same step:
   ```powershell
   pipenv run python scripts/run_ansi.py <dir>/<file>.lsp --update-checklist
   ```
5. **`pipenv run python scripts/gate.py`** (~50s) — the three cheap checks in
   one, exiting non-zero if any fails: `pytest -q` for unit regressions,
   `duplicates.py --baseline` for standing rule 3, and
   `ansi_checklist.py --baseline` for per-file ANSI regressions. **A file that
   got worse is a failure even when the total improved** — a total can hide a
   mechanism trade where one fix breaks another subsystem.
   The duplicates check is the only automatic guard on the defect class that
   has cost this project the most: `registry.cl_function` ends in
   `function_registry[name] = entry`, so a second implementation of an
   operator wins or loses on *import order*, silently. There are 22 such
   operators today — deleting one side of a pair is some of the cheapest
   remaining work in the suite (plan.md §2, "The duplicate register").
   **Never clear a gate failure with `--save-baseline`.** Both baselines are
   committed so that changing one shows up in `git diff` as the reviewable
   event it is; see plan.md, "Ways to fake compliance".
6. **Re-run the directories the change could plausibly reach.** A change to a
   binder, the printer, the reader or the type lattice reaches nearly
   everything: run several directories, not just the one you targeted. A fix
   that moves files you did not target is a mechanism fix; a fix that moves
   only the one you aimed at is a symptom fix, and worth a second look.
7. `pipenv run python scripts/ansi_checklist.py --baseline docs/ansi_checklist_baseline.json`
   to classify it — every file you did not touch must show no `+N REGRESSION`.
8. **Run the full suite when it is required — and it sometimes is.** It is not
   the inner loop and must never be used as one, but "targeted runs passed" is
   *not* full-suite verification, and there are three cases where only a full
   run can tell you anything:

   - **Mandatory** after touching any operator on the ansi-test bootstrap
     path — `APPEND`, `DIRECTORY`, `MAPC`, `MAKE-PATHNAME`,
     `COMPILE-FILE-PATHNAME`, `LOAD`, `COMPILE-FILE`, `DELETE-FILE`, `OPEN`,
     or the reader/printer paths they use. `scripts/run_ansi.py` starts at
     `gclload1.lsp` and **never loads `init.lsp`**, so a defect here reports
     as "0 tests ran", not as a failing test. See
     ["The one thing a targeted run cannot verify"](#the-one-thing-a-targeted-run-cannot-verify)
     below, and run the `(listp (directory "*.lsp"))` probe there *first*.
   - **Mandatory** before moving the official scoreboard in plan.md §1, before
     `--save-baseline`, and at the final compliance gate. An amended checklist
     count is an **index, not a scoreboard**.
   - Worth it after a change with wide blast radius (a binder, the printer,
     the reader, the type lattice) even when step 6 looked clean.

   Otherwise, don't. It is ~86 minutes.

### Rules for this loop
1. One mechanism at a time — fix it, verify it, move on. Don't batch unrelated
   fixes; a combined diff makes an untargeted improvement unattributable.
2. No refactoring beyond what the fix requires — *except* deleting a duplicate
   implementation of the thing you are fixing, which is always in scope.
3. Never leave debug `print()`/diagnostic code in a fix.
4. Never commit automatically or with failing tests — commits are the user's call.

### If the suite stops completing

Crashes have not been the constraint since 2026-08-12, but if a full run dies
partway, **REPAIR.md** is the step-by-step SOP for that case: find the crashing
test (the one *after* the last name printed in `run_all_tests.log`, confirmed
by the traceback in `run_all_tests.err`, with `doit.log` to disambiguate
execution order), isolate it with `run_do_test.py`, fix the root cause, and
re-run. Prefer reader fixes over evaluator hacks when the input syntax is the
real problem.

### The one thing a targeted run cannot verify

`scripts/run_ansi.py` starts at `gclload1.lsp`, so **nothing in the normal
development loop ever loads `ansi-test/init.lsp`** — and `init.lsp` opens with
`(mapc #'delete-file (append (directory ...) (directory ...) (directory ...)))`.
A defect in APPEND, DIRECTORY, MAPC, MAKE-PATHNAME or COMPILE-FILE-PATHNAME
therefore breaks the harness bootstrap in a way that only the ~86 minute full
run reports, and it reports it as "0 tests ran", not as a failing test. This
cost two full runs on 2026-08-18: `DIRECTORY` was returning a Python `list`,
i.e. a *vector*, so the `APPEND` over it signalled once APPEND began requiring
lists. If you touch any of those operators, evaluate that form first:

```powershell
pipenv run python -c "import sys; sys.path.insert(0,'.'); from fclpy import lispenv; from fclpy.lispfunc import eval_string; lispenv.setup_standard_environment(); print(eval_string('(listp (directory \"*.lsp\"))'))"
```

## Secondary checks

- `pipenv run python scripts/gate.py` — **the three cheap checks together**,
  non-zero exit if any fails. This is what to run after a repair;
  `--skip-pytest` drops it under a second. It is not a substitute for a full
  run: see step 8 of the loop for when a full run is mandatory.
- `pipenv run pytest -q` — the `tests/` unit-test suite (fast regression net for
  individual functions/forms; not the same thing as the ANSI conformance run).
- `pipenv run python scripts/coverage.py` — compares `docs/ansi_targets.txt`
  against the live function/special registries to report symbol coverage.
- `pipenv run python scripts/duplicates.py` — every operator registered from
  two modules, and every module-level name defined twice in one file. Static
  (no import), so it also sees a *dead* module that still competes for a name,
  which is the case that costs the most. `--baseline` gates against
  `docs/duplicates_baseline.json` (the known debt) and exits 1 on a new one.

## Architectural gotchas learned from prior repairs

- `*PACKAGE*` is a dynamic special variable but its value is mirrored in
  `state.current_package`. Anything that binds it (`LET`, `LET*`, `IN-PACKAGE`)
  must update both or symbol interning silently goes to the wrong package. For
  binding forms this now lives in one place, `BindingFrame._mirror_package`.
  **Read it only through `state.current_package_value()`**, the one resolver:
  it consults the variable first (environment chain, then value cell) and the
  mirror last. The mirror is written only when a *binding form* binds
  `*PACKAGE*`, so a plain `(setq *package* ...)` — which is what a loaded file
  does — changes the variable and leaves the mirror stale. Four readers used to
  consult the mirror alone (`readtable._read_symbol`,
  `lispreader._read_symbol`, `reader.LispReader.__init__`,
  `utilities_symbols.get_current_package`) and therefore interned into the old
  package after any such SETQ.
- **`&rest` gets *all* the remaining arguments, `&key` parameters included**,
  and the keyword region starts immediately after the required and `&optional`
  parameters — a property of the lambda list, never inferred from what the
  arguments look like. `evaluation_special_forms._bind_keyword_parameters`
  applies CLHS 3.4.1.4/3.5.1.5 to a *user* lambda list the way
  `evaluation_core._split_declared_keywords` applies it to a builtin's Python
  signature; the two must agree. The binder used to locate the keyword region
  by scanning for the first keyword-shaped value, so
  `(defun g (a &rest args) ...)` called as `(g 1 :b 2)` bound ARGS to **NIL**
  and the `&rest args &key ...` forward-my-arguments idiom silently forwarded
  nothing. `_keyword_param_parts` is the one place a `&key` spec is
  decomposed, because the keyword a parameter answers to and the variable it
  binds are not always the same name (`((:x y) 9)`).
- **`lisptype.is_symbol` / `is_keyword` are the symbol predicates**, shared by
  `SYMBOLP`/`KEYWORDP` and TYPEP's SYMBOL/KEYWORD branches. A symbol is a
  `LispSymbol`, a `lispKeyword` (KEYWORD is a *subtype* of SYMBOL), or NIL in
  any of its three Python spellings. `SYMBOLP` used to be
  `type(obj) is LispSymbol`, an exact test, so `(symbolp :foo)` and
  `(symbolp nil)` were NIL while TYPEP said T for both.
- **`lisptype.OMITTED` is the one "argument not supplied" sentinel.** A `=None`
  default cannot express it wherever NIL is itself meaningful, which in Common
  Lisp is usually: `(load f :if-does-not-exist nil)` must return NIL while
  `(load f)` must signal, and `(load f :verbose nil)` must override
  `*LOAD-VERBOSE*` where an omitted `:verbose` defers to it.
- **A `LispString`'s content stops at its fill pointer.** `__len__`,
  `__iter__`, `__str__` and `__repr__` all go through `_active()`. The first
  two honoured the fill pointer and the last two did not, so one object
  reported two different contents and every Python reader that goes through
  `str()` saw the inactive characters.
- **A copied readtable is a readtable in its own right.** `Readtable.copy()`
  and `copy_into` rebind the table's own reader *methods* onto the target
  (`_rebind`), because those methods read sub-expressions through
  `self._read_item` — i.e. through the macro characters of the table they are
  bound to. Copying the dict alone gave the copy readers that still consulted
  the original, so `(copy-readtable nil)` plus `set-macro-character` — the
  standard idiom — worked at top level and was invisible inside any list.
- **An uncaught THROW is a CONTROL-ERROR, not a Python exception** (CLHS 5.2).
  `eval_catch` pushes its tag on `state.catch_tags` for its body's dynamic
  extent and `eval_throw` checks it; `_tags_match` is the one place the
  comparison is written. Without this a throw with no catcher left the
  evaluator as a bare `ThrowException`, matching no handler and aborting
  whatever was running the code rather than failing it.
- **`Environment.unbind_function` is the one place a function binding is
  removed.** A definition lives in two structures — the `function_bindings`
  list and the `_function_map` name cache `find_func` reads *first* — so a
  removal that forgets the cache removes nothing observable. That was
  FMAKUNBOUND, and it failed sixteen `system-construction` tests that had
  nothing to do with function cells, because `compile-file-test` and
  `load-file-test` both open with `(fmakunbound funname)` and then assert the
  function is not defined.
- **`binding.dynamic_value` / `set_dynamic_value`** are how Python-side code
  reads and assigns a dynamic variable: environment chain first, then the
  symbol's value cell, matching `eval`'s own order for a variable reference.
  Every builtin that consulted a control variable used to write those four
  lines itself.
- **The global environment has no lexical variables** (CLHS 3.1.1.1), and that
  is now enforced: `Environment.is_global` is true for the parentless
  environment at the root of every chain, and its `add_variable`/
  `find_variable`/`has_variable`/`set_variable` read and write the symbol's
  **value cell** — the same cell `SYMBOL-VALUE`/`BOUNDP`/`SET`/`MAKUNBOUND`/
  `PROGV` and every dynamic binding use. A global variable therefore has
  exactly one home. It used to have two: `DEFVAR`/`DEFPARAMETER` and the
  bootstrap wrote a *lexical* binding in the global environment which shadowed
  every dynamic binding, so `(boundp '*x*)` was NIL after `(defvar *x* 1)` and
  `(let ((*x* 2)) *x*)` read 1. **A consequence worth knowing:** global lookup
  is by symbol *identity*, not by name, so two same-named symbols from
  different packages are two variables — code doing
  `env.find_variable(LispSymbol('*FOO*'))` with a freshly built symbol will no
  longer find an interned one.
- **What makes a variable special is one table**, written only by
  `binding.proclaim_special` (from `DEFVAR`/`DEFPARAMETER`, `DECLAIM`/
  `PROCLAIM`'s `(SPECIAL ...)`, and the bootstrap's
  `lispenv.STANDARD_SPECIAL_VARIABLES`) and read only by
  `binding.is_proclaimed_special`. The proclamation is what makes a binding
  form bind in the value cell rather than its own environment — without it,
  `(let ((*print-base* 2)) ...)` binds lexically and the printer, which reads
  the variable from Python through the *global* environment, never sees it.
- `DEFSTRUCT`/`DEFUN`-style forms must define their functions in the *global*
  environment (walk the environment's parent chain to the root), not the lexical
  environment they were evaluated in — otherwise they vanish once the defining
  form returns.
- NIL shows up as Python `None`, the `lisptype.NIL` singleton, *and* as a
  `LispSymbol` named `"NIL"` interned in some other package. Code branching on
  "is this NIL" needs to handle all three.
- Special forms that need unevaluated arguments (`MACROLET`, `SYMBOL-MACROLET`,
  `DEFSETF`, `DEFPACKAGE`, ...) must be registered via `cl_special`, never as a
  plain function — see the registry note above.
- **A new control-transfer exception must be added to every pass-through tuple**
  (`except (ReturnFromException, ThrowException, GoException)` in
  `evaluation_core.py`'s APPLY/FUNCALL and the special forms), or the one site
  you miss silently converts a control transfer into an error — plan.md
  Finding K's defect class. Prefer subclassing an existing one, as
  `HandlerCaseTransfer` subclasses `ThrowException`, so the existing tuples
  cover it. **`lisptype.RestartException` does *not* subclass any of them and is
  in none of those tuples** — `funcall` wraps it into a condition, which is why
  a handler still cannot invoke a restart (confirmed, see plan.md's Discovered
  issues). Note also that `lisptype.Error` extends `BaseException`, not
  `Exception`, so `except Exception` does not catch a directly-raised one.
- `merge-pathnames` must tell apart "defaults names a file" (use its parent
  directory) from "defaults names a directory" (use as-is); getting this backward
  makes `LOAD` resolve relative paths to the wrong place.
- Custom path/stream-like classes need to implement the relevant Python protocol
  (e.g. `__fspath__` for `os.PathLike`) to interoperate with stdlib functions.

## Where things are tracked

- `plan.md` — current status snapshot and pointers; the front door for "what's
  the state of this project".
- `plans/ansi_test_plan.md` — original bootstrap plan; now mostly historical,
  check it only for its few remaining open items.
- `REPAIR.md` — the authoritative crash-repair SOP referenced above.
- `docs/ansi_targets.txt` / `scripts/coverage.py` — ANSI symbol coverage tracking.
