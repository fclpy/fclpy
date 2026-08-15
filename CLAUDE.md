# fclpy — CLAUDE.md

fclpy is a Common Lisp interpreter written in Python. The goal is **full ANSI Common
Lisp compliance**, measured by running the real ANSI test suite (`ansi-test/`, a
sibling directory one level above this repo) to completion without crashing, and
passing as many of its tests as possible.

> **Current status (2026-08-15).** The suite runs to completion and is **past
> half passing**: `COMPLETENESS: OK`, 22113/22113 accounted, 0 missing,
> **11548 passing (52.2%)**, ~67 minutes. Crashes are no longer the constraint;
> **semantics are**. (The first complete run was 2026-08-12: 8960 of 22036,
> 40.7%, ~7.5 hours.)
>
> **[plan.md](plan.md) is the roadmap**, organised around the mechanism at fault
> rather than test counts, and **`docs/ansi_checklist.md` is the authority for
> what is failing and where.** Read both before starting. The checklist is
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
> complete even when a run died partway. `expected-failures/` is still unwired.

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
  `run_all_tests.py` takes **about 67 minutes** end to end (measured 2026-08-15;
  it was ~7.5 hours before LOOP got one iteration engine, and the "20 minutes"
  this file used to claim was never right). A single `scripts/run_ansi.py`
  *group* is usually 2–30s, but a few are far slower because one form in them
  never terminates and burns the 600s LOOP cap — `characters` takes ~9 minutes
  for 259 tests for that reason.

## Architecture map

- **Reader**: `tokenizer.py` (character-level) → `lispreader.py` (token → form) →
  `readtable.py` (macro characters, `#`-dispatch, case/readtable state). If a crash
  looks like a Python type error on oddly-shaped data, check here first — many
  "evaluator" bugs are actually mis-parsed syntax (dotted pairs, bit-vectors,
  exponent markers, etc).
- **Types**: `lisptype_basic.py` (symbols, cons cells, NIL/T, `MultipleValues`) and
  `lisptype_extended.py` (`Environment`, **`Package` — at `:322`, *not* in
  `lisptype_basic.py`**, symbol-macros, condition types). `lisptype.py` re-exports
  both. Note `setf-expanders` is **monkey-patched onto `Environment` at runtime**
  (`evaluation_core.py:1229-1230`) rather than declared in `__init__`.
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
    early and it will crash or silently misbehave.
- **Packages**: `lispfunc/misc_packages.py` — `coerce_to_package` (the package
  *designator* rule, CLHS 11.1.1.1) and `package_symbols(pkg, kind)` for the
  accessible / present / external symbol sets. `DO-SYMBOLS`,
  `DO-EXTERNAL-SYMBOLS` and LOOP's `for x being the symbols of p` all go through
  them; the copies they replaced disagreed, because `Package.use_packages` holds
  package **names** as well as `Package` objects and a copy that read
  `external_symbols` off a string silently dropped every inherited symbol.
- **Hash tables**: `MAKE-HASH-TABLE` returns `misc_hashtables.HashTableDict`, a
  `dict` subclass whose test/size/rehash options are **attributes**. They used
  to be `'__hashmeta__...'` *keys*, i.e. entries in the table, so every
  traversal needed to know to skip them and only four did. Note there is still a
  second, dead hash-table implementation (`lispfunc/hashtables.py`'s
  `HashTable`) that registers the same operators and loses the registration —
  standing rule 3, not yet resolved.
- **Sequences**: `lispfunc/sequence_protocol.py` — **the one place that answers
  both halves of CLHS 17.1**: `seq_elements` (what are the elements of this Lisp
  sequence — `lispCons`, Python `list`/`tuple` vector, `LispString`, `str`,
  `AdjustableVector`), and the constructors, `rebuild_like` (a result of the
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
- **State**: `state.py` holds the few intentional cross-module globals
  (`packages`, `current_package`, `current_environment`, `restart_stack`,
  `handler_stack`). Don't add new ad-hoc globals elsewhere — put them here.
- **Environment bootstrap**: `lispenv.py` — `setup_standard_environment()` builds
  the initial global environment from the registries above.

## The development loop (crash repair)

The full step-by-step SOP is in **REPAIR.md** — follow it exactly rather than
improvising. Summary:

1. `pipenv run python run_all_tests.py > run_all_tests.log 2> run_all_tests.err`
   loads `../ansi-test/doit.lsp` end to end. This is slow — expect a long run.
2. Find the crash: the crashing test is the one *after* the last test name printed
   in `run_all_tests.log`; confirm with the traceback in `run_all_tests.err`. If
   the log's last test is ambiguous (e.g. it's also the last test in that `.lsp`
   file), use `doit.log` to recover execution order.
3. Isolate: point `run_do_test.py`'s `test_lisp` line at just that test
   (`(in-package :cl-test) (do-test 'TESTNAME.N)`) and run
   `pipenv run python run_do_test.py` to reproduce it in isolation.
4. Fix the root cause — prefer reader fixes over evaluator hacks when the input
   syntax is the real problem; prioritize matching ANSI semantics over whatever is
   locally convenient.
5. Re-run the isolated test to confirm the fix, `git diff` to strip every debug
   `print()`/temp variable, then re-run the full suite to confirm no regression
   and that the previously-crashing test now appears in the log.
6. Repeat until `doit.lsp` runs to completion (tests failing is fine — the loop's
   goal is *zero crashes*, not zero failures).

### Rules for this loop
1. One crash at a time — fix it, verify it, move on. Don't batch unrelated fixes.
2. No refactoring beyond what the fix requires.
3. Never leave debug `print()`/diagnostic code in a fix.
4. Never commit automatically or with failing tests — commits are the user's call.

## Secondary checks

- `pipenv run pytest -q` — the `tests/` unit-test suite (fast regression net for
  individual functions/forms; not the same thing as the ANSI conformance run).
- `pipenv run python scripts/coverage.py` — compares `docs/ansi_targets.txt`
  against the live function/special registries to report symbol coverage.

## Architectural gotchas learned from prior repairs

- `*PACKAGE*` is a dynamic special variable but its value is mirrored in
  `state.current_package`. Anything that binds it (`LET`, `LET*`, `IN-PACKAGE`)
  must update both or symbol interning silently goes to the wrong package. For
  binding forms this now lives in one place, `BindingFrame._mirror_package`.
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
