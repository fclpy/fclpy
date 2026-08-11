# fclpy — CLAUDE.md

fclpy is a Common Lisp interpreter written in Python. The goal is **full ANSI Common
Lisp compliance**, measured by running the real ANSI test suite (`ansi-test/`, a
sibling directory one level above this repo) to completion without crashing, and
passing as many of its tests as possible.

> **Current status (2026-08-12).** The suite runs **8971 of 22036** tests
> (`passed=4514 failed=4457`), up from `accounted=4687 passed=2920` — the
> signal-before-unwind handler-stack rewrite (plan.md's 2026-08-12 update)
> removed the `HANDLER-BIND.13` abort. **It still does not run to completion:**
> the truncation point is now `DO-SYMBOLS.8` (`packages/do-symbols.lsp`), because
> `DO-SYMBOLS` establishes no implicit `tagbody`, so its `(go foo)` raises an
> uncaught `GoException` — a control transfer, not a condition, so RT's own
> `handler-case` cannot stop it. That is the **sixth** instance of "the form
> doesn't establish the block/tagbody CLHS requires"; auditing every iteration
> and mapping form for both in one pass is recommended over fixing them one
> crash at a time. `conditions/` (116/303) and `packages/` (72/340) are
> measurable for the first time; 14 directories after `packages/` in
> `gclload2.lsp` are still at zero. Trust the `COMPLETENESS:` line, not the
> "N failures ... out of 22036 tests" summary, which prints the initial pending
> count unconditionally and looks complete even when the run died partway.
>
> **⚠ Older status correction, superseded 2026-08-09 (see plan.md's "Update" sections for
> details) — kept here for history.** The suite used to silently abort after ~2 990 of
> 22 036 tests (13.6% coverage) because `LOOP` established no implicit `NIL` block.
> **That was previously (falsely) marked fixed here — it was not; see plan.md.** It
> and three more instances of the same "form doesn't establish the block/condition
> CLHS requires" defect class (CLOS methods, LOOP's `NAMED` clause, `ERROR`/`CERROR`'s
> condition dispatch) are now genuinely fixed, each verified in isolation and via
> `pytest -q` (1172 passed / 1 pre-existing unrelated failure throughout). FORMAT's
> argument-cursor bugs are also fixed (2026-08-09, earlier in the same day).
>
> **The suite still does not run to completion.** `run_all_tests.py` now prints a live
> `COMPLETENESS: total=... accounted=... missing=...` line every run (pulled directly
> from RT's `*entries*`/`*passed-tests*`/`*failed-tests*`, not parsed from
> FORMAT-rendered text) — trust that line, not the "N failures ... out of 22036 tests"
> summary line, which prints the *initial pending count* unconditionally and looks
> complete even when the run crashed partway through. As of the last verified run the
> truncation point is past `ERROR.1`/`CERROR.1` (accounted 4623/22036) and not yet
> identified — see plan.md's later Update for the diagnostic method and why this
> session stopped there rather than continuing one-crash-at-a-time. A LOOP hitting an
> unimplemented clause (e.g. `AS`, `BEING`) can no longer hang the whole run forever:
> `LOOP_TIMEOUT_ERROR` now converts a >10-minute loop into a loud `LispError`.
> `scripts/ansi_score.py` now exists — run it after `run_all_tests.py` to get a
> per-subsystem table from `ansi_results/*.txt` and a `docs/ansi_baseline.json`
> snapshot. `expected-failures` is still unwired.
>
> **Current work mode is milestone-driven semantic repair, not crash repair — but
> crashes (not just this document's claims about them) still need re-verifying before
> being treated as fixed.** Read [plan.md](plan.md) — it is now the roadmap, not a
> status snapshot.

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
  `run_all_tests.py` takes about 20 minutes end to end.

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
  - `evaluation_loops_conditionals.py` — `LET`/`LET*`/`DO`/`DOLIST`/`DOTIMES`/`LOOP`/`COND`.
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
  must update both or symbol interning silently goes to the wrong package.
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
