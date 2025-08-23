# Phase 0 – Quick Wins & Hygiene

Agent MUST run `pipenv run pytest -q` after EACH checkbox before ticking it.

## Tasks (Breakdowns the agent must follow in order)

- [x] Fix `cddddr` bug (`core.py`). (Verified present and correct)
	- (Done) Confirm implementation: nested four `cdr` calls.
	- (Done) Unit test exists & passes.

- [x] Introduce canonical `T` symbol; unify predicates to return `T` / `NIL` only.
	- (Done) `T` symbol present in `lisptype.py`.
	- [ ] Audit predicates still returning raw Python bool.
		- [ ] Grep for `return True` / `return False` in lisp source dirs.
		- [ ] For each occurrence decide: replace with `T` / `NIL` or wrap via `lisp_bool` helper.
		- [ ] Add / extend tests asserting `(type-of (predicate ...))` yields symbol not Python bool (if such type checks exist) OR assert printer outputs `T`/`NIL`.

- [ ] Centralize reader macro/readtable API (single module e.g. `readtable.py`).
	- [ ] Inventory existing readtable / macro char functions in `io.py` and stubs in `utilities.py`.
	- [ ] Design target module interface: `*READTABLE*`, `copy_readtable`, `make_dispatch_macro_character`, `set_syntax_from_char`, `readtable_case`, registration of macro chars.
	- [ ] Create new module (if not present) and move canonical implementations there.
	- [ ] Replace stubs in `utilities.py` with imports or remove + re-export.
	- [ ] Update any import sites to new module path.
	- [ ] Add tests: copying readtable preserves macro char, dispatch macro installation, case mode.
	- [ ] Run full test suite.

- [ ] Remove duplicate function registrations; generate env strictly from registry.
	- [ ] Implement script/test to build list of registered symbol names (function namespace) and assert uniqueness.
	- [ ] Grep for duplicate function definitions (same Lisp name bound multiple times).
	- [ ] Eliminate or consolidate duplicates (prefer canonical location; keep docstrings).
	- [ ] Ensure environment construction code iterates only registry metadata (Phase 1 will enrich metadata—keep forward compatibility).
	- [ ] Run tests.

- [ ] Replace package stubs in `utilities` with real `lisptype.Package` exports.
	- [ ] Locate stub functions (package create/find/ensure etc.).
	- [ ] Inspect existing `Package` class (if exists) or implement minimal version: name, nicknames, symbols table, use list.
	- [ ] Implement real operations: `find-package`, `intern`, `export`, `use-package` (stubs may just raise or no-op now).
	- [ ] Adjust registration decorators to point to real implementations.
	- [ ] Add tests: interning same symbol returns identical object; keyword self-evaluates (Phase 1 dependency—can write pending tests skipped until Phase 1 completes if needed).

- [ ] Add test asserting no duplicate symbol registrations after env init.
	- [ ] Implement helper `collect_function_symbols()` returning list of names.
	- [ ] Assert `len(set(names)) == len(names)`.
	- [ ] Fail test first by artificially duplicating (optional) then remove duplication.

- [ ] Normalize printer for booleans: prints `T` and `NIL`.
	- [ ] Identify printer function(s) (likely in `io` or `printer` module).
	- [ ] Ensure `lisp_bool` always returns canonical symbols; printer should simply print symbol names.
	- [ ] Add tests capturing printed output of `(list t nil)` equals `(T NIL)` (or appropriate formatting) and not Python `True`/`False`.

- [ ] Add simple round‑trip test for `(A B C)` and a keyword symbol.
	- [ ] Use printer to render structure; feed to reader; compare structural equality (symbols identity + list shape).
	- [ ] Include a keyword (e.g., `:FOO`) verifying keyword package interning.
	- [ ] Mark test expected-fail (xfail) until reader/package pieces implemented if sequence depends on later phases.

## Sequencing Notes
Execute remaining unchecked tasks roughly in listed order; package stubs and reader centralization can be parallel but finalize centralization before duplicate-registration test to avoid churn.

## Exit Criteria
All boxes checked + full test suite green.
