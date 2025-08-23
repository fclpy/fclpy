# Phase 1 – Canonical Core & Registry

Run `pipenv run pytest -q` after every task.

## Tasks (Step-by-step)

- [ ] Extend `LispSymbol` with slots: `value`, `function`, `plist`.
	- [ ] Modify class definition; keep backward compat constructor (supply defaults).
	- [ ] Add accessors if Lisp-visible (`symbol-value`, `symbol-function`, `symbol-plist`).
	- [ ] Write tests: creating symbol, setting value, function, plist round-trip.

- [ ] Implement keyword auto-interning (self-evaluating).
	- [ ] Ensure KEYWORD package exists (create if absent) with `:KEYWORD` nickname.
	- [ ] Reader (Phase 2) will rely on this; interim: implement `intern_keyword(name)` utility.
	- [ ] Guarantee returned symbol's home package is KEYWORD and symbol evaluates to itself (variable binding).
	- [ ] Test: evaluating printed keyword returns same object; printing preserves leading colon.

- [ ] Enforce package-aware interning everywhere.
	- [ ] Grep for direct `LispSymbol(` constructions; wrap with `intern` or keyword helpers.
	- [ ] Add linter test: forbid bare `LispSymbol(` outside symbol/type module (regex scan).
	- [ ] Update affected modules.

- [ ] Add registry metadata (name, kind, arg-spec, doc) & regenerate env from it.
	- [ ] Define dataclass `RegistryEntry` with fields.
	- [ ] Replace ad-hoc decorator to populate list/dict of entries.
	- [ ] Environment init iterates metadata to bind functions/macros (macros added Phase 3—design now with `kind` field).
	- [ ] Add tool to emit markdown coverage (Phase 7 will extend).
	- [ ] Test: number of entries matches expected; one canonical entry per symbol.

- [ ] Remove legacy static mapping dict remnants.
	- [ ] Grep for `function_mappings` / old structures; delete or refactor to use registry.
	- [ ] Add test that old global names no longer exist (`hasattr` false) or are aliases to new structure if needed.

- [ ] Tests: symbol identity, keyword self-eval, registry completeness.
	- [ ] Consolidate new tests into `tests/test_symbols.py`.
	- [ ] Ensure pytest passes.

## Implementation Order Hint
Do symbol slot extension first, then registry (so decorator stores function into symbol slot optionally). Apply enforcement of package interning after registry to minimize double edits.

## Exit Criteria
Registry is single source; tests green.
