# Phase 7 – Declarations, Optimization, Coverage

After each checkbox: `pipenv run pytest -q`.

## Tasks (Detailed Steps)

- [ ] Parse DECLARE / DECLAIM; store metadata.
	- [ ] Extend parser (Phase 3) to recognize (declare (optimize ...) (special ...) (type ... var)).
	- [ ] Store metadata on symbol plist or registry entry.
	- [ ] Tests: macro expanding form preserves metadata structure.

- [ ] Expose DOCUMENTATION retrieval.
	- [ ] Implement DOCUMENTATION function consulting registry/plists.
	- [ ] Permit setting documentation via DEFUN/DEFMACRO optional string.
	- [ ] Tests: retrieving set docstring.

- [ ] Optional performance toggles.
	- [ ] Define global optimization policy object (speed, safety) placeholders.
	- [ ] Provide DECLAIM (optimize ...) updating policy; no-op impact but stored.
	- [ ] Tests: policy updated.

- [ ] Coverage script enumerating implemented ANSI symbols vs target set.
	- [ ] Maintain canonical target list (import from spec file).
	- [ ] Compare with registry; produce markdown table (Implemented, Stub, Missing).
	- [ ] Integrate into CI (failing if coverage regresses below threshold defined).
	- [ ] README section auto-updated (optional gated by flag to avoid manual edits).

## Order Guidance
Parse declarations first (foundation for documentation & optimization), then coverage tooling last so it reflects final state.

## Exit Criteria
Coverage script integrated; declarations persisted.
