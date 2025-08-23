# Phase 5 – Sequences / Arrays / Streams / Pathnames

Always test: `pipenv run pytest -q`.

## Tasks (Detailed Steps)

- [ ] Unified sequence protocol.
	- [ ] Define interface: iterate(seq, start, end) yielding elements + indices.
	- [ ] Implement key/test handling wrapper.
	- [ ] Support lists, vectors, strings (extendable).
	- [ ] Tests: slicing semantics with :start/:end.

- [ ] Reimplement FIND, POSITION, REMOVE, MAP, REDUCE atop protocol.
	- [ ] Write regression tests capturing current behavior before refactor.
	- [ ] Replace implementations using protocol.
	- [ ] Ensure keyword arguments honored.

- [ ] Adjustable vector with fill-pointer.
	- [ ] Implement vector structure supporting resize + fill pointer ops (VECTOR-PUSH, VECTOR-PUSH-EXTEND if included or document omission).
	- [ ] Tests: push operations, bounds.

- [ ] Optional multi-dim arrays baseline.
	- [ ] Decide minimal shape storage; row-major index calculation.
	- [ ] Basic accessors AREF / MAKE-ARRAY subset; tests.
	- [ ] Mark advanced features (displacement, adjustable) deferred.

- [ ] Stream class hierarchy.
	- [ ] Define abstract stream; subclasses: file, string.
	- [ ] Implement OPEN returning file-stream; READ-CHAR, WRITE-CHAR minimal; CLOSE.
	- [ ] Tests using temporary files + string stream round-trip.

- [ ] Pathname parsing + basic accessors.
	- [ ] Implement parser splitting namestring into host/device/dir/name/type/version (simplify where unsupported).
	- [ ] Accessors returning components.
	- [ ] Tests: constructing from path; round-trip namestring where lossless.

- [ ] Hash tables (ANSI subset).
	- [ ] Implement `MAKE-HASH-TABLE` (tests: :TEST functions EQ/EQL/EQUAL subset) storing Python dict + test function.
	- [ ] Implement `GETHASH` (return two values: value or NIL + T/NIL found flag when Phase 4 available; interim single value + doc).
	- [ ] Implement `REMHASH`, `CLRHASH`, `MAPHASH`, `HASH-TABLE-COUNT`.
	- [ ] Printer placeholder `#<HASH-TABLE n>`; reader does not read back (document).
	- [ ] Tests: insertion, removal, iteration counts.
	- [ ] Document deferred: rehash-size, rehash-threshold, weak tables.

- [ ] Property tests.
	- [ ] Reversal invariant: (reverse (reverse seq)) == seq.
	- [ ] Pathname join/split round-trip for simple cases.

## Order Guidance
Do sequence protocol before refactoring sequence functions; streams and pathnames can be developed in parallel if desired.

## Exit Criteria
Core sequence ops spec subset works; omissions documented.
