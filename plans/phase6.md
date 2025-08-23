# Phase 6 – Type System & Early CLOS

Run tests after each task: `pipenv run pytest -q`.

## Tasks (Detailed Steps)

- [ ] Class metaobjects + DEFCLASS.
	- [ ] Define Class object storing slot definitions and precedence list (simplified linear order parent->...->T).
	- [ ] Implement DEFCLASS macro storing definition (macro layer may rely on Phase 3; else provide function variant temporarily).
	- [ ] Tests: defining class with readers/writers generating accessor functions.

- [ ] MAKE-INSTANCE path.
	- [ ] Allocate instance structure (dict of slot-name -> value) with defaults evaluated.
	- [ ] Support :initarg mapping to slots.
	- [ ] Tests: initargs set slots; defaults applied.

- [ ] TYPEP for primitives + classes + cons + arrays + hash-tables.
	- [ ] Implement dispatcher by object type.
	- [ ] Extend to user-defined classes referencing class metaobjects.
	- [ ] Tests: matrix of (object, type) truth table.

- [ ] ENSURE-GENERIC-FUNCTION & ADD-METHOD minimal single dispatch.
	- [ ] Generic function object holds list of methods (specializers + function).
	- [ ] ADD-METHOD registers method sorted by specificity (class depth or order defined).
	- [ ] Invocation selects first matching method.
	- [ ] Tests: redefining generic updates method list; dispatch chooses most specific.

- [ ] CALL-NEXT-METHOD stub & method selection order.
	- [ ] Provide mechanism capturing remaining methods chain; stub may raise if chain empty.
	- [ ] Test method calling CALL-NEXT-METHOD reaches less specific method.

- [ ] Tests: TYPEP matrix, simple generic dispatch.
	- [ ] Consolidate into dedicated test module.

## Order Guidance
Define classes before generic functions; implement TYPEP before relying on type checks inside method dispatch.

## Exit Criteria
Basic dispatch working; missing MOP features documented.
