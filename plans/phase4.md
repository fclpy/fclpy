# Phase 4 – Multiple Values, Conditions, Restarts

Post-task: `pipenv run pytest -q`.

## Tasks (Detailed Steps)

- [ ] Multiple value representation.
	- [ ] Decide structure: Python tuple + marker or custom class.
	- [ ] Update evaluator to propagate multiple values where specified (initial minimal propagation at RETURN-FROM, function return).
	- [ ] Tests: function returning 0,1,3 values; NTH-VALUE extraction (after defined).

- [ ] Implement VALUES, VALUES-LIST, MULTIPLE-VALUE-CALL, MULTIPLE-VALUE-PROG1, MULTIPLE-VALUE-BIND, NTH-VALUE.
	- [ ] Each function updates value propagation rules; add tests per form.

- [ ] Condition hierarchy.
	- [ ] Define base Condition + subclasses (Simple-Condition, Error, Warning, Type-Error, File-Error etc.).
	- [ ] Provide slot accessors as needed.
	- [ ] Tests: subclass relationships via TYPEP.

- [ ] SIGNAL, ERROR, CERROR, WARN.
	- [ ] Implement signaling mechanism raising Python exceptions mapping to condition objects.
	- [ ] CERROR provides restart named `CONTINUE` by default.
	- [ ] Tests: capturing condition, continuing on CERROR.

- [ ] RESTART-CASE, RESTART-BIND, INVOKE-RESTART, ABORT.
	- [ ] Implement dynamic restart stack (list of frames each with name->function).
	- [ ] INVOKE-RESTART searches stack; ABORT invokes abort restart or raises if absent.
	- [ ] Tests: nested restarts, selecting non-top restart.

- [ ] Tests for propagation and restart selection.
	- [ ] Scenario: SIGNAL inside with restarts offered; choosing one alters control flow as expected.

- [ ] Document any deferred condition types.
	- [ ] Add section to docs listing omitted or simplified types.

## Order Guidance
Implement multiple values first (needed by NTH-VALUE etc.), then conditions signaling, then restarts; documentation last.

## Exit Criteria
All implemented functions tested; restart scenario passes.
