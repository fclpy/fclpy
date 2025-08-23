# Phase 3 – Evaluator & Macros

After each task: `pipenv run pytest -q`.

## Tasks (Detailed Steps)

- [ ] Special operator registry & dispatcher.
	- [ ] Define enumeration/list of special forms.
	- [ ] Create dispatcher function mapping symbol -> evaluator fn.
	- [ ] Implement minimal evaluators for QUOTE, IF, PROGN first; add tests.
	- [ ] Incrementally add remaining forms; each with tests for semantics.

- [ ] Dynamic binding & SPECIAL declarations foundation.
	- [ ] Introduce dynamic environment stack separate from lexical env.
	- [ ] On binding a symbol declared SPECIAL, store previous value (push) and restore after scope exit.
	- [ ] Implement `LET` / `LET*` awareness of specials (Phase 7 declarations will mark them); interim: treat `*earmuffed*` names as SPECIAL heuristic (document until declarations parsed).
	- [ ] Implement helpers `bind_dynamic(symbol, value)` / `lookup_dynamic(symbol)`.
	- [ ] Tests: nested dynamic bindings for `*package*`, shadowed lexical var doesn't affect dynamic lookup for non-special, SPECIAL does.
	- [ ] Prepare for Section 7 parsing of (declare (special ...)).

- [ ] Lambda list parser.
	- [ ] Define grammar for parameter categories.
	- [ ] Implement parser producing structured object: params + defaults + supplied-p flags.
	- [ ] Tests: combinations of &optional, &rest, &key (including :keyword external names), &aux.

- [ ] Macro namespace separation or tagging.
	- [ ] Extend registry entry kind to include `macro`.
	- [ ] Environment lookup distinguishes macro vs function.
	- [ ] Tests: function and macro same name possible? (Document if unsupported initially.)

- [ ] Implement DEFMACRO / MACRO-FUNCTION / MACROEXPAND / MACROEXPAND-1.
	- [ ] Store macro expander function in registry (kind=macro).
	- [ ] Implement expansion algorithm (loop until non-macro or MACROEXPAND-1 stops after single level).
	- [ ] Tests: simple macro (identity, with &rest), nested expansion chain.

- [ ] Backquote expansion (basic subset).
	- [ ] Implement recursive expander handling quasiquote lists and vectors (if vectors exist) with , and ,@.
	- [ ] Tests from examples: `` `(a ,b ,@c d)``.

- [ ] Non-local exits.
	- [ ] Implement BLOCK/RETURN-FROM using Python exceptions carrying tag + values.
	- [ ] Implement TAGBODY/GO (may simplify to label search + exception jump; doc limitations).
	- [ ] CATCH/THROW with dynamic binding stack.
	- [ ] Tests: nested blocks, throw across frames.

- [ ] UNWIND-PROTECT support.
	- [ ] Wrap evaluation with try/finally ensuring cleanup executed on normal & non-local exit.
	- [ ] Test raising inside protected form triggers cleanup.

- [ ] Evaluator updated for placeholder multiple values.
	- [ ] Accept internal tuple sentinel from Phase 4 design placeholder (if Phase 4 not done, stub capturing first value only; document restriction).

## Order Guidance
Implement minimal evaluator & macro system before advanced control transfers; add non-local exits after macro correctness to simplify debugging.

## Exit Criteria
Macroexpansion golden tests + special form tests green.
