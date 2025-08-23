# Phase 2 – Reader + Printer + Packages

Always: `pipenv run pytest -q` post-task.

## Tasks (Detailed Steps)

- [ ] Implement `Readtable` (macro chars, dispatch, case mode).
	- [ ] Define class with: mapping char -> function, dispatch table, case mode enum.
	- [ ] Provide clone method used by `copy_readtable`.
	- [ ] Register standard macro chars: `(` `)` `'` `"` `;` `#`.
	- [ ] Tests: copying preserves handlers; modifying copy not affecting original.

- [ ] Tokenizer fundamentals.
	- [ ] Implement character stream abstraction (peek, next, position).
	- [ ] Implement token assembly handling escapes `|...|` and `\`.
	- [ ] Parse integer, ratio (a/b), float, complex `#C(...)` (if deferred, note in docs), symbols, keywords `:FOO`.
	- [ ] Dotted list parsing inside list reader.
	- [ ] Tests for each token type.

- [ ] Character literals & character type baseline.
	- [ ] Introduce `Character` class (subclass of lispT) with slot `code`.
	- [ ] Reader support for `#\A`, named chars (`#\Space`, `#\Newline`, case-insensitive).
	- [ ] Printer prints `#\A` or canonical name if named.
	- [ ] Tests: round-trip for alpha, space, newline; TYPEP returns true for CHARACTER.
	- [ ] Document deferred: full char attribute predicates (graphic, whitespace), charset beyond ASCII.

- [ ] Dispatch macros.
	- [ ] Implement `#'` (function shorthand) placeholder (may integrate later with evaluator).
	- [ ] Implement `#()` vector literal (store as Python list for now) with test.
	- [ ] Implement block comments `#| |#` (nesting allowed) and line comments `;` skipping rest of line.
	- [ ] Add tests: nested block comments ignored, vector length preserved.

- [ ] Integrate interning.
	- [ ] On symbol token finalize: if starts with `:` intern in KEYWORD package else current package.
	- [ ] Provide dynamic variable `*PACKAGE*`; default to COMMON-LISP or USER.
	- [ ] Tests: reading two identical symbols yields identical object (by identity), keywords self-evaluate.

- [ ] Basic printer (`prin1`, `princ`).
	- [ ] Implement dispatch by type (symbol, number, list, string, vector).
	- [ ] Ensure quoting of strings with escapes; symbols printed uppercase by default (unless escaped originally—defer case folding doc if not implemented).
	- [ ] Tests: round-trip for simple forms.

- [ ] Reader error classes.
	- [ ] Define base `ReaderError` + specific (UnexpectedEOF, InvalidNumber, UnbalancedParen, etc.).
	- [ ] Raise appropriate errors; add tests expecting exceptions.

- [ ] Round-trip test corpus (table-driven).
	- [ ] Create data file or list of (source, expected-ast) pairs.
	- [ ] Parametrize test over corpus; include edge: empty list, dotted list, nested vectors, keywords.
	- [ ] Achieve ≥95% pass; remaining cases documented.

- [ ] Document unimplemented features list.
	- [ ] Create section in README or `docs/reader.md` enumerating deferred items (e.g., unread-char, read-base, dispatch variants).
	- [ ] Keep updated before exit.

## Order Guidance
Implement readtable first, then tokenizer, then dispatch macros, then integration & printer, followed by error classes and corpus/testing & docs.

## Exit Criteria
≥95% success on corpus; exceptions documented.
