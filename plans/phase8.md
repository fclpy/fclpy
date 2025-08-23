# Phase 8 – Extended Numeric & Advanced I/O / Presentation

Run tests after each task: `pipenv run pytest -q`.

This phase is not required for the initial "Meaningful ANSI Subset" milestone but prepares for broader ANSI alignment.

## Goals
Provide incremental support for:
- Numeric tower extensions (bignums, ratios normalization, complex numbers)
- FORMAT subset & pretty printer foundations
- LOOP macro minimal subset (optional – may be deferred further)
- Advanced printer control variables
- Random numbers & math function coverage subset
- Pathname & stream feature enrichments (logical pathnames, *standard-output* abstractions)

## Tasks (Detailed)

- [ ] Rational normalization & exact arithmetic
  - [ ] Ensure ratio constructor reduces by GCD (implement `gcd` helper if missing)
  - [ ] Tests: (make-ratio 6 8) => 3/4, sign normalization (-6 8) => -3/4
  - [ ] Division producing exact ratios instead of floats when evenly representable

- [ ] Bignum support (thin wrapper)
  - [ ] Leverage Python arbitrary precision ints; add TYPEP recognition for BIGNUM vs FIXNUM (simple heuristic: abs(n) > 2**29)
  - [ ] Tests: boundary classification, arithmetic preserves type
  - [ ] Document deviation: no separate machine word size tuning

- [ ] Complex numbers (basic)
  - [ ] Introduce Complex type storing real/imag (ints, ratios, floats)
  - [ ] Reader for #C(real imag)
  - [ ] Printer canonical form #C(real imag)
  - [ ] Arithmetic: + - * / for complex (delegate to Python complex internally but preserve component types where feasible)
  - [ ] Tests: arithmetic closure, printing round-trip
  - [ ] Defer: transcendental functions (document)

- [ ] Numeric function coverage subset
  - [ ] Implement/verify: ABS, FLOOR, CEILING, TRUNCATE, MOD, REM, EXPT, SQRT (real only if complex sqrt deferred), LOG (defer complex), SIN COS TAN (float/complex path, may defer complex), RANDOM (floats + integer range)
  - [ ] Tests: truth table style for representative values
  - [ ] Document deferred: SCALE-FLOAT, DECODE-FLOAT, complex log trig handling

- [ ] FORMAT subset (Phase 8 focal)
  - [ ] Implement dispatcher parse for control string scanning ~<directive>
  - [ ] Support directives: ~~ ~A ~S ~% ~& ~D ~X ~O ~B ~R (cardinal only) ~C ~F (basic) ~T (tab stop minimal) ~{ ~} iteration
  - [ ] Tests: table of (format-string, args, expected-output)
  - [ ] Provide *print-escape* semantics for ~S vs ~A (tie into printer flags Phase 2/3)
  - [ ] Defer advanced directives (~<, justification, conditional, pluralization) – list in docs

- [ ] Pretty printer hooks
  - [ ] Introduce *print-level* and *print-length* handling in core printer
  - [ ] Implement ellipsis marker when exceeding
  - [ ] Tests: nested list depth & length truncation
  - [ ] Defer full dispatch tables & logical blocks

- [ ] Printer control dynamic variables
  - [ ] *print-case*, *print-circle* (stub false), *print-gensym*, *print-array* (stub), *print-readably*
  - [ ] Make them SPECIAL dynamic bindings (tie to Phase 3 dynamic env)
  - [ ] Tests: case toggling for symbols, readably enforcing quotes for strings
  - [ ] Document deviations for unsupported flags

- [ ] LOOP macro (optional minimal)
  - [ ] Implement recognition of `(loop for x in list collect x)` and `(loop repeat n do ...)`
  - [ ] Macroexpansion into LET / TAGBODY constructs
  - [ ] Tests: simple list comprehension, repeat counter
  - [ ] Defer full LOOP grammar (document)

- [ ] Logical pathnames (starter)
  - [ ] Parse logical names like "MYAPP:SRC;FILE.LISP" into components
  - [ ] Translate-logical-pathname stub raising unless mapping defined
  - [ ] Tests: basic parse; document deferral of translation tables

- [ ] Stream enhancements
  - [ ] Introduce string input/output streams (with MAKE-STRING-INPUT-STREAM, MAKE-STRING-OUTPUT-STREAM, GET-OUTPUT-STREAM-STRING)
  - [ ] Tests: writing then retrieving buffer; reading incremental chars
  - [ ] Defer: binary element-type specialization

- [ ] Random state object
  - [ ] Implement make-random-state (copy) & *random-state*
  - [ ] RANDOM uses state; reproducible sequences test
  - [ ] Defer: advanced state seeding portability

## Order Guidance
1. Rational normalization & numeric foundations
2. Complex numbers & numeric function subset
3. Printer control vars & pretty printer truncation
4. FORMAT subset (depends on printer flags)
5. Streams & random state
6. Optional LOOP macro & logical pathnames last

## Exit Criteria
- All above checkboxes ticked OR explicitly moved to deviations doc with rationale.
- Tests green.
- Coverage script updated to include new symbols (no regression).

## Documentation Updates
- Update `docs/ansi_targets.txt` adding newly implemented symbols.
- Add `docs/format.md` describing supported directives + deferred list.
- Update spec deviations for each deferred numeric / FORMAT / LOOP feature.

## Risks / Notes
- FORMAT parsing complexity: keep grammar incremental; avoid premature optimization.
- Pretty printing may interact with existing printer; ensure backward compatibility in default settings (unchanged output when flags unset).

End Phase 8 Plan.
