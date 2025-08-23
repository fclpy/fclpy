# Phase 8 – Advanced Features (Optional)

**CRITICAL**: Run `pipenv run pytest -q` after every task.

**NOTE**: This phase is NOT required for basic ANSI compliance. Only work on this after phases 0-7 are completely done.

## Step-By-Step Tasks (Do These In Order)

### Task 1: Improve rational number arithmetic
- [ ] **Goal**: Better fraction support
	- [ ] **Step 1.1**: Add GCD (greatest common divisor) helper function  
	- [ ] **Step 1.2**: Make ratio constructor automatically reduce fractions: 6/8 becomes 3/4
	- [ ] **Step 1.3**: Handle sign normalization: -6/8 becomes -3/4
	- [ ] **Step 1.4**: Make division return exact ratios when possible instead of floats
	- [ ] **Step 1.5**: Write tests for fraction reduction and arithmetic
	- [ ] **Step 1.6**: Run `pipenv run pytest -q`

### Task 2: Add big integer support
- [ ] **Goal**: Handle very large integers
	- [ ] **Step 2.1**: Use Python's built-in arbitrary precision integers
	- [ ] **Step 2.2**: Add TYPEP recognition for BIGNUM vs FIXNUM (use simple rule: abs(n) > 2^29)
	- [ ] **Step 2.3**: Write tests: boundary classification, arithmetic preserves type
	- [ ] **Step 2.4**: Document that we don't have separate machine word tuning
	- [ ] **Step 2.5**: Run `pipenv run pytest -q`

### Task 3: Add complex number support
- [ ] **Goal**: Numbers like 3+4i
	- [ ] **Step 3.1**: Create Complex type that stores real and imaginary parts
	- [ ] **Step 3.2**: Support integers, ratios, and floats as real/imaginary parts  
	- [ ] **Step 3.3**: Add reader support for `#C(real imag)` syntax
	- [ ] **Step 3.4**: Add basic arithmetic operations for complex numbers
	- [ ] **Step 3.5**: Write tests for complex number operations
	- [ ] **Step 3.6**: Run `pipenv run pytest -q`

### Task 4: Add FORMAT function (basic subset)
- [ ] **Goal**: Formatted output like `(format t "Hello ~A" name)`
	- [ ] **Step 4.1**: Create basic FORMAT function that handles ~A (aesthetic), ~S (standard), ~D (decimal)
	- [ ] **Step 4.2**: Handle ~% (newline) and ~~ (literal tilde)
	- [ ] **Step 4.3**: Support output to string or stream
	- [ ] **Step 4.4**: Write tests for basic format directives
	- [ ] **Step 4.5**: Document which format directives are not supported yet
	- [ ] **Step 4.6**: Run `pipenv run pytest -q`

### Task 5: Add random number support
- [ ] **Goal**: Random number generation
	- [ ] **Step 5.1**: Create random state object
	- [ ] **Step 5.2**: Implement RANDOM function for integers and floats
	- [ ] **Step 5.3**: Implement MAKE-RANDOM-STATE function
	- [ ] **Step 5.4**: Add *RANDOM-STATE* global variable
	- [ ] **Step 5.5**: Write tests for random number generation
	- [ ] **Step 5.6**: Run `pipenv run pytest -q`

### Task 6: Improve math functions
- [ ] **Goal**: More complete math library
	- [ ] **Step 6.1**: Add missing trig functions (SIN, COS, TAN, etc.)
	- [ ] **Step 6.2**: Add logarithm and exponential functions  
	- [ ] **Step 6.3**: Add square root and power functions
	- [ ] **Step 6.4**: Handle complex number inputs where appropriate
	- [ ] **Step 6.5**: Write tests for math function accuracy
	- [ ] **Step 6.6**: Run `pipenv run pytest -q`

### Task 7: Add LOOP macro (basic subset)
- [ ] **Goal**: Basic iteration construct (optional - can skip if too complex)
	- [ ] **Step 7.1**: Support simple FOR clauses
	- [ ] **Step 7.2**: Support COLLECT and DO clauses
	- [ ] **Step 7.3**: Support WHEN/UNLESS conditionals  
	- [ ] **Step 7.4**: Write tests for basic loop patterns
	- [ ] **Step 7.5**: Document which LOOP features are not supported
	- [ ] **Step 7.6**: Run `pipenv run pytest -q`

### Task 8: Enhance I/O system
- [ ] **Goal**: Better stream and file handling
	- [ ] **Step 8.1**: Add *STANDARD-OUTPUT*, *STANDARD-INPUT*, *ERROR-OUTPUT* variables
	- [ ] **Step 8.2**: Improve file stream support with more options
	- [ ] **Step 8.3**: Add string streams for in-memory I/O
	- [ ] **Step 8.4**: Write tests for enhanced I/O capabilities
	- [ ] **Step 8.5**: Run `pipenv run pytest -q`

## Important Notes
- This entire phase is optional for basic ANSI compliance
- These features make the implementation more complete but aren't essential
- Feel free to skip tasks that seem too complex
- Focus on getting basic functionality working in each area

## How to Know Phase 8 is Done
✅ All desired checkboxes above are checked (you can skip some)
✅ All tests pass when you run `pipenv run pytest -q`
✅ Implementation is significantly more complete and ANSI-compliant
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
