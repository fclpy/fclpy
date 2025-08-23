# Phase 2 – Reader and Printer

**CRITICAL**: Run `pipenv run pytest -q` after every task.

## Step-By-Step Tasks (Do These In Order)

### Task 1: Create Readtable class
- [ ] **Goal**: Create a class to manage how characters are interpreted when reading Lisp code
	- [ ] **Step 1.1**: Create a `Readtable` class with: character-to-function mapping, dispatch table, case mode setting
	- [ ] **Step 1.2**: Add a method to copy/clone the readtable 
	- [ ] **Step 1.3**: Add standard macro characters: `(` `)` `'` `"` `;` `#`
	- [ ] **Step 1.4**: Write tests: copying readtable preserves handlers, modifying copy doesn't affect original
	- [ ] **Step 1.5**: Run `pipenv run pytest -q`

### Task 2: Build basic tokenizer
- [ ] **Goal**: Create code that can break Lisp text into tokens (symbols, numbers, etc.)
	- [ ] **Step 2.1**: Create character stream abstraction that can peek at next character and advance
	- [ ] **Step 2.2**: Handle token assembly with escapes using `|...|` and `\` 
	- [ ] **Step 2.3**: Parse different token types:
		- Integers like `123`
		- Ratios like `1/2` 
		- Floats like `3.14`
		- Symbols like `FOO` 
		- Keywords like `:FOO`
	- [ ] **Step 2.4**: Handle dotted lists like `(a . b)` inside list reader
	- [ ] **Step 2.5**: Write tests for each token type
	- [ ] **Step 2.6**: Run `pipenv run pytest -q`

### Task 3: Add Character type and character literals
- [ ] **Goal**: Support character literals like `#\A` and `#\Space`
	- [ ] **Step 3.1**: Create `Character` class (subclass of base Lisp type) with `code` attribute
	- [ ] **Step 3.2**: Add reader support for `#\A` (single character literals)
	- [ ] **Step 3.3**: Add reader support for named characters: `#\Space`, `#\Newline` (case-insensitive)
	- [ ] **Step 3.4**: Make printer output characters as `#\A` or canonical name for special chars
	- [ ] **Step 3.5**: Write tests: round-trip for alphabetic chars, space, newline
	- [ ] **Step 3.6**: Add test that `TYPEP` returns true for CHARACTER type
	- [ ] **Step 3.7**: Document deferred features: full character attributes, non-ASCII characters
	- [ ] **Step 3.8**: Run `pipenv run pytest -q`

### Task 4: Implement dispatch macros
- [ ] **Goal**: Handle special `#` constructs in Lisp reader
	- [ ] **Step 4.1**: Add `#'` (function shorthand) - can be placeholder for now
	- [ ] **Step 4.2**: Add `#()` vector literal support (store as Python list for now)
	- [ ] **Step 4.3**: Add block comments `#| comment |#` (with nesting support)
	- [ ] **Step 4.4**: Add line comments `;` that skip to end of line
	- [ ] **Step 4.5**: Write tests: nested block comments work, vector length preserved
	- [ ] **Step 4.6**: Run `pipenv run pytest -q`

### Task 5: Connect reader to package system
- [ ] **Goal**: Make reader use proper package interning
	- [ ] **Step 5.1**: When reader sees symbol token, intern it properly
	- [ ] **Step 5.2**: If symbol starts with `:`, intern in KEYWORD package
	- [ ] **Step 5.3**: Otherwise intern in current package
	- [ ] **Step 5.4**: Add dynamic variable `*PACKAGE*` defaulting to COMMON-LISP or USER
	- [ ] **Step 5.5**: Write tests: reading same symbol twice gives same object (identity)
	- [ ] **Step 5.6**: Write test: keywords self-evaluate
	- [ ] **Step 5.7**: Run `pipenv run pytest -q`

### Task 6: Build basic printer
- [ ] **Goal**: Create functions to convert Lisp objects back to text
	- [ ] **Step 6.1**: Implement `prin1` and `princ` functions
	- [ ] **Step 6.2**: Add printing dispatch based on object type: symbol, number, list, string, vector
	- [ ] **Step 6.3**: Make sure strings are quoted properly with escape sequences
	- [ ] **Step 6.4**: Print symbols in uppercase by default (handle case properly)
	- [ ] **Step 6.5**: Write tests: simple round-trip tests for basic forms
	- [ ] **Step 6.6**: Run `pipenv run pytest -q`

### Task 7: Add proper error handling
- [ ] **Goal**: Give good error messages when reading fails
	- [ ] **Step 7.1**: Create base `ReaderError` exception class
	- [ ] **Step 7.2**: Add specific errors: `UnexpectedEOF`, `InvalidNumber`, `UnbalancedParen`
	- [ ] **Step 7.3**: Make reader functions raise appropriate errors
	- [ ] **Step 7.4**: Write tests that expect specific exceptions for malformed input
	- [ ] **Step 7.5**: Run `pipenv run pytest -q`

### Task 8: Create comprehensive round-trip tests
- [ ] **Goal**: Test that reading and printing work together correctly
	- [ ] **Step 8.1**: Create test data with many different Lisp expressions
	- [ ] **Step 8.2**: Include: empty list, dotted list, nested vectors, keywords, strings, numbers
	- [ ] **Step 8.3**: For each test case: read it, print it, read it again, compare
	- [ ] **Step 8.4**: Aim for ≥95% success rate
	- [ ] **Step 8.5**: Document any remaining failing cases
	- [ ] **Step 8.6**: Run `pipenv run pytest -q`

### Task 9: Document what's not implemented yet
- [ ] **Goal**: Keep track of features we haven't built yet  
	- [ ] **Step 9.1**: Create `docs/reader.md` file 
	- [ ] **Step 9.2**: List deferred features like: unread-char, read-base, complex dispatch variants
	- [ ] **Step 9.3**: Update this list as you discover things that need work
	- [ ] **Step 9.4**: Keep this updated before finishing phase

## Important Notes
- Build readtable first, then tokenizer, then dispatch macros, then printer, then error handling, then testing
- Focus on getting basic functionality working before adding advanced features
- Document anything you can't implement right now

## How to Know Phase 2 is Done
✅ All checkboxes above are checked
✅ All tests pass when you run `pipenv run pytest -q`
✅ ≥95% success rate on round-trip test corpus  
✅ Remaining issues are documented
