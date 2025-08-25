# Phase 0 – Fix Basic Problems

**CRITICAL**: Run `pipenv run pytest -q` after EVERY task. If tests fail, fix them before moving on.

**REMEMBER**: Use PowerShell syntax! Use `;` not `&&` to join commands.

## Current Phase 0 Status
- [x] Task 1: Fix `cddddr` function ✅ (Already complete)
- [ ] Task 2: Fix predicate functions (Next to work on)
- [ ] Task 3: Move reader functions 
- [ ] Task 4: Remove duplicate function definitions
- [ ] Task 5: Replace package stubs
- [ ] Task 6: Add test to catch duplicate registrations
- [ ] Task 7: Fix printer to show T and NIL correctly  
- [ ] Task 8: Add basic round-trip test

## Step-By-Step Tasks (Do These In Order)

### Task 1: Fix the `cddddr` function
- [x] Fix `cddddr` bug in `fclpy/lispfunc/core.py`. (Verified present and correct)
	- (Done) Confirm implementation: nested four `cdr` calls.
	- (Done) Unit test exists & passes.

### Task 2: Fix predicate functions to return T and NIL instead of True and False
- [x] Make sure canonical `T` symbol exists in `lisptype.py`.
	- (Done) `T` symbol present in `lisptype.py`.

- [ ] Find and fix predicates that return Python True/False instead of Lisp T/NIL.
	- [x] **Step 2.1**: Ran search for `return True` / `return False` (see test log) (Done 2025-08-26)
	- [ ] **Step 2.2**: For each file that shows up, open it and find lines with `return True` or `return False`
		- Progress: characters.py in progress
	- [ ] **Step 2.3**: Change `return True` to `return lisptype.T` and `return False` to `return lisptype.NIL`
		- Progress: updating characters.py predicates
	- [ ] **Step 2.4**: If you see complex expressions, wrap them like: `return lisptype.lisp_bool(some_complex_test)`
	- [ ] **Step 2.5**: Add tests to check that predicates like `(symbolp 'x)` return `T` not Python `True`
		- Planned: add new test file test_predicates_characters.py
	- [ ] **Step 2.6**: Run tests after each file you change: `pipenv run pytest -q`

### Task 3: Move reader functions to one place
- [ ] **Goal**: Put all reader-related functions in one file called `readtable.py`
	- [ ] **Step 3.1**: Look for readtable functions in `fclpy/io.py` and `fclpy/lispfunc/utilities.py`
	- [ ] **Step 3.2**: Find functions with names like `*READTABLE*`, `copy_readtable`, `make_dispatch_macro_character`
	- [ ] **Step 3.3**: Create new file `fclpy/readtable.py` if it doesn't exist  
	- [ ] **Step 3.4**: Move all readtable functions to this new file
	- [ ] **Step 3.5**: Update any other files that import these functions to import from the new location
	- [ ] **Step 3.6**: Add tests that copying a readtable works and macro characters can be installed
	- [ ] **Step 3.7**: Run `pipenv run pytest -q` to make sure nothing broke

### Task 4: Remove duplicate function definitions
- [ ] **Goal**: Make sure each Lisp function is only defined once
	- [ ] **Step 4.1**: Create a script to find duplicates (or check manually)
	- [ ] **Step 4.2**: Look through files in `fclpy/lispfunc/` for functions with same `@_registry.cl_function('NAME')`  
	- [ ] **Step 4.3**: When you find duplicates, keep the best implementation and delete the others
	- [ ] **Step 4.4**: Make sure the environment only loads functions from the registry, not from old hard-coded lists
	- [ ] **Step 4.5**: Run `pipenv run pytest -q` to check everything still works

### Task 5: Replace package stubs with real Package objects
- [ ] **Goal**: Use real Package objects instead of fake stub functions  
	- [ ] **Step 5.1**: Find stub functions in `fclpy/lispfunc/utilities.py` that just return `None` or raise errors
	- [ ] **Step 5.2**: Look for functions like `find-package`, `intern`, `export`, `use-package`
	- [ ] **Step 5.3**: Check if `lisptype.Package` class exists. If not, create a simple one with: name, nicknames, symbol table, use list
	- [ ] **Step 5.4**: Implement real `find_package` that actually finds packages by name
	- [ ] **Step 5.5**: Implement real `intern` that actually adds symbols to packages and returns same object for same symbol
	- [ ] **Step 5.6**: Update the registry decorators to use the real package functions
	- [ ] **Step 5.7**: Add tests that interning the same symbol twice returns the same object
	- [ ] **Step 5.8**: Add test that keywords like `:FOO` evaluate to themselves

### Task 6: Add test to catch duplicate registrations  
- [ ] **Goal**: Make sure we never accidentally register the same function twice
	- [ ] **Step 6.1**: Write a function `collect_function_symbols()` that gets all registered function names
	- [ ] **Step 6.2**: Write a test that calls this function and checks `len(set(names)) == len(names)`
	- [ ] **Step 6.3**: Run the test - it should pass (no duplicates)
	- [ ] **Step 6.4**: If test fails, find and fix the duplicate registrations

### Task 7: Fix printer to show T and NIL correctly
- [ ] **Goal**: When printing Lisp values, show `T` and `NIL` not Python `True`/`False`
	- [ ] **Step 7.1**: Find the printer functions (probably in `fclpy/io.py` or similar)
	- [ ] **Step 7.2**: Make sure `lisp_bool` function always returns `T` and `NIL` symbols
	- [ ] **Step 7.3**: Update printer to print symbol names correctly  
	- [ ] **Step 7.4**: Add test that printing `(list t nil)` shows `(T NIL)` not `(True False)`
	- [ ] **Step 7.5**: Run `pipenv run pytest -q`

### Task 8: Add basic round-trip test
- [ ] **Goal**: Test that we can read a simple expression and print it back correctly
	- [ ] **Step 8.1**: Write test that uses printer to convert `(A B C)` to string
	- [ ] **Step 8.2**: Feed that string to reader and make sure we get back the same structure
	- [ ] **Step 8.3**: Test the same thing with a keyword like `:FOO`
	- [ ] **Step 8.4**: Make sure reading same symbol twice gives identical objects (use `is` in Python)
	- [ ] **Step 8.5**: If this test fails, mark it as expected to fail until reader/packages are better implemented
	- [ ] **Step 8.6**: Run `pipenv run pytest -q`

## Important Notes
- Do these tasks in the exact order shown above
- Don't skip steps within a task
- Reader and package improvements can be done at same time, but finish completely before testing duplicate registrations
- If you get stuck on a task after trying twice, write down the problem in the main plan file section 18

## How to Know Phase 0 is Done
✅ All checkboxes above are checked
✅ All tests pass when you run `pipenv run pytest -q`
✅ No Python `True`/`False` values are returned by predicate functions
✅ No duplicate function registrations exist
