# Phase 1 – Core Symbol System

**CRITICAL**: Run `pipenv run pytest -q` after every task.

## Step-By-Step Tasks (Do These In Order)

### Task 1: Add slots to LispSymbol class
- [ ] **Goal**: Make symbols able to store values, functions, and property lists
	- [ ] **Step 1.1**: Open the file where `LispSymbol` class is defined (probably `lisptype.py`)
	- [ ] **Step 1.2**: Add three new attributes to the class: `value`, `function`, `plist`
	- [ ] **Step 1.3**: Set default values: `value=None`, `function=None`, `plist=[]`
	- [ ] **Step 1.4**: Make sure the constructor still works the same way (backward compatibility)
	- [ ] **Step 1.5**: Add simple accessor functions if needed for `symbol-value`, `symbol-function`, `symbol-plist` 
	- [ ] **Step 1.6**: Write tests: create a symbol, set its value/function/plist, read them back
	- [ ] **Step 1.7**: Run `pipenv run pytest -q`

### Task 2: Make keywords work properly
- [ ] **Goal**: Keywords like `:FOO` should evaluate to themselves
	- [ ] **Step 2.1**: Make sure KEYWORD package exists (look in package system code)
	- [ ] **Step 2.2**: Create helper function `intern_keyword(name)` that puts keywords in KEYWORD package
	- [ ] **Step 2.3**: Make sure keyword symbols return themselves when evaluated (self-evaluating)
	- [ ] **Step 2.4**: Write test: evaluate `:FOO` returns the same `:FOO` object
	- [ ] **Step 2.5**: Write test: printing a keyword shows the colon like `:FOO` not `FOO`
	- [ ] **Step 2.6**: Run `pipenv run pytest -q`

### Task 3: Use packages everywhere for symbol creation
- [ ] **Goal**: Stop creating symbols directly, use proper package interning instead
	- [ ] **Step 3.1**: Search for `LispSymbol(` in all Python files
	- [ ] **Step 3.2**: For each one, replace with proper `intern` call or `intern_keyword` call  
	- [ ] **Step 3.3**: Make sure all symbols go through the package system
	- [ ] **Step 3.4**: Add a test that fails if anyone creates symbols with bare `LispSymbol(` constructor 
	- [ ] **Step 3.5**: Update any modules that were using direct symbol creation
	- [ ] **Step 3.6**: Run `pipenv run pytest -q`

### Task 4: Improve the registry system
- [ ] **Goal**: Store more information about each registered function
	- [ ] **Step 4.1**: Create a dataclass called `RegistryEntry` with fields: name, kind (function/macro), arg_spec, documentation
	- [ ] **Step 4.2**: Update the `@cl_function` decorator to create these objects instead of simple dicts
	- [ ] **Step 4.3**: Make environment initialization use this registry to bind functions and macros
	- [ ] **Step 4.4**: Add `kind` field so we can handle macros later (set to 'function' for now)
	- [ ] **Step 4.5**: Create a tool that can generate markdown showing what's implemented
	- [ ] **Step 4.6**: Write test: check that registry has the expected number of entries and they're all unique
	- [ ] **Step 4.7**: Run `pipenv run pytest -q`

### Task 5: Remove old function mapping code
- [ ] **Goal**: Delete old ways of storing function mappings
	- [ ] **Step 5.1**: Search for variables like `function_mappings` or similar old structures
	- [ ] **Step 5.2**: Delete them or replace them with registry-based lookups
	- [ ] **Step 5.3**: Make sure no code still uses the old global function mapping variables
	- [ ] **Step 5.4**: Add test that old global names either don't exist or are aliases to new registry
	- [ ] **Step 5.5**: Run `pipenv run pytest -q`

### Task 6: Add comprehensive tests
- [ ] **Goal**: Test all the symbol and registry improvements
	- [ ] **Step 6.1**: Create file `tests/test_symbols.py` (or add to existing symbol test file)
	- [ ] **Step 6.2**: Test that symbols have identity (same symbol interned twice = same object)
	- [ ] **Step 6.3**: Test that keywords evaluate to themselves
	- [ ] **Step 6.4**: Test that registry is complete (expected number of functions registered)
	- [ ] **Step 6.5**: Run `pipenv run pytest -q` and make sure all tests pass

## Important Notes  
- Do symbol slot extension first, then registry improvements, then enforce package interning
- This will minimize having to edit the same code twice
- Make sure backward compatibility - don't break existing code

## How to Know Phase 1 is Done
✅ All checkboxes above are checked
✅ All tests pass when you run `pipenv run pytest -q` 
✅ Registry is the single source of truth for function/macro information
✅ All symbols go through proper package interning system
