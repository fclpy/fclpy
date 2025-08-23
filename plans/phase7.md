# Phase 7 – Declarations and Tools

**CRITICAL**: Run `pipenv run pytest -q` after every task.

## Step-By-Step Tasks (Do These In Order)

### Task 1: Parse declarations
- [ ] **Goal**: Handle DECLARE and DECLAIM forms  
	- [ ] **Step 1.1**: Extend parser to recognize `(declare (optimize ...) (special ...) (type ... var))`
	- [ ] **Step 1.2**: Store declaration metadata on symbol property lists or in registry
	- [ ] **Step 1.3**: Handle DECLAIM for global declarations
	- [ ] **Step 1.4**: Write tests: macro expansion preserves declaration structure
	- [ ] **Step 1.5**: Run `pipenv run pytest -q`

### Task 2: Add documentation support
- [ ] **Goal**: DOCUMENTATION function retrieves docstrings
	- [ ] **Step 2.1**: Implement DOCUMENTATION function that checks registry and symbol plists
	- [ ] **Step 2.2**: Allow DEFUN and DEFMACRO to store optional documentation strings
	- [ ] **Step 2.3**: Store documentation in accessible place (registry or symbol plist)
	- [ ] **Step 2.4**: Write tests: set docstring via DEFUN, retrieve with DOCUMENTATION
	- [ ] **Step 2.5**: Run `pipenv run pytest -q`

### Task 3: Add optimization policy placeholders
- [ ] **Goal**: Handle optimization declarations (even if they don't do anything yet)
	- [ ] **Step 3.1**: Create global optimization policy object with speed, safety, debug settings  
	- [ ] **Step 3.2**: Make DECLAIM (optimize ...) update this policy
	- [ ] **Step 3.3**: Policy changes don't affect code generation yet (just stored)
	- [ ] **Step 3.4**: Write tests: policy gets updated by declarations
	- [ ] **Step 3.5**: Run `pipenv run pytest -q`

### Task 4: Create coverage reporting tool
- [ ] **Goal**: Track which ANSI symbols are implemented
	- [ ] **Step 4.1**: Create file `docs/ansi_targets.txt` with canonical ANSI symbol list  
	- [ ] **Step 4.2**: Create script `scripts/coverage.py` that reads target list
	- [ ] **Step 4.3**: Compare targets with current registry to see what's implemented
	- [ ] **Step 4.4**: Generate markdown table showing: Implemented, Stub, Missing
	- [ ] **Step 4.5**: Add option to fail if coverage goes below threshold
	- [ ] **Step 4.6**: Write test that coverage script runs without errors
	- [ ] **Step 4.7**: Optionally: auto-update README section with coverage info
	- [ ] **Step 4.8**: Run `pipenv run pytest -q`

## Important Notes
- Parse declarations first since documentation and optimization depend on it
- Coverage tooling should come last to reflect final implementation state
- Optimization doesn't need to actually optimize yet - just store the settings

## How to Know Phase 7 is Done
✅ All checkboxes above are checked
✅ All tests pass when you run `pipenv run pytest -q`  
✅ Declarations are parsed and stored properly
✅ Documentation system works
✅ Coverage reporting shows current implementation status

## Exit Criteria
Coverage script integrated; declarations persisted.
