# Phase 4 – Multiple Values and Error Handling

**CRITICAL**: Run `pipenv run pytest -q` after every task.

## Step-By-Step Tasks (Do These In Order)

### Task 1: Support multiple return values
- [ ] **Goal**: Functions can return more than one value
	- [ ] **Step 1.1**: Choose representation: Python tuple with special marker or custom class
	- [ ] **Step 1.2**: Update evaluator to handle multiple values in return statements
	- [ ] **Step 1.3**: Handle multiple values in RETURN-FROM and function returns
	- [ ] **Step 1.4**: Write tests: function returning 0, 1, or 3 values
	- [ ] **Step 1.5**: Run `pipenv run pytest -q`

### Task 2: Implement multiple value functions  
- [ ] **Goal**: Add VALUES, MULTIPLE-VALUE-BIND, etc.
	- [ ] **Step 2.1**: Implement VALUES function to create multiple values
	- [ ] **Step 2.2**: Implement VALUES-LIST to create multiple values from list
	- [ ] **Step 2.3**: Implement MULTIPLE-VALUE-CALL for calling function with multiple args
	- [ ] **Step 2.4**: Implement MULTIPLE-VALUE-BIND to destructure multiple values  
	- [ ] **Step 2.5**: Implement NTH-VALUE to extract specific value
	- [ ] **Step 2.6**: Write tests for each function
	- [ ] **Step 2.7**: Run `pipenv run pytest -q`

### Task 3: Create condition (error) hierarchy
- [ ] **Goal**: Structured error/warning system like ANSI Common Lisp
	- [ ] **Step 3.1**: Create base `Condition` class 
	- [ ] **Step 3.2**: Create subclasses: `Simple-Condition`, `Error`, `Warning`
	- [ ] **Step 3.3**: Create specific errors: `Type-Error`, `File-Error`, etc.
	- [ ] **Step 3.4**: Add slot accessors for condition data
	- [ ] **Step 3.5**: Write tests: check subclass relationships with TYPEP
	- [ ] **Step 3.6**: Run `pipenv run pytest -q`

### Task 4: Add signaling functions
- [ ] **Goal**: Functions to signal errors and warnings
	- [ ] **Step 4.1**: Implement SIGNAL function (raises Python exception mapped to condition)
	- [ ] **Step 4.2**: Implement ERROR function (signals error condition)
	- [ ] **Step 4.3**: Implement CERROR function (error with built-in continue restart)
	- [ ] **Step 4.4**: Implement WARN function (signals warning)
	- [ ] **Step 4.5**: Write tests: capturing condition objects, continuing on CERROR
	- [ ] **Step 4.6**: Run `pipenv run pytest -q`

### Task 5: Add restart system
- [ ] **Goal**: Allow recovery from errors with restarts
	- [ ] **Step 5.1**: Create dynamic restart stack (list of restart frames)
	- [ ] **Step 5.2**: Each restart frame has name->function mapping
	- [ ] **Step 5.3**: Implement RESTART-CASE to establish restarts
	- [ ] **Step 5.4**: Implement RESTART-BIND to bind restart functions
	- [ ] **Step 5.5**: Implement INVOKE-RESTART to call a restart by name
	- [ ] **Step 5.6**: Implement ABORT restart (exits or raises if no abort restart)
	- [ ] **Step 5.7**: Write tests: nested restarts, selecting non-top restart
	- [ ] **Step 5.8**: Run `pipenv run pytest -q`

## How to Know Phase 4 is Done
✅ All checkboxes above are checked
✅ All tests pass when you run `pipenv run pytest -q`
✅ Multiple values work correctly
✅ Error handling and recovery with restarts works
	- [ ] Scenario: SIGNAL inside with restarts offered; choosing one alters control flow as expected.

- [ ] Document any deferred condition types.
	- [ ] Add section to docs listing omitted or simplified types.

## Order Guidance
Implement multiple values first (needed by NTH-VALUE etc.), then conditions signaling, then restarts; documentation last.

## Exit Criteria
All implemented functions tested; restart scenario passes.
