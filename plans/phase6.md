# Phase 6 – Type System

**CRITICAL**: Run `pipenv run pytest -q` after every task.

## Step-By-Step Tasks (Do These In Order)

### Task 1: Create class system foundation
- [ ] **Goal**: Support DEFCLASS and class objects
	- [ ] **Step 1.1**: Create Class object that stores slot definitions and parent classes
	- [ ] **Step 1.2**: Use simplified linear order: parent -> grandparent -> ... -> T
	- [ ] **Step 1.3**: Implement DEFCLASS (as function if macros not ready yet)
	- [ ] **Step 1.4**: Make DEFCLASS store class definition somewhere accessible
	- [ ] **Step 1.5**: Write tests: define class with slots, generate accessor functions
	- [ ] **Step 1.6**: Run `pipenv run pytest -q`

### Task 2: Support creating instances
- [ ] **Goal**: MAKE-INSTANCE function works
	- [ ] **Step 2.1**: Create instance structure (dict of slot-name -> value)
	- [ ] **Step 2.2**: Apply default values when slots aren't initialized
	- [ ] **Step 2.3**: Support :initarg parameters that map to specific slots
	- [ ] **Step 2.4**: Write tests: initargs set correct slots, defaults applied when missing
	- [ ] **Step 2.5**: Run `pipenv run pytest -q`

### Task 3: Improve TYPEP for all data types
- [ ] **Goal**: TYPEP works for built-in and user-defined types
	- [ ] **Step 3.1**: Create dispatcher that checks type based on object
	- [ ] **Step 3.2**: Handle primitives: numbers, strings, symbols
	- [ ] **Step 3.3**: Handle compound types: cons, arrays, hash-tables
	- [ ] **Step 3.4**: Handle user-defined classes using class metaobjects
	- [ ] **Step 3.5**: Create test matrix: many (object, type) combinations
	- [ ] **Step 3.6**: Run `pipenv run pytest -q`

### Task 4: Add basic generic function support
- [ ] **Goal**: Support simple method dispatch
	- [ ] **Step 4.1**: Create generic function object that holds list of methods
	- [ ] **Step 4.2**: Each method has specializers (types) and a function
	- [ ] **Step 4.3**: Implement ADD-METHOD to register methods sorted by specificity
	- [ ] **Step 4.4**: Implement ENSURE-GENERIC-FUNCTION to create/find generic functions
	- [ ] **Step 4.5**: Make method invocation select most specific matching method
	- [ ] **Step 4.6**: Write tests: redefining generic updates methods, dispatch chooses right one
	- [ ] **Step 4.7**: Run `pipenv run pytest -q`

### Task 5: Add CALL-NEXT-METHOD stub
- [ ] **Goal**: Methods can call next most specific method
	- [ ] **Step 5.1**: Create mechanism to track remaining method chain during dispatch
	- [ ] **Step 5.2**: Implement CALL-NEXT-METHOD that calls next method in chain
	- [ ] **Step 5.3**: Make it raise error if no next method available
	- [ ] **Step 5.4**: Write test: method calling CALL-NEXT-METHOD reaches less specific method
	- [ ] **Step 5.5**: Run `pipenv run pytest -q`

### Task 6: Add comprehensive type tests
- [ ] **Goal**: Test type system thoroughly
	- [ ] **Step 6.1**: Test all built-in type predicates work correctly
	- [ ] **Step 6.2**: Test user-defined classes integrate with type system
	- [ ] **Step 6.3**: Test method dispatch chooses correct methods
	- [ ] **Step 6.4**: Test inheritance works as expected
	- [ ] **Step 6.5**: Run `pipenv run pytest -q`

## Important Notes
- This is simplified CLOS - just basic single dispatch
- Focus on getting type checking and basic method dispatch working
- Don't worry about advanced CLOS features like method combinations

## How to Know Phase 6 is Done
✅ All checkboxes above are checked  
✅ All tests pass when you run `pipenv run pytest -q`
✅ TYPEP works for built-in and user-defined types
✅ Basic single dispatch method calls work

- [ ] Tests: TYPEP matrix, simple generic dispatch.
	- [ ] Consolidate into dedicated test module.

## Order Guidance
Define classes before generic functions; implement TYPEP before relying on type checks inside method dispatch.

## Exit Criteria
Basic dispatch working; missing MOP features documented.
