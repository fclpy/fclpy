# Phase 3 – Basic Evaluation

**CRITICAL**: Run `pipenv run pytest -q` after every task.

## Step-By-Step Tasks (Do These In Order)

### Task 1: Create special form dispatcher
- [ ] **Goal**: Handle special Lisp forms like IF, QUOTE, PROGN
	- [ ] **Step 1.1**: Create list of special forms (QUOTE, IF, PROGN, etc.)
	- [ ] **Step 1.2**: Create dispatcher function that maps symbol name to evaluator function
	- [ ] **Step 1.3**: Implement simple evaluators for QUOTE (returns argument unchanged)
	- [ ] **Step 1.4**: Implement IF evaluator (if test then-form else-form)  
	- [ ] **Step 1.5**: Implement PROGN evaluator (evaluate all forms, return last result)
	- [ ] **Step 1.6**: Add tests for each special form
	- [ ] **Step 1.7**: Run `pipenv run pytest -q`

### Task 2: Add dynamic binding support
- [ ] **Goal**: Support special variables that can be dynamically bound
	- [ ] **Step 2.1**: Create dynamic environment stack separate from lexical environment
	- [ ] **Step 2.2**: When binding SPECIAL variables, save old value and restore after scope
	- [ ] **Step 2.3**: Implement `LET` and `LET*` with awareness of special variables
	- [ ] **Step 2.4**: Use heuristic: names like `*package*` (with asterisks) are special for now
	- [ ] **Step 2.5**: Create helper functions `bind_dynamic(symbol, value)` and `lookup_dynamic(symbol)`  
	- [ ] **Step 2.6**: Write tests: nested dynamic bindings, lexical vs dynamic variable lookup
	- [ ] **Step 2.7**: Run `pipenv run pytest -q`

### Task 3: Build lambda list parser
- [ ] **Goal**: Parse function parameter lists with &optional, &rest, &key
	- [ ] **Step 3.1**: Define grammar for different parameter types
	- [ ] **Step 3.2**: Create parser that produces structured object with: regular params, optional params with defaults, rest param, keyword params
	- [ ] **Step 3.3**: Handle &optional parameters with default values
	- [ ] **Step 3.4**: Handle &rest parameter (collects remaining arguments)
	- [ ] **Step 3.5**: Handle &key parameters (keyword arguments)  
	- [ ] **Step 3.6**: Handle &aux parameters (local variables)
	- [ ] **Step 3.7**: Write tests for different lambda list combinations
	- [ ] **Step 3.8**: Run `pipenv run pytest -q`

### Task 4: Separate macros from functions  
- [ ] **Goal**: Track which names are macros vs functions
	- [ ] **Step 4.1**: Extend registry entry `kind` field to include `macro`
	- [ ] **Step 4.2**: Make environment lookup distinguish between macro and function names
	- [ ] **Step 4.3**: Decide: can function and macro have same name? (Document if not supported initially)
	- [ ] **Step 4.4**: Write tests for macro vs function namespace separation
	- [ ] **Step 4.5**: Run `pipenv run pytest -q`

### Task 5: Implement basic macro system
- [ ] **Goal**: Support DEFMACRO, MACROEXPAND, etc.
	- [ ] **Step 5.1**: Store macro expander functions in registry with kind=macro
	- [ ] **Step 5.2**: Implement DEFMACRO to register macro functions
	- [ ] **Step 5.3**: Implement MACRO-FUNCTION to retrieve macro expanders
	- [ ] **Step 5.4**: Implement MACROEXPAND-1 (expand once) and MACROEXPAND (expand until done)
	- [ ] **Step 5.5**: Create expansion loop that stops when result is not a macro call
	- [ ] **Step 5.6**: Write tests: simple identity macro, macro with &rest, nested expansion chain  
	- [ ] **Step 5.7**: Run `pipenv run pytest -q`

### Task 6: Add backquote support
- [ ] **Goal**: Support `` `(a ,b ,@c)`` quasiquote syntax
	- [ ] **Step 6.1**: Implement recursive backquote expander 
	- [ ] **Step 6.2**: Handle quasiquote in lists: `` `(a b c)``
	- [ ] **Step 6.3**: Handle unquote: `` `(a ,variable b)``
	- [ ] **Step 6.4**: Handle unquote-splicing: `` `(a ,@list b)``
	- [ ] **Step 6.5**: Handle backquote in vectors if vectors exist
	- [ ] **Step 6.6**: Write tests with examples: `` `(a ,b ,@c d)``
	- [ ] **Step 6.7**: Run `pipenv run pytest -q`

### Task 7: Add non-local exits
- [ ] **Goal**: Support BLOCK/RETURN-FROM, TAGBODY/GO, CATCH/THROW
	- [ ] **Step 7.1**: Implement BLOCK/RETURN-FROM using Python exceptions with tag and values
	- [ ] **Step 7.2**: Implement TAGBODY/GO (simplified: label search + exception jump)
	- [ ] **Step 7.3**: Implement CATCH/THROW with dynamic binding stack
	- [ ] **Step 7.4**: Document any limitations of the implementation  
	- [ ] **Step 7.5**: Write tests: nested blocks, throw across call frames
	- [ ] **Step 7.6**: Run `pipenv run pytest -q`

### Task 8: Add cleanup support
- [ ] **Goal**: Support UNWIND-PROTECT for cleanup code
	- [ ] **Step 8.1**: Wrap evaluation with try/finally to ensure cleanup runs
	- [ ] **Step 8.2**: Make sure cleanup executes on both normal and non-local exits
	- [ ] **Step 8.3**: Write test that raising exception inside protected form still runs cleanup
	- [ ] **Step 8.4**: Run `pipenv run pytest -q`

### Task 9: Add multiple values placeholder
- [ ] **Goal**: Basic support for functions returning multiple values
	- [ ] **Step 9.1**: Create simple multiple-value system (can be basic for now)
	- [ ] **Step 9.2**: Update evaluator to handle multiple values in some cases
	- [ ] **Step 9.3**: Document what's not fully implemented yet
	- [ ] **Step 9.4**: Add basic tests for multiple value returns  
	- [ ] **Step 9.5**: Run `pipenv run pytest -q`

## Important Notes
- Build special form dispatcher first, then dynamic binding, then lambda lists, then macros
- Non-local exits can be simplified initially - document limitations
- Multiple values support can be minimal for now

## How to Know Phase 3 is Done
✅ All checkboxes above are checked
✅ All tests pass when you run `pipenv run pytest -q`
✅ Basic evaluation of simple expressions works
✅ Macro expansion works for simple cases
	- [ ] Accept internal tuple sentinel from Phase 4 design placeholder (if Phase 4 not done, stub capturing first value only; document restriction).

## Order Guidance
Implement minimal evaluator & macro system before advanced control transfers; add non-local exits after macro correctness to simplify debugging.

## Exit Criteria
Macroexpansion golden tests + special form tests green.
