# Phase 3 – Basic Evaluation

**CRITICAL**: Run `pipenv run pytest -q` after every task.

## Step-By-Step Tasks (Do These In Order)

### Task 1: Create special form dispatcher
- [x] **Goal**: Handle special Lisp forms like IF, QUOTE, PROGN ✅ 15 tests
	- [x] **Step 1.1**: Create list of special forms (QUOTE, IF, PROGN, etc.)
	- [x] **Step 1.2**: Create dispatcher function that maps symbol name to evaluator function
	- [x] **Step 1.3**: Implement simple evaluators for QUOTE (returns argument unchanged)
	- [x] **Step 1.4**: Implement IF evaluator (if test then-form else-form)  
	- [x] **Step 1.5**: Implement PROGN evaluator (evaluate all forms, return last result)
	- [x] **Step 1.6**: Add tests for each special form
	- [x] **Step 1.7**: Run `pipenv run pytest -q`

### Task 2: Add dynamic binding support
- [x] **Goal**: Support special variables that can be dynamically bound ✅ 12 tests
	- [x] **Step 2.1**: Create dynamic environment stack separate from lexical environment
	- [x] **Step 2.2**: When binding SPECIAL variables, save old value and restore after scope
	- [x] **Step 2.3**: Implement `LET` and `LET*` with awareness of special variables
	- [x] **Step 2.4**: Use heuristic: names like `*package*` (with asterisks) are special for now
	- [x] **Step 2.5**: Create helper functions `bind_dynamic(symbol, value)` and `lookup_dynamic(symbol)`  
	- [x] **Step 2.6**: Write tests: nested dynamic bindings, lexical vs dynamic variable lookup
	- [x] **Step 2.7**: Run `pipenv run pytest -q`

### Task 3: Build lambda list parser
- [x] **Goal**: Parse function parameter lists with &optional, &rest, &key ✅ 11 tests
	- [x] **Step 3.1**: Define grammar for different parameter types
	- [x] **Step 3.2**: Create parser that produces structured object with: regular params, optional params with defaults, rest param, keyword params
	- [x] **Step 3.3**: Handle &optional parameters with default values
	- [x] **Step 3.4**: Handle &rest parameter (collects remaining arguments)
	- [x] **Step 3.5**: Handle &key parameters (keyword arguments)  
	- [x] **Step 3.6**: Handle &aux parameters (local variables)
	- [x] **Step 3.7**: Write tests for different lambda list combinations
	- [x] **Step 3.8**: Run `pipenv run pytest -q`

### Task 4: Separate macros from functions  
- [x] **Goal**: Track which names are macros vs functions ✅ 8 tests
	- [x] **Step 4.1**: Extend registry entry `kind` field to include `macro`
	- [x] **Step 4.2**: Make environment lookup distinguish between macro and function names
	- [x] **Step 4.3**: Decide: can function and macro have same name? (Document if not supported initially)
	- [x] **Step 4.4**: Write tests for macro vs function namespace separation
	- [x] **Step 4.5**: Run `pipenv run pytest -q`

### Task 5: Implement basic macro system
- [x] **Goal**: Support DEFMACRO, MACROEXPAND, etc. ✅ 9 tests
	- [x] **Step 5.1**: Store macro expander functions in registry with kind=macro
	- [x] **Step 5.2**: Implement DEFMACRO to register macro functions
	- [x] **Step 5.3**: Implement MACRO-FUNCTION to retrieve macro expanders
	- [x] **Step 5.4**: Implement MACROEXPAND-1 (expand once) and MACROEXPAND (expand until done)
	- [x] **Step 5.5**: Create expansion loop that stops when result is not a macro call
	- [x] **Step 5.6**: Write tests: simple identity macro, macro with &rest, nested expansion chain  
	- [x] **Step 5.7**: Run `pipenv run pytest -q`

### Task 6: Add backquote support
- [x] **Goal**: Support `` `(a ,b ,@c)`` quasiquote syntax ✅ 11 tests (2 skipped)
	- [x] **Step 6.1**: Implement recursive backquote expander 
	- [x] **Step 6.2**: Handle quasiquote in lists: `` `(a b c)``
	- [x] **Step 6.3**: Handle unquote: `` `(a ,variable b)``
	- [x] **Step 6.4**: Handle unquote-splicing: `` `(a ,@list b)``
	- [x] **Step 6.5**: Handle backquote in vectors if vectors exist
	- [x] **Step 6.6**: Write tests with examples: `` `(a ,b ,@c d)``
	- [x] **Step 6.7**: Run `pipenv run pytest -q`

### Task 7: Add non-local exits
- [x] **Goal**: Support BLOCK/RETURN-FROM, TAGBODY/GO, CATCH/THROW ✅ 16 tests
	- [x] **Step 7.1**: Implement BLOCK/RETURN-FROM using Python exceptions with tag and values ✅
	- [x] **Step 7.2**: Implement TAGBODY/GO (simplified: label search + exception jump) ✅
	- [x] **Step 7.3**: Implement CATCH/THROW with exception matching ✅
	- [x] **Step 7.4**: Document any limitations of the implementation  
	- [x] **Step 7.5**: Write tests: nested blocks, throw across call frames
	- [x] **Step 7.6**: Run `pipenv run pytest -q` ✅ All passing

### Task 8: Add cleanup support
- [x] **Goal**: Support UNWIND-PROTECT for cleanup code ✅ 7 tests
	- [x] **Step 8.1**: Wrap evaluation with try/finally to ensure cleanup runs ✅
	- [x] **Step 8.2**: Make sure cleanup executes on both normal and non-local exits ✅
	- [x] **Step 8.3**: Write test that raising exception inside protected form still runs cleanup ✅
	- [x] **Step 8.4**: Run `pipenv run pytest -q`

### Task 9: Add multiple values placeholder
- [x] **Goal**: Basic support for functions returning multiple values ✅ 12 tests
	- [x] **Step 9.1**: Create simple multiple-value system (tuple-based) ✅
	- [x] **Step 9.2**: Update evaluator to handle multiple values in some cases ✅
	- [x] **Step 9.3**: Document what's not fully implemented yet ✅
	- [x] **Step 9.4**: Add basic tests for multiple value returns ✅
	- [x] **Step 9.5**: Run `pipenv run pytest -q`

## Important Notes
- Build special form dispatcher first, then dynamic binding, then lambda lists, then macros
- Non-local exits can be simplified initially - document limitations
- Multiple values support can be minimal for now

## How to Know Phase 3 is Done
✅ All checkboxes above are checked
✅ All tests pass: **508 passing, 0 skipped** in `pipenv run pytest -q`
✅ Basic evaluation of simple expressions works
✅ Macro expansion works for simple cases
✅ Non-local exits (BLOCK/RETURN-FROM, CATCH/THROW, TAGBODY/GO) with exception-based control flow
✅ UNWIND-PROTECT cleanup support using try/finally
✅ Basic multiple values support (tuple-based representation)

## PHASE 3 COMPLETE ✅
**All 9 tasks successfully implemented and tested!**

**Test Breakdown by Task:**
- Task 1: 15 tests
- Task 2: 12 tests  
- Task 3: 11 tests
- Task 4: 8 tests
- Task 5: 9 tests
- Task 6: 13 tests (backquote in macros now works)
- Task 7: 16 tests (TAGBODY/GO fully implemented)
- Task 8: 7 tests
- Task 9: 12 tests

**Final Metrics: 508 passing, 0 skipped**

## Order Guidance
Implement minimal evaluator & macro system before advanced control transfers; add non-local exits after macro correctness to simplify debugging.

## Exit Criteria
Macroexpansion golden tests + special form tests green.
