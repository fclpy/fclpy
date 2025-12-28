# Plan: Running ANSI Test Suite with FCLPY

## Current State Analysis

### What FCLPY Has
1. **CLI exists** (`run.py`): Can load and evaluate Lisp files
2. **Basic evaluation works**: `(+ 1 2)` → `3`
3. **Package system**: `IN-PACKAGE` function works
4. **Many CL functions**: 825+ function bindings registered
5. **File loading**: `load_and_evaluate_file()` in runtime.py

### What ANSI Test Suite Requires
The test suite (`ansi-test/`) needs:

1. **RT (Regression Test) framework** - `rt.lsp` provides DEFTEST macro
2. **Special variables**:
   - `*LOAD-TRUENAME*` - pathname of file being loaded
   - `*COMPILE-FILE-TRUENAME*` - pathname of file being compiled
   - `*DEFAULT-PATHNAME-DEFAULTS*` - default pathname
   - `*LOAD-PATHNAME*` - logical pathname being loaded
3. **Pathname operations**: `MAKE-PATHNAME`, `MERGE-PATHNAMES`, `PATHNAME-DIRECTORY` (partially implemented)
4. **DEFVAR special form** - currently raises `LispNotImplementedError`
5. **LET/LET*** - need to verify working
6. **HANDLER-BIND** - for error handling
7. **DECLAIM/DECLARE** - for declarations
8. **LOOP macro** - partially implemented
9. **Multiple values** - `VALUES`, `MULTIPLE-VALUE-BIND`
10. **Condition system** - `SIGNAL`, `ERROR`, `HANDLER-CASE`

---

## Implementation Plan

### Phase 1: Critical Infrastructure (Priority: HIGH)

#### Task 1.1: Implement DEFVAR special form
- Currently raises `LispNotImplementedError`
- Need to bind variable in environment with initial value
- Support documentation string

#### Task 1.2: Add *LOAD-TRUENAME* and *LOAD-PATHNAME* 
- Set before loading a file
- Restore after loading
- Need to be dynamic variables

#### Task 1.3: Add *DEFAULT-PATHNAME-DEFAULTS*
- Initialize to current directory
- Used by pathname functions

#### Task 1.4: Verify/fix file LOAD function
- Currently `runtime.load_and_evaluate_file()` exists
- Need `(LOAD filename)` Lisp function
- Set `*LOAD-TRUENAME*` during load

### Phase 2: RT Framework Support (Priority: HIGH)

#### Task 2.1: Ensure DEFMACRO works correctly
- RT framework heavily uses macros
- `DEFTEST` is a macro

#### Task 2.2: Verify MAKE-HASH-TABLE and hash operations
- RT uses hash tables for test registry

#### Task 2.3: Ensure LOOP basics work
- RT uses LOOP for iterating tests

#### Task 2.4: Verify HANDLER-BIND/HANDLER-CASE
- Needed for `*CATCH-ERRORS*` functionality

### Phase 3: Core CL Features for Tests (Priority: MEDIUM)

#### Task 3.1: FLET/LABELS local functions
- Many tests use local function bindings

#### Task 3.2: MULTIPLE-VALUE-BIND
- Tests check multiple return values

#### Task 3.3: Complete FORMAT function
- Tests produce formatted output

#### Task 3.4: COMPILE-FILE basics (can be stub)
- Test suite has compile-and-load.lsp
- Stub that just loads would work initially

### Phase 4: Test Runner Integration (Priority: MEDIUM)

#### Task 4.1: Create fclpy-specific init file
- Like `init.lsp` but for fclpy
- Disable tests known to fail
- Set up expected-failures

#### Task 4.2: Add `--load` CLI option
- Load multiple files in sequence
- Support for `(load "file.lsp")` calls

#### Task 4.3: Create test runner script
- Python script to run ansi-test
- Capture results
- Generate report

---

## Detailed Implementation Steps

### Step 1: Implement DEFVAR (evaluation_special_forms.py)

```python
def eval_defvar(form, env):
    """Evaluate DEFVAR special form.
    
    (DEFVAR name)           - declares special variable
    (DEFVAR name value)     - declares and initializes
    (DEFVAR name value doc) - with documentation
    """
    args = cdr(form)
    name = car(args)
    
    # Get current value if exists
    current = env.find_variable(name)
    
    if _consp_internal(cdr(args)) and current is None:
        # Has initial value and not already bound
        value_form = car(cdr(args))
        value = eval(value_form, env)
        env.add_variable(name, value)
    elif current is None:
        # Just declare, bind to NIL
        env.add_variable(name, lisptype.NIL)
    
    return name
```

### Step 2: Add special variables to lispenv.py

```python
def setup_standard_environment():
    # ... existing code ...
    
    # Add special variables
    env.add_variable(LispSymbol('*LOAD-TRUENAME*'), NIL)
    env.add_variable(LispSymbol('*LOAD-PATHNAME*'), NIL)
    env.add_variable(LispSymbol('*COMPILE-FILE-TRUENAME*'), NIL)
    env.add_variable(LispSymbol('*DEFAULT-PATHNAME-DEFAULTS*'), 
                     make_pathname_from_string(os.getcwd()))
```

### Step 3: Update LOAD function (create if not exists)

```python
@_registry.cl_function('LOAD')
def load_file(filespec, verbose=None, print_p=None, if_does_not_exist=None, external_format=None):
    """Load a Lisp file."""
    from fclpy import state
    
    # Save old values
    old_truename = state.current_environment.find_variable(LispSymbol('*LOAD-TRUENAME*'))
    
    # Set new values
    pathname = make_pathname(filespec)
    truename = truename_fn(pathname)
    state.current_environment.set_variable(LispSymbol('*LOAD-TRUENAME*'), truename)
    
    try:
        # Load and evaluate file
        result = runtime.load_and_evaluate_file(str(pathname), state.current_environment)
        return result
    finally:
        # Restore old values
        state.current_environment.set_variable(LispSymbol('*LOAD-TRUENAME*'), old_truename)
```

---

## Testing Strategy

### Milestone 1: Load rt.lsp successfully
```bash
pipenv run python run.py ../ansi-test/rt.lsp
```

### Milestone 2: Load init.lsp successfully
```bash
pipenv run python run.py ../ansi-test/init.lsp
```

### Milestone 3: Run a single test
```bash
# After loading init.lsp
(rt:do-test 'cl-test::+.1)
```

### Milestone 4: Run test category
```bash
pipenv run python run.py ../ansi-test/numbers/plus.lsp
```

---

## Estimated Effort

| Phase | Tasks | Estimated Time |
|-------|-------|----------------|
| Phase 1 | Critical Infrastructure | 4-6 hours |
| Phase 2 | RT Framework Support | 4-6 hours |
| Phase 3 | Core CL Features | 8-12 hours |
| Phase 4 | Test Runner Integration | 2-4 hours |

**Total: 18-28 hours**

---

## Quick Start (Minimum Viable)

To get *something* running quickly:

1. Implement `DEFVAR` in evaluator
2. Add `*LOAD-TRUENAME*`, `*DEFAULT-PATHNAME-DEFAULTS*`
3. Create/fix `LOAD` function with truename binding
4. Try loading `rt.lsp` and see what errors occur
5. Fix errors iteratively

This approach will reveal exactly what's missing faster than theoretical analysis.

---

## Files to Modify

1. `fclpy/lispfunc/evaluation_special_forms.py` - Add eval_defvar
2. `fclpy/lispfunc/evaluation_core.py` - Wire up DEFVAR dispatcher
3. `fclpy/lispenv.py` - Add special variables to standard environment
4. `fclpy/lispfunc/io.py` or new file - Add/fix LOAD function
5. `fclpy/lispfunc/pathnames.py` - Ensure TRUENAME works
6. `run.py` - Add options for ANSI test suite running
