# Plan: Running ANSI Test Suite with FCLPY

## Current State Analysis

### What FCLPY Has (Updated Dec 29, 2025)
1. **CLI exists** (`run.py`): Can load and evaluate Lisp files
2. **Basic evaluation works**: `(+ 1 2)` → `3`
3. **Package system**: `IN-PACKAGE`, `MAKE-PACKAGE`, `FIND-PACKAGE`, `USE-PACKAGE` work
4. **Many CL functions**: 1152 tests passing
5. **File loading**: `LOAD` function works with `*LOAD-TRUENAME*` and `*LOAD-PATHNAME*`
6. **Special forms**: DEFVAR, DEFPARAMETER, LET, LET*, HANDLER-BIND, HANDLER-CASE, IGNORE-ERRORS implemented
7. **Compile stubs**: `COMPILE-FILE`, `COMPILE-FILE-PATHNAME` return appropriate values
8. **Package registration**: Dynamic packages stored in `state.packages`

### Recent Progress (Dec 29, 2025)
- ✅ `rt.lsp` loads successfully (58 expressions)
- ✅ `rt-package.lsp` loads successfully  
- ✅ `cl-test-package.lsp` loads successfully
- ✅ Fixed `MAKE-PACKAGE` keyword argument parsing (first arg was being treated as keyword)
- ✅ Package `CL-TEST` created and uses `REGRESSION-TEST`
- ✅ `DEFTEST` macro defined in `REGRESSION-TEST` package and inherited by `CL-TEST`
- ✅ Fixed `*PACKAGE*` dynamic binding in LET/LET* forms
- ✅ Fixed `find_symbol` to check inherited packages (USE'd packages as Package objects)
- ✅ Fixed DEFSTRUCT to use global environment (like DEFUN)
- ✅ Fixed DEFSTRUCT to use current package for accessor symbols
- ✅ Fixed DEFSTRUCT `:conc-name nil` to recognize NIL symbols from any package
- ✅ Fixed Pathname class to implement `__fspath__` protocol (os.PathLike)
- ✅ Fixed `merge-pathnames` to properly handle file vs directory defaults

### Issues Found and Fixed

#### Issue 1: *PACKAGE* Dynamic Binding in LET (FIXED)
**Problem**: When code like `(let ((*package* (find-package :cl-test))) ...)` was evaluated,
the LET form created a binding but didn't update `state.current_package`. Functions that
looked up the current package from state got the wrong package.

**Fix**: Modified `eval_let` and `eval_letstar` in `evaluation_loops_conditionals.py` to:
- Detect when `*PACKAGE*` is being bound
- Update `state.current_package` to the new package value
- Restore the old package value in the finally block

#### Issue 2: find_symbol Not Checking Inherited Packages (FIXED)  
**Problem**: `find_symbol` didn't find symbols in USE'd packages when `use_packages` 
contained actual Package objects instead of package name strings.

**Fix**: Modified `find_symbol` in `lisptype_extended.py` to:
- Check if `use_packages` entry is a string or Package object
- Handle both cases when checking external symbols

#### Issue 3: DEFSTRUCT Using Wrong Environment (FIXED)
**Problem**: DEFSTRUCT accessor functions were being defined in local environment instead
of global, making them invisible after the defining form completed.

**Fix**: Modified `eval_defstruct` in `evaluation_special_forms.py` to:
- Walk up the environment parent chain to find the global/root environment
- Define all accessor functions in global_env (same pattern as DEFUN)

#### Issue 4: DEFSTRUCT Interning Symbols in Wrong Package (FIXED)
**Problem**: DEFSTRUCT accessor symbols were being interned in COMMON-LISP-USER regardless
of the current package at definition time.

**Fix**: Changed `eval_defstruct` to use `state.current_package` for interning accessor
symbols instead of hardcoding `COMMON_LISP_USER_PACKAGE`.

#### Issue 5: DEFSTRUCT :conc-name nil Not Working (FIXED)
**Problem**: When `:conc-name nil` was specified to disable name prefixes, the NIL value
from a different package (like REGRESSION-TEST) wasn't being recognized as NIL.

**Fix**: Added check for `isinstance(opt_value, LispSymbol) and opt_value.name == 'NIL'`
in addition to checking for `None` and `lisptype.NIL`.

#### Issue 6: Pathname Not os.PathLike Compatible (FIXED)
**Problem**: The Pathname class didn't implement Python's os.PathLike protocol, causing
errors like "expected str, bytes or os.PathLike object, not Pathname" when passed to
Python's os functions.

**Fix**: Added `__fspath__` method to Pathname class in `pathnames.py`:
```python
def __fspath__(self):
    return self.original
```

#### Issue 7: merge-pathnames Wrong Path Resolution (FIXED)
**Problem**: `merge-pathnames` was treating file paths as directories, causing paths like
`gclload1.lsp/rt.fasl` instead of `rt.lsp` in the same directory.

**Fix**: Rewrote `merge_pathnames` in `pathnames.py` to:
- Check if defaults ends with `/` or `\` → treat as directory
- Check if defaults is existing directory → use as-is
- Check if defaults is existing file → use parent directory
- Check if defaults has file extension → treat as file, use parent
- Otherwise → treat as directory

### Current Status: init.lsp Partial Load

After all fixes:
- ✅ Path resolution works correctly
- ✅ Files load from correct directories  
- ✅ DEFTEST is accessible in CL-TEST via inheritance
- ⚠️ Some test files fail during load due to unimplemented features

### Remaining Unimplemented Features Causing Errors
1. **LOCALLY** special form - Not implemented
2. **MAKE-STRING** function - Not implemented  
3. **#c reader macro** - Complex number syntax not implemented
4. **#* reader macro** - Bit-vector syntax not implemented
5. **Various type specifiers** - Some MAKE-ARRAY options not supported
6. **D exponent marker** - `1.31283D2` not parsed (double-float)

---

## Implementation Plan

### Phase 1: Critical Infrastructure ✅ COMPLETED

#### Task 1.1: ✅ Implement DEFVAR special form
#### Task 1.2: ✅ Add *LOAD-TRUENAME* and *LOAD-PATHNAME* 
#### Task 1.3: ✅ Add *DEFAULT-PATHNAME-DEFAULTS*
#### Task 1.4: ✅ Verify/fix file LOAD function

### Phase 2: RT Framework Support ✅ COMPLETED

#### Task 2.1: ✅ Ensure DEFMACRO works correctly
#### Task 2.2: ✅ Verify MAKE-HASH-TABLE and hash operations
#### Task 2.3: ✅ Ensure LOOP basics work
#### Task 2.4: ✅ Verify HANDLER-BIND/HANDLER-CASE

### Phase 3: Package System Integration ✅ COMPLETED

#### Task 3.1: ✅ Fix MAKE-PACKAGE keyword parsing
- First argument was being treated as keyword when it was `:CL-TEST`
- Fixed to always treat first arg as positional

#### Task 3.2: ✅ Package registration in state.packages
- `make_package` now registers in `state.packages`
- `find_package` checks `state.packages`

#### Task 3.3: ✅ Fix *PACKAGE* dynamic binding in LET/LET*
- LET/LET* now update `state.current_package` when binding `*PACKAGE*`
- Old value restored in finally block

#### Task 3.4: ✅ Fix find_symbol to check inherited packages
- Now handles Package objects in use_packages (not just strings)

#### Task 3.5: ✅ Fix DEFSTRUCT for global function definitions
- Uses global environment like DEFUN
- Uses current package for symbol interning
- Handles NIL from any package for :conc-name

### Phase 4: Pathname and File Loading ✅ COMPLETED

#### Task 4.1: ✅ Implement __fspath__ in Pathname class
- Pathname now implements os.PathLike protocol
- Can be used directly with Python os functions

#### Task 4.2: ✅ Fix merge-pathnames path resolution
- Properly distinguishes file vs directory defaults
- Correct relative path joining

### Phase 5: Reader Enhancements (Priority: MEDIUM) - TODO

#### Task 5.1: 🔲 Implement #c reader macro (complex numbers)
- `#c(1 2)` → `(complex 1 2)` → `1+2i`

#### Task 5.2: 🔲 Implement #* reader macro (bit-vectors)  
- `#*101` → bit vector

#### Task 5.3: 🔲 Implement #. reader macro (read-time eval)
- `#.(+ 1 2)` → `3` at read time

#### Task 5.4: 🔲 Implement D exponent marker for floats
- `1.5D0` → double-float 1.5

### Phase 6: Missing Functions (Priority: MEDIUM) - TODO

#### Task 6.1: 🔲 Implement LOCALLY special form
- `(locally (declare ...) body...)` → evaluates body

#### Task 6.2: 🔲 Implement MAKE-STRING function
- `(make-string 5 :initial-element #\x)` → "xxxxx"

#### Task 6.3: 🔲 Extend MAKE-ARRAY for all options
- Various element-type and other keyword args

### Phase 7: Test Runner Integration (Priority: LOW) - TODO

#### Task 7.1: 🔲 Create fclpy-specific init file
#### Task 7.2: 🔲 Add `--load` CLI option for multiple files
#### Task 7.3: 🔲 Create test runner script with reporting

---

## Key Lessons Learned

### 1. Package Symbol Resolution is Multi-Layered
The original assumption was that the reader needed to be "package-aware". In reality:
- The **reader** already interns symbols in the current package
- The **evaluator** already looks up functions via find_symbol which checks USE'd packages
- The actual issues were subtle bugs in how these layers interacted

### 2. Special Variables Need State Synchronization  
`*PACKAGE*` is special because it affects how symbols are interned. When LET binds `*PACKAGE*`,
it's not enough to just create an environment binding - `state.current_package` must also be
updated so that functions like `intern_symbol` use the correct package.

### 3. DEFSTRUCT Needs Global Environment
Unlike simple variable bindings, struct accessor functions must be visible globally.
The evaluator walks the environment parent chain during local evaluation, so DEFSTRUCT
must define functions in the root/global environment (same pattern as DEFUN).

### 4. NIL Can Come From Different Packages
When checking for NIL as an option value (like `:conc-name nil`), the NIL might be:
- Python's `None`
- The `lisptype.NIL` constant
- A LispSymbol with name "NIL" from a different package

All three cases need to be handled.

### 5. Python's os Module Needs __fspath__
When integrating with Python's file system functions, custom path classes must implement
the `os.PathLike` protocol (`__fspath__` method) or they'll fail with type errors.

### 6. merge-pathnames Semantics Are Subtle
In Common Lisp, `merge-pathnames` fills in missing components from defaults. The tricky part
is determining whether defaults represents a file or directory:
- File defaults → use parent directory as base
- Directory defaults → use as-is
This affects whether relative paths are joined correctly.

---

## Testing Strategy

### ✅ Milestone 1: Load rt.lsp successfully
```bash
pipenv run python run.py ../ansi-test/rt.lsp
# PASSED - 58 expressions loaded
```

### ✅ Milestone 2: Load package setup files
```bash
# rt-package.lsp - PASSED
# cl-test-package.lsp - PASSED (after MAKE-PACKAGE fix)
```

### ✅ Milestone 3: Path resolution works correctly
```bash
# merge-pathnames correctly joins paths
# LOAD finds files in correct directories
```

### ✅ Milestone 4: DEFTEST accessible after loading
```bash
# After loading, CL-TEST inherits DEFTEST from REGRESSION-TEST
# Verified: DEFTEST in CL-TEST: (DEFTEST, ':INHERITED')
```

### 🔲 Milestone 5: Load init.lsp without errors
```bash
pipenv run python run.py ../ansi-test/init.lsp
# Currently: Fails on unimplemented features (LOCALLY, MAKE-STRING, etc.)
# Target: All expressions load successfully
```

### 🔲 Milestone 6: Run a single test
```bash
# After loading init.lsp
(rt:do-test 'cl-test::+.1)
```

---

## Estimated Remaining Effort

| Phase | Tasks | Status | Estimated Time |
|-------|-------|--------|----------------|
| Phase 1 | Critical Infrastructure | ✅ DONE | - |
| Phase 2 | RT Framework Support | ✅ DONE | - |
| Phase 3 | Package System Integration | ✅ DONE | - |
| Phase 4 | Pathname and File Loading | ✅ DONE | - |
| Phase 5 | Reader Enhancements | 🔲 TODO | 2-4 hours |
| Phase 6 | Missing Functions | 🔲 TODO | 2-4 hours |
| Phase 7 | Test Runner Integration | 🔲 TODO | 2-4 hours |

**Remaining: 6-12 hours**

---

## Files Modified (Dec 29, 2025)

1. ✅ `fclpy/lispfunc/evaluation_loops_conditionals.py` - *PACKAGE* binding in LET/LET*
2. ✅ `fclpy/lisptype_extended.py` - find_symbol inherited package handling
3. ✅ `fclpy/lispfunc/evaluation_special_forms.py` - DEFSTRUCT fixes (global env, current pkg, NIL handling)
4. ✅ `fclpy/lispfunc/pathnames.py` - __fspath__ and merge-pathnames fixes
5. ✅ `fclpy/lispfunc/misc_packages.py` - MAKE-PACKAGE keyword parsing (earlier)
