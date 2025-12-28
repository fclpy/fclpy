# Plan: Running ANSI Test Suite with FCLPY

## Current State Analysis

### What FCLPY Has (Updated Dec 29, 2025)
1. **CLI exists** (`run.py`): Can load and evaluate Lisp files
2. **Basic evaluation works**: `(+ 1 2)` → `3`
3. **Package system**: `IN-PACKAGE`, `MAKE-PACKAGE`, `FIND-PACKAGE`, `USE-PACKAGE` work
4. **Many CL functions**: 925+ tests passing
5. **File loading**: `LOAD` function works with `*LOAD-TRUENAME*` and `*LOAD-PATHNAME*`
6. **Special forms**: DEFVAR, DEFPARAMETER, LET, LET*, HANDLER-BIND, HANDLER-CASE, IGNORE-ERRORS implemented
7. **Compile stubs**: `COMPILE-FILE`, `COMPILE-FILE-PATHNAME` return appropriate values
8. **Package registration**: Dynamic packages stored in `state.packages`

### Recent Progress (Dec 29, 2025)
- ✅ `rt.lsp` loads successfully (58 expressions)
- ✅ `rt-package.lsp` loads successfully  
- ✅ `cl-test-package.lsp` loads successfully
- ✅ `init.lsp` loads first 56 expressions before hitting DEFTEST errors
- ✅ Fixed `MAKE-PACKAGE` keyword argument parsing (first arg was being treated as keyword)
- ✅ Package `CL-TEST` created and uses `REGRESSION-TEST`
- ✅ `DEFTEST` macro defined in `REGRESSION-TEST` package

### Current Blocker: Package-Aware Reader
The main issue preventing full ANSI test loading:

**Problem**: When reading `DEFTEST` in code, the reader creates an unqualified symbol. When evaluating, we look in the environment but NOT in USE'd packages.

**Expected Behavior** (Real Common Lisp):
1. `*PACKAGE*` is set to `CL-TEST` 
2. `CL-TEST` uses `REGRESSION-TEST`
3. Reader sees `DEFTEST`, looks in `CL-TEST` first
4. Not found → looks in USE'd packages → finds in `REGRESSION-TEST`
5. Symbol resolves to `REGRESSION-TEST:DEFTEST`

**Current Behavior**:
1. Reader creates plain `LispSymbol("DEFTEST")` 
2. Evaluator looks in environment bindings
3. Not found → "Unbound variable: DEFTEST" error

### What Still Needs Implementation
1. **Package-aware symbol interning** - Reader must check `*PACKAGE*` and USE'd packages
2. **Reader dispatch characters** - `#c` (complex), `#*` (bit-vector) not implemented
3. **Various minor issues** - Type comparisons, some arithmetic edge cases

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

### Phase 3: Package System Integration (Priority: HIGH) - IN PROGRESS

#### Task 3.1: ✅ Fix MAKE-PACKAGE keyword parsing
- First argument was being treated as keyword when it was `:CL-TEST`
- Fixed to always treat first arg as positional

#### Task 3.2: ✅ Package registration in state.packages
- `make_package` now registers in `state.packages`
- `find_package` checks `state.packages`

#### Task 3.3: 🔲 Implement package-aware reader (BLOCKER)
**This is the critical next step.**

The reader (`lispreader.py`) needs to:
1. Track current `*PACKAGE*` during reading
2. When reading an unqualified symbol like `DEFTEST`:
   - Look in current package's symbols
   - Look in current package's USE-list packages (external symbols)
   - Intern appropriately
3. Handle package prefixes like `RT:DEFTEST` or `CL:CAR`

**Implementation approach:**
```python
# In lispreader.py or a new package_reader.py
def intern_symbol_for_read(name, current_package):
    """Intern a symbol during reading, respecting USE'd packages."""
    # 1. Check if symbol exists in current package
    sym, status = current_package.find_symbol(name)
    if sym is not None:
        return sym
    
    # 2. Check USE'd packages for exported symbol
    for used_pkg in current_package.use_packages:
        sym, status = used_pkg.find_symbol(name)
        if sym is not None and status == ':EXTERNAL':
            return sym
    
    # 3. Not found - intern in current package
    return current_package.intern_symbol(name)
```

#### Task 3.4: 🔲 Handle package-qualified symbols in reader
- Parse `PKG:SYMBOL` (external) and `PKG::SYMBOL` (internal)
- Look up package, find/intern symbol appropriately

### Phase 4: Reader Enhancements (Priority: MEDIUM)

#### Task 4.1: 🔲 Implement #c reader macro (complex numbers)
- `#c(1 2)` → `(complex 1 2)` → `1+2i`

#### Task 4.2: 🔲 Implement #* reader macro (bit-vectors)  
- `#*101` → bit vector

#### Task 4.3: 🔲 Implement #. reader macro (read-time eval)
- `#.(+ 1 2)` → `3` at read time

### Phase 5: Test Runner Integration (Priority: LOW)

#### Task 5.1: 🔲 Create fclpy-specific init file
#### Task 5.2: 🔲 Add `--load` CLI option for multiple files
#### Task 5.3: 🔲 Create test runner script with reporting

---

## Detailed Implementation Steps

### Step 1: Package-Aware Reader (NEXT PRIORITY)

The reader needs to be updated to handle package context. Key files:
- `fclpy/lispreader.py` - Main reader implementation
- `fclpy/readtable.py` - Readtable and macro characters

**Changes needed in `lispreader.py`:**

```python
class LispReader:
    def __init__(self, get_macro_func, stream):
        self.get_macro_func = get_macro_func
        self.stream = stream
        # Add package awareness
        self._current_package = None  # Will be set from state
    
    def read_symbol(self, first_char):
        """Read a symbol, handling package prefixes."""
        name = self._read_symbol_name(first_char)
        
        # Check for package prefix
        if ':' in name:
            return self._parse_package_qualified_symbol(name)
        
        # Unqualified symbol - use current package context
        return self._intern_in_current_package(name)
    
    def _intern_in_current_package(self, name):
        """Intern symbol respecting USE'd packages."""
        import fclpy.state as state
        pkg = getattr(state, 'current_package', None)
        if pkg is None:
            # Fallback to COMMON-LISP-USER
            pkg = lisptype.COMMON_LISP_USER_PACKAGE
        
        # Check current package
        sym, status = pkg.find_symbol(name)
        if sym is not None:
            return sym
        
        # Check USE'd packages for external symbol
        for used_pkg in getattr(pkg, 'use_packages', []):
            sym, status = used_pkg.find_symbol(name) 
            if sym is not None and status == ':EXTERNAL':
                return sym
        
        # Not found - intern in current package
        return pkg.intern_symbol(name)
```

### Step 2: Package find_symbol to check external symbols

Update `Package.find_symbol` to properly indicate external vs internal:

```python
def find_symbol(self, name):
    """Find symbol, checking this package and USE'd packages."""
    # Check own symbols first
    if name in self.symbols:
        status = ':EXTERNAL' if name in self.external_symbols else ':INTERNAL'
        return (self.symbols[name], status)
    
    # Check USE'd packages (only external symbols)
    for pkg in self.use_packages:
        if name in pkg.external_symbols:
            return (pkg.symbols[name], ':INHERITED')
    
    return (None, None)
```

### Step 3: Ensure DEFTEST is exported from REGRESSION-TEST

When rt.lsp defines DEFTEST, it needs to be exported. Check that the
`export` function in `misc_packages.py` properly adds to `external_symbols`.

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

### 🔲 Milestone 3: Load init.lsp without DEFTEST errors
```bash
pipenv run python run.py ../ansi-test/init.lsp
# Currently: 56 expressions before "Unbound variable: DEFTEST"
# Target: All expressions load, DEFTEST resolves correctly
```

### 🔲 Milestone 4: Run a single test
```bash
# After loading init.lsp
(rt:do-test 'cl-test::+.1)
```

### 🔲 Milestone 5: Run test category
```bash
pipenv run python run.py ../ansi-test/numbers/plus.lsp
```

---

## Estimated Remaining Effort

| Phase | Tasks | Status | Estimated Time |
|-------|-------|--------|----------------|
| Phase 1 | Critical Infrastructure | ✅ DONE | - |
| Phase 2 | RT Framework Support | ✅ DONE | - |
| Phase 3 | Package-Aware Reader | 🔲 IN PROGRESS | 4-6 hours |
| Phase 4 | Reader Enhancements | 🔲 TODO | 2-4 hours |
| Phase 5 | Test Runner Integration | 🔲 TODO | 2-4 hours |

**Remaining: 8-14 hours**

---

## Quick Next Steps

1. **Implement package-aware symbol interning in reader** (BLOCKER)
   - Modify `LispReader` to check `*PACKAGE*` and USE'd packages
   - Handle `:` package prefix syntax
   
2. **Test with init.lsp again**
   - Should now find `DEFTEST` in `REGRESSION-TEST` package
   
3. **Fix any remaining reader dispatch issues**
   - `#c` for complex numbers
   - `#*` for bit-vectors

---

## Files to Modify (Updated)

1. ~~`fclpy/lispfunc/evaluation_special_forms.py`~~ ✅ Done
2. ~~`fclpy/lispfunc/evaluation_core.py`~~ ✅ Done  
3. ~~`fclpy/lispenv.py`~~ ✅ Done
4. ~~`fclpy/lispfunc/misc_macros.py`~~ ✅ Done (LOAD function)
5. ~~`fclpy/lispfunc/misc_packages.py`~~ ✅ Done (MAKE-PACKAGE fix)
6. **`fclpy/lispreader.py`** - Add package-aware symbol interning (NEXT)
7. **`fclpy/readtable.py`** - Add #c, #* dispatch macros
8. `run.py` - Add options for ANSI test suite running
