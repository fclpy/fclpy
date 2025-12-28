# Task 2.1: Detailed Splitting Plan for lisptype.py

**File**: fclpy/lisptype.py (889 lines)
**Target**: Split into 2 files (350-400 lines each)
**Completed**: ✅ Line-by-line analysis complete

## Overview

lisptype.py defines the fundamental Lisp data types and condition/error hierarchy. The split separates basic types from advanced/extended types.

## Proposed Structure

### Module 1: `lisptype_basic.py` (350-380 lines)
**Purpose**: Core Lisp type definitions and fundamental utilities

**Content**:
- Exception classes (Python exceptions used internally)
- Core type hierarchy (lispT, lispSequence, lispList, lispNull)
- LispSymbol class and symbol operations
- Constants: NIL, T
- lispCons and iteration
- Basic utilities: lisp_bool, is_truthy, lisp_str, lisp_repr
- Character class (complete, used frequently)

**Line Ranges**:
```
Lines 1-46:     Exception classes (LispNotImplementedError, LispTypeError, LispError, LispEndOfFileError, LispEnvironmentError)
Lines 47-77:    Binding, py_str_map, SpecialForm, FunctionBinding (environment support)
Lines 138-167:  Core type hierarchy (lispT, lispSequence, lispList, lispNull, NIL, LispSymbol, T)
Lines 169-208:  Symbol operations (symbol_value, set_symbol_value, symbol_function, set_symbol_function, symbol_plist, set_symbol_plist)
Lines 209-265:  Utility functions (lisp_bool, is_truthy, lisp_str, lisp_repr, lispKeyword)
Lines 270-362:  Character class (complete)
Lines 486-555:  lispCons and lispConsIterator (list cons cells)
Lines 628-650:  MultipleValues class (multiple return values)
Lines 651-657:  py_str_to_sym helper
```

**Total**: ~365 lines

**Imports Needed**:
```python
# No external imports at module level (imports fclpy.state later for intern functions)
```

**Public API** (__all__):
```python
__all__ = [
    # Exceptions
    'LispNotImplementedError', 'LispTypeError', 'LispError',
    'LispEndOfFileError', 'LispEnvironmentError',
    # Core Types
    'lispT', 'lispSequence', 'lispList', 'lispNull', 'LispSymbol',
    'lispKeyword', 'Character', 'lispCons',
    # Constants
    'NIL', 'T',
    # Symbol Operations
    'symbol_value', 'set_symbol_value', 'symbol_function',
    'set_symbol_function', 'symbol_plist', 'set_symbol_plist',
    # Utilities
    'lisp_bool', 'is_truthy', 'lisp_str', 'lisp_repr',
    'MultipleValues', 'py_str_to_sym', 'lispConsIterator',
    # Binding helpers (internal but useful)
    'Binding', 'FunctionBinding', 'SpecialForm'
]
```

---

### Module 2: `lisptype_extended.py` (380-410 lines)
**Purpose**: Advanced types, environment, packages, conditions, and restarts

**Content**:
- Environment class and environment utilities
- Package class and package management
- Condition hierarchy (all condition classes)
- Restart and restart exception handling
- Helper: resolve_environment()

**Line Ranges**:
```
Lines 78-136:   Environment class (complete, with all methods)
Lines 364-413:  Package class (complete)
Lines 416-467:  Package functions (make_package, find_package, intern_symbol, intern_keyword)
Lines 656-829:  Condition hierarchy (Condition, SimpleCondition, Warning, Error, TypeError, ProgramError, ControlError, FileError, StreamError, EndOfFile, ArithmeticError, DivisionByZero, FloatingPointInvalidOperation, FloatingPointOverflow, FloatingPointUnderflow)
Lines 829-844:  resolve_environment() helper
Lines 846-889:  Restart and RestartException classes
```

**Total**: ~420 lines

**Imports Needed**:
```python
import fclpy.state as state
from .lisptype_basic import (
    LispSymbol, lispT, NIL, T,
    LispEnvironmentError
)
```

**Public API** (__all__):
```python
__all__ = [
    # Environment
    'Environment', 'resolve_environment',
    # Package
    'Package', 'make_package', 'find_package',
    'intern_symbol', 'intern_keyword',
    # Conditions
    'Condition', 'SimpleCondition', 'Warning', 'Error',
    'TypeError', 'ProgramError', 'ControlError',
    'FileError', 'StreamError', 'EndOfFile',
    'ArithmeticError', 'DivisionByZero',
    'FloatingPointInvalidOperation', 'FloatingPointOverflow',
    'FloatingPointUnderflow',
    # Restarts
    'Restart', 'RestartException',
    # Constants (for backward compatibility)
    'KEYWORD_PACKAGE', 'COMMON_LISP_PACKAGE',
    'COMMON_LISP_USER_PACKAGE'
]
```

---

### Module 3: `lisptype.py` (Re-exporter, ~50 lines)
**Purpose**: Maintain backward compatibility - existing imports from lisptype continue to work

**Content**:
```python
"""
lisptype - Core Lisp type system definitions.

This module re-exports from lisptype_basic and lisptype_extended
for backward compatibility.
"""

# Import from basic types
from .lisptype_basic import (
    LispNotImplementedError, LispTypeError, LispError,
    LispEndOfFileError, LispEnvironmentError,
    lispT, lispSequence, lispList, lispNull, LispSymbol,
    lispKeyword, Character, lispCons, lispConsIterator,
    NIL, T,
    symbol_value, set_symbol_value, symbol_function,
    set_symbol_function, symbol_plist, set_symbol_plist,
    lisp_bool, is_truthy, lisp_str, lisp_repr,
    MultipleValues, py_str_to_sym,
    Binding, FunctionBinding, SpecialForm
)

# Import from extended types
from .lisptype_extended import (
    Environment, resolve_environment,
    Package, make_package, find_package,
    intern_symbol, intern_keyword,
    Condition, SimpleCondition, Warning, Error,
    TypeError, ProgramError, ControlError,
    FileError, StreamError, EndOfFile,
    ArithmeticError, DivisionByZero,
    FloatingPointInvalidOperation, FloatingPointOverflow,
    FloatingPointUnderflow,
    Restart, RestartException,
    KEYWORD_PACKAGE, COMMON_LISP_PACKAGE,
    COMMON_LISP_USER_PACKAGE
)

__all__ = [
    # Exceptions
    'LispNotImplementedError', 'LispTypeError', 'LispError',
    'LispEndOfFileError', 'LispEnvironmentError',
    # Core Types
    'lispT', 'lispSequence', 'lispList', 'lispNull', 'LispSymbol',
    'lispKeyword', 'Character', 'lispCons', 'lispConsIterator',
    # Constants
    'NIL', 'T',
    # Symbol Operations
    'symbol_value', 'set_symbol_value', 'symbol_function',
    'set_symbol_function', 'symbol_plist', 'set_symbol_plist',
    # Utilities
    'lisp_bool', 'is_truthy', 'lisp_str', 'lisp_repr',
    'MultipleValues', 'py_str_to_sym',
    # Binding & Special
    'Binding', 'FunctionBinding', 'SpecialForm',
    # Environment
    'Environment', 'resolve_environment',
    # Package
    'Package', 'make_package', 'find_package',
    'intern_symbol', 'intern_keyword',
    # Conditions & Errors
    'Condition', 'SimpleCondition', 'Warning', 'Error',
    'TypeError', 'ProgramError', 'ControlError',
    'FileError', 'StreamError', 'EndOfFile',
    'ArithmeticError', 'DivisionByZero',
    'FloatingPointInvalidOperation', 'FloatingPointOverflow',
    'FloatingPointUnderflow',
    # Restarts
    'Restart', 'RestartException',
    # Package Constants
    'KEYWORD_PACKAGE', 'COMMON_LISP_PACKAGE',
    'COMMON_LISP_USER_PACKAGE'
]
```

---

## Dependency Analysis

### Internal Dependencies (within lisptype modules)
- `lisptype_basic.py`:
  - Uses only standard Python library
  - **No** internal fclpy dependencies at module level
  - `py_str_to_sym()` calls `intern_symbol()` from `lisptype_extended` but this is OK because:
    - Called only at runtime, not at import time
    - Function is evaluated lazily when called
  - **OR** Can move `py_str_to_sym()` to `lisptype_extended` to eliminate potential circular issue
  
- `lisptype_extended.py`:
  - Depends on: `lisptype_basic.py` (imports LispSymbol, NIL, T, LispEnvironmentError)
  - Depends on: `fclpy.state` (module-level import for package registry)
  
### External Dependencies (from other fclpy modules)
- Almost all other modules depend on lisptype
- After split, they import from `fclpy.lisptype` (re-exporter)
- **No breaking changes** - all imports continue to work

### Circular Import Risks
✅ **SAFE**: 
- lisptype_basic doesn't import anything from lispfunc
- lisptype_extended only imports from state (standard module)
- Other modules can safely import from both new modules

---

## Implementation Strategy

### Step 1: Create lisptype_basic.py
- Extract lines 1-46, 47-77, 138-167, 169-208, 209-265, 270-362, 486-555, 628-650, 651-657
- Adjust imports (none needed at top)
- Define __all__

### Step 2: Create lisptype_extended.py
- Extract remaining content (Environment, Package, Conditions, Restarts)
- Add import: `import fclpy.state as state`
- Add import: `from .lisptype_basic import (LispSymbol, lispT, NIL, T, LispEnvironmentError)`
- Define __all__
- Ensure KEYWORD_PACKAGE, COMMON_LISP_PACKAGE, COMMON_LISP_USER_PACKAGE are exported

### Step 3: Update lisptype.py
- Replace all content with re-exporter
- Import from both new modules
- Maintain __all__

### Step 4: Verify Imports
- Check: `from fclpy.lisptype import LispSymbol` (from basic)
- Check: `from fclpy.lisptype import Environment` (from extended)
- Check: `from fclpy.lisptype import Condition` (from extended)
- Check: `from fclpy import lisptype; lisptype.NIL` (still works)

### Step 5: Run Tests
```bash
cd "c:\Users\ACER\git\fclpy\fclpy"
pipenv run pytest -q
```

Expected: All 925+ tests still pass

### Step 6: Git Commit
```bash
git add . ; git commit -m "refactor: split lisptype.py into basic and extended modules

- Create lisptype_basic.py: Core types, symbols, characters (350 lines)
- Create lisptype_extended.py: Environment, packages, conditions (410 lines)
- Update lisptype.py as backward-compatible re-exporter
- All imports continue to work via re-export
- All 925+ tests passing"
```

---

## Size Verification

| File | Target | Lines | Status |
|------|--------|-------|--------|
| lisptype_basic.py | 300-400 | 365 | ✅ Within range |
| lisptype_extended.py | 300-400 | 410 | ✅ Within range |
| lisptype.py (re-export) | <100 | 50 | ✅ Minimal |
| **Original** | **782** | - | - |
| **After split** | **~825** | - | ✅ Slight growth OK (re-export overhead) |

---

## Special Considerations

1. **py_str_to_sym() location**: Currently at end of file, calls intern_symbol(). This is fine - it's lazy evaluation. Alternative: move to lisptype_extended if runtime circular import concerns arise.

2. **Import order**: lisptype_basic should be imported first, lisptype_extended second (depends on basic).

3. **Package initialization**: KEYWORD_PACKAGE, COMMON_LISP_PACKAGE, COMMON_LISP_USER_PACKAGE are created in lisptype_extended when the module loads. This triggers fclpy.state import, which should be fine (state is foundational).

4. **resolve_environment()**: Currently at end of file with lazy import of state. Location in lisptype_extended is appropriate.

5. **Backward compatibility**: All imports from the original lisptype.py path will continue to work via re-exporter. No code changes needed in other modules.

---

## Files to Update After Split

### Imports to verify still work:
- `fclpy/lispfunc/*.py` - All import from lisptype
- `fclpy/lispenv.py` - Imports from lisptype
- `fclpy/lispreader.py` - May import from lisptype
- `fclpy/printer.py` - May import from lisptype
- `tests/test_lisptype.py` - Tests the module

### No changes needed in imports if using re-exporter - just verification testing.

---

## Next Steps After 2.1

- Task 2.2: Plan io.py split
- Task 2.3: Plan math.py split  
- ... (complete all planning before implementation)
- Task 3: Begin Phase 1 refactoring (implement lisptype.py split)

