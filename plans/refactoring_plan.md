# Refactoring Plan: Large File Splitting

## Goal
Refactor large Python files into smaller modules (300-600 lines each) to improve maintainability and readability while keeping all tests passing.

## Current Status (December 28, 2025)

### ✅ COMPLETED Files (Already Split)
- `lispfunc/io.py`: Split → io_read.py (196), io_write.py (479), io.py (55 re-exporter)
- `lispfunc/math.py`: Split → math_arithmetic.py (509), math_advanced.py (302), math.py (52 re-exporter)
- `lispfunc/sequences.py`: Split → sequences_*.py (multiple modules), sequences.py (50 re-exporter)
- `lispfunc/utilities.py`: Split → utilities_*.py (multiple modules), utilities.py (297)
- `lisptype.py`: Was 782 lines, now reorganized

### 🔴 REMAINING Files (Need Refactoring)
| File | Lines | Target | Priority |
|------|-------|--------|----------|
| `lispfunc/evaluation.py` | 2,151 | 5 files (300-500 each) | HIGH |
| `lispfunc/utilities_misc.py` | 1,149 | 4 files (250-350 each) | HIGH |

### ⚠️ IGNORED Files
- `build/lib/fclpy/lispfunc.py` (1,978 lines) - Build artifact, auto-generated

**Total Remaining**: 3,300 lines across 2 files

## Target Structure
- Each file should be 300-600 lines
- Maintain all existing functionality
- Preserve import hierarchy
- Keep `__all__` exports clean
- Run full test suite after each refactoring

## Files Requiring Refactoring

### 1. lisptype.py (782 lines) → 2-3 files
**Current**: All Lisp type definitions in one file
**Proposal**:
- `lisptype_basic.py` (300-400 lines): LispObject, LispSymbol, LispCons, primitives
- `lisptype_extended.py` (300-400 lines): Environment, Lambda, Macro, Stream, other complex types

### 2. lispfunc/evaluation.py (2151 lines) → 5 files [REMAINING]
**Current**: All evaluation logic (LARGEST file - STILL NEEDS SPLITTING)
**Proposal**:
- `evaluation_core.py` (400-500 lines): eval(), apply(), core dispatch logic
- `evaluation_special_forms.py` (400-500 lines): QUOTE, IF, DEFUN, DEFMACRO, DO, etc.
- `evaluation_control_flow.py` (300-400 lines): CATCH, THROW, UNWIND-PROTECT, condition handling
- `evaluation_declarations.py` (200-300 lines): DECLARE, DECLAIM, optimization policies
- `evaluation_debugging.py` (200-300 lines): TRACE, DEBUG, instrumentation

### 3. lispfunc/utilities_misc.py (1149 lines) → 4 files [NEW - REMAINING]
**Current**: Mixed utilities including CLOS, hash tables, packages, WITH macros
**Proposal**:
- `misc_hashtables.py` (250 lines): Hash table operations (MAKE-HASH-TABLE, GETHASH, REMHASH, etc.)
- `misc_clos.py` (350 lines): CLOS class/instance/slot/method operations
- `misc_packages.py` (280 lines): Package operations (MAKE-PACKAGE, USE-PACKAGE, etc.)
- `misc_macros.py` (270 lines): WITH macros, declarations, debugging, system limits

### 4. lispfunc/io.py (631 lines) → 2 files [✅ COMPLETED]
**Current**: All I/O operations in one file
**Proposal**:
- `io_read.py` (300-350 lines): READ, READ-FROM-STRING, input operations
- `io_write.py` (300-350 lines): PRINT, WRITE, PPRINT, WRITE-TO-STRING, output operations

### 4. lispfunc/math.py (778 lines) → 2 files
**Current**: All math operations in one file
**Proposal**:
- `math_arithmetic.py` (400-450 lines): +, -, *, /, //, MOD, ABS, MIN, MAX, rounding
- `math_advanced.py` (300-350 lines): LOG, EXP, SQRT, SIN, COS, ATAN, complex math

### 5. lispfunc/sequences.py (1245 lines) → 2-3 files
**Current**: All sequence operations (lists, vectors, strings) in one file
**Proposal**:
- `sequences_list.py` (400-450 lines): CAR, CDR, CONS, LIST, APPEND, REVERSE, NREVERSE, SORT, etc.
- `sequences_vector.py` (300-350 lines): MAKE-ARRAY, AREF, ASET, VECTOR, SVREF, etc.
- `sequences_string.py` (350-400 lines): STRING-*, CHAR-*, SUBSEQ, CONCATENATE for strings

### 6. lispfunc/utilities.py (1528 lines) → 3 files
**Current**: Mixed utility functions in one file
**Proposal**:
- `utilities_core.py` (400-500 lines): IDENTITY, TYPE, SYMBOL-NAME, SYMBOL-PLIST, basic predicates
- `utilities_comparison.py` (300-350 lines): EQ, EQL, EQUAL, EQUALP, comparison functions
- `utilities_introspection.py` (350-400 lines): DOCUMENTATION, GET-OPTIMIZATION-POLICY, FBOUNDP, BOUNDP, etc.

## Refactoring Strategy

### Phase 1: Planning & Analysis (BEFORE CODING)
- [ ] Analyze import dependencies for each file
- [ ] Identify natural breakpoints for splitting
- [ ] Plan __all__ exports for new modules
- [ ] Identify shared internal utilities

### Phase 2: Refactor Each File
For EACH file:
1. Create new module files with extracted code
2. Update imports in original file (import from new modules)
3. Update `__init__.py` or main imports
4. Run full test suite: `pipenv run pytest -q`
5. Commit: `git add . ; git commit -m "refactor: split [filename] into [new files]"`

### Phase 3: Validation
- [ ] All 925+ tests still passing
- [ ] No import cycles
- [ ] All public APIs still accessible
- [ ] Code coverage maintained

## Important Constraints
✅ Must run `pipenv run pytest -q` after EVERY file split
✅ No breaking changes to public API
✅ All imports must work correctly
✅ Git commits after each successful refactoring
✅ Each new file must be between 300-600 lines

## Files to Update After Splitting
1. `fclpy/__init__.py` - ensure exports still work
2. `fclpy/lispfunc/__init__.py` - re-export from submodules
3. Any files that import from these modules

## Success Criteria
✅ All 6 files refactored
✅ All new files are 300-600 lines each
✅ All 925+ tests passing
✅ No import errors
✅ Code is more maintainable and readable
