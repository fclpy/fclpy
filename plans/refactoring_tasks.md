# Refactoring Side Project - Task Checklist

## Phase 1: Analysis & Planning

### Task 1: Analyze File Dependencies & Structure
- [ ] Map import dependencies for lisptype.py
- [ ] Map import dependencies for lispfunc/evaluation.py
- [ ] Map import dependencies for lispfunc/io.py
- [ ] Map import dependencies for lispfunc/math.py
- [ ] Map import dependencies for lispfunc/sequences.py
- [ ] Map import dependencies for lispfunc/utilities.py
- [ ] Identify circular import risks
- [ ] Document which functions are public vs internal

### Task 2: Create Detailed Splitting Plans for Each File
- [x] lisptype.py → basic + extended (COMPLETED - reorganized)
- [x] io.py → read + write (COMPLETED)
- [x] math.py → arithmetic + advanced (COMPLETED)
- [x] sequences.py → list + vector + string + functional (COMPLETED)
- [x] utilities.py → core + comparison + introspection (COMPLETED)
- [ ] evaluation.py → core + special_forms + control_flow + declarations + debugging (create plan)
- [ ] utilities_misc.py → hashtables + clos + packages + macros (NEW - create plan)

## Phase 2: Refactoring (In Priority Order)

### Task 3: Refactor lisptype.py [✅ COMPLETED]
**Current**: Reorganized - no longer over 700 lines
- [x] Reorganized into smaller modules
- [x] Run: `pipenv run pytest -q`
- [x] Verify coverage maintained

### Task 4: Refactor lispfunc/io.py [✅ COMPLETED]
**Original**: 631 lines → Split into: io_read.py (196), io_write.py (479), io.py (55 re-exporter)
- [x] Create fclpy/lispfunc/io_read.py with read operations
- [x] Create fclpy/lispfunc/io_write.py with write operations
- [x] Update fclpy/lispfunc/io.py to re-export from new modules
- [x] Run: `pipenv run pytest -q`
- [x] Verify all I/O tests pass

### Task 5: Refactor lispfunc/math.py [✅ COMPLETED]
**Original**: 778 lines → Split into: math_arithmetic.py (509), math_advanced.py (302), math.py (52 re-exporter)
- [x] Create fclpy/lispfunc/math_arithmetic.py with basic math
- [x] Create fclpy/lispfunc/math_advanced.py with transcendental functions
- [x] Update fclpy/lispfunc/math.py to re-export from new modules
- [x] Run: `pipenv run pytest -q`
- [x] Verify all math tests pass

### Task 6: Refactor lispfunc/sequences.py [✅ COMPLETED]
**Original**: 1245 lines → Split into multiple modules + sequences.py (50 re-exporter)
- [x] Create fclpy/lispfunc/sequences_*.py modules
- [x] Update fclpy/lispfunc/sequences.py to re-export from new modules
- [x] Run: `pipenv run pytest -q`
- [x] Verify all sequence tests pass

### Task 7: Refactor lispfunc/utilities.py [✅ COMPLETED]
**Original**: 1528 lines → Split into multiple modules + utilities.py (297 lines)
- [x] Create fclpy/lispfunc/utilities_*.py modules
- [x] Update fclpy/lispfunc/utilities.py to re-export from new modules
- [x] Run: `pipenv run pytest -q`
- [x] Verify all utility tests pass

### Task 7B: Refactor lispfunc/utilities_misc.py [NEW - REMAINING]
**Current**: 1149 lines → Target: 4 files (250-350 lines each)
- [ ] Create fclpy/lispfunc/misc_hashtables.py with hash table operations (lines 1-95)
- [ ] Create fclpy/lispfunc/misc_clos.py with CLOS operations (lines 96-420)
- [ ] Create fclpy/lispfunc/misc_packages.py with package operations (lines 830-990)
- [ ] Create fclpy/lispfunc/misc_macros.py with WITH macros, declarations, debugging, limits
- [ ] Update fclpy/lispfunc/utilities_misc.py to re-export from new modules
- [ ] Run: `pipenv run pytest -q`
- [ ] Verify all utility tests pass
- [ ] Commit: git add . ; git commit -m "refactor: split utilities_misc.py into hashtables, clos, packages, macros"

### Task 8: Refactor lispfunc/evaluation.py [REMAINING - HIGHEST PRIORITY]
**Current**: 2151 lines → Target: 5 files (200-500 lines each) [LARGEST - DO LAST]
- [ ] Create fclpy/lispfunc/evaluation_core.py with main eval/apply logic
- [ ] Create fclpy/lispfunc/evaluation_special_forms.py with special form handlers
- [ ] Create fclpy/lispfunc/evaluation_control_flow.py with CATCH/THROW/condition handling
- [ ] Create fclpy/lispfunc/evaluation_declarations.py with DECLARE/DECLAIM
- [ ] Create fclpy/lispfunc/evaluation_debugging.py with TRACE/DEBUG (optional, if needed)
- [ ] Update fclpy/lispfunc/evaluation.py to re-export from new modules
- [ ] Run: `pipenv run pytest -q`
- [ ] Verify all evaluation tests pass
- [ ] Commit: git add . ; git commit -m "refactor: split evaluation.py into core, special_forms, control_flow, declarations"

## Phase 3: Validation & Cleanup

### Task 9: Update Package Imports
- [ ] Update fclpy/__init__.py exports if needed
- [ ] Update fclpy/lispfunc/__init__.py to re-export all functions
- [ ] Verify no import cycles exist
- [ ] Test import paths: `python -c "from fclpy import ..."`
- [ ] Run: `pipenv run pytest -q` (full suite)

### Task 10: Final Validation
- [ ] All 925+ tests passing
- [ ] No deprecation warnings
- [ ] No unused imports in new files
- [ ] Code style consistent (PEP 8)
- [ ] Documentation updated where needed
- [ ] Create summary report

## Notes
- Do lisptype.py early (other files may depend on it)
- Do io.py, math.py early (relatively straightforward)
- Do sequences.py and utilities.py before evaluation.py
- Save evaluation.py for last (most complex and most tests)
- Keep original module files as re-exporters for 1-2 commits (for safety)
- Can remove original files later if desired
