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
- [ ] lisptype.py → basic + extended (create splitting plan)
- [ ] evaluation.py → core + special_forms + control_flow + declarations + debugging (create plan)
- [ ] io.py → read + write (create plan)
- [ ] math.py → arithmetic + advanced (create plan)
- [ ] sequences.py → list + vector + string (create plan)
- [ ] utilities.py → core + comparison + introspection (create plan)

## Phase 2: Refactoring (In Priority Order)

### Task 3: Refactor lisptype.py
**Current**: 782 lines → Target: 2-3 files (300-600 lines each)
- [ ] Create fclpy/lisptype_basic.py with fundamental types
- [ ] Create fclpy/lisptype_extended.py with complex types
- [ ] Update fclpy/lisptype.py to re-export from new modules
- [ ] Update all imports in other files
- [ ] Run: `pipenv run pytest -q`
- [ ] Verify coverage maintained
- [ ] Commit: git add . ; git commit -m "refactor: split lisptype.py into basic and extended"

### Task 4: Refactor lispfunc/io.py
**Current**: 631 lines → Target: 2 files (300-350 lines each)
- [ ] Create fclpy/lispfunc/io_read.py with read operations
- [ ] Create fclpy/lispfunc/io_write.py with write operations
- [ ] Update fclpy/lispfunc/io.py to re-export from new modules
- [ ] Run: `pipenv run pytest -q`
- [ ] Verify all I/O tests pass
- [ ] Commit: git add . ; git commit -m "refactor: split io.py into read and write modules"

### Task 5: Refactor lispfunc/math.py
**Current**: 778 lines → Target: 2 files (400-450 lines + 300-350 lines)
- [ ] Create fclpy/lispfunc/math_arithmetic.py with basic math
- [ ] Create fclpy/lispfunc/math_advanced.py with transcendental functions
- [ ] Update fclpy/lispfunc/math.py to re-export from new modules
- [ ] Run: `pipenv run pytest -q`
- [ ] Verify all math tests pass
- [ ] Commit: git add . ; git commit -m "refactor: split math.py into arithmetic and advanced modules"

### Task 6: Refactor lispfunc/sequences.py
**Current**: 1245 lines → Target: 2-3 files (300-450 lines each)
- [ ] Create fclpy/lispfunc/sequences_list.py with list operations
- [ ] Create fclpy/lispfunc/sequences_vector.py with vector operations
- [ ] Create fclpy/lispfunc/sequences_string.py with string operations
- [ ] Update fclpy/lispfunc/sequences.py to re-export from new modules
- [ ] Run: `pipenv run pytest -q`
- [ ] Verify all sequence tests pass
- [ ] Commit: git add . ; git commit -m "refactor: split sequences.py into list, vector, and string modules"

### Task 7: Refactor lispfunc/utilities.py
**Current**: 1528 lines → Target: 3 files (300-500 lines each)
- [ ] Create fclpy/lispfunc/utilities_core.py with basic utilities
- [ ] Create fclpy/lispfunc/utilities_comparison.py with comparison functions
- [ ] Create fclpy/lispfunc/utilities_introspection.py with introspection
- [ ] Update fclpy/lispfunc/utilities.py to re-export from new modules
- [ ] Run: `pipenv run pytest -q`
- [ ] Verify all utility tests pass
- [ ] Commit: git add . ; git commit -m "refactor: split utilities.py into core, comparison, and introspection"

### Task 8: Refactor lispfunc/evaluation.py
**Current**: 2151 lines → Target: 4-5 files (200-500 lines each) [LARGEST - DO LAST]
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
