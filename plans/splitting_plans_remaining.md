# Task 2.3-2.6: Remaining Splitting Plans (Quick Reference)

## 2.3: math.py Split → arithmetic + advanced

**File**: 778 lines
**Target**: 2 files (400-450 lines each)

### math_arithmetic.py (420 lines)
- +, -, *, /, //, MOD, REM
- MIN, MAX, ABS, GCD, LCM
- Numeric comparisons (<, >, =, <=, >=, /=)
- CEILING, FLOOR, ROUND, TRUNCATE, SIGN
- Helper functions for type conversion

**Functions**: eval_plus, eval_minus, eval_times, eval_divide, eval_floor_divide, eval_modulo, eval_less_than, eval_greater_than, eval_numeric_equal, eval_min, eval_max, eval_abs, eval_gcd, eval_lcm, eval_ceiling, eval_floor, eval_round, eval_truncate, eval_sign, etc.

### math_advanced.py (360 lines)
- EXP, LOG, SQRT (transcendental)
- SIN, COS, TAN, ASIN, ACOS, ATAN (trigonometric)
- SINH, COSH, TANH (hyperbolic)
- RANDOM, RANDOM-STATE (randomness)
- Complex math functions
- Math helpers shared with arithmetic

**Imports**: Both use: import fclpy.lisptype, from . import registry

---

## 2.4: sequences.py Split → list+vector+string+functional

**File**: 1245 lines
**Target**: 4 files (300-450 lines each)

### sequences_list.py (450 lines)
- CAR, CDR, CONS, LIST, APPEND, REVERSE, NREVERSE
- SORT, LENGTH, MEMBER, ASSOC, RASSOC
- LAST, NTH, NTHCDR, COPY-LIST
- List-specific SUBSEQ, REDUCE-list-specific logic

### sequences_vector.py (310 lines)
- MAKE-ARRAY, AREF, ASET, VECTOR, SVREF, SSET
- Vector/array-specific operations
- ELT (element access)
- ARRAY-DIMENSION, ARRAY-DIMENSIONS

### sequences_string.py (380 lines)
- STRING-UPCASE, STRING-DOWNCASE
- STRING-EQUAL, STRING-NOT-EQUAL, STRING-<, STRING->
- CHAR-CODE, CODE-CHAR
- CHAR-UPCASE, CHAR-DOWNCASE
- String-specific SUBSEQ
- CONCATENATE (strings)
- COMMON-LISP:STRING type predicates

### sequences_functional.py (300 lines)
- MAP, MAPCAR, MAPLIST, MAPC, MAPL
- FIND, POSITION, COUNT
- SOME, EVERY, NOTANY, NOTEVERY
- REMOVE, REMOVE-IF, DELETE, SUBSTITUTE
- Functional programming helpers

**All import**: fclpy.lisptype, registry, functools

---

## 2.5: utilities.py Split → symbol+function+system+introspection+clos

**File**: 1528 lines
**Target**: 5 files (200-450 lines each)

### utilities_symbol.py (380 lines)
- symbol_name, symbol_package, symbol_value
- make_symbol, copy_symbol, gensym
- import_symbol, in_package
- Symbol predicates and accessors

### utilities_function.py (370 lines)
- fboundp, fmakunbound, fdefinition
- symbol_function, functionp, compiled_function_p
- macro_function, compiler_macro_function
- special_operator_p, function_keywords
- function_lambda_expression

### utilities_system.py (380 lines)
- Time: get_universal_time, decode_universal_time, get_decoded_time, time_fn, sleep
- System info: lisp_implementation_type, machine_*, software_*
- Site info: short_site_name, long_site_name, user_homedir_pathname
- Environment: get_env, exit, quit
- Random: random, make_random_state, random_state_p

### utilities_introspection.py (280 lines)
- abort, apropos, apropos_list, describe
- documentation, get_optimization_policy, is_variable_special
- parse_macro_lambda_list, progv
- compile_fn, eval_when, locally

### utilities_clos.py (200 lines)
- CLOS stubs (mostly not implemented)
- find_class, find_method, add_method, allocate_instance
- make_instance, defmethod, reinitialize_instance
- Method accessors (method_function, method_generic_function, etc.)
- Slot operations (slot_boundp, slot_exists_p, slot_value, etc.)

**All import**: time, inspect, lisptype, state, registry

---

## 2.6: evaluation.py Split → core+special+control+loops+conditions

**File**: 2151 lines - LARGEST
**Target**: 5 files (200-500 lines each)

### evaluation_core.py (420 lines)
- eval() main dispatcher (line 143-301)
- apply() function application (line 1229+)
- apply_fn wrapper
- Shared eval helper functions
- Registry setup/dispatch table
- Central eval/apply logic

### evaluation_special_forms.py (480 lines)
- eval_quote, eval_if, eval_setq (control)
- eval_defun, eval_defmacro (definitions)
- eval_macroexpand_1, eval_macro_function (macros)
- eval_lambda (lambdas)
- eval_declare, eval_declaim (declarations from Phase 7)
- Declaration helpers: _store_optimization_declaration, etc.
- Special form registration

### evaluation_control_flow.py (380 lines)
- eval_block, eval_return_from (blocks)
- eval_catch, eval_throw (exceptions)
- eval_unwind_protect (cleanup)
- eval_tagbody, eval_go (tags)
- Exception handling helpers

### evaluation_loops_conditionals.py (380 lines)
- eval_when, eval_unless (conditional execution)
- eval_cond, eval_and, eval_or (conditionals)
- eval_progn, eval_prog1, eval_prog2 (sequencing)
- eval_let, eval_letstar (bindings)
- eval_quasiquote (quasi-quoting)
- Scoping and binding helpers

### evaluation_conditions.py (320 lines)
- eval_signal, eval_error, eval_cerror, eval_warn (signaling)
- eval_restart_case, eval_restart_bind (restarts)
- eval_invoke_restart, eval_abort (restart control)
- eval_multiple_value_* functions (multiple values)
- Condition/restart helpers

**All import**: state, lisptype, lispreader, core, lispenv, registry, lispfunc (lazy)

---

## Critical Cross-Cutting Concerns

### For All Files
1. **Registry decorator**: All use @_registry.cl_function() decorator
   - Located in lispfunc/registry.py
   - Must remain accessible

2. **Imports**: All new modules maintain same external imports
   - lisptype, state, registry (from . import registry)
   - No new dependencies

3. **Re-exporter pattern**: Each original file becomes re-exporter
   - Maintains backward compatibility
   - All existing imports continue to work

4. **Testing**: Full `pipenv run pytest -q` after each split
   - Must maintain 925+ passing tests
   - No functionality lost

---

## Total After All Splits

| Category | Files | Typical Size | Total |
|----------|-------|--------------|-------|
| Original | 6 | 782-2151 | 6,915 |
| New modules | 14 | 200-500 | ~4,300 |
| Re-exporters | 6 | 50 | 300 |
| **Total** | **26** | **Avg 250** | **~11,515** |

Note: Growth from re-exporters is acceptable trade-off for maintainability.

---

## Implementation Sequence

**Phase 1 (Foundation)**: Task 3
- Refactor lisptype.py → 2 files
- Test & commit

**Phase 2 (Independent, Parallel OK)**:  
- Refactor io.py → 2 files
- Refactor math.py → 2 files
- Test & commit each

**Phase 3 (Large files, Parallel OK)**:
- Refactor sequences.py → 4 files
- Refactor utilities.py → 5 files
- Test & commit each

**Phase 4 (Most complex, Do Last)**:
- Refactor evaluation.py → 5 files
- Comprehensive testing
- Final validation

---

## Next Steps

All splitting plans now complete. Ready to begin Task 3: Actual refactoring implementation.

Start with lisptype.py (foundation), then proceed through phases.
