# Remaining Splitting Plans (Updated December 28, 2025)

## Status Summary

### ✅ COMPLETED
- 2.3: math.py → math_arithmetic.py (509), math_advanced.py (302)
- 2.4: sequences.py → sequences_*.py (multiple modules)
- 2.5: utilities.py → utilities_*.py (multiple modules)

### 🔴 REMAINING
- 2.6: evaluation.py (2151 lines) → 5 modules
- 2.7: utilities_misc.py (1149 lines) → 4 modules [NEW]

---

## 2.3: math.py Split [✅ COMPLETED]

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

## 2.4: sequences.py Split [✅ COMPLETED]

**Original**: 1245 lines → **Result**: Multiple modules including:
- sequences_compose.py (351), sequences_search.py (345), sequences_higher.py (340)
- sequences_modify.py (212), vectors.py (311), sequences.py (50 re-exporter)

---

## 2.5: utilities.py Split [✅ COMPLETED]

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

## 2.6: evaluation.py Split [REMAINING - HIGHEST PRIORITY]

**File**: 2151 lines - LARGEST FILE - STILL NEEDS SPLITTING
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

## 2.7: utilities_misc.py Split [NEW - REMAINING]

**File**: 1149 lines - NEEDS SPLITTING
**Target**: 4 files (250-350 lines each)

### misc_hashtables.py (250 lines)
Lines 1-95:
- make_hash_table, gethash, remhash, maphash, clrhash, sxhash
- hash_table_count, hash_table_size, hash_table_test
- hash_table_rehash_size, hash_table_rehash_threshold
- Array operations: array_row_major_index, upgraded_array_element_type, etc.

### misc_clos.py (350 lines)
Lines 96-420:
- CLOS class operations: find_class, make_instance, allocate_instance, etc.
- Instance operations: initialize_instance, reinitialize_instance, etc.
- Slot operations: slot_boundp, slot_exists_p, slot_value, slot_missing, etc.
- Method operations: find_method, add_method, remove_method, defmethod, etc.
- Generic function operations: ensure_generic_function, generic_function_*, etc.

### misc_packages.py (280 lines)
Lines 830-990:
- Package operations: make_package, package_name, package_nicknames, rename_package
- Package lists: package_use_list, package_used_by_list, package_shadowing_symbols
- list_all_packages, unintern, unexport, shadowing_import, shadow
- use_package, unuse_package
- Macro expansion: macroexpand, macroexpand_1

### misc_macros.py (270 lines)
Lines 420-830, 990-1149:
- WITH macros: with_accessors, with_compilation_unit, with_input_from_string, etc.
- Miscellaneous utilities: complex_fn, load, describe, copy_tree, incf, etc.
- Type designators: keyword_type, integer_type, fixnum_type, etc.
- System limits: array_dimension_limit, call_arguments_limit, etc.
- Stream predicates: echo_stream_p, broadcast_stream_p, file_stream_p, etc.
- Debugging: break_fn, ed, dribble, disassemble, trace, untrace, etc.
- Documentation: documentation, get_optimization_policy, is_variable_special

**All import**: lisptype, registry, state (some functions)

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
| Original | 2 | 1149-2151 | 3,300 |
| New modules | 9 | 250-480 | ~2,900 |
| Re-exporters | 2 | 50 | 100 |
| **Total** | **13** | **Avg 250** | **~6,300** |

Note: Numbers reflect REMAINING work only.

---

## Implementation Sequence

**Phase 1-3**: ✅ COMPLETED
- lisptype.py, io.py, math.py, sequences.py, utilities.py all split

**Phase 4 (Most complex, Do Next)**:
- Refactor evaluation.py → 5 files
- Comprehensive testing
- Final validation

**Phase 5 (Cleanup)**:
- Refactor utilities_misc.py → 4 files
- Test & commit

---

## Next Steps

**REMAINING WORK**:
1. Task 7B: Split utilities_misc.py (1149 lines) → 4 modules
2. Task 8: Split evaluation.py (2151 lines) → 5 modules

Start with utilities_misc.py (simpler), then tackle evaluation.py (most complex).
