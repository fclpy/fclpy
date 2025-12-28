# Refactoring Analysis Report

## File-by-File Dependency & Structure Analysis

### 1. lisptype.py (782 lines)

**Purpose**: Core Lisp type definitions and data structures

**Imports**:
- `fclpy.state` (at line 416, lazy import for Package operations)
- Standard library: None at top level

**Key Classes & Functions** (Public API):
- **Exception Classes**: LispNotImplementedError, LispTypeError, LispError, LispEndOfFileError, LispEnvironmentError
- **Core Types**: lispT, lispSequence, lispList, lispNull, LispSymbol, lispKeyword, Character, Package, lispCons, MultipleValues
- **Condition Hierarchy**: Condition, SimpleCondition, Warning, Error, TypeError, ProgramError, ControlError, FileError, StreamError, EndOfFile, ArithmeticError, DivisionByZero, FloatingPointInvalidOperation, FloatingPointOverflow, FloatingPointUnderflow
- **Other**: Binding, FunctionBinding, Environment, SpecialForm, Restart, RestartException
- **Symbol/Package Management**: make_package, find_package, intern_symbol, intern_keyword
- **Symbol Operations**: symbol_value, set_symbol_value, symbol_function, set_symbol_function, symbol_plist, set_symbol_plist
- **Type Utilities**: lisp_bool, is_truthy, lisp_str, lisp_repr, resolve_environment
- **Constants**: NIL, T, KEYWORD_PACKAGE, COMMON_LISP_PACKAGE, COMMON_LISP_USER_PACKAGE

**Internal Utilities**:
- py_str_map, py_str_to_sym, lispConsIterator

**Interdependencies**:
- ✅ FOUNDATIONAL - Almost everything depends on lisptype
- ❌ Minimal imports (only fclpy.state for make_package)
- ⚠️ Circular import risk: Avoided by lazy import of state in resolve_environment()

**Suggested Split**:
```
lisptype_basic.py (350 lines):
  - Exception classes
  - Core type hierarchy (lispT, lispSequence, lispList, lispNull, LispSymbol)
  - LispCons, Character
  - Utilities: lisp_bool, is_truthy, lisp_str, lisp_repr
  - Constants: NIL, T
  - Symbol operations (symbol_value, set_symbol_value, etc.)

lisptype_extended.py (350 lines):
  - Environment, Binding, FunctionBinding, SpecialForm
  - Package, lispKeyword
  - MultipleValues
  - Condition hierarchy (entire tree)
  - Restart, RestartException
  - Package management: make_package, find_package, intern_symbol, intern_keyword
  - resolve_environment
```

**Tests Affected**: tests/test_lisptype.py (if exists)

---

### 2. lispfunc/evaluation.py (2151 lines) - LARGEST FILE

**Purpose**: Core Lisp evaluator and special form handlers

**Imports**:
- `fclpy.state`, `fclpy.lisptype`, `fclpy.lispreader`, `fclpy.lispenv`
- `.core` (car, cdr, cons, _consp_internal, _atom_internal)
- `.registry` (function/special form registry)
- `fclpy.lispfunc` (circular, used inside function bodies)

**Key Functions** (200+ eval_* functions):
- **Core**: eval(), apply()
- **Special Forms**: eval_quote, eval_if, eval_setq, eval_defun, eval_defmacro, eval_macroexpand_1, eval_macro_function
- **Control Flow**: eval_block, eval_return_from, eval_catch, eval_throw, eval_unwind_protect, eval_tagbody, eval_go
- **Loop/Iteration**: eval_do, eval_prog1, eval_prog2
- **Conditionals**: eval_when, eval_unless, eval_cond, eval_and, eval_or, eval_progn, eval_let, eval_letstar
- **Meta/Reflection**: eval_quasiquote, eval_lambda
- **Declarations**: eval_declare, eval_declaim
- **Multiple Values**: eval_multiple_value_setq, eval_multiple_value_call, eval_multiple_value_bind
- **Conditions/Errors**: eval_signal, eval_error, eval_cerror, eval_warn, eval_restart_case, eval_restart_bind, eval_invoke_restart, eval_abort
- **Functions**: apply_fn, apply, eval_fn

**Internal Functions**:
- _store_optimization_declaration, _store_special_declaration, _expand_macro_form, _evaluate_lambda_list, _evaluate_defun_body, etc. (helpers prefixed with _)

**Interdependencies**:
- ✅ CRITICAL - Nearly every file depends on eval()
- ✅ Depends on: lisptype, lispreader, lispenv, core
- ⚠️ Circular dependency with lispfunc (imports at end of file, inside functions)

**Suggested Split** (Strategy: Group by functional area):
```
evaluation_core.py (400-450 lines):
  - eval() main dispatcher (lines 143-301)
  - apply() and apply_fn() (lines 1229-1285, 1857-1889)
  - eval_fn() wrapper (lines 1890-1904)
  - apply_lambda() helper logic
  - Core dispatch table/registry setup
  
evaluation_special_forms.py (450-500 lines):
  - eval_quote, eval_if, eval_setq (lines 269-341)
  - eval_defun, eval_defmacro (lines 343-473)
  - eval_macroexpand_1, eval_macro_function (lines 474-559)
  - eval_lambda (lines 771-805)
  - eval_declare, eval_declaim (lines 806-930)
  - Declaration helpers: _store_optimization_declaration, _store_special_declaration

evaluation_control_flow.py (350-400 lines):
  - eval_block, eval_return_from (lines 560-619)
  - eval_catch, eval_throw (lines 620-680)
  - eval_unwind_protect (lines 681-704)
  - eval_tagbody, eval_go (lines 705-770)
  - eval_prog1, eval_prog2 (lines 1190-1228)

evaluation_loops_conditionals.py (350-400 lines):
  - eval_when, eval_unless (lines 931-972)
  - eval_cond, eval_and, eval_or (lines 973-1023)
  - eval_progn, eval_let, eval_letstar (lines 1024-1122)
  - eval_quasiquote (lines 1123-1189)

evaluation_condition_handling.py (250-300 lines):
  - eval_signal, eval_error, eval_cerror, eval_warn (lines 1582-1697)
  - eval_restart_case, eval_restart_bind, eval_invoke_restart, eval_abort (lines 1697-1857)
  - Condition/restart related logic

evaluation_multiple_values.py (200-250 lines):
  - eval_multiple_value_setq (line 1456)
  - eval_multiple_value_call (lines 1466-1522)
  - eval_multiple_value_bind (lines 1523-1581)
  - Helper logic for multiple value handling
```

**Tests Affected**: tests/test_evaluation.py, tests/test_special_forms.py, etc.

---

### 3. lispfunc/io.py (631 lines)

**Purpose**: Input/output operations (READ, WRITE, PRINT)

**Imports**:
- `fclpy.lisptype`, `fclpy.lispreader`, `fclpy.state`
- `fclpy.printer` (for output)
- `.core`

**Key Functions**:
- **Read Operations**: eval_read, eval_read_from_string
- **Write Operations**: eval_print, eval_write, eval_pprint, eval_write_to_string, eval_write_sequence, eval_write_char
- **Stream Operations**: eval_open, eval_close, eval_stream_p, eval_input_stream_p, eval_output_stream_p, eval_file_position

**Interdependencies**:
- ✅ Depends on: lisptype, lispreader, printer
- ⚠️ Related to: streams (stateful, may complicate testing)

**Suggested Split**:
```
io_read.py (300-350 lines):
  - eval_read, eval_read_from_string
  - Stream input operations
  - Helper functions for reading

io_write.py (300-350 lines):
  - eval_print, eval_write, eval_pprint
  - eval_write_to_string, eval_write_sequence, eval_write_char
  - Stream output operations
  - Helper functions for writing
```

**Tests Affected**: tests/test_io.py

---

### 4. lispfunc/math.py (778 lines)

**Purpose**: Mathematical and arithmetic operations

**Imports**:
- `fclpy.lisptype`, `fclpy.state`
- `.core`
- `math` (standard library)

**Key Functions**:
- **Arithmetic**: eval_plus, eval_minus, eval_times, eval_divide, eval_floor_divide, eval_modulo
- **Comparison**: eval_less_than, eval_greater_than, eval_equal_numeric, etc.
- **Advanced Math**: eval_exp, eval_log, eval_sin, eval_cos, eval_sqrt, eval_atan, eval_asin, eval_acos, eval_tan, eval_sinh, eval_cosh, eval_tanh
- **Utilities**: eval_abs, eval_min, eval_max, eval_ceiling, eval_floor, eval_round, eval_truncate, eval_sign, eval_gcd, eval_lcm, eval_random

**Interdependencies**:
- ✅ Depends on: lisptype, core, math
- ✅ Clean - no circular dependencies

**Suggested Split**:
```
math_arithmetic.py (400-450 lines):
  - +, -, *, /, //, MOD, REM (lines ~200-400)
  - MIN, MAX, ABS, GCD, LCM (arithmetic helpers)
  - Numeric comparisons
  - CEILING, FLOOR, ROUND, TRUNCATE, SIGN
  - Helper functions for type conversion

math_advanced.py (350-400 lines):
  - EXP, LOG, SQRT (lines ~420-500)
  - SIN, COS, TAN, ASIN, ACOS, ATAN
  - SINH, COSH, TANH
  - RANDOM, RANDOM-STATE functions
  - Complex transcendental functions
```

**Tests Affected**: tests/test_math.py

---

### 5. lispfunc/sequences.py (1245 lines)

**Purpose**: Sequence operations (lists, vectors, strings)

**Imports**:
- `fclpy.lisptype`, `fclpy.state`
- `.core` (car, cdr, cons)
- `functools` (for partial)

**Key Functions** (80+):
- **List Operations**: eval_car, eval_cdr, eval_cons, eval_list, eval_append, eval_reverse, eval_nreverse, eval_sort, eval_length, eval_member, eval_assoc, eval_rassoc, eval_subseq, eval_last, eval_nth, eval_nthcdr, eval_copy_list
- **Vector Operations**: eval_make_array, eval_aref, eval_aset, eval_vector, eval_svref, eval_sset
- **String Operations**: eval_string_upcase, eval_string_downcase, eval_string_equal, eval_string_not_equal, eval_string_less_than, eval_string_greater_than, eval_char_code, eval_code_char, eval_char_upcase, eval_char_downcase
- **Advanced**: eval_find, eval_position, eval_count, eval_concatenate, eval_map, eval_mapcar, eval_some, eval_every, eval_notany, eval_notevery, eval_reduce, eval_remove, eval_remove_if, eval_remove_if_not, eval_delete, eval_substitute, eval_substitute_if

**Interdependencies**:
- ✅ Depends on: lisptype, core
- ✅ Clean - no circular dependencies

**Suggested Split**:
```
sequences_list.py (450-500 lines):
  - CAR, CDR, CONS, LIST, APPEND, REVERSE, NREVERSE
  - SORT, LENGTH, MEMBER, ASSOC, RASSOC
  - LAST, NTH, NTHCDR, COPY-LIST
  - SUBSEQ (for lists)
  - List-specific helper functions

sequences_vector.py (300-350 lines):
  - MAKE-ARRAY, AREF, ASET, VECTOR, SVREF, SSET
  - Vector/array operations
  - ELT (generic element access)
  - Vector-specific helpers

sequences_string.py (400-450 lines):
  - STRING-UPCASE, STRING-DOWNCASE
  - STRING-EQUAL, STRING-NOT-EQUAL, STRING-<, STRING->
  - CHAR-CODE, CODE-CHAR
  - CHAR-UPCASE, CHAR-DOWNCASE
  - CONCATENATE (for strings)
  - String-specific predicates and operations
  - SUBSEQ (for strings)

sequences_functional.py (300-350 lines):
  - MAP, MAPCAR, MAPLIST, MAPC, MAPL
  - FIND, POSITION, COUNT
  - SOME, EVERY, NOTANY, NOTEVERY
  - REDUCE, REMOVE, REMOVE-IF, DELETE, SUBSTITUTE
  - Functional sequence operations
```

**Tests Affected**: tests/test_sequences.py

---

### 6. lispfunc/utilities.py (1528 lines)

**Purpose**: Miscellaneous utility functions (introspection, symbols, timing, environment)

**Imports**:
- `time`, `inspect` (standard library)
- `fclpy.lisptype`, `fclpy.state`
- `fclpy.lispfunc.registry`

**Key Functions** (100+):
- **Symbol Management**: symbol_name, symbol_package, symbol_value, make_symbol, copy_symbol, import_symbol, gensym
- **Function Introspection**: fboundp, fmakunbound, fdefinition, symbol_function, functionp, compiled_function_p, macro_function, compiler_macro_function, special_operator_p
- **Comparison**: (EQ, EQL, EQUAL, EQUALP) - NOT FOUND in grep, likely in other file
- **Time Operations**: get_universal_time, decode_universal_time, get_decoded_time, time_fn, sleep
- **System Info**: lisp_implementation_type, lisp_implementation_version, machine_instance, machine_type, machine_version, software_type, software_version, short_site_name, long_site_name, user_homedir_pathname, get_env
- **System Control**: exit, quit
- **Randomness**: random, make_random_state, random_state_p
- **Compilation**: compile_fn, eval_when (context-dependent), locally
- **Function Keywords/Lambda**: function_keywords, function_lambda_expression
- **Class/Method (CLOS)**: find_class, find_method, add_method, allocate_instance, defmethod, make_instance, make_method, method_combination_error, method_function, method_generic_function, method_specializers, method_lambda_list, method_qualifiers, next_method_p, no_applicable_method, no_next_method, reinitialize_instance, remove_method, shared_initialize
- **Slot Operations**: slot_boundp, slot_exists_p, slot_makunbound, slot_unbound, slot_value
- **Class Utilities**: standard_class, standard_object
- **Misc**: abort, apropos, apropos_list, describe, list_to_cons, parse_macro_lambda_list, progv, in_package, function_keywords, documentation (from Phase 7), get_optimization_policy, is_variable_special

**Interdependencies**:
- ✅ Depends on: lisptype, state, registry
- ⚠️ Large file with mixed concerns (symbol ops, time, system info, CLOS, introspection)

**Suggested Split**:
```
utilities_symbol.py (400-450 lines):
  - symbol_name, symbol_package, symbol_value
  - make_symbol, copy_symbol, gensym
  - import_symbol, intern (if used)
  - Symbol-related predicates and utilities

utilities_function.py (350-400 lines):
  - fboundp, fmakunbound, fdefinition
  - symbol_function, functionp, compiled_function_p
  - macro_function, compiler_macro_function
  - special_operator_p
  - function_keywords, function_lambda_expression

utilities_system.py (350-400 lines):
  - Time operations: get_universal_time, decode_universal_time, get_decoded_time, time_fn, sleep
  - System info: lisp_implementation_type, lisp_implementation_version
  - machine_*, software_*, site_name functions
  - user_homedir_pathname, get_env, exit, quit
  - Random state: random, make_random_state, random_state_p

utilities_introspection.py (250-300 lines):
  - abort, apropos, apropos_list, describe
  - documentation, get_optimization_policy, is_variable_special
  - parse_macro_lambda_list, progv, in_package
  - compile_fn, eval_when, locally

utilities_clos.py (200-250 lines):
  - CLOS (Common Lisp Object System) stubs
  - find_class, find_method, add_method
  - make_instance, allocate_instance, defmethod
  - method_* functions (method_function, method_generic_function, etc.)
  - reinitialize_instance, shared_initialize
  - no_applicable_method, no_next_method
  - Mostly stubs that raise "not implemented" - can be minimal
```

**Tests Affected**: tests/test_utilities.py

---

## Import Dependency Summary

```
┌─────────────────────────────────────────────┐
│         lisptype.py (FOUNDATION)             │
└──────────┬──────────────────────────────────┘
           │
           ├─→ evaluation.py ────┐
           │                     ├─→ io.py
           ├─→ math.py           │
           │                     ├─→ sequences.py
           ├─→ sequences.py      │
           │                     └─→ utilities.py
           └─→ utilities.py
                 │
                 └─→ registry.py
                 └─→ state.py
```

**Key Observations**:
1. **lisptype.py is foundational** - Split it first, update all imports
2. **evaluation.py is the hub** - Most complex, split last
3. **io.py, math.py, sequences.py are independent** - Can split in parallel
4. **utilities.py is mixed-bag** - Many unrelated functions, good refactoring candidate
5. **Circular imports**: evaluation.py ↔ lispfunc (managed via late imports/function scope)

## Import Update Strategy

**After splitting each file**, update:
1. New module files to import from each other
2. Original file to re-export all public symbols
3. `fclpy/lispfunc/__init__.py` to include new exports
4. Any test files that import from the original module
5. Any other files that directly import functions from the split file

**Example pattern**:
```python
# Original: lispfunc/io.py (becomes re-exporter)
from .io_read import eval_read, eval_read_from_string
from .io_write import eval_print, eval_write, eval_pprint, ...

__all__ = ['eval_read', 'eval_read_from_string', 'eval_print', 'eval_write', ...]

# New: lispfunc/io_read.py (actual implementation)
# imports as needed, no circular risk

# New: lispfunc/io_write.py (actual implementation)
# imports as needed, no circular risk
```

## Testing Strategy

For each file split:
1. Create new module files with extracted code
2. Update original file to re-export
3. Run: `pipenv run pytest -q` 
4. Verify all 925+ tests still pass
5. Commit successfully refactored split
6. Then move to next file

This approach minimizes disruption - code continues to work with old import paths.
