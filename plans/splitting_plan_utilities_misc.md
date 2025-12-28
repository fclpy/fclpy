# Task 2.7: Detailed Splitting Plan for utilities_misc.py

**File**: fclpy/lispfunc/utilities_misc.py (1149 lines)
**Target**: Split into 4 files (250-350 lines each)
**Created**: December 28, 2025

## Overview

utilities_misc.py contains mixed utility functions including hash tables, CLOS operations, package operations, WITH macros, type designators, and debugging tools. Natural split: group by functional area.

## Current Structure Analysis

### Line Ranges by Category:
- **Lines 1-95**: Hash table operations, array utilities
- **Lines 96-420**: CLOS class/instance/slot/method operations
- **Lines 420-650**: WITH macros, miscellaneous utilities
- **Lines 650-830**: Type designators, stream predicates
- **Lines 830-990**: Package operations
- **Lines 990-1149**: System limits, debugging, documentation

## Proposed Structure

### Module 1: `misc_hashtables.py` (~250 lines)
**Purpose**: Hash table and array operations

**Functions** (Lines 1-95):
- make_hash_table (line 9)
- gethash (line 20)
- remhash (line 28)
- maphash (line 37)
- clrhash (line 47)
- sxhash (line 57)
- hash_table_count (line 66)
- hash_table_size (line 72)
- hash_table_test (line 78)
- hash_table_rehash_size (line 84)
- hash_table_rehash_threshold (line 90)
- array_row_major_index (line 97)
- upgraded_array_element_type (line 103)
- upgraded_complex_part_type (line 109)
- adjustable_array_p (line 115)
- row_major_aref (line 121)
- Stream accessors (echo_stream_*, broadcast_stream_*, etc.)

**Imports**:
```python
import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry
```

**Public API**:
```python
__all__ = [
    'make_hash_table', 'gethash', 'remhash', 'maphash', 'clrhash', 'sxhash',
    'hash_table_count', 'hash_table_size', 'hash_table_test',
    'hash_table_rehash_size', 'hash_table_rehash_threshold',
    'array_row_major_index', 'upgraded_array_element_type',
    'upgraded_complex_part_type', 'adjustable_array_p', 'row_major_aref',
    'echo_stream_input_stream', 'echo_stream_output_stream',
    'broadcast_stream_streams', 'concatenated_stream_streams',
    'synonym_stream_symbol', 'two_way_stream_input_stream',
    'two_way_stream_output_stream',
]
```

---

### Module 2: `misc_clos.py` (~350 lines)
**Purpose**: CLOS class, instance, slot, and method operations

**Functions** (Lines 96-420):
- Class operations: find_class, make_instance, allocate_instance, initialize_instance
- Instance updates: reinitialize_instance, shared_initialize, update_instance_for_*
- Class accessors: class_of, class_name, change_class
- Class types: built_in_class, standard_class, standard_object, structure_class, structure_object
- Slot operations: slot_boundp, slot_exists_p, slot_makunbound, slot_unbound, slot_value, slot_missing
- Method operations: find_method, add_method, remove_method, defmethod, make_method
- Method accessors: method_function, method_generic_function, method_specializers, method_lambda_list, method_qualifiers
- Method dispatch: next_method_p, no_applicable_method, no_next_method, call_method, call_next_method
- Generic functions: compute_applicable_methods, ensure_generic_function, generic_function_*

**Imports**:
```python
import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry
```

**Public API**:
```python
__all__ = [
    # Class operations
    'find_class', 'make_instance', 'allocate_instance', 'initialize_instance',
    'reinitialize_instance', 'shared_initialize',
    'update_instance_for_different_class', 'update_instance_for_redefined_class',
    'class_of', 'class_name', 'change_class',
    'built_in_class', 'standard_class', 'standard_object',
    'structure_class', 'structure_object',
    # Slot operations
    'slot_boundp', 'slot_exists_p', 'slot_makunbound', 'slot_unbound',
    'slot_value', 'slot_missing',
    # Method operations
    'find_method', 'add_method', 'remove_method', 'defmethod', 'make_method',
    'method_function', 'method_generic_function', 'method_specializers',
    'method_lambda_list', 'method_qualifiers',
    'next_method_p', 'no_applicable_method', 'no_next_method',
    'call_method', 'call_next_method', 'compute_applicable_methods',
    'ensure_generic_function', 'generic_function_lambda_list',
    'generic_function_methods', 'generic_function_name',
]
```

---

### Module 3: `misc_packages.py` (~280 lines)
**Purpose**: Package operations and macro expansion

**Functions** (Lines 830-990):
- Package creation: make_package
- Package accessors: package_name, package_nicknames, rename_package
- Package lists: package_use_list, package_used_by_list, package_shadowing_symbols, list_all_packages
- Symbol import/export: unintern, unexport, shadowing_import, shadow, use_package, unuse_package
- Macro expansion: macroexpand, macroexpand_1

**Imports**:
```python
import fclpy.lisptype as lisptype
import fclpy.state as state
from fclpy.lispfunc import registry as _registry
```

**Public API**:
```python
__all__ = [
    'make_package', 'package_name', 'package_nicknames', 'rename_package',
    'package_use_list', 'package_used_by_list', 'package_shadowing_symbols',
    'list_all_packages', 'unintern', 'unexport', 'shadowing_import', 'shadow',
    'use_package', 'unuse_package', 'macroexpand', 'macroexpand_1',
]
```

---

### Module 4: `misc_macros.py` (~270 lines)
**Purpose**: WITH macros, type designators, system limits, debugging, documentation

**Functions** (Lines 420-830, 990-1149):

**WITH macros** (420-500):
- with_accessors, with_compilation_unit, with_input_from_string
- with_open_stream, with_output_to_string, with_pprint_logical_block
- with_slots, with_standard_io_syntax

**Miscellaneous utilities** (500-650):
- complex_fn, load_time_value, load, load_logical_pathname_translations
- logical_pathname_translations, directory, ensure_directories_exist
- define_setf_expander, defsetf, get_setf_expansion, proclaim
- describe, inspect_object, type_fn, copy_tree, incf
- octets_to_string, string_to_octets, get, rplaca, rplacd
- char_bits_limit, char_font_limit, optimize, special
- nil_symbol, nil_symbol_function, t_symbol, t_symbol_function
- map_into, mapcon

**Type designators** (700-800):
- keyword_type, integer_type, fixnum_type, double_float_type, single_float_type
- short_float_type, extended_char_type, hash_table_type, generic_function_type
- file_stream_type, file_error_type, end_of_file_type
- floating_point_inexact_type, floating_point_invalid_operation_type
- floating_point_overflow_type, floating_point_underflow_type
- arithmetic_error_operands, arithmetic_error_operation, file_error_pathname
- multiple_value_bind, multiple_value_call

**System limits** (1000-1050):
- array_dimension_limit, array_rank_limit, array_total_size_limit
- call_arguments_limit, multiple_values_limit, char_code_limit

**Symbol iteration** (1050-1090):
- do_symbols, do_external_symbols, do_all_symbols, with_package_iterator

**Declarations** (1090-1120):
- declaim, declare, defclass, defconstant, defgeneric, defpackage, defstruct, deftype, defparameter

**Stream predicates** (1120-1160):
- echo_stream_p, broadcast_stream_p, concatenated_stream_p, file_stream_p
- string_stream_p, synonym_stream_p, two_way_stream_p

**Debugging** (1160-1220):
- break_fn, continue_fn, ed, dribble, disassemble, room, step, trace, untrace
- provide, require, make_load_form, make_load_form_saving_slots, fill_pointer

**Documentation** (1240-1320):
- documentation, get_optimization_policy, is_variable_special

**Imports**:
```python
import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry
```

---

### Module 5: `utilities_misc.py` (Re-exporter, ~80 lines)
**Purpose**: Maintain backward compatibility

**Content**:
```python
"""Hash tables, arrays, CLOS operations, WITH macros, and miscellaneous utilities."""

from .misc_hashtables import (
    make_hash_table, gethash, remhash, maphash, clrhash, sxhash,
    hash_table_count, hash_table_size, hash_table_test,
    hash_table_rehash_size, hash_table_rehash_threshold,
    array_row_major_index, upgraded_array_element_type,
    upgraded_complex_part_type, adjustable_array_p, row_major_aref,
    echo_stream_input_stream, echo_stream_output_stream,
    broadcast_stream_streams, concatenated_stream_streams,
    synonym_stream_symbol, two_way_stream_input_stream,
    two_way_stream_output_stream,
)

from .misc_clos import (
    find_class, make_instance, allocate_instance, initialize_instance,
    reinitialize_instance, shared_initialize,
    update_instance_for_different_class, update_instance_for_redefined_class,
    class_of, class_name, change_class,
    built_in_class, standard_class, standard_object,
    structure_class, structure_object,
    slot_boundp, slot_exists_p, slot_makunbound, slot_unbound,
    slot_value, slot_missing,
    find_method, add_method, remove_method, defmethod, make_method,
    method_function, method_generic_function, method_specializers,
    method_lambda_list, method_qualifiers,
    next_method_p, no_applicable_method, no_next_method,
    call_method, call_next_method, compute_applicable_methods,
    ensure_generic_function, generic_function_lambda_list,
    generic_function_methods, generic_function_name,
)

from .misc_packages import (
    make_package, package_name, package_nicknames, rename_package,
    package_use_list, package_used_by_list, package_shadowing_symbols,
    list_all_packages, unintern, unexport, shadowing_import, shadow,
    use_package, unuse_package, macroexpand, macroexpand_1,
)

from .misc_macros import (
    # WITH macros
    with_accessors, with_compilation_unit, with_input_from_string,
    with_open_stream, with_output_to_string, with_pprint_logical_block,
    with_slots, with_standard_io_syntax,
    # Utilities
    complex_fn, load_time_value, load, describe, type_fn, copy_tree, incf,
    octets_to_string, string_to_octets, get, rplaca, rplacd,
    # Type designators
    keyword_type, integer_type, fixnum_type, # ... etc
    # System limits
    array_dimension_limit, array_rank_limit, array_total_size_limit,
    # Debugging
    break_fn, trace, untrace, documentation,
    get_optimization_policy, is_variable_special,
    # ... etc (full list)
)

# Preserve full __all__ from original file
__all__ = [
    # ... copy from original utilities_misc.py
]
```

---

## Implementation Steps

1. **Create misc_hashtables.py**
   - Extract lines 1-170 (hash table + stream accessors)
   - Add imports and __all__

2. **Create misc_clos.py**
   - Extract lines 170-420 (CLOS operations)
   - Add imports and __all__

3. **Create misc_packages.py**
   - Extract lines 830-1000 (package operations)
   - Add imports and __all__

4. **Create misc_macros.py**
   - Extract remaining functions (WITH macros, utilities, type designators, debugging)
   - Add imports and __all__

5. **Update utilities_misc.py**
   - Convert to re-exporter
   - Import from new modules
   - Preserve original __all__

6. **Test**
   - Run: `pipenv run pytest -q`
   - Verify all tests pass

7. **Commit**
   - `git add . ; git commit -m "refactor: split utilities_misc.py into hashtables, clos, packages, macros"`

---

## Dependencies

- All modules depend on: `fclpy.lisptype`, `fclpy.lispfunc.registry`
- misc_packages.py also depends on: `fclpy.state`
- misc_macros.py: Some functions use late imports of `fclpy.state`, `fclpy.lispenv`

## Risk Assessment

- **LOW RISK**: Functions are mostly independent stubs
- **NO CIRCULAR IMPORTS**: All imports are from parent modules
- **BACKWARD COMPATIBLE**: Re-exporter pattern maintains all existing imports
