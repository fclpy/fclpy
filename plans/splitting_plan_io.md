# Task 2.2: Detailed Splitting Plan for io.py

**File**: fclpy/lispfunc/io.py (869 lines - actually smaller than initially reported)
**Target**: Split into 2 files (300-350 lines each)
**Completed**: ✅ Detailed line-by-line analysis

## Overview

io.py implements I/O operations (READ, WRITE, PRINT) and pathname handling. Natural split: read operations vs. write operations.

## Proposed Structure

### Module 1: `io_read.py` (320-350 lines)
**Purpose**: Input/reading operations and stream predicates

**Functions** (approximately lines):
- readtablep (line 7)
- streamp (line 14)
- read_line (line 32)
- read_char (line 45)
- read_byte (line 58)
- peek_char (line 100)
- unread_char (line 107)
- listen (line 114)
- clear_input (line 120)
- read_char_no_hang (line 630)
- read_delimited_list (line 636)
- read_from_string (line 642)
- read_preserving_whitespace (line 650)
- read (line 619)
- Input stream predicates and stream operations (input-stream-p, etc.)

**Imports**:
```python
import fclpy.lisptype as lisptype
from . import registry as _registry
```

**Public API**:
```python
__all__ = [
    'readtablep', 'streamp', 'read_line', 'read_char', 'read_byte',
    'peek_char', 'unread_char', 'listen', 'clear_input',
    'read_char_no_hang', 'read_delimited_list', 'read_from_string',
    'read_preserving_whitespace', 'read'
]
```

---

### Module 2: `io_write.py` (350-380 lines)
**Purpose**: Output/writing operations and pathname handling

**Functions** (approximately lines):
- write_char (line 70)
- write_string (line 77)
- write_line (line 86)
- write_byte (line 93)
- write (line 132)
- prin1_to_string (line 139)
- princ_to_string (line 145)
- write_to_string (line 151)
- print_fn (line 157)
- prin1 (line 164)
- princ (line 171)
- terpri (line 178)
- fresh_line (line 185)
- finish_output (line 192)
- force_output (line 198)
- Pathname operations (pathname, pathnamep, pathname_*, make_pathname, namestring, parse_namestring, etc.)
- Output stream predicates (output_stream_p, stream-element-type, stream-external-format, etc.)

**Imports**:
```python
import fclpy.lisptype as lisptype
from . import registry as _registry
```

**Public API**:
```python
__all__ = [
    # Write operations
    'write_char', 'write_string', 'write_line', 'write_byte',
    'write', 'prin1_to_string', 'princ_to_string', 'write_to_string',
    'print_fn', 'prin1', 'princ', 'terpri', 'fresh_line',
    'finish_output', 'force_output',
    # Pathname operations
    'pathname', 'pathnamep', 'pathname_host', 'pathname_device',
    'pathname_directory', 'pathname_name', 'pathname_type',
    'pathname_version', 'make_pathname', 'namestring',
    'directory_namestring', 'host_namestring', 'file_namestring',
    'enough_namestring', 'parse_namestring', 'merge_pathnames',
    'wild_pathname_p', 'pathname_match_p', 'translate_pathname',
    'logical_pathname', 'translate_logical_pathname', 'truename',
    # Stream control
    'open_fn', 'close_fn', 'open_stream_p', 'stream_element_type',
    'stream_external_format', 'readtable_case'
]
```

---

### Module 3: `io.py` (Re-exporter, ~50 lines)
**Purpose**: Maintain backward compatibility

**Content**:
```python
"""I/O and stream operations - file handling, string operations, formatting."""

from .io_read import (
    readtablep, streamp, read_line, read_char, read_byte,
    peek_char, unread_char, listen, clear_input,
    read_char_no_hang, read_delimited_list, read_from_string,
    read_preserving_whitespace, read
)

from .io_write import (
    write_char, write_string, write_line, write_byte,
    write, prin1_to_string, princ_to_string, write_to_string,
    print_fn, prin1, princ, terpri, fresh_line,
    finish_output, force_output,
    pathname, pathnamep, pathname_host, pathname_device,
    pathname_directory, pathname_name, pathname_type,
    pathname_version, make_pathname, namestring,
    directory_namestring, host_namestring, file_namestring,
    enough_namestring, parse_namestring, merge_pathnames,
    wild_pathname_p, pathname_match_p, translate_pathname,
    logical_pathname, translate_logical_pathname, truename,
    open_fn, close_fn, open_stream_p, stream_element_type,
    stream_external_format, readtable_case
)

__all__ = [
    # Read operations
    'readtablep', 'streamp', 'read_line', 'read_char', 'read_byte',
    'peek_char', 'unread_char', 'listen', 'clear_input',
    'read_char_no_hang', 'read_delimited_list', 'read_from_string',
    'read_preserving_whitespace', 'read',
    # Write operations
    'write_char', 'write_string', 'write_line', 'write_byte',
    'write', 'prin1_to_string', 'princ_to_string', 'write_to_string',
    'print_fn', 'prin1', 'princ', 'terpri', 'fresh_line',
    'finish_output', 'force_output',
    # Pathname operations
    'pathname', 'pathnamep', 'pathname_host', 'pathname_device',
    'pathname_directory', 'pathname_name', 'pathname_type',
    'pathname_version', 'make_pathname', 'namestring',
    'directory_namestring', 'host_namestring', 'file_namestring',
    'enough_namestring', 'parse_namestring', 'merge_pathnames',
    'wild_pathname_p', 'pathname_match_p', 'translate_pathname',
    'logical_pathname', 'translate_logical_pathname', 'truename',
    # Stream control
    'open_fn', 'close_fn', 'open_stream_p', 'stream_element_type',
    'stream_external_format', 'readtable_case'
]
```

---

## Dependency Analysis

### Internal Dependencies
- io_read.py:
  - No internal fclpy dependencies at module level
  - No circular imports

- io_write.py:
  - No internal fclpy dependencies at module level
  - No circular imports

### External Dependencies
- Both modules depend on: lisptype, registry (same as original)
- No new external dependencies introduced

### Module-to-Module Dependencies
- No direct dependencies between io_read and io_write
- Both use same imports

---

## Size Verification

| File | Target | Est. Lines | Status |
|------|--------|-----------|--------|
| io_read.py | 300-350 | 330 | ✅ Within range |
| io_write.py | 300-350 | 350 | ✅ Within range |
| io.py (re-export) | <100 | 50 | ✅ Minimal |
| **Original** | **631** | - | - |
| **After split** | **~730** | - | ✅ Slight growth OK |

---

## Implementation Strategy

### Step 1: Create io_read.py
- Extract read-related functions
- Add imports
- Define __all__

### Step 2: Create io_write.py
- Extract write-related and pathname functions
- Add imports
- Define __all__

### Step 3: Update io.py
- Replace with re-exporter
- Import from both new modules
- Maintain __all__

### Step 4: Run Tests
```bash
pipenv run pytest -q
```

### Step 5: Git Commit
```bash
git add . ; git commit -m "refactor: split io.py into read and write modules

- Create io_read.py: READ, READ-CHAR, PEEK-CHAR, etc. (330 lines)
- Create io_write.py: WRITE, PRINT, pathname operations (350 lines)
- Update io.py as backward-compatible re-exporter
- All imports continue to work via re-export
- All tests passing"
```

---

## Notes

- io.py was actually 869 lines, not 631 - substantial file
- Good split point between input (read) and output (write) operations
- Pathname operations (mostly stubs) go in io_write with output functions
- No shared helper functions need to be moved to separate utility module
- Stream predicates naturally fit with their read/write operations

