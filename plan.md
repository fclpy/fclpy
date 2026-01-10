# fclpy Development Plan

## Project Overview

fclpy is a Python implementation of Common Lisp. The goal is to achieve ANSI Common Lisp compliance.

**Current Status**: 100% coverage of target ANSI symbols (347/347), 1159 tests passing.

---

## Recent Progress (Dec 31, 2025)

### ANSI Test Loading Analysis

**Error capture script**: `scripts/ansi_load_errors.py`
**Error report**: `ansi_load_errors.txt`

**Loading Results** (after fixes):
- `gclload1.lsp` - ✅ Loads cleanly (0 errors)
- `gclload2.lsp` - ⚠️ 575 errors during loading

**Progress This Session**:
| Change | Outcome | Notes |
|--------|---------|------|
| Environment lookup caching | ✅ Unblocked gclload2 completion | Removed prior hang/slowdown from O(n) `find_func` scans |
| Treat `DEFPACKAGE` as special-form/macro | ✅ Reduced loader failures | Avoids evaluating option clauses like `(:USE ...)` |
| LOOP destructuring varspec | ✅ Fixed `FOR (KEY . VAL) IN ...` | Supports dotted-pair and simple list patterns |
| Fix `NCONC` for Lisp cons lists | ✅ Removed `'lispCons' object has no attribute 'extend'` | Implements safe cons-aware concatenation |
| Net effect | ✅ 592 → 575 | gclload2 baseline after these fixes |

**Current Error Categories** (575 total):
| Category | Count | Description |
|----------|-------|-------------|
| Unbound variable | 268 | Variables referenced before definition (mostly CLOS/test harness) |
| Assertion failed | 202 | Tests with failing assertions during load |
| Other | 85 | Python type errors and misc runtime failures |
| Not implemented | 2 | Explicit not-implemented messages |
| Not a function | 6 | Symbol called as function but isn't |
| Argument errors | 9 | Wrong argument count/type |
| EOF | 1 | Premature end of file |

**Remaining High-Frequency Issues**:
- `CLASS-*` (200+) - CLOS class definitions not loading (DEFCLASS/DEFGENERIC needed)
- `DGMC-CLASS-*` (70x) - CLOS generic function tests
- `DEFSETF`/compiler-macro-related harness macros - several still effectively unimplemented
- `DEFSTRUCT-WITH-TESTS` failures: `'LispSymbol' object is not iterable` (struct tests)
- Some remaining LOOP edge-cases (now down to 2 LOOP-category loader errors)
- Various Python type errors in edge cases (e.g., cons/list sequence APIs)

### Next Load Error: `Not a function: LOOKUP-TABLE`

**Symptom**: Loader reports: `[1x] Not a function: LOOKUP-TABLE: Not implemented | expr=(SYMBOL-MACROLET ((LOOKUP-T ...` (see `ansi_load_errors.txt`).

**Impact**: Blocks loading of test files that use `symbol-macrolet`/`lookup-table` macro patterns.

**Proposed next steps**:
- **Investigate**: Find where `SYMBOL-MACROLET` and lookup-table expansion are handled (reader/loader/compiler).
- **Implement**: Add `LOOKUP-TABLE` expansion/support so instances are not treated as function calls. Likely implement as a macro expansion or create a symbol-macro binding handler in the evaluator/loader.
- **Test**: Re-run `scripts/ansi_load_errors.py` and targeted loader for failing files to confirm the error is resolved.
- **Document**: Record the change and the failing forms in `ansi_load_errors.txt` and update this `plan.md`.

Add to TODOs: implement LOOKUP-TABLE handling and verify load passes.

### Fixes Completed This Session
- ✅ Loader diagnostics + gclload1 regression kept clean (0 errors)
- ✅ Environment lookup caching (gclload2 performance)
- ✅ `DEFPACKAGE` handled as macro/special-form in evaluator
- ✅ LOOP destructuring varspec support `(KEY . VAL)` and `(A B ...)`
- ✅ `NCONC` made cons-aware (removed `.extend` crashes)

### Running Error Capture

```powershell
cd C:\Users\ACER\git\fclpy\fclpy
pipenv run python scripts/ansi_load_errors.py
# Results in ansi_load_errors.txt
```

---

## Environment Setup

### Prerequisites

 - Python 3.10+
 - pipenv (required) — use `pipenv run` for all Python commands and dependency management

### Initial Setup

```powershell
pipenv install --dev
pipenv --venv
```

**Important:** Always run Python and project scripts through Pipenv. Do not call the system `python` or `pip` directly; instead use `pipenv run <command>` (for example `pipenv run python scripts/ansi_load_errors.py`).

### Running Tests

```powershell
# Run all tests (quick mode)
pipenv run pytest -q

# Run tests with verbose output
pipenv run pytest -v

# Run specific test file
pipenv run pytest tests/test_reader.py -v

# Run tests matching a pattern
pipenv run pytest -k "test_loop" -v
```

### Coverage Check

```powershell
pipenv run python scripts/coverage.py
```

---

## MCP Task Manager

This project uses the MCP Task Manager for task tracking. Key tools:

| Tool | Purpose |
|------|---------|
| `mcp_task-manager_list_tasks` | View all tasks in tree format |
| `mcp_task-manager_get_summary` | Get project statistics |
| `mcp_task-manager_add_task` | Create new task |
| `mcp_task-manager_start_task` | Begin work on a task |
| `mcp_task-manager_complete_task` | Mark task as done |
| `mcp_task-manager_get_next_task` | Get next incomplete task |

---

## Git Workflow

### PowerShell Syntax

Always use `;` to chain commands (not `&&`):

```powershell
# Correct
cd "path"; pipenv run pytest -q

# Wrong (will error)
cd "path" && pipenv run pytest -q
```

### Commit Pattern

```powershell
git add .
git commit -m "Brief description of change"
git push origin <branch-name>
```

### Emergency Rollback

```powershell
git reset --hard HEAD
git clean -fd
```

---

## Development Direction

### Primary Goal: ANSI Compliance

All development should focus on achieving full ANSI Common Lisp compliance by running and passing the ANSI test suite.

**START HERE: [plans/ansi_test_plan.md](plans/ansi_test_plan.md)**

### Key Files

| File | Purpose |
|------|---------|
| `scripts/coverage.py` | Check symbol coverage |
| `docs/ansi_targets.txt` | Target ANSI symbols |
| `plans/ansi_test_plan.md` | **ANSI compliance roadmap** |

---

## Quick Reference

```powershell
pipenv run pytest -q                    # Quick test run
pipenv run pytest -v                    # Verbose output
pipenv run python scripts/coverage.py   # Coverage check
git reset --hard HEAD; git clean -fd    # Emergency rollback
```

---

## Rules

1. **Run tests after every change**: `pipenv run pytest -q`
2. **Never commit with failing tests**
3. **Always run Python commands via `pipenv run` (required)**
4. **Use PowerShell syntax** (`;` not `&&`)
5. **One task at a time**, test, commit, repeat

