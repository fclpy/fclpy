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
- `gclload2.lsp` - ⚠️ ~680 errors during loading (down from 1154)

**Error Reduction Progress**:
| Fix | Errors Fixed | Description |
|-----|--------------|-------------|
| DEFMACRO &key parameters | 314 | Macro keyword params weren't being bound |
| Supplied-p parameters | 164 | `(param default supplied-p)` form not handled |
| LOOP FOR-AS-EQUALS-THEN | 71 | `for x = 1 then (1+ x)` wasn't recognized |
| **Total Fixed** | **549** | Down from 1154 to ~680 |

**Current Error Categories** (~680 total):
| Category | Count | Description |
|----------|-------|-------------|
| Unbound variable | 354 | Variables referenced before definition |
| Assertion failed | 268 | Tests with failing assertions during load |
| Other | 34 | Miscellaneous errors |
| Not implemented | 12 | Explicit not-implemented messages |
| Not a function | 8 | Symbol called as function but isn't |
| Argument errors | 3 | Wrong argument count/type |
| EOF | 1 | Premature end of file |

**Remaining High-Frequency Issues**:
- `CLASS-*` (200+) - CLOS class definitions not loading (DEFCLASS/DEFGENERIC needed)
- `DGMC-CLASS-*` (70x) - CLOS generic function tests
- `ACROSS` - LOOP FOR-AS-ACROSS clause not implemented
- Various Python type errors in edge cases

### Fixes Completed This Session
- ✅ DEFMACRO `&key` parameter binding (copied from DEFUN)
- ✅ Supplied-p parameter support for `(param default supplied-p)` form
- ✅ LOOP FOR-AS-EQUALS-THEN: `for x = 1 then (1+ x) until (> x 5)`
- ✅ LOOP termination types can combine with FOR iteration
- ✅ Function namespace lookup (fixed "Not a function: NAME")
- ✅ LOOP conditional scoping (fixed infinite loop in DEFTEST)
- ✅ LISP_CWD environment variable for path resolution

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
- pipenv for dependency management

### Initial Setup

```powershell
pipenv install --dev
pipenv --venv
```

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
3. **Use `pipenv run` for all Python commands**
4. **Use PowerShell syntax** (`;` not `&&`)
5. **One task at a time**, test, commit, repeat

