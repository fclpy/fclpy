# fclpy Development Plan

## Project Overview

fclpy is a Python implementation of Common Lisp. The goal is to achieve ANSI Common Lisp compliance.

**Current Status**: 100% coverage of target ANSI symbols (347/347), 1159 tests passing.

---

## Recent Progress (Dec 31, 2025)

### ANSI Test Loading Analysis

**Error capture script**: `scripts/ansi_load_errors.py`
**Error report**: `ansi_load_errors.txt`

**Loading Results**:
- `gclload1.lsp` - ✅ Loads cleanly (0 errors)
- `gclload2.lsp` - ⚠️ 1154 errors during loading

**Error Categories** (from 1154 total):
| Category | Count | Description |
|----------|-------|-------------|
| Unbound variable | 829 | Variables referenced before definition |
| Assertion failed | 268 | Tests with failing assertions during load |
| Other | 33 | Miscellaneous errors |
| Not implemented | 12 | Explicit not-implemented messages |
| Not a function | 8 | Symbol called as function but isn't |
| Argument errors | 3 | Wrong argument count/type |
| EOF | 1 | Premature end of file |

**Top Unbound Variables**:
- `PRETTY` (237x) - Macro parameter in def-pprint-test
- `ELEMENT-TYPE-P` (76x) - Macro parameter in def-open-test
- `BUILD-FORM` (54x) - Macro parameter in def-open-test
- `MARGIN` (37x) - Macro parameter in def-pprint-test
- `CLASS-*` (100+) - CLOS class definitions not loading

**Root Causes**:
1. **Macro expansion issue** - Macro parameters (PRETTY, BUILD-FORM, etc.) are being looked up as global variables instead of local bindings
2. **CLOS not implemented** - Many tests define classes with DEFCLASS which isn't fully working
3. **compile-and-load** - Test infrastructure tries to compile files, which fclpy doesn't support

### Previous Fixes (this session)
- ✅ Function namespace lookup (fixed "Not a function: NAME")
- ✅ LOOP conditional scoping (fixed infinite loop in DEFTEST)
- ✅ LISP_CWD environment variable for path resolution
- ✅ FIND-CLASS optional arguments
- ✅ 2-minute loop timeout warnings

### Next Priority: Fix Macro Expansion
The macro parameter issue is causing 400+ errors. Need to investigate why `&key` parameters with default values aren't being properly bound during macro expansion.

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

