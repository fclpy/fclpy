# fclpy Development Plan

## Project Overview

fclpy is a Python implementation of Common Lisp. The goal is to achieve ANSI Common Lisp compliance.

**Current Status**: 100% coverage of target ANSI symbols (347/347), 1159 tests passing.

---

## Recent Progress (Dec 31, 2025)

### Milestone: ANSI Test Infrastructure Ready

**Commits**:
- Fixed LOOP conditional scoping, function namespace lookup, and LISP_CWD support

**Key Fixes**:
- ✅ **Function namespace lookup** - Fixed eval to use `find_func` directly for symbols in function position, preventing variable namespace shadowing (fixed "Not a function: NAME" errors)
- ✅ **LOOP conditional scoping** - WHEN/UNLESS now only apply to DO body when followed by DO clause; accumulation (APPEND/COLLECT) runs independently (fixed infinite loop in DEFTEST)
- ✅ **LISP_CWD support** - Added environment variable for separating Python CWD from Lisp working directory (enables embedded Lisp scenarios like running ANSI tests)
- ✅ **FIND-CLASS** - Now accepts optional errorp and environment arguments per ANSI spec
- ✅ **Loop timeout warnings** - All loop types (LOOP, DO, DO*, DOTIMES, DOLIST) warn after 2 minutes

### ANSI Test Suite Status
- `gclload1.lsp` - ✅ Loads cleanly (~90s for large array initialization)
- `gclload2.lsp` - ⚠️ Loads with errors (files found correctly with LISP_CWD)
- Many test files have loading errors due to missing features (CLOS, FORMAT directives, etc.)

### Next Steps: Clean Load Strategy
1. **Write loading errors to file** - Capture all errors during gclload2.lsp
2. **Fix errors incrementally** - Address each issue before re-running
3. **Track progress** - Document which test categories load cleanly
4. **Then run DO-TESTS** - Only after clean load

### Running ANSI Tests

```powershell
cd C:\Users\ACER\git\fclpy\fclpy
$env:LISP_CWD = "C:\Users\ACER\git\fclpy\ansi-test"
pipenv run python run.py ../ansi-test/gclload1.lsp ../ansi-test/gclload2.lsp
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

