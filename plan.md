# fclpy Development Plan

## Project Overview

fclpy is a Python implementation of Common Lisp. The goal is to achieve ANSI Common Lisp compliance.

**Current Status**: 100% coverage of target ANSI symbols (347/347), 1159 tests passing.

---

## Recent Progress (Dec 31, 2025)

### Completed
- ✅ **FLET/LABELS** - Local function binding special forms implemented
- ✅ **IN-PACKAGE special form** - Now handles uninterned symbols (`#:cl-test` syntax)
- ✅ **Convenience API** - Added `eval_string`, `get_environment`, `setup_environment` to lispfunc
- ✅ **gclload1.lsp loads** - RT (regression test) package infrastructure works (takes ~70s)

### ANSI Test Suite Status
- `gclload1.lsp` - ✅ Loads cleanly (RT package, auxiliary functions, test infrastructure)
- `gclload2.lsp` - ⚠️ Partially working - IN-PACKAGE and first LOAD work, then "Not a function: NAME" errors
- `init.lsp` - ❌ Blocked by missing pathname operations and `string-equal` with `:test` keyword

### Current Issue (Dec 31, 2025 - Session)
**Problem**: When loading test files, get "Not a function: NAME" errors.

**Root Cause Investigation**:
- DEFSTRUCT accessor functions (NAME, PEND, etc.) ARE defined in the environment
- Functions are found when looking up CL-USER or RT package symbols
- Issue: Symbol lookup during evaluation may be using wrong package symbols

**Fix Applied**:
- Made `IN-PACKAGE` a special form in `evaluation_core.py` that doesn't evaluate its argument
- This allows `(in-package #:cl-test)` with uninterned symbols to work correctly

### Next Steps
1. Debug symbol resolution for DEFSTRUCT accessors across packages
2. Ensure CL-TEST inherits RT functions properly (DEFTEST macro, entry accessors)
3. Get gclload2.lsp loading all test definition files
4. Run actual ANSI tests with `(rt:do-tests)`

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

