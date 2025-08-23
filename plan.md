<!-- AGENT INSTRUCTION: Before executing any task OR immediately after a context summarization event, re-read this file (read_file plan.md) to ensure the freshest directives are loaded. -->
## Simple AI Agent Instructions: ANSI Compliance Roadmap (fclpy)

**WHAT IS THIS PROJECT?**
fclpy is a Python implementation of Common Lisp. We're building it step-by-step to support a meaningful subset of the ANSI Common Lisp specification. The project is organized into phases, each phase building on the previous ones.

**YOUR JOB:**
Follow the instructions in this file EXACTLY. Work on one small task at a time. Test after every change. Don't skip steps or try to be clever.

This document tells you EXACTLY what to do. Follow each step in order. 

**CRITICAL RULE**: After EVERY task you complete, you MUST run this command:

```powershell
pipenv run pytest -q
```

**CRITICAL RULE**: If any tests fail, you MUST fix them before doing the next task. Never move forward with broken tests.

**CRITICAL RULE**: Always use `pipenv run` when running any Python command.

**CRITICAL RULE**: Always use PowerShell syntax. Use `;` to join commands, NOT `&&`.


### Safety and Version Control Protocol (PowerShell)

1. **Work in Feature Branch**
   - Always create or remain in a dedicated feature branch.
   - Do not merge into `main` unless explicitly instructed.

2. **Atomic Step Execution**
   - Execute only one step at a time.
   - After each step, run all unit tests:
     ```powershell
     pipenv run pytest -q
     ```
     (Note: Ignore any references to `Invoke-Pester` - this is PowerShell-specific and not relevant)
   - If tests fail, mark the step as **FAILED** and stop.
   - Regenerate the plan only for that failed step, preserving previous successful work.

3. **Git Commit Rules**
   - After all tests **pass**, stage and commit changes:
     ```powershell
     git add .
     git commit -m "Step <step_number> successful"
     ```
   - Never commit when tests are failing.

4. **Change Validation**
   - Before committing, review changes:
     ```powershell
     git diff --stat
     ```
   - If more than 200 lines are deleted, or if entire files are removed unexpectedly, halt and mark the step as **FAILED**.
   - Do not proceed until confirmed or corrected.

5. **Preservation of Existing Functions**
   - When modifying code, **preserve all previously defined functions, classes, and signatures** unless the step explicitly requires removal.
   - If deduplication or refactoring removes required definitions, the step is automatically **FAILED**.

6. **Recovery**
   - On failure, do not continue to the next step.
   - Retry the failed step with constraints to avoid the detected issue.
   - Use the last successful commit as the rollback point if corruption occurs:
     ```powershell
     git reset --hard HEAD
     git clean -fd
     ```

7. **Checkpoints**
   - Treat every successful commit as a checkpoint.
   - Always base retries from the most recent successful commit.

---
### 1. Basic Rules (READ THESE FIRST)
1. **Do one small task at a time**. Complete it fully before starting the next.
2. **After completing ANY task**, check the box and add `(Done YYYY-MM-DD)` next to it.
3. **Never register the same Lisp function twice**. If you see duplicates, remove them.
4. **Add tests for new code**. Write the test first if possible.
5. **Keep all tests passing**. If something breaks, fix it immediately.
6. **Use real implementations**, not empty stubs that just return None.
7. **Always run the full test suite after each task**: `pipenv run pytest -q`.
8. **Make small changes**. Don't try to do everything at once.
9. **This file is the master plan**. Don't change it without good reason.
10. **Read the task descriptions carefully**. Do exactly what they say.
11. **Use PowerShell syntax**: Use `;` not `&&` to join commands.
12. **When in doubt, ask questions**. Don't guess.

---
### 2. Before You Start (Run Once Each Day)
Run these commands to make sure everything works:

```powershell
pipenv --venv
pipenv install
pipenv run pytest -q
```

**If any tests fail, fix them before starting work on tasks.**

**If you see PowerShell syntax errors with `&&`, use `;` instead:**
- ❌ Wrong: `cd "path" && pipenv run pytest -q`  
- ✅ Right: `cd "path"; pipenv run pytest -q`

---
### 3. How To Do Each Task (STEP BY STEP)
1. **Find the next unchecked box [ ]** in the current phase.
2. **Read what the task says to do** carefully.
3. **Look at the code files** mentioned in the task.
4. **Write a test** that checks if your change works (if one doesn't exist).
5. **Make the code change** (ONE SMALL CHANGE AT A TIME).
6. **Run tests**: `pipenv run pytest -q` to check all tests pass.
7. **If tests pass**: check the box [x] and write `(Done YYYY-MM-DD)` next to it.
8. **Commit your change**: 
   ```powershell
   git add .
   git commit -m "phase0: fix cddddr function"
   git push origin develop
   ```
9. **If tests fail**: try to fix the problem. If you can't fix it after 2 tries, ask for help and revert your changes using:
   ```powershell
   git reset --hard HEAD
   git clean -fd
   ```
10. **Go to step 1** for the next task.

---
### 4. What Each Phase Does (Overview)
| Phase | What It Fixes | How You Know It's Done |
|-------|---------------|------------------------|
| 0 | Basic bugs, cleanup | All checkboxes done, tests pass |
| 1 | Core symbol system | Symbols work properly, registry complete |
| 2 | Reading and printing | Can read "(a b c)" and print it back |
| 3 | Basic evaluation | Can evaluate simple Lisp expressions |
| 4 | Multiple values, errors | Error handling works |
| 5 | Lists, arrays, files | Basic data structures work |
| 6 | Object types | Type checking works |
| 7 | Declarations, optimization | Code declarations work |
| 8 | Advanced numbers and I/O | Complex math and file operations |

**IMPORTANT**: Finish ALL tasks in a phase before starting the next phase.

---
### 5. Phase 0 – Fix Basic Problems
**TASK**: Go to file `plans/phase0.md` and do every task listed there, in order.

**REMEMBER**: Run `pipenv run pytest -q` after each task and update checkboxes in both files.

---
### 6. Phase 1 – Core Symbol System
**TASK**: Go to file `plans/phase1.md` and do every task listed there, in order.

**REMEMBER**: Run tests after each task and update checkboxes.

---
### 7. Phase 2 – Reader and Printer
**TASK**: Go to file `plans/phase2.md` and do every task listed there, in order.

---
### 8. Phase 3 – Basic Evaluation
**TASK**: Go to file `plans/phase3.md` and do every task listed there, in order.

---
### 9. Phase 4 – Multiple Values and Error Handling  
**TASK**: Go to file `plans/phase4.md` and do every task listed there, in order.

---
### 10. Phase 5 – Data Structures and I/O
**TASK**: Go to file `plans/phase5.md` and do every task listed there, in order.

---
### 11. Phase 6 – Type System
**TASK**: Go to file `plans/phase6.md` and do every task listed there, in order.

---
### 12. Phase 7 – Declarations and Optimization
**TASK**: Go to file `plans/phase7.md` and do every task listed there, in order.

---
### 13. Phase 8 – Advanced Features (Optional)
**TASK**: Go to file `plans/phase8.md` and do every task listed there, in order.

---
### 14. Extra Tasks (Always Keep These Updated)
These tasks help with quality and tracking progress:

- [ ] Add logging system with environment variable `FCLPY_TRACE`
	- [ ] Create simple logger that only logs when FCLPY_TRACE is set
	- [ ] Add logging calls in reader and macro expansion code
	- [ ] Add tests that logging works when enabled and is silent when disabled

- [ ] Add performance testing
	- [ ] Create `scripts/bench.py` to time basic operations
	- [ ] Run benchmark and save results to `benchmarks/latest.json`
	- [ ] Add test that benchmark script runs without errors

- [ ] Track which ANSI symbols are implemented
	- [ ] Create `docs/ansi_targets.txt` with list of target symbols
	- [ ] Create `scripts/coverage.py` to generate implementation status
	- [ ] Add test that coverage script works

- [ ] Track when we deviate from ANSI standard
	- [ ] Create `docs/spec_deviations.md` for documenting differences
	- [ ] Keep this in sync with section 16 of this file

---
### 15. Testing Rules
**MOST IMPORTANT**: After each task, always run:
```powershell
pipenv run pytest -q
```

Write tests before writing code when possible. Use simple test files for checking that reader and macro expansion work correctly.

---
### 16. Git Commit Messages
When you commit code, use messages like:
- Branch name: `phase0/fix-cddddr` or `phase1/add-symbol-slots`
- Commit message: `phase0: fix cddddr function` or `phase1: add symbol slots`

In your commit message, include:
```
Task: <what task you completed>
Tests: <what tests you added or changed>
Symbols Added: <list of new Lisp functions, or "none">
```

---
### 17. Differences From ANSI Standard
_When you implement something differently than the ANSI Common Lisp standard, write it down here with the date and reason._

Example:
- 2024-01-15: DEFMACRO doesn't support all lambda-list keywords yet (only implementing basic &optional, &rest for now)

---
### 18. Problems That Need Help
_If you try to complete a task twice and can't get it working, write it down here with what went wrong._

**Template for reporting problems:**
```
YYYY-MM-DD - Task [Phase X, Task Y]: [Brief description]
Problem: [What went wrong]
Attempted: [What you tried]
Current state: [Tests passing? Files changed?]
```

---
### 19. EMERGENCY PROCEDURES

**If tests are broken and you can't fix them:**
1. Don't panic - this is fixable
2. Run: `git status` to see what files changed
3. Run: `git reset --hard HEAD; git clean -fd` to undo all changes
4. Run: `pipenv run pytest -q` to verify tests work again
5. Report the problem in section 18 above
6. Try the same task again with a smaller change

**If you accidentally break many files:**
1. `git reset --hard HEAD; git clean -fd`
2. `pipenv run pytest -q` (should pass now)
3. Start over with smaller changes

**If git commands don't work:**
1. Check you're in the right directory: `pwd` should show `.../fclpy/fclpy`
2. Check git status: `git status`
3. If lost, get help

---
### 19. Completed Phase Reports
_After completing each phase, write a short summary here of what was accomplished._

---
### 20. Progress Tracker (UPDATE AFTER EACH COMPLETED TASK)

**Current Status: Working on Phase 0**

| Phase | Completed Tasks | Total Tasks | Status | Next Task | Notes |
|-------|----------------|-------------|---------|-----------|-------|
| 0 | 1 | 8 | **ACTIVE** | Task 2: Fix predicates | Task 1 done, tests passing |
| 1 | 0 | 6 | Not Started | - | Depends on Phase 0 completion |
| 2 | 0 | 9 | Not Started | - | Depends on Phase 1 completion |
| 3 | 0 | 9 | Not Started | - | |
| 4 | 0 | 5 | Not Started | - | |
| 5 | 0 | 7 | Not Started | - | |
| 6 | 0 | 6 | Not Started | - | |
| 7 | 0 | 4 | Not Started | - | |
| 8 | 0 | 10 | Not Started | - | Optional phase |
| Extra | 0 | 4 | Ongoing | - | Can be done anytime |

**Instructions for updating this table:**
1. After completing each task, increment the "Completed Tasks" count
2. Update "Next Task" to show what to do next
3. Change "Status" to "COMPLETE" when all tasks in a phase are done
4. Move "ACTIVE" status to the next phase when starting it

---
### 21. When Is The Project "Done"?
The project reaches "Meaningful ANSI Subset" milestone when:
1. Phases 0-5 are 100% complete with all tests passing
2. At least 35% of target ANSI symbols are implemented (use coverage script)
3. All extra tasks in section 14 are done
4. Documentation exists for what's not implemented yet
5. No major blocking problems in section 18

---
### 22. What To Do Right Now (START HERE!)

**IMMEDIATE NEXT STEPS:**

1. **Verify tests are working**: 
   ```powershell
   pipenv run pytest -q
   ```
   
2. **If tests fail, stop and fix them first**

3. **Start Phase 0**: Go to `plans/phase0.md` and look for the first unchecked task `[ ]`

4. **Current Phase 0 status check**: 
   - Look for Task 2 in `plans/phase0.md` (Fix predicate functions)
   - This appears to be the next major uncompleted task
   
5. **Work on ONE task at a time** - don't try to do multiple tasks

6. **After each task**: run tests, check the box, commit, and update the progress table below

---
### 23. Important Helper Commands
```powershell
# Find functions that return True/False instead of T/NIL
Get-ChildItem -Recurse .\fclpy\fclpy\lispfunc | Select-String -Pattern 'return True','return False'

# Check for duplicate function registrations  
pipenv run python -c "from fclpy.lispfunc.registry import function_registry; print(len(function_registry), 'functions registered')"

# Check current test status
pipenv run pytest -q

# Emergency rollback if things break
git reset --hard HEAD; git clean -fd

# Check git status
git status

# See recent commits
git log --oneline -5
```

---
### 24. Quality Rules
1. If a task fails tests after trying 3 times, ask for help and revert changes
2. Don't change more than 5 files at once unless they're tightly related
3. Add any new global variables to appropriate `__all__` exports
4. Write tests before implementing when possible

---
### 25. Common Troubleshooting

**Problem: PowerShell syntax error with `&&`**
- Solution: Use `;` instead. Example: `cd "path"; pipenv run pytest`

**Problem: Tests fail after making a change**
- Solution: `git reset --hard HEAD; git clean -fd` then try a smaller change

**Problem: Can't find files mentioned in tasks**
- Solution: Use `Get-ChildItem -Recurse -Name "filename"` to search
- Or use: `dir /s filename` to find files

**Problem: Don't understand what a task is asking**
- Solution: Look at the **Goal** description and existing code first
- Break the task into smaller pieces
- Ask for clarification if needed

**Problem: Not sure if task is complete**
- Solution: All tests should pass: `pipenv run pytest -q`
- Check box should be marked [x]
- Code should do exactly what the **Goal** describes

---
End of Instructions.