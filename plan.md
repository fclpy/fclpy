## Agent Execution Instructions: ANSI Compliance Roadmap (fclpy)

This document INSTRUCTS the engineering agent. Follow exactly. Maintain checkboxes live. After EVERY checked item you MUST run:

```powershell
pipenv run pytest -q
```

If tests fail, FIX before proceeding. Never mark a task complete with a red test suite. Always use `pipenv run` for any Python invocation.

---
### 1. Operating Rules
1. One logical task per commit / PR (granular).
2. Update the relevant checkbox immediately after the change merges (append short note/date).
3. No duplicate Lisp symbol registrations (halt & reconcile if encountered).
4. Add or adjust tests together with feature code (prefer TDD).
5. Keep build green at every commit; revert quickly if stuck.
6. Document intentional spec deviations in Section 11.
7. Use only canonical implementations (no re‑introducing removed stubs).
8. Always re-run full test suite after each task: `pipenv run pytest -q`.
9. Avoid large refactors without incremental tests.
10. Keep this file authoritative; do not diverge in external docs without syncing checklist state here.

---
### 2. Session Bootstrap (Run Once Per Work Session)
```powershell
pipenv --venv
pipenv install
pipenv run pytest -q
```
If baseline fails: repair before starting new tasks.

---
### 3. Workflow Loop (Per Task)
1. Pick next unchecked box (topmost in lowest-numbered active phase).
2. Read involved code/tests.
3. Add/modify failing test that expresses intended behavior (if missing).
4. Implement code.
5. Run `pipenv run pytest -q`.
6. If green: tick box, append `(Done YYYY-MM-DD)`.
7. Commit (message: `phaseX: <short description>`), push.
8. If failing: iterate ≤2 more attempts; if still failing create NOTE in Section 12 (Open Issues) and revert.
9. Repeat.

---
### 4. Phase Summary Table
| Phase | Theme | Exit Condition |
|-------|-------|----------------|
| 0 | Quick Wins & Hygiene | All predicate returns normalized, duplicates removed |
| 1 | Canonical Core & Registry | Registry-driven env, symbol slots, keywords self-eval |
| 2 | Reader + Printer + Packages | Round-trip core literals, package-aware reader |
| 3 | Evaluator + Macros | Working macroexpansion + special forms |
| 4 | Multiple Values + Conditions | Values propagation + basic restarts |
| 5 | Sequences/Arrays/Streams/Pathnames | Unified sequence protocol & base IO |
| 6 | Type System & Early CLOS | TYPEP + minimal generic dispatch |
| 7 | Declarations & Optimization | Declaration capture + coverage reporting |
| 8 | Extended Numeric & Advanced I/O | FORMAT subset, numeric tower, pretty printer |

Do not advance to a new phase until all prior phase tasks are checked and exit criteria satisfied.

---
### 5. Phase 0 – Quick Wins & Hygiene
Phase detail moved to `plans/phase0.md`.

Agent: Load that file, execute tasks top-to-bottom, updating checkboxes there AND reflecting progress in the dashboard here. Always run `pipenv run pytest -q` after each checkbox.

---
### 6. Phase 1 – Canonical Core & Registry
See `plans/phase1.md` for actionable checklist & exit criteria. Follow same execution protocol.

---
### 7. Phase 2 – Reader + Printer + Packages
See `plans/phase2.md`.

---
### 8. Phase 3 – Evaluator & Macros
See `plans/phase3.md`.

---
### 9. Phase 4 – Multiple Values, Conditions, Restarts
See `plans/phase4.md`.

---
### 10. Phase 5 – Sequences / Arrays / Streams / Pathnames
See `plans/phase5.md`.

---
### 11. Phase 6 – Type System & Early CLOS
See `plans/phase6.md`.

---
### 12. Phase 7 – Declarations, Optimization, Coverage
See `plans/phase7.md`.

---
### 13. Cross-Cutting / Continuous
- [ ] Logging hooks feature flag
	- [ ] Define env var `FCLPY_TRACE` (values: `reader,macro,all`)
	- [ ] Implement lightweight logger utility `trace(event_type, payload)` (no-op if flag unset)
	- [ ] Add trace calls in reader tokenization (Phase 2) & macroexpander (Phase 3)
	- [ ] Tests: enabling flag emits lines (capture via capsys) / disabled emits none
- [ ] Performance baseline scripts
	- [ ] Create `scripts/bench.py` harness (time N evaluations of list construction, simple arithmetic, macroexpansion)
	- [ ] Add `pipenv run python scripts/bench.py --json > benchmarks/latest.json`
	- [ ] Store previous run in `benchmarks/history/` (git tracked) and fail CI if >30% regression (later)
- [ ] Compliance matrix maintained
	- [ ] Add `docs/ansi_targets.txt` listing targeted symbols (UPPERCASE, one per line)
	- [ ] Implement `scripts/coverage.py` producing markdown table (Symbol | Status)
	- [ ] Add test asserting coverage file generation does not error (smoke)
- [ ] Spec deviations list updated when needed
	- [ ] Create `docs/spec_deviations.md` (source of Section 16 summary) & keep synchronized
	- [ ] Add CI check that every line in Section 16 appears in file and vice versa

NOTE: Keep these tasks updated; they contribute to Phase 7 readiness and early visibility.

---
### 14. Testing Doctrine
ALWAYS after each task:
```powershell
pipenv run pytest -q
```
Add tests first when practical. Use golden files for macroexpansion and reader round-trips. Reject PR if implemented symbol count regresses (coverage script).

---
### 15. Commit & Branch Naming
Branch: `phase<Number>/<short-task>`
Commit message: `phase<Number>: <task>`
Include PR block:
```
Task: <checkbox text>
Tests: <list>
Symbols Added: <list or none>
Spec Deviations: <list or none>
```

---
### 16. Spec Deviations (Maintain)
_None recorded yet._ Add entries describing intentional departures (reason, date, alternative plan).

---
### 17. Open Issues / Needs Design
_Populate when a task cannot be completed within two focused attempts._

---
### 18. Phase Reports (Add upon completion)
Add `PHASE-<n>-REPORT` sections summarizing tasks completed, tests added, symbol deltas, risks.

---
### 19. Progress Dashboard (Update continuously)
| Phase | Completed / Total | Status | Notes |
|-------|-------------------|--------|-------|
| 0 | 0 / 8 | In Progress | |
| 1 | 0 / 6 | Pending | |
| 2 | 0 / 8 | Pending | |
| 3 | 0 / 9 | Pending | |
| 4 | 0 / 8 | Pending | |
| 5 | 0 / 7 | Pending | |
| 6 | 0 / 6 | Pending | |
| 7 | 0 / 4 | Pending | |
| 8 | 0 / 10 | Pending | |
| Continuous | 0 / 4 | Ongoing | |

---
### 20. Milestone Definition: "Meaningful ANSI Subset"
Definition now explicit to aid automated gating:
Reached when ALL conditions true:
1. Phases 0–5 dashboards show 100% completion & green tests.
2. Coverage report (see Section 24) shows ≥ (configurable) 35% of target ANSI symbol list implemented (baseline), with zero REGRESSED symbols vs previous run.
3. All items in Section 13 completed.
4. `docs/spec_deviations.md` exists and Section 16 mirrors it verbatim.
5. No open blocking issues in Section 17 tagged `core-eval` or `reader`.
6. Benchmarks script runs successfully (non-empty JSON output).
Documented omissions at this milestone (to be listed in deviations file):
	- Advanced CLOS (multi-method combination, MOP)
	- Full condition & restart hierarchy breadth
	- Printer pretty dispatch & *print-xxx* dynamic variables
	- Pathname host/device portability semantics
	- Complex numeric tower beyond integers / ratios / floats (complex optional)

Agent: Before declaring milestone met, append a `PHASE-MILESTONE-REPORT` summarizing coverage %, symbol additions last 7 days, and top 3 risk areas.

---
### 21. Immediate Next Actions (Boot Sequence)
1. Verify baseline tests green.
2. Implement Phase 0: fix `cddddr` (add failing test then code).
3. Normalize predicate returns (batch safe changes; test after each logical group).
4. Proceed down Phase 0 list.

If baseline not green: create entry in Section 17, label root cause area (reader/eval/registry).

If predicate normalization touches >15 return sites, split into sub-commits each <=10 edits for review clarity.

Add temporary test `tests/test_predicate_canonical_bool.py` asserting key predicates return `T`/`NIL` (remove after Phase 0 complete to avoid redundancy if needed).

PowerShell helper commands:
```powershell
Get-ChildItem -Recurse .\fclpy\fclpy\lispfunc | Select-String -Pattern 'return True','return False'
```
Use this to drive the predicate audit sub-tasks.

When replacing returns, prefer wrapping expression with `lisp_bool(<expr>)` if non-trivial computation, else direct `T`/`NIL`.

Ensure no Python `bool` objects leak into lists printed by printer (add assertion in test).

On completion of predicate task, run a one-off scan:
```powershell
pipenv run python - <<'PY'
import inspect, builtins, sys
import fclpy.lispfunc.utilities as u
from fclpy import lisptype
violations=[]
for mod in [u]:
	for name,obj in vars(mod).items():
		if callable(obj):
			src=inspect.getsource(obj)
			if 'return True' in src or 'return False' in src:
				violations.append(f"{mod.__name__}.{name}")
print('Violations:' if violations else 'No raw bool returns left.', violations)
PY
```

---
### 22. Completion Summary (Dynamic)
_To be updated as tasks complete._

---
### 23. Agent Decision Rules (Escalation & Revert)
1. If a task fails tests after 3 focused attempts (<60 lines changed each), revert and open Section 17 issue.
2. If a change requires touching >5 modules, split by concern (e.g., symbol slots vs predicate returns) unless tightly coupled.
3. Never introduce a new global without adding it to an appropriate module `__all__` export (if pattern exists) & a smoke test.
4. Prefer adding failing test before implementation unless refactor purely internal (no behavioral change). Document exceptions in commit message footer: `Test-First-Exception: <reason>`.
5. If coverage script (once implemented) shows net negative implemented symbol count, block merge.

### 24. Quality Gates & Automation
Add these before Phase 2 work begins (may start during late Phase 0 if time):
- [ ] Lint gate (pyflakes or ruff): create `scripts/lint.py` (or use tool) & optional CI step.
- [ ] Coverage gate: `pipenv run python scripts/coverage.py --fail-on-regress` run in CI.
- [ ] Benchmark gate (non-failing initially): collect baseline JSON; later flip to failing on >30% regression.
- [ ] Docs sync check: script compares Section 16 with `docs/spec_deviations.md`.

### 25. ANSI Target Scope (Initial)
Create `docs/ansi_targets.txt` containing (illustrative — expand via CLHS):
Core symbols to implement early (Phases 0–3): QUOTE IF PROGN CONS CAR CDR LIST EQ EQL EQUAL NOT AND OR LET LET* SETF DEFUN LAMBDA FUNCTION MACROLET LABELS FLET TYPE-OF SYMBOL-VALUE SYMBOL-FUNCTION SYMBOL-PLIST INTERN EXPORT USE-PACKAGE IN-PACKAGE READ PRINT PRINC PRIN1 TERPRI.
Sequence & data (Phases 4–5 subset): LENGTH ELT FIND POSITION REMOVE MAP REDUCE VECTOR MAKE-ARRAY AREF.
Conditions & restarts: ERROR CERROR WARN SIGNAL RESTART-CASE INVOKE-RESTART ABORT.
Type system: TYPEP CLASS-OF DEFCLASS MAKE-INSTANCE ENSURE-GENERIC-FUNCTION ADD-METHOD CALL-NEXT-METHOD.
Document omissions explicitly (e.g., FORMAT, LOOP, full arithmetic tower) until scheduled.

### 26. Documentation Integration
- [ ] Add `docs/reader.md` & `docs/evaluator.md` skeletons when starting Phases 2 & 3.
- [ ] Each adds "Deferred Features" list maintained with tasks.
- [ ] Link these docs from README once Phase 2 starts.

### 27. Metrics Snapshot (Optional Automation)
Add script `scripts/metrics.py` capturing:
	- Implemented symbol count
	- Lines of Lisp core (exclude tests)
	- Test count
	- Benchmark median ops/s for simple list eval
Store CSV row in `metrics/history.csv` each run (date,timestamp,values).

---
Agent: If any new section is added, update numeric references (ensure milestone definition references correct sections). Maintain backward compatible anchors (do not rename existing section titles) to avoid breaking external docs.

---
### 28. Phase 8 – Extended Numeric & Advanced I/O
See `plans/phase8.md` (ADDED WITHOUT RENumbering earlier sections to preserve references). This phase is OPTIONAL for the initial "Meaningful ANSI Subset" milestone but tracks expansion toward a richer ANSI surface.

---
End of Agent Instructions.