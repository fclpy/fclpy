# fclpy Development Plan

## Project Overview

fclpy is a Python implementation of Common Lisp. The goal is to achieve ANSI Common Lisp compliance.

**Read [CLAUDE.md](CLAUDE.md) first** — it has the architecture map and the current
crash-repair development loop. This file is a status snapshot and a pointer to the
other docs, not a changelog.

**Last recorded status** (2026-08-09 snapshot, may be stale — recheck with
`pipenv run pytest -q` and `pipenv run python scripts/coverage.py`): 100% coverage
of target ANSI symbols (347/347), 1158/1159 unit tests passing (one pre-existing
failure: `STREAM-ELEMENT-TYPE` missing a function binding). The ANSI test suite
(`../ansi-test/doit.lsp`) now runs to completion end to end with **zero crashes**
(21980/21980 tests processed; 817 unexpected failures remain — see
`run_all_tests.log`). The last fixed crash was `PUSH` being registered as a plain
function (mutating a Python list) instead of a place-mutating macro, which made it
silently no-op on real Lisp cons-cell lists — see `git log` for the fix. Since crashes
are now eliminated, work should shift to reducing the 817 unexpected test failures.

---

## Current Focus: ANSI Test Suite Crash Repair

The active development mode is the crash-repair loop documented in
[REPAIR.md](REPAIR.md): run `../ansi-test/doit.lsp` via `run_all_tests.py`, find the
first crash, isolate it with `run_do_test.py`, fix the root cause, verify, and
repeat until the suite runs to completion. [CLAUDE.md](CLAUDE.md) has the
architecture map (reader vs. evaluator vs. registry) needed to find where a given
crash's root cause lives, plus a running list of non-obvious gotchas already
learned in previous repair sessions.

---

## Key Files

| File | Purpose |
|------|---------|
| `CLAUDE.md` | Architecture map + crash-repair dev loop (read first) |
| `REPAIR.md` | Step-by-step ANSI crash-repair SOP |
| `plans/ansi_test_plan.md` | Original bootstrap plan (historical; few open items remain) |
| `scripts/coverage.py` | Check ANSI symbol coverage |
| `docs/ansi_targets.txt` | Target ANSI symbol list |

## Quick Reference

```powershell
pipenv install --dev                                              # one-time setup
pipenv run pytest -q                                               # unit test suite
pipenv run python scripts/coverage.py                              # symbol coverage check
pipenv run python run_all_tests.py > run_all_tests.log 2> run_all_tests.err  # ANSI crash-repair run, see REPAIR.md
```

Note: prior session logs (fix-by-fix changelogs) have been removed from this file —
that history lives in `git log`. Durable lessons from those sessions (things that
would otherwise be re-discovered the hard way) have been moved into CLAUDE.md's
"Architectural gotchas" section instead of being repeated here.

