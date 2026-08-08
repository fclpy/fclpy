# Plan: Running ANSI Test Suite with FCLPY

> **Status: bootstrap phase complete.** This document originally covered getting
> the ANSI test harness (`rt.lsp`, `init.lsp`, package setup, pathname handling)
> loading at all. That work is done — the test harness loads and runs. Current
> work has moved to the crash-repair loop described in [CLAUDE.md](../CLAUDE.md)
> and [REPAIR.md](../REPAIR.md): running `../ansi-test/doit.lsp` (the full suite)
> to completion.

## Open Items

These are the only items from this plan not superseded by the crash-repair loop
in [CLAUDE.md](../CLAUDE.md) / [REPAIR.md](../REPAIR.md). Verify each is still
accurate before acting on it — this plan predates 1000+ later commits.

- **Test runner reporting**: `run_all_tests.py` / `run_do_test.py` (see REPAIR.md)
  run the suite and isolate individual tests, but there's no script that produces
  a structured pass/fail/crash summary across the whole `doit.lsp` run.
- **`#*` bit-vector reader macro**: had partial support as of this writing —
  confirm current status before assuming it's complete.
- **`MAKE-ARRAY` keyword coverage**: flagged as incomplete for some
  element-type/option combinations — confirm current status.

The detailed bootstrap history that used to follow here (session-by-session fix
logs for the reader/package/pathname work, Dec 2025) has been removed — that
history lives in `git log`, and the durable lessons from it are condensed in
CLAUDE.md's "Architectural gotchas" section.
